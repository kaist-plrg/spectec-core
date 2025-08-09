(* Cache entry for relation and function invocations *)

module Entry = struct
  type t = string * Value.t list

  let equal (id_a, values_a) (id_b, values_b) =
    id_a = id_b
    && List.compare (fun v_a v_b -> Value.compare v_a v_b) values_a values_b = 0

  let hash = Hashtbl.hash
end

(* LFU (with LRU tiebreak) cache over Entry keys *)

module LFU : sig
  type 'a t

  val create : capacity:int -> 'a t
  val clear : 'a t -> unit
  val length : 'a t -> int
  val mem : 'a t -> Entry.t -> bool
  val find : 'a t -> Entry.t -> 'a option
  val add : 'a t -> Entry.t -> 'a -> unit
  val remove : 'a t -> Entry.t -> unit
end = struct
  module KeyTbl = Hashtbl.Make (Entry)

  let maxf = 255

  type 'a node = {
    key : Entry.t;
    mutable value : 'a;
    mutable freq : int;
    mutable prev : 'a node option;
    mutable next : 'a node option;
  }

  type 'a dlist = {
    mutable head : 'a node option; (* MRU *)
    mutable tail : 'a node option; (* LRU *)
    mutable size : int;
  }

  let dlist_create () = { head = None; tail = None; size = 0 }

  let dlist_add_front lst (n : 'a node) =
    n.prev <- None;
    n.next <- lst.head;
    (match lst.head with
    | Some h -> h.prev <- Some n
    | None -> lst.tail <- Some n);
    lst.head <- Some n;
    lst.size <- lst.size + 1

  let dlist_remove lst (n : 'a node) =
    (match n.prev with
    | Some p -> p.next <- n.next
    | None -> lst.head <- n.next);
    (match n.next with
    | Some q -> q.prev <- n.prev
    | None -> lst.tail <- n.prev);
    n.prev <- None;
    n.next <- None;
    if lst.size > 0 then lst.size <- lst.size - 1

  let dlist_pop_tail lst =
    match lst.tail with
    | None -> None
    | Some n ->
        dlist_remove lst n;
        Some n

  type 'a t = {
    capacity : int;
    capacity_one : int;
    key_tbl : 'a node KeyTbl.t;
    freq_tbl : (int, 'a dlist) Hashtbl.t;
    mutable size : int;
    mutable min_freq : int;
  }

  let create ~capacity =
    {
      capacity;
      capacity_one = max 1 (capacity / 4);
      key_tbl = KeyTbl.create (max 1 (capacity * 2));
      freq_tbl = Hashtbl.create 128;
      size = 0;
      min_freq = 0;
    }

  let clear c =
    KeyTbl.reset c.key_tbl;
    Hashtbl.reset c.freq_tbl;
    c.size <- 0;
    c.min_freq <- 0

  let length c = c.size
  let mem c k = KeyTbl.mem c.key_tbl k

  let get_bucket c f =
    match Hashtbl.find_opt c.freq_tbl f with
    | Some b -> b
    | None ->
        let b = dlist_create () in
        Hashtbl.replace c.freq_tbl f b;
        b

  let promote c (n : 'a node) =
    let oldf = n.freq in
    let oldb =
      match Hashtbl.find_opt c.freq_tbl oldf with
      | Some b -> b
      | None -> invalid_arg "LFU.promote: missing old frequency bucket"
    in
    dlist_remove oldb n;
    if oldf = c.min_freq && oldb.size = 0 then
      c.min_freq <- min maxf (c.min_freq + 1);
    let f' = if oldf < maxf then oldf + 1 else maxf in
    n.freq <- f';
    dlist_add_front (get_bucket c f') n

  let find c k =
    match KeyTbl.find_opt c.key_tbl k with
    | None -> None
    | Some n ->
        promote c n;
        Some n.value

  let evict_if_full c =
    if c.size < c.capacity || c.capacity = 0 then ()
    else
      let b = get_bucket c c.min_freq in
      match dlist_pop_tail b with
      | None -> ()
      | Some victim ->
          KeyTbl.remove c.key_tbl victim.key;
          c.size <- c.size - 1

  let add c k v =
    if c.capacity = 0 then ()
    else
      match KeyTbl.find_opt c.key_tbl k with
      | Some n ->
          n.value <- v;
          promote c n
      | None ->
          evict_if_full c;
          if c.size < c.capacity then (
            let b1 = get_bucket c 1 in
            while b1.size >= c.capacity_one do
              match dlist_pop_tail b1 with
              | Some victim ->
                  KeyTbl.remove c.key_tbl victim.key;
                  c.size <- c.size - 1;
                  if b1.size = 0 then Hashtbl.remove c.freq_tbl 1
              | None -> ()
            done;
            let n =
              { key = k; value = v; freq = 1; prev = None; next = None }
            in
            KeyTbl.replace c.key_tbl k n;
            let b1 = get_bucket c 1 in
            dlist_add_front b1 n;
            c.min_freq <- 1;
            c.size <- c.size + 1)

  let remove c k =
    match KeyTbl.find_opt c.key_tbl k with
    | None -> ()
    | Some n ->
        let b = get_bucket c n.freq in
        dlist_remove b n;
        KeyTbl.remove c.key_tbl n.key;
        c.size <- c.size - 1
end

(* Cache targets *)

let is_cached_func = function
  | "subst_type" | "subst_typeDef" | "specialize_typeDef" | "canon"
  | "free_type" | "is_nominal_typeIR" | "bound_tids" | "merge_constraint"
  | "merge_constraint'" | "find_matchings" | "nestable_struct"
  | "nestable_struct_in_header" ->
      true
  | _ -> false

let is_cached_rule = function
  | "Sub_expl" | "Sub_expl_canon" | "Sub_expl_canon_neq" | "Sub_impl"
  | "Sub_impl_canon" | "Sub_impl_canon_neq" | "Type_wf" | "Type_alpha" ->
      true
  | _ -> false
