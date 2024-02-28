open Frontend

let program_dir = "test/program"
let arch_dir = "test/arch"

let () =
  let files = Sys.readdir program_dir in
  let total = Array.length files in
  Array.iteri
    (fun count file ->
      let file = program_dir ^ "/" ^ file in
      try Parse.parse_file arch_dir file |> ignore with
      | _ -> 
          Printf.sprintf "[%d/%d] %s" (count + 1) total file
          |> print_endline)
    files;
