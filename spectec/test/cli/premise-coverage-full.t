impty CLI: premise-coverage full against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Full premise coverage emits a GCOV-style annotated spec with per-premise counts:

  $ spectec impty typecheck --spec $SPEC -p $HELLO --color never --premise-coverage.level full
  
  === IL Node Coverage ===
  
  IL Premises: 35/147 attempted (23.81%), 27/147 succeeded (18.37%)
  38 rel premises
  60 if-premises: succeeded 12/60 (20.00%), failed 11/60 (18.33%), neither 40/60 (66.67%), total 23/120 (19.17%)
  
  def $lookup:
        clause 0:
     0: ####/   1     -- if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches []
        ####          ?()
        clause 1:
     1:    1/####     -- if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _
     2:    1         -- let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} = pair<K, V>*{pair<K, V> <- pair<K, V>*}
     3:    1/####     -- if (K_h = K_query)
           1          ?(V_h)
        clause 2:
     4: ####/####     -- if pair<K, V>*{pair<K, V> <- pair<K, V>*} matches _ :: _
     5: ####         -- let K_h -> V_h :: K_t -> V_t*{K_t <- K_t*, V_t <- V_t*} = pair<K, V>*{pair<K, V> <- pair<K, V>*}
     6: ####         -- otherwise
        ####          $lookup<K, V>(K_t -> V_t*{K_t <- K_t*, V_t <- V_t*}, K_query)
  
  relation Check_expr:
        rule num:
     7:    2/   2     -- if expr <: literal
     8:    2         -- let literal = expr as literal
     9:    2/####     -- if literal matches `_NUM %`
    10:    2         -- let i = literal
           2          tenv |- expr : int
        rule boollit:
    11: ####/   2     -- if expr <: literal
    12: ####         -- let literal = expr as literal
    13: ####/####     -- if literal matches `_BOOL %`
    14: ####         -- let b = literal
        ####          tenv |- expr : bool
        rule id:
    15:    1/   1     -- if expr <: id
    16:    1         -- let x = expr as id
    17:    1         -- let type?{type <- type?} = $lookup<id, type>(tenv, x)
    18:    1/####     -- if type?{type <- type?} matches (_)
    19:    1         -- let ?(t) = type?{type <- type?}
           1          tenv |- expr : t
        rule not:
    20: ####/   1     -- if expr matches `! %`
    21: ####         -- let ! e = expr
    22: ####/####     -- rel Check_expr: tenv |- e : type
    23: ####/####     -- if type matches `BOOL`
        ####          tenv |- expr : bool
        rule add:
    24: ####/   1     -- if expr matches `% + %`
    25: ####         -- let e_l + e_r = expr
    26: ####/####     -- rel Check_expr: tenv |- e_l : type
    27: ####/####     -- if type matches `INT`
    28: ####/####     -- rel Check_expr: tenv |- e_r : type'
    29: ####/####     -- if type' matches `INT`
        ####          tenv |- expr : int
        rule leq:
    30:    1/####     -- if expr matches `% <= %`
    31:    1         -- let e_l <= e_r = expr
    32:    1/####     -- rel Check_expr: tenv |- e_l : type
    33:    1/####     -- if type matches `INT`
    34:    1/####     -- rel Check_expr: tenv |- e_r : type'
    35:    1/####     -- if type' matches `INT`
           1          tenv |- expr : bool
        rule and:
    36: ####/####     -- if expr matches `% && %`
    37: ####         -- let e_l && e_r = expr
    38: ####/####     -- rel Check_expr: tenv |- e_l : type
    39: ####/####     -- if type matches `BOOL`
    40: ####/####     -- rel Check_expr: tenv |- e_r : type'
    41: ####/####     -- if type' matches `BOOL`
        ####          tenv |- expr : bool
  
  relation Check_command:
        rule skip:
    42: ####/   3     -- if command matches `SKIP`
        ####          tenv |- command -| tenv
        rule decl:
    43:    2/   1     -- if command matches `% % = %`
    44:    2         -- let t x = e = command
    45:    2/####     -- rel Check_expr: tenv |- e : type
    46:    2/####     -- if (type = t)
           2          tenv |- command -| x -> t :: tenv
        rule assign:
    47: ####/   1     -- if command matches `% = %`
    48: ####         -- let x = e = command
    49: ####/####     -- rel Check_expr: tenv |- e : t
    50: ####/####     -- if ($lookup<id, type>(tenv, x) = ?(t))
        ####          tenv |- command -| tenv
        rule ite:
    51: ####/   1     -- if command matches `IF % THEN % ELSE % END`
    52: ####         -- let if e then c_1 else c_2 end = command
    53: ####/####     -- rel Check_expr: tenv |- e : type
    54: ####/####     -- if type matches `BOOL`
    55: ####/####     -- rel Check_command: tenv |- c_1 -| tenv_1
    56: ####/####     -- rel Check_command: tenv |- c_2 -| tenv_2
        ####          tenv |- command -| tenv
        rule while:
    57: ####/   1     -- if command matches `WHILE % DO % END`
    58: ####         -- let while e do c end = command
    59: ####/####     -- rel Check_expr: tenv |- e : type
    60: ####/####     -- if type matches `BOOL`
    61: ####/####     -- rel Check_command: tenv |- c -| tenv_1
        ####          tenv |- command -| tenv
        rule seq:
    62:    1/####     -- if command matches `% ; %`
    63:    1         -- let c_1 ; c_2 = command
    64:    1/####     -- rel Check_command: tenv |- c_1 -| tenv_1
    65:    1/####     -- rel Check_command: tenv_1 |- c_2 -| tenv_2
           1          tenv |- command -| tenv_2
  
  relation Check_prog:
        rule :
    66:    1/####     -- rel Check_command: [] |- command -| tenv
           1          |- command
  
  relation Eval_expr:
        rule num:
    67: ####/####     -- if expr <: literal
    68: ####         -- let literal = expr as literal
    69: ####/####     -- if literal matches `_NUM %`
    70: ####         -- let i = literal
        ####          env |- expr ==> i
        rule boollit:
    71: ####/####     -- if expr <: literal
    72: ####         -- let literal = expr as literal
    73: ####/####     -- if literal matches `_BOOL %`
    74: ####         -- let b = literal
        ####          env |- expr ==> b
        rule id:
    75: ####/####     -- if expr <: id
    76: ####         -- let x = expr as id
    77: ####         -- let value?{value <- value?} = $lookup<id, value>(env, x)
    78: ####/####     -- if value?{value <- value?} matches (_)
    79: ####         -- let ?(v) = value?{value <- value?}
        ####          env |- expr ==> v
        rule not:
    80: ####/####     -- if expr matches `! %`
    81: ####         -- let ! e = expr
    82: ####/####     -- rel Eval_expr: env |- e ==> literal
    83: ####/####     -- if literal matches `_BOOL %`
    84: ####         -- let b_e = literal
    85: ####         -- let b = ~b_e
        ####          env |- expr ==> b
        rule add:
    86: ####/####     -- if expr matches `% + %`
    87: ####         -- let e_l + e_r = expr
    88: ####/####     -- rel Eval_expr: env |- e_l ==> literal
    89: ####/####     -- if literal matches `_NUM %`
    90: ####         -- let i_l = literal
    91: ####/####     -- rel Eval_expr: env |- e_r ==> literal'
    92: ####/####     -- if literal' matches `_NUM %`
    93: ####         -- let i_r = literal'
    94: ####         -- let i = (i_l + i_r)
        ####          env |- expr ==> i
        rule leq:
    95: ####/####     -- if expr matches `% <= %`
    96: ####         -- let e_l <= e_r = expr
    97: ####/####     -- rel Eval_expr: env |- e_l ==> literal
    98: ####/####     -- if literal matches `_NUM %`
    99: ####         -- let i_l = literal
   100: ####/####     -- rel Eval_expr: env |- e_r ==> literal'
   101: ####/####     -- if literal' matches `_NUM %`
   102: ####         -- let i_r = literal'
   103: ####         -- let b = (i_l <= i_r)
        ####          env |- expr ==> b
        rule and:
   104: ####/####     -- if expr matches `% && %`
   105: ####         -- let e_l && e_r = expr
   106: ####/####     -- rel Eval_expr: env |- e_l ==> literal
   107: ####/####     -- if literal matches `_BOOL %`
   108: ####         -- let b_l = literal
   109: ####/####     -- rel Eval_expr: env |- e_r ==> literal'
   110: ####/####     -- if literal' matches `_BOOL %`
   111: ####         -- let b_r = literal'
   112: ####         -- let b = (b_l /\ b_r)
        ####          env |- expr ==> b
  
  relation Eval_command:
        rule skip:
   113: ####/####     -- if command matches `SKIP`
        ####          env |- command -| env
        rule decl:
   114: ####/####     -- if command matches `% % = %`
   115: ####         -- let t x = e = command
   116: ####/####     -- rel Eval_expr: env |- e ==> v
        ####          env |- command -| x -> v :: env
        rule assign:
   117: ####/####     -- if command matches `% = %`
   118: ####         -- let x = e = command
   119: ####/####     -- rel Eval_expr: env |- e ==> v
        ####          env |- command -| x -> v :: env
        rule ite-true:
   120: ####/####     -- if command matches `IF % THEN % ELSE % END`
   121: ####         -- let if e then c_1 else c_2 end = command
   122: ####/####     -- rel Eval_expr: env |- e ==> literal
   123: ####/####     -- if (literal = true)
   124: ####/####     -- rel Eval_command: env |- c_1 -| env_1
        ####          env |- command -| env_1
        rule ite-false:
   125: ####/####     -- if command matches `IF % THEN % ELSE % END`
   126: ####         -- let if e then c_1 else c_2 end = command
   127: ####/####     -- rel Eval_expr: env |- e ==> literal
   128: ####/####     -- if (literal = false)
   129: ####/####     -- rel Eval_command: env |- c_2 -| env_2
        ####          env |- command -| env_2
        rule while-false:
   130: ####/####     -- if command matches `WHILE % DO % END`
   131: ####         -- let while e do c end = command
   132: ####/####     -- rel Eval_expr: env |- e ==> literal
   133: ####/####     -- if (literal = false)
        ####          env |- command -| env
        rule while-true:
   134: ####/####     -- if command matches `WHILE % DO % END`
   135: ####         -- let while e do c end = command
   136: ####/####     -- rel Eval_expr: env |- e ==> literal
   137: ####/####     -- if (literal = true)
   138: ####/####     -- rel Eval_command: env |- c -| env_1
   139: ####/####     -- rel Eval_command: env_1 |- while e do c end -| env_2
        ####          env |- command -| env_2
        rule seq:
   140: ####/####     -- if command matches `% ; %`
   141: ####         -- let c_1 ; c_2 = command
   142: ####/####     -- rel Eval_command: env |- c_1 -| env_1
   143: ####/####     -- rel Eval_command: env_1 |- c_2 -| env_2
        ####          env |- command -| env_2
  
  relation Eval_prog:
        rule :
   144: ####/####     -- rel Eval_command: [] |- command -| env
        ####          |- command -| env
  
  relation Run_prog:
        rule :
   145: ####         -- if Check_prog: |- command holds
   146: ####/####     -- rel Eval_prog: |- command -| env
        ####          |- command -| env
  Typecheck succeeded
