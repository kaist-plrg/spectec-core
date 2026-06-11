impty CLI: instruction-coverage full against hello.imp.

  $ SPEC=../../specs/impty/base/spec.spectec
  $ HELLO=../../testdata/interp/impty/base/hello.imp

Full instruction coverage emits a GCOV-style annotated spec over the SL
instructions:

  $ spectec impty eval --spec $SPEC -p $HELLO --color never --sl --instruction-coverage.level full
  
  === SL Node Coverage ===
  
  SL Instructions: 65/152 (42.76%)
  
  def $lookup:
      2   Case on pair<K, V>*
      -     Case (% matches pattern []):
   ####       Return ?()
      -     Case (% matches pattern _ :: _):
      2       Let (K_h -> V_h) :: (K_t -> V_t)* = pair<K, V>*
      2         If ((K_h = K_query))
      2           Return ?(V_h)
   ####   If ((pair<K, V>* matches pattern _ :: _))
   ####     Let (K_h -> V_h) :: (K_t -> V_t)* = pair<K, V>*
   ####       Return $lookup<K, V>((K_t -> V_t)*, K_query)
  
  relation Check_expr:
      4   Case on expr
      -     Case (% has type literal):
      2       Let literal = (expr as literal)
      2         Case on literal
      -           Case (% matches pattern `_NUM %`):
      2             Let (i) = literal
      2               Result (int)
      -           Case (% matches pattern `_BOOL %`):
   ####             Let (b) = literal
   ####               Result (bool)
      -     Case (% has type id):
      1       Let x = (expr as id)
      1         Let type? = $lookup<id, type>(tenv, x)
      1           If ((type? matches pattern (_)))
      1             Let ?(t) = type?
      1               Result t
      1   Case on expr
      -     Case (% matches pattern `! %`):
   ####       Let (! e) = expr
   ####         Check_expr: tenv |- e : type
   ####           If ((type matches pattern `BOOL`))
   ####             Result (bool)
      -     Case (% matches pattern `% + %`):
   ####       Let (e_l + e_r) = expr
   ####         Check_expr: tenv |- e_l : type
   ####           If ((type matches pattern `INT`))
   ####             Check_expr: tenv |- e_r : type'
   ####               If ((type' matches pattern `INT`))
   ####                 Result (int)
      -     Case (% matches pattern `% <= %`):
      1       Let (e_l <= e_r) = expr
      1         Check_expr: tenv |- e_l : type
      1           If ((type matches pattern `INT`))
      1             Check_expr: tenv |- e_r : type'
      1               If ((type' matches pattern `INT`))
      1                 Result (bool)
      -     Case (% matches pattern `% && %`):
   ####       Let (e_l && e_r) = expr
   ####         Check_expr: tenv |- e_l : type
   ####           If ((type matches pattern `BOOL`))
   ####             Check_expr: tenv |- e_r : type'
   ####               If ((type' matches pattern `BOOL`))
   ####                 Result (bool)
  
  relation Check_command:
      3   Case on command
      -     Case (% matches pattern `SKIP`):
   ####       Result tenv
      -     Case (% matches pattern `% % = %`):
      2       Let (t x = e) = command
      2         Check_expr: tenv |- e : type
      2           If ((type = t))
      2             Result (x -> t) :: tenv
      -     Case (% matches pattern `% = %`):
   ####       Let (x = e) = command
   ####         Check_expr: tenv |- e : t
   ####           If (($lookup<id, type>(tenv, x) = ?(t)))
   ####             Result tenv
      -     Case (% matches pattern `IF % THEN % ELSE % END`):
   ####       Let (if e then c_1 else c_2 end) = command
   ####         Check_expr: tenv |- e : type
   ####           If ((type matches pattern `BOOL`))
   ####             Check_command: tenv |- c_1 -| tenv_1
   ####               Check_command: tenv |- c_2 -| tenv_2
   ####                 Result tenv
      -     Case (% matches pattern `WHILE % DO % END`):
   ####       Let (while e do c end) = command
   ####         Check_expr: tenv |- e : type
   ####           If ((type matches pattern `BOOL`))
   ####             Check_command: tenv |- c -| tenv_1
   ####               Result tenv
      -     Case (% matches pattern `% ; %`):
      1       Let (c_1 ; c_2) = command
      1         Check_command: tenv |- c_1 -| tenv_1
      1           Check_command: tenv_1 |- c_2 -| tenv_2
      1             Result tenv_2
  
  relation Check_prog:
      1   Check_command: [] |- command -| tenv
      1     Relation holds
  
  relation Eval_expr:
      4   Case on expr
      -     Case (% has type literal):
      2       Let literal = (expr as literal)
      2         Case on literal
      -           Case (% matches pattern `_NUM %`):
      2             Let (i) = literal
      2               Result (i)
      -           Case (% matches pattern `_BOOL %`):
   ####             Let (b) = literal
   ####               Result (b)
      -     Case (% has type id):
      1       Let x = (expr as id)
      1         Let value? = $lookup<id, value>(env, x)
      1           If ((value? matches pattern (_)))
      1             Let ?(v) = value?
      1               Result v
      1   Case on expr
      -     Case (% matches pattern `! %`):
   ####       Let (! e) = expr
   ####         Eval_expr: env |- e ==> literal
   ####           If ((literal matches pattern `_BOOL %`))
   ####             Let (b_e) = literal
   ####               Let b = ~b_e
   ####                 Result (b)
      -     Case (% matches pattern `% + %`):
   ####       Let (e_l + e_r) = expr
   ####         Eval_expr: env |- e_l ==> literal
   ####           If ((literal matches pattern `_NUM %`))
   ####             Let (i_l) = literal
   ####               Eval_expr: env |- e_r ==> literal'
   ####                 If ((literal' matches pattern `_NUM %`))
   ####                   Let (i_r) = literal'
   ####                     Let i = (i_l + i_r)
   ####                       Result (i)
      -     Case (% matches pattern `% <= %`):
      1       Let (e_l <= e_r) = expr
      1         Eval_expr: env |- e_l ==> literal
      1           If ((literal matches pattern `_NUM %`))
      1             Let (i_l) = literal
      1               Eval_expr: env |- e_r ==> literal'
      1                 If ((literal' matches pattern `_NUM %`))
      1                   Let (i_r) = literal'
      1                     Let b = (i_l <= i_r)
      1                       Result (b)
      -     Case (% matches pattern `% && %`):
   ####       Let (e_l && e_r) = expr
   ####         Eval_expr: env |- e_l ==> literal
   ####           If ((literal matches pattern `_BOOL %`))
   ####             Let (b_l) = literal
   ####               Eval_expr: env |- e_r ==> literal'
   ####                 If ((literal' matches pattern `_BOOL %`))
   ####                   Let (b_r) = literal'
   ####                     Let b = (b_l /\ b_r)
   ####                       Result (b)
  
  relation Eval_command:
      3   Case on command
      -     Case (% matches pattern `SKIP`):
   ####       Result env
      -     Case (% matches pattern `% % = %`):
      2       Let (t x = e) = command
      2         Eval_expr: env |- e ==> v
      2           Result (x -> v) :: env
      -     Case (% matches pattern `% = %`):
   ####       Let (x = e) = command
   ####         Eval_expr: env |- e ==> v
   ####           Result (x -> v) :: env
      -     Case (% matches pattern `IF % THEN % ELSE % END`):
   ####       Let (if e then c_1 else c_2 end) = command
   ####         Eval_expr: env |- e ==> literal
   ####           Case on literal
      -             Case (% = (true)):
   ####               Eval_command: env |- c_1 -| env_1
   ####                 Result env_1
      -             Case (% = (false)):
   ####               Eval_command: env |- c_2 -| env_2
   ####                 Result env_2
      -     Case (% matches pattern `WHILE % DO % END`):
   ####       Let (while e do c end) = command
   ####         Eval_expr: env |- e ==> literal
   ####           Case on literal
      -             Case (% = (false)):
   ####               Result env
      -             Case (% = (true)):
   ####               Eval_command: env |- c -| env_1
   ####                 Eval_command: env_1 |- (while e do c end) -| env_2
   ####                   Result env_2
      -     Case (% matches pattern `% ; %`):
      1       Let (c_1 ; c_2) = command
      1         Eval_command: env |- c_1 -| env_1
      1           Eval_command: env_1 |- c_2 -| env_2
      1             Result env_2
  
  relation Eval_prog:
      1   Eval_command: [] |- command -| env
      1     Result env
  
  relation Run_prog:
      1   If (Check_prog: |- command holds)
      1     Eval_prog: |- command -| env
      1       Result env
  
  relation Check_value:
   ####   Case on literal
      -     Case (% matches pattern `_NUM %`):
   ####       Let (int) = literal
   ####         Result (int)
      -     Case (% matches pattern `_BOOL %`):
   ####       Let (bool) = literal
   ####         Result (bool)
  [
    y -> true,
    x -> 5
  ]
