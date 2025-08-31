# \_ok Relations

## Expr

### Expr_ok

#### `BinE`

* [FEASIBLE] Phantom#811

```
4. If binop is in [(DIV), (MOD)], then
  6. Let (( _typ ; ctk' )) be $annot(exprIL_r'')
  7. If (ctk' matches pattern `LCTK`), then
    1. Let n_r be (int as nat)
    2. If (n_r > 0), then
    2. Else Phantom#811
```

Division by zero.

* [FALLTHROUGH] Phantom#813

```
7. If (ctk' matches pattern `LCTK`), then
7. Else Phantom#813
```

If the right operand is not local-compile time known, then it will fall through to the next case.

* [FEASIBLE] Phantom#816

```
5. If binop is in [(SHL), (SHR)], then
  6. If (($is_fintt(typ_r') \/ $is_intt(typ_r')) => (ctk_r' = (LCTK))), then
  6. Else Phantom#816
```

If the right operand of a shift operation is `IntT` or `FIntT`, then it should be local-compile time known.

* [UNREACHABLE] Phantom#823

```
9. If (binop matches pattern `CONCAT`), then
  8. If $is_fbitt(typ_l'), then
  8. Else Phantom#823
```

Unreachable because it was already checked by `$compatible_concat`.

#### `TernE`

* [FEASIBLE] Phantom#807

```
6. If ($is_intt(typ') => ((ctk_c = (CTK)) \/ (ctk_c = (LCTK)))), then
6. Else Phantom#807
```

If both types of true and false case are `IntT`, then the condition must be compile-time known.

#### `CastE`

* [FEASIBLE] Phantom#805

```
1. If (Type_wf: $bound_tids(p', C) |- typ holds), then
1. Else Phantom#805
```

If the cast type is not well-formed.

* [FEASIBLE] Phantom#806

```
3. If (tid* matches pattern []), then
3. Else Phantom#806
```

If the cast type contains `AnyT`.

#### `ArrAccE`

* [FEASIBLE] Phantom#793

```
6. If (ctk matches pattern `LCTK`), then
  2. If (datatyp matches pattern `TupleT%`), then
    2. Eval_static: p' C |- exprIL_i' ~> val_i
    3. Let int be $get_num(val_i)
    4. If (int has type nat), then
    4. Else Phantom#793
```

If indexing a tuple with a negative number.

* [FALLTHROUGH] Phantom#796
* [FALLTHROUGH] Phantom#800

```
6. If (ctk matches pattern `LCTK`), then
6. Else Phantom#796
8. If (ctk matches pattern `LCTK`), then
8. Else Phantom#800
```

Will be handled in the next case.

* [FALLTHROUGH] Phantom#798

```
8. If (ctk matches pattern `LCTK`), then
  2. Let typ be $canon_typ(typ_b)
  3. If (typ has type datatyp), then
    1. Let datatyp be (typ as datatyp)
    2. If (datatyp matches pattern `StackT%%`), then
    2. Else Phantom#798
```

Will be handled in the next case.

* [FEASIBLE] Phantom#801
* [FEASIBLE] Phantom#802

```
10. If ~(ctk_i matches pattern `LCTK`), then
  3. If (typ has type datatyp), then
    2. If (datatyp matches pattern `StackT%%`), then
    2. Else Phantom#801
  3. Else Phantom#802
```

When indexing a non-stack type.

#### `BitAccE`

* [FEASIBLE] Phantom#789

```
3. Let int' be $get_num(val_h)
4. If (int' has type nat), then
4. Else Phantom#789
```

If the high bit of a slice is a negative number.

* [FEASIBLE] Phantom#790
* [FEASIBLE] Phantom#792

```
4. Let (( typ_h ; ctk' )) be $annot(exprIL_h)
5. If (ctk' matches pattern `LCTK`), then
5. Else Phantom#790
```

```
6. Let (( typ_l ; ctk )) be $annot(exprIL_l)
7. If (ctk matches pattern `LCTK`), then
7. Else Phantom#792
```

If the high/low bit of a slice is not local-compile time known.

#### `ErrAccE`

* [FEASIBLE] Phantom#787

```
3. If (?((ErrV member)) = $find_val(p', C, (TOP id))), then
3. Phantom #787
```

If accessing a non-existent member of an error type.

#### `TypeAccE`

* [FEASIBLE] Phantom#781
* [FEASIBLE] Phantom#783

```
2. Else If (datatyp matches pattern `SEnumT%%%`), then
2. If (datatyp matches pattern `EnumT%%`), then
  2. Let val? be $assoc_<member, val>(member, (member_t, val_t)*)
  3. If (val? matches pattern (_)), then
  3. Else Phantom#781, #783
```

If accessing a non-existent member of `EnumT` or `SEnumT`.

* [FEASIBLE] Phantom#782
* [FEASIBLE] Phantom#784

```
2. If (typdef has type monotypdef), then
  3. If (typ' has type datatyp), then
    2. If (datatyp matches pattern `EnumT%%`), then
    2. Else If (datatyp matches pattern `SEnumT%%%`), then
    2. Else Phantom#782
  3. Else Phantom#784
```

If accessing a type member other than `EnumT` or `SEnumT`.

#### `ExprAccE`

* [FALLTHROUGH] Phantom#763
* [FALLTHROUGH] Phantom#764
* [FALLTHROUGH] Phantom#766
* [FALLTHROUGH] Phantom#767
* [FALLTHROUGH] Phantom#769
* [FALLTHROUGH] Phantom#770
* [FALLTHROUGH] Phantom#772
* [FALLTHROUGH] Phantom#773

```
4. If (typ has type datatyp), then
  2. If (datatyp matches pattern `StackT%%`), then
  2. Else Phantom#763, #766, #769, #772
4. Else Phantom#764, #767, #770, #773
```

When "next" of a non-stack type is accessed, it will fall through to the other cases.
When "last" of a non-stack type is accessed, it will fall through to the other cases.
When "lastIndex" of a non-stack type is accessed, it will fall through to the other cases.
When "size" of a non-stack type is accessed, it will fall through to the other cases.

* [FEASIBLE] Phantom#774
* [FEASIBLE] Phantom#777
* [FEASIBLE] Phantom#779

```
2. If (synthtyp matches pattern `TableStructT%%`), then
2. Else If (datatyp matches pattern `UnionT%%`), then
2. Else If (datatyp matches pattern `HeaderT%%`), then
  2. Let typ? be $assoc_<member, typ>(member, (member_t, typ_t)*)
  3. If (typ? matches pattern (_)), then
  3. Else Phantom#774, #777
```

If accessing a non-existent member of a table struct, union, header type.

* [FEASIBLE] Phantom#775

```
2. If (synthtyp matches pattern `TableStructT%%`), then
2. Phantom#775
```

If accessing a non-table-struct synthesized type.

* [FEASIBLE] Phantom#776

```
7. If (typ' has type datatyp), then
7. Else If (typ' has type synthtyp), then
7. Else Phantom#776
```

If accessing a non-data and non-synthesized type.

#### `SelectE`

* [UNREACHABLE-PARSER] Phantom#828

```
2. If (C.LOCAL.KIND matches pattern `PARSERSTATE`), then
2. Else Phantom#828
```

Unreachable as `SelectE` is only used in parser state, enforced by the grammar.

#### `CallTypeE`

* [UNREACHABLE] Phantom#747
* [UNREACHABLE] Phantom#748

```
1. If (targIL* matches pattern []), then
  1. If (argIL* matches pattern []), then
  1. Else Phantom#747
1. Else Phantom#748
```

Unreachable as `targ` and `arg` were already checked of `[]`.

* [UNREACHABLE] Phantom#749

```
3. If (typ = ((IntT) as typ)), then
3. Else Phantom#749
```

Unreachable as `CallTypeE` is defined on builtin methods that return `IntT` only.

* [FEASIBLE] Phantom#750

```
2. If member is in ["minSizeInBits", "minSizeInBytes", "maxSizeInBits", "maxSizeInBytes"], then
2. Else Phantom#750
```

If a method is not one of the four methods.

* [FEASIBLE] Phantom#751

```
1. Let typdef? be $find_typdef(p', C, name)
2. If (typdef? matches pattern (_)), then
2. Else Phantom#751
```

If a call type method on an unknown type.

* [FEASIBLE] Phantom#752

```
1. If (arg* matches pattern []), then
1. Else Phantom#752
```

If a call type method has arguments.

* [UNREACHABLE-PARSER] Phantom#753

```
2. If (targ* matches pattern []), then
2. Else Phantom#753
```

Unreachable as the grammar disallows type arguments.

#### `CallMethodE`

##### If builtin method call

* [UNREACHABLE] Phantom#754
* [UNREACHABLE] Phantom#755

```
1. If (targIL* matches pattern []), then
  1. If (argIL* matches pattern []), then
  1. Else Phantom#754
1. Else Phantom#755
```

Unreachable as `targ` and `arg` were already checked of `[]`.

* [UNREACHABLE] Phantom#756

```
3. If (typ = ((IntT) as typ)), then
3. Else Phantom#756
```

Unreachable as builtin methods here return `IntT` only.

* [FALLTHROUGH] Phantom#758

```
1. If (arg* matches pattern []), then
1. Else Phantom#758
```

The else case will fall through to non-builtin method call.

##### If not builtin method call

* [FEASIBLE] Phantom#760

```
6. If (typ =/= ((VoidT) as typ)), then
6. Else Phantom#760
```

If a method call returns `VoidT`.

## Stmt

### CParam_ok

* [FEASIBLE] Phantom#871

```
1. If (Sub_impl: typ_e << typ_p holds), then ...
1. Else Phantom#871
```

If the default constructor parameter value is local-compile time known but is not a subtype of the parameter type.

* [FEASIBLE] Phantom#872

```
3. If (ctk matches pattern `LCTK`), then ...
3. Else Phantom#872
```

If the default constructor parameter value is not local-compile time known.

* [FEASIBLE] Phantom#873

```
4. If (Type_wf: tidset |- typ_p holds), then ...
4. Else Phantom#873

```

If the constructor parameter type is not well-formed.

### Stmt_ok

* [UNREACHABLE] Phantom#865

```
2. Else Phantom#865
```

Unreachable because `Stmt_ok` is total.

#### `TransS`

* [FEASIBLE] Phantom#840

```
4. If (typ = ((StateT) as typ)), then
4. Else Phantom#840
```

When transitioning to a non-state type.

#### `CallInstS`

* [UNREACHABLE-PARSER] Phantom#841

```
1. If (text = "apply"), then
1. Else Phantom#841
```

If the method is not `apply`.

* [UNREACHABLE] Phantom#842
* [UNREACHABLE] Phantom#843
* [UNREACHABLE] Phantom#844

```
3. Stmt_ok: p' C' f |- (CallMethodS (NameE (CURRENT id)) "apply" targ* arg*) : context f' stmtIL
  4. If (context = C'), then
    1. Let stmtIL' be stmtIL
    2. If (stmtIL' matches pattern `CallMethodS%%%%`), then
      2. If (exprIL matches pattern `NameE%%`), then
        1. Let (NameE name' _annotIL) be exprIL
        2. If (name' = (CURRENT id)), then
        2. Else Phantom#842
      2. Else Phantom#843
    2. Else Phantom#844
```

Unreachable because `CallInstS` uses `CallMethodS` as a subroutine.

* [MISTAKE] Phantom#845

```
4. If (context = C'), then
4. Else Phantom#845
```

Should revise the spec.

* [FEASIBLE] Phantom#846

```
2. If ($is_parsert(typ') \/ $is_controlt(typ')), then
2. Else Phantom#846
```

If direct invocation was used on a non-parser or non-control type.

* [UNREACHABLE] Phantom#847
* [UNREACHABLE] Phantom#848
* [UNREACHABLE] Phantom#849
* [UNREACHABLE] Phantom#850

```
3. If (tid* matches pattern []), then
  1. If (id* matches pattern []), then
    2. If (targIL* matches pattern []), then
      1. If (argIL* matches pattern []), then
      1. Else Phantom#847
    2. Else Phantom#848
  1. Else Phantom#849
3. Else Phantom#850
```

Instantiated with zero type arguments and arguments, so should also produce zero type arguments and arguments.
Same goes for inferred type arguments and default parameters.

#### `SwitchS`

* [FEASIBLE] Phantom#858

```
2. If (synthtyp matches pattern `TableEnumT%%`), then
2. Else Phantom#858
```

If switching on a non-table synthesized type.

* [FEASIBLE] Phantom#860

```
5. If (((($is_errt(typ') \/ $is_fintt(typ')) \/ $is_fbitt(typ')) \/ $is_enumt(typ')) \/ $is_senumt(typ')), then
  2. If $distinct_<switchlabel>(switchlabel*), then
  2. Else Phantom#860
```

If switching on a non-table type has duplicate labels.

### Block_ok

```
relation Block_ok: cursor, C, f, blkctxt, (BlockB stmt*)
```

* [UNREACHABLE] Phantom#838

```
1. If (cursor matches pattern `LOCAL`), then ...
2. Else Phantom#838
```

This is unreachable as `Block_ok` is never invoked with a `cursor` other than `LOCAL`.

## Decl

### Decl_ok

```
relation Decl_ok: p', C'''', decl''
```

#### Constant

* [FEASIBLE] Phantom#897

```
2. Type_ok: p' C'''' |- type : typ_c tid*
3. If (tid* matches pattern []), then
  1. If (Type_wf: $bound_tids(p', C'''') |- typ_c holds), then
  1. Else Phantom#897
```

If a constant type is not well-formed.

* [FEASIBLE] Phantom#898

```
2. Type_ok: p' C'''' |- type : typ_c tid*
3. If (tid* matches pattern []), then
3. Else Phantom#898
```

If a constant's type contains `AnyT`.

#### Variable

* [FEASIBLE] Phantom#892
* [FEASIBLE] Phantom#895

```
2. If (expr? matches pattern ()), then
  2. If (tid* matches pattern []), then
  3. Else Phantom#895
2. Else If (expr? matches pattern (_)), then
  3. If (tid* matches pattern []), then
  3. Else Phantom#892
```

If a variable's type contains `AnyT`.

```p4
struct S<T> { T t; }
S<_> s;
```

#### Instantiation

* [UNREACHABLE] Phantom#883

```
2. If (abstyp matches pattern `SpecT%%`), then
  2. If (typ'' has type objtyp), then
    1. Let objtyp be (typ'' as objtyp)
      2. If (objtyp matches pattern `ExternT%%`), then
      9. Let funcdef* be fd_ext*
      10. If ((funcdef has type polyfuncdef))*, then
      10. Else Phantom#883
```

Unreachable as all function definitions inside an extern object are generic.

* [FEASIBLE] Phantom#884
* [FEASIBLE] Phantom#885
* [FEASIBLE] Phantom#886
* [FEASIBLE] Phantom#887

```
6. If (typ''' has type abstyp), then
  2. If (abstyp matches pattern `SpecT%%`), then
    2. If (typ'' has type objtyp), then
      2. If (objtyp matches pattern `ExternT%%`), then
      2. Else Phantom#884
    2. Else Phantom#885
  2. Else Phantom#886
6. Else Phantom#887
```

If object initializer was supplied for an instantiation of non-extern type.

#### Action

* [MAY-UNREACHABLE] Phantom#880

```
2. If ((p' = (GLOBAL)) \/ ((p' = (BLOCK)) /\ (C''''.BLOCK.KIND = (CONTROL)))), then
  3. (Param_ok: (LOCAL) C_1 |- param : paramIL tid*)*
  4. If ((tid* matches pattern []))*, then
  4. Else Phantom#880
```

If an action's parameter type contains `AnyT`.

```p4
struct S<T> { T t; }
action a(S<_> s) {}
```

This is allowed in `p4test`, but do we really want to allow such?
It is essentially quite similar to allowing generic actions.

* [UNREACHABLE-PARSER] Phantom#881

```
2. If ((p' = (GLOBAL)) \/ ((p' = (BLOCK)) /\ (C''''.BLOCK.KIND = (CONTROL)))), then
2. Else Phantom#881
```

If an action was not declared in the global scope or a control block.
But this should have been rejected by the parser already.

#### Value Set

* [FEASIBLE] Phantom#876

```
2. If ((p' = (GLOBAL)) \/ ((p' = (BLOCK)) /\ (C''''.BLOCK.KIND = (PARSER)))), then
  1. Type_ok: p' C'''' |- type : typ_s tid*
  2. If (tid* matches pattern []), then
  2. Else Phantom#876
```

If a value set's underlying type was `AnyT`.

* [UNREACHABLE-PARSER] Phantom#877

```
2. If ((p' = (GLOBAL)) \/ ((p' = (BLOCK)) /\ (C''''.BLOCK.KIND = (PARSER)))), then
2. Else Phantom#877
```

If a value set was not declared in a parser block.
But this should have been rejected by the parser already.

#### Error

* [FEASIBLE] Phantom#952

```
2. If $distinct_<member>(member*), then
2. Else Phantom#952
```

If an error's members are not distinct.

#### Match kind

* [FEASIBLE] Phantom#951

```
2. If $distinct_<member>(member*), then
2. Else Phantom#951
```

If a match kind's members are not distinct.

#### Union

* [FEASIBLE] Phantom#948

```
7. If (TypeDef_wf: $bound_tids((GLOBAL), C'''') |- td holds), then
7. Else Phantom#948
```

If a union type is not well-formed.

#### Enum

* [UNREACHABLE] Phantom#946

```
8. If (TypeDef_wf: tidset |- td holds), then
8. Else Phantom#946
```

If an enum type is not well-formed.
But this is unreachable as its well-formedness is already ensured by the declaration typing rule.

* [FEASIBLE] Phantom#947

```
2. If $distinct_<member>(member*), then
2. Else Phantom#947
```

If an enum's members are not distinct.

#### Serializable Enum

* [FEASIBLE] Phantom#941

```
6. If (TypeDef_wf: $bound_tids((GLOBAL), C'''') |- td holds), then
6. Else Phantom#941
```

If a serializable enum type is not well-formed.
Can use nesting rule to make an ill-formed serializable enum type.

* [UNREACHABLE] Phantom#942

```
2. If ((member' = member))*, then
2. Else Phantom#942
```

`SEnum_fields_ok` folds over the enum members, so it should collect the same members.

* [FEASIBLE] Phantom#943

```
1. If (Type_wf: $bound_tids((GLOBAL), C'''') |- typ holds), then
1. Else Phantom#943
```

If a serializable enum's underlying type is not well-formed.

* [FEASIBLE] Phantom#944

```
2. If (tid* matches pattern []), then
2. Else Phantom#944
```

If a serializable enum's underlying type contains `AnyT`.

* [FEASIBLE] Phantom#945

```
2. If $distinct_<member>(member*), then
2. Else Phantom#945
```

If a serializable enum's members are not distinct.

#### Type Definition and New Type

* [FEASIBLE] Phantom#927

```
2. Else If (typedef matches pattern `DeclD%`), then
  2. Else If (typdef has type polytypdef), then
    3. If (TypeDef_wf: $bound_tids((GLOBAL), C'''') |- td holds), then
    3. Else Phantom#927, #934
```

If a type definition as declaration is not well-formed.

* [FEASIBLE] Phantom#939

```
1. If (Type_wf: $bound_tids((GLOBAL), C'''') |- typ holds), then
1. Else Phantom#939
```

If a new type definition's type is not well-formed.

* [FEASIBLE] Phantom#940

```
3. If (tid* matches pattern []), then
3. Else Phantom#940
```

If a new type definition's type contains `AnyT`.

* [UNREACHABLE] Phantom#928
* [UNREACHABLE] Phantom#931

```
2. If (typedef matches pattern `TypeD%`), then
  2. Let td be ((MonoD typ_d) as typdef)
  4. If (TypeDef_wf: tidset |- td holds), then
  4. Else Phantom#931
2. Else If (typedef matches pattern `DeclD%`), then
  2. If (typdef has type monotypdef), then
    3. If (TypeDef_wf: $bound_tids((GLOBAL), C'''') |- td holds), then
    3. Else Phantom#928
```

Monotype definition's well-formedness is already implied by the type well-formedness check.

* [UNREACHABLE] Phantom#929
* [UNREACHABLE] Phantom#930

```
2. Else If (typedef matches pattern `DeclD%`), then
  4. If (tid* matches pattern [ _/1 ]), then
    2. Let typdef? be $find_typdef((GLOBAL), C', (CURRENT tid_d))
    3. If (typdef? matches pattern (_)), then
    3. Else Phantom#929
  4. Else Phantom#930
```

Type definition as declaration is always named.

* [FEASIBLE] Phantom#933

```
2. Type_ok: (GLOBAL) C'''' |- type : typ tid*
  3. If (tid* matches pattern []), then
  3. Else Phantom#933
```

If a type definition's type contains `AnyT`.

```p4
extern E<T> { void f(T t); }
typedef E<_> Foo;
```

This seems like a frontend bug!

* [UNREACHABLE] Phantom#936

```
3. If (typdef? matches pattern (_)), then
3. Else Phantom#936
```

Unreachable since new types can only alias primitive types.

* [UNREACHABLE-PARSER] Phantom#934
* [UNREACHABLE-PARSER] Phantom#935
* [UNREACHABLE-PARSER] Phantom#937

```
4. If (tid* matches pattern [ _/1 ]), then
  2. If (typdef has type monotypdef), then
    3. If (TypeDef_wf: $bound_tids((GLOBAL), C'''') |- td holds), then
    3. Else Phantom#935
  2. Else If (typdef has type polytypdef), then
    3. If (TypeDef_wf: $bound_tids((GLOBAL), C'''') |- td holds), then
    3. Else Phantom#934
4. Else Phantom#937
```

Unreachable since new types cannot have nested declarations.
i.e., should fix the definition of `NewTypeD`.

#### Function

* [FEASIBLE] Phantom#924

```
2. If (FuncDef_wf: $bound_tids((GLOBAL), C'''') |- fd holds), then
2. Else Phantom#924
```

If a function's type is not well-formed.

* [FEASIBLE] Phantom#926

```
5. If (tid* matches pattern []), then
5. Else Phantom#926
```

If a function's parameter type contains `AnyT`.

#### Extern Function

* [FEASIBLE] Phantom#923

```
5. If (tid* matches pattern []), then
5. Else Phantom#923
```

If an extern function's parameter type contains `AnyT`.

#### Parser Type

* [FEASIBLE] Phantom#921

```
8. If (TypeDef_wf: $bound_tids((GLOBAL), C'''') |- td holds), then
8. Else Phantom#921
```

If a parser type is not well-formed.

#### Parser

* [FEASIBLE] Phantom#913

```
8. Let cd be (ConsD ([], []) -> (ConsT (cparamIL* as paramtyp*) typ))
9. If (ConsDef_wf: $bound_tids((GLOBAL), C'''') |- cd holds), then
9. Else Phantom#913
```

If a parser's constructor type is not well-formed.

* [FEASIBLE] Phantom#914
* [FEASIBLE] Phantom#915

```
1. If ~"accept " is in statelabel*, then
  1. If ~"reject " is in statelabel*, then
  1. Else Phantom#914
1. Else Phantom#915
```

If a parser's state label contains `accept` or `reject`.
Yet this coverage metric depends on the order of if-premises.
i.e., the negative test suite already covers the below program,

```p4
// accept_e.p4
state accept {  // reserved name
    transition reject;
}
```

but this counts as an ill-typed program that doesn't have a `start` state.

* [FEASIBLE] Phantom#917

```
4. If $distinct_<statelabel>(statelabel*), then
4. Else Phantom#917
```

If a parser's state labels are not distinct.

* [FEASIBLE] Phantom#919

```
3. If ((tid*' matches pattern []))*, then
3. Else Phantom#919
```

If a parser's parameter type contains `AnyT`.

* [FEASIBLE] Phantom#920

```
5. If ((tid* matches pattern []))*, then
5. Else Phantom#919
```

If a parser's constructor parameter type contains `AnyT`.

#### Table

* [UNREACHABLE] Phantom#899

```
2. If (C''''.BLOCK.KIND matches pattern `CONTROL`), then
  1. If (|$keys_of_table(tbl)| <= 1), then
    1. If (|$actions_of_table(tbl)| = 1), then
      3. Table_type_decl_ok: (BLOCK) C_1 tblctx' |- id : C_2 typ_tbl
      4. Let typ be ((TableT id typ_tbl) as typ)
      5. If (Type_wf: $bound_tids((BLOCK), C_2) |- typ holds), then
      5. Else Phantom#899
```

If a table itself was type-checked, its type should be well-formed.

* [FEASIBLE] Phantom#901

```
1. If (decl'' matches pattern `TableD%%`), then
  1. If (|$keys_of_table(tbl)| <= 1), then
  1. Else Phantom#901
```

If a table has more than one key.

* [UNREACHABLE-PARSER] Phantom#902

```
2. If (C''''.BLOCK.KIND matches pattern `CONTROL`), then
2. Else Phantom#902
```

If a table was not declared in a control block.
This should have been rejected by the parser already.

#### Control

* [FEASIBLE] Phantom#911

```
4. (Param_ok: (LOCAL) C_1 |- cparam : cparamIL tid*)*
5. If ((tid* matches pattern []))*, then
5. Else Phantom#911
```

If a control's parameter type contains `AnyT`.

```p4
struct S<T> { T t; }
control c(S<_> s) { apply {} }
```

#### Package Type

* [FEASIBLE] Phantom#906

```
8. Let ptd be (PolyD (tparam*, tparam_hidden*) -> ((PackageT typ_c*) as typ))
9. If (TypeDef_wf: $bound_tids((BLOCK), C_2) |- (ptd as typdef) holds), then
9. Else Phantom#906
```

If a package type was not well-formed.

## Call

### Call_convention_arg_ok

```
relation Call_convention_arg_ok: p, C, actctxt, pt', (argIL, typ')
```

* [FALLTHROUGH] Phantom#962

```
2. If (exprIL? matches pattern (_)), then ...
2. Else Phantom#962
```

If a don't care argument is named.
This case will be handled later in the same relation.

* [UNREACHABLE] Phantom#964

```
1. If (argIL matches pattern `NameA%%`), then ...
1. Else If (argIL matches pattern `AnyA`), then ...
1. Else Phantom#964
```

The remaining case, `ExprA` should have been already handled beforehand.

* [UNREACHABLE] Phantom#965

```
2. If (exprIL? matches pattern ()), then ...
2. Else Phantom#965
```

If a named argument carries an expression, where its corresponding parameter direction is `OUT`.
This should have been already handled beforehand.

### Call_convention_ok

```
relation Call_convention_ok: p, C, actctxt, (id dir typ_p' val?), (exprIL, typ_a')
```

* [FEASIBLE] Phantom#953

```
1. If (Lval_ok: p C |- exprIL holds), then ...
1. Else Phantom#953
```

If an argument for `INOUT` parameter is not a l-value.

* [UNREACHABLE] Phantom#961

```
1. If (dir matches pattern `NO`), then ...
1. Else Phantom#961
```

If an action is called on a parameter direction other than `NO`.
This is unreachable as the other cases are already handled beforehand.

### Call_ok

```
relation Call_ok: p, C, tid*', functyp, targIL*', arg*', id*'
```

#### Well-formedness of the function type being called

* [MAY-FEASIBLE] Phantom#988
* [MAY-FEASIBLE] Phantom#997
* [MAY-FEASIBLE] Phantom#1003
* [MAY-FEASIBLE] Phantom#1007
* [MAY-FEASIBLE] Phantom#1032
* [MAY-FEASIBLE] Phantom#1035
* [MAY-FEASIBLE] Phantom#1042

```
2. If (FuncType_wf: $bound_tids(p, C) |- (ExternFuncT pt* typ_r) holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (ExternAbstractMethodT pt'''* typ_r') holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (ExternMethodT pt'''* typ_r') holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (FuncT pt'''* typ_r') holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (ExternAbstractMethodT pt* typ_r) holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (ExternMethodT pt* typ_r) holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (FuncT pt* typ_r) holds), then
2. Else Phantom#988, #997, #1003, #1007, #1032, #1035, #1042
```

If the function being called is not well-formed.
May try to specialize a generic function definition to make it ill-formed and then call it.
Note that #1032, #1035, #1042 happens when not type-inferred.
i.e., second-order well-formedness failure.

* [UNREACHABLE] Phantom#993
* [UNREACHABLE] Phantom#1024
* [UNREACHABLE] Phantom#1028
* [UNREACHABLE] Phantom#1038

```
2. If (FuncType_wf: $bound_tids(p, C) |- (ActionT pt*) holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (ControlApplyMethodT pt*) holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (ParserApplyMethodT pt*) holds), then
2. If (FuncType_wf: $bound_tids(p, C) |- (BuiltinMethodT pt* typ_r) holds), then
2. Else Phantom#993, #1024, #1028, #1038
```

If the `ActionT` (#993), `ControlApplyMethodT` (#1024), `ParserApplyMethodT` (#1028), `BuiltinMethodT` (#1038) being called is not well-formed.
This is unreachable as actions are not generic.

* [UNREACHABLE] Phantom#1018

```
1. If (FuncType_wf: $bound_tids(p, C) |- (TableApplyMethodT typ_r) holds), then
1. Else Phantom#1018
```

If the `TableApplyMethodT` being called is not well-formed.
But this is unreachable as the well-formedness of `TableApplyMethodT` is already guaranteed by a table declaration check.

#### Arity mismatch

* [UNREACHABLE] Phantom#989
* [UNREACHABLE] Phantom#994
* [UNREACHABLE] Phantom#1000
* [UNREACHABLE] Phantom#1006
* [UNREACHABLE] Phantom#1011
* [UNREACHABLE] Phantom#1016
* [UNREACHABLE] Phantom#1025
* [UNREACHABLE] Phantom#1029
* [UNREACHABLE] Phantom#1033
* [UNREACHABLE] Phantom#1036
* [UNREACHABLE] Phantom#1039
* [UNREACHABLE] Phantom#1043

```
relation Call_ok: p, C, tid*', functyp, targIL*', arg*', id*'

4. If (|pt'*| = |argIL*|), then ...
4. Else Phantom#989, #994, #1000, #1006, #1011, #1016, #1025, #1029, #1033, #1036, #1039, #1043
```

Unreachable as argument arity was already checked in `FuncType_ok` which calls `$find_funcdef`.

* [UNREACHABLE] Phantom#1020

```
1. If (arg*' matches pattern []), then
1. Else Phantom#1020
```

If table apply method call has arguments.
Unreachable as argument arity was already checked in `FuncType_ok` which calls `$find_funcdef`.

* [UNREACHABLE] Phantom#1021
* [UNREACHABLE] Phantom#1026
* [UNREACHABLE] Phantom#1030
* [UNREACHABLE] Phantom#1040

```
2. If (targIL*' matches pattern []), then
2. Else Phantom#1021, #1026, #1030, #1040
```

If table, control apply method call has type arguments.
Unreachable as type argument arity was already checked in `FuncType_ok` which calls `$find_funcdef`.

#### Wrong call-site

* [MAY-UNREACHABLE] Phantom#996
* [MAY-UNREACHABLE] Phantom#1031

```
1. If (Call_site_ok: p C |- (ExternAbstractMethodT pt'''* typ_r') : CALLSITE_OK holds), then ...
1. If (Call_site_ok: p C |- (ExternAbstractMethodT pt* typ_r) : CALLSITE_OK holds), then
1. Else Phantom#996, #1031
```

If the `ExternAbstractMethodT` is called at a wrong call-site (when type-inferred, #996, and when not #1031).
But calling `ExternAbstractMethdT` happens rarely, only when instantiating with object initializer, AFAIK.

* [UNREACHABLE] Phantom#1037

```
1. If (Call_site_ok: p C |- (BuiltinMethodT pt* typ_r) :CALLSITE_OK holds), then
1. Else Phantom#1037
```

Unreachable as `BuiltinMethodT` can be called anywhere.

* [FEASIBLE] Phantom#1041

```
1. If (Call_site_ok: p C |- (FuncT pt* typ_r) : CALLSITE_OK holds), then
1. Else Phantom#1041
```

If `FuncT` is called at a wrong call-site, with no type inference.

#### Failed type inference

#### Others

* [FEASIBLE] Phantom#991

```
1. If (~$is_table_apply_in_action_arg(argIL))*, then ...
1. Else Phantom#991
```

If an action is called with an argument containing a table application.

* [UNREACHABLE] Phantom#998
* [UNREACHABLE] Phantom#1004
* [UNREACHABLE] Phantom#1009
* [UNREACHABLE] Phantom#1014

```
4. If (functyp' matches pattern `ExternAbstractMethodT%%`), then
4. If (functyp' matches pattern `ExternMethodT%%`), then
4. If (functyp' matches pattern `FuncT%%`), then
4. If (functyp' matches pattern `ExternFuncT%%`), then
4. Else Phantom#998, #1003, #1009, #1014
```

Unreachable as `functyp'` is derived from substituting `functyp`.

* [UNREACHABLE] Phantom#999
* [UNREACHABLE] Phantom#1005
* [UNREACHABLE] Phantom#1010
* [UNREACHABLE] Phantom#1015

```
4. If ((typ? matches pattern (_)))*, then
4. Else Phantom#999, #1005, #1010, #1015
```

Type inference failure should have been already handled in `$infer_targs`.

* [UNREACHABLE] Phantom#1019

```
1. If (id*' matches pattern []), then
1. Else Phantom#1019
```

Table apply method call expects no arguments, so it should not have default arguments resolved.

### Call_site_ok

```
relation Call_site_ok: cursor, C, functyp
```

* [FALLTHROUGH] Phantom#970

```
1. Else If (functyp matches pattern `ExternAbstractMethodT%%`), then
    1. Let (ExternAbstractMethodT _paramtyp* _typ) be functyp
    2. Let lkind be C.LOCAL.KIND
    3. If (lkind matches pattern `EXTERNABSTRACTMETHOD%`), then
    3. Else Phantom#970
```

Other cases will be handled later.

* [MAY-UNREACHABLE] Phantom#971
* [MAY-UNREACHABLE] Phantom#975
* [MAY-UNREACHABLE] Phantom#977
* [MAY-UNREACHABLE] Phantom#979

```
1. Else If (functyp matches pattern `ExternAbstractMethodT%%`), then
1. Else If (functyp matches pattern `BuiltinMethodT%%`), then
1. Else If (functyp matches pattern `ExternFuncT%%`), then
1. Else If (functyp matches pattern `FuncT%%`), then
    4. If ((((C.LOCAL.KIND = (ACTION)) \/ (C.LOCAL.KIND = (PARSERSTATE))) \/ (C.LOCAL.KIND = (CONTROLAPPLYMETHOD))) \/ (C.LOCAL.KIND = (TABLEAPPLYMETHOD))), then
    4. Else Phantom#971, #975, #977, #979
```

When called from a local kind other than `ACTION`, `PARSERSTATE`, `CONTROLAPPLYMETHOD`, `TABLEAPPLYMETHOD`, (and implcitly) `EXTERNABSTRACTMETHOD`.

* [UNREACHABLE] Phantom#982
* [UNREACHABLE] Phantom#984
* [UNREACHABLE] Phantom#985
* [UNREACHABLE] Phantom#986

```
1. Else If (functyp matches pattern `ExternAbstractMethodT%%`), then
1. Else If (functyp matches pattern `ExternMethodT%%`), then
1. Else If (functyp matches pattern `BuiltinMethodT%%`), then
1. If (functyp matches pattern `ExternFuncT%%`), then
    2. If ((C.BLOCK.KIND = (PARSER)) \/ (C.BLOCK.KIND = (CONTROL))), then
    2. Else Phantom#982, #984, #985, #986
```

When `ExternAbstractMethodT`, `ExternMethodT`, `BuiltinMethodT`, `ExternFuncT` is called from a block kind other than `PARSER`, `CONTROL`.
But impossible since other kinds are `EMPTY`, `EXTERN`, and `PACKAGE`.

* [FEASIBLE] Phantom#983

```
1. If (cursor matches pattern `BLOCK`), then
  1. If (functyp matches pattern `ExternFuncT%%`), then
  1. Else If (functyp matches pattern `BuiltinMethodT%%`), then
  1. Else If (functyp matches pattern `ExternMethodT%%`), then
  1. Else If (functyp matches pattern `ExternAbstractMethodT%%`), then
  1. Else Phantom#983
```

If called anything other than `ExternFunc`, `BuiltinMethod`, `ExternMethod`, `ExternAbstractMethod` from a block context.
