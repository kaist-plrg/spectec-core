## \_wf Relations

### CParamType_wf

```
relation CParamType_wf: tidset, consctxt, (id dir typ val?)
```

* [MAY-UNREACHABLE] Phantom#264

```
1. If (Type_wf: tidset |- typ holds), then ...
1. Else Phantom#264
```

If the constructor parameter's type is not well-formed.
This is may-unreachable as `CParamType_wf` is invoked after `CParamType_ok`, where it already invokes `Type_ok` which again, invokes `Type_wf`.

### CParamTypes_wf

```
relation CParamTypes_wf: tidset, consctxt, pt*
```

* [FEASIBLE] Phantom#267

```
2. If $distinct_<id>(id*), then ...
2. Else Phantom#267
```

If the constructor parameters are not distinct.

### ConsType_wf

```
relation ConsType_wf: tidset, (ConsT pt* typ)
```

* [UNREACHABLE] Phantom#270
* [UNREACHABLE] Phantom#275

```
1. If (CParamTypes_wf: tidset (CONSCTXT) |- pt* holds), then
  1. If (Type_wf: tidset |- typ holds), then
    1. Let typ'' be $canon_typ(typ)
    2. If (typ'' has type objtyp), then
    2. Else Phantom#270
    3. Let typ' be $canon_typ(typ)
    4. If (typ' has type objtyp), then
    4. Else Phantom#275
```

Constructor types return only object types.

* [FEASIBLE] Phantom#271

```
2. Else If (objtyp matches pattern `PackageT%`), then
    1. Let (PackageT _typ*) be objtyp
    2. (Let (_id _dir typ_p _val?) be pt)*
    3. (Let typ_p' be $canon_typ(typ_p))*
    4. If (((~$is_deft(typ_p') /\ ~$is_spect(typ_p')) /\ ~$is_tablet(typ_p')))*, then
    4. Else Phantom#271
```

But maybe refine this with an auxiliary function checking nesting of package types.
Essentially checking whether table appears as a constructor parameter to a package type.

* [FEASIBLE] Phantom#274

```
2. If (objtyp matches pattern `ParserT%`), then
    1. Let (ParserT _paramtyp*) be objtyp
    2. (Let (_id _dir typ_p _val?) be pt)*
    3. (Let typ_p' be $canon_typ(typ_p))*
    4. If (((((~$is_deft(typ_p') /\ ~$is_spect(typ_p')) /\ ~$is_controlt(typ_p')) /\ ~$is_packaget(typ_p')) /\ ~$is_tablet(typ_p')))*, then
    4. Else Phantom#274
```

Also, refine this.



### ConsDef_wf

```
relation ConsDef_wf: tidset, (ConsD (tparam*, tparam_hidden*) -> constyp)
```

* [UNREACHABLE] Phantom#279
* [UNREACHABLE] Phantom#288

```
3. If $is_externt(typ_r'), then
6. If $is_packaget(typ_r'), then
  1. If $distinct_<tid>(tparam* ++ tparam_hidden*), then
  1. Else Phantom#279, #288
```

If the extern or package constructor type parameters are not distinct.
But this is unreachable as `$add_typdefs` in extern object and package declaration typing rules already enforce this.

* [MAY-FEASIBLE] Phantom#281

```
4. If $is_parsert(typ_r'), then
  1. If $distinct_<tid>(tparam* ++ tparam_hidden*), then
    2. If (ConsType_wf: tidset' |- constyp holds), then
    2. Else Phantom#281
```

If the parser constructor type is not well-formed.
But this is likely to be already handled by `FuncType_wf` on `ParserApplyMethodT` in parser declaration typing rule.
But Phantom#284 was covered, check why.

* [UNREACHABLE] Phantom#282
* [UNREACHABLE] Phantom#285

```
4. If $is_parsert(typ_r'), then
4. If $is_controlt(typ_r'), then
  1. If $distinct_<tid>(tparam* ++ tparam_hidden*), then
  1. Else Phantom#282
```

Unreachable as parser constructors are not generic.


