# Room for Optimization

```
5. Let tid_fresh* be tid*'
```

Substitute `tid_fresh*` for `tid*'`.

```
1. Let typ'' be $canon_typ(typ)
2. If (typ'' has type objtyp), then
3. Let typ' be $canon_typ(typ)
4. If (typ' has type objtyp), then
... 
```

Substitute `typ'` for `typ''`.

```
3. Let (( typ_b ; ctk_b )) be $annot(exprIL_b)
4. Expr_ok: p' C |- expr_i : exprIL_i
5. Let (( typ_i ; ctk )) be $annot(exprIL_i)
6. If (ctk matches pattern `LCTK`), then
7. Let (( typ_b ; _ctk )) be $annot(exprIL_b)
8. If (ctk matches pattern `LCTK`), then
9. Let (( typ_i ; ctk_i )) be $annot(exprIL_i)
10. If ~(ctk_i matches pattern `LCTK`), then
```
