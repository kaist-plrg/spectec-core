# A list of TODOs to revisit

## For P4-SpecTec

**[1] Add support for `^n (ListN)` iterator**

  -- replace `$repeat_` and `$init_` list helpers
  -- need more thought in binding analysis though

**[2] Implement capture-avoiding type substitution**

  -- when substituting a function parameter `(def $f<X>(...))[theta]`,
     take care of the situation where `theta` captures `X`

**[3] Maybe add a validation pass post-elaboration**

  -- to check if elaboration is really working as intended

**[4] Tweak parser to properly parse hint expressions where commented out**

**[5] Add subscripted turnstile `|-_`**

  -- this would de-necessiate markers like `consctxt`

**[6] Add `IfLetPr` in IL to signify partial bindings**

  -- variant case, cons-list case, and option case

**[7] Of course, big ones:**

  -- LaTeX renderer
  -- Prose generator
  -- IL interpreter

## For the Spec Itself in DSL

**[1] Add `NewTypeD id decl` and `TypeDefD id decl` to syntax**

**[2] Think of adding `syntax sexprIL = exprIL typIL ctk`, like in p4cherry note**
