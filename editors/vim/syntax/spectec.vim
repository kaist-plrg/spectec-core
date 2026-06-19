" Regex syntax highlighting for SpecTec / SpecTecX specs (*.spectec). A portable
" fallback; for full fidelity in Neovim, use the tree-sitter-spectec plugin.

if exists("b:current_syntax")
  finish
endif

" A region, not a match, so atoms inside a commented-out rule stay grey.
syntax region spectecComment start=";;" end="$" oneline contains=@Spell

syntax keyword spectecKeyword syntax relation rule def dec var builtin hint
syntax keyword spectecPremiseKw if otherwise
syntax keyword spectecConstant true false eps

" Names are CamelCase; the required lower-case letter rules out the atoms below.
syntax match spectecName "\<[A-Z][A-Za-z0-9_]*[a-z][A-Za-z0-9_]*\>\(/[A-Za-z0-9_-]\+\)\?"

" Object syntax: ALL-CAPS atoms, _-tags, quoted operators, backtick markers.
syntax match spectecAtom "\<[A-Z][A-Z0-9_]*\>"
syntax match spectecAtom "\<_[A-Za-z][A-Za-z0-9_]*\>"
syntax match spectecAtom "`[(){}\[\]<>]"
syntax match spectecAtom "`->"
syntax region spectecAtom start=+'+ end=+'+ oneline

syntax match spectecFunc "\$[A-Za-z_(]"me=e-1
syntax match spectecFunc "\$[A-Za-z_][A-Za-z0-9_]*"

syntax region spectecString start=+"+ skip=+\\"+ end=+"+ oneline
syntax match spectecPlaceholder "%\d\+"
syntax match spectecNumber "\<\d\+\>"

" Meta-notation recedes so the variables and atoms it joins read by contrast.
syntax match spectecPremise "^\s*--"
syntax match spectecMeta "|-\|-|\|==>"

hi def link spectecComment      Comment
hi def link spectecKeyword      Keyword
hi def link spectecPremiseKw    Keyword
hi def link spectecConstant     Constant
hi def link spectecName         Function
hi def link spectecAtom         Type
hi def link spectecFunc         Function
hi def link spectecString       String
hi def link spectecPlaceholder  Special
hi def link spectecNumber       Number
hi def link spectecPremise      Special
hi def link spectecMeta         Operator

let b:current_syntax = "spectec"
