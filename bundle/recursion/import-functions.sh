#!/bin/sh
# Copy the completed function rules from ../impty.spectec into recursion.spectec,
# replacing their commented stubs. Refuses until the main exercise is finished;
# safe to re-run, since rules already filled in here are left alone.

set -e

SRC=../impty.spectec
DST=recursion.spectec
RULES="Check_expr/fun Check_expr/call Eval_expr/fun Eval_expr/call"

if [ ! -f "$SRC" ]; then
  echo "error: $SRC not found -- run this from the recursion/ directory." >&2
  exit 1
fi

# A blank output is marked `???` (never valid syntax), so a leftover means an
# unfilled rule -- unlike a bare `_`, which a finished rule may use as a wildcard.
if grep -nF '???' "$SRC" >/dev/null 2>&1; then
  echo "The function rules in $SRC are not finished yet -- a '???' placeholder remains:"
  grep -nF '???' "$SRC" | sed 's/^/  /'
  echo
  echo "Replace every '???' in the function rules, then re-run 'make import'."
  exit 1
fi

for r in $RULES; do
  if ! grep -qE "^rule $r:[[:space:]]*\$" "$SRC"; then
    echo "error: rule $r is missing from $SRC." >&2
    exit 1
  fi
done

# Two passes: capture each rule block from SRC, then swap it over its stub in DST.
awk -v rules="$RULES" '
  BEGIN { split(rules, n, " ") }
  FNR==NR {
    if (cap) {
      if ($0 ~ /^[[:space:]]*$/) cap=0
      else body[cur]=body[cur] "\n" $0
    }
    for (i in n) if ($0 ~ ("^rule " n[i] ":[[:space:]]*$")) { cur=n[i]; body[cur]=$0; cap=1 }
    next
  }
  {
    if (skip) { if ($0 ~ /^;;/) next; else skip=0 }
    hit=0
    for (i in n) if ($0 ~ ("^;; rule " n[i] ":[[:space:]]*$")) {
      print body[n[i]]; imported[n[i]]=1; skip=1; hit=1
    }
    if (!hit) print
  }
  END {
    c=0
    for (i in n) if (imported[n[i]]) { print "  imported " n[i] > "/dev/stderr"; c++ }
    if (c==0) print "  (every function rule was already filled in -- nothing to do)" > "/dev/stderr"
  }
' "$SRC" "$DST" > "$DST.tmp"

mv "$DST.tmp" "$DST"
echo "Wrote the function rules into $DST."
