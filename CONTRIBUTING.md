# Contributing

This guide is the source of truth for how to contribute to SpecTecX. It is read by both human contributors and AI coding agents, so conventions are stated with the reasoning behind them — not just the rule.

For project orientation, build, and run instructions, see [README.md](README.md). This guide assumes you can build the project and run the test suite.

A few `make` targets matter mostly to contributors:

- `make fmt` — runs `dune fmt`. Run before committing. Requires `opam install ocamlformat 0.27.0` in your switch.
- `make fmt-check` — runs `dune build @fmt`; fails if anything is unformatted. CI runs the same check.
- `make check` — runs `dune build @check`: type-checks every library and executable without producing the final binary. Faster than `make exe` for catching type errors during iteration.
- `make test-quick` runs the fast test groups, including package ownership and plugin discovery.
- `make promote` — regenerates `.expected` files. The test suite is diff-based, so a spec or interpreter change that shifts output requires this before the commit lands, otherwise `make test` is red.

## Code Conventions

**Names are part of the spec.** SpecTecX is a language-specification compiler; names are vocabulary, not decoration. A misleading name is a semantic bug, not a style nit. Before settling on a name, check it against existing usage sites for the same concept, and prefer a name that communicates *responsibility*, not mechanism. Sweep all usage sites when renaming.

OCaml conventions: `snake_case` for values and types, `PascalCase` for modules and constructors.

**Don't let naming get ahead of architecture.** Rename only once the code has earned the new name. If the boundary or responsibility behind the name isn't yet right, fix that first.

**No backward-compatibility aliases during refactors.** A rename worth doing is worth completing. Transitional names accumulate.

**Prefer self-documenting code over comments.** Before writing a comment, ask whether a clearer name, a smaller function, or a tighter type makes it unnecessary. Comments that survive that test capture what the code genuinely can't: a non-obvious choice taken over the obvious alternative, an invariant relied on but not visible locally, the spec rule being implemented. Comments that paraphrase the next line, restate a function name, or mark sections do not survive.

**Public APIs reflect the final semantic model.** Internal module paths can differ when dependency direction forces it, but the user-facing surface should preserve the clearest ownership story.

**Boundary between `lib/` and `bin/`.** Reusable code such as domain presentation, CLI infrastructure, and error rendering lives in `lib/`. CLI machinery specifically lives in `lib/cli/` so targets can instantiate it. Target plugins register their `Cli` modules through the registry in `lib/cli/`. The `bin/` directory holds only the top-level entrypoints that load available plugins and dispatch commands. New logic should land in `lib/`, not `bin/`.

**Prefer explicit organization over umbrella buckets** like `core` once distinct concerns have separated.

**Don't introduce one-off meta-patterns** unless they clearly pay for themselves. Small local duplication beats a bespoke helper used nowhere else.

**Use `with_*` only for true scoped wrappers** that run a callback under setup/teardown. When a helper is fundamentally an accumulator update, prefer accumulator-first parameter order to match `fold_left` style.

**Prefer direct code over clever abstractions** when exception handling is involved.

**Prefer small local recursion or folds over mutable refs** when they make control flow easier to read.

**Use `@@` only when it clearly reduces indentation** around a single callback body.

## Why these conventions

Three values drive the workflow rules below: **bisectability**, **reviewability**, and **provenance**. Most specific rules trace back to one of these.

*Bisectability* — `git bisect` should land on a small, buildable, single-purpose commit. That requires three things: every commit builds (otherwise bisect stalls on a non-buildable revision), commits are grouped into merge bubbles (so bisect can step over a whole PR when it isn't the culprit), and refactors stay separate from fixes and features (so the commit bisect lands on isn't doing two things at once).

*Reviewability* is the counterweight. Bisect alone would push toward ever-smaller commits, but the reviewer needs the changes to add up to a coherent story. This is why:

- we don't introduce stubs solely to keep the build green (noise to the reader);
- every non-trivial commit names its motivation;
- PRs are organized around one arc rather than a flat changelog.

*Provenance* — most code in this repo lives downstream of P4-SpecTec, so each change should record where it came from and how much was adapted. This is what backs the `Original-commit:` trailer, the Port vs. Sync PR distinction, and the `Adapted` / `Omitted` scope buckets. With provenance preserved, comparing against upstream or another SpecTec variant remains a tractable git operation rather than archeology.

When a rule below feels arbitrary, it's usually one of these three showing through.

## Commits

We use [Conventional Commits](https://www.conventionalcommits.org/) with one project-specific type:

```
type(scope): imperative summary

<motivation: one or two sentences on the prior problem>

<solution: what the change makes true now>
```

Standard types apply (`feat`, `fix`, `refactor`, `test`, `docs`, `chore`, `perf`, `style`). Plus:

- **`spec`** — changes to `.spectec` files. *All other commits are assumed to be OCaml changes* under the appropriate standard type. A spec rule update is `spec(...)`; a change to the elaborator that consumes it is `refactor(elaborate): ...`.
- **`reorg`** — directory renames, file moves, or layout-only changes that don't restructure code or alter behavior. Distinct from `refactor` (changes code structure) and `chore` (build config or deps). When a rename forces caller updates, the change stays `reorg` if the caller updates are mechanical path/identifier swaps; promote to `refactor` when the rename motivates a real API or structure change.

  The type exists because behavior preservation is a useful property to advertise — tools and reviewers can act differently when they know a change is layout-only. Reviewers seeing `reorg:` skip semantic review and focus on the mechanical check ("did all references update? did anything break the build?"). `git bisect skip $(git log --grep "^reorg" bad..good --format=%H)` removes layout noise from regression bisects. Cherry-picks and reverts are mechanical (paths or identifiers) rather than requiring behavioral analysis.

  Examples: renaming a directory, splitting a corpus into subdirectories, consolidating duplicated test data.

Scope is the narrowest area that honestly describes the change: `cli`, `elaborate`, `il`, `interp`, `instrumentation`, `mixop`, `lang`, `targets/p4`, etc.

### Subject

The subject is **imperative**, present-tense — `extract error handling`, not `extracted` or `extracts`. It names a *concept*, not specific code. Identifiers belong in the body.

- Good: `refactor(cli): extract error handling and group shared flags by role`
- Avoid: `refactor(cli): extract guard from subcommand.ml and group flags into Output/Spec/Batch/Checkpoint`

### Body

The body answers **why** (motivation: what was wrong or limiting) and **what** (solution: the shape the code is in now). The **how** is in the diff — do not restate it in prose.

Two parts, distinguished by **tense**:

- **Motivation** — describes the prior state, one or two sentences. Past tense (`The diagnostic helper lived inline in subcommand.ml, duplicated across multiple consumers.`) or a `Currently, …` framing (`Currently, errors and warnings print immediately when encountered.`); past tense is the default and preferred form.
- **Solution** — third-person present, reading as if narrating what *this commit* does (`The helper is extracted to error_handling.ml…`, `Adds…`, `Replaces…`, `Updates…`). Avoid first-person (`We extract…`) and avoid future tense (`Will extract…`). Bullets follow the same voice.

The motivation paragraph is **optional when the motivation is self-evident from the subject** — adding tests, syncing with upstream, a one-line typo fix. Where it earns its keep is `fix` and `refactor` commits: both must justify themselves against the prior state, so a short past-tense sentence on what was wrong or what was limiting is almost always worth writing.

Name actual identifiers in the body — modules, functions, flags. Plain English is for framing; identifiers are for specifics.

Bullets only when the change spans distinct scopes that don't flow as prose. A single-scope commit gets a prose paragraph.

**ASCII only.** Commit messages flow through varied consumers (`git log`, `gh`, CI logs, `git send-email`, changelog generators), not all of which render UTF-8 reliably. Prefer `->` over `→`. Avoid em-dashes (`—`) entirely rather than substituting `--`; restructure with commas, parens, or periods instead. PR bodies render as Markdown and are fair game for typography; commit messages aren't.

### Trailers

One trailer carries provenance:

- `Original-commit:` — the upstream or sibling-repo commit this change is based on, regardless of how literal or adapted. May appear multiple times when one local commit consolidates a chain of upstream changes. The prose body carries the *degree* of adaptation: a one-line "Ported from P4-SpecTec." signals a direct port; a paragraph explaining what was kept and changed signals an adaptation.

When attributing to upstream in prose, write **"Ported from P4-SpecTec."** as its own paragraph between the solution prose and the trailer block. It pairs visually with `Original-commit:` to form a provenance stanza, separate from the motivation and solution above:

```
<motivation paragraph>

<solution paragraph and/or bullets>

Ported from P4-SpecTec.

Original-commit: https://github.com/kaist-plrg/p4-spectec/commit/<sha>
```

A second trailer, `Copied-from:`, is used for literal tree imports (e.g. dropping in a fresh upstream `spec/` directory) rather than commit ports.

**Use the full GitHub URL in commit-message trailers**, not the `org/repo@sha` shorthand:

```
Original-commit: https://github.com/kaist-plrg/p4-spectec/commit/3e806c83fc38
```

The shorthand (`kaist-plrg/p4-spectec@3e806c83fc38`) renders nicely on GitHub but is opaque in `git log` and unclickable in plain terminals. Reserve it for **PR bodies and merge-commit cover letters**, where GitHub's rendering is the primary read path.

### Atomicity

Each commit must build on its own. Split work as far as possible *without* introducing placeholders or scaffolding solely to satisfy the rule. If keeping a commit buildable would require dead code, fold it into the next.

### Worked examples

A refactor, from [`388d6b3d`](https://github.com/kaist-plrg/spectecx/commit/388d6b3d):

> `refactor(cli): extract error handling and group shared flags by role`
>
> The diagnostic helper lived inline in `subcommand.ml`, duplicated across multiple consumers. Shared flag definitions sat at the top of `cli_args.ml` with no organization.
>
> The helper is extracted to `error_handling.ml` as `guard`/`guard_unit`, called from both `subcommand.ml` and `bin/main.ml`. Shared CLI flags are grouped into `Output`, `Spec`, `Batch`, `Checkpoint` and `Interpreter` submodules. Checkpoint flags become a composite record since they always appear together; the rest stay individual for now.

A feature, from [`39f18ac1`](https://github.com/kaist-plrg/spectecx/commit/39f18ac1):

> `feat(diagnostic): scaffolding for comprehensive diagnostics`
>
> Currently, errors and warnings print immediately when encountered.
>
> To improve diagnostic structure and error messages, as well as support LSPs in the future, a new global `Diagnostic` type is added, with basic scaffolding such as constructors and collectors.

Both follow the same shape: a past-tense framing sentence, then a present-tense paragraph narrating what the commit does.

## Rebasing

A messy WIP branch becomes a reviewable history through interactive rebase. The atomicity rule above defines the *target shape*; this section is *how to get there*.

### Folding WIP into atomic commits

```bash
git rebase -i <base>
```

Use `fixup` and `squash` to absorb follow-ups into the commit they belong to. The criterion is the per-commit buildability rule: if combining produces the smallest commit that still builds and tells one coherent story, fold.

Rewrite messages during the rebase, not before — the final subject and body describe the *folded* result, not any intermediate state.

When folding port commits, use `squash` rather than `fixup` so that `Original-commit:` trailers from the absorbed commits survive into the rewritten message — `fixup` discards messages, including trailers. Consolidate all surviving trailers in the rewritten message; one local commit may carry multiple `Original-commit:` lines.

### Verifying buildability

Don't trust the invariant; enforce it:

```bash
git rebase -i --exec 'make exe' <base>
# or, slower but stronger:
git rebase -i --exec 'make test-quick' <base>
```

The rebase stops on the first commit that fails. Fix or fold and continue.

### `.expected` files during rebase

`.expected` files are generated output, not source. When a rebase conflict lands inside one, **do not hand-merge the diff** — regenerate it. Roughly:

1. Resolve any non-`.expected` conflicts and ensure `make exe` succeeds.
2. `make promote` to regenerate the affected `.expected` files from current code.
3. `git add` the regenerated files and `git rebase --continue`.

If a commit shifts output and a later commit depends on the new expected state, mid-rebase regeneration is the only correct resolution. Hand-merging silently bakes stale expectations into the history.

### Authoring messages during the rebase

When the rebase pauses on a `reword` or `squash`, the commit-message conventions above apply — the rebase is when messages get rewritten, not just when commits get reordered.

## Direct commits to main

The default is that every change lands through a PR. Two narrow categories may be pushed directly to `main`:

- **Documentation-only changes** — `README.md`, `CONTRIBUTING.md`, in-tree docs. No code, no expectations shifted.
- **Trivial project-wide fixes** with no semantic content — e.g. a missing `dune` dependency, a typo in a build flag, a dead import. The bar is that a reviewer's only useful response would be "yes, obviously."

Anything that touches behavior, output, or the spec goes through a PR even when small. When in doubt, open the PR — the cost of a one-line PR is low; the cost of a direct commit that turned out to need discussion is a revert.

The commit-message conventions above still apply: a direct-to-main commit isn't an excuse for a terse subject and no body.

## Pull Requests

PRs fall into five shapes. The shape determines title prefix and body structure:

| Type     | Title                          | Body skeleton                                                                |
| -------- | ------------------------------ | ---------------------------------------------------------------------------- |
| Refactor | `Refactor <concept>`           | `## Motivation` → optional `## Core Concept(s)` → optional `## Scope` → `## Commit Log` |
| Feature  | concept-led, no prefix         | `## Motivation` → `## Core Concept(s)` → optional `## Scope` → `## Commit Log` |
| Port     | concept-led, no prefix         | `## Motivation` (cite original PR) → `## Scope` (Ported / Adapted / Omitted) → `## Commit Log` |
| Sync     | `Sync <area>`                  | `## Motivation` → `## Scope` (Ported / Adapted / Local Changes) → `## Commit Log` |
| Reorg    | `Reorg <concept>`              | `## Motivation` → optional `## Scope` (rename list) → `## Commit Log` |

`## Future Work` is optional on any type for deferred follow-ups and open design questions. `## Minor Changes` holds commits that ship in the PR but don't fit the main story.

The Port/Sync distinction is intentional: a **Port** tracks one upstream PR end-to-end and contains only the adaptations needed to land it locally; a **Sync** is a catch-up that may bundle several upstream commits *and* genuinely local fixes or features. The two share vocabulary but differ in what each is allowed to bundle.

**Reorgs** typically fast-forward (single-commit) and use `## Scope` to list the moves explicitly. Behavior preservation is the implicit precondition — if the change has any semantic shift, it's a Refactor.

### Title

One clean angle, not a two-sided story. If you reach for `X and Y`, ask whether the two halves are one concept under a better name.

- One concept: [`#34 Refactor CLI into per-target modules`](https://github.com/kaist-plrg/spectecx/pull/34) — clearer than `Refactor CLI to define subcommand interfaces and consolidate target modules`, even though the diff does both.
- Two genuinely independent threads, joined explicitly: [`#32 Refactor instrumentation architecture and make lifecycle exception-safe`](https://github.com/kaist-plrg/spectecx/pull/32).

`Refactor`, `Sync`, and `Reorg` lead with the verb. Features and Ports lead with the concept directly ([`#30 Simplify elaboration using IL types`](https://github.com/kaist-plrg/spectecx/pull/30)) — the verb prefix is dropped because the concept already names the change.

When themes mix, pick the dominant one for the title and let off-arc commits live under `## Minor Changes` in the body. Avoid abstract nouns like `composition` or `orchestration` in the title; reserve those for the body where they have room to be defined.

### Body

Open every PR with `## Motivation` — the problem, pressure, or design goal. The remaining sections depend on the type.

**Refactors and Features** share the skeleton shown in the table above, with optional `## Scope` and `## Future Work` when the change needs more explanation. The criterion for adding `## Scope` mirrors the criterion for bullets in a commit message: the concept is coherent at the top level, but it touches enough distinct scopes that prose alone leaves the reader without a map. A small refactor in one module needs `Motivation` and `Commit Log` only; a cross-cutting one needs the scoped breakdown.

`## Core Concept(s)` typically lists ideas as bolded bullets and closes with one paragraph naming the direction the PR moves the code in. The bullets are the *what*; the closing paragraph is *why this hangs together*. Drop it when the motivation already names the concept clearly.

**Ports and Syncs** share a vocabulary for what comes from upstream:

- `### Ported` — upstream commits taken nearly directly.
- `### Adapted` — upstream commits whose intent is preserved but whose code was adjusted for the local context.
- `### Omitted` — upstream commits explicitly not taken, with a one-line reason.

`### Omitted` is mainly used in Ports, where one-to-one upstream tracking makes the gap worth noting. Bullets in all three sections cite their origin: `Original: kaist-plrg/p4-spectec@<sha>`.

The two types diverge in one bucket: **only Syncs allow `### Local Changes`**, capturing local-origin fixes or features that ride along with the catch-up. A Port that wants to include a local fix should split the fix into its own commit landing through a separate PR — Ports stay one-to-one with their upstream PR. Ports also expect the Motivation to link the upstream PR being tracked.

Scoped bullets summarize **thematically**, not one-per-commit. Group commits serving the same idea under one bullet — five commits often collapse to two or three. The per-commit view is preserved by `## Commit Log`; scope bullets give the reader the *shape*.

End multi-commit PRs with `## Commit Log` listing commit subjects verbatim — including their `type(scope):` prefixes — in final-history order. GitHub's `is:pr` search matches title and body only, not commit subjects, so inlining them keeps the PR discoverable.

The body is prose-first. Bullets enumerate concrete changes or scope boundaries — they aren't the default format for the whole PR. Within the body, GitHub shorthand for cross-repo references (`org/repo@sha`, `org/repo#NN`) is preferred over full URLs since GitHub renders them inline.

### Opening a PR

```bash
git push -u origin <branch>
gh pr create --title "<title>" --body-file <body.md>
```

Drafting the body in a file produces cleaner prose than typing it into the `gh` flag.

### Worked examples

- **Refactor:** [#34 — Refactor CLI into per-target modules](https://github.com/kaist-plrg/spectecx/pull/34): scoped refactor with `Core Concepts` + `Scope` and an off-arc `feat(cli)` bullet under `Minor Changes`.
- **Refactor (two-threaded):** [#32 — Refactor instrumentation architecture and make lifecycle exception-safe](https://github.com/kaist-plrg/spectecx/pull/32): legitimate two-threaded title; the cover letter splits the threads in the scope bullets.
- **Port:** [#30 — Simplify elaboration using IL types](https://github.com/kaist-plrg/spectecx/pull/30): concept-led title (no `Refactor` prefix) tracking one upstream PR, with `Scope` split into `Ported` / `Adapted` / `Omitted`.
- **Sync:** [#35 — Sync new P4 concrete spec](https://github.com/kaist-plrg/spectecx/pull/35): catch-up bundling upstream ports with local changes, with `Scope` split into `Ported` / `Adapted` / `Local Changes`.

When unsure, run `git log --merges` and find the most recent merge whose shape matches yours. (Single-commit ff-merged PRs — see below — won't appear in `--merges`; scan `git log` directly for those.)

## Merge Commits

Merge commits are *cover letters*, not rewritten commit logs. Their reader is scanning `git log` to answer "what landed, where, and what's deferred" without opening individual commits or PRs. Structure follows that role: one framing paragraph summarizing the branch-level outcome, then scoped bullets only when they make the cover letter quicker to parse than prose alone.

Subject: `Merge: <lowercase summary> (#PR)`. The summary mirrors the PR title (lowercased) so a reader scanning `git log` sees consistent phrasing on both sides.

Avoid one-bullet-per-commit. Prefer scoped summaries (`refactor(instrumentation): ...`, `fix(interp): ...`) describing the merged result. Apply the same thematic grouping as `## Scope`: a six-commit branch may merit only three or four scoped bullets.

Off-arc commits (the `## Minor Changes` of the PR body) get their own bullet at the end of the scoped block in their original `feat(...)` / `fix(...)` form, so the reader sees them as distinct from the refactor arc.

```bash
git checkout main && git pull
git merge --no-ff <branch>      # editor opens; write the cover letter
git push origin main
```

Squash merges are never used: they discard the per-commit history that the rest of this guide is built around. Fast-forward merges are reserved for the single-commit-PR exception below.

### Scoped sections

A single thematic bullet list needs no header; bullets sit directly under the framing paragraph. Use section headers only when the cover letter benefits from peer grouping. A Port or Sync may lift its `### Ported / ### Adapted / ### Omitted / ### Local Changes` distinctions to peer sections (`Ported:`, `Adapted:`, …) instead of mixing them inside one bullet list. Mixing `PORTED:`, `ADAPTED:`, `OMITTED:` prefixes inside a single list makes the cover letter obscure: a reader has to disambiguate every bullet by prefix instead of skimming uniform peer lists.

Bullets, whether in a peer section or as a single list, take the form `type(scope): Description.` (matching the corresponding commit subject's prefix). Optional `Original: <ref>` suffix when citing upstream provenance, in the same form the PR body uses (full URL, GitHub shorthand, bare SHA, or absent).

### Deferred items

Deferred items appear as a bullet group with a `DEFERRED:` prefix per bullet and no section header:

```
- DEFERRED: <description>.
- DEFERRED: <description>.
```

The prefix is the signal: `git log --grep "DEFERRED:"` collects backlogs scattered across history. A wrapping section header would only repeat that signal, so it is omitted.

### Single-commit PRs (exception)

The default above — `--no-ff`, cover letter, no squash — assumes a multi-commit PR whose internal structure deserves preservation. Large refactors invert this: splitting a sweeping rename or cross-cutting restructure into many atomic commits produces intermediate states that don't individually clarify the change, and the reviewer ends up reading the diff as a whole anyway. For these, a **single-commit PR** is allowed, and is **fast-forwarded** onto main.

Fast-forward, not squash: the branch is already one commit, so merging just advances `main`. No merge commit is created, and the commit's SHA is preserved across the merge.

Because there is no merge commit, the lone commit's message *is* the cover letter. It must carry the framing a `Merge:` subject and body would otherwise provide — motivation, the shape of the result, and (if relevant) scope bullets — under the normal `type(scope):` subject. The PR body still follows the Pull Requests conventions above; ff-merging does not skip the PR.

```bash
git checkout main && git pull
git merge --ff-only <branch>
git push origin main
```

`--ff-only` makes the merge fail loudly if the branch isn't actually one commit ahead of `main`, rather than silently creating a merge commit. If it fails, rebase the branch onto `main` first.

This exception is for refactors whose atomicity would be artificial. A feature, fix, or port stays multi-commit and `--no-ff`.
