# ggtree Implementation Backlog

## Purpose

This backlog turns the current roadmap into issue-sized, execution-ready tasks. Each item is scoped so it can be worked on independently or grouped into a milestone.

## Conventions

### Priority

Checklist convention in this file:

- `[x]` implemented
- `[ ]` not yet implemented


- P0: high-impact foundation or correctness work
- P1: important feature completion work
- P2: valuable enhancement with lower urgency
- P3: longer-term exploration

### Status

- Ready: can be implemented now
- Blocked: depends on another backlog item
- Deferred: intentionally postponed

### Issue template fields

Each issue below includes:

- Goal
- Scope
- Deliverables
- Tests
- Dependencies
- Suggested files

## Milestone A: Complete paired-tree / tanglegram support

### [ ] TG-01: Document supported and unsupported replayable layers

- Priority: P0
- Status: Ready
- Goal: Make paired-tree `ggtree` input behavior predictable by documenting which layers are replay-safe.
- Scope:
  - classify existing side-local layers into supported / partially supported / unsupported
  - document the current replay contract in `.dev/` and user-facing docs
- Deliverables:
  - a replay support matrix in `.dev/`
  - a short user-facing note in `ggdoubletree` docs
- Tests:
  - no new logic required; doc consistency check only
- Dependencies:
  - none
- Suggested files:
  - `.dev/tanglegram-implementation.md`
  - `R/tanglegram.R`
  - `man/ggdoubletree.Rd`

### [ ] TG-02: Expand replay support for common tree-side layers

- Subtasks:
  - [x] Basic replay path for `ggtree` object inputs exists
  - [x] Basic replay works for simple tip labels and tip points
  - [ ] Verify replay for `geom_point2()`
  - [ ] Verify replay for `geom_text2()`
  - [ ] Verify replay for `geom_segment2()`
  - [ ] Add tests for mixed left/right replay combinations
- Priority: P0
- Status: Ready
- Goal: Support more `ggtree` layers when inputs are two annotated `ggtree` objects.
- Scope:
  - verify and improve replay for `geom_tiplab()`, `geom_tippoint()`, `geom_point2()`, `geom_text2()`, `geom_segment2()`
  - ensure node-based and label-based mappings survive coordinate remapping
- Deliverables:
  - improved layer remapping helpers
  - explicit tests per supported layer class
- Tests:
  - `ggplot_build()` tests for each supported replayed layer
  - mixed left/right annotation tests
- Dependencies:
  - TG-01 recommended but not required
- Suggested files:
  - `R/tanglegram.R`
  - `tests/testthat/test-tanglegram.R`

### [ ] TG-03: Add partial support for clade-oriented layers in paired-tree mode

- Subtasks:
  - [ ] Map node-offset-aware `geom_hilight()` inputs across paired trees
  - [ ] Map node-offset-aware `geom_cladelabel()` inputs across paired trees
  - [ ] Test left-only clade annotations
  - [ ] Test right-only clade annotations
  - [ ] Test both-side clade annotations together
- Priority: P1
- Status: Ready
- Goal: Improve replay for clade-based annotations.
- Scope:
  - support `geom_hilight()`, `geom_cladelabel()`, and related node-anchored layers when remapping paired trees
  - preserve left/right side ownership cleanly
- Deliverables:
  - remapping logic for node-offset-aware clade layers
  - documented limits for unsupported edge cases
- Tests:
  - replay tests with left-only, right-only, and both-side clade annotations
- Dependencies:
  - TG-02
- Suggested files:
  - `R/tanglegram.R`
  - `tests/testthat/test-tanglegram.R`

### [ ] TG-04: Improve `cophylo` compatibility coverage

- Subtasks:
  - [x] `fortify.cophylo()` compatibility path exists
  - [ ] Add conditional `phytools` test coverage
  - [ ] Test `cophylo` plotting via `ggdoubletree()`
  - [ ] Record remaining parity gaps against `phytools::plot.cophylo()`
- Priority: P0
- Status: Ready
- Goal: Raise confidence that `fortify.cophylo()` behaves well on real `phytools` objects.
- Scope:
  - add conditional tests when `phytools` is installed
  - verify association extraction, plotting, and optimization path behavior
- Deliverables:
  - conditional `testthat` coverage
  - notes on any remaining parity gaps
- Tests:
  - `skip_if_not_installed("phytools")`
  - `cophylo` object fortify/build tests
- Dependencies:
  - none
- Suggested files:
  - `tests/testthat/test-tanglegram.R`
  - `DESCRIPTION` (Suggests only if needed)

### [ ] TG-05: Add paired-tree diagnostics to user-facing output

- Subtasks:
  - [x] Internal optimization diagnostics are already attached as `attr(df, "tangle_optimize")`
  - [ ] Define stable user-facing diagnostics structure
  - [ ] Add `tangle_diagnostics()` helper
  - [ ] Document diagnostics fields and examples
- Priority: P0
- Status: Ready
- Goal: Make `ggdoubletree()` optimization results inspectable.
- Scope:
  - standardize `attr(df, "tangle_optimize")`
  - expose helper to summarize crossings / iterations / optimize_side
- Deliverables:
  - `tangle_diagnostics()` or equivalent helper
  - docs and examples
- Tests:
  - tests for diagnostics presence and values
- Dependencies:
  - none
- Suggested files:
  - `R/tanglegram.R`
  - `man/ggdoubletree.Rd`
  - `tests/testthat/test-tanglegram.R`

### [ ] TG-06: Add link styling presets and richer link controls

- Priority: P1
- Status: Ready
- Goal: Make paired-tree links easier to tune for dense plots.
- Scope:
  - add named presets or helper arguments for sparse vs dense links
  - optionally support side-aware color mapping or grouped links
- Deliverables:
  - `geom_tanglelink()` enhancements
  - updated docs and examples
- Tests:
  - build tests for curve and segment modes
  - mapping tests for grouped metadata
- Dependencies:
  - none
- Suggested files:
  - `R/tanglegram.R`
  - `R/method-ggplot-add-tanglegram.R`
  - `tests/testthat/test-tanglegram.R`

### [ ] TG-07: Add circular or radial paired-tree exploration design

- Priority: P2
- Status: Deferred
- Goal: Define whether non-rectangular paired-tree layouts are worth supporting and how.
- Scope:
  - evaluate geometry, link readability, and annotation implications
  - decide on one feasible prototype layout
- Deliverables:
  - design note in `.dev/`
- Tests:
  - none yet
- Dependencies:
  - TG-01 through TG-06 should mature first
- Suggested files:
  - `.dev/tanglegram-implementation.md`

## Milestone B: Layout diagnostics and internal layout consistency

### [ ] LD-01: Define a shared layout diagnostics contract

- Priority: P0
- Status: Ready
- Goal: Standardize layout metadata across `daylight`, `tree_and_leaf`, paired-tree optimization, and future layouts.
- Scope:
  - define common fields such as `layout_name`, `iterations`, `before`, `after`, `warnings`, `timing`
  - document which fields are optional
- Deliverables:
  - `.dev/layout-diagnostics.md`
  - helper constructor for diagnostics objects
- Tests:
  - unit tests for diagnostics structure
- Dependencies:
  - none
- Suggested files:
  - `.dev/`
  - `R/fortify-utilities.R` or new `R/layout-diagnostics.R`

### [ ] LD-02: Add diagnostics to `daylight`

- Subtasks:
  - [x] `daylight` implementation is already deterministic and tested
  - [ ] Attach iteration/convergence metadata to fortified output
  - [ ] Add regression tests for diagnostics fields
- Priority: P1
- Status: Ready
- Goal: Record optimization iterations and convergence metadata for `daylight`.
- Scope:
  - attach diagnostics to fortified output
  - keep behavior deterministic
- Deliverables:
  - `attr(df, "layout_diagnostics")` for `daylight`
- Tests:
  - regression tests for diagnostics fields
- Dependencies:
  - LD-01
- Suggested files:
  - `R/tree-utilities.R`
  - `tests/testthat/test-daylight.R`

### [ ] LD-03: Add diagnostics to `tree_and_leaf`

- Subtasks:
  - [x] `tree_and_leaf` layout is implemented and regression-tested
  - [ ] Attach relaxation diagnostics to fortified output
  - [ ] Add regression tests for diagnostics fields
- Priority: P1
- Status: Ready
- Goal: Expose relaxation metadata for `tree_and_leaf`.
- Scope:
  - add iteration count, convergence indicator, and basic spread metrics
- Deliverables:
  - layout diagnostics on fortified output
- Tests:
  - regression tests for diagnostics presence
- Dependencies:
  - LD-01
- Suggested files:
  - `R/tree-utilities.R`
  - `tests/testthat/test-tree-and-leaf.R`

### [ ] LD-04: Add diagnostics printing helpers

- Priority: P2
- Status: Ready
- Goal: Make diagnostics easier to inspect interactively.
- Scope:
  - helper like `layout_diagnostics(x)`
  - optional print method for diagnostics objects
- Deliverables:
  - user helper and minimal docs
- Tests:
  - helper returns expected object structure
- Dependencies:
  - LD-01
- Suggested files:
  - new `R/layout-diagnostics.R`
  - `man/`

## Milestone C: Graphics regression protection

### [ ] GT-01: Audit tests that only check `ggplot` class

- Subtasks:
  - [x] Some weak tests were already upgraded during recent fixes
  - [ ] Audit remaining class-only tests across `tests/testthat/`
  - [ ] Upgrade safe cases to `ggplot_build()`
  - [ ] Record intentionally unchanged tests and why
- Priority: P0
- Status: Ready
- Goal: Replace weak object-type tests with build/render-aware tests where appropriate.
- Scope:
  - identify tests that still only assert class
  - upgrade to `ggplot_build()` where safe
- Deliverables:
  - test audit checklist
  - first pass of upgraded tests
- Tests:
  - the changed tests themselves
- Dependencies:
  - none
- Suggested files:
  - `tests/testthat/`

### [ ] GT-02: Add visual snapshot tests for layout-sensitive features

- Priority: P1
- Status: Ready
- Goal: Protect against silent geometry regressions.
- Scope:
  - evaluate use of visual snapshot tooling for selected stable examples
  - start with `daylight`, `tree_and_leaf`, and `ggdoubletree`
- Deliverables:
  - visual snapshot test setup
  - baseline snapshots for selected cases
- Tests:
  - snapshot suite
- Dependencies:
  - GT-01 recommended
- Suggested files:
  - `tests/testthat/`
  - `DESCRIPTION` if new suggests are needed

### [ ] GT-03: Add multi-layer integration tests

- Priority: P1
- Status: Ready
- Goal: Catch failures that only appear when several layers are combined.
- Scope:
  - create representative plots combining tree layout, labels, highlights, heatmap, and paired-tree links
- Deliverables:
  - integration test cases
- Tests:
  - `ggplot_build()` on combined plots
- Dependencies:
  - none
- Suggested files:
  - `tests/testthat/`

## Milestone D: Performance and scale

### [ ] PF-01: Profile `gheatmap()` on large inputs

- Priority: P1
- Status: Ready
- Goal: Identify where large matrix/tree combinations slow down.
- Scope:
  - benchmark several sizes
  - locate matching, reshaping, and plotting hot spots
- Deliverables:
  - `.dev/gheatmap-performance.md`
  - optional benchmark script
- Tests:
  - no new unit tests; benchmark artifacts only
- Dependencies:
  - none
- Suggested files:
  - `.dev/`
  - `R/gheatmap.R`

### [ ] PF-02: Optimize large-tip label workflows

- Priority: P2
- Status: Ready
- Goal: Improve `geom_tiplab()` performance for large trees.
- Scope:
  - profile label angle/alignment calculations and filtering paths
  - reduce repeated computations where possible
- Deliverables:
  - targeted performance improvements
- Tests:
  - regression tests plus benchmark notes
- Dependencies:
  - PF-01 optional
- Suggested files:
  - `R/method-ggplot-add.R`
  - `R/geom_tiplab*.R`

### [ ] PF-03: Benchmark paired-tree links on larger trees

- Priority: P2
- Status: Ready
- Goal: Understand scaling limits of `ggdoubletree()` and `geom_tanglelink()`.
- Scope:
  - benchmark increasing tip counts and association densities
  - record when curve links become impractical
- Deliverables:
  - benchmark note and practical guidance
- Tests:
  - no new unit tests; benchmark artifacts only
- Dependencies:
  - TG-06 helpful
- Suggested files:
  - `.dev/`
  - `R/tanglegram.R`

## Milestone E: Comparison grammar and higher-level APIs

### [ ] CG-01: Design node correspondence helpers

- Priority: P2
- Status: Ready
- Goal: Move from tip-only associations toward richer tree comparison workflows.
- Scope:
  - define helpers for matched clades or node correspondence tables
- Deliverables:
  - design note and candidate API
- Tests:
  - none yet
- Dependencies:
  - TG milestone should stabilize first
- Suggested files:
  - `.dev/`

### [ ] CG-02: Prototype shared-clade highlighting across paired trees

- Priority: P2
- Status: Blocked
- Goal: Highlight corresponding clades on both sides of a paired-tree plot.
- Scope:
  - use node or label-set mappings to drive highlighting
- Deliverables:
  - prototype API
  - example plot
- Tests:
  - integration tests once implemented
- Dependencies:
  - CG-01
  - TG-03
- Suggested files:
  - `R/tanglegram.R`
  - `tests/testthat/test-tanglegram.R`

### [ ] CG-03: Design a broader comparison grammar

- Priority: P3
- Status: Deferred
- Goal: Decide whether `ggdoubletree()` should grow into a family of comparison helpers.
- Scope:
  - compare possible APIs for tree-vs-tree, multi-tree, and consensus workflows
- Deliverables:
  - `.dev/comparison-grammar.md`
- Tests:
  - none yet
- Dependencies:
  - practical experience from TG milestone
- Suggested files:
  - `.dev/`

## Milestone F: Documentation and developer hygiene

### [ ] DOC-01: Add a paired-tree vignette or book subsection expansion

- Subtasks:
  - [x] Initial paired-tree example already added to the treedata-book
  - [ ] Add a raw-tree input example
  - [ ] Add a richer annotated-`ggtree` comparison example
  - [ ] Consider a dedicated vignette or expanded book subsection
- Priority: P1
- Status: Ready
- Goal: Make paired-tree workflows easier to discover and reuse.
- Scope:
  - extend current book example
  - include raw-tree and annotated-`ggtree` examples
- Deliverables:
  - updated book chapter and/or vignette
- Tests:
  - render check if practical
- Dependencies:
  - none
- Suggested files:
  - `04_Mybooks/treedata-book-2ed/04_ggtree_visualization.qmd`

### [ ] DOC-02: Add developer note for replay architecture

- Priority: P1
- Status: Ready
- Goal: Reduce future confusion when extending paired-tree layer support.
- Scope:
  - document how replay works today
  - record known limits and edge cases
- Deliverables:
  - `.dev/tanglegram-replay.md`
- Tests:
  - none
- Dependencies:
  - TG-01
- Suggested files:
  - `.dev/`

### [ ] DOC-03: Keep implementation-status docs synchronized

- Subtasks:
  - [x] `tanglegram-implementation.md` now has checklist-style status
  - [x] `ggtree-roadmap.md` exists
  - [x] `implementation-backlog.md` exists
  - [ ] Add a lightweight update checklist for future milestones
- Priority: P2
- Status: Ready
- Goal: Prevent `.dev/` plans from drifting away from code.
- Scope:
  - update implementation-status sections whenever major milestones land
- Deliverables:
  - lightweight maintenance checklist
- Tests:
  - none
- Dependencies:
  - none
- Suggested files:
  - `.dev/tanglegram-implementation.md`
  - `.dev/tree-and-leaf-implementation.md`
  - `.dev/ggtree-roadmap.md`

## Suggested First Queue

If the next few issues need to be prioritized tightly, the best order is:

1. TG-01: document replay support matrix
2. TG-02: expand replay support for common layers
3. TG-04: improve `cophylo` compatibility coverage
4. TG-05: add paired-tree diagnostics helper
5. LD-01: define shared layout diagnostics contract
6. GT-01: upgrade weak graphics tests
7. DOC-02: add developer note for replay architecture

## Suggested Milestone Grouping

### Milestone A1: paired-tree stabilization

- TG-01
- TG-02
- TG-04
- TG-05
- DOC-02

### Milestone A2: paired-tree polish

- TG-03
- TG-06
- DOC-01

### Milestone B1: diagnostics foundation

- LD-01
- LD-02
- LD-03
- LD-04

### Milestone C1: regression hardening

- GT-01
- GT-02
- GT-03

## Summary

This backlog is designed to make continued `ggtree` development incremental and low-friction. The most immediate value still comes from stabilizing paired-tree workflows, establishing diagnostics as a first-class concept, and raising confidence in geometry-heavy features through stronger regression protection.
