# Tanglegram Support Implementation Plan

## Implementation Status

### Checklist

- [x] Native paired-tree entry point via `ggdoubletree()`
- [x] Native `tanglegram` data structure and `fortify.tanglegram()`
- [x] `fortify.cophylo()` compatibility path
- [x] Raw tree inputs and two `ggtree` object inputs
- [x] Dedicated cross-tree link layer via `geom_tanglelink()`
- [x] Deterministic crossing minimization with `optimize = TRUE`
- [x] `optimize_side = "right"`, `"left"`, and `"both"`
- [x] Regression tests for fortify shape, plotting, layer replay, and optimization behavior
- [x] User-facing documentation in `NEWS.md`, `man/`, and the treedata-book
- [ ] Full replay coverage for arbitrary `ggtree` side-local layers
- [ ] Richer `phytools`/`cophylo` parity coverage
- [ ] Radial or circular paired-tree layouts
- [ ] Exact parity with every `phytools::plot.cophylo()` feature
- [ ] Facet/inset-aware replay for all compound plot extensions
- [ ] More advanced optimization strategies beyond the current deterministic subtree-order heuristic

## Goal

Add native tanglegram support to `ggtree` so users can compare two trees side by side, optimize their relative tip ordering, and draw association links between matched taxa while staying inside the usual `ggtree` / `ggplot2` workflow.

The design should support three entry paths:

1. raw tree-like inputs (`phylo`, `treedata`, `phylo4`) plus an association table
2. two fully constructed `ggtree` plot objects, so each side can carry its own data annotation and styling before tanglegram assembly
3. compatibility with `phytools::cophylo` objects through `fortify.cophylo()`

This is intentionally broader than a plotting shim. The goal is to make tanglegrams a first-class data-to-coordinates transformation inside `ggtree`, while preserving one of `ggtree`'s biggest strengths: rich per-tree annotation layered before or after the paired view is created. The current implementation now covers the core paired-tree workflow described below; the remaining sections still capture both rationale and future extension points.

## References

- treedata book, Chapter 2 section on `ggtree` / `fortify`: <https://yulab-smu.top/treedata-book/chapter2.html#ggtree-fortify>
- `phytools::cophylo` documentation: <https://www.rdocumentation.org/packages/phytools/versions/0.7-70/topics/cophylo>
- local `ggtree` extension points:
  - `R/method-fortify.R`
  - `R/facet_plot.R`
  - `R/method-ggplot-add.R`
  - `R/geom_taxalink.R`

## What the external references imply

### `ggtree` / `fortify`

The book chapter makes the key architectural point: `ggtree` becomes extensible when new object classes can be converted into a fortified tabular tree representation. That means tanglegram support should not be designed only as a plotting convenience. It should ideally include a `fortify()` path so downstream geoms keep working.

### `phytools::cophylo`

`cophylo()` solves two jobs:

1. optimize relative tip order by rotating internal nodes to reduce crossing links
2. plot two trees facing each other with association lines between matched tips

The important takeaway for `ggtree` is that **tanglegram is not just a layout**. It is a compound object made of:

- two trees
- an association table
- an ordering / rotation solution
- a cross-tree link layer

That is why a plain `layout = "tanglegram"` on a single `phylo` is not enough.

## Recommended product shape

Use a two-level API, but make sure both raw trees and pre-annotated `ggtree` objects are first-class citizens.

### Level 1: low-level data support

Status: implemented.

`fortify.cophylo()` and a native `fortify.tanglegram()` now provide the canonical coordinate-building path. This gives us a testable foundation and aligns with the `fortify` extension story from the treedata book.

### Level 2: user-facing helper

Status: implemented as `ggdoubletree()`.

Use a new high-level helper, but avoid the name `ggtangle` because that name is already used by another package in your workflow.

Recommended function name:

```r
ggdoubletree <- function(x, y = NULL, assoc = NULL, ...)
```

Why `ggdoubletree`:

- it avoids collision with your existing `ggtangle` package
- it reads naturally inside the `ggtree` family
- it leaves room for a future lower-level `tanglegram()` constructor without naming conflicts

Supported calls:

```r
ggdoubletree(tr1, tr2, assoc)
ggdoubletree(p1, p2, assoc)
ggdoubletree(cophylo_obj)
ggdoubletree(tanglegram_obj)
```

`ggdoubletree()` should:

1. build or accept a tanglegram object
2. normalize either raw trees or prebuilt `ggtree` objects into a common internal representation
3. preserve side-specific layers / labels / scales where feasible
4. add a dedicated tanglegram link layer
5. return a normal `ggplot`-derived object that still behaves like a `ggtree` plot

This balances ergonomics and extensibility:

- power users can call `fortify()` directly
- users with richly annotated trees can pass two ready-made `ggtree` objects
- most users still get a single high-level entry point

## Why support `ggtree` objects directly

This is the most important design adjustment relative to a basic cophylogeny implementation.

### Why this matters

A lot of `ggtree` value comes from per-tree annotation, for example:

- metadata-driven tip labels
- heatmaps or facet panels next to one tree
- clade highlights and clade labels
- custom scales and theme adjustments
- attached `treedata` annotations already mapped into layers

If tanglegram support only accepts raw trees, users would need to rebuild all that decoration after assembling the paired plot. That would throw away one of `ggtree`'s core strengths.

### Practical implication

The implementation should treat a `ggtree` object as more than a source of a `phylo`. It should preserve enough of the plot-level information to let the left and right trees arrive with their own annotations.

That means the internal representation should have two normalization paths:

- `tree -> fortified tree data`
- `ggtree plot -> fortified tree data + reusable layer metadata`

## Why not only use `facet_plot()` or `aplot::plot_list()`

`facet_plot()` and `plot_list()` are useful composition tools, but they are not a complete tanglegram foundation.

### Advantages of a single combined coordinate system

A true tanglegram implementation benefits from representing both trees in one Cartesian space:

- association links are just geoms with explicit endpoints
- `xlim` / `ylim` and clipping are easier to control
- both trees can share a single scale and a single theme
- later annotations such as highlights, tip labels, and extra layers can be added with fewer coordinate translations

### Why a panel-based approach is still useful

`facet_plot()` shows that `ggtree` already knows how to align data to tree tip order. That is a useful precedent for reordering and label matching, but not sufficient for drawing cross-tree links between two independent trees. We should reuse the matching discipline, not the whole panel architecture.

## Object model

Introduce a native lightweight S3 class, tentatively `tanglegram`, plus a normalized side wrapper for prebuilt `ggtree` plots.

### Constructor shape

```r
new_tanglegram(
  left,
  right,
  assoc,
  rotations = NULL,
  tip_order = NULL,
  left_layers = NULL,
  right_layers = NULL,
  call = NULL,
  params = list()
)
```

### Required fields

- `left`, `right`: normalized side objects, each ultimately backed by a `phylo` plus optional plot metadata
- `assoc`: `data.frame` with at least `left` and `right` columns containing tip labels
- `rotations`: optional optimization metadata
- `tip_order`: optional final tip order for both trees
- `left_layers`, `right_layers`: optional preserved layer specifications when inputs came from `ggtree` objects
- `params`: gap, layout, optimization controls, etc.

### Compatibility layer for `phytools::cophylo`

Add a normalizer:

```r
as_tanglegram.cophylo <- function(x, ...)
```

Responsibilities:

- read `x$trees`
- read and normalize `x$assoc`
- preserve any already-computed rotation result when present
- convert into the native `tanglegram` structure used by `ggtree`

This keeps the rest of the implementation independent from `phytools` internals.

### Support for `ggtree` inputs

Status: implemented in a minimal but working form.

Add a side normalizer:

```r
as_tangle_side.ggplot <- function(x, side = c("left", "right"), ...)
```

Responsibilities:

- extract the underlying tree data and plot data from the `ggtree` object
- recover the effective tree layout and any already-computed coordinates when reuse is safe
- capture reusable annotation layers for later replay on the left or right side
- record which layers are side-safe and which should be rejected or deferred in v1

This is the key mechanism that makes annotated `ggtree`-to-`ggtree` tanglegrams possible.

## User-facing API design

### Primary helper

```r
ggdoubletree(
  x,
  y = NULL,
  assoc = NULL,
  layout = "rectangular",
  ladderize = TRUE,
  mirror = TRUE,
  optimize = TRUE,
  gap = 0.08,
  link_geom = c("curve", "segment", "bezier"),
  link_alpha = 0.4,
  link_colour = "grey50",
  link_width = 0.4,
  preserve_layers = TRUE,
  ...
)
```

### Behavior

- if `x` is `cophylo`, normalize then plot
- if `x` is `tanglegram`, plot directly
- if `x` and `y` are raw trees, build a native tanglegram object from them
- if `x` and `y` are `ggtree` objects, preserve compatible side annotations and rebuild coordinates in a shared paired space
- `layout` initially supports only mirrored rectangular / cladogram-like display
- `optimize = TRUE` runs crossing-reduction before coordinate generation
- `preserve_layers = TRUE` attempts to replay side-local layers after paired coordinates are built

### Deliberate first-version constraints

- first version should focus on left-right rectangular tanglegrams
- no radial tanglegrams in v1
- `ggtree` input support should initially target side-safe layers such as tip labels, points, clade labels, highlights, and simple segments
- facet-like add-ons and inset-like plot composition can be explicitly deferred if they do not survive coordinate remapping cleanly

This is important for keeping the implementation predictable.

## Canonical data representation after `fortify()`

`fortify.tanglegram()` should return a single combined tibble containing both trees, plus enough metadata to replay side-specific annotation layers when the source inputs were `ggtree` objects.

### Required columns

Existing `ggtree` columns:

- `node`
- `parent`
- `label`
- `isTip`
- `x`
- `y`
- `branch`
- `angle`

New tanglegram columns:

- `tree_id`: `"left"` / `"right"`
- `side`: same as `tree_id`, kept for plot logic readability
- `orig_node`: original node id inside the source tree
- `partner_label`: optional, populated for matched tips
- `is_associated`: logical
- `.panel`: optional, reserved for future facet integration

### Attributes on fortified output

Store link-ready metadata as attributes:

- `attr(df, "tangle_assoc")`: resolved association table with endpoint node ids and labels
- `attr(df, "tangle_gap")`: numeric gap used between trees
- `attr(df, "layout") <- "tanglegram"`

This lets `ggtangle()` add link layers without re-deriving matches from scratch.

## Coordinate generation strategy

### Step 1: normalize and validate inputs

Validation rules:

- both trees must be convertible to `phylo`
- `assoc` must contain exactly one row per requested link
- all referenced labels must exist in the corresponding tree
- duplicated labels inside a tree should error early unless an explicit escape hatch is added later

Normalize `assoc` to columns:

- `left`
- `right`
- optional extra aesthetics retained as metadata

If either side is a `ggtree` object, also normalize:

- the base tree data used by that plot
- the subset of layers that can be replayed after coordinate remapping
- any side-specific mappings that depend on tip or node labels rather than hard-coded x/y positions

### Step 2: optimize relative tip order

This is the core algorithmic part.

#### Design goal

Reduce crossings without changing topology, only by rotating internal nodes.

#### Recommended first implementation

Status: implemented with a deterministic subtree-order heuristic.

Use a native greedy subtree-rotation algorithm inspired by `phytools::tipRotate`, but implemented in `ggtree` terms.

#### Proposed algorithm

1. choose one tree as the anchor, default `left`
2. compute initial tip order for both trees from standard `fortify(..., layout = "rectangular")`
3. repeatedly evaluate rotatable internal nodes on the non-anchor tree
4. for each candidate rotation:
   - swap the vertical order of the node's child subtrees
   - recompute only affected tip ranks, not the whole tree from scratch
   - score the new arrangement using crossing count or an inversion-based surrogate
5. accept the best improving move
6. alternate sides if `optimize_side = "both"`
7. stop when no improvement remains or `max_iter` is reached

#### Scoring function

Use a staged strategy.

##### v1 scoring

Minimize inversions in the association mapping.

Implementation idea:

- represent each association as `(rank_left, rank_right)`
- after sorting by `rank_left`, count inversions in `rank_right`
- fewer inversions means fewer crossings

This is simpler and faster than exact segment-intersection counting.

##### v2 scoring (optional)

Add penalties for:

- extreme imbalance in subtree spread
- excessive movement from original ladderized order
- ties resolved by shorter average vertical link span

### Step 3: build mirrored tree coordinates

After final tip order is chosen:

1. fortify left tree normally with `layout = "rectangular"`
2. fortify right tree normally with `layout = "rectangular"`
3. remap `y` on both sides to the optimized tip ranks
4. mirror right-tree `x` values
5. shift both trees apart by a configurable `gap`

Suggested coordinate convention:

- left tree root near `x = 0`
- left tips end near `x = left_width`
- right tips start near `x = left_width + gap`
- right root ends near `x = left_width + gap + right_width`

For a more classic facing layout, mirror the right tree so its root is on the far right and its tips face inward.

### Step 4: offset node identifiers and merge trees

To keep `geom_tree()` working on one data frame:

- keep left tree node ids unchanged
- offset right tree `node` and `parent` by `nrow(left_df)` or `max(left_df$node)`
- preserve original ids in `orig_node`

This is exactly the kind of merge pattern hinted at by the commented `fortify.cophylo()` stub already present in `R/method-fortify.R`.

### Step 4.5: replay compatible side annotations

When inputs are `ggtree` objects, reapply preserved layers after the paired coordinates are assembled.

#### Layer handling policy for v1

Supported or likely supportable:

- `geom_tiplab()`
- point / text layers keyed by node or label
- clade labels and clade highlights that can be resolved by node id or label set
- simple tree-side segments derived from node coordinates

Deferred unless proven easy:

- arbitrary layers that depend on the old absolute x/y coordinates
- multi-panel facet extensions
- insets and externally composed plots

This replay stage is the main difference between a generic tanglegram and a truly `ggtree`-native tanglegram.

### Step 5: resolve link endpoints

Create a resolved association table with:

- `left_label`, `right_label`
- `left_node`, `right_node`
- `x`, `y`, `xend`, `yend`
- any user-supplied metadata columns

Link endpoints should always connect to tip rows, not inferred from raw order vectors.

## Plotting layer design

### New layer: `geom_tanglelink()`

Status: implemented.

Add a dedicated layer instead of overloading `geom_taxalink()`.

#### Why a dedicated geom is better

`geom_taxalink()` assumes one tree in one plot and resolves taxa against `plot$data` without side-awareness. A tanglegram needs two namespaces of taxa and explicit left/right endpoint resolution. Reusing its curve-drawing machinery is fine, but the public API should be separate.

### Proposed API

```r
geom_tanglelink(
  data = NULL,
  mapping = NULL,
  left = NULL,
  right = NULL,
  curvature = 0.15,
  outward = FALSE,
  ...
)
```

Supported usage:

- explicit `data` + `aes(left, right)`
- or rely on `attr(plot$data, "tangle_assoc")` prepared by `fortify.tanglegram()` / `ggtangle()`

### Implementation approach

- reuse `GeomCurvelink` drawing logic where possible
- new `ggplot_add.tanglelink` method resolves endpoints using both `tree_id` and `label`
- default to low-curvature inward-facing links
- allow straight segments for large association sets

## Proposed file-level changes

### New files

- `R/tanglegram.R`
  - constructors
  - validators
  - `ggdoubletree()`
  - `as_tanglegram()` helpers
  - `as_tangle_side()` helpers for raw tree and `ggtree` inputs
- `R/geom_tanglelink.R`
  - public geom wrapper
  - optional shared helpers with `geom_taxalink`
- `tests/testthat/test-tanglegram.R`
- `man/ggdoubletree.Rd`
- `man/geom_tanglelink.Rd`
- `.dev/tanglegram-implementation.md`

### Existing files to update

- `R/method-fortify.R`
  - add `fortify.tanglegram`
  - add `fortify.cophylo`
- `R/method-ggplot-add.R`
  - add `ggplot_add.tanglelink`
- `R/reexports.R`
  - only if any new re-export is needed
- `NAMESPACE`
- `DESCRIPTION`
  - only if a new dependency becomes necessary; avoid this if possible
- `NEWS.md`

## Detailed implementation plan

### Phase 1: native data structure and fortify support

Status: completed.

Deliverables:

- `new_tanglegram()`
- `validate_tanglegram()`
- `as_tanglegram.cophylo()`
- `as_tangle_side.ggplot()`
- `fortify.tanglegram()`
- `fortify.cophylo()`

Acceptance criteria:

- fortified output contains both trees with non-overlapping node ids
- `ggtree(fortify_obj)` style internals remain compatible
- a pair of plain trees can be normalized
- a pair of `ggtree` objects can be normalized, even if layer replay is still partial
- no links yet required for this phase

### Phase 2: crossing minimization engine

Status: completed for deterministic one-side and both-side optimization.

Deliverables:

- subtree rotation helpers
- incremental tip-rank updater
- inversion-based scoring
- deterministic optimizer with `max_iter` / `seed`

Acceptance criteria:

- optimization never changes topology
- identical input gives identical output
- simple known examples reduce or preserve crossing count

### Phase 3: plot helper and link layer

Status: completed for the current rectangular paired-tree workflow.

Deliverables:

- `ggdoubletree()`
- `geom_tanglelink()`
- default styling for readable links
- side-layer replay for supported `ggtree` annotations

Acceptance criteria:

- `ggdoubletree(tr1, tr2, assoc)` returns a normal `ggplot`
- `ggdoubletree(p1, p2, assoc)` works for supported `ggtree` layers
- `ggdoubletree(cophylo_obj)` works
- user can add normal `ggtree` layers after the fact

### Phase 4: documentation and examples

Status: completed for package docs, `NEWS.md`, and an initial treedata-book example.

Deliverables:

- man pages
- book example or vignette snippet
- NEWS entry

## Testing strategy

### Unit tests: constructors and validation

- reject missing `assoc` columns
- reject nonexistent labels
- reject duplicate labels when unsupported
- preserve metadata columns in association table

### Unit tests: fortify shape

- left/right trees both present in one data frame
- node ids are unique after merge
- `tree_id` values are correct
- `attr(df, "tangle_assoc")` exists and is resolved

### Unit tests: optimization correctness

Use small trees where the optimal answer is obvious.

Test cases:

1. already-aligned trees keep the same crossing score
2. a single rotatable conflict reduces inversions after optimization
3. repeated runs are deterministic
4. optimization does not change tip label membership

### Plot-level tests

- `ggdoubletree()` builds successfully with `ggplot_build()`
- `geom_tanglelink()` works with default association metadata
- explicit mapping overrides default metadata
- additional layers such as `geom_tiplab()` and `geom_hilight()` still build
- supported side annotations from `ggtree` inputs are replayed onto the correct side

### Regression tests for `cophylo`

If `phytools` is available in Suggests:

- build a small `cophylo` object
- ensure `fortify.cophylo()` returns valid combined coordinates
- ensure `ggdoubletree(cophylo_obj)` builds

These tests should be conditional to avoid making `phytools` a hard dependency.

## Performance plan

### Likely hot spots

- repeated rescoring during internal-node rotations
- repeated `y` remapping if implemented naively
- link endpoint lookup on large association tables

### Mitigations for v1

- cache descendant tip sets for every internal node
- update only affected tip ranks after a candidate rotation
- use inversion-count scoring rather than exact segment intersections
- resolve association endpoints once and store them in attributes

### When to optimize further

Only if we see meaningful slowdowns on trees with a few hundred tips. For the first implementation, correctness and API clarity matter more than squeezing every last millisecond.

## Important design decisions

### Decision 1: helper plus fortify, not helper alone

Reason: this aligns with `ggtree`'s extensibility model and makes later reuse much easier. It also gives us a clean place to normalize both raw trees and `ggtree` objects.

### Decision 2: native optimizer, not runtime dependency on `phytools::cophylo`

Reason: users should not need `phytools` just to plot a tanglegram in `ggtree`, and we want full control over deterministic behavior and future maintenance.

### Decision 3: rectangular mirrored layout first

Reason: it solves the main biological use case with the lowest implementation risk.

### Decision 4: dedicated `geom_tanglelink()`

Reason: the semantics differ enough from `geom_taxalink()` that a separate layer will stay clearer and more maintainable. It also avoids overloading single-tree assumptions when the input is two annotated `ggtree` objects.

## Non-goals for the first version

- radial or circular tanglegrams
- automatic many-to-many edge bundling
- solving tip-label duplication across a single tree
- exact parity with every plotting feature of `phytools::plot.cophylo`

## Open questions for future refinement

1. should the default optimizer rotate only the right tree, or both trees?
   - recommendation: default `"right"`, optional `"both"`
2. should the helper expose `seed`, even if the first optimizer is deterministic?
   - recommendation: yes, reserve now for future-proofing
3. how should links behave when one tip maps to multiple partners?
   - recommendation: support many-to-many in the association table, but do not optimize specially for it in v1
4. should branch lengths be respected on both trees?
   - recommendation: yes when present; otherwise fall back to cladogram spacing

## Recommended first implementation order

1. add native `tanglegram` class and validators
2. add side normalizers for raw tree and `ggtree` inputs
3. add `fortify.tanglegram()` for fixed input order, no optimization
4. add `ggdoubletree()` with `geom_tanglelink()` using explicit resolved associations
5. add side-layer replay for supported `ggtree` annotations
6. add deterministic crossing-reduction optimizer
7. add `cophylo` compatibility method
8. document and benchmark

This order keeps the work incremental. We get a correct but simple tanglegram early, then improve ordering quality without destabilizing the plotting surface.

## Success criteria

We should consider the feature successful when all of the following are true:

- users can create a tanglegram from two trees plus an association table in one function call
- users can also start from two annotated `ggtree` objects without rebuilding side annotations from scratch
- `phytools::cophylo` objects can be plotted by `ggtree` without manual unpacking
- the resulting plot is a normal `ggplot` object that accepts later `ggtree` layers
- the optimized order reduces or preserves link crossings on benchmark examples
- the implementation is deterministic, dependency-light, and covered by tests
