# TreeAndLeaf-Inspired Layout Implementation Plan

## Goal

Add a new unrooted layout, `tree_and_leaf`, to `ggtree` that produces a leaf-centric static layout inspired by the `TreeAndLeaf` Bioconductor package while remaining deterministic, dependency-light, and compatible with existing `ggtree` layers.

## References

- Bioconductor package page: https://bioconductor.org/packages/release/bioc/html/TreeAndLeaf.html
- Package vignette: https://www.bioconductor.org/packages/devel/bioc/vignettes/TreeAndLeaf/inst/doc/TreeAndLeaf.html
- Package manual: https://bioc.r-universe.dev/TreeAndLeaf/doc/manual.html
- Paper: https://doi.org/10.1093/bioinformatics/btab819

## Why implement this natively in `ggtree`

`TreeAndLeaf` relies on a `RedeR`-style graph relaxation workflow. `ggtree` needs stable, reproducible coordinates that plug into `fortify()`, `geom_tree()`, `geom_tiplab()`, `geom_hilight()`, and existing tree manipulation helpers. A native implementation keeps layout generation deterministic and avoids introducing an interactive graph dependency into the rendering path.

## Scope

### In scope

- New layout string: `"tree_and_leaf"`
- `phylo` and `treedata` support through the existing `fortify()` pipeline
- Deterministic, static coordinates
- Leaf-prioritized relaxation that increases tip separation while keeping the tree skeleton recognizable
- Regression tests and package documentation updates

### Out of scope for the first version

- Full reimplementation of the original `RedeR` optimization machinery
- Barnes-Hut or other large-tree approximation methods
- Interactive tuning tools
- Non-tree graph inputs

## API design

Users will access the layout through the standard `ggtree()` layout parameter:

```r
ggtree(tr, layout = "tree_and_leaf")
```

Additional tuning parameters will be passed through `...` into `fortify()` and then `layout.unrooted()`:

- `initial_layout = c("daylight", "equal_angle", "ape")`
- `max_iter = 200L`
- `leaf_force = 0.08`
- `edge_force = 0.25`
- `anchor_force = 0.02`
- `internal_anchor = 0.08`
- `outward_force = 0.01`
- `step = 0.2`
- `cooling = 0.98`
- `tol = 1e-4`

Defaults favor a stable, modest leaf-spreading effect and deterministic output.

## Algorithm

### Phase 1: initial tree layout

Start from an existing unrooted layout:

1. Build `equal_angle`, `daylight`, or `ape` coordinates.
2. Cache the initial coordinates as the anchor geometry.
3. Derive target edge lengths from the initial layout so the relaxed layout remains visually close to the original tree.

### Phase 2: leaf-centric relaxation

At each iteration:

1. **Leaf repulsion**
   - Apply pairwise repulsion only among tips.
   - Use inverse-square force so nearby leaves separate strongly.
   - Break exact overlaps deterministically using a node-id-based fallback direction.

2. **Edge springs**
   - Apply Hooke-style spring forces along tree edges.
   - Target edge lengths come from the initial layout.
   - This keeps the topology legible and prevents the layout from turning into a generic force-directed graph.

3. **Anchor forces**
   - Pull all nodes back toward their initial coordinates.
   - Use a stronger anchor on internal nodes than on tips.
   - This keeps the internal skeleton stable and lets most visible movement happen at the leaves.

4. **Outward bias**
   - Add a small outward push in the direction from root to node.
   - Scale by normalized root distance so outer nodes move more than inner nodes.

5. **Mobility weighting**
   - Root is fixed.
   - Internal nodes move with a smaller mobility factor.
   - Tips move more, scaled by normalized root distance.

6. **Cooling and convergence**
   - Multiply step size by `cooling` after each iteration.
   - Stop when maximum node displacement is below `tol` or `max_iter` is reached.

## Data structures

A dedicated cache will be constructed once per layout run:

- `row_index`: node id -> row index
- `root_row`
- `tip_rows`
- `internal_rows`
- `edge_parent_rows`
- `edge_child_rows`
- `target_edge_length`
- `depth_norm`
- `outward_dir`
- `mobility`

This keeps the iterative loop numeric and base-R friendly.

## Integration points in `ggtree`

### Core layout dispatch

Update:

- `R/tree-utilities.R`
- `R/method-fortify.R`
- `R/ggtree.R`

so `tree_and_leaf` is treated as an unrooted-like layout.

### Layout-aware branches

Where `ggtree` currently special-cases `daylight`, `equal_angle`, and `ape`, add `tree_and_leaf` as appropriate, especially for:

- `coord_fixed()` setup
- angle recomputation
- radial/slanted branch drawing behavior
- label and annotation utilities that already handle unrooted layouts

## Validation plan

### Unit tests

1. `layout.unrooted(..., layout.method = "tree_and_leaf")` returns finite coordinates.
2. Node and parent structure are unchanged.
3. Output is deterministic for a fixed tree.
4. Tip separation improves relative to the initial layout on a representative tree.
5. `ggtree(..., layout = "tree_and_leaf")` returns a valid plot object.

### Non-goals for tests

- Exact coordinate matching to `TreeAndLeaf`
- Performance benchmarking as a unit test

## Expected trade-offs

### Advantages

- Native `ggtree` integration
- Reproducible static coordinates
- Good fit for annotation-heavy plots
- No new runtime dependency

### Limitations

- It is an inspired implementation, not a verbatim port of the original package
- Pairwise tip repulsion is still quadratic in the number of tips
- Large trees may need future approximation work

## First-version acceptance criteria

- `layout = "tree_and_leaf"` works in `ggtree()` and `fortify()`
- Result is visually leaf-centric and stable
- Tests pass
- Documentation explains that the method is inspired by `TreeAndLeaf` rather than a byte-for-byte reimplementation
