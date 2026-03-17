# ggtree Roadmap

## Purpose

This document summarizes high-value directions for improving `ggtree` based on the current codebase, recent layout work, and the package's strongest differentiator: using trees as a general annotation canvas rather than only as a tree drawing tool.

## Guiding Principles

- Strengthen `ggtree` as a compositional visualization framework, not only a collection of tree geoms
- Prefer deterministic, testable implementations over opaque heuristics
- Keep new features compatible with the existing `fortify()` and `ggplot2` workflow
- Invest in APIs that make advanced use cases easier without weakening simple defaults
- Improve performance and regression protection as feature complexity grows

## Short-Term Priorities

### 1. Finish paired-tree / tanglegram support

#### Why this matters

This is currently the clearest new capability with strong user value. It builds directly on `ggtree`'s unique advantage: annotated trees that can carry rich side-local metadata.

#### Recommended next steps

- improve `cophylo` parity testing under conditional `phytools` availability
- expand `ggtree` layer replay beyond the current common cases
- support more explicit user control for paired-tree links and styling
- add diagnostics to report crossing counts and optimization results more clearly
- document supported versus unsupported layer types for replay

#### Nice follow-ups

- circular or radial paired-tree layouts
- node-to-node correspondence helpers
- summary tables for matched / unmatched taxa

### 2. Introduce layout diagnostics

#### Why this matters

`ggtree` now has multiple nontrivial layouts (`daylight`, `tree_and_leaf`, custom layouts, paired-tree optimization). Users and developers need visibility into what those algorithms did.

#### Recommended deliverables

- standard layout diagnostics attribute on fortified data
- optional fields such as `iterations`, `before`, `after`, `crossings`, `tip_overlap`, `warnings`
- consistent printing helpers for debugging and tests
- documentation for what diagnostics mean across layout types

### 3. Formalize replayable `ggtree` layers

#### Why this matters

Supporting `ggtree` objects as input is strategically important, but long-term maintainability depends on a clearer contract for which layers can be coordinate-remapped safely.

#### Recommended deliverables

- define a replay contract for side-local layers
- classify current layers into replay-safe, replay-with-translation, and unsupported
- add helper utilities for remapping x/y/node-dependent layer data
- add tests per supported layer category

### 4. Strengthen graphics regression testing

#### Why this matters

More of the package now depends on geometry correctness at build/render time, not just object type checks.

#### Recommended deliverables

- expand `ggplot_build()`-level tests around layout and annotation workflows
- add snapshot-style visual regression tests where practical
- prioritize tests for new layouts, paired-tree views, and large annotation combinations

## Mid-Term Priorities

### 5. Unify layout infrastructure

#### Why this matters

The package now has multiple layout families with overlapping concerns: coordinate generation, angle calculation, diagnostics, optimization, and post-processing.

#### Recommended deliverables

- a more explicit internal layout registry or dispatcher
- shared post-layout hooks for `branch`, `angle`, `branch.x`, `branch.y`
- consistent handling of layout-specific metadata and diagnostics
- clearer separation between layout generation and plot-layer concerns

### 6. Improve scalability for large trees

#### Why this matters

As more annotation-heavy workflows emerge, users will feel performance limits sooner than before.

#### Recommended targets

- optimize `gheatmap()` for large tip counts and wide matrices
- reduce repeated joins or coordinate recomputation in tree-manipulation helpers
- profile heavy label workflows (`geom_tiplab`, highlight layers, paired-tree links)
- add benchmark scripts for representative large-tree use cases

### 7. Build a comparison grammar beyond tanglegram

#### Why this matters

`ggdoubletree()` is likely the first member of a broader family of tree comparison workflows.

#### Possible extensions

- tree-vs-tree comparison helpers beyond matched tips
- consensus / disagreement visualization across trees
- shared-clade detection and highlighting
- higher-level APIs for correspondence and topology difference display

### 8. Expand annotation recipes

#### Why this matters

Many users want common tree-plus-data layouts but do not want to assemble them manually from low-level geoms.

#### Candidate recipes

- tree + heatmap + grouped bars
- tree + statistical summary side panel
- tree + network-style links
- reusable wrappers for common pathogen / microbiome / comparative genomics figures

## Long-Term Priorities

### 9. Interactive linked views

#### Why this matters

The package already has some interactive infrastructure. A future direction is coordinated interaction across tree regions and associated views.

#### Potential goals

- click a clade and update linked panels
- interactive highlight and subtree focus tools
- linked paired-tree exploration
- integration with existing interactive `ggtree` pathways rather than a separate ecosystem

### 10. Multi-tree analytical dashboards

#### Why this matters

For advanced users, the next frontier is not just one annotated tree, but linked analytical views built around multiple trees and multiple data layers.

#### Candidate directions

- comparative dashboards for several related trees
- compact overview + detail layouts
- reusable plot compositions for manuscripts and teaching materials

## Suggested Implementation Order

### Near-term execution order

1. finish `ggdoubletree()` support and replay coverage
2. add layout diagnostics and expose them consistently
3. strengthen graphics regression tests
4. unify internal layout utilities where recent additions overlap
5. optimize large-tree annotation bottlenecks

### Why this order

This sequence compounds well:

- paired-tree work creates immediate user value
- diagnostics and testing reduce risk as complexity grows
- layout unification becomes easier after the new workflows settle
- performance work becomes more targeted once the new APIs stabilize

## Recommended Documentation Strategy

### Package docs

- keep `man/` pages aligned with source-level roxygen
- add task-oriented examples, not only API reference examples

### Book / vignette docs

- add a dedicated paired-tree / tanglegram section
- add more comparison-oriented examples
- include examples that start from annotated `ggtree` objects, not only raw trees

### Developer docs

- keep `.dev/` design notes updated with implementation status
- record what is intentionally deferred, especially for replay support and layout scope

## Risks to Watch

- feature growth without a clear internal layout contract
- more geometry-dependent bugs escaping tests
- replay complexity for arbitrary `ggtree` layers
- user confusion if supported and unsupported paired-tree layers are not documented clearly

## Most Valuable Next Deliverables

If only a few things are tackled next, the best return-on-effort items are:

1. complete paired-tree support with better replay and diagnostics
2. establish a common layout diagnostics contract
3. expand graphics regression testing
4. profile and optimize annotation-heavy large-tree workflows

## Summary

`ggtree` is already strong as a tree visualization package. Its biggest opportunity now is to become even better as a tree-centered visualization system: one that compares trees, composes annotations cleanly, exposes layout behavior transparently, and stays reliable as workflows become more ambitious.
