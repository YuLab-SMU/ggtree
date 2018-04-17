# ggtree 1.11.6

+ reexport `treeio::read.iqtree` (2018-04-17, Tue)
+ now geom_treescale will plot the text that can **always** be separated from
  the bar (2018-03-07, Wed)
+ optimize daylight layout
    - <https://github.com/GuangchuangYu/ggtree/pull/165>
    - <https://github.com/GuangchuangYu/ggtree/pull/169>
+ geom_tiplab2 works with mapping = aes(subset) defined by users  (2018-02-24, Sta)
    - <https://groups.google.com/d/msgid/bioc-ggtree/72e18c5b-0ce5-4c25-80fa-3e2dabda8f72%40googlegroups.com>
+ fixed overlapping branch after `flip`
    - <https://github.com/GuangchuangYu/ggtree/issues/167>

# ggtree 1.11.5

+ support passing `aes(subset)` in geom_tippoint (2018-01-30, Tue)
+ support passing `aes(subset)` in geom_nodepoint (2018-01-26, Fri)
+ fine tune y position after flip; flip compatible with collapse
    - <https://groups.google.com/d/msgid/bioc-ggtree/1d06342c-9645-4f71-9e66-7d7a7099bf0b%40googlegroups.com?utm_medium=email&utm_source=footer>

# ggtree 1.11.4

+ re-implement collapse as collapse.ggtree method by importing dplyr::collapse
  generic to prevent function name collision (2018-01-03, Wed)
+ update treeVisualization vignette, with more layout examples added (2017-12-22, Fri)
+ update vignette (2017-12-21, Thu)
    - remove ggtreeUtilities.Rmd
    - merge treeAnnotation and advanceTreeAnnotation

