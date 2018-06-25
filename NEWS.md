# ggtree 1.13.0.007

+ `geom_nodepoint` & `geom_tippoint` now compatible with `ggplot2` v 2.2.1 &
  2.2.2 (2018-06-25, Mon)
+ `geom_rootedge` (2018-06-13, Wed)
  - <https://github.com/GuangchuangYu/ggtree/issues/183>
+ `clade_name` parameter added in `collapse` (2018-05-28, Mon)
  - <https://groups.google.com/forum/?utm_medium=email&utm_source=footer#!msg/bioc-ggtree/0H1PCJPlI1Q/DeZf9EVhBQAJ>
+ bug fixed of `gheatmap` for rowname subsetting is partial match in R
  (2018-05-23, Wed)
  - <https://github.com/GuangchuangYu/ggtree/issues/182>
+ compatible with ggplot2 2.2.1.9000 for `geom_text2(parse="emoji")`
+ typo on vignettes (2018-05-08, Tue)
  - <https://github.com/GuangchuangYu/ggtree/pull/178>, thanks [@abichat](https://github.com/abichat)
+ compatible with ggplot2 2.2.1.9000 (2018-05-02, Wed)
  - incorporate newly introduce parameter `linejoin` and `arrow.fill` in `geom_segment`

# ggtree 1.12.0

+ Bioconductor 3.7 release (2018-05-01, Tue)

# ggtree 1.11.6

+ reexport `treeio::read.iqtree` & `treeio::read.astral`(2018-04-17, Tue)
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

