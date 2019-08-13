# ggtree 1.17.4

+ now `geom_cladelabel` supports `unrooted` and `daylight` layouts (2019-08-14, Wed)
  - by integrating `geom_cladelabel2`
+ defined `nodelab` method for ggtree to convert node number to label (2019-08-09, Fir)
+ redefined `nodeid` as S3 generic in `tidytree` v=0.2.6 
  - change the original function as a method for ggtree
  - move the `nodeid` function for tree object to treeio
+ defunct `gzoom` function
+ introduce `rootnode` parameter in `geom_tree` with default = TRUE and behave as previous version (2019-08-08, Thu)
  - the invisible root to itself line segment have advantage for the number of line segments is consistent with the number of nodes. 
  - if `rootnode = FALSE`, there will be no line segment of root to itself.
+ extend `gheatmap` to support collapsed node (2019-08-06, Tue)
  - <https://github.com/GuangchuangYu/ggtree/pull/243>
+ support `hclust` and `dendrogram` (2019-07-31, Wed)

# ggtree 1.17.3

+ remove re-export treeio parser function, user now need to load treeio explictly (2019-07-24, Wed) 
+ export `layout_circular`, `layout_fan` and `layout_rectangular`
+ `layout_dendrogram` and `theme_dendrogram` 
  - <https://yulab-smu.github.io/treedata-book/chapter10.html#dendrogram>
+ `scale_x_range` for adding second x-axis for `geom_range` (2019-07-23, Tue)
+ change `branch.length` parameter to `center` for `geom_range`

# ggtree 1.17.2

+ extend `expand` according to the change of `collapse` (2019-07-11, Thu)
+ `mode` parameter in `collapse`
+ `geom_tiplab` now works with 'circular' and 'fan' layouts (2019-07-05, Fri)
+ `geom_inset` for adding subplots to specific nodes (see also the `inset` function introduced in v=1.3.8)

# ggtree 1.17.1

+ `facet_data` to extract data used in `facet_plot` or `geom_facet` (2019-07-02, Tue)
+ `continuous` parameter in `geom_tree` to to continuous color edge from parent to child (2019-09-25, Tue)
  - <https://yulab-smu.github.io/treedata-book/chapter4.html#color-tree>
+ `root.position` parameter for `fortify` and `ggtree` (2019-05-27, Mon)
+ `geom_facet`, a geom layer version of `facet_plot` (2019-05-23, Thu)
+ update `scale_x_ggtree`, now we can use `gheatmap() + scale_x_ggtree()` (2019-05-22, Wed)
+ extend `xlim_expand` to work with `ggplot2` (2019-05-20, Tue)
  - <https://yulab-smu.github.io/treedata-book/chapter9.html#xlim_expand>
+ add `legend_title` variable in `gheatmap` (2019-05-16, Thu)

# ggtree 1.16.0

+ Bioconductor 3.9 release

# ggtree 1.15.6

+ compatible with range is NULL in `geom_range` (2019-04-27, Sat)
  - <https://groups.google.com/d/msg/bioc-ggtree/yNzjtioVVGU/MCh3MPl_CwAJ>
+ remove `getChild`, `getChild.df`, `getParent`, `getParent.df`, `getSibling`, `getAncestor` and `getAncestor.df`,
  instead use `child`, `parent`, `sibling` and `ancestor` methods implemented in `tidytree` and `treeio` (2019-01-30, Wed)
+ remove `get.offspring.df` and `get.offspring.tip` and instead use `tidytree::offspring` (2019-01-28, Mon)
+ `facet_widths` function to set relative widths of facet panels (2019-01-28, Mon)
  - the output is `ggplotify::as.ggplot(grid_object)`, so it is not the original `ggtree` object.

# ggtree 1.15.5

+ bug fixed of `theme_tree2` (2019-01-14, Mon)
  - <https://github.com/GuangchuangYu/ggtree/issues/218>  
+ mv `rescale_tree` to `treeio` (2019-01-11, Fri)

# ggtree 1.15.4

+ reimplement `MRCA` as a method inherited from `tidytree` (2019-01-10, Thu)
+ mv vignettes to [treedata-book](https://yulab-smu.github.io/treedata-book/) 

# ggtree 1.15.3

+ move `reroot` method to `treeio` package and rename to `root` (2018-12-28, Fri)
+ bug fixed for setting `branch.length="none"` in unrooted layouts (2018-12-26, Wed)
  - bug introduced in <https://github.com/GuangchuangYu/ggtree/pull/201>

# ggtree 1.15.2

+ compatible with `tibble` v=2.0.0 (2018-11-29, Thu)

# ggtree 1.15.1

+ now `revts` also reverse the `branch` column (2018-11-11, Sun)
  - <https://groups.google.com/d/msgid/bioc-ggtree/50765a34-53e5-44d2-bb8f-6d20a11fa890%40googlegroups.com>
+ better msg when taxa name not consistent in sequence and tree files with `msaplot`. (2018-11-07, Wed)
  - <https://github.com/GuangchuangYu/ggtree/issues/172#issuecomment-436585370>
  
# ggtree 1.14.0

+ Bioconductor 3.8 release

# ggtree 1.13.6

+ set `scale_y_continuous(expand = c(0, 0))` for `gheatmap` when `colnames = FALSE` (2018-10-17, Wed)
  - <https://github.com/GuangchuangYu/ggtree/issues/204>
+ made data usable with treedata in 'equal_angle' and 'daylight' layouts (2018-10-11, Thu)
  - <https://github.com/GuangchuangYu/ggtree/pull/201>
  
# ggtree 1.13.5

+ enable additional parameters passed to `geom_rootedge` (2018-09-13, Thu)
  - <https://github.com/GuangchuangYu/ggtree/issues/195>
+ `facet_labeller` to label panels of `facet_plot` output (2018-09-10, Mon)
  - <https://groups.google.com/forum/?utm_medium=email&utm_source=footer#!msg/bioc-ggtree/nAl3bNswxQM/crBwKtS3BAAJ>
  
# ggtree 1.13.3

+ update `viewClade` according to the change of `ggplot2` (2018-08-07, Tue)
  - <https://github.com/GuangchuangYu/ggtree/issues/188>
  
# ggtree 1.13.2

+ xmax_adjust  in `viewClade` (2018-07-21, Sat)
  - <https://github.com/GuangchuangYu/ggtree/issues/187>
+ update rd of `facet_plot` by adding description and example (2018-07-19, Thu)
+ order by y in `facet_plot` (2018-07-09, Mon)
+ `extend` parameter in `geom_cladelabel` and `geom_strip` supports specifying
  extension of both sides (2018-07-06, Fri)
+ `geom_nodelab2` for circular layout (2018-07-03, Tue)
  - <https://groups.google.com/d/msgid/bioc-ggtree/cbe5e1e3-99c4-481d-8523-d5b50cf0460b%40googlegroups.com>
+ re-export `ggplot2::fortify`
  - <https://groups.google.com/d/msgid/bioc-ggtree/bf7a9b3c-9a8f-4d8d-a782-1b467fba38bc%40googlegroups.com>

# ggtree 1.13.1

+ `geom_nodepoint` & `geom_tippoint` now compatible with `ggplot2` v 2.2.1 &
  3.0.0 (2018-06-26, Tue)
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

