# TODO LIST

+ tanglegram support
  - <https://yulab-smu.top/treedata-book/chapter2.html#ggtree-fortify>
  - <https://www.rdocumentation.org/packages/phytools/versions/0.7-70/topics/cophylo> 
+ `fortify` method for `phyloseq` object should return a tidy data.frame
  - maybe we can defined another object (inherited from treedata?) and provide converter for `phyloseq`
+ update ggtree man files
+ The `daylight` algorithm is quite slow compare to `ggraph` and needs to  be optimized
  - <https://github.com/thomasp85/ggraph/commit/14de66f1225336179b4598cb42a4beda95682211>
+ ~~mplement quarter ellipse layout to draw tree~~
+ ~~A new geom, `geom_cladelab`, that supports aes mapping and the parameter `horizontal` works for all layouts~~
  - <https://github.com/YuLab-SMU/ggtree/pull/342>
  - extend `geom_cladelabel` to support aes mapping
  - `geom_cladelabel` internally calls `geom_cladelabel_rectangular` and `geom_cladelabel2`. The parameter `horizontal` introduced in `geom_cladelabel_retangular` should also be supported in `geom_cladelabel2`
+ ~~update `geom_hilight` to support `geom_hilight(data = mydata, node = selected_node)`~~
   - <https://yulab-smu.top/treedata-book/chapter2.html#ggtree-fortify>
   - <https://groups.google.com/g/bioc-ggtree/c/swpAjXLZMOQ/m/igXf9crVBwAJ>
   - <https://github.com/YuLab-SMU/ggtree/pull/341>
