# TODO LIST

+ [ ]update ggtree man files
+ [ ] extend `geom_cladelabel` to support aes mapping   
+ [ ] `geom_cladelabel` internally calls `geom_cladelabel_rectangular` and `geom_cladelabel2`. The parameter `horizontal` introduced in `geom_cladelabel_retangular` should also be supported in `geom_cladelabel2`
+ [ ] The `daylight` algorithm is quite slow compare to `ggraph` and needs to  be optimized
   - <https://github.com/thomasp85/ggraph/commit/14de66f1225336179b4598cb42a4beda95682211>
+ [ ] Implement quarter ellipse layout to draw tree
+ [x] update `geom_hilight` to support `geom_hilight(data = mydata, node = selected_node)` (2020-09-03, Thu)
   - <https://yulab-smu.top/treedata-book/chapter2.html#ggtree-fortify>
   - <https://groups.google.com/g/bioc-ggtree/c/swpAjXLZMOQ/m/igXf9crVBwAJ>
   - <https://github.com/YuLab-SMU/ggtree/pull/341>