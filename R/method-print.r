## @method print ggtree
## @export
#print.ggtree <- function(x, ...) {
#    # REF: https://github.com/YuLab-SMU/tidytree/pull/39
#    flag <- getOption(x="check.tbl_tree.verbose", default=TRUE)
#    on.exit(options(check.tbl_tree.verbose=flag))
#    options(check.tbl_tree.verbose=FALSE)
#    plot(x, ...)
#}
