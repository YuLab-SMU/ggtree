##' parse output from r8s
##'
##' 
##' @title read.r8s
##' @param file r8s output log file
##' @return r8s instance
##' @export
##' @author Guangchuang Yu
read.r8s <- function(file) {
    r8s <- readLines(file)
    label_idx <- grep("\\[\\w+\\sDESCRIPTION\\sof\\stree\\s.*\\]", r8s)
    tree_idx <- grep("^tree\\s.*\\s=\\s", r8s)
    if (length(label_idx) != length(tree_idx)) {
        stop("fail to parse the file...")
    }

    tree_text <- gsub("^tree\\s.*\\s=\\s", "", r8s[tree_idx])
    trees <- read.tree(text=tree_text)

    label <- gsub("^\\[(\\w+)\\s.*", "\\1", r8s[label_idx])
    names(trees) <- label

    new("r8s",
        file = filename(file),
        fields = label,
        phylo = trees)
}




##' @rdname groupClade-methods
##' @exportMethod groupClade
##' @param tree which tree selected
setMethod("groupClade", signature(object="r8s"),
          function(object, node, group_name="group", tree="TREE") {
              groupClade_(get.tree(object)[[tree]], node, group_name)
          })

##' @rdname scale_color-methods
##' @exportMethod scale_color
##' @param tree which tree selected
setMethod("scale_color", signature(object="r8s"),
          function(object, by="bootstrap", tree="TREE") {
              scale_color_(get.tree(object)[[tree]], by)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
##' @param tree which tree selected
setMethod("gzoom", signature(object="r8s"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7), tree="TREE") {
              gzoom.phylo(get.tree(object)[[tree]], focus, subtree, widths)
          })


##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="r8s"),
          function(object,...) {
              object@phylo
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="r8s"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )
