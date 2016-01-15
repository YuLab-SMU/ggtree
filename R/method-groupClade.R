##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="beast"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="codeml"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="gg"),
          function(object, node, group_name) {
              groupClade.ggplot(object, node, group_name)
          })

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="ggplot"),
          function(object, node, group_name) {
              groupClade.ggplot(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="jplace"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )

##' group selected clade
##'
##' 
##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="nhx"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="phylip"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="phylo"),
          function(object, node, group_name="group") {
              groupClade.phylo(object, node, group_name)
          })



groupClade.phylo <- function(object, node, group_name) {
    if (length(node) == 1) {
        clade <- extract.clade(object, node)
        tips <- clade$tip.label
    } else {
        tips <- lapply(node, function(x) {
            clade <- extract.clade(object, x)
            clade$tip.label
        })
    }
    
    groupOTU.phylo(object, tips, group_name)
}


groupClade_ <- function(object, node, group_name) {
    if (is(object, "phylo")) {
        object <- groupClade.phylo(object, node, group_name)
    } else {
        object@phylo <- groupClade.phylo(get.tree(object), node, group_name)
    }
    return(object)
}


groupClade.ggplot <- function(object, nodes, group_name) {
    df <- object$data
    df[, group_name] <- 0
    for (node in nodes) {
        df <- groupClade.df(df, node, group_name)
    }
    df[, group_name] <- factor(df[, group_name])
    object$data <- df
    return(object)
}

groupClade.df <- function(df, node, group_name) {
    foc <- c(node, get.offspring.df(df, node))
    idx <- match(foc, df$node)
    df[idx, group_name] <- max(df[, group_name]) + 1
    return(df)
}
