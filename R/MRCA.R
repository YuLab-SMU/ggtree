##' Find Most Recent Common Ancestor among a vector of tips
##'
##'
##' @title MRCA
##' @param obj supported tree object or ggplot object
##' @param tip a vector of mode numeric or character specifying the tips
##' @return MRCA of two or more tips
##' @importFrom ape getMRCA
##' @export
##' @author Guangchuang Yu
MRCA <- function(obj, tip) {
    if (is(obj,"gg")) {
        return(getMRCA.df(obj$data, tip))
    }

    getMRCA(as.phylo(obj), tip)
}


getMRCA.df <- function(data, tip) {
    if (length(tip) <= 1)
        return(NULL)

    anc <- getMRCA.df_internal(data, tip[1], tip[2])
    if (length(tip) == 2) {
        return(anc)
    }
    for (i in 3:length(tip)) {
        anc <- getMRCA.df_internal(data, tip[i], anc)
    }
    return(anc)
}


getMRCA.df_internal <- function(data, node1, node2) {
    node1 <- which(node1 == data$label | node1 == data[, "node"])
    node2 <- which(node2 == data$label | node2 == data[, "node"])

    anc1 <- get.ancestor.df(data, node1)
    anc2 <- get.ancestor.df(data, node2)

    if(is.null(anc1)){
      print("Warning getMRCA.df_internal(): node1 is root")
    } else if(is.null(anc2)){
      print("Warning getMRCA.df_internal(): node2 is root")
    }

    # Return common ancestors.
    intersect(c(node1, anc1), c(node2, anc2))[1]
}


get.ancestor.df <- function(df, node) {
    pp <- getParent.df(df, node)
    pp <- pp[pp != 0]
    if (length(pp) == 0) {
      #stop("input node is root...")
      cat("WARNING: input node ",node," is root.")
      return(NULL) # root has no ancestor
    }
    i <- 1
    while(i <= length(pp)) {
        pp <- c(pp, getParent.df(df, pp[i]))
        pp <- pp[pp!=0]
        i <- i+1
    }
    return(pp)
}
