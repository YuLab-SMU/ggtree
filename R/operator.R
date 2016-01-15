##' update tree 
##'
##'
##' @rdname update-TREE
##' @title \%<\%
##' @param pg ggplot2 object
##' @param x update by x
##' @return updated ggplot object
##' @export
##' @author Yu Guangchuang
##' @examples
##' library("ggplot2")
##' nwk <- system.file("extdata", "sample.nwk", package="ggtree")
##' tree <- read.tree(nwk)
##' p <- ggtree(tree) + geom_tippoint(color="#b5e521", alpha=1/4, size=10)
##' p %<% rtree(30)
`%<%` <- function(pg, x) {
    if (! is.tree(x)) {
        stop("input should be a tree object...")
    }
    pg %place% x
}

##' add annotation data to a tree
##'
##'
##' @rdname add-TREEDATA
##' @title \%<+\%
##' @param pg ggplot2 object
##' @param data annotation data
##' @return ggplot object with annotation data added
##' @export
##' @author Yu Guangchuang
##' @examples
##' nwk <- system.file("extdata", "sample.nwk", package="ggtree")
##' tree <- read.tree(nwk)
##' p <- ggtree(tree) 
##' dd <- data.frame(taxa=LETTERS[1:13], 
##'    		 place=c(rep("GZ", 5), rep("HK", 3), rep("CZ", 4), NA),
##'              value=round(abs(rnorm(13, mean=70, sd=10)), digits=1))
##' row.names(dd) <- NULL
##' p %<+% dd + geom_text(aes(color=place, label=label), hjust=-0.5)
`%<+%` <- function(pg, data) {
    if (! is.data.frame(data)) {
        stop("input should be a data.frame...")
    }
    pg %add% data
}

`%place%` <- function(pg, tree) {
    param <- attr(pg, "param")
    pg$data <- fortify(tree,
                       layout        = param[["layout"]],
                       yscale        = param[["yscale"]],
                       ladderize     = param[["ladderize"]],
                       right         = param[["right"]],
                       branch.length = param[["branch.length"]],
                       ndigits       = param[["ndigits"]])
    return(pg)
}


`%add%` <- function(p, data) {
    p$data <- p$data %add2% data
    return(p)
}

`%add2%` <- function(d1, d2) {
    if ("node" %in% colnames(d2)) {
        cn <- colnames(d2)
        ii <- which(cn %in% c("node", cn[!cn %in% colnames(d1)]))
        d2 <- d2[, ii]
        dd <- merge(d1, d2, by.x="node", by.y="node", all.x=TRUE)
    } else {
        d2[,1] <- as.character(d2[,1])
        dd <- merge(d1, d2, by.x="label", by.y=1, all.x=TRUE)
    }
    dd <- dd[match(d1$node, dd$node),]
    return(dd)
}

##' pipe
##' @importFrom magrittr %>%
##' @name %>%
##' @export
##' @rdname pipe
##' @param lhs left hand side
##' @param rhs right hand side
##' @usage lhs \%>\% rhs
##' @seealso
##' \link[magrittr]{pipe}
NULL
