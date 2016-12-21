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
    mrsd      <- get("mrsd", envir=pg$plot_env)
    layout    <- get("layout", envir = pg$plot_env)
    yscale    <- get("yscale", envir = pg$plot_env)
    ladderize <- get("ladderize", envir = pg$plot_env)
    right     <- get("right", envir = pg$plot_env)
    branch.length <- get("branch.length", envir = pg$plot_env)
    ndigits <- get("ndigits", envir = pg$plot_env)


    pg$data <- fortify(tree,
                       layout        = layout,
                       yscale        = yscale,
                       ladderize     = ladderize,
                       right         = right,
                       branch.length = branch.length,
                       ndigits       = ndigits,
                       mrsd          = mrsd)
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
        d2[,1] <- as.character(unlist(d2[,1])) ## `unlist` to work with tbl_df
        dd <- merge(d1, d2, by.x="label", by.y=1, all.x=TRUE)
    }
    dd <- dd[match(d1$node, dd$node),]
    return(dd)
}

##' update data with tree info (y coordination and panel)
##'
##'
##' @rdname add_TREEINFO
##' @title \%+>\%
##' @param p tree view
##' @param data data.frame
##' @return updated data.frame
##' @importFrom methods is
##' @export
##' @author Guangchuang Yu
`%+>%` <- function(p, data) {
    df <- p$data
    lv <- levels(df$.panel)

    if (is(data, "GRanges") || is(data, "GRangesList")) {
        names(data) <- df$y[match(names(data), df$label)]
        res <- data[order(as.numeric(names(data)))]
        mcols <- get_fun_from_pkg("GenomicRanges", "mcols")
        `mcols<-` <- get_fun_from_pkg("GenomicRanges", "`mcols<-`")
        mcols(res)$.panel <- factor(lv[length(lv)], levels=lv)
    } else if (is(data, "data.frame") || is(data, "tbl_df")) {
        data <- as.data.frame(data)
        ## res <- merge(df[, c('label', 'y')], data, by.x='label', by.y=1) ## , all.x=TRUE)
        res <- merge(df[, !names(df) %in% c('node', 'parent', 'x', 'branch', 'angle')], data, by.x='label', by.y=1)
        res$.panel <- factor(lv[length(lv)], levels=lv)
    } else {
        stop("input 'data' is not supported...")
    }
    return(res)
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
