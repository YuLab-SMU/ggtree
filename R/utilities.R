

##' @importFrom ggplot2 last_plot
get_tree_view <- function(tree_view) {
    if (is.null(tree_view))
        tree_view <- last_plot()

    return(tree_view)
}

get_layout <- function(tree_view = NULL) {
    plot <- get_tree_view(tree_view)
    layout <- get("layout", envir = plot$plot_env)
    if (!is(layout, "character")) {
        layout <- attr(plot$data, "layout")
    }
    return(layout)
}

reverse.treeview <- function(tv) {
    tv$data <- reverse.treeview.data(tv$data)
    return(tv)
}

reverse.treeview.data <- function(df) {
    root <- df$node[df$node == df$parent]
    df$x <- getXcoord2(df$x, root, df$parent, df$node,
                       df$length, start=max(df$x), rev=TRUE)
    return(df)
}


color_scale <- function(c1="grey", c2="red", n=100) {
    pal <- grDevices::colorRampPalette(c(c1, c2))
    colors <- pal(n)
    return(colors)
}

getIdx <- function(v, MIN, MAX, interval=NULL) {
    res <- sapply(v, getIdx_internal, MIN=MIN, MAX=MAX, interval=interval)
    attr(res, "interval") <- interval
    return(res)
}

getIdx_internal <- function(v, MIN, MAX, interval=NULL) {
    if (is.na(v)) {
        return(NA)
    }
    if ( MIN == MAX ) {
        return(100)
    }
    res <- max(which(interval <= v))
    return(res)
}


get_color_attribute <- function(p) {
    p$data[, "color"]
}

is.tree_attribute <- function(df, var) {
    if(length(var) == 1 &&
       !is.null(var)    &&
       var %in% colnames(df)) {
        return(TRUE)
    }
    return(FALSE)
}

is.tree_attribute_ <- function(p, var) {
    is.tree_attribute(p$data, var)
}



roundDigit <- function(d) {
    i <- 0
    while(d < 1) {
        d <- d * 10
        i <- i + 1
    }
    round(d)/10^i
}


globalVariables(".")

## ## . function was from plyr package
## ##' capture name of variable
## ##'
## ##' @rdname dotFun
## ##' @export
## ##' @title .
## ##' @param ... expression
## ##' @param .env environment
## ##' @return expression
## ##' @examples
## ##' x <- 1
## ##' eval(.(x)[[1]])
## . <- function (..., .env = parent.frame()) {
##     structure(as.list(match.call()[-1]), env = .env, class = "quoted")
## }


## from ChIPseeker
## @importFrom grDevices colorRampPalette
getCols <- function (n) {
    col <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
             "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
             "#ccebc5", "#ffed6f")
    col2 <- c("#1f78b4", "#ffff33", "#c2a5cf", "#ff7f00", "#810f7c",
              "#a6cee3", "#006d2c", "#4d4d4d", "#8c510a", "#d73027",
              "#78c679", "#7f0000", "#41b6c4", "#e7298a", "#54278f")
    col3 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
              "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
              "#ffff99", "#b15928")
    grDevices::colorRampPalette(col3)(n)
}

message_wrap <- function(...){
    msg <- .return_wrap(...)
    message(msg)
}

.return_wrap <- function(...){
    msg <- paste(..., collapse = "", sep = "")
    wrapped <- strwrap(msg, width = getOption("width") - 2) %>%
        glue::glue_collapse(., "\n", last = "\n")
    wrapped
}

##
##
## use ape::multi2di
##
##
## ##' convert polytomy to binary tree
## ##'
## ##' as.binary method for \code{phylo} object
## ##' @rdname as.binary
## ##' @return binary tree
## ##' @method as.binary phylo
## ##' @importFrom ape is.binary.tree
## ##' @export
## ##' @author Guangchuang Yu \url{http://ygc.name}
## ##' @examples
## ##' require(ape)
## ##' tr <- read.tree(text="((A, B, C), D);")
## ##' is.binary.tree(tr)
## ##' tr2 <- as.binary(tr)
## ##' is.binary.tree(tr2)
## as.binary.phylo <- function(tree, ...) {
##     if(is.binary.tree(tree)) {
##         message("The input tree is already binary...")
##         invisible(tree)
##     }
##     polyNode <- tree$edge[,1] %>% table %>% '>'(2) %>%
##         which %>% names %>% as.numeric
##     N <- getNodeNum(tree)
##     ii <- 0
##     for (pn in polyNode) {
##         idx <- which(tree$edge[,1] == pn)
##         while(length(idx) >2) {
##             ii <- ii + 1
##             newNode <- N+ii
##             tree$edge[idx[-1],1] <- newNode
##             newEdge <- matrix(c(tree$edge[idx[1],1], newNode), ncol=2)
##             tree$edge <- rbind(tree$edge, newEdge)
##             idx <- idx[-1]
##         }
##     }
##     tree$Nnode <- tree$Nnode+ii
##     tree$edge.length <- c(tree$edge.length, rep(0, ii))
##     return(tree)
## }
