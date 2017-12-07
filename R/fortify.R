##' @importFrom ggplot2 fortify
##' @method fortify treedata
##' @export
fortify.treedata <- function(model, data,
                             layout        = "rectangular",
                             yscale        = "none",
                             ladderize     = TRUE,
                             right         = FALSE,
                             branch.length = "branch.length",
                             mrsd          = NULL,
                             as.Date       = FALSE, ...) {

    model <- set_branch_length(model, branch.length)

    fortify.phylo(model, data,
                  layout        = layout,
                  yscale        = yscale,
                  ladderize     = ladderize,
                  right         = right,
                  branch.length = branch.length,
                  mrsd          = mrsd,
                  as.Date       = as.Date, ...)
}

##' @importFrom ape ladderize
##' @importFrom treeio as.phylo
##' @importFrom treeio Nnode
##' @importFrom tibble data_frame
##' @importFrom dplyr full_join
##' @importFrom tidytree as_data_frame
##' @method fortify phylo
##' @export
fortify.phylo <- function(model, data,
                          layout        = "rectangular",
                          ladderize     = TRUE,
                          right         = FALSE,
                          branch.length = "branch.length",
                          mrsd          = NULL,
                          as.Date       = FALSE,
                          yscale        = "none",
                          ...) {

    x <- as.phylo(model) ## reorder.phylo(get.tree(model), "postorder")
    if (ladderize == TRUE) {
        x <- ladderize(x, right=right)
    }

    if (! is.null(x$edge.length)) {
        if (anyNA(x$edge.length)) {
            warning("'edge.length' contains NA values...\n## setting 'edge.length' to NULL automatically when plotting the tree...")
            x$edge.length <- NULL
        }
    }

    if (is.null(x$edge.length) || branch.length == "none") {
        xpos <- getXcoord_no_length(x)
    } else {
        xpos <- getXcoord(x)
    }

    ypos <- getYcoord(x)
    N <- Nnode(x, internal.only=FALSE)
    xypos <- data_frame(node=1:N, x=xpos, y=ypos)

    df <- as_data_frame(model)

    res <- full_join(df, xypos, by = "node")

    ## add branch mid position
    res <- calculate_branch_mid(res)

    if (!is.null(mrsd)) {
        res <- scaleX_by_time_from_mrsd(res, mrsd, as.Date)
    }

    if (layout == "slanted") {
        res <- add_angle_slanted(res)
    } else {
        ## angle for all layout, if 'rectangular', user use coord_polar, can still use angle
        res <- calculate_angle(res)
    }
    scaleY(as.phylo(model), res, yscale, layout, ...)
}

##' @importFrom treeio get_tree_data
set_branch_length <- function(tree_object, branch.length) {
    if (branch.length == "branch.length") {
        return(tree_object)
    } else if (branch.length == "none") {
        tree_object@phylo$edge.length <- NULL
        return(tree_object)
    }

    if (is(tree_object, "phylo")) {
        return(tree_object)
    }

    tree_anno <- get_tree_data(tree_object)
    tree_anno$node <- as.integer(tree_anno$node)

    phylo <- as.phylo(tree_object)

    cn <- colnames(tree_anno)
    cn <- cn[!cn %in% c('node', 'parent')]

    length <- match.arg(branch.length, cn)

    if (all(is.na(as.numeric(tree_anno[[length]])))) {
        stop("branch.length should be numerical attributes...")
    }

    edge <- as_data_frame(phylo$edge)
    colnames(edge) <- c("parent", "node")

    dd <- full_join(edge, tree_anno, by = "node")

    dd <- dd[match(edge[['node']], dd[['node']]),]
    len <- unlist(dd[[length]])
    len <- as.numeric(len)
    len[is.na(len)] <- 0

    phylo$edge.length <- len

    tree_object@phylo <- phylo
    return(tree_object)
}


calculate_angle <- function(data) {
    data$angle <- 360/(diff(range(data$y)) + 1) * data$y
    return(data)
}



scaleY <- function(phylo, df, yscale, layout, ...) {
    if (yscale == "none") {
        return(df)
    }
    if (! yscale %in% colnames(df)) {
        warning("yscale is not available...\n")
        return(df)
    }
    if (is.numeric(df[[yscale]])) {
        y <- getYcoord_scale_numeric(phylo, df, yscale, ...)
        ## if (order.y) {
        ##     y <- getYcoord_scale2(phylo, df, yscale)
        ## } else {
        ##     y <- getYcoord_scale(phylo, df, yscale)
        ## }
    } else {
        y <- getYcoord_scale_category(phylo, df, yscale, ...)
    }

    df[, "y"] <- y

    return(df)
}
