#' Calculate angle coordinates for circular layouts
#'
#' @param data A data frame containing tree node coordinates
#' @return A data frame with added 'angle' column
#' @noRd
calculate_angle <- function(data) {
    if (!"y" %in% names(data)) {
        stop("Input data must contain 'y' column")
    }
    y_range <- diff(range(data$y, na.rm = TRUE))
    if (y_range == 0) {
        warning("Y range is 0, setting all angles to 0")
        data$angle <- 0
    } else {
        data$angle <- 360 / (y_range + 1) * data$y
    }
    return(data)
}


#' Scale Y coordinates based on specified variable
#'
#' @param phylo Phylogenetic tree object
#' @param df Data frame with tree data
#' @param yscale Variable name to use for scaling
#' @param layout Tree layout
#' @param ... Additional arguments passed to scaling functions
#' @return Modified data frame with scaled y coordinates
#' @noRd
scaleY <- function(phylo, df, yscale, layout, ...) {
    if (yscale == "none") {
        return(df)
    }
    if (!yscale %in% colnames(df)) {
        warning("yscale variable '", yscale, "' is not available in data frame")
        return(df)
    }

    if (is.numeric(df[[yscale]])) {
        y <- getYcoord_scale_numeric(phylo, df, yscale, ...)
    } else {
        y <- getYcoord_scale_category(phylo, df, yscale, ...)
    }

    df[, "y"] <- y
    return(df)
}


#' Adjust tip edge lengths for hclust objects
#'
#' @param df Data frame with tree data
#' @param phylo Phylogenetic tree object
#' @param layout Tree layout
#' @param branch.length Branch length specification
#' @return Modified data frame with adjusted tip edge lengths
#' @noRd
adjust_hclust_tip_edge_len <- function(
    df,
    phylo,
    layout,
    branch.length = "branch.length"
) {
    # Determine tip.edge.len based on object type
    if (inherits(phylo, 'treedata')) {
        tip.edge.len <- attr(phylo@phylo, 'tip.edge.len')
    } else {
        tip.edge.len <- attr(phylo, 'tip.edge.len')
    }

    # Apply adjustments if conditions met
    if (
        !is.null(tip.edge.len) &&
            branch.length != 'none' &&
            layout %in% c('rectangular', 'dendrogram', 'roundrect')
    ) {
        mx <- max(df$x, na.rm = TRUE)
        df$x <- df$x - mx
        df$branch <- df$branch - mx
        df[df$isTip, "x", drop = TRUE] <- tip.edge.len
        attr(df, 'revts.done') <- TRUE
    }
    return(df)
}
