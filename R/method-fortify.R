##' @importFrom ape ladderize
##' @importFrom treeio as.phylo
##' @importFrom treeio Nnode
##' @importFrom dplyr full_join
##' @importFrom dplyr mutate_
##' @importFrom tidytree as_tibble
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
                          root.position = 0,
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

    if (layout %in% c("equal_angle", "daylight")) {
        res <- layout.unrooted(model, layout.method = layout, branch.length = branch.length, ...)
    } else {
        if (is.null(x$edge.length) || branch.length == "none") {
            xpos <- getXcoord_no_length(x)
        } else {
            xpos <- getXcoord(x)
        }

        ypos <- getYcoord(x)
        N <- Nnode(x, internal.only=FALSE)
        xypos <- tibble::data_frame(node=1:N, x=xpos + root.position, y=ypos)

        df <- as_tibble(model) %>%
            mutate_(isTip = ~(! node %in% parent))

        res <- full_join(df, xypos, by = "node")
    }

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
    res <- scaleY(as.phylo(model), res, yscale, layout, ...)
    class(res) <- c("tbl_tree", class(res))
    return(res)
}


##' @method fortify multiPhylo
##' @export
fortify.multiPhylo <-  function(model, data,
                                layout    = "rectangular",
                                ladderize = TRUE,
                                right     = FALSE,
                                mrsd      = NULL, ...) {

    df.list <- lapply(model, function(x) fortify(x, layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...))
    if (is.null(names(model))) {
        names(df.list) <- paste0("Tree ", "#", seq_along(model))
    } else {
        names(df.list) <- names(model)
    }
    df <- do.call("rbind", df.list)
    df$.id <- rep(names(df.list), times=sapply(df.list, nrow))
    df$.id <- factor(df$.id, levels=names(df.list))
    return(df)
}

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


##' @method fortify phylo4
##' @importFrom treeio as.phylo
##' @export
fortify.phylo4 <- function(model, data,
                           layout    = "rectangular",
                           yscale    = "none",
                           ladderize = TRUE,
                           right     = FALSE,
                           mrsd      = NULL,
                           ...) {
    phylo <- as.phylo(model)
    df <- fortify.phylo(phylo, data,
                        layout, ladderize, right, mrsd=mrsd, ...)
    scaleY(phylo, df, yscale, layout, ...)
}

##' @method fortify phylo4d
##' @importFrom treeio as.treedata
##' @export
fortify.phylo4d <- function(model, data,
                            layout        = "rectangular",
                            yscale        = "none",
                            ladderize     = TRUE,
                            right         = FALSE,
                            branch.length = "branch.length",
                            mrsd          = NULL,
                            ...) {
    fortify(as.treedata(model), data, layout, yscale, ladderize, right, branch.length, mrsd, ...)
}


##' @method fortify obkData
##' @export
fortify.obkData <- function(model, data,
                            layout    = "rectangular",
                            ladderize = TRUE,
                            right     = FALSE,
                            mrsd      = NULL, ...) {

    df <- fortify(model@trees[[1]], layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...)

    meta.df <- model@dna@meta
    meta.df <- data.frame(taxa=rownames(meta.df), meta.df)
    loc <- model@individuals
    loc <- data.frame(individualID=rownames(loc), loc)
    meta_loc <- merge(meta.df, loc, by="individualID")
    meta_loc <- meta_loc[,-1]

    df <- merge(df, meta_loc, by.x="label", by.y="taxa", all.x=TRUE)
    df <- df[order(df$node, decreasing = FALSE),]
    return(df)
}

##' @method fortify phyloseq
##' @export
fortify.phyloseq <- function(model, data,
                             layout    = "rectangular",
                             ladderize = TRUE,
                             right     = FALSE,
                             mrsd      = NULL, ...) {

    df <- fortify(model@phy_tree, layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...)
    phyloseq <- "phyloseq"
    require(phyloseq, character.only=TRUE)
    psmelt <- eval(parse(text="psmelt"))
    dd <- psmelt(model)
    if ('Abundance' %in% colnames(dd)) {
        dd <- dd[dd$Abundance > 0, ]
    }

    data <- merge(df, dd, by.x="label", by.y="OTU", all.x=TRUE)
    spacing <- 0.02
    idx <- with(data, sapply(table(node)[unique(node)], function(i) 1:i)) %>% unlist
    data$hjust <- spacing * idx * max(data$x)
    ## data$hjust <- data$x + hjust

    data[order(data$node, decreasing = FALSE), ]
}


## fortify.cophylo <- function(model, data, layout="rectangular",
##                             ladderize=TRUE, right=FALSE, mrsd = NULL, ...) {
##     trees <- model$trees
##     df.list <- lapply(trees, function(x) fortify(x, layout=layout, ladderize=ladderize, right=right, mrsd=mrsd, ...))
##     df1 <- df.list[[1]]
##     df2 <- df.list[[2]]
##     df2$x <- max(df2$x) + df2$x * -1 + max(df1$x) * 1.1
##     df2$parent <- df2$parent+nrow(df1)
##     df <- rbind(df1, df2)
##     ggplot(df) + geom_tree()
## }

