##' scale x for tree with gheatmap
##'
##'
##' @title scale_x_ggtree
##' @param breaks breaks for tree
##' @param labels lables for corresponding breaks
##' @return updated tree view
##' @importFrom ggplot2 waiver
##' @export
##' @author Guangchuang Yu
scale_x_ggtree <- function(breaks = waiver(), labels = waiver()) {
    structure(list(breaks = breaks, labels = labels), class="scale_ggtree")
}


##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_x_date
##' @method ggplot_add scale_ggtree
##' @export
ggplot_add.scale_ggtree <- function(object, plot, object_name) {
    mrsd <- get("mrsd", envir = plot$plot_env)
    if (!is.null(mrsd) && class(plot$data$x) == "Date") {
        x <- Date2decimal(plot$data$x)
    } else {
        x <- plot$data$x
    }

    breaks <- object$breaks
    labels <- object$labels

    if (length(breaks) == 0) {
        breaks <- graphics::hist(x, breaks=5, plot=FALSE)$breaks
    }
    m <- attr(plot, "mapping")

    if (!is.null(mrsd) && class(m$to) == "Date") {
        to <- Date2decimal(m$to)
    } else {
        to <- m$to
    }
    
    idx <- which(sapply(breaks, function(x) any(x > m$to)))
    if (length(idx)) {
        breaks <- breaks[-idx]
    }

    if (length(labels) == 0) {
        labels <- breaks
    }

    if (length(breaks) != length(labels)) {
        stop("breaks and labels should be in equal length.")
    }

    breaks <- c(breaks, to)
    labels <- c(labels, gsub("\\.", "", as.character(m$from)))

    if (!is.null(mrsd) && class(plot$data$x) == "Date") {
        obj <- scale_x_date(breaks=decimal2Date(breaks), labels)
    } else {
        obj <- scale_x_continuous(breaks=breaks, labels=labels)
    }
    ggplot_add(obj, plot, object_name)
}
