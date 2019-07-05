##' @importFrom rlang quo_name
##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add facet_xlim
##' @export
ggplot_add.facet_xlim <- function(object, plot, object_name) {
    var <- panel_col_var(plot)
    free_x <- plot$facet$params$free$x
    if (!is.null(free_x)) {
        if (!free_x)
            message('If you want to adjust xlim for specific panel, ',
                    'you need to set `scales = "free_x"`')
    }

    dummy <- data.frame(x = object$x, .panel = object$panel)
    if (!is.null(var)) {
        names(dummy)[2] <- var
    }

    obj <- geom_blank(aes_(x = ~x), dummy, inherit.aes = FALSE)
    ggplot_add(obj, plot, object_name)
}

##' @method ggplot_add tree_inset
##' @export
ggplot_add.tree_inset <- function(object, plot, object_name) {
    object$tree_view <- plot
    do.call(inset, object)
}

##' @method ggplot_add facet_plot
##' @export
ggplot_add.facet_plot <- function(object, plot, object_name) {
    plot <- add_panel(plot, object$panel)
    df <- plot %+>% object$data
    params <- c(list(data = df, mapping = object$mapping),
                object$params)
    obj <- do.call(object$geom, params)
    ggplot_add(obj, plot, object_name)
}

##' @method ggplot_add tiplab
##' @export
ggplot_add.tiplab <- function(object, plot, object_name) {
    layout <- get("layout", envir = plot$plot_env)
    if (layout == 'circular' || layout == 'fan') {
        ly <- do.call(geom_tiplab_circular, object)
    } else {
        ly <- do.call(geom_tiplab_rectangular, object)
    }
    ggplot_add(ly, plot, object_name)
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
