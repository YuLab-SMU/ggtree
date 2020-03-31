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


## ##' @method ggplot_add axisAlign
## ##' @importFrom ggplot2 scale_x_discrete
## ##' @importFrom ggplot2 scale_y_discrete
## ##' @export
## ggplot_add.axisAlign <- function(object, plot, object_name) {
##     limits <- object$limits

##     ## expand_limits <- object$expand_limits
##     ## limits[1] <- limits[1] + (limits[1] * expand_limits[1]) - expand_limits[2]
##     ## limits[2] <- limits[2] + (limits[2] * expand_limits[3]) + expand_limits[4]

##     if (is.numeric(limits)) {
##         lim_x <- scale_x_continuous(limits=limits, expand=c(0,0))
##         lim_y <- scale_y_continuous(limits = limits, expand = c(0, 0))
##     } else {
##         lim_x <- scale_x_discrete(limits=limits, expand = c(0, 0.6))
##         lim_y <- scale_y_discrete(limits = limits, expand = c(0, 0.6))
##     }

##     if (object$axis == 'x') {
##         ## if (object$by == "x") {
##         if (is(plot$coordinates, "CoordFlip")) {
##             message("the plot was flipped and the x limits will be applied to y-axis")
##             scale_lim <- lim_y
##         } else {
##             scale_lim <- lim_x
##         }
##         ## } else {
##         ##     if (is(plot$coordinates, "CoordFlip")) {
##         ##         message("the plot was flipped and the x limits will be applied to x-axis")
##         ##         scale_lim <- scale_x_continuous(limits=limits, expand=c(0,0))
##         ##     } else {
##         ##         scale_lim <- scale_y_continuous(limits=limits, expand=c(0,0))
##         ##     }
##         ## }
##     } else { ## axis == 'y'
##         ## if (object$by == "x") {
##         ##     if (is(plot$coordinates, "CoordFlip")) {
##         ##         message("the plot was flipped and the y limits will be applied to y-axis")
##         ##         scale_lim <- scale_y_continuous(limits = limits, expand = c(0, 0))
##         ##     } else {
##         ##         scale_lim <- scale_x_continuous(limits = limits, expand = c(0, 0))
##         ##     }
##         ## } else {
##         if (is(plot$coordinates, "CoordFlip")) {
##             message("the plot was flipped and the y limits will be applied to x-axis")
##             scale_lim <- lim_x
##         } else {
##             scale_lim <- lim_y
##         }
##         ## }
##     }
##     ggplot_add(scale_lim, plot, object_name)
## }

##' @method ggplot_add geom_range
##' @export
ggplot_add.geom_range <- function(object, plot, object_name) {
    obj <- do.call(geom_range_internal, object)
    assign(x = "range_range", value = object$range, envir = plot$plot_env)
    assign(x = "range_center", value = object$center, envir = plot$plot_env)
    ggplot_add(obj, plot, object_name)
}

##' @method ggplot_add layout_ggtree
##' @importFrom ggplot2 expansion
##' @export
ggplot_add.layout_ggtree <- function(object, plot, object_name) {
    if(object$layout == 'fan') {
        return(open_tree(plot, object$angle))
    }

    if (object$layout == 'dendrogram') {
        plot <- revts(plot)
        obj <- list(scale_x_reverse(labels = abs),
                    coord_flip(clip = 'off')
                    )
    } else if (object$layout == 'circular') {
        ## refer to: https://github.com/GuangchuangYu/ggtree/issues/6
        ## and also have some space for tree scale (legend)
        obj <- list(coord_polar(theta='y', start=-pi/2, -1, clip = 'off'),
                    scale_y_continuous(limits = c(0, NA), expand = expansion(0, 0.6))
                    )
    } else { ## rectangular
        obj <- coord_cartesian(clip = 'off')
    }
    assign("layout", object$layout, envir = plot$plot_env)
    ggplot_add(obj, plot, object_name)
}



##' @method ggplot_add range_xaxis
##' @export
ggplot_add.range_xaxis <- function(object, plot, object_name) {
    d <- plot$data
    center <- get("range_center", envir = plot$plot_env)
    if (center == "auto") {
        range <- get("range_range", envir = plot$plot_env)
        center_value <- range_center(d[[range]])
        i <- which(!is.na(center_value))[1]
        cc <- center_value[i]
    } else {
        i <- which(!is.na(d[[center]]))[1]
        cc <- as.numeric(d[[center]][i])
    }

    diff <- cc - d$x[i]
    obj <- scale_x_continuous(sec.axis = ~. + diff)
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
    if (!is(layout, "character")) {
        layout <- attr(plot$data, "layout")
    }
    if (layout == 'circular' || layout == 'fan') {
        ly <- do.call(geom_tiplab_circular, object)
    } else {
        ly <- do.call(geom_tiplab_rectangular, object)
    }
    ggplot_add(ly, plot, object_name)
}

##' @method ggplot_add cladelabel
##' @export
ggplot_add.cladelabel <- function(object, plot, object_name) {
    layout <- get("layout", envir = plot$plot_env)
    if (layout == "unrooted" || layout == "daylight") {
        ly <- do.call(geom_cladelabel2, object)
    } else {
        ly <- do.call(geom_cladelabel_rectangular, object)
    }
    ggplot_add(ly, plot, object_name)
}

##' @method ggplot_add hilight
##' @export
ggplot_add.hilight <- function(object, plot, object_name) {
    layout <- get("layout", envir = plot$plot_env)

    ## if the plot was not produce by ggtree, but ggplot
    ## instead of the tree layout, you may get graphics::layout
    if (!is.character(layout)) layout <- 'rectangular'

    if ("branch.length" %in% colnames(plot$data)) {
        object$mapping <- aes_(branch.length = ~branch.length)
    }

    if (layout == "unrooted" || layout == "daylight") {
        ly <- do.call(geom_hilight_encircle, object)
    } else {
        ly <- do.call(geom_hilight_rectangular, object)
    }
    ggplot_add(ly, plot, object_name)
}

##' @method ggplot_add striplabel
##' @export
ggplot_add.striplabel <- function(object, plot, object_name) {
    d <- plot$data
    strip_df <- get_striplabel_position(d, object$taxa1, object$taxa2,
                                        object$offset, object$align,
                                        object$extend, adjustRatio=1.02)
    ly_bar <- geom_segment(aes_(x = ~x, xend = ~xend,
                                y = ~y, yend = ~yend),
                           data = strip_df, size = object$barsize,
                           color = object$color)

    strip_text_df <- get_striplabel_position(d, object$taxa1, object$taxa2,
                                        offset = object$offset + object$offset.text,
                                        align = object$align, 
                                        object$extend, adjustRatio=1.02)
    strip_text_df$y <- mean(c(strip_text_df$y, strip_text_df$yend))
    strip_text_df$label <- object$label

    if (is.null(object$label) || is.na(object$label)) {
        return(ggplot_add(ly_bar, plot, object_name))
    }

    if(object$geom == 'text') {
        ly_text <- geom_text(aes_(x = ~x, y = ~y, label = ~label),
                             data = strip_text_df, size = object$fontsize,
                             angle = object$angle, family = object$family,
                             hjust = object$hjust, parse = object$parse,
                             color = object$color
                             )
    } else {
        ly_text <- geom_label(aes_(x = ~x, y = ~y, label = ~label),
                              data = strip_text_df, size = object$fontsize,
                              angle = object$angle, family = object$family,
                              hjust = object$hjust, parse = object$parse,
                              color = object$color, fill = object$fill
                              )
    }

    ggplot_add(list(ly_bar, ly_text), plot, object_name)
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
