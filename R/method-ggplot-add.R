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

##' @method ggplot_add zoom_clade
##' @export
ggplot_add.zoom_clade <- function(object, plot, object_name) {
    zoomClade(plot, object$node, xexpand=object$xexpand)
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
    } else if (object$layout == 'circular' || object$layout == "inward_circular") {
        ## refer to: https://github.com/GuangchuangYu/ggtree/issues/6
        ## and also have some space for tree scale (legend)
        obj <- list(coord_polar(theta='y', start=-pi/2, -1, clip = 'off'),
                    scale_y_continuous(limits = c(0, NA), expand = expansion(0, 0.6))
                    )
        if (object$layout == 'inward_circular') {
            obj[[3]] <- scale_x_reverse(limits = object$xlim)
        }
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

##' @method ggplot_add hexpand
##' @export
ggplot_add.hexpand <- function(object, plot, object_name) {
    ## xr <- aplot::xrange(plot) ## panel range
    xr <- ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range ## plot range
    rr <- range(xr)
    if (object$direction == 1) {
        xx <- xr[2] + rr * object$ratio
    } else if (object$direction == -1) {
        xx <- xr[1] - rr * object$ratio
    } else {
        stop("direction should be 1 or -1")
    }

    obj <- ggplot2::expand_limits(x = xx)
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
    layout <- get_layout(plot)
    if (object$as_ylab) {
        if (layout != "rectangular" && layout != "dendrogram") {
            stop("displaying tiplab as y labels only supports rectangular layout")
        }
        ## remove parameters that are not useful
        object$mapping <- NULL
        object$align <- NULL
        object$linetype <- NULL
        object$linesize <- NULL
        object$geom <- NULL
        object$offset <- NULL
        object$as_ylab <- NULL
            
        res <- ggplot_add.tiplab_ylab(object, plot, object_name)
        return(res)
    }

    object$as_ylab <- NULL
    if (layout == 'circular' || layout == 'fan' || layout == "unrooted" ||
        layout == "equal_angle" || layout == "daylight" || layout == "ape" || 
        layout == "inward_circular") {
        ly <- do.call(geom_tiplab_circular, object)
    } else {
        ly <- do.call(geom_tiplab_rectangular, object)
    }
    ggplot_add(ly, plot, object_name)
}

##' @method ggplot_add tiplab_ylab
##' @export
ggplot_add.tiplab_ylab <- function(object, plot, object_name) {
    layout <- get_layout(plot)
    if (is.null(object$position)) {
        if (layout == "rectangular") {
            object$position <- "right"
        } else if (layout == "dendrogram") {
            object$position <- "left"
        }
    }

    df <- plot$data
    df <- df[df$isTip, ]
    yscale <- scale_y_continuous(breaks = df$y, labels = df$label,
                                 position = object$position, expand = expansion(0, 0.6))

    object$position <- NULL
    ytext <- do.call(element_text, object)

    if (is.null(object$position)) {
        if (layout == "rectangular") {
            thm <- theme(axis.text.y = ytext)
        } else if (layout == "dendrogram") {
            thm <- theme(axis.text.x = ytext)
        }
    }
    plot + yscale + thm
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

##' @method ggplot_add cladelab
##' @export
ggplot_add.cladelab <- function(object, plot, object_name){
    layout <- get("layout", envir=plot$plot_env)
    if (is.null(object$mapping) && (is.null(object$node) || is.null(object$label))){
        abort("mapping and node or label can't be NULL simultaneously, we can't get the
              data to be displayed in this layer, please provide a data or subset, node 
              and label in mapping, we will extract the node and label from tree data,
              or provide node and label!")
    }
    if (!is.null(object$mapping)){
        if (!"node" %in% names(object$mapping) || !"label" %in% names(object$mapping)){
            abort("when mapping is not NULL, the node and label should be set in mapping!")
        }
        if (is.null(object$data)){
            if (!is.null(object$mapping$subset)){
                object$data <- subset(plot$data, eval(parse(text=quo_name(object$mapping$subset))))
                object$mapping <- object$mapping[names(object$mapping)!="subset"]
            }else{
                abort("data and aesthetics subset in mapping are NULL simultaneously,
                       we can't get the data to be displayed in this layer, please provide a data or
                       set subset (we will extract the data from tree data.)")
            }
        }else{
            if (!is.null(object$mapping$subset)){
                object$data <- subset(object$data, eval(parse(text=quo_name(object$mapping$subset))))
                object$mapping <- object$mapping[names(object$mapping)!="subset"]
            }
        }
        da_node_label <- data.frame(node=as.vector(object$data[[as_name(object$mapping$node)]]),
                                    label=as.vector(object$data[[as_name(object$mapping$label)]]))
    }else{
        da_node_label <-data.frame(node=object$node, label=object$label)
    }
    default_raw_aes <- list(offset=0, offset.text=0, align=FALSE, angle=0, extend=0, horizontal=TRUE)
    default_raw_aes <- reset_params(defaultp=default_raw_aes, inputp=object$params, type="other")
    bar_params <- list(barsize=0.5, barcolour = "black")
    bar_params <- reset_params(defaultp=bar_params, inputp=object$params, type="bar")
    text_params <- list(fontsize= 3.88, family = "sans", textcolour="black", hjust=0)
    text_params <- reset_params(defaultp=text_params, inputp=object$params, type="text")
    image_params <- list(imagesize=0.05, alpha=0.8, imagecolour=NULL)
    image_params <- reset_params(defaultp=image_params, inputp=object$params, type="image")
    da_node_label <- transform_df(data=da_node_label, object=object, default_aes=default_raw_aes)
    object$mapping <- object$mapping[!names(object$mapping)%in%names(default_raw_aes)]
    object$params <- object$params[!names(object$params) %in% c("angle", "size", "color", "colour", "hjust")]
    flagnode <- match(da_node_label$node, plot$data$node)
    if (anyNA(flagnode)){
        flagnode <- da_node_label$node[is.na(flagnode)]
        abort(paste0("ERROR: clade node id ", paste(flagnode, collapse='; ')," can not be found in tree data."))
    }
    if (object$parse == 'emoji') {
        emoji <- get_fun_from_pkg("emojifont", "emoji")
        clade_label <- emoji(clade_label)
        object$parse <- FALSE
        family <- "EmojiOne"
    }
    if (layout == "unrooted" || layout == "daylight"){
        textdata <- build_cladelabel_df2(trdf=plot$data,
                                         nodeids=da_node_label$node,
                                         label=da_node_label$label,
                                         offset=da_node_label$offset.text,
                                         align=da_node_label$align,
                                         angle=da_node_label$angle,
                                         horizontal=da_node_label$horizontal)
        bardata <- build_cladebar_df2(trdf=plot$data,
                                      nodeids=da_node_label$node,
                                      offset=da_node_label$offset,
                                      align=da_node_label$align)
    }else{
        textdata <- build_cladelabel_df(trdf=plot$data,
                                        nodeids=da_node_label$node,
                                        label=da_node_label$label,
                                        offset=da_node_label$offset.text,
                                        align=da_node_label$align, 
                                        angle=da_node_label$angle, 
                                        horizontal=da_node_label$horizontal)
        bardata <- build_cladebar_df(trdf=plot$data,
                                     nodeids=da_node_label$node,
                                     offset=da_node_label$offset,
                                     align=da_node_label$align, 
                                     extend=da_node_label$extend)
    }
    if (!is.null(object$data) && !is.null(object$mapping)){
        object$data <- object$data[,!colnames(object$data) %in% c("x", "xend", "y", "yend", "label"),drop=FALSE]
        textdata <- merge(textdata, object$data, by.x="node", by.y=as_name(object$mapping$node))
        bardata <- merge(bardata, object$data, by.x="node", by.y=as_name(object$mapping$node))
        object$mapping <- object$mapping[!names(object$mapping) %in% c("node", "label")]
    }
    annot_obj <- switch(object$geom,
                        text=build_text_layer(data=textdata, object=object, params=text_params),
                        label=build_text_layer(data=textdata, object=object, params=text_params),
                        image=build_image_layer(data=textdata, object=object, params=image_params),
                        phylopic=build_image_layer(data=textdata, object=object, params=image_params),
                        shadowtext=build_text_layer(data=textdata, object=object, params=text_params),
                       )
    bar_obj <- list()
    bar_obj$data <- bardata
    bar_default_aes <- list(barcolour="black", barsize=0.5, colour="black", size=0.5, linetype=1, alpha=NA)
    bar_obj$mapping <- reset_mapping(defaultm=bar_default_aes, inputm=object$mapping)
    ifelse(is.null(bar_obj$mapping),bar_obj$mapping <- aes_(x=~x, xend=~xend, y=~y, yend=~yend),
           bar_obj$mapping <- modifyList(bar_obj$mapping, aes_(x=~x, xend=~xend, y=~y, yend=~yend)))
    bar_dot_params <- reset_dot_params(mapping=bar_obj$mapping, 
                                       defaultp=bar_params, 
                                       default_aes=bar_default_aes,
                                       params=object$params)
    bar_obj <- c(bar_obj, bar_dot_params)
    if (layout == "unrooted" || layout == "daylight"){
        bar_obj <- do.call("geom_curve", bar_obj)
    }else{
        bar_obj <- do.call("geom_segment", bar_obj)
    }
    obj <- list(annot_obj, bar_obj)
    ggplot_add(obj, plot, object_name)
}

## ##' @method ggplot_add hilight
## ##' @export
## ggplot_add.hilight <- function(object, plot, object_name) {
##     layout <- get("layout", envir = plot$plot_env)
## 
##     ## if the plot was not produce by ggtree, but ggplot
##     ## instead of the tree layout, you may get graphics::layout
##     if (!is.character(layout)) layout <- 'rectangular'
## 
##     if ("branch.length" %in% colnames(plot$data)) {
##         object$mapping <- aes_(branch.length = ~branch.length)
##     }
## 
##     if (layout == "unrooted" || layout == "daylight") {
##         ly <- do.call(geom_hilight_encircle, object)
##     } else {
##         ly <- do.call(geom_hilight_rectangular, object)
##     }
##     ggplot_add(ly, plot, object_name)
## }


##' @method ggplot_add hilight
##' @importFrom rlang quo_name
##' @export
ggplot_add.hilight <- function(object, plot, object_name){
    layout <- get("layout", envir = plot$plot_env)
    if (!is.character(layout)) layout <- "rectangular"
    if (is.null(object$mapping) && is.null(object$node)){
        abort("mapping and node can't be NULL simultaneously, we can't get the 
              data to be displayed in this layer, please provide a data or subset 
              (we will extract the data from tree data.), or provide node!")
    }
    flag_names <- c("parent", "node", "branch.length", "label", "isTip", "x", "y", "branch", "angle")
    flag_tbl_tree <- all(flag_names %in% names(object$data))
    if (flag_tbl_tree){
        framedat <- object$data
    }else{
        framedat <- plot$data
    }
    if (!is.null(object$mapping)){
        if (is.null(object$data)){
             if (!is.null(object$mapping$subset)){
                 object$data <- subset(plot$data, eval(parse(text=quo_name(object$mapping$subset))))
                 object$mapping <- modifyList(object$mapping, aes_(node=~node))
                 object$mapping <- object$mapping[names(object$mapping)!="subset"]
                 clade_node <- as.vector(object$data[["node"]])
             }else{
                 abort("data and aesthetics subset in mapping are NULL simultaneously,
                       we can't get the data to be displayed in this layer, please provide a data or
                       set subset (we will extract the data from tree data.)")
             }
        }else{
             if (!is.null(object$mapping$subset)){
                 object$data <- subset(object$data, eval(parse(text=quo_name(object$mapping$subset))))
                 object$mapping <- object$mapping[names(object$mapping)!="subset"]
                 clade_node <- as.vector(object$data[[as_name(object$mapping$node)]])
             }else{
                 clade_node <- as.vector(object$data[[as_name(object$mapping$node)]])
             }
        }
    }else{
        clade_node <- object$node
    }
    flagnode <- match(clade_node, framedat$node)
    if (anyNA(flagnode)){
        flagnode <- clade_node[is.na(flagnode)]
        abort(paste0("ERROR: clade node id ", paste(flagnode, collapse='; ')," can not be found in tree data."))
    }
    if (layout == "unrooted" || layout == "daylight"){
        data <- switch(object$type,
                       auto = build_cladeids_df(trdf=framedat, nodeids=clade_node),
                       rect = build_cladeids_df2(trdf=framedat, nodeids=clade_node),
                       encircle = build_cladeids_df(trdf=framedat, nodeids=clade_node))
    }else{
        data <- switch(object$type,
                       auto = build_cladeids_df2(trdf=framedat, nodeids=clade_node),
                       rect = build_cladeids_df2(trdf=framedat, nodeids=clade_node),
                       encircle = build_cladeids_df(trdf=framedat, nodeids=clade_node))
    }
    if (!is.null(object$data) && !is.null(object$mapping)){
        if (flag_tbl_tree){
            object$data <-  object$data[,!colnames(object$data) %in% setdiff(flag_names, as_name(object$mapping$node)),drop=FALSE]
        }
        object$data <- merge(data, object$data, by.x="clade_root_node", by.y=as_name(object$mapping$node))
        object$data[[as_name(object$mapping$node)]] <- as.vector(object$data$clade_root_node)
        object$mapping <- object$mapping[!names(object$mapping) %in% c("node")]
    }else{
        object$data <- data
    }
    if (layout == "unrooted" || layout == "daylight"){
        ly <- switch(object$type,
                     auto = choose_hilight_layer(object=object, type="encircle"),
                     rect = choose_hilight_layer(object=object, type="rect"),
                     encircle = choose_hilight_layer(object=object, type="encircle"))
    }else{
        ly <- switch(object$type,
                     auto = choose_hilight_layer(object=object, type="rect"),
                     rect = choose_hilight_layer(object=object, type="rect"),
                     encircle = choose_hilight_layer(object=object, type="encircle"))
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

##' @importFrom ggplot2 aes_
##' @importFrom rlang abort as_name
##' @export
ggplot_add.taxalink <- function(object, plot, object_name){
    layout <- get("layout", envir = plot$plot_env)
    if (object$outward=="auto"){
       if(layout=="inward_circular"){
           object$outward <- FALSE
       }else{
           object$outward <- TRUE
       }
    }
    if (is.null(object$data) && is.null(object$taxa1) && is.null(object$taxa2)){
        abort("data and taxa1, taxa2 can't be NULL simultaneously!")
    }
    if (!is.null(object$data)){
        if (is.null(object$mapping) || is.null(object$mapping$taxa1) || is.null(object$mapping$taxa2)){
            abort("when data is provided, the mapping also should be provided, and taxa1, taxa2 are required aesthetics.")
        }else{
            node1 <- taxa2node(plot$data, as.vector(object$data[[as_name(object$mapping$taxa1)]]))
            node2 <- taxa2node(plot$data, as.vector(object$data[[as_name(object$mapping$taxa2)]]))
        }
    }else{
        node1 <- taxa2node(plot$data, object$taxa1)
        node2 <- taxa2node(plot$data, object$taxa2)
    }
    if (any(is.na(node1)) || any(is.na(node2))){
        missingtaxa <- c(as.vector(object$data[[as_name(object$mapping$taxa1)]])[is.na(node1)], 
                         as.vector(object$data[[as_name(object$mapping$taxa2)]])[is.na(node2)])
        abort(paste0("The taxa: ", paste(missingtaxa, collapse=", "), " can not be found.")) 
    }
    x <- plot$data$x
    y <- plot$data$y
    if (!is.null(object$offset)){
        tmpshift <- object$offset * (max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
        dat <- data.frame(x    = x[node1] + tmpshift,
                          xend = x[node2] + tmpshift,
                          y    = y[node1],
                          yend = y[node2])
    }else{
        dat <- data.frame(x    = x[node1],
                          xend = x[node2],
                          y    = y[node1],
                          yend = y[node2])
    }
    if (!is.null(object$data) && !is.null(object$mapping)){
        object$data <- cbind(object$data, dat)
        object$mapping <- object$mapping[!names(object$mapping) %in% c("taxa1", "taxa2")]
        object$mapping <- modifyList(object$mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend))
    }else{
        object$data <- dat
        object$mapping <- aes_(x=~x, y=~y, xend=~xend, yend=~yend)
    }
    params <- c(list(data=object$data, mapping=object$mapping, outward=object$outward), object$params)
    obj <- do.call("geom_curvelink", params)
    ggplot_add(obj, plot, object_name)
}
