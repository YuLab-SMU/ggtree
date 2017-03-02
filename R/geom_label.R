##' geom_label2 support aes(subset) via setup_data
##'
##'
##' @title geom_label2
##' @param mapping the aesthetic mapping
##' @param data A layer specific dataset -
##'             only needed if you want to override he plot defaults.
##' @param ... other arguments passed on to 'layer'
##' @param position The position adjustment to use for overlapping points on this layer
##' @param family sans by default, can be any supported font
##' @param parse if TRUE, the labels will be passd into expressions
##' @param nudge_x horizontal adjustment
##' @param nudge_y vertical adjustment
##' @param label.padding Amount of padding around label.
##' @param label.r Radius of rounded corners.
##' @param label.size Size of label border, in mm
##' @param na.rm logical
##' @param show.legend logical
##' @param inherit.aes logical
##' @return label layer
##' @importFrom ggplot2 layer
##' @importFrom ggplot2 position_nudge
##' @export
##' @seealso
##' \link[ggplot2]{geom_label}
##' @author Guangchuang Yu
geom_label2 <- function(mapping = NULL, data = NULL,
                        ...,
                        position = "identity",
                        family = "sans",
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25,
                        na.rm = TRUE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
        }

        position <- position_nudge(nudge_x, nudge_y)
    }

    default_aes <- aes_(node=~node)
    if (is.null(mapping)) {
        mapping <- default_aes
    } else {
        mapping <- modifyList(mapping, default_aes)
    }

    if (parse == "emoji") {
        label_aes <- aes_string(label=paste0("suppressMessages(emoji(", as.list(mapping)$label,"))"))
        mapping <- modifyList(mapping, label_aes)
        emoji <- get_fun_from_pkg("emojifont", "emoji")
        parse <- FALSE
        family <- "EmojiOne"
    }


    layer(
        data = data,
        mapping = mapping,
        stat = StatTreeData,
        geom = GeomLabelGGtree,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            parse = parse,
            family = family,
            label.padding = label.padding,
            label.r = label.r,
            label.size = label.size,
            na.rm = na.rm,
            ...
        ),
        check.aes = FALSE
    )
}



##' @importFrom ggplot2 GeomLabel
GeomLabelGGtree <- ggproto("GeomLabelGGtree", GeomLabel,
                           setup_data = function(data, params) {
                               if (is.null(data$subset))
                                   return(data)
                               data[which(data$subset),]
                           },
                           draw_panel = function(self, data, panel_scales, coord, parse = FALSE,
                                                 na.rm = FALSE,
                                                 label.padding = unit(0.25, "lines"),
                                                 label.r = unit(0.15, "lines"),
                                                 label.size = 0.25) {
                               GeomLabel$draw_panel(data, panel_scales, coord, parse,
                                                   na.rm, label.padding, label.r, label.size)
                           },
                           required_aes = c("node", "x", "y", "label"),

                           default_aes = aes(
                               colour = "black", fill = "white", size = 3.88, angle = 0,
                               hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
                               lineheight = 1.2
                           ),

                           draw_key = draw_key_label
                           )


