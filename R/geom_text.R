##' geom_text2 support aes(subset) via setup_data
##'
##' 
##' @title geom_text2
##' @inheritParams geom_text
##' @return text layer
##' @importFrom ggplot2 layer
##' @importFrom ggplot2 position_nudge
##' @export
##' @seealso
##' \link[ggplot2]{geom_text}
##' @author Guangchuang Yu
geom_text2 <- function(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", parse = FALSE, na.rm=TRUE, show.legend = NA, inherit.aes = TRUE,
  ..., nudge_x = 0, nudge_y = 0, check_overlap = FALSE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomTextGGtree,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
          parse = parse,
          check_overlap = check_overlap,
          na.rm = na.rm,
          ...
          )
      )
}

##' text annotations
##' @export
##' @rdname geom_text
##' @param mapping the aesthetic mapping
##' @param data A layer specific dataset -
##'             only needed if you want to override he plot defaults.
##' @param stat The statistical transformation to use on the data for this layer
##' @param position The position adjustment to use for overlapping points on this layer
##' @param parse if TRUE, the labels will be passd into expressions
##' @param na.rm logical
##' @param show.legend logical
##' @param inherit.aes logical
##' @param ... other arguments passed on to 'layer'
##' @param nudge_x horizontal adjustment
##' @param nudge_y vertical adjustment
##' @param check_overlap if TRUE, text that overlaps previous text in the same layer will not be plotted
##' @source
##' This is just the imported function
##' from the ggplot2 package. The documentation you should
##' read for the geom_text function can be found here: \link[ggplot2]{geom_text}
##'
##' @seealso
##' \link[ggplot2]{geom_text}
geom_text <- ggplot2::geom_text


##' @importFrom ggplot2 GeomText
##' @importFrom ggplot2 draw_key_text
GeomTextGGtree <- ggproto("GeomTextGGtree", GeomText,
                          setup_data = function(data, params) {
                              data[data$subset,]
                          },
                          
                          draw_panel = function(data, panel_scales, coord, parse = FALSE,
                              na.rm = FALSE, check_overlap = FALSE) {
                              GeomText$draw_panel(data, panel_scales, coord, parse,
                                                  na.rm, check_overlap)
                          },

                          required_aes = c("x", "y", "label"),
                          
                          default_aes = aes(colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                              vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2),
                          
                          draw_key = draw_key_text
                          )
