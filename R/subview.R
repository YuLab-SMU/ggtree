##' add subview to mainview for ggplot2 objects
##'
##' 
##' @title subview
##' @param mainview main view
##' @param subview a ggplot or grob object
##' @param x x position
##' @param y y position
##' @param width width of subview, [0,1]
##' @param height height of subview, [0,1]
##' @return ggplot object
##' @importFrom ggplot2 annotation_custom
##' @importFrom ggplot2 ggplotGrob
##' @export
##' @author Guangchuang Yu
subview <- function(mainview, subview, x, y, width=.1, height=.1) {
    mapping <- mainview$mapping %>% as.character
    aes_x <- mapping["x"]
    aes_y <- mapping["y"]
    
    xrng <- mainview$data[, aes_x] %>% range %>% diff
    yrng <- mainview$data[, aes_y] %>% range %>% diff
    
    if (!any(class(subview) %in% c("ggplot", "grob", "character"))) {
        stop("subview should be a ggplot or grob object, or an image file...")
    }
    
    if (is(subview, "ggplot")) {
        sv <- ggplotGrob(subview)
    } else if (is(subview, "grob")) {
        sv <- subview
    } else if (file.exists(subview)) {
        sv <- rasterGrob(readImage(subview))
    } else {
        stop("subview should be a ggplot or grob object, or an image file...")
    }
    
    mainview + annotation_custom(
        sv,
        xmin = x - width*xrng,
        xmax = x + width*xrng,
        ymin = y - height*yrng,
        ymax = y + height*yrng)
}
