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
    
    xrng <- mainview$data[, aes_x] %>% range 
    yrng <- mainview$data[, aes_y] %>% range

    for (i in seq_along(mainview$layers)) {
        layer <- mainview$layers[[i]]
        dd <- layer$data
        if (is(dd, "data.frame")) {
            mapping <- as.character(layer$mapping)
            mn <- names(mapping)
            if ('x' %in% mn) {
                aes_x <- mapping["x"]
                xrng <- c(xrng, layer$data[, aes_x])
            }
            if ('xmin' %in% mn) {
                aes_x <- mapping["xmin"]
                xrng <- c(xrng, layer$data[, aes_x])
            }
            if ('xmax' %in% mn) {
                aes_x <- mapping["xmax"]
                xrng <- c(xrng, layer$data[, aes_x])
            }
            if ('y' %in% mn) {
                aes_y <- mapping["y"]
                yrng <- c(yrng, layer$data[, aes_y])
            }
            if ('ymin' %in% mn) {
                aes_y <- mapping["ymin"]
                yrng <- c(yrng, layer$data[, aes_y])
            }
            if ('ymax' %in% mn) {
                aes_y <- mapping["ymax"]
                yrng <- c(yrng, layer$data[, aes_y])
            }
            xrng <- range(xrng)
            yrng <- range(yrng)
        }
    }

    xrng <- diff(xrng)
    yrng <- diff(yrng)
    
    if (!any(class(subview) %in% c("ggplot", "grob", "character"))) {
        stop("subview should be a ggplot or grob object, or an image file...")
    }
    
    if (is(subview, "ggplot")) {
        sv <- ggplotGrob(subview)
    } else if (is(subview, "grob")) {
        sv <- subview
    } else if (file.exists(subview)) {
        readImage <- get_fun_from_pkg("EBImage", "readImage")
        sv <- rasterGrob(readImage(subview))
    } else {
        stop("subview should be a ggplot or grob object, or an image file...")
    }

    width <- width/2
    height <- height/2
    
    mainview + annotation_custom(
        sv,
        xmin = x - width*xrng,
        xmax = x + width*xrng,
        ymin = y - height*yrng,
        ymax = y + height*yrng)
}
