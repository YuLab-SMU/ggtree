##' add subview to mainview for ggplot2 objects
##'
##' 
##' @title subview
##' @param mainview main view
##' @param subview sub view
##' @param x x position
##' @param y y position
##' @param width width of subview, [0,1]
##' @param height height of subview, [0,1]
##' @return ggplot object
##' @importFrom grid editGrob
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

    grob <- ggplotGrob(subview)
    mainview + annotation_custom(
        editGrob(grob, name=paste(grob$name, annotation_id())),
        xmin = x - width*xrng,
        xmax = x + width*xrng,
        ymin = y - height*yrng,
        ymax = y + height*yrng)
}

annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})
