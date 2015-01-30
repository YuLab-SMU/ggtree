##' download phylopic and convert to grob object
##'
##' 
##' @title get.phylopic
##' @param id phylopic id
##' @param size size of the phylopic
##' @param color color
##' @param alpha alpha
##' @return grob object
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @importFrom RCurl getURLContent
##' @importFrom png readPNG
##' @importFrom grid rasterGrob
##' @export
##' @author Guangchuang Yu
##' @references https://github.com/sckott/rphylopic/blob/master/R/add_phylopic.r
get.phylopic <- function(id, size=512, color="black", alpha=1) {
    size %<>% as.character %>% match.arg(c("64", "128", "256", "512", "1024"))
    img <- paste0("http://phylopic.org/assets/images/submissions/", id, ".", size, ".png") %>%
        getURLContent %>%
            readPNG

    color %<>% col2rgb
    n <- length(img[,,1])
    
    matrix(ifelse(img[,,4] > 0,
                  rgb(red   = rep(color[1,1], n),
                      green = rep(color[2,1], n),
                      blue  = rep(color[3,1], n),
                      alpha = img[,,4] * 255 * alpha, maxColorValue = 255),
                  rgb(red   = rep(1, n),
                      green = rep(1, n),
                      blue  = rep(1, n),
                      alpha = img[,,4] * alpha)), 
           nrow = nrow(img)) %>%
               rasterGrob
}
