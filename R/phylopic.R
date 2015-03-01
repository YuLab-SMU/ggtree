##' download phylopic and convert to grob object
##'
##' 
##' @title get.phylopic
##' @param id phylopic id
##' @param size size of the phylopic
##' @param color color
##' @param alpha alpha
##' @return grob object
##' @importFrom grid rasterGrob
##' @export
##' @author Guangchuang Yu
get.phylopic <- function(id, size=512, color="black", alpha=1) {
    download.phylopic(id, size, color, alpha) %>% rasterGrob
}

##' download phylopic
##'
##' @title download.phylopic
##' @param id phyopic id
##' @param size size of phylopic
##' @param color color
##' @param alpha alpha
##' @return matrix
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @importFrom EBImage readImage
##' @importFrom EBImage channel
##' @export
##' @author Guangchuang Yu
download.phylopic <- function(id, size=512, color="black", alpha=1) {
    size %<>% as.character %>%
        match.arg(c("64", "128", "256", "512", "1024"))

    imgurl <- paste0("http://phylopic.org/assets/images/submissions/", id, ".", size, ".png")
    imgfile <- tempfile(fileext = ".PNG") ## .png is not recognize by WINDOWS platform
    download.file(imgurl, imgfile, quiet = TRUE)
    img <- readImage(imgfile)
       
    color <- col2rgb(color) / 255

    img <- channel(img, 'rgb')
    img[,,1] <- color[1]
    img[,,2] <- color[2]
    img[,,3] <- color[3]
    img[,,4] <- img[,,4]*alpha
    
    return(img)
}

