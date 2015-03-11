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
    imgfile <- tempfile(fileext = ".png") 
    if (Sys.info()["sysname"] == "Windows") {
        download.file(imgurl, imgfile, mode="wb", quiet = TRUE)
    } else {
        download.file(imgurl, imgfile, quiet = TRUE)
    }
    img <- readImage(imgfile)
       
    color <- col2rgb(color) / 255

    img <- channel(img, 'rgb')
    img[,,1] <- color[1]
    img[,,2] <- color[2]
    img[,,3] <- color[3]
    img[,,4] <- img[,,4]*alpha
    
    return(img)
}

##' add phylopic layer
##'
##' 
##' @title phylopic
##' @param tree_view tree view
##' @param phylopic_id phylopic id
##' @param size size of phylopic to download
##' @param color color
##' @param alpha alpha
##' @param node selected node
##' @param x x position
##' @param y y position
##' @param width width of phylopic
##' @return phylopic layer
##' @export
##' @importFrom ggplot2 annotation_custom
##' @importFrom grid rasterGrob
##' @author Guangchuang Yu
phylopic <- function(tree_view, phylopic_id,
                     size=512, color="black", alpha=0.5,
                     node=NULL, x=NULL, y=NULL, width=NULL) {
    img <- download.phylopic(phylopic_id, size, color, alpha)
    if ( is.null(node) ) {
        xmin <- ymin <- -Inf
        xmax <- ymax <- Inf
    } else {
        if (is.null(x) || is.null(y)) {
            if (is.null(node)) {
                stop("node or x and y should not be NULL...")
            }
            x <- tree_view$data[node, "x"]
            y <- tree_view$data[node, "y"]
        }
        if (is.null(width)) {
            width <- 5
        }
        
        dims <- dim(img)[1:2]
        AR <- dims[1]/dims[2]
        xmin <- x - width/2
        xmax <- x + width/2
        ymin <- y - AR * width/2
        ymax <- y + AR * width/2
    }
    
    tree_view + annotation_custom(xmin=xmin, ymin=ymin,
                                  xmax=xmax, ymax=ymax,
                                  rasterGrob(img))
}

