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
## @importFrom EBImage readImage
## @importFrom EBImage channel
##' @export
##' @author Guangchuang Yu
download.phylopic <- function(id, size=512, color="black", alpha=1) {
    imgfile <- tempfile(fileext = ".png")
    download.phylopic_internal(id, size, imgfile)

    requireNamespace("EBImage")
    channel <- eval(parse(text=paste0("EBImage::", "channel")))
    img <- readImage(imgfile)
       
    color <- col2rgb(color) / 255

    img <- channel(img, 'rgb')
    img[,,1] <- color[1]
    img[,,2] <- color[2]
    img[,,3] <- color[3]
    img[,,4] <- img[,,4]*alpha
    
    return(img)
}

##' @importFrom utils download.file
##' @importFrom utils modifyList
download.phylopic_internal <- function(id, size=512, outfile=NULL) {
    size %<>% as.character %>%
        match.arg(c("64", "128", "256", "512", "1024"))

    imgurl <- paste0("http://phylopic.org/assets/images/submissions/", id, ".", size, ".png")
    if (is.null(outfile)) {
        outfile <- sub(".*/", "", imgurl)
    }
    ## mode = "wb" for Windows platform
    download.file(imgurl, outfile, mode="wb", quiet = TRUE) 
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
                     node=NULL, x=NULL, y=NULL, width=.1) {
    width <- diff(range(tree_view$data$x)) * width
    img <- download.phylopic(phylopic_id, size, color, alpha)
    if ( is.null(node) ) {
        xmin <- ymin <- -Inf
        xmax <- ymax <- Inf
    } else {
        if (is.null(x) || is.null(y)) {
            if (is.null(node)) {
                stop("node or x and y should not be NULL...")
            }
            df <- tree_view$data
            x <- df[match(node, df$node), "x"]
            y <- df[match(node, df$node), "y"]
        }
        AR <- getAR(img)
        xmin <- x - width/2
        xmax <- x + width/2
        ymin <- y - AR * width/2
        ymax <- y + AR * width/2
    }
    
    tree_view + annotation_custom(xmin=xmin, ymin=ymin,
                                  xmax=xmax, ymax=ymax,
                                  rasterGrob(img))
}

getAR <- function(img) {
    dims <- dim(img)[1:2]
    dims[1]/dims[2]
}


##' annotation taxa with images
##'
##' 
##' @title annotation_image
##' @param tree_view tree view
##' @param img_info data.frame with first column of taxa name and second column of image names
##' @param width width of the image to be plotted in image
##' @param align logical
##' @param linetype line type if align = TRUE
##' @param linesize line size if align = TRUE
##' @param offset offset of image from the tree view
##' @return tree view
##' @export
##' @author Guangchuang Yu
annotation_image <- function(tree_view, img_info, width=0.1, align=TRUE, linetype="dotted", linesize =1, offset=0) {
    df <- tree_view$data
    idx <- match(img_info[,1], df$label)
    x <- df[idx, "x"]
    y <- df[idx, "y"]

    images <- lapply(img_info[,2], readImage)

    ARs <- sapply(images, getAR)

    width <- width * diff(range(df$x))
    if (align) {
        xmin <- max(df$x) + offset
        xmin <- rep(xmin, length(x))
    } else {
        xmin <- x - width/2 + offset
    }
    xmax <- xmin + width
    
    ymin <- y - ARs * width/2
    ymax <- y + ARs * width/2
    image_layers <- lapply(1:length(xmin), function(i) {
        annotation_custom(xmin=xmin[i], ymin=ymin[i],
                          xmax=xmax[i], ymax=ymax[i],
                          rasterGrob(images[[i]]))
    })

    tree_view <- tree_view + image_layers

    if (align && (!is.null(linetype) && !is.na(linetype))) {
        tree_view <- tree_view + geom_segment(data=df[idx,],
                                              x=xmin, xend = x*1.01,
                                              y = y, yend = y,
                                              linetype=linetype, size=linesize)
    }
    tree_view
}
