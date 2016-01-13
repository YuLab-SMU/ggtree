##' drawing phylogenetic tree from phylo object
##'
##' 
##' @title ggtree
##' @param tr phylo object
##' @param mapping aes mapping
##' @param layout one of 'rectangular', 'slanted', 'fan'/'circular', 'radial' or 'unrooted'
##' @param mrsd most recent sampling date
##' @param as.Date logical whether using Date class in time tree
##' @param yscale y scale
##' @param yscale_mapping yscale mapping for category variable
##' @param ladderize logical
##' @param right logical
##' @param branch.length variable for scaling branch, if 'none' draw cladogram
##' @param ndigits number of digits to round numerical annotation variable
##' @param ... additional parameter
##' @return tree
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 annotate
##' @importFrom ggplot2 scale_x_reverse
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom ggplot2 coord_flip
##' @importFrom ggplot2 coord_polar
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' ggtree(tr)
ggtree <- function(tr,
                   mapping        = NULL,
                   layout         = "rectangular",
                   mrsd           = NULL,
                   as.Date        = FALSE,
                   yscale         = "none",
                   yscale_mapping = NULL,
                   ladderize      = TRUE,
                   right          = FALSE,
                   branch.length  = "branch.length",
                   ndigits        = NULL,
                   ...) {

    layout %<>% match.arg(c("rectangular", "slanted", "fan", "circular", "radial", "unrooted"))

    if (is(tr, "r8s") && branch.length == "branch.length") {
        branch.length = "TREE"
    }
    
    if(yscale != "none") {
        ## for 2d tree
        layout <- "slanted"
    }
    if (layout == "fan" || layout == "circular") {
        layout <- "circular"
        type <- "circular"
    } else if (layout == "radial") {
        layout <- "slanted"
        type <- "radial"
    } else {
        type <- "none"
    }
    if (is.null(mapping)) {
        mapping <- aes_(~x, ~y)
    } else {
        mapping <- modifyList(aes_(~x, ~y), mapping)
    }
    p <- ggplot(tr, mapping=mapping,
                layout        = layout,
                mrsd          = mrsd,
                as.Date       = as.Date,
                yscale        = yscale,
                yscale_mapping= yscale_mapping,
                ladderize     = ladderize,
                right         = right,
                branch.length = branch.length,
                ndigits       = ndigits, ...)
    
    p <- p + geom_tree(layout, ...)  + theme_tree()
    
    if (type == "circular" || type == "radial") {
        p <- p + coord_polar(theta = "y")
        ## refer to: https://github.com/GuangchuangYu/ggtree/issues/6
        ## and also have some space for tree scale (legend)
        p <- p + scale_y_continuous(limits=c(0, max(p$data$y)))
    } 
    
    attr(p, "mrsd") <- mrsd
    attr(p, "param") <- list(layout        = layout,
                             yscale        = yscale,
                             ladderize     = ladderize,
                             right         = right,
                             branch.length = branch.length,
                             ndigits       = ndigits)
    return(p)
}

##' add tree layer
##'
##' 
##' @title geom_tree
##' @param layout one of 'rectangular', 'slanted', 'circular', 'radial' or 'unrooted'
##' @param ... additional parameter
##' @return tree layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang
##' @examples
##' require(ape)
##' tr <- rtree(10)
##' require(ggplot2)
##' ggplot(tr) + geom_tree()
geom_tree <- function(layout="rectangular", ...) {
    x <- y <- parent <- NULL
    lineend  = "round"
    if (layout == "rectangular" || layout == "fan" || layout == "circular") {
        list(
            geom_segment(aes(x    = x[parent],
                             xend = x,
                             y    = y,
                             yend = y),
                         lineend  = lineend, ...),
            
            geom_segment(aes(x    = x[parent],
                             xend = x[parent],
                             y    = y[parent],
                             yend = y),
                         lineend  = lineend, ...)
            )
    } else if (layout == "slanted" || layout == "radial" || layout == "unrooted") {
        geom_segment(aes(x    = x[parent],
                         xend = x,
                         y    = y[parent],
                         yend = y),
                     lineend  = lineend, ...)
    }
}









##' add colorbar legend
##'
##' 
##' @title add_colorbar
##' @param p tree view
##' @param color output of scale_color function
##' @param x x position
##' @param ymin ymin
##' @param ymax ymax
##' @param font.size font size 
##' @return ggplot2 object
##' @export
##' @importFrom ggplot2 annotate
##' @author Guangchuang Yu
add_colorbar <- function(p, color, x=NULL, ymin=NULL, ymax=NULL, font.size=4) {
    mrsd <- attr(p, "mrsd")
    if (!is.null(mrsd)) {
        attr(p, "mrsd") <- NULL
        
        p$data$x <- Date2decimal(p$data$x)
        p$data$branch <- Date2decimal(p$data$branch)
        ## annotation segment not support using Date as x-axis
    }
    
    legend <- do.call("cbind", attr(color, "scale"))
    
    legend[,1] <- round(as.numeric(legend[,1]), 2)
    
    ## legend[nrow(legend),1] <- paste(">=", legend[nrow(legend),1])

    if (is.null(x)) {
        xx <- range(p$data$x)
        x <- min(xx)+diff(xx)/100
    }

    yy <- range(p$data$y)
    if (is.null(ymin)) {
        if (is.null(ymax)) {        
            ymax <- max(yy) - diff(yy)/100
        }
        ymin <- ymax - diff(yy)/15
    }

    if (is.null(ymax)) {
        ymax <- ymin + diff(yy)/15
    }
        
    yy <- seq(ymin, ymax, length.out=nrow(legend)+1)

    ymin <- yy[1:nrow(legend)]
    ymax <- yy[2:length(yy)]
    y <- (ymin+ymax)/2

    i <- seq(1, length(y), length.out = 5) %>% round(0)
    offset <- diff(range(p$data$x))/40
    barwidth <- offset/5
    
    p + annotate("text", x=x+offset*1.5, y=y[i], label=legend[i,1], size=font.size, hjust=0) +
        annotate("rect", xmin=x, xmax=x+offset, ymin=ymin,
                 ymax = ymax, fill=legend[,2], color=legend[,2]) +
                     annotate("segment", x=x, xend=x+barwidth, y=y[i], yend=y[i], color="white") +
                         annotate("segment", x=x+offset-barwidth, xend=x+offset, y=y[i], yend=y[i], color="white")
    
}






