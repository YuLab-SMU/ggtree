##' scale color by a numerical tree attribute
##'
##'
##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="beast"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="treedata"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="paml_rst"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="phylo"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


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




## @importFrom colorspace rainbow_hcl
scale_color_ <- function(phylo, by, low=NULL, high=NULL, na.color=NULL, default.color="darkgrey", interval=NULL) {
    df <- fortify(phylo)
    vals <- df[, by]

    MIN=min(vals, na.rm=TRUE)
    MAX=max(vals, na.rm=TRUE)

    if (is.null(interval)) {
        interval <- seq(MIN, MAX, length.out=100)
    }
    n <- length(interval)

    if (!is.null(low) & ! is.null(high)) {
        cols <- color_scale(low, high, n)
    } else {
        colorspace <- "colorspace"
        require(colorspace, character.only = TRUE)
        rainbow_hcl <- eval(parse(text="rainbow_hcl"))
        cols <- rainbow_hcl(n)
    }

    idx <- getIdx(vals, MIN=MIN, MAX=MAX, interval=interval)
    interval <- attr(idx, "interval")

    df$color <- cols[idx]

    tree <- get.tree(phylo)

    if (is.null(na.color)) {
        nodes <- getNodes_by_postorder(tree)
        for (curNode in nodes) {
            children <- getChild(tree, curNode)
            if (length(children) == 0) {
                next
            }
            idx <- which(is.na(df[children, "color"]))
            if (length(idx) > 0) {
                df[children[idx], "color"] <- df[curNode, "color"]
            }
        }
        ii <- which(is.na(df[, "color"]))
        if (length(ii) > 0) {
            df[ii, "color"] <- default.color
        }
    } else {
        ii <- which(is.na(df[, "color"]))
        if (length(ii) > 0) {
            df[ii, "color"] <- na.color
        }
    }

    ## cols[is.na(cols)] <- "grey"
    color <- df$color

    attr(color, "scale") <- list(interval=interval, color=cols)
    return(color)
}


