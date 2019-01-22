##' format a list of range (HPD, CI, etc that has length of 2)
##'
##'
##' @title range_format
##' @param x input list
##' @param trans transformation function
##' @return character vector of '[lower, upper]'
##' @export
##' @author Guangchuang Yu
range_format <- function(x, trans = NULL) {
    sapply(x, function(y) {
        if(length(y) == 1 && is.na(y)) {
            return(NA)
        } else {
            if (!is.null(trans) && is(trans, 'function'))
                y <- trans(y)
            return(sprintf("[%f, %f]", y[1], y[2]))
        }
    })
}
