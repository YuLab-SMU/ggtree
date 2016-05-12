##' print information of a list of beast trees
##'
##' 
##' @title print
##' @param x a list of beast object
##' @param ... no used
##' @return message
##' @method print beastList
##' @export
##' @author Guangchuang Yu
print.beastList <- function(x, ...) {
    msg <- paste(length(x), "beast trees")
    cat(msg, "\n")
}
