##' filter data for tree annotation layer
##'
##' The 'td_filter()' function returns another function that can be
##' used to subset ggtree() plot data. The function can be passed to the 'data' parameter
##' of geom layer to perform subsetting. All rows that satisy your conditions will be retained.
##' @title td-filter
##' @param ... Expressions that return a logical value.
##' @param .f a function (if any, default is NULL) that pre-operate the data
##' @return A function to filter ggtree plot data using conditions defined by '...'.
##' @seealso
##' [filter][dplyr::filter] 
##' @author Guangchuang Yu
##' @examples
##' tree <- rtree(30)
##' ## similar to 'ggtree(tree) + geom_tippoint()'
##' ggtree(tree) + geom_point(data = td_filter(isTip))
##' @export
td_filter <- function(..., .f = NULL) {
    dots <- rlang::quos(...)
    function(.data) {
        if (!is.null(.f)) .data <- .f(.data)
        dplyr::filter(.data, !!!dots)
    }
}

##' flatterns a list-column of data frame
##'
##' The 'td_unnest' function returns another function that can be
##' used to unnest ggtree() plot data. The function can be passed to
##' the 'data' parameter of a geom layer to flattern list-cloumn tree data.
##' @title td-unnest
##' @param cols columns to unnest
##' @param ... additional parameters that pass to tidyr::unnest
##' @param .f a function (if any, default is NULL) that pre-operate the data
##' @return A function to unnest ggtree plot data
##' @seealso
##' [unnest][tidyr::unnest]
##' @author Guangchuang Yu
##' @export
td_unnest <- function(cols, ..., .f = NULL) {
    function(.data) {
        if (!is.null(.f)) .data <- .f(.data)
        tidyr::unnest(.data, {{cols}}, ...) 
    }
}

##' mutate data for tree annotation layer
##'
##' The 'td_mutate()' function returns another function that can be
##' used to mutate ggtree() plot data. The function can be passed to the 'data' parameter
##' of geom layer to perform adding new variables and preserving existing ones.
##' @title td-mutate
##' @param ... additional parameters that pass to dplyr::mutate
##' @param .f a function (if any, default is NULL) that pre-operate the data
##' @return A function to mutate ggtree plot data
##' @seealso
##' [mutate][dplyr::mutate]
##' @export
td_mutate <- function(..., .f=NULL){
    function(.data){
        if (!is.null(.f)) .data <- .f(.data)
        dplyr::mutate(.data, ...)
    }
}
