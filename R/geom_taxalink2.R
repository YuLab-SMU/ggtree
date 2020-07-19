#' link between taxa 
#'
#' `geom_taxalink2` is the updated verison of `geom_taxalink`. It support data.frame as input,
#' the `colour`, `size`, `linetype` and `alpha` can be mapped. When the `data` was provided, 
#' the `mapping` should be also provided, which `taxa1` and `taxa2` should be mapped created 
#' by `aes`, `aes_` or `aes_string`. In addition, the `hratio`, control the height of curve line, 
#' when tree layout is `cirular`, default is 1. `ncp`, the number of control points used to draw the 
#' curve, more control points creates a smoother curve, default is 1. They also can be mapped to
#' a column of data. 
#' 
#' @param data data.frame, The data to be displayed in this layer, default is NULL.
#' @param mapping Set of aesthetic mappings, default is NULL.
#' @param taxa1 can be label or node number.
#' @param taxa2 can be label or node number.
#' @param offset numeric, control the shift of curve line (the ratio of axis value,
#' range is "(0-1)"), default is NULL.
#' @param outward logical, control the orientation of curve when the layout of tree is circular,
#' fan or other layout in polar coordinate, default is "auto", meaning It will automatically.
#' @param ..., additional parameter.
#' @section Aesthetics:
#' \code{geom_taxalink2()} understands the following aesthethics (required aesthetics are in bold):
#'     \itemize{
#'        \item \strong{\code{taxa1}}
#'        \item \strong{\code{taxa2}}
#'        \item \code{group}
#'        \item \code{colour}
#'        \item \code{linetype}
#'        \item \code{size}
#'        \item \code{curvature}
#'        \item \code{hratio}
#'        \item \code{ncp}
#'     }
#' @return a list object.
#' @export
geom_taxalink2 <- function(data=NULL, 
                           mapping=NULL,
                           taxa1=NULL, 
                           taxa2=NULL, 
                           offset = NULL,
                           outward = "auto",
                           ...){
    params <- list(...)
    structure(list(data    = data,
                   mapping = mapping,
                   taxa1   = taxa1,
                   taxa2   = taxa2,
                   offset  = offset, 
                   outward = outward,
                   params  = params), 
              class = 'taxalink')
}
