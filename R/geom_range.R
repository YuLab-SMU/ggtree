##' horizontal bar of range (HPD, range etc) on nodes to present uncertainty of evolutionary inference
##'
##'
##' @title geom_range
##' @param range range(interval) to be displayed, e.g. "height_0.95_HPD"
##' @param center center of the range, mean, median or auto (default, the center of the range)
##' @param ... additional parameter, e.g. color, linewidth, alpha
##' @return ggplot layer
##' @export
##' @author Guangchuang Yu
##' @references  
##' For demonstration of this function, please refer to chapter 5.2.4 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
geom_range <- function(range, center = "auto", ...) {
    structure(list(range = range, center = center, ...), class = "geom_range")
}


geom_range_internal <- function(range, center, mapping=NULL, position = "identity", ...) {
    show.legend = NA
    na.rm = TRUE
    inherit.aes = FALSE

    default_aes <- aes(x=!!sym("x"), y=!!sym("y"), xend=!!sym("x"), yend=!!sym("y"))
    if (!is.null(mapping)){
        default_aes <- modifyList(mapping, default_aes)
    }
    lower <- paste0('range_lower(', range, ')')
    upper <- paste0('range_upper(', range, ')')
    if (center == "auto") {
        center <- paste0('range_center(', range, ')')
    }


    mapping <- modifyList(default_aes, aes(center = !!new_quosure(parse_expr(center)), 
                                           lower  = !!new_quosure(parse_expr(lower)), 
                                           upper  = !!new_quosure(parse_expr(upper)))
    )

    layer(
        stat = StatRange,
        mapping = mapping,
        data = NULL,
        geom = GeomInteractiveSegment,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...),
        check.aes = FALSE
    )

}



StatRange <- ggproto("StatRange", Stat,
                     compute_panel = function(self, data, scales, params) {
                         df <- data[!is.na(data[["lower"]]),]
                         df[["lower"]] <- as.numeric(df[["center"]]) + df[["x"]] - df[["lower"]]
                         df[["upper"]] <- as.numeric(df[["center"]]) + df[["x"]] - df[["upper"]]
                         df <- df %>% 
                               select(-c("x", "xend", "center")) %>%
                               rename(x=.data$lower, xend=.data$upper)
                         df

                     },
                     required_aes = c("x", "y", "xend", "yend"),
                     optional_aes = c("center", "lower", "upper")
                     )

range_center <- function(range) {
    vapply(range, function(x) {
        if (length(x) == 0) 
            return(NA) 
        if (length(x) == 1 && is.na(x))
            return(NA)
        (as.numeric(x[1]) + as.numeric(x[2]))/2
    }, numeric(1))
}

range_lower <- function(range) {
    vapply(range, function(x) {
        ## length(x) == 0 for x is NULL
        ## see https://groups.google.com/d/msg/bioc-ggtree/yNzjtioVVGU/MCh3MPl_CwAJ
        if (length(x) == 0)
            return(NA)
        as.numeric(x[1])
    }, numeric(1))
}

range_upper <- function(range) {
    vapply(range, function(x) {
        if (length(x) == 0) 
            return(NA) 
        if (length(x) == 1 && is.na(x))
            return(NA)
        as.numeric(x[2])
    }, numeric(1))
}

