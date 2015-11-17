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
setMethod("scale_color", signature(object="nhx"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="paml_rst"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


