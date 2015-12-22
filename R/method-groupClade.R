##' group selected clade
##'
##' 
##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="nhx"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })


