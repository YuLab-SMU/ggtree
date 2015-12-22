##' group selected clade
##'
##' 
##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="nhx"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="ggplot"),
          function(object, node, group_name) {
              groupClade.ggplot(object, node, group_name)
          })


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="gg"),
          function(object, node, group_name) {
              groupClade.ggplot(object, node, group_name)
          })

