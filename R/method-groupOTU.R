##' group tree based on selected OTU, will traceback to MRCA
##'
##' 
##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phylo"),
          function(object, focus, group_name="group") {
              groupOTU.phylo(object, focus, group_name)
          })

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="apeBootstrap"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="beast"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="codeml"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="codeml_mlc"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="nhx"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="paml_rst"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )




##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phangorn"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
##' @param tree which tree selected
setMethod("groupOTU", signature(object="r8s"),
          function(object, focus, group_name="group", tree="TREE") {
              groupOTU_(get.tree(object)[[tree]], focus, group_name)
          }
          )



##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="ggplot"),
          function(object, focus, group_name="group") {
              groupOTU.ggplot(object, focus, group_name)
          })


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="gg"),
          function(object, focus, group_name) {
              groupOTU.ggplot(object, focus, group_name)
          })


