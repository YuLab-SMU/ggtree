##' Class "jplace"
##' This class stores information of jplace file.
##'
##'
##' @name jplace-class
##' @aliases jplace-class
##'   show,jplace-method get.tree,jplace-method
##'
##' @docType class
##' @slot fields colnames of first variable of placements
##' @slot tree tree text
##' @slot placements placement information
##' @slot version version
##' @slot metadata metadata
##' @exportClass jplace
##' @author Guangchuang Yu \url{http://ygc.name}
##' @seealso \code{\link{show}} \code{\link{get.tree}}
##' @keywords classes
setClass("jplace",
         representation = representation(
             fields = "character",
             tree = "character",
             placements = "data.frame",
             version = "numeric",
             metadata = "list"
             )
         )

##' read jplace file
##'
##' 
##' @title read.jplace
##' @param file jplace file
##' @return \code{jplace} instance
##' @importFrom jsonlite fromJSON
##' @export
##' @author ygc
read.jplace <- function(file) {
    with(fromJSON(file),
         new("jplace",
             fields = fields,
             tree = tree,
             placements = placements,
             version = version,
             metadata = metadata))
}


##' show method for \code{jplace} instance
##'
##' 
##' @name show
##' @docType methods
##' @rdname show-methods
##'
##' @title show method
##' @param object A \code{jplace} instance.
##' @return print info
##' @importFrom methods show
##' @exportMethod show
##' @usage show(object)
##' @author Guangchuang Yu \url{http://ygc.name}
setMethod("show", signature(object = "jplace"),
          function(object) {
              cat("jplace class\n   ..@ tree      : ")
              cat(str(object@tree), "\n",
                  "  ..@ placements:'data.frame':	", nrow(object@placements),
                  " obs. of ", ncol(object@placements), "variables\n",
                  "  ..@ version   : int", object@version, "\n")
          }
          )

##' get.tree method
##'
##'
##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @aliases get.tree,jplace,ANY-method
##' @title get.tree method
##' @param object jplace object
##' @param ... additional parameter
##' @return tree text
##' @exportMethod get.tree
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.tree(object, ...)
setMethod("get.tree", signature(object = "jplace"),
          function(object, ...) {
              get.tree.jplace(object, ...)
          }
          )
