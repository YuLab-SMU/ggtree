##' Class "jplace"
##' This class stores information of jplace file.
##'
##'
##' @name jplace-class
##' @aliases jplace-class
##'   show,jplace-method get.tree,jplace-method
##'   get.placements,jplace-method
##'   get.treeinfo,jplace-method
##'   set.treeinfo<-,jplace-method
##'   set.treeinfo,jplace-method
##'
##' @docType class
##' @slot fields colnames of first variable of placements
##' @slot tree tree text
##' @slot placements placement information
##' @slot version version
##' @slot metadata metadata
##' @slot treeinfo tree infomation
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
             metadata = "list",
             treeinfo = "data.frame"
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
             metadata = metadata
             )
         )
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

##' get.treeinfo method
##'
##'
##' @docType methods
##' @name get.treeinfo
##' @rdname get.treeinfo-methods
##' @aliases get.treeinfo,jplace,ANY-method
##' @title get.treeinfo method
##' @param object jplace object
##' @param ladderize ladderize, logical
##' @param right logical, parameter for ladderize
##' @param ... additional parameter
##' @return data.frame
##' @exportMethod get.treeinfo
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.treeinfo(object, ladderize, right, ...)
setMethod("get.treeinfo", signature(object = "jplace"),
          function(object, ladderize=TRUE, right=FALSE, ...) {
              get.treeinfo.jplace(object, ladderize, right, ...)
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


## setMethod("get.fields", signature(object = "jplace"),
##           function(object, ...) {
##               object@fields
##           })

##' get.placement method
##'
##'
##' @docType methods
##' @name get.placements
##' @rdname get.placements-methods
##' @aliases get.placements,jplace,ANY-method
##' @title get.placements method
##' @param object jplace object
##' @param by get best hit or others
##' @param ... additional parameter
##' @return data.frame
##' @exportMethod get.placements
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.placements(object, by, ...)
setMethod("get.placements", signature(object = "jplace"),
          function(object, by="best", ...) {
              placements <- object@placements
              place <- placements[,1]
              ids <- sapply(placements[,2], function(x) x[1])
              names(place) <- ids
              if (by == "best") { ## best hit
                  place <- lapply(place, function(x) x[1,])
              }
              place.df <- do.call("rbind", place)
              place.df <- data.frame(name=rep(ids, sapply(place, function(x) {
                  nr <- nrow(x)
                  if (is.null(nr))
                      return(1)
                  return(nr)
              })), place.df)
              
              colnames(place.df) <- c("name", object@fields)
              return(place.df)
          })


##' set.treeinfo method for \code{jplace} object
##'
##'
##' @name set.treeinfo<-
##' @docType methods
##' @rdname set.treeinfo-methods
##' @aliases set.treeinfo<-,jplace,ANY-method
##' @title set.treeinfo<- method
##' @param x jplace object
##' @param value tree info
##' @exportMethod "set.treeinfo<-"
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage set.treeinfo(x) <- value
setReplaceMethod(f="set.treeinfo",
                 signature = "jplace",
                 definition = function(x, value) {
                     x@treeinfo <- value                     
                 })


get.tree.jplace <- function(object, ...) {
    object@tree
}

get.fields.jplace <- function(object, ...) {
    object@fields
}

get.treeinfo.jplace <- function(object, ladderize, right, ...) {
    treeinfo <- object@treeinfo
    if(nrow(treeinfo) == 0) {
        tree.text <- get.tree(object)
        treeinfo <- extract.treeinfo(tree.text, ladderize, right)
        set.treeinfo(object) <- treeinfo
    } else if (attr(treeinfo, "ladderize") != ladderize) {
        treeinfo <- extract.treeinfo(tree.text, ladderize, right)
        set.treeinfo(object) <- treeinfo
    }
    return(treeinfo)
}
