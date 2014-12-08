##' get.tree method generics
##'
##'
##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @title get.tree method
##' @return tree text
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
setGeneric("get.tree", function(object, ...) standardGeneric("get.tree"))

##' get.treeinfo method generics
##'
##'
##' @docType methods
##' @name get.treeinfo
##' @rdname get.treeinfo-methods
##' @title get.treeinfo method
##' @return data.frame
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
setGeneric("get.treeinfo", function(object, ladderize=TRUE, right=FALSE, ...) standardGeneric("get.treeinfo"))


##' get.placements method generics
##'
##'
##' @docType methods
##' @name get.placements
##' @rdname get.placements-methods
##' @title get.placements method
##' @return data.frame
##' @export
##' @author GuangchuangYu \url{http://ygc.name}
setGeneric("get.placements", function(object, by, ...) standardGeneric("get.placements"))

##' "set.treeinfo<-" method generic
##'
##'
##' @docType methods
##' @name "set.treeinfo<-"
##' @rdname set.treeinfo-methods
##' @title "set.treeinfo<-" method
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
setGeneric("set.treeinfo<-", function(x, value) standardGeneric("set.treeinfo<-"))
