

##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "beast"),
          function(object) {
              cat("'beast' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))                  
              cat("\nwith the following features available:\n")
              print_fields(object)
          })

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "codeml"),
          function(object) {
              cat("'codeml' S4 object that stored information of\n\t",
                  paste0("'", object@rst@rstfile, "' and \n\t'",
                         object@mlc@mlcfile, "'."),
                  "\n\n")
              cat("...@ tree:")
              print.phylo(get.tree(object))                  
              cat("\nwith the following features available:\n")
              print_fields(object, len=4)
          })

##' show method for \code{jplace} instance
##'
##' 
##' @name show
##' @docType methods
##' @rdname show-methods
##'
##' @title show method
##' @param object one of \code{jplace}, \code{beast} object
##' @return print info
##' @importFrom methods show
##' @exportMethod show
##' @usage show(object)
##' @author Guangchuang Yu \url{http://ygc.name}
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="ggtree")
##' jp <- read.jplace(jp)
##' show(jp)
setMethod("show", signature(object = "jplace"),
          function(object) {
              cat("'jplace' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'."),
                  "\n\n")

              cat("...@ tree: ")

              phylo <- get.tree(object)
              phylo$node.label <- NULL
              phylo$tip.label %<>% gsub("\\@\\d+", "", .) 
        
              print.phylo(phylo)

              cat("\nwith the following features availables:\n")
              cat("\t", paste0("'",
                               paste(get.fields(object), collapse="',\t'"),
                               "'."),
                  "\n")
          }
          )


##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "nhx"),
          function(object) {
              cat("'nhx' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))                  
              cat("\nwith the following features available:\n")
              print_fields(object)
          })


##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "phylip"),
          function(object) {
              cat("'phylip' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))                  
              msg <- paste0("\nwith sequence alignment available (", length(object@sequence),
                            " sequences of length ", width(object@sequence)[1], ")\n")
              cat(msg)
          })

