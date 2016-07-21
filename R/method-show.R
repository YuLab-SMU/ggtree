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

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "codeml_mlc"),
          function(object) {
              cat("'codeml_mlc' S4 object that stored information of\n\t",
                  paste0("'", object@mlcfile, "'."),
                  "\n\n")
              
              cat("...@ tree:")
              print.phylo(get.tree(object))                  
              
              cat("\nwith the following features available:\n")
              cat("\t", paste0("'",
                                 paste(get.fields(object), collapse="',\t'"),
                                 "'."),
                  "\n")
          }
          )

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
                            " sequences of length ", nchar(object@sequence)[1], ")\n")
              cat(msg)
          })

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "paml_rst"),
          function(object) {
              cat("'paml_rst' S4 object that stored information of\n\t",
                  paste0("'", object@rstfile, "'.\n\n"))
              ## if (length(object@tip.fasfile) != 0) {
              ##     cat(paste0(" and \n\t'", object@tip.fasfile, "'.\n\n"))
              ## } else {
              ##     cat(".\n\n")
              ## }
              fields <- get.fields(object)

              if (nrow(object@marginal_subs) == 0) {
                  fields <- fields[fields != "marginal_subs"]
                  fields <- fields[fields != "marginal_AA_subs"]
              }
              if (nrow(object@joint_subs) == 0) {
                  fields <- fields[fields != "joint_subs"]
                  fields <- fields[fields != "joint_AA_subs"]
              }
              
              cat("...@ tree:")
              print.phylo(get.tree(object))                  
              cat("\nwith the following features available:\n")
              cat("\t", paste0("'",
                               paste(fields, collapse="',\t'"),
                               "'."),
                  "\n")
          })



##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "r8s"),
          function(object) {
              cat("'r8s' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))                  
              ## cat("\nwith the following features available:\n")
              ## print_fields(object)
          })
