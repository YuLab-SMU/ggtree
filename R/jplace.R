##' read jplace file
##'
##' 
##' @title read.jplace
##' @param file jplace file
##' @return \code{jplace} instance
##' @importFrom jsonlite fromJSON
##' @export
##' @author ygc
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="ggtree")
##' read.jplace(jp)
read.jplace <- function(file) {  
    fields <- tree <- placements <- NULL
    version <- metadata <- NULL
    with(fromJSON(file),
         new("jplace",
             fields     = fields,
             treetext   = tree,
             phylo      = jplace_treetext_to_phylo(tree),
             placements = placements,
             version    = version,
             metadata   = metadata,
             file       = file
             )
         )
}

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="jplace"),
          function(object, focus) {
              groupOTU_(object, focus)
          }
          )

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="jplace"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="jplace"),
          function(object) {
              object@phylo
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

##' get.treeinfo method
##'
##'
##' @docType methods
##' @name get.treeinfo
##' @rdname get.treeinfo-methods
##' @aliases get.treeinfo,jplace,ANY-method
##' @exportMethod get.treeinfo
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.treeinfo(object, layout, ladderize, right, ...)
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="ggtree")
##' jp <- read.jplace(jp)
##' get.treeinfo(jp)
setMethod("get.treeinfo", signature(object = "jplace"),
          function(object, layout="phylogram",
                   ladderize=TRUE, right=FALSE, ...) {
              get.treeinfo.jplace(object, layout,
                                  ladderize, right, ...)
          }
          )


##' get.treetext method
##'
##'
##' @docType methods
##' @name get.treetext
##' @rdname get.treetext-methods
##' @aliases get.treetext,jplace,ANY-method
##' @exportMethod get.treetext
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.treetext(object, ...)
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="ggtree")
##' jp <- read.jplace(jp)
##' get.treetext(jp)
setMethod("get.treetext", signature(object = "jplace"),
          function(object, ...) {
              get.treetext.jplace(object, ...)
          }
          )


##' get.fields method
##'
##'
##' @docType methods
##' @name get.fields
##' @rdname get.fields-methods
##' @aliases get.fields,jplace,ANY-method
##' @exportMethod get.fields
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.fields(object, ...)
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="ggtree")
##' jp <- read.jplace(jp)
##' get.fields(jp)
setMethod("get.fields", signature(object = "jplace"),
          function(object, ...) {
              object@fields
          }
          )

##' get.placement method
##'
##'
##' @docType methods
##' @name get.placements
##' @rdname get.placements-methods
##' @aliases get.placements,jplace,ANY-method
##' @exportMethod get.placements
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.placements(object, by, ...)
##' @examples
##' jp <- system.file("extdata", "sample.jplace", package="ggtree")
##' jp <- read.jplace(jp)
##' get.placements(jp, by="all")
setMethod("get.placements", signature(object = "jplace"),
          function(object, by="best", ...) {
              placements <- object@placements
              place <- placements[,1]
              ids <- NULL
              if (length(placements) == 2) {
                  ids <- sapply(placements[,2], function(x) x[1])
                  names(place) <- ids
              }
              if (by == "best") { ## best hit
                  place <- lapply(place, function(x) {
                      if (is(x, "data.frame") || is(x, "matrix")) {
                          return(x[1,])
                      } else {
                          return(x)
                      }
                  })
              }
              place.df <- do.call("rbind", place)
              row.names(place.df) <- NULL
              if (!is.null(ids)) {
                  nn <- rep(ids, sapply(place, function(x) {
                      nr <- nrow(x)
                      if (is.null(nr))
                          return(1)
                      return(nr)
                  }))
                  place.df <- data.frame(name=nn, place.df)
                  colnames(place.df) <- c("name", object@fields)
              } else {
                  colnames(place.df) <- object@fields
              }
              
              return(as.data.frame(place.df))
          })


get.treetext.jplace <- function(object, ...) {
    object@treetext
}

get.fields.jplace <- function(object, ...) {
    object@fields
}

get.treeinfo.jplace <- function(object, layout,
                                ladderize, right, ...) {
    extract.treeinfo.jplace(object, layout,
                            ladderize, right)
}

##' generate jplace file
##'
##' 
##' @title write.jplace 
##' @param nwk tree in newick format
##' @param data annotation data
##' @param outfile jplace output file
##' @return jplace file
##' @export
##' @author ygc
##' @examples
##' tree <- system.file("extdata", "pa.nwk", package="ggtree")
##' data <- read.csv(system.file("extdata", "pa_subs.csv", package="ggtree"),
##'                 stringsAsFactor=FALSE)
##' outfile <- tempfile()
##' write.jplace(tree, data, outfile)
write.jplace <- function(nwk, data, outfile) {
    out <- file(outfile, "w")
    data[] = lapply(data, as.character) ## remove factor
    writeLines("{", out)
    writeLines(paste0('\t"tree": "', readLines(nwk), '",'), out)
    writeLines('\t"placements": [', out)
    for (i in 1:nrow(data)) {
        writeLines(paste0('\t{"p":["', paste(data[i,], collapse = '", "'), '"]}'), out, sep="")
        if (i != nrow(data)) {
            writeLines(',', out)
        } 
    }
    writeLines('],', out)
    writeLines('\t"metadata": {"info": "generated by ggtree package"},',
               out)
    writeLines('\t"version": 2,', out)
    writeLines(paste0('\t"fields": [', '"',
                      paste(colnames(data), collapse='", "'),
                      '"'),
               out)
    writeLines('\t]\n}', out)
    close(out)
}

