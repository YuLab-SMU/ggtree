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
             file       = filename(file)
             )
         )
}



##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="jplace"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })




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
              get.fields.tree(object)
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
                          if (nrow(x) == 1) {
                              return(x)
                          }
                          ## http://astrostatistics.psu.edu/su07/R/html/base/html/all.equal.html
                          ## due to precision, number are identical maynot be equal, so use all.equal which can test nearly equal number
                          ## if not equals, the output is a descript string of the differences
                          idx <- sapply(2:nrow(x), function(i) all.equal(x[1,2], x[i,2]))
                          if (any(idx == TRUE)) {
                              return(x[c(1, which(idx==TRUE)+1),])
                          } else {
                              return(x[1,])
                          }
                          
                      } else {
                          ## if only 1 row, it may stored as vector
                          ## the edge number, for example 523 can be 523.0000 due to R stored number as real number
                          ## be careful in mapping edge number.
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
              res <- as.data.frame(place.df, stringsAsFactor=FALSE)
              
              ## res[] <- lapply(res, as.character)
              ## for (i in 1:ncol(res)) {
              ##     if (all(grepl("^[0-9\\.e]+$", res[,i]))) {
              ##         res[,i] <- as.numeric(res[,i])
              ##     }
              ## }
              return(res)
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
                            ladderize, right, ...)
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

