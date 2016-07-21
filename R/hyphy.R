##' read HYPHY output
##'
##'
##' @title read.hyphy
##' @param nwk tree file in nwk format, one of hyphy output
##' @param ancseq ancestral sequence file in nexus format,
##'               one of hyphy output
##' @param tip.fasfile tip sequence file
##' @return A hyphy object
## @importFrom Biostrings readBStringSet
## @importFrom Biostrings toString
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
##' @examples
##' nwk <- system.file("extdata/HYPHY", "labelledtree.tree", package="ggtree")
##' ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="ggtree")
##' read.hyphy(nwk, ancseq)
read.hyphy <- function(nwk, ancseq, tip.fasfile=NULL) {
    anc <- scan(ancseq, what="", sep="\n", quiet=TRUE)
    end <- grep("END;", anc, ignore.case=TRUE)
    
    seq.start <- grep("MATRIX", anc, ignore.case=TRUE)
    seq.end   <- end[end > seq.start][1]
    seq       <- anc[(seq.start+1):(seq.end-1)]
    seq       <- seq[seq != ";"]
    seq       <- seq[seq != ""]
    seq       <- gsub(" ", "", seq)
    seq       <- gsub(";", "", seq)
    
    ## some files may only contains sequences (should have TAXALABELS block that contains seq names).
    ## some may contains sequence name like phylip format in MATRIX block (no need to have TAXALABELS block).
    ##
    ## extract sequence name if available
    if (all(grepl("\\s+", seq))) {
        ## if contains blank space, may contains seq name
        sn <- gsub("(\\w*)\\s.*", "\\1", seq)
    }
    
    seq <- gsub("\\w*\\s+", "", seq)
    
    label.start <- grep("TAXLABELS", anc, ignore.case = TRUE)
    if (length(label.start) == 0) {
        if (all(sn == "")) {
            stop("taxa labels is not available...")
        }
        label <- sn
    } else {
        label.end   <- end[end > label.start][1]
        label       <- anc[(label.start+1):(label.end-1)]
        
        label <- sub("^\t+", "", label)
        label <- sub("\\s*;$", "", label)
        label <- unlist(strsplit(label, split="\\s+"))
        label <- gsub("'|\"", "", label)
    }
    
    names(seq) <- label

    tr <- read.tree(nwk)
    nl <- tr$node.label
    ## root node may missing, which was supposed to be 'Node1'
    ##
    ## from a user's file, which is 'Node0', but it seems the file is not from the output of HYPHY.
    ##
    ## I am not sure. But it's safe to use "label[!label %in% nl]" instead of just assign it to "Node1".
    ##
    ## nl[nl == ""] <- "Node1"
    nl[nl == ""] <- label[!label %in% nl]
    
    tr$node.label <- nl

    type <- get_seqtype(seq)
    fields <- "subs"
    if (type == "NT") {
        fields <- c(fields, "AA_subs")
    }

    res <- new("hyphy",
               fields = fields,
               treetext = scan(nwk, what='', quiet=TRUE),
               phylo = tr,
               seq_type = type,
               ancseq = seq,
               tree.file = filename(nwk),
               ancseq.file = ancseq
               )

    if ( !is.null(tip.fasfile) ) {
        readBStringSet <- get_fun_from_pkg("Biostrings", "readBStringSet")
        toString <- get_fun_from_pkg("Biostrings", "toString")
        
        tip_seq <- readBStringSet(tip.fasfile)
        nn <- names(tip_seq)
        tip_seq <- sapply(seq_along(tip_seq), function(i) {
            toString(tip_seq[i])
        })
        names(tip_seq) <- nn
        res@tip_seq <- tip_seq
        res@tip.fasfile <- tip.fasfile
    }
    set.hyphy_(res)
}

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="hyphy"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="hyphy"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="hyphy"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="hyphy"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })

##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "hyphy"),
          function(object) {
              cat("'hyphy' S4 object that stored information of \n\t",
                  paste0("'", object@tree.file, "'"))
              if (length(object@tip_seq) == 0) {
                  cat(paste0("and '", object@ancseq.file, "'"), ".\n")
              } else {
                  cat(paste0(", \n\t'", object@ancseq.file, "'"),
                      paste0("and \n\t'", object@tip.fasfile, "'."),
                      "\n\n")
              }
              cat("...@ tree:")
              print.phylo(get.tree(object))
              cat("\nwith the following features available:\n")
              cat("\t", paste0("'",
                               paste(get.fields(object), collapse="',\t'"),
                               "'."),
                  "\n")
              
          })

##' @rdname get.tree-methods
##' @exportMethod get.tree
##' @examples
##' nwk <- system.file("extdata/HYPHY", "labelledtree.tree", package="ggtree")
##' ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="ggtree")
##' hy <- read.hyphy(nwk, ancseq)
##' get.tree(hy)
setMethod("get.tree", signature(object = "hyphy"),
          function(object) {
              object@phylo
          }
          )

##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "hyphy"),
          function(object, ...) {
              if(length(object@tip_seq) == 0) {
                  warning("tip sequence not available...\n")
              } else {
                  get.fields.tree(object)
              }
          })


##' @rdname get.subs-methods
##' @exportMethod get.subs
##' @examples
##' nwk <- system.file("extdata/HYPHY", "labelledtree.tree", package="ggtree")
##' ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="ggtree")
##' tipfas <- system.file("extdata", "pa.fas", package="ggtree")
##' hy <- read.hyphy(nwk, ancseq, tipfas)
##' get.subs(hy, type="AA_subs")
setMethod("get.subs", signature(object="hyphy"),
          function(object, type, ...) {
              if (length(object@tip_seq) == 0) {
                  stop("tip sequence not available...\n")
              }
              if (type == "subs") {
                  return(object@subs)
              } else {
                  return(object@AA_subs)
              }
          })


set.hyphy_ <- function(object, ...) {
    if (!is(object, "hyphy")) {
        stop("object should be an instance of 'hyphy'")
    }

    if (length(object@tip_seq) == 0) {
        return(object)
    }

    types <- get.fields(object)
    seqs <- c(object@tip_seq, object@ancseq)
    for (type in types) {
        if (type == "subs") {
            translate <- FALSE
        } else {
            translate <- TRUE
        }
        subs <- get.subs_(object@phylo, seqs, translate, ...)
        if (type == "subs") {
            object@subs <- subs
        } else {
            object@AA_subs <- subs
        }
    }
    return(object)
}
