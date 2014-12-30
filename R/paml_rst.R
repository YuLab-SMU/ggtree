##' read rst file from paml output
##'
##' 
##' @importFrom Biostrings readBStringSet
##' @importFrom Biostrings toString
##' @title read.paml_rst
##' @param rstfile rst file
##' @param tip.fasfile fasta file of tips
##' @return A \code{paml_rst} object
##' @export
##' @author ygc
read.paml_rst <- function(rstfile, tip.fasfile = NULL) {
    ms <- read.ancseq_paml_rst(rstfile, by="Marginal")
    phylo <- read.phylo_paml_rst(rstfile)
    ## class(phylo) <- "list"
    type <- get_seqtype(ms)
    fields <- c("marginal_subs", "joint_subs")
    if (type == "NT") {
        fields <- c(fields, "marginal_AA_subs", "joint_AA_subs")
    }
    res <- new("paml_rst",
               fields          = fields,
               treetext        = read.treetext_paml_rst(rstfile),
               phylo           = phylo, 
               seq_type        = type,
               marginal_ancseq = ms,
               joint_ancseq    = read.ancseq_paml_rst(rstfile, by = "Joint"),
               rstfile = rstfile
               )
    if (!is.null(tip.fasfile)) {
        seqs <- readBStringSet(tip.fasfile)
        tip_seq <- sapply(1:length(seqs), function(i) {
            toString(seqs[i])
        })
        res@tip_seq <- tip_seq
        res@tip.fasfile <- tip.fasfile
    }
    return(res)
}



##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "paml_rst"),
          function(object) {
              head(str(object))
          }
          )

##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "paml_rst"),
          function(object) {
              if (length(object@tip.fasfile) == 0) {
                  warning("tip.fasfile not available...\n")
              } else {
                  object@fields
              }
          }
          )

get.subs_paml_rst <- function(x, type, ...) {
    if (!is(x, "paml_rst")) {
        stop("x should be an object of paml_rst...")
    }
    seqs <- x@tip_seq
    if (type %in% c("marginal_subs", "marginal_AA_subs")) {
        seqs <- c(seqs, x@marginal_ancseq)
    } else if (type %in% c("joint_subs", "joint_AA_subs")){
        seqs <- c(seqs, x@joint_ancseq)
    } else {
        stop("type should be one of 'marginal_subs',
                             'marginal_AA_subs', 'joint_subs' or 'joint_AA_subs'. ")
    }
    if( type %in% c("marginal_subs", "joint_subs")) {
        translate = FALSE
    } else {
        translate = TRUE
    }
    
    get.subs_(x@phylo, seqs, translate=translate, ...)
}

##' @rdname get.subs-methods
##' @exportMethod get.subs
setMethod("get.subs", signature(object = "paml_rst"),
          function(object, type, ...) {
              if (type == "marginal_subs") {
                  res <- object@marginal_subs
              } else if (type == "marginal_AA_subs") {
                  res <- object@marginal_AA_subs
              } else if (type == "joint_subs") {
                  res <- object@joint_subs
              } else if (type == "joint_AA_subs") {
                  res <- object@joint_AA_subs
              } else {
                  stop("type should be one of 'marginal_subs',
                             'marginal_AA_subs', 'joint_subs' or 'joint_AA_subs'. ")
              }
              if (nrow(res) == 0) {
                  res <- get.subs_paml_rst(object, type, ...)
                  set.subs(object, type) <- res
              }
              return(res)
          }
          )

##' @name set.subs<-
##' @rdname set.subs-methods
##' @exportMethod "set.subs<-"
##' @aliases set.subs<-,paml_rst-method,ANY-method
##' @usage set.subs(x, type) <- value
setReplaceMethod(f="set.subs",
                 signature = "paml_rst",
                 definition = function(x, type, value){
                     if (type == "marginal_subs") {
                         x@marginal_subs <- value
                     } else if (type == "marginal_AA_subs") {
                         x@marginal_AA_subs <- value
                     } else if (type == "joint_subs") {
                         x@joint_subs <- value
                     } else if (type == "joint_AA_subs") {
                         x@joint_AA_subs <- value
                     } else {
                         stop("type should be one of 'marginal_subs',
                             'marginal_AA_subs', 'joint_subs' or 'joint_AA_subs'. ")
                     }
                 }
                 )


