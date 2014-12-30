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
    set.paml_rst_(res)
}

set.paml_rst_ <- function(object) {
    if (!is(object, "paml_rst")) {
        stop("object should be an instance of 'paml_rst'")
    }
    if (length(object@tip_seq) == 0) {
        return(object)
    }
    
    types <- get.fields(object)
    for (type in types) {
        value <- subs_paml_rst(object, type)
        if (type == "marginal_subs") {
            object@marginal_subs <- value
        } else if (type == "marginal_AA_subs") {
            object@marginal_AA_subs <- value
        } else if (type == "joint_subs") {
            object@joint_subs <- value
        } else if (type == "joint_AA_subs") {
            object@joint_AA_subs <- value
        }
    }
    return(object)
}


##' @rdname show-methods
##' @exportMethod show
setMethod("show", signature(object = "paml_rst"),
          function(object) {
              cat("'paml_rst' S4 object that stored information of\n\t",
                  paste0("'", object@rstfile, "'"))
              if (length(object@tip.fasfile) != 0) {
                  cat(paste0("and '", object@tip.fasfile, "'"), ".\n")
              } else {
                  cat(".\n")
              }
              cat("  with the following features available:\n")
              cat("\t", paste0("'",
                               paste(get.fields(object), collapse="',   '"),
                               "'"),
                  "\n")
              
          }
          )

##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "paml_rst"),
          function(object) {
              if (length(object@tip_seq) == 0) {
                  warning("tip sequence not available...\n")
              } else {
                  object@fields
              }
          }
          )

##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object = "paml_rst"),
          function(object) {
              object@phylo
          }
          )

##' @rdname plot-methods
##' @exportMethod plot
setMethod("plot", signature(x = "paml_rst"),
          function(x, layout = "phylogram",
                   show.tip.label = TRUE,
                   position = "branch",
                   annotation = "marginal_subs",
                   ...) {
              
              p <- ggtree(x, layout=layout)
              if (show.tip.label) {
                  p <- p + geom_tiplab()
              }
              if (!is.null(annotation) && !is.na(annotation)) {
                  anno <- get.subs(x, type=annotation)
                  p <- p %<+% anno + geom_text(aes_string(x=position, label="subs"),
                                               size=3, vjust=-.5)
              }
              p + theme_tree2()
          })


##' @rdname get.subs-methods
##' @exportMethod get.subs
setMethod("get.subs", signature(object = "paml_rst"),
          function(object, type, ...) {
              get.subs_paml_rst(object, type)
          }
          )



