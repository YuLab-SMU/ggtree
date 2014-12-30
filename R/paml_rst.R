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

## nts <- get.subs(object@phylo, c(object@tip_seq, object@marginal_ancseq)) 
