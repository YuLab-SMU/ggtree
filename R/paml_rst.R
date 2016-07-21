##' read rst and mlb file from baseml output
##'
##'
##' @title read.baseml
##' @param rstfile rst file
##' @param mlbfile mlb file
##' @return A \code{paml_rst} object
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
##' @examples
##' rstfile <- system.file("extdata/PAML_Baseml", "rst", package="ggtree")
##' mlbfile <- system.file("extdata/PAML_Baseml", "mlb", package="ggtree")
##' read.baseml(rstfile, mlbfile)
read.baseml <- function(rstfile, mlbfile) {
    res <- read.paml_rst(rstfile)
    ## res@tip_seq <- read.tip_seq_mlb(mlbfile)
    set.paml_rst_(res)
}

##' read rst file from paml output
##'
##' 
## @importFrom Biostrings readBStringSet
## @importFrom Biostrings toString
##' @title read.paml_rst
##' @param rstfile rst file
##' @return A \code{paml_rst} object
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
##' @examples
##' rstfile <- system.file("extdata/PAML_Baseml", "rst", package="ggtree")
##' read.paml_rst(rstfile)
read.paml_rst <- function(rstfile) {
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
               rstfile = filename(rstfile)
               )
    ## if (!is.null(tip.fasfile)) {
    ##     seqs <- readBStringSet(tip.fasfile)
    ##     tip_seq <- sapply(seq_along(seqs), function(i) {
    ##         toString(seqs[i])
    ##     })
    ##     res@tip_seq <- tip_seq
    ##     res@tip.fasfile <- tip.fasfile
    ## }
    res@tip_seq <- ms[names(ms) %in% phylo$tip.label]
    
    set.paml_rst_(res)
}



##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="paml_rst"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          }
          )


##' get tipseq
##'
##' 
##' @rdname get.tipseq-methods
##' @exportMethod get.tipseq
setMethod("get.tipseq", signature(object="paml_rst"),
          function(object, ...) {
              if (length(object@tip_seq) == 0) {
                  warning("tip sequence not available...\n")
              } else {
                  object@tip_seq
              }
          })


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "paml_rst"),
          function(object) {
              if (length(object@tip_seq) == 0) {
                  warning("tip sequence not available...\n")
              } else {
                  get.fields.tree(object) 
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


##' get substitution information
##'
##' 
##' @rdname get.subs-methods
##' @exportMethod get.subs
setMethod("get.subs", signature(object = "paml_rst"),
          function(object, type, ...) {
              get.subs_paml_rst(object, type)
          }
          )



