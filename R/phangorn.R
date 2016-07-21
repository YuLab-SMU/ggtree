##' tree annotation of sequence substitution by comparing to parent node
##'
##' 
##' @title treeAnno.pml
##' @param pmlTree tree in pml object, output of phangorn::optim.pml 
##' @param type one of 'ml' and 'bayes' for inferring ancestral sequences
##' @return phangorn object
##' @importFrom ape read.tree
##' @importFrom ape reorder.phylo
##' @export
##' @author Yu Guangchuang
phyPML <- function(pmlTree, type = "ml") {
    sequences <- pmlToSeqString(pmlTree, type, includeAncestor=TRUE)
    tr <- pmlTree$tree
    tr <- reorder.phylo(tr)
        
    if (is.null(tr$node.label)) {
        n <- length(tr$tip.label)
        nl <- (n+1):(2*n-2)
        tr$node.label <- as.character(nl)
    } else {
        names(sequences) <- c(tr$tip.label, tr$node.label)
    }
    
    seq_type <- get_seqtype(sequences)
    res <- new("phangorn",
               phylo = tr,
               fields = "subs",
               seq_type = seq_type,
               ancseq = sequences)
    

    res@tip_seq <- sequences[names(sequences) %in% tr$tip.label]

    res@subs <- get.subs_(res@phylo, sequences, translate=FALSE)
    if (seq_type == "NT") {
        res@AA_subs <- get.subs_(res@phylo, sequences, translate=TRUE)
        res@fields %<>% c("AA_subs")
    }
    
    return(res)
}



##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "phangorn"),
          function(object) {
              cat("'phangorn' S4 object that stored ancestral sequences inferred by 'phangorn::ancestral.pml'", ".\n\n")
              cat("...@ tree: ")
              print.phylo(get.tree(object))
              fields <- get.fields(object)
              cat("\nwith the following features available:\n")
              cat("\t", paste0("'",
                               paste(fields, collapse="',\t'"),
                               "'."),
                  "\n")
          })


##' @rdname get.subs-methods
##' @exportMethod get.subs
setMethod("get.subs", signature(object = "phangorn"),
          function(object, type, ...) {
              if (type == "AA_subs")
                  return(object@AA_subs)
              return(object@subs)
          }
          )


##' @rdname groupClade-methods
##' @exportMethod groupClade
setMethod("groupClade", signature(object="phangorn"),
          function(object, node, group_name="group") {
              groupClade_(object, node, group_name)
          })

##' @rdname scale_color-methods
##' @exportMethod scale_color
setMethod("scale_color", signature(object="phangorn"),
          function(object, by, ...) {
              scale_color_(object, by, ...)
          })


##' @rdname gzoom-methods
##' @exportMethod gzoom
setMethod("gzoom", signature(object="phangorn"),
          function(object, focus, subtree=FALSE, widths=c(.3, .7)) {
              gzoom.phylo(get.tree(object), focus, subtree, widths)
          })


##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="phangorn"),
          function(object,...) {
              object@phylo
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="phangorn"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )


##' convert pml object to XStringSet object
##'
##' 
##' @title pmlToSeq 
##' @param pml pml object
##' @param includeAncestor logical 
##' @return XStringSet
## @importFrom Biostrings DNAStringSet
##' @export
##' @author ygc
pmlToSeq <- function(pml, includeAncestor=TRUE) {
    DNAStringSet <- get_fun_from_pkg("Biostrings", "DNAStringSet")
    pmlToSeqString(pml, includeAncestor) %>%
        DNAStringSet
}

## @importFrom phangorn ancestral.pml
pmlToSeqString <- function(pml, type, includeAncestor=TRUE) {
    if (includeAncestor == FALSE) {
        phyDat <- pml$data
    } else {
        ancestral.pml <- get_fun_from_pkg("phangorn", "ancestral.pml")
        phyDat <- ancestral.pml(pml, type)
    }
    
    phyDat <- matrix2vector.phyDat(phyDat)
    ## defined by phangorn
    labels <- c("a", "c", "g", "t", "u", "m", "r", "w", "s", 
                "y", "k", "v", "h", "d", "b", "n", "?", "-")
    labels <- toupper(labels)

    index <- attr(phyDat, "index")
    
    result <- do.call(rbind, phyDat)
    result <- result[, index, drop=FALSE]

    res <- apply(result, 2, function(i) labels[i])
    res <- apply(res, 1, paste, collapse="")
    names(res) <- rownames(result)
    return(res)
}



matrix2vector.phyDat <- function(x) {
    index <- attr(x, "index")
    res <- lapply(x, matrix2vector.phyDat.item)
    names(res) <- names(x)
    attr(res, "index") <- index
    class(res) <- "phyDat"
    return(res)
}

matrix2vector.phyDat.item <- function(y) {
    ii <- apply(y, 1, function(xx) {
        ## return index of a c g and t, if it has highest probability
        ## otherwise return index of -
        jj <- which(xx == max(xx))
        if ( length(jj) > 1) {
            if (length(jj) < 4) {
                warning("ambiguous found...\n")
            } else {
                ## cat("insertion found...\n")
            }
            ## 18 is the gap(-) index of base character defined in phangorn
            ## c("a", "c", "g", "t", "u", "m", "r", "w", "s", 
	    ##   "y", "k", "v", "h", "d", "b", "n", "?", "-")
            18
        } else {
            jj
        }
    })
    unlist(ii)
}

