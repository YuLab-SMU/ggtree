##' parsing phylip tree format
##'
##' 
##' @title read.phylip
##' @param file phylip file
##' @return an instance of 'phylip'
##' @export
##' @importFrom Biostrings BStringSet
##' @author Guangchuang Yu
read.phylip <- function(file) {
    phylip <- readLines(file)
    i <- grep("^\\d+$", phylip)
    if (length(i) != 1) {
        stop("input file is not phylip tree format...")
    }
    n <- length(phylip)
    ntree <- as.numeric(phylip[i])
    trees <- read.tree(text=phylip[(i+1):n])

    phylipInfo <- strsplit(phylip[1], split="\\s") %>% unlist
    nseq <- phylipInfo[1]
    seqLen <- phylipInfo[2]
    if (nseq != i-2) {
        stop("only sequential format is supported...\n-> see http://evolution.genetics.washington.edu/phylip/doc/sequence.html")
    }
    seqlines <- phylip[2:(i-1)]
    seq_with_name <- lapply(seqlines, function(x) unlist(strsplit(x, "\\s+")))
    seqs <- sapply(seq_with_name, function(x) x[2])
    names(seqs) <- sapply(seq_with_name, function(x) x[1])
    seq_obj <- BStringSet(seqs)

    if (any(width(seq_obj) != seqLen)) {
        stop(paste("sequence length not consistent...\n->", paste0(width(seq_obj), collapse=" ")))
    }
    
    new("phylip",
        file = file,
        phylo = trees,
        ntree = ntree,
        sequence = seq_obj
        )
}


