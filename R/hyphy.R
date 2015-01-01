## tree <- read.tree("labelledtree.tree")
## nex <- read.nexus("ancseq.nex")

## ancseq <- "ancseq.nex"
## tree <- "labelledtree.tree"

##' nwk <- system.file("extdata/HYPHY", "labelledtree.tree", package="ggtree")
##' ancseq <- system.file("extdata/HYPHY", "ancseq.nex", package="ggtree")
read.hyphy <- function(nwk, ancseq, tip.fasfile=NULL) {
    anc <- scan(ancseq, what="", sep="\n", quiet=TRUE)
    end <- grep("END;", anc)
    
    seq.start <- grep("MATRIX", anc)
    seq.end   <- end[end > seq.start][1]
    seq       <- anc[(seq.start+1):(seq.end-1)]
    seq       <- gsub(" ", "", seq)

    label.start <- grep("TAXLABELS", anc)
    label.end   <- end[end > label.start][1]
    label       <- anc[(label.start+1):(label.end-1)]
    
    label <- sub("^\t+", "", label)
    label <- sub("\\s*;$", "", label)
    label <- unlist(strsplit(label, split="\\s+"))
    label <- gsub("'|\"", "", label)

    names(seq) <- label

    tr <- read.tree(nwk)
    nl <- tr$node.label
    nl[nl == ""] <- "Node1"
    tr$node.label <- nl

    
}

