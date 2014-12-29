

setClass("paml_rst",
         representation       = representation(
             treetext         = "character",
             phylo            = "phylo",
             seq_type         = "character",
             tip_seq          = "BStringSet",
             marginal_ancseq  = "character",
             joint_ancseq     = "character",
             marginal_subs    = "character",
             joint_subs       = "character",
             marginal_AA_subs = "character",
             joint_AA_subs    = "character",
             tip.fasfile      = "character",
             rstfile          = "character"
         )
         )

read.paml_rst <- function(rstfile, tip.fasfile = NULL) {
    ms <- read_ancseq_paml_rst(rstfile, by="Marginal")
    if (length(grep("[^-ACGT]+", ms[1])) == 0) {
        seq_type = "NT" ## NucleoTide
    } else {
        seq_type = "AA" ## Amino Acid
    }
    phylo <- read_phylo_paml_rst(rstfile)
    class(phylo) <- "list"
    
    res <- new("paml_rst",
               treetext        = read_treetext_paml_rst(rstfile),
               phylo           = phylo, 
               seq_type        = seq_type,
               marginal_ancseq = ms,
               joint_ancseq    = read_ancseq_paml_rst(rstfile, by = "Joint"),
               rstfile = rstfile
               )
    if (!is.null(tip.fasfile)) {
        res@tip_seq <- readBStringSet(tip.fasfile)
        res@tip.fasfile <- tip.fasfile
    }
    return(res)
}

read_treetext_paml_rst<- function(rstfile) {
    ## works fine with baseml and codeml
    rst <- readLines(rstfile)
    tr.idx <- grep("\\)[ 0-9]*;", rst)

    return(rst[tr.idx][1])
}


read_phylo_paml_rst <- function(rstfile) {
    ## works fine with baseml and codeml
    rst <- readLines(rstfile)
    tr.idx <- grep("\\)[ 0-9]*;", rst)

    tr1 <- read.tree(text=rst[tr.idx][1])
    tr3 <- read.tree(text=rst[tr.idx][3])

    edge.idx <- grep("\\d+\\.\\.\\d+", rst)

    edge.idx <- edge.idx[edge.idx < tr.idx[3]]

    nodeNum <- strsplit(rst[edge.idx], split="\\.\\.") %>%
        unlist %>% strsplit(split="[[:space:]]") %>% unlist

    nodeNum %<>% `[`(nzchar(.))

    edge <- matrix(as.numeric(nodeNum), ncol=2, byrow = TRUE)

    label=c(tr3$tip.label, tr3$node.label)
    root <- getRoot(tr3)
    label %<>% `[`(. != root)

    node.length <- data.frame(label=label,
                              length=tr1$edge.length)

    node.length$node <- sub("_\\w+", "", node.length$label)
    node.length$label %<>% sub("\\d+_", "", .)
    
    edge <- as.data.frame(edge)
    colnames(edge) <- c("parent", "node")

    treeinfo <- merge(edge, node.length, by.x="node", by.y="node")
    edge2 <- treeinfo[, c("parent", "node")]
    edge2 %<>% as.matrix
    
    Ntip <- length(tr3$tip.label)

    phylo <- list(edge= cbind(as.numeric(treeinfo$parent),
                      as.numeric(treeinfo$node)),
              edge.length = treeinfo$length,
              tip.label = treeinfo$label[1:Ntip],
              Nnode = tr3$Nnode,
              node.label = c(root, treeinfo$label[(Ntip+1):nrow(treeinfo)]))

    class(phylo) <- "phylo"
    phylo <- reorder.phylo(phylo, "cladewise")
    
    return(phylo)
}

read_ancseq_paml_rst <- function(rstfile, by="Marginal") {
    ## works fine with baseml and codeml
    rst <- readLines(rstfile)

    by <- match.arg(by, c("Marginal", "Joint"))
    query <- paste(by, "reconstruction of ancestral sequences") 
    idx <- grep(query, rst)
    if(length(idx) == 0) {
        ## in some paml setting, joint_ancseq are not available. 
        return("")
    }
    si <- grep("reconstructed sequences", rst)
    idx <- si[which.min(abs(si-idx))]

    nl <- strsplit(rst[idx+2], split=" ") %>% unlist %<>% `[`(nzchar(.))
    N <- as.numeric(nl[1])
    seq.leng <- as.numeric(nl[2])

    seqs <- rst[(idx+4):(idx+3+N)]

    seq.name <- character(N)
    res <- character(N)
    for (i in 1:N) {
        ss <- gsub(" ", "", seqs[i])
        nn <- nchar(ss)
        res[i] <- substring(ss, nn-seq.leng+1,nn)
        seq.name[i] <- substring(ss, 1, nn-seq.leng)
    }
    seq.name <- sub("\\w+#", "", seq.name)
    names(res) <- seq.name

    return(res)
}
