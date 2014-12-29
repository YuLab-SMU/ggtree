##' read baseml output
##'
##' 
##' @title read.codeml 
##' @param rstfile rst file
##' @param mlcfile mlc file
##' @return A \code{codeml} object
##' @export
##' @author ygc
read.codeml <- function(rstfile, mlcfile) {
    new("codeml",
        rst = read.paml_rst(rstfile),
        mlc = read.codeml_mlc(mlcfile)
        )
}

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
    
    res <- new("paml_rst",
               treetext        = read.treetext_paml_rst(rstfile),
               phylo           = phylo, 
               seq_type        = get_seqtype(ms),
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

    
##' read mlc file of codeml output
##'
##' 
##' @title read.codeml_mlc 
##' @param mlcfile mlc file
##' @return A \code{codeml_mlc} object
##' @export
##' @author ygc
read.codeml_mlc <- function(mlcfile) {
    tip_seq <- read.tip_seq_mlc(mlcfile)
    new("codeml_mlc",
        treetext = read.treetext_paml_mlc(mlcfile),
        phylo    = read.phylo_paml_mlc(mlcfile),
        dNdS     = read.dnds_mlc(mlcfile),
        seq_type = get_seqtype(tip_seq),
        tip_seq  = tip_seq,
        mlcfile  = mlcfile)
}

read.tip_seq_mlc <- function(mlcfile) {
    info <- getPhyInfo(mlcfile)
    mlc <- readLines(mlcfile)
    ## remove blank lines
    blk <- grep("^\\s*$", mlc)
    if (length(blk) > 0) {
        mlc <- mlc[-blk]
    }

    seqs <- mlc[2:(info$num+1)]
    seqs <- gsub("\\s+", "", seqs)
    wd <- info$width
    res <- sapply(seqs, function(x) substring(x, (nchar(x) - wd + 1), nchar(x)))
    nn <- sapply(seqs, function(x) substring(x, 1, (nchar(x) - wd)))
    names(res) <- nn
    return(res)
}

read.dnds_mlc <- function(mlcfile) {
    mlc <- readLines(mlcfile)
    i <- grep("dN & dS for each branch", mlc)
    j <- grep("tree length for dN", mlc)
    
    mlc <- mlc[i:j]
    hi <- grep("dN/dS", mlc)
    cn <- strsplit(mlc[hi], " ") %>% unlist %>% `[`(nzchar(.))

    ii <- grep("\\d+\\.\\.\\d+", mlc)
    info <- mlc[ii]
    info %<>% sub("^\\s+", "", .)
    info %<>% sub("\\s+$", "", .)
    res <- t(sapply(info, function(x) {
        y <- unlist(strsplit(x, "\\s+"))
        edge <- unlist(strsplit(y[1], "\\.\\."))
        yy <- c(edge, y[-1])
        as.numeric(yy)
    }))
             
    row.names(res) <- NULL
    colnames(res) <- c("parent", "node", cn[-1])
    return(res)
}

read.treetext_paml_mlc <- function(mlcfile) {
    read.treetext_paml(mlcfile, "mlc")
}

read.treetext_paml_rst<- function(rstfile) {
    read.treetext_paml(rstfile, "rst")
}

read.treetext_paml <- function(file, by) {
    ## works fine with baseml and codeml
    x <- readLines(file)
    tr.idx <- get_tree_index_paml(x)

    if ( by == "rst" ) {
        ii <- 1
    } else if (by == "mlc") {
        ii <- 3
    } else {
        stop("_by_ should be one of 'rst' or 'mlc'")
    }
        
    return(x[tr.idx][ii])
}

##' @importFrom ape Ntip
read.phylo_paml_mlc <- function(mlcfile) {
    mlc <- readLines(mlcfile)
    edge <- get_tree_edge_paml(mlc)

    tr.idx <- get_tree_index_paml(mlc)
    tr2 <- read.tree(text=mlc[tr.idx[2]])
    tr3 <- read.tree(text=mlc[tr.idx[3]])

    treeinfo <- as.data.frame(edge)
    colnames(treeinfo) <- c("parent", "node")
    treeinfo$length <- NA
    treeinfo$label <- NA
    treeinfo$isTip <- FALSE
    ntip <- Ntip(tr3)
    ii <- match(tr2$tip.label, treeinfo$node)
    treeinfo[ii, "label"] <- tr3$tip.label
    treeinfo[ii, "isTip"] <- TRUE
    jj <- match(1:ntip, tr3$edge[,2])
    treeinfo[ii, "length"] <- tr3$edge.length[jj]

    root <- getRoot(tr3) ## always == (Ntip(tr3) + 1)
    currentNode <- treeinfo$label[ii]
    treeinfo.tr3 <- fortify(tr3)

    while(any(is.na(treeinfo$length))) {
        pNode <- treeinfo$label %in% currentNode %>% which %>%
            treeinfo$parent[.] %>% unique
        pNode.tr3 <- treeinfo.tr3$label %in% currentNode %>% which %>%
            treeinfo.tr3$parent[.] %>% unique

        pNode %<>% `[`(. != root)
        pNode.tr3 %<>% `[`(. != root)

        i <- match(pNode, treeinfo$node)
        j <- match(pNode.tr3, treeinfo.tr3$node)

        treeinfo$label[i] <- as.character(treeinfo$node[i])
        treeinfo.tr3$label[j] <- treeinfo$label[i]
        treeinfo$length[i] <- treeinfo.tr3$length[j]

        kk <- treeinfo$parent %in% treeinfo$node[is.na(treeinfo$length)]
        currentNode <- treeinfo$label[kk]
    }
      
    phylo <- with(treeinfo,
                  list(edge= cbind(as.numeric(parent),
                           as.numeric(node)),
                       edge.length = length,
                       tip.label = label[isTip],
                       Nnode = tr3$Nnode,
                       node.label = c(root, label[!isTip])
                       )
                  )
    class(phylo) <- "phylo"
    phylo <- reorder.phylo(phylo, "cladewise")
    return(phylo)
}

##' @importFrom ape reorder.phylo
read.phylo_paml_rst <- function(rstfile) {
    ## works fine with baseml and codeml
    rst <- readLines(rstfile)
    tr.idx <- get_tree_index_paml(rst)

    tr1 <- read.tree(text=rst[tr.idx][1])
    tr3 <- read.tree(text=rst[tr.idx][3])

    edge <- get_tree_edge_paml(rst)
    
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

read.ancseq_paml_rst <- function(rstfile, by="Marginal") {
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


get_tree_index_paml <- function(paml) {
    grep("\\)[ \\.0-9]*;", paml)
}

get_tree_edge_index_paml <- function(paml) {
    grep("\\d+\\.\\.\\d+", paml)
}

get_tree_edge_paml <- function(paml) {
    tr.idx <- get_tree_index_paml(paml)

    edge.idx <- get_tree_edge_index_paml(paml)
    edge.idx <- edge.idx[edge.idx < tr.idx[3]]

    nodeNum <- strsplit(paml[edge.idx], split="\\.\\.") %>%
        unlist %>% strsplit(split="[[:space:]]") %>% unlist

    nodeNum %<>% `[`(nzchar(.))

    edge <- matrix(as.numeric(nodeNum), ncol=2, byrow = TRUE)

    return(edge)
}

