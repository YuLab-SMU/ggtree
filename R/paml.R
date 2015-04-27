
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

read.tip_seq_mlb <- read.tip_seq_mlc

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
    colnames(res) <- gsub("\\*", "_x_", colnames(res))
    colnames(res) <- gsub("\\/", "_vs_", colnames(res))
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
    parent <- node <- label <- NULL
    
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
    ii <- match(tr2$tip.label, treeinfo[, "node"])
    treeinfo[ii, "label"] <- tr3$tip.label
    treeinfo[ii, "isTip"] <- TRUE
    ## jj <- match(1:ntip, tr3$edge[,2])
    treeinfo[ii, "length"] <- tr2$edge.length[ii] ##tr3$edge.length[jj]

    root <- getRoot(tr3) ## always == (Ntip(tr3) + 1)
    currentNode <- treeinfo$label[ii]
    treeinfo.tr3 <- fortify(tr3)

    treeinfo$visited <- FALSE
    while(any(treeinfo$visited == FALSE)) {
        pNode <- c()
        for( kk in currentNode ) {
            i <- which(treeinfo$label == kk)
            treeinfo[i, "visited"] <- TRUE
            j <- which(treeinfo.tr3$label == kk)
            ip <- treeinfo[i, "parent"]
            if (ip != root) {
                ii <- which(treeinfo[, "node"] == ip)
                if (treeinfo$visited[ii] == FALSE) {
                    jp <- treeinfo.tr3[j, "parent"]
                    jj <- which(treeinfo.tr3[, "node"] == jp)
                    treeinfo[ii, "label"] <- as.character(ip)
                    treeinfo.tr3[jj, "label"] <- as.character(ip)
                    treeinfo[ii, "length"] <- treeinfo.tr3[jj, "branch.length"]
                    pNode <- c(pNode, ip)
                }
                treeinfo[ii, "visited"] <- TRUE
            }
            
        }
        currentNode <- unique(pNode)
    }

    phylo <- with(treeinfo,
                  list(edge= cbind(as.numeric(parent),
                           as.numeric(node)),
                       edge.length = length,
                       tip.label = label[order(node)][1:ntip],
                       Nnode = tr3$Nnode,
                       node.label = c(root, label[order(node)][-c(1:ntip)])
                       )
                  )
    class(phylo) <- "phylo"
    phylo <- reorder.phylo(phylo, "cladewise")
    return(phylo)
}

##' @importFrom ape reorder.phylo
read.phylo_paml_rst <- function(rstfile) {
    parent <- node <- label <- NULL
    
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

    ## node.length$node <- sub("_\\w+", "", node.length$label
    node.length$node <- gsub("^(\\d+)_.*", "\\1", node.length$label)
    node.length$label %<>% sub("\\d+_", "", .)
    
    edge <- as.data.frame(edge)
    colnames(edge) <- c("parent", "node")

    treeinfo <- merge(edge, node.length, by.x="node", by.y="node")
    edge2 <- treeinfo[, c("parent", "node")]
    edge2 %<>% as.matrix
    
    ntip <- Ntip(tr3)

    phylo <- with(treeinfo,
                  list(edge= cbind(as.numeric(parent),
                      as.numeric(node)),
                       edge.length = length,
                       tip.label = label[order(node)][1:ntip],
                       Nnode = tr3$Nnode,
                       node.label = c(root, label[order(node)][-c(1:ntip)])
                       )
                  )

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


get.subs_paml_rst <- function(object, type) {
    if (!is(object, "paml_rst")) {
        stop("object should be an instance of 'paml_rst'")
    }
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
    return(res)
}

subs_paml_rst <- function(x, type, ...) {
    if (class(x) != "paml_rst") {
        stop("x should be an object of paml_rst...")
    }
    seqs <- x@tip_seq
    if (length(seqs) == 0) {
        stop("tip sequences is not available...")
    }
    if (type %in% c("marginal_subs", "marginal_AA_subs")) {
        seqs <- c(seqs, x@marginal_ancseq)
    } else if (type %in% c("joint_subs", "joint_AA_subs")){
        seqs <- c(seqs, x@joint_ancseq)
    } else {
        stop("type should be one of 'marginal_subs',
                             'marginal_AA_subs', 'joint_subs' or 'joint_AA_subs'. ")
    }
    if( type %in% c("marginal_subs", "joint_subs")) {
        translate <- FALSE
    } else {
        translate <- TRUE
    }
    
    get.subs_(x@phylo, seqs, translate=translate, ...)
}
