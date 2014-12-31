
get.subs_ <- function(tree, fasta, translate=TRUE, removeGap=TRUE) {
    N <- getNodeNum(tree)
    node <- 1:N
    parent <- sapply(node, getParent, tr=tree)
    label <- getNodeName(tree)
    subs <- sapply(seq_along(node), function(i) {
        if (i == getRoot(tree)) {
            return(NA)
        }
        res <- getSubsLabel(fasta, label[parent[i]], label[i], translate, removeGap)
        if (is.null(res)) {
            return('')
        }
        return(res)
    })
    
    dd <- data.frame(node=node, parent=parent, label=label, subs=subs)
    dd <- dd[dd$parent != 0,]
    dd <- dd[, -c(1,2)]
    dd[,1] <- as.character(dd[,1])
    dd[,2] <- as.character(dd[,2])
    return(dd)
}

getSubsLabel <- function(seqs, A, B, translate, removeGap) {
    seqA <- seqs[A]
    seqB <- seqs[B]

    if (translate == TRUE) {
        AA <- seqA %>% seq2codon %>% codon2AA
        BB <- seqB %>% seq2codon %>% codon2AA
    } else {
        n <- nchar(seqA) ## should equals to nchar(seqB)
        AA <- substring(seqA, 1:n, 1:n)
        BB <- substring(seqB, 1:n, 1:n)
    }
    
    ii <- which(AA != BB)

    if (removeGap == TRUE) {
        if (length(ii) > 0 && translate == TRUE) {
            ii <- ii[AA[ii] != "X" & BB[ii] != "X"]
        }

        if (length(ii) > 0 && translate == FALSE) {
            ii <- ii[AA[ii] != "-" & BB[ii] != "-"]
        }
    }
    
    if (length(ii) == 0) {
        return(NULL)
    }
    
    res <- paste(AA[ii], ii, BB[ii], sep="", collapse="/")
    return(res)
}

seq2codon <- function(x) {
    substring(x, first=seq(1, nchar(x)-2, 3), last=seq(3, nchar(x), 3))
}

##' @importFrom Biostrings GENETIC_CODE
codon2AA <- function(codon) {
    aa <- GENETIC_CODE[codon]
    aa[is.na(aa)] <- "X"
    return(aa)
}


getPhyInfo <- function(phy) {
    line1 <- readLines(phy, n=1)
    res <- strsplit(line1, split="\\s")[[1]]
    res <- res[res != ""]

    return(list(num=as.numeric(res[1]), width=as.numeric(res[2])))
}

get_seqtype <- function(seq) {
    if (length(grep("[^-ACGT]+", seq[1])) == 0) {
        seq_type = "NT" ## NucleoTide
    } else {
        seq_type = "AA" ## Amino Acid
    }
    return(seq_type)
}

reverse.treeview <- function(tv) {
    tv$data <- reverse.treeview.data(tv$data)
    return(tv)
}

reverse.treeview.data <- function(df) {
    root <- df$node[df$node == df$parent]
    df$x <- getXcoord2(df$x, root, df$parent, df$node,
                       df$length, start=max(df$x), rev=TRUE)
    return(df)
}


extract.treeinfo.jplace <- function(tree.text, layout="phylogram", ladderize=TRUE, right=FALSE) {
    ## move edge label to node label separate by @
    tr <- gsub('(:[0-9.e-]+)\\{(\\d+)\\}', '\\@\\2\\1', tree.text)
    tree <- read.tree(text=tr)
    df <- fortify.phylo(tree, layout=layout, ladderize=ladderize, right=right)

    root.idx <- which(df$parent == df$node)
    root.lab <- df[,"label"]
    df$label[root.idx] <- gsub("(.*)\\{(\\d+)\\}", "\\1@\\2", df$label[root.idx])

    if ( length(grep('@', df$label)) > 0) {
        df$edge <- as.numeric(gsub("[^@]*@(\\d*)", "\\1",df$label))
    }
    
    ## remove edge label from node label
    df$label <- gsub("@\\d*", "", df$label)
    df$label[df$label == ""] <- NA
    attr(df, "ladderize") <- ladderize
    attr(df, "right") <- right
    return(df)
}

is.tree <- function(x) {
    if (class(x) %in% c("phylo",
                        "phylo4",
                        "jplace",
                        "baseml",
                        "paml_rst",
                        "baseml_mlc")
        ) {
        return(TRUE)
    }
    return(FALSE)
}

`%add%` <- function(p, data) {
    p$data <- p$data %add2% data
    return(p)
}

`%add2%` <- function(d1, d2) {
    dd <- merge(d1, d2, by.x="label", by.y=1, all.x=TRUE)
    dd <- dd[match(d1$node, dd$node),]
    return(dd)
}

`%place%` <- function(pg, tree) {
    pg$data <- fortify(tree)
    return(pg)
}

`%IN%` <- function(x, table) {
    ii <- NULL ## satisify codetools
    idx <- match(x, table, nomatch=NA)
    ii <<- idx[!is.na(idx)]
    res <- as.logical(idx)
    res[is.na(res)] <- FALSE
    return(res)
}

roundDigit <- function(d) {
    i <- 0
    while(d < 1) {
        d <- d * 10
        i <- i + 1
    }
    round(d)/10^i
}


## . function was from plyr package
##' capture name of variable
##'
##'
##' @rdname dotFun
##' @title .
##' @param ... expression
##' @param .env environment
##' @export
##' @return expression
##' @examples
##' x <- 1
##' eval(.(x)[[1]])
. <- function (..., .env = parent.frame()) {
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}


