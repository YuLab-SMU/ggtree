has.slot <- function(object, slotName) {
    if (!isS4(object)) {
        return(FALSE)
    }
    
    slot <- tryCatch(slot(object, slotName), error=function(e) NULL)
    ! is.null(slot)
}

has.extraInfo <- function(object) {
    if (!is.tree(object)) {
        return(FALSE)
    }

    if (! has.slot(object, "extraInfo")) {
        return(FALSE)
    }

    extraInfo <- object@extraInfo

    if (nrow(extraInfo) > 0) {
        return(TRUE)
    }

    return(FALSE)        
}

append_extraInfo <- function(df, object) {
    if (has.extraInfo(object)) {
        info <- object@extraInfo
        res <- merge(df, info, by.x=c("node", "parent"), by.y=c("node", "parent"))
    } else {
        res <- df
    }

    i <- order(res$node, decreasing = FALSE)
    res <- res[i,]
    return(res)
}

get.fields.tree <- function(object) {
    if (is(object, "codeml")) {
        fields <- c(get.fields(object@rst),
                    get.fields(object@mlc))
        fields <- unique(fields)
    } else {
        fields <- object@fields
    }
    
    if (has.slot(object, "extraInfo")) {
        extraInfo <- object@extraInfo
        if (nrow(extraInfo) > 0) {
            cn <- colnames(extraInfo)
            i <- match(c("x", "y", "isTip", "node", "parent", "label", "branch", "branch.length"), cn)
            i <- i[!is.na(i)]
            fields %<>% c(cn[-i])
        }
    }
    return(fields)
}

print_fields <- function(object, len=5) {
    fields <- get.fields(object)    
    n <- length(fields)
    i <- floor(n/len)
    for (j in 0:i) {
        ii <- 1:len + len * j
        if (j == i) {
            x <- n %% len
            if (x == 0) {
                ii <- NULL
            } else {
                ii <- ii[1:x]
            }
        }

        if (!is.null(ii)) {
            cat("\t", paste0("'",
                             paste(fields[ii], collapse="',\t'"),
                             "'")
                )
        }
        if ( j == i) {
            cat(".\n")
        } else {
            cat(",\n")
        }
    }
}

plot.subs <- function(x, layout, show.tip.label,
                      tip.label.size,
                      tip.label.hjust,
                      position, annotation,
                      annotation.color = "black",
                      annotation.size=3, ...) {
    
    p <- ggtree(x, layout=layout, ...)
    if (show.tip.label) {
        p <- p + geom_tiplab(hjust = tip.label.hjust,
                             size  = tip.label.size)
    }
    if (!is.null(annotation) && !is.na(annotation)) {
        p <- p + geom_text(aes_string(x=position, label=annotation),
                           size=annotation.size,
                           color=annotation.color, vjust=-.5)
    }
    p + theme_tree2()
}

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


jplace_treetext_to_phylo <- function(tree.text) {
    ## move edge label to node label separate by @
    tr <- gsub('(:[0-9\\.eE-]+)\\{(\\d+)\\}', '\\@\\2\\1', tree.text)
    phylo <- read.tree(text=tr)
    if (length(grep('@', phylo$tip.label)) > 0) {
        phylo$node.label[1] %<>% gsub("(.*)\\{(\\d+)\\}", "\\1@\\2", .)
        tip.edgeNum <- as.numeric(gsub("[^@]*@(\\d*)", "\\1",phylo$tip.label))
        node.edgeNum <- as.numeric(gsub("[^@]*@(\\d*)", "\\1",phylo$node.label))
        phylo$tip.label %<>% gsub("@\\d+", "", .)
        phylo$node.label %<>% gsub("@\\d+", "", .)
        if (all(phylo$node.label == "")) {
            phylo$node.label <- NULL
        }

        N <- getNodeNum(phylo)
        edgeNum.df <- data.frame(node=1:N, edgeNum=c(tip.edgeNum, node.edgeNum))
        edgeNum.df <- edgeNum.df[!is.na(edgeNum.df[,2]),]
        edgeNum <- edgeNum.df[match( phylo$edge[,2], edgeNum.df$node), 2]
        attr(phylo, "edgeNum") <- edgeNum
    }
    return(phylo)
}

extract.treeinfo.jplace <- function(object, layout="phylogram", ladderize=TRUE, right=FALSE) {

    tree <- get.tree(object)
    
    df <- fortify.phylo(tree, layout=layout, ladderize=ladderize, right=right)

    edgeNum <- attr(tree, "edgeNum")
    if (!is.null(edgeNum)) {
        edgeNum.df <- data.frame(node=tree$edge[,2], edge=edgeNum)
        df2 <- merge(df, edgeNum.df, by.x="node", by.y="node", all.x=TRUE) 
        df <- df2[match(df[, "node"], df2[, "node"]),]
    }
    attr(df, "ladderize") <- ladderize
    attr(df, "right") <- right
    return(df)
}


is.character_beast <- function(stats3, cn) {
    for (i in 1:nrow(stats3)) {
        if ( is.na(stats3[i,cn]) ) {
            next
        } else {
            ## res <- grepl("[a-df-zA-DF-Z]+", unlist(stats3[i, cn]))
            ## return(all(res == TRUE))
            res <- grepl("^[0-9\\.eE-]+$", unlist(stats3[i, cn]))
            return(all(res == FALSE))
        }
    }
    return(FALSE)
}


is.tree <- function(x) {
    if (class(x) %in% c("phylo",
                        "phylo4",
                        "jplace",
                        "baseml",
                        "paml_rst",
                        "baseml_mlc",
                        "codeml",
                        "hyphy",
                        "beast")
        ) {
        return(TRUE)
    }
    return(FALSE)
}



color_scale <- function(c1="grey", c2="red", n=100) {
    pal <- colorRampPalette(c(c1, c2))
    colors <- pal(n)
    return(colors)
}

getIdx <- function(v, MIN, MAX, interval=NULL) {
    res <- sapply(v, getIdx_internal, MIN=MIN, MAX=MAX, interval=interval)
    attr(res, "interval") <- interval
    return(res)
}

getIdx_internal <- function(v, MIN, MAX, interval=NULL) {
    if (is.na(v)) {
        return(NA)
    }
    if ( MIN == MAX ) {
        return(100)
    }
    res <- max(which(interval <= v))
    return(res)
}


get_color_attribute <- function(p) {
    p$data[, "color"]
}

is.tree_attribute <- function(df, var) {
    if(length(var) == 1 &&
       !is.null(var)    &&
       var %in% colnames(df)) {
        return(TRUE)
    } 
    return(FALSE)
}

is.tree_attribute_ <- function(p, var) {
    is.tree_attribute(p$data, var)
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
    param <- attr(pg, "param")
    pg$data <- fortify(tree,
                       layout        = param[["layout"]],
                       yscale        = param[["yscale"]],
                       ladderize     = param[["ladderize"]],
                       right         = param[["right"]],
                       branch.length = param[["branch.length"]],
                       ndigits       = param[["ndigits"]])
    return(pg)
}


## `%IN%` <- function(x, table) {
##     ii <- NULL ## satisify codetools
##     idx <- match(x, table, nomatch=NA)
##     ii <<- idx[!is.na(idx)]
##     res <- as.logical(idx)
##     res[is.na(res)] <- FALSE
##     return(res)
## }
## geom_nplace <- function(data, map, place, ...) {
##     label <- NULL
##     ii <- 1:nrow(data)
##     geom_text(subset=.(label %IN% data[[map]]), label = data[ii, place], ...)
## }


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
##' @rdname dotFun
##' @export
##' @title .
##' @param ... expression
##' @param .env environment
##' @return expression
##' @examples
##' x <- 1
##' eval(.(x)[[1]])
. <- function (..., .env = parent.frame()) {
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}

##' pipe
##' @importFrom magrittr %>%
##' @name %>%
##' @export
##' @rdname pipe
##' @param lhs left hand side
##' @param rhs right hand side
##' @usage lhs \%>\% rhs
##' @seealso
##' \link[magrittr]{pipe}
NULL


