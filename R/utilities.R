
filename <- function(file) {
    ## textConnection(text_string) will work just like a file
    ## in this case, just set the filename as ""
    file_name <- ""
    if (is.character(file)) {
        file_name <- file
    }
    return(file_name)
}


##' @importFrom ggplot2 last_plot
get_tree_view <- function(tree_view) {
    if (is.null(tree_view)) 
        tree_view <- last_plot()

    return(tree_view)
}


##' @importFrom methods .hasSlot is missingArg new slot slot<-
has.slot <- function(object, slotName) {
    if (!isS4(object)) {
        return(FALSE)
    }
    .hasSlot(object, slotName)
    ## slot <- tryCatch(slot(object, slotName), error=function(e) NULL)
    ## ! is.null(slot)
}

has.field <- function(tree_object, field) {
    if ( ! field %in% get.fields(tree_object) ) {
        return(FALSE)
    }
    
    if (is(tree_object, "codeml")) {
        is_codeml <- TRUE
        tree <- tree_object@rst
    } else {
        is_codeml <- FALSE
        tree <- tree_object
    }
    
    if (.hasSlot(tree, field)) {
        has_slot <- TRUE
    } else {
        has_slot <- FALSE
    }
    
    if (has_slot == FALSE) {
        if (has.extraInfo(tree_object) == FALSE) {
            return(FALSE)
        }
        
        if (nrow(tree_object@extraInfo) == 0) {
            return(FALSE)
        }
        
        if (!field %in% colnames(tree_object@extraInfo)) {
            return(FALSE)
        }
    }
    res <- TRUE
    attr(res, "has_slot") <- has_slot
    attr(res, "is_codeml") <- is_codeml
    return(res)
}

has.extraInfo <- function(object) {
    if (!is.tree(object)) {
        return(FALSE)
    }

    if (! .hasSlot(object, "extraInfo")) {
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
        if ("parent" %in% colnames(info)) {
            res <- merge(df, info, by.x=c("node", "parent"), by.y=c("node", "parent"))
        } else {
            res <- merge(df, info, by.x="node", by.y="node")
        }
    } else {
        return(df)
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

.add_new_line <- function(res) {
    ## res <- paste0(strwrap(res, 50), collapse="\n")
    ## res %<>% gsub("\\s/\n", "\n", .) %>% gsub("\n/\\s", "\n", .) 
    if (nchar(res) > 50) {
        idx <- gregexpr("/", res)[[1]]
        i <- idx[floor(length(idx)/2)]
        res <- paste0(substring(res, 1, i-1), "\n", substring(res, i+1))
    }
    return(res)
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
        .add_new_line(res)
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

    if (nchar(seqA) != nchar(seqB)) {
        stop("seqA should have equal length to seqB")
    }
    
    if (translate == TRUE) {
        AA <- seqA %>% seq2codon %>% codon2AA
        BB <- seqB %>% seq2codon %>% codon2AA
    } else {
        ## strsplit is faster than substring
        ##
        ## n <- nchar(seqA) ## should equals to nchar(seqB)
        ## AA <- substring(seqA, 1:n, 1:n)
        ## BB <- substring(seqB, 1:n, 1:n)
        AA <- strsplit(seqA, split="") %>% unlist
        BB <- strsplit(seqB, split="") %>% unlist
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
    
    res <- paste(AA[ii], ii, BB[ii], sep="", collapse=" / ")
    return(res)
}

seq2codon <- function(x) {
    substring(x, first=seq(1, nchar(x)-2, 3), last=seq(3, nchar(x), 3))
}

## @importFrom Biostrings GENETIC_CODE
codon2AA <- function(codon) {
    ## a genetic code name vector
    GENETIC_CODE <- get_fun_from_pkg("Biostrings", "GENETIC_CODE") 
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
    tr <- gsub('(:[0-9\\.eE\\+\\-]+)\\{(\\d+)\\}', '\\@\\2\\1', tree.text)
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
        ## root node is not encoded with edge number
        edgeNum.df <- edgeNum.df[!is.na(edgeNum.df[,2]),]
        attr(phylo, "edgeNum") <- edgeNum.df
    }

    ## using :edge_length{edge_num} to match edge_num to node_num
    ## this is not a good idea since there may exists identical edge_length.
    ## but we can use it to verify our method.
    ##
    ## en.matches <- gregexpr(":[0-9\\.eE\\+\\-]+\\{\\d+\\}", tree.text)
    ## matches <- en.matches[[1]]
    ## match.pos <- as.numeric(matches)
    ## match.len <- attr(matches, 'match.length')

    ## edgeLN <- substring(tree.text, match.pos+1, match.pos+match.len-2)
    ## edgeLN.df <- data.frame(length=as.numeric(gsub("\\{.+", "", edgeLN)),
    ##                         edgeNum = as.numeric(gsub(".+\\{", "", edgeLN)))

    ## xx <- merge(edgeLN.df, edgeNum.df, by.x="node", by.y="node")
    
    return(phylo)
}

extract.treeinfo.jplace <- function(object, layout="phylogram", ladderize=TRUE, right=FALSE, ...) {

    tree <- get.tree(object)
    
    df <- fortify.phylo(tree, layout=layout, ladderize=ladderize, right=right, ...)

    edgeNum.df <- attr(tree, "edgeNum")
    if (!is.null(edgeNum.df)) {
        df2 <- merge(df, edgeNum.df, by.x="node", by.y="node", all.x=TRUE) 
        df <- df2[match(df[, "node"], df2[, "node"]),]
    }
    
    attr(df, "ladderize") <- ladderize
    attr(df, "right") <- right
    return(df)
}

## convert edge number to node number for EPA/pplacer output
edgeNum2nodeNum <- function(jp, edgeNum) {
    edges <- attr(jp@phylo, "edgeNum")

    idx <- which(edges$edgeNum == edgeNum)
    if (length(idx) == 0) {
        return(NA)
    }
    
    edges[idx, "node"]
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
                        "codeml_mlc",
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


## from ChIPseeker
##' @importFrom grDevices colorRampPalette
getCols <- function (n) {
    col <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", 
             "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", 
             "#ccebc5", "#ffed6f")
    col2 <- c("#1f78b4", "#ffff33", "#c2a5cf", "#ff7f00", "#810f7c", 
              "#a6cee3", "#006d2c", "#4d4d4d", "#8c510a", "#d73027", 
              "#78c679", "#7f0000", "#41b6c4", "#e7298a", "#54278f")
    col3 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", 
              "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", 
              "#ffff99", "#b15928")
    colorRampPalette(col3)(n)
}


get_fun_from_pkg <- function(pkg, fun) {
    ## requireNamespace(pkg)
    ## eval(parse(text=paste0(pkg, "::", fun)))
    require(pkg, character.only = TRUE)
    eval(parse(text = fun))
}

hist <- get_fun_from_pkg("graphics", "hist")






