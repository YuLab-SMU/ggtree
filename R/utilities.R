reverse.treeview <- function(tv) {
    df <- tv$data
    root <- df$node[df$node == df$parent]
    df$x <- getXcoord2(df$x, root, df$parent, df$node,
                       df$length, start=max(df$x), rev=TRUE)
    tv$data <- df
    tv
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
    if (is(x, "phylo")) {
        return(TRUE)
    }
    if (is(x, "phylo4")) {
        return(TRUE)
    }
    if (is(x, "jplace")) {
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


