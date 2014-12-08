extract.treeinfo <- function(tree.text, ladderize=TRUE, right=FALSE) {
    ## move edge label to node label separate by @
    tr <- gsub('(:[0-9.e-]+)\\{(\\d+)\\}', '\\@\\2\\1', tree.text)
    tree <- read.tree(text=tr)
    df <- fortify.phylo(tree, ladderize=ladderize, right=right)

    root.idx <- which(df$parent == df$node)
    root.lab <- df[,"label"]
    df$label[root.idx] <- gsub("(.*)\\{(\\d+)\\}", "\\1@\\2", df$label[root.idx])
    
    df$edge <- as.numeric(gsub("[^@]*@(\\d*)", "\\1",df$label))
    ## remove edge label from node label
    df$label <- gsub("@\\d*", "", df$label)
    df$label[df$label == ""] <- NA
    attr(df, "ladderize") <- ladderize
    attr(df, "right") <- right
    return(df)
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
. <- function (..., .env = parent.frame()) {
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}


