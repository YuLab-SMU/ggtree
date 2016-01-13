`%add%` <- function(p, data) {
    p$data <- p$data %add2% data
    return(p)
}

`%add2%` <- function(d1, d2) {
    if ("node" %in% colnames(d2)) {
        cn <- colnames(d2)
        ii <- which(cn %in% c("node", cn[!cn %in% colnames(d1)]))
        d2 <- d2[, ii]
        dd <- merge(d1, d2, by.x="node", by.y="node", all.x=TRUE)
    } else {
        d2[,1] <- as.character(d2[,1])
        dd <- merge(d1, d2, by.x="label", by.y=1, all.x=TRUE)
    }
    dd <- dd[match(d1$node, dd$node),]
    return(dd)
}

