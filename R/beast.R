
get.BEASTstats <- function(file) {
    beast <- scan(file = file, what = "", sep = "\n", quiet = TRUE)
    tree <- beast[grep("tree TREE1\\s+=", beast)]
    tree <- sub("tree TREE1\\s+= \\[&R\\] ", "", tree) 
    stats <- unlist(strsplit(tree, "\\["))[-1]
    stats <- sub(":.+$", "", stats)
    stats <- sub("^&", "", stats)
    stats <- sub("];*$", "", stats)
        
    stats2 <- lapply(stats, function(x) {
        y <- unlist(strsplit(x, ","))
        idx <- grep("=\\{", y)
        names.range <- gsub("=\\{.*", "", y[idx]) 
        lo <- as.numeric(gsub(".*=\\{", "", y[idx]))
        hi <- as.numeric(gsub("\\}", "", y[idx+1]))

        jj <- -c(idx, idx+1)
        names <- gsub("=.*", "", y[jj])
        val <- as.numeric(gsub(".*=", "", y[jj]))

        nn <- c(names.range, names)
        res <- numeric(length(nn))
        names(res) <- nn
        for (i in seq_along(names.range)) {
            res[i] <- list(c(lo[i], hi[i]))
        }
        j <- length(names.range)
        for (i in seq_along(names)) {
            res[i+j] <- val[i]
        }
        return(res)
    })

    nn <- lapply(stats2, names) %>% unlist %>% unique

    ## stats3 is a matrix with entry can be numeric of list of numeric.
    stats3 <- t(sapply(stats2, function(x) {
        for (ii in nn[!nn %in% names(x)]) {
            x[ii] <- NA
        }
        x[nn]
    }))
    return(stats3)
}

read.beast <- function(file) {
    


}
