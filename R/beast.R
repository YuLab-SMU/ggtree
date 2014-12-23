
get.info.beast <- function(file) {
    beast <- readLines(file)
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
        nr.lower <- paste0(names.range, "_lower")
        nr.upper <- paste0(names.range, "_upper")
        
        lo <- as.numeric(gsub(".*=\\{", "", y[idx]))
        hi <- as.numeric(gsub("\\}", "", y[idx+1]))
        
        
        jj <- -c(idx, idx+1)
        names <- gsub("=.*", "", y[jj])
        val <- as.numeric(gsub(".*=", "", y[jj]))

        nn <- c(nr.lower, nr.upper, names)
        res <- numeric(length(nn))
        names(res) <- nn
        for (i in seq_along(nr.lower)) {
            res[i] <- lo[i]
        }
        j <- i
        for (i in seq_along(nr.upper)) {
            res[i+j] <- hi[i] 
        }
        j <- i
        for (i in seq_along(names)) {
            res[i+j] <- val[i]
        }
        return(res)
    })

    nn <- lapply(stats2, names) %>% unlist %>% unique

    ## stats3 is a matrix
    stats3 <- t(sapply(stats2, function(x) {
        for (ii in nn[!nn %in% names(x)]) {
            x[ii] <- NA
        }
        x[nn]
    }))
    return(stats3)
}


rm.info.beast <- function(beast) {
    LEFT <- grep("\\[", beast)
    RIGHT <- grep("\\]", beast)
    if (length(LEFT) > 0) {
        ii <- (LEFT == RIGHT)
        if (length(ii) > 0) {
            ## if in one line
            idx <- LEFT[ii]
            beast[idx] %<>% gsub("\\[[^]]+\\]", "", .)
        }
        jj <- !ii
        if (length(jj) > 0) {
            ## if in separate line
            lidx <- LEFT[jj]
            beast[lidx] %<>% gsub("\\[.+$", "", .)
            ridx <- RIGHT[jj]
            beast[ridx] %<>% gsub("^.*\\]", "", .)
            ## remove any lines between them when they exist
            for (i in seq_along(lidx)) {
                if (lidx[i] < (ridx[i] - 1)) {
                    beast <- beast[-(lidx[i]+1):(ridx[i]-1)]
                }
            }
        }
    }
    
    return(beast)
}
    
get.trans.beast <- function(beast) {
    i <- grep("TRANSLATE", beast, ignore.case = TRUE)
    end <- grep(";", beast)
    j <- end[end %>% `>`(i) %>% which %>% `[`(1)]
    trans <- beast[(i+1):j]
    trans %<>% gsub("\\t+", "", .)
    trans %<>% gsub(",|;", "", .)
    trans %<>% `[`(nzchar(trans))
    ## remove quote if strings were quoted
    trans %<>% gsub("'|\"", "",.)
    trans %<>% sapply(., strsplit, split="\\s+")
    trans %<>% do.call(rbind, .)
    ## trans is a matrix
    return(trans)
}

read.beast <- function(file) {
    info <- get.info.beast(file)
    beast <- readLines(file)
    beast <- rm.info.beast(beast)



   
    
}
