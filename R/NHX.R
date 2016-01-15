##' read nhx tree file
##'
##'
##' @title read.nhx
##' @param file nhx file
##' @return nhx object
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
read.nhx <- function(file) {
    treetext <- suppressWarnings(readLines(file))
    treetext <- treetext[treetext != ""]
    treetext <- treetext[treetext != " "]

    if (length(treetext) > 1) {
        treetext <- paste0(treetext, collapse = '')
    }
    treetext %<>% gsub(" ", "",. )
    
    phylo <- read.tree(file)
    nnode <- phylo$Nnode + Ntip(phylo)
    nlab <- paste("X", 1:nnode, sep="")
    tree2 <- treetext

    for (i in 1:nnode) {
        tree2 <- sub("(\\w+)?(:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*)?\\[&&NHX.*?\\]", paste0("\\", nlab[i], "\\2"), tree2)
    }

    phylo2 <- read.tree(text = tree2)
    treeinfo <- fortify(phylo2)
    node <- as.character(treeinfo$node[match(nlab, treeinfo$label)])

    nhx.matches <- gregexpr("(\\w+)?(:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*)?\\[&&NHX.*?\\]", treetext)
    matches <- nhx.matches[[1]]
    match.pos <- as.numeric(matches)
    match.len <- attr(matches, 'match.length')
    
    nhx_str <- substring(treetext, match.pos, match.pos+match.len-1)

    ## nhx_features <- gsub("^(\\w+)?:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*", "", nhx_str) %>%
    nhx_features <- gsub("^[^\\[]*", "", nhx_str) %>%
        gsub("\\[&&NHX:", "", .) %>%
            gsub("\\]", "", .)
    
    nhx_stats <- get_nhx_feature(nhx_features)
    fields <- names(nhx_stats)
    for (i in ncol(nhx_stats)) {
        if(any(grepl("\\D+", nhx_stats[,i])) == FALSE) {
            ## should be numerical varialbe
            nhx_stats[,i] <- as.numeric(nhx_stats[,i])
        }
    }
    nhx_stats$node <- node
    
    new("nhx",
        file = file,
        fields = fields,
        phylo = phylo,
        nhx_tags = nhx_stats
        )
}


get_nhx_feature <- function(nhx_features) {
    nameSET <- strsplit(nhx_features, split=":") %>% unlist %>%
        gsub("=.*", "", .) %>% unique
    lapply(nhx_features, get_nhx_feature_internal, nameSET=nameSET) %>%
        do.call(rbind, .) %>% as.data.frame(., stringsAsFactors = FALSE)
}

get_nhx_feature_internal <- function(feature, nameSET) {
    x <- strsplit(feature, ":") %>% unlist
    name <- gsub("=.*", "", x)
    val <- gsub(".*=", "", x)

    names(val) <- name
    y <- character(length(nameSET))
    for (i in seq_along(nameSET)) {
        if (nameSET[i] %in% name) {
            y[i] <- val[nameSET[i]]
        } else {
            y[i] <- NA
        }
    }
    names(y) <- nameSET
    return(y)
}





##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="nhx"),
          function(object, ...) {
              get.fields.tree(object)
          }
          )
