filename <- function(file) {
    ## textConnection(text_string) will work just like a file
    ## in this case, just set the filename as ""
    file_name <- ""
    if (is.character(file)) {
        file_name <- file
    }
    return(file_name)
}

##' read nhx tree file
##'
##'
##' @title read.nhx
##' @param file nhx file
##' @return nhx object
##' @export
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
read.nhx <- function(file) {
    treetext <- suppressWarnings(readLines(file))
    treetext <- treetext[treetext != ""]
    treetext <- treetext[treetext != " "]

    if (length(treetext) > 1) {
        treetext <- paste0(treetext, collapse = '')
    }
    treetext %<>% gsub(" ", "",. )

    phylo <- read.tree(text=treetext)
    nnode <- phylo$Nnode + Ntip(phylo)
    nlab <- paste("X", 1:nnode, sep="")
    tree2 <- treetext

    for (i in 1:nnode) {
        tree2 <- sub("(\\w+)?(:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*)?\\[&&NHX.*?\\]", paste0("\\", nlab[i], "\\2"), tree2)
    }

    phylo2 <- read.tree(text = tree2)
    treeinfo <- fortify(phylo2)
    node <- treeinfo$node[match(nlab, sub(".+(X\\d+)$","\\1",treeinfo$label))] # as.character

    nhx.matches <- gregexpr("(\\w+)?(:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*)?\\[&&NHX.*?\\]", treetext)
    matches <- nhx.matches[[1]]
    match.pos <- as.numeric(matches)
    if (length(match.pos) == 1 && (match.pos == -1)) {
        nhx_tags <- data.frame(node = as.numeric(treeinfo$node))
    } else {
        match.len <- attr(matches, 'match.length')

        nhx_str <- substring(treetext, match.pos, match.pos+match.len-1)

        ## nhx_features <- gsub("^(\\w+)?:?\\d*\\.?\\d*[Ee]?[\\+\\-]?\\d*", "", nhx_str) %>%
        nhx_features <- gsub("^[^\\[]*", "", nhx_str) %>%
            gsub("\\[&&NHX:", "", .) %>%
            gsub("\\]", "", .)

        nhx_tags <- get_nhx_feature(nhx_features)
        fields <- names(nhx_tags)
        for (i in ncol(nhx_tags)) {
            if(any(grepl("\\D+", nhx_tags[,i])) == FALSE) {
                ## should be numerical varialbe
                nhx_tags[,i] <- as.numeric(nhx_tags[,i])
            }
        }
        nhx_tags$node <- as.numeric(node)
    }

    # Order rows by row number to facilitate downstream manipulations
    nhx_tags=nhx_tags[order(nhx_tags$node),]

    new("nhx",
        file = filename(file),
        fields = fields,
        phylo = phylo,
        nhx_tags = nhx_tags
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


Ntip <- function(tree) {
    phylo <- get.tree(tree)
    length(phylo$tip.label)
}

Nnode <- function(tree, internal.only=TRUE) {
    phylo <- get.tree(tree)
    if (internal.only)
        return(phylo$Nnode)

    Ntip(phylo) + phylo$Nnode
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

##' @importFrom methods .hasSlot is missingArg new slot slot<-
has.slot <- function(object, slotName) {
    if (!isS4(object)) {
        return(FALSE)
    }
    .hasSlot(object, slotName)
    ## slot <- tryCatch(slot(object, slotName), error=function(e) NULL)
    ## ! is.null(slot)
}

