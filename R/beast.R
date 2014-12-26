
##' read beast output
##'
##' 
##' @title read.beast
##' @param file beast file
##' @return \code{beast} object
##' @importFrom ape read.nexus
##' @export
##' @author Yu Guangchuang
##' @examples
##' file <- system.file("extdata", "beast_mcc.tree", package="ggtree")
##' read.beast(file)
read.beast <- function(file) {
    beast <- readLines(file)
    stats <- get.stats.beast(file)

    nex <- read.nexus(file)
    df <- fortify(nex)
    attr(df, "ladderize") <- TRUE
    attr(df, "right") <- FALSE

    class(nex) <- "list"
    new("beast",
        fields = sub("_lower|_upper", "", names(stats)) %>% unique,
        treetext = get.treetext.beast(beast),
        tree = nex,
        translation = get.trans.beast(beast),
        stats = stats,
        treeinfo = df,
        file = file
        )
}


##' Class "beast"
##' This class stores information of beast output
##'
##'
##' @name beast-class
##' @aliases beast-class
##'      set.treeinfo<-,beast-method
##'      set.treeinfo,beast-method
##'
##' @docType class
##' @slot fields beast statistic variables
##' @slot treetext tree text in beast file
##' @slot tree tree phylo object
##' @slot translation tip number to name translation in beast file
##' @slot stats beast statistics
##' @slot treeinfo tree information for plotting
##' @slot file beast file, nexus format
##' @exportClass beast
##' @author Guangchuang Yu \url{http://ygc.name}
##' @seealso \code{\link{show}} \code{\link{get.fields}}
##'           \code{\link{ggtree}}
##' @keywords classes
setClass("beast",
         representation  = representation(
             fields      = "character",
             treetext    = "character",
             tree        = "list", ## phylo
             translation = "matrix",
             stats       = "data.frame",
             treeinfo    = "data.frame",
             file        = "character"
             )
         )

##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "beast"),
          function(object) {
              cat("beast class\n...@ tree     : ")
              phylo <- get.tree(object)
              print.phylo(phylo)
              cat("...@ stats variables    :\n")
              for (x in object@fields) {
                  cat("\t", x, "\n")
              }
          }
          )


##' @method fortify beast
##' @export
fortify.beast <- function(model, data, layout="phylogram", ladderize=TRUE, right=FALSE,...) {
    df <- get.treeinfo(model, layout, ladderize, right, ...)
    stats <- model@stats

    idx <- match(df$length, stats$length)
    stats <- stats[idx,]
    stats <- stats[,colnames(stats) != "length"]
    
    return(cbind(df, stats))
}

##' @rdname get.tree-methods
##' @exportMethod get.tree
setMethod("get.tree", signature(object="beast"),
          function(object,...) {
              tree <- object@tree
              class(tree) <- "phylo"
              return(tree)
          }
          )

##' @rdname get.treeinfo-methods
##' @exportMethod get.treeinfo
setMethod("get.treeinfo", signature(object="beast"),
          function(object, ...) {
              get.treeinfo.beast(object, ...)
          }
          )

setReplaceMethod(f="set.treeinfo",
                 signature = "beast",
                 definition = function(x, value) {
                     x@treeinfo <- value
                 })

##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="beast"),
          function(object, ...) {
              object@fields
          }
          )

get.stats.beast <- function(file) {
    beast <- readLines(file)
    tree <- get.treetext.beast(beast)

    len <- unlist(strsplit(tree, ":"))[-1]
    len <- sub("([0-9.]+)\\D+.*", '\\1', len) %>% as.numeric

    
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
        j <- i+j
        for (i in seq_along(names)) {
            res[i+j] <- val[i]
        }
        return(res)
    })

    nn <- lapply(stats2, names) %>% unlist %>%
        unique %>% sort

    ## stats3 is a matrix
    stats3 <- t(sapply(stats2, function(x) {
        for (ii in nn[!nn %in% names(x)]) {
            x[ii] <- NA
        }
        x[nn]
    }))

    if ( nrow(stats3) == (length(len) + 1) ) {
        len <- c(len, NA) ## root node has no length
    } else {
        stop("missing length exists...")
    }

    stats3 <- as.data.frame(stats3)
    stats3$length <- len
    return(stats3)
}


rm.stats.beast <- function(beast) {
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

get.treetext.beast <- function(beast) {
    ii <- grep("tree TREE1\\s+=", beast)
    jj <- grep("End;", beast)
    jj <- jj[jj > ii][1]
    tree <- beast[ii:(jj-1)]
    if (length(tree) > 1) {
        tree <- paste0(tree)
    }
    tree %<>% sub("tree TREE1\\s+=\\s+\\[&R\\]\\s+", "", .)
    return(tree)
}


get.treeinfo.beast <- function(object, layout="phylogram",
                               ladderize=TRUE, right=FALSE, ...) {
    treeinfo <- object@treeinfo
    if (attr(treeinfo, "ladderize") != ladderize ||
        attr(treeinfo, "right")     != right) {
        tree <- get.tree(object)
        treeinfo <- fortify(object, layout=layout,
                            ladderize=ladderize, right=right, ...)
        attr(treeinfo, "ladderize") <- ladderize
        attr(treeinfo, "right")     <- right
        set.treeinfo(object) <- treeinfo
    }

    return(treeinfo)
}
