
##' read beast output
##'
##' 
##' @title read.beast
##' @param file beast file
##' @return \code{beast} object
##' @importFrom ape read.nexus
##' @export
##' @author Guangchuang Yu \url{http://ygc.name}
##' @examples
##' file <- system.file("extdata/BEAST", "beast_mcc.tree", package="ggtree")
##' read.beast(file)
read.beast <- function(file) {
    stats <- read.stats_beast(file)
    fields <- sub("_lower|_upper", "", names(stats)) %>% unique
    fields %<>% `[`(.!="node")
    
    new("beast",
        fields      = fields,
        treetext    = read.treetext_beast(file),
        phylo       = read.nexus(file),
        translation = read.trans_beast(file),
        stats       = stats,
        file        = file
        )
}

##' @rdname plot-methods
##' @exportMethod plot
##' @param tip.label.size size of tip label
##' @param tip.label.hjust hjust of tip.label
##' @param annotation.size size of annotation
##' @param annotation.color color of annotation
setMethod("plot", signature( x= "beast"),
          function(x, layout = "phylogram",
                   branch.length = "branch.length",
                   show.tip.label = TRUE,
                   tip.label.size = 4,
                   tip.label.hjust = -0.1,
                   position = "branch",
                   annotation = "rate",
                   ndigits = 2,
                   annotation.size = 3,
                   annotation.color = "black",
                   ...) {

              p <- ggtree(x, layout     = layout,
                          branch.length = branch.length,
                          ndigits       = ndigits, ...)

              if (show.tip.label) {
                  p <- p + geom_tiplab(hjust=tip.label.hjust,
                                       size=tip.label.size)
                  offset <- ceiling(max(p$data$x)) * 0.1
                  p <- p + xlim(-offset, max(p$data$x) + offset)
              }
              if (!is.null(annotation) && !is.na(annotation)) {
                  fields <- colnames(x@stats)
                  fields <- gsub("_lower", "", fields)
                  fields <- gsub("_upper", "", fields)
                  
                  m <- grep(paste0("^", annotation, "$"),
                            fields)
                  
                  if (length(m) == 0 || length(m) > 2) {
                      stop("annotation should be one of ",
                           paste(get.fields(x), collapse=", "),
                           ".")
                  }
                  if (length(m) == 1) {
                      p <- p + geom_text(aes_string(x=position,
                                                    label=annotation),
                                         size=annotation.size, vjust=-.5,
                                         color=annotation.color)
                  } else {
                      lo <- paste0(annotation, "_lower")
                      hi <- paste0(annotation, "_upper")
                      df <- p$data
                      lo <- df[,lo]
                      hi <- df[, hi]
                      range <- paste0("[", lo, ", ", hi, "]")
                      range[is.na(lo)] <- NA
                      df[, annotation] <- range
                      p$data <- df
                      p <- p+geom_text(aes_string(x = position,
                                                  label = annotation),
                                       size=annotation.size, vjust=-.5,
                                       color=annotation.color)
                  }
              }
              p + theme_tree2()
          })

##' @rdname show-methods
##' @importFrom ape print.phylo
##' @exportMethod show
setMethod("show", signature(object = "beast"),
          function(object) {
              cat("'beast' S4 object that stored information of\n\t",
                  paste0("'", object@file, "'.\n\n"))
              cat("...@ tree: ")
              print.phylo(get.tree(object))                  
              cat("\nwith the following features available:\n")
              fields <- get.fields(object)
              n <- length(fields)
              i <- floor(n/5)
              for (j in 0:i) {
                  ii <- 1:5 + 5 * j
                  if (j == i) {
                      ii <- ii[1:(n %% 5)]
                  }
                  cat("\t", paste0("'",
                                   paste(fields[ii], collapse="',\t'"),
                                   "'")
                      )
                  if ( j == i) {
                      cat(".\n")
                  } else {
                      cat(",\n")
                  }
              }
              
          })


##' get.tree method
##'
##'
##' @docType methods
##' @name get.tree
##' @rdname get.tree-methods
##' @aliases get.tree,beast
##' @exportMethod get.tree
##' @author Guangchuang Yu \url{http://ygc.name}
##' @usage get.tree(object, ...)
setMethod("get.tree", signature(object="beast"),
          function(object,...) {
              object@phylo
          }
          )


##' @rdname get.fields-methods
##' @exportMethod get.fields
setMethod("get.fields", signature(object="beast"),
          function(object, ...) {
              object@fields
          }
          )


read.treetext_beast <- function(file) {
    beast <- readLines(file)
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

read.trans_beast <- function(file) {
    beast <- readLines(file)
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

read.stats_beast <- function(file) {
    beast <- readLines(file)
    tree <- read.treetext_beast(file)

    tree2 <- gsub("\\[[^\\[]*\\]", "", tree)
    phylo <- read.tree(text = tree2)
    if(is.null(phylo$node.label)) {
        nnode <- phylo$Nnode
        nlab <- paste("X", 1:nnode, sep="")
        for (i in 1:nnode) {
            tree2 <- sub("\\)([:;])", paste0("\\)", nlab[i], "\\1"), tree2)
        }
    }

    ## node name corresponding to stats
    nn <- strsplit(tree2, split=",") %>% unlist %>%
        strsplit(., split="\\)") %>% unlist %>%
            gsub("\\(*", "", .) %>%
                gsub("[:;].*", "", .)
    
    phylo <- read.tree(text = tree2)
    root <- getRoot(phylo)
    nnode <- phylo$Nnode
    
    ## phylo2 <- read.nexus(file)
    ## treeinfo <- fortify.phylo(phylo)
    ## treeinfo2 <- fortify.phylo(phylo2)
    ## treeinfo$label2 <- NA
    ## treeinfo$label2[treeinfo$isTip] <- treeinfo2$node[as.numeric(treeinfo$label[treeinfo$isTip])]
    ## treeinfo$visited <- FALSE
    ## root <- getRoot(phylo2)
    ## treeinfo[root, "visited"] <- TRUE
    ## currentNode <- 1:Ntip(phylo2)
    ## while(any(treeinfo$visited == FALSE)) {
    ##     pNode <- c()
    ##     for (kk in currentNode) {
    ##         i <- which(treeinfo$label2 == kk)
    ##         treeinfo[i, "visited"] <- TRUE
    ##         j <- which(treeinfo2$node == kk)
    ##         ip <- treeinfo$parent[i]
    ##         if (ip != root) {
    ##             ii <- which(treeinfo$node == ip)
    ##             if (treeinfo$visited[ii] == FALSE) {
    ##                 jp <- treeinfo2$parent[j]
    ##                 jj <- which(treeinfo2$node == jp)
    ##                 treeinfo[ii, "label2"] <- treeinfo2[jj, "node"]
    ##                 pNode <- c(pNode, jp)
    ##             }
    ##             treeinfo[ii, "visited"] <- TRUE
    ##         }
    ##     }
    ##     currentNode <- unique(pNode)
    ## }
    ## treeinfo[root, "label2"] <- root
    ## ## convert nn to node that encoded in phylo2
    ## node <- treeinfo$label2[match(nn, treeinfo$label)]

    
    ####################################################
    ##                                                ##
    ##  after doing it in the hard way                ##
    ##  I finally figure out the following easy way   ##
    ##                                                ##
    ####################################################
    treeinfo <- fortify.phylo(phylo)
    label2 <- c(treeinfo[treeinfo$isTip, "label"],
                root:(root+nnode-1))
    node <- label2[match(nn, treeinfo$label)]
    

    
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

    stats3 <- as.data.frame(stats3)
    stats3$node <- node
    colnames(stats3) <- make.names(colnames(stats3))
    return(stats3)
}

