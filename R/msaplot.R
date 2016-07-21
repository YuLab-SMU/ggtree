##' multiple sequence alignment with phylogenetic tree
##'
##' 
##' @title msaplot
##' @param p tree view
##' @param fasta fasta file, multiple sequence alignment
##' @param offset offset of MSA to tree
##' @param width total width of alignment, compare to width of tree
##' @param color color 
##' @param window specific a slice to display
##' @return tree view
##' @export
## @importFrom Biostrings readBStringSet
## @importMethodsFrom Biostrings width
## @importFrom colorspace rainbow_hcl
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 geom_rect
##' @importFrom ggplot2 scale_fill_manual
##' @author Guangchuang Yu
msaplot <- function(p, fasta, offset=0, width=1, color=NULL, window=NULL){
    if (missingArg(fasta)) {
        aln <- NULL
    } else if (is(fasta, "BStringSet")) {
        aln <- fasta
    } else if (is(fasta, "character")) {
        readBStringSet <- get_fun_from_pkg("Biostrings", "readBStringSet")
        aln <- readBStringSet(fasta)
    } else {
        aln <- NULL
    }
        
    if (is(p, "phylip")) {
        BStringSet <- get_fun_from_pkg("Biostrings", "BStringSet")
        aln <- BStringSet(p@sequence)
        p <- ggtree(p) + geom_tiplab()
    }

    if (is.null(aln)) {
        stop("multiple sequence alignment is not available...\n-> check the parameter 'fasta'...")
    }

    width_fun <- get_fun_from_pkg("Biostrings", "width")
    
    if (is.null(window)) {
        window <- c(1, width_fun(aln)[1])
    }
    slice <- seq(window[1], window[2], by=1)
    
    seqs <- lapply(1:length(aln), function(i) {
        x <- toString(aln[i])
        seq <- substring(x, slice, slice)

        seq[seq == '?'] <- '-'
        seq[seq == '*'] <- '-'
        seq[seq == ' '] <- '-'
        return(seq)
    })
    names(seqs) <- names(aln)
    
    if(is.null(color)) {
        alphabet <- unlist(seqs) %>% unique
        alphabet <- alphabet[alphabet != '-']
        ## color <- rainbow_hcl(length(alphabet))
        color <- getCols(length(alphabet))
        names(color) <- alphabet
        color <- c(color, '-'=NA)
    }

    df <- p$data
    ## if (is.null(width)) {
    ##     width <- (df$x %>% range %>% diff)/500
    ## }

    ## convert width to width of each cell
    width <- width * (df$x %>% range %>% diff) / diff(window)
    
    df=df[df$isTip,]
    start <- max(df$x) * 1.02 + offset

    seqs <- seqs[df$label[order(df$y)]]
    ## seqs.df <- do.call("rbind", seqs)

    h <- ceiling(diff(range(df$y))/length(df$y))
    xmax <- start + seq_along(slice) * width
    xmin <- xmax - width
    y <- sort(df$y)
    ymin <- y - 0.4 *h
    ymax <- y + 0.4 *h

    from <- to <- NULL
    
    lines.df <- data.frame(from=min(xmin), to=max(xmax), y = y)

    p <- p + geom_segment(data=lines.df, aes(x=from, xend=to, y=y, yend=y))
    msa <- lapply(1:length(y), function(i) {
        data.frame(name=names(seqs)[i],
                   xmin=xmin,
                   xmax=xmax,
                   ymin=ymin[i],
                   ymax=ymax[i],
                   seq=seqs[[i]])
    })

    msa.df <- do.call("rbind", msa)

    p <- p + geom_rect(data=msa.df, aes(x=xmin, y=ymin, 
                           xmin=xmin, xmax=xmax,
                           ymin=ymin, ymax=ymax, fill=seq)) +
                               scale_fill_manual(values=color)

    breaks <- hist(seq_along(slice), breaks=10, plot=FALSE)$breaks
    pos <- start + breaks * width
    mapping <- data.frame(from=breaks+1, to=pos)
    attr(p, "mapping") <- mapping
    
    return(p)
}

