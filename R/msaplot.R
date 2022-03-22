##' visualize phylogenetic tree with multiple sequence alignment
##'
##'
##' @title msaplot
##' @param p tree view
##' @param fasta fasta file that contains multiple sequence alignment information
##' @param offset set the offset of MSA to tree
##' @param width total width of alignment, compare to width of tree, defaults to 1,
##' which means they are of the same length
##' @param color set color of the tree
##' @param window specific a slice of alignment to display
##' @param bg_line whether to add background line in alignment, defaults to "TRUE"
##' @param height height ratio of sequence, defaults to 0.8
##' @return tree view
##' @export
##' @importFrom colorspace rainbow_hcl
##' @importFrom treeio read.fasta
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 geom_rect
##' @importFrom ggplot2 scale_fill_manual
##' @author Guangchuang Yu
##' @references
##' For demonstration of this function, please refer to chapter 7.4 of 
##' *Data Integration, Manipulation and Visualization of Phylogenetic Trees*
##' <http://yulab-smu.top/treedata-book/index.html> by Guangchuang Yu.
msaplot <- function(p, fasta, offset=0, width=1, color=NULL, window=NULL, bg_line = TRUE, height = 0.8){
    if (missingArg(fasta)) {
        x <- NULL
    } else if (is(fasta, "DNAbin") || is(fasta, "AAbin") ) {
        x <- fasta
    } else if (is(fasta, "character")) {
        x <- treeio::read.fasta(fasta)
    } else {
        x <- NULL
    }


    if (is.null(x) && is(p, "treedata") && length(p@tip_seq)) {
        x <- p@tip_seq
        p <- ggtree(p) + geom_tiplab()
    }

    if (is.null(x)) {
        stop("multiple sequence alignment is not available...\n-> check the parameter 'fasta'...")
    }

    x <- as.matrix(x)

    if (!all(labels(x) %in% p$data$label)) {
        stop("taxa name in input sequences are not match with the ones on the tree, please check your input files...")
    }

    if (is.null(window)) {
        window <- c(1, ncol(x))
    }

    slice <- seq(window[1], window[2], by=1)
    x <- x[, slice]

    seqs <- lapply(1:nrow(x), function(i) {
        seq <- as.vector(as.character(x[i,]))
        seq[seq == '?'] <- '-'
        seq[seq == '*'] <- '-'
        seq[seq == ' '] <- '-'
        return(seq)
    })

    names(seqs) <- labels(x)

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
    ymin <- y - height/2 *h
    ymax <- y + height/2 *h

    from <- to <- NULL

    lines.df <- data.frame(from=min(xmin), to=max(xmax), y = y)

    if (bg_line) {
        p <- p + geom_segment(data=lines.df, aes(x=from, xend=to, y=y, yend=y), 
                              size=h*.2, inherit.aes = FALSE)
    }

    msa <- lapply(1:length(y), function(i) {
        data.frame(name=names(seqs)[i],
                   xmin=xmin,
                   xmax=xmax,
                   ymin=ymin[i],
                   ymax=ymax[i],
                   seq=seqs[[i]])
    })

    msa.df <- do.call("rbind", msa)

    p <- p + geom_rect(aes(xmin=xmin, xmax=xmax,
                           ymin=ymin, ymax=ymax,
                           fill=seq),
                       data=msa.df, inherit.aes = FALSE) +
                               scale_fill_manual(values=color, na.value = 'white')

    breaks <- graphics::hist(seq_along(slice), breaks=10, plot=FALSE)$breaks
    pos <- start + breaks * width
    mapping <- data.frame(from=breaks+1, to=pos)
    attr(p, "mapping") <- mapping

    return(p)
}
