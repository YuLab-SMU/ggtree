##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="apeBootstrap"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="beast"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="codeml"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="codeml_mlc"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="gg"),
          function(object, focus, group_name) {
              groupOTU.ggplot(object, focus, group_name)
          })

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="ggplot"),
          function(object, focus, group_name="group") {
              groupOTU.ggplot(object, focus, group_name)
          })


##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="jplace"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="nhx"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phangorn"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phylip"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="paml_rst"),
          function(object, focus, group_name="group") {
              groupOTU_(object, focus, group_name)
          }
          )


##' group tree based on selected OTU, will traceback to MRCA
##'
##'
##' @rdname groupOTU-methods
##' @exportMethod groupOTU
setMethod("groupOTU", signature(object="phylo"),
          function(object, focus, group_name="group") {
              groupOTU.phylo(object, focus, group_name)
          })

##' @rdname groupOTU-methods
##' @exportMethod groupOTU
##' @param tree which tree selected
setMethod("groupOTU", signature(object="r8s"),
          function(object, focus, group_name="group", tree="TREE") {
              groupOTU_(get.tree(object)[[tree]], focus, group_name)
          }
          )




##' @importFrom ape which.edge
gfocus <- function(phy, focus, group_name, focus_label=NULL) {
    if (is.character(focus)) {
        focus <- which(phy$tip.label %in% focus)
    }

    n <- getNodeNum(phy)
    if (is.null(attr(phy, group_name))) {
        foc <- rep(0, n)
    } else {
        foc <- attr(phy, group_name)
    }
    i <- max(suppressWarnings(as.numeric(foc)), na.rm=TRUE) + 1
    ## sn <- phy$edge[which.edge(phy, focus),] %>% as.vector %>% unique
    sn <- unique(as.vector(phy$edge[which.edge(phy, focus),]))
    if (is.null(focus_label)) {
        foc[sn] <- i
    } else {
        foc[sn] <- focus_label
    }

    attr(phy, group_name) <- foc
    phy
}


##' group OTU
##'
##'
##' @title groupOTU.phylo
##' @param phy tree object
##' @param focus tip list
##' @param group_name name of the group
##' @return phylo object
##' @author ygc
groupOTU.phylo <- function(phy, focus, group_name="group") {
    attr(phy, group_name) <- NULL
    if ( is(focus, "list") ) {
        for (i in 1:length(focus)) {
            phy <- gfocus(phy, focus[[i]], group_name, names(focus)[i])
        }
    } else {
        phy <- gfocus(phy, focus, group_name)
    }
    attr(phy, group_name) <- factor(attr(phy, group_name))
    return(phy)
}

groupOTU_ <- function(object, focus, group_name) {
    if (is(object, "phylo")) {
        object <- groupOTU.phylo(object, focus, group_name)
    } else {
        object@phylo <- groupOTU.phylo(get.tree(object), focus, group_name)
    }
    return(object)
}


groupOTU.ggplot <- function(object, focus, group_name) {
    df <- object$data
    df[, group_name] <- 0
    object$data <- groupOTU.df(df, focus, group_name)
    return(object)
}


groupOTU.df <- function(df, focus, group_name) {
    if (is(focus, "list")) {
        for (i in 1:length(focus)) {
            df <- gfocus.df(df, focus[[i]], group_name, names(focus)[i])
        }
    } else {
        df <- gfocus.df(df, focus, group_name)
    }
    df[, group_name] <- factor(df[, group_name])
    return(df)
}

gfocus.df <- function(df, focus, group_name, focus_label=NULL) {
    focus <- df$node[which(df$label %in% focus)]
    if (is.null(focus_label))
        focus_label <- max(suppressWarnings(as.numeric(df[, group_name])), na.rm=TRUE) + 1

    if (length(focus) == 1) {
        df[match(focus, df$node), group_name] <-focus_label
        return(df)
    }

    anc <- getAncestor.df(df, focus[1])
    foc <- c(focus[1], anc)
    for (j in 2:length(focus)) {
        anc2 <- getAncestor.df(df, focus[j])
        comAnc <- intersect(anc, anc2)
        foc <- c(foc, focus[j], anc2)
        foc <- foc[! foc %in% comAnc]
        foc <- c(foc, comAnc[1])
    }
    idx <- match(foc, df$node)
    df[idx, group_name] <- focus_label
    return(df)
}

