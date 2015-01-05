
merge_baseml_hyphy <- function(baseml, hyphy) {
    ml_phylo <- baseml@mlc@phylo
    hy_phylo <- hyphy@phylo
    phylo_merge_check(ml_phylo, hy_phylo)
}


phylo_merge_check <- function(phy1, phy2) {
    if (Ntip(phy1) != Ntip(phy2)) {
        stop("Number of tip is not eqaul...")
    }
    if ( ! all(phy1$tip.label %in% phy2$tip.label)) {
        stop("tip label is not consistent...")
    }

    if (phy1$Nnode != phy2$Nnode) {
        stop("Number of node is not eqaul...")
    }
    if (!is.null(phy1$node.label) && !is.null(phy2$node.label)) {
        n <- getNodeNum(phy1)
        node <- (Ntip(phy1)+1):n
        if (all(phy1$node.label == node) || all(phy2$node.label == node)) {
            ## nothing happend.
        } else if (any(phy1$node.label != phy2$node.label)){
            stop("node label is not consistent...")
        }
    }
    
    degree1 <- table(phy1$edge[,1]) %>% sort
    degree2 <- table(phy2$edge[,1]) %>% sort
    if (length(degree1) != length(degree2)) {
        stop("node degree is not consistent...")
    } else if (!all(degree1 == degree2)) {
        stop("node degree is not consistent...")
    }
}


node_mapper <- function(phy1, phy2) {
   df1 <- fortify(phy1)
   df2 <- fortify(phy2)

   df1$node2 <- NA
   df1$visited <- FALSE
   ntip <- Ntip(phy1)
   df1$node2[1:ntip] <- match(phy1$tip.label, phy2$tip.label)
 
   root <- getRoot(phy1)
   currentNode <- 1:ntip
   while(any(df1$visited == FALSE)) {
       pNode <- c()
       for (i in currentNode) {
           df1[i, "visited"] <- TRUE
           j <- df1[i, "node2"]
           ip <- df1[i, "parent"]
           if (df1[ip, "visited"] == FALSE) {
               jp <- df2[j, "parent"]
               df1[ip, "node2"] <- jp
               df1[ip, "visited"] <- TRUE
           }
           pNode <- c(pNode, ip)
       }
       currentNode <- unique(pNode)
   }

   if (df1[root, "node"] != df1[root, "node2"]) {
       stop("phylogenies not compatible...")
   }
   
   
   node <- df1$node
   names(node) <- df1$node2
   phy2$edge <- cbind(node[as.character(phy2$edge[,1])],
                      node[as.character(phy2$edge[,2])])
   phy2$tip.label <- phy1$tip.label

   reorder(phy2)
}
