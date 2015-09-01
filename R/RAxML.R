##' parse RAxML bootstrapping analysis output
##'
##' 
##' @title read.raxml
##' @param file RAxML bootstrapping analysis output
##' @return raxml object
##' @export
##' @author Guangchuang Yu
read.raxml <- function(file) {
    tree.text <- readLines(file)
    tree_text <- gsub('(:[0-9\\.eE+\\-]+)\\[(\\d+)\\]', '\\@\\2\\1', tree.text)
    phylo <- read.tree(text=tree_text)
    if(any(grepl('@', phylo$node.label))) {
        bootstrap <- as.numeric(gsub("[^@]*@(\\d+)", "\\1", phylo$node.label))
        phylo$node.label %<>% gsub("@\\d+", "", .)
    }

    if (all(phylo$node.label == "")) {
        phylo$node.label <- NULL
    }

    bootstrap <- data.frame(node = Ntip(phylo) + 1:phylo$Nnode,
                            bootstrap = bootstrap)

    bootstrap <- bootstrap[!is.na(bootstrap[,2]), ] ## root node maynot have bootstrap value

    new("raxml",
        fields = "bootstrap",
        treetext = tree.text,
        phylo = phylo,
        bootstrap = bootstrap
        )
}


