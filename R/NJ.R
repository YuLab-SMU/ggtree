##' neighbor-joining method
##'
##' 
##' @title NJ
##' @param X distance matrix
##' @return phylo object
##' @author ygc
##' @examples
##' \dontrun{
##' X <- matrix(c(0,5,4,7,6,8,
##'		5,0,7,10,9,11,
##'		4,7,0,7,6,8,
##'		7,10,7,0,5,9,
##'		6,9,6,5,0,8,
##'		8,11,8,9,8,0), ncol=6)
##' rownames(X) <- colnames(X) <- LETTERS[1:6]
##' tree <- NJ(X)
##' print(tree)
##' }
NJ <- function(X) {
    labels <- colnames(X)
    N <- ncol(X)
    otu_labs <- 1:N
    
    dm <- as.matrix(X)
    S <- colSums(dm)

    ## edge list of node 1 and node 2 
    edge1 <- edge2 <- numeric(2*N-3)
    edge_length <- numeric(2*N-3)
    k <- 1
    cur_node <- 2*N-2
    while (N > 3) {
        ds <- 1e50
        for (i in 1:(N-1)) {
            for (j in (i+1):N) {
                A <- N * dm[i,j] - S[i] - S[j]
                if (A < ds) {
                    OUT1 <- i;
                    OUT2 <- j;
                    ds <- A
                }
            }
        }
        edge2[k] <- otu_labs[OUT1]
        edge2[k+1] <- otu_labs[OUT2]
        edge1[k] <- edge1[k+1] <- cur_node
        dij <- dm[OUT1, OUT2]
        B <- (S[OUT1]-S[OUT2]) / (N-2)
        edge_length[k] <- (dij + B)/2
        edge_length[k+1] <- (dij - B)/2
        
        ij <- 1
        new_dist <- numeric(N-2)
        ## d_kn <- 1/2 * (d_ik + d_jk - d_ij)
        for (i in 1:N) {
            if (i == OUT1 || i == OUT2) next
            x <- dm[i, OUT1]
            y <- dm[i, OUT2]
            new_dist[ij] <- 1/2 * (x+y-dij)
            ij <- ij + 1
        }
        ## update data
        dm <- dm[-c(OUT1, OUT2), -c(OUT1, OUT2)]
        dm <- rbind(dm, new_dist)
        dm <- cbind(dm, c(new_dist, 0))
        otu_labs <- otu_labs[-c(OUT1, OUT2)]
        otu_labs <- c(otu_labs, cur_node)
        rownames(dm) <- otu_labs
        colnames(dm) <- otu_labs
        S <- colSums(dm)
        cur_node <- cur_node-1
        k <- k+2
        N <- N - 1
    }

    n <- length(edge1)
    edge1[(n-2):n] <- cur_node
    edge2[(n-2):n] <- otu_labs
    edge_length[n-2] <- (dm[1,2]+dm[1,3]-dm[2,3])/2
    edge_length[n-1] <- (dm[2,1]+dm[2,3]-dm[1,3])/2
    edge_length[n] <- (dm[3,1]+dm[3,2]-dm[1,2])/2
    obj <- list(edge=cbind(as.numeric(edge1), as.numeric(edge2)),
                edge.length=edge_length,
                tip.label=labels, Nnode=length(labels)-2)
    class(obj) <- "phylo"
    return(obj)
}
