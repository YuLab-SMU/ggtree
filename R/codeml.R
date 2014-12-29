##' read baseml output
##'
##' 
##' @title read.codeml 
##' @param rstfile rst file
##' @param mlcfile mlc file
##' @return A \code{codeml} object
##' @export
##' @author ygc
read.codeml <- function(rstfile, mlcfile) {
    new("codeml",
        rst = read.paml_rst(rstfile),
        mlc = read.codeml_mlc(mlcfile)
        )
}



