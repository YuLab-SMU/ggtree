##' @importFrom utils packageDescription
##' @importFrom pillar style_subtle
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- style_subtle(paste0(pkgname, " v", pkgVersion, "  ",
                  "For help: https://yulab-smu.top/treedata-book/", "\n\n"))

    #citation <- paste0("If you use ", pkgname,
    #                   " in published research, please cite the most appropriate paper(s):\n\n",
    #                   ggtree_citations(), "\n")
    citation <- random_ref()
    packageStartupMessage(paste(strwrap(paste0(msg, citation)), collapse = "\n"))
}

random_ref <- getFromNamespace("random_ref", 'tidytree')
