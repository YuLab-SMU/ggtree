##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "  ",
                  "For help: https://yulab-smu.top/treedata-book/", "\n\n")

    citation <- paste0("If you use ", pkgname,
                       " in published research, please cite the most appropriate paper(s):\n\n",
                       ggtree_citations(), "\n")

    packageStartupMessage(paste0(msg, citation))
}


