##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "  ",
                  "For help: https://guangchuangyu.github.io/", pkgname, "\n\n")

    citation <- paste0("If you use ", pkgname, " in published research, please cite:\n",
                  "Guangchuang Yu, David Smith, Huachen Zhu, Yi Guan, Tommy Tsan-Yuk Lam. ",
                  "ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. ",
                  "Methods in Ecology and Evolution 2017, 8(1):28-36, doi:10.1111/2041-210X.12628")

    packageStartupMessage(paste0(msg, citation))
}
