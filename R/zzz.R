.onAttach <- function(libname, pkgname) {
    ##	pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0("If you use ggtree in published research, please cite:\n\n",
                  "Guangchuang Yu, David Smith, Huachen Zhu, Yi Guan, Tommy Tsan-Yuk Lam.\n",
                  "ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data.\n",
                  "Methods in Ecology and Evolution 2016, doi:10.1111/2041-210X.12628\n\n")
    packageStartupMessage(msg)
}
