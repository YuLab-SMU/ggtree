##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    options(check.tbl_tree.verbose = FALSE)
    pkgVersion <- packageDescription(pkgname, fields = "Version")
    ref <- random_ref(pkgname = pkgname, pkgVersion = pkgVersion, random_n = 2)
    if (!is.null(ref)) packageStartupMessage(ref)
}

random_ref <- getFromNamespace("random_ref", 'tidytree')
