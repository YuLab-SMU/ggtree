#' @keywords internal
#' @aliases package-ggtree
"_PACKAGE"

## The layout engine lives in treelayout; ggtree imports the pieces its
## plotting code still calls directly. Importing them (rather than calling
## treelayout::x()) keeps the S3 methods registered inside tidytree working,
## which treelayout::offspring() and friends depend on.
##' @importFrom treelayout tree_layout layout_names get_layout
##' @importFrom treelayout calculate_angle calculate_branch_mid
##' @importFrom treelayout getNodeEuclDistances getNodes_by_postorder
##' @importFrom treelayout getSubtree getSubtree.df getTreeArcAngles
##' @importFrom treelayout getXcoord2 getYcoord
##' @importFrom treelayout getYcoord_scale_category getYcoord_scale_numeric
##' @importFrom treelayout re_assign_ycoord_df
##' @importFrom treelayout .getRoot.df
NULL
