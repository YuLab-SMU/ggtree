## @method print ggtree
## @export
#print.ggtree <- function(x, ...) {
#    # REF: https://github.com/YuLab-SMU/tidytree/pull/39
#    flag <- getOption(x="check.tbl_tree.verbose", default=TRUE)
#    on.exit(options(check.tbl_tree.verbose=flag))
#    options(check.tbl_tree.verbose=FALSE)
#    plot(x, ...)
#}


#' @export
print.ggtree <- function(x, ...) {
    # use NextMethod() or strip class to avoid infinite recursion
    class(x) <- setdiff(class(x), "ggtree")
    
    interactive <- is_ggtree_interactive()
    
    if (!.check_interactive_attr(x) && interactive){
        cli::cli_warn(c("Interactive visualization of ggtree requires specific interactive attributes,",
                        "such as {.var tooltip}, {.var onclick}, or {.var data_id}; without them,",
                        "no interactive features will be available!"))
    }
    
    if (interactive) {
        x <- girafe_tree(x)
    } 
    
    # If running in knitr (Quarto), delegate to knit_print
    if (isTRUE(getOption('knitr.in.progress'))) {
        return(knitr::knit_print(x, ...))
    }

  print(x, ...)
}

#' @importFrom ggiraph girafe
#' @importFrom ggiraph opts_sizing
#' @importFrom ggiraph opts_toolbar
#' @importFrom ggiraph opts_zoom
#' @importFrom ggiraph opts_tooltip
girafe_tree <- function(p, width_svg = 10, height_svg = 10, options = NULL) {
    default_options <- list(
      opts_sizing(rescale = TRUE),
      opts_toolbar(position = 'topleft', saveaspng = FALSE),
      opts_zoom(min = .7, max = 10),
      opts_tooltip(use_fill = TRUE)
    )
    if (!is.null(options) && is.list(options)) {
        options <- modifyList(default_options, options)
    } else {
        options <- default_options
    }
    
    fig <- girafe(ggobj = p, 
            width_svg = width_svg, 
            height_svg=height_svg,
            options = options
        )

  return(fig)
}

#' set ggtree interactive mode
#' 
#' @title Set ggtree interactive mode
#' @param check whether to check interactive attributes
#' @return NULL
#' @export
#' @importFrom yulab.utils update_cache_item
ggtree_set_interactive <- function(check=TRUE) {
    update_cache_item('ggtree', 
        list(interactive = TRUE, check_interactive = check)
    )
}

#' unset ggtree interactive mode
#' 
#' @title Unset ggtree interactive mode
#' @return NULL
#' @export
ggtree_unset_interactive <- function() {
    update_cache_item('ggtree', list(interactive = FALSE))
}

#' @importFrom yulab.utils get_cache_item
is_ggtree_interactive <- function() {
    x <- get_cache_item('ggtree')[['interactive']]
    if (is.null(x)) return (FALSE)
    return(x)
}


.check_interactive_attr <- function(x) {
    check <- get_cache_item('ggtree')[['check_interactive']]
    if (is.null(check)) check <- FALSE

    # if not check interactive attributes
    # assume it is satisfied
    if (!check) return(TRUE)

    attrs <- c("tooltip", "data_id", "onclick")
    gb <- ggplot2::ggplot_build(x)
    flag <- any(lapply(gb$data, function(i)colnames(i) %in% attrs) |> unlist())
    return(flag)
}
