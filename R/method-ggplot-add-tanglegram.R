#' @method ggplot_add tanglelink
#' @export
ggplot_add.tanglelink <- function(object, plot, object_name, ...) {
    dat <- object$data

    if (is.null(dat)) {
        dat <- attr(plot$data, "tangle_assoc")
        if (is.null(dat)) {
            stop("No tanglegram association data is attached to the plot.", call. = FALSE)
        }
    } else {
        dat <- as.data.frame(dat, stringsAsFactors = FALSE)
        if (!all(c("x", "y", "xend", "yend") %in% names(dat))) {
            if (!is.null(object$mapping) && all(c("left", "right") %in% names(object$mapping))) {
                left_col <- rlang::as_name(object$mapping$left)
                right_col <- rlang::as_name(object$mapping$right)
                names(dat)[match(c(left_col, right_col), names(dat))] <- c("left", "right")
            } else if (!is.null(object$left) && !is.null(object$right)) {
                dat <- data.frame(left = object$left, right = object$right, stringsAsFactors = FALSE)
            }
            dat <- .resolve_assoc_endpoints(plot$data, .normalize_tangle_assoc(dat))
        }
    }

    params <- c(
        list(
            data = dat,
            mapping = ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
            inherit.aes = FALSE
        ),
        object$params
    )

    layer_obj <- if (identical(object$geom, "segment")) {
        do.call(ggplot2::geom_segment, params)
    } else {
        do.call(ggplot2::geom_curve, c(params, list(curvature = object$curvature)))
    }

    ggplot2::ggplot_add(layer_obj, plot, object_name, ...)
}
