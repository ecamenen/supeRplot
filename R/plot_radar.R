#' Radar plot
#'
#' Display multivariate data in the form of a two-dimensional chart of three
#' or more quantitative variables represented on axes starting from the same
#' point. The relative position and angle of the axes is typically
#' uninformative. The radar chart is also known as web chart, spider chart,
#' star chart, cobweb chart, irregular polygon, polar chart, or kiviat diagram.
#' This is an wrapper for the function [fmsb::radarchart()].
#'
#' @param x either a integer vector (with named elements) or a data frame
#' containing variables in columns and samples in rows (with named columns
#'  and rows).
#' @param colour Color or vector of colors for the samples.
#' @param cex Double for the size of the labels.
#' @param n_max Integer for the maximum value of the axis.
#' @param add_percent Boolean to include percentage values in the axis labels.
#' @param digits Integer for the number of digits to be displayed.
#' @param n_interval Integer for the number of intervals for the axis.
#' @param alpha Double for the transparency.
#'
#' @examples
#' library(magrittr)
#'
#' df <- sapply(
#'     seq(2),
#'     function(x) {
#'         runif(10, 1, 100) %>%
#'             set_names(paste("Variable", letters[seq(10)]))
#'     }
#' ) %>% set_colnames(paste("Sample", LETTERS[seq(2)]))
#'
#' # Default parameters
#' plot_radar(df[, 1])
#'
#' # Advanced parameters
#' plot_radar(
#'     df,
#'     colour = c("orange", "forestgreen"),
#'     cex = 0.9,
#'     n_max = 100,
#'     digits = 1,
#'     n_interval = 4,
#'     alpha = 0.5
#' )
#'
#' @return NULL (launch a basic plot)
#' @export
plot_radar <- function(
    x,
    colour = palette_discrete(),
    cex = 1,
    n_max = NULL,
    add_percent = FALSE,
    digits = 0,
    n_interval = 2,
    alpha = 0.25) {
    if (!is.null(ncol(x))) {
        x <- t(x)
        n <- ncol(x)
        colors <- colour[seq(n)]
    } else {
        n <- length(x)
        colors <- colour[1]
    }
    if (is.null(n_max)) {
        n_max <- unlist(x) %>%
            as.numeric() %>%
            max(na.rm = TRUE) %>%
            round(digits)
    }
    caxislabels <- seq(0, n_max, length = n_interval + 1) %>%
        .[-1] %>%
        round(digits)
    if (add_percent) {
        caxislabels <- paste0(caxislabels, "%")
    }
    x[is.na(x)] <- 0
    x0 <- rbind(
        rep(n_max, n),
        rep(0, n)
    ) %>%
        set_rownames(c("Max", "Min")) %>%
        set_colnames(colnames(x)) %>%
        rbind(x)

    apply(x0, 2, as.numeric) %>%
        as.data.frame() %>%
        radarchart(
            cglwd = 3,
            cglty = 2,
            cglcol = "gray",
            plty = 1,
            pcol = colors,
            pfcol = alpha(colors, alpha),
            plwd = 4,
            axistype = 1,
            axislabcol = colors[1],
            caxislabels = c("", caxislabels),
            seg = n_interval,
            vlcex = cex * 1.5,
            calcex = cex * 1.25,
            centerzero = TRUE
        )
    if (!is.null(ncol(x))) {
        legend(
            "bottomleft",
            legend = rownames(x),
            bty = "n",
            pch = 15,
            col = colors,
            text.col = "black",
            pt.cex = cex * 2.5,
            cex = cex * 1.5
        )
    }
}
