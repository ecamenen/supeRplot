#' Correlation matrix
#'
#' Display a heatmap of correlation coefficients (with p-values between several
#'  numerical variables). Each cell indicates the correlation between two
#'  variables. The darker the color, the stronger the correlation. Blue
#'  indicates a positive correlation, red a negative one. Non-significant
#'  adjusted p-values are crossed out. This is an wrapper for the function
#'  [corrplot::corrplot()].
#'
#' @inheritParams plot_violin
#' @inheritParams plot_bar
#' @param x Data.frame of double variables (with column names).
#' @param colour Color or vector of colors for the gradient of the bars.
#' @param method Character for the test method ('pearson' or 'spearman').
#' @param mat Matrix of double for correlation coefficients (with column and row names).
#' @param p_mat Matrix of double for adjusted p-values (with column and row names).
#'
#' @return NULL (launch a basic plot)
#' @export
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' x <- sapply(seq(20), function(x) runif(20)) %>%
#'     set_colnames(paste("Variable", seq(20)))
#'
#' # Default parameters
#' plot_mcor(x)
#'
#' # Advanced parameters
#' plot_mcor(
#'     x,
#'     colour = c("black", brewer.pal(n = 6, name = "RdBu"), 1),
#'     method = "spearman",
#'     method_adjust = "none",
#'     cex = 0.8
#' )
plot_mcor <- function(
    x,
    colour = brewer.pal(n = 8, name = "RdBu"),
    cex = 1,
    method = "spearman",
    method_adjust = "BH",
    mat = NULL,
    p_mat = NULL) {
    x <- as.data.frame(x)
    if (is.null(mat)) {
        mat <- mcor_test(x, FALSE, method = method)
    }
    if (is.null(p_mat)) {
        p_mat <- mcor_test(
            x,
            TRUE,
            method = method,
            method_adjust = method_adjust
        )
    }

    corrplot(
        mat,
        col = colour,
        type = "upper",
        order = "original",
        tl.col = "gray40",
        tl.srt = 45,
        tl.cex = 1 * cex,
        p.mat = p_mat,
        sig.level = 0.05,
        addgrid.col = NA,
        insig = "pch",
        pch = 4,
        pch.cex = 2.5 * cex,
        pch.col = "white",
        diag = FALSE,
        na.label = " ",
        cl.cex = cex * 0.95,
        cl.align.text = "l"
    )
}
