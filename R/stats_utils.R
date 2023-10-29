#' Print the median and standard deviation
#'
#' @inheritParams plot_violin
#' @inheritParams str_pretty
#' @param x Vector or integers.
#'
#' @return String
#' @export
#'
#' @examples
#' print_median(runif(10))
print_median <- function(x, digits = 1, width = 10) {
    tmp <- paste0(
        median(x, na.rm = TRUE) %>% round(digits),
        "\u00b1",
        IQR(x, na.rm = TRUE) %>% round(digits)
    )
    if (str_length(tmp) > width) {
        str_replace_all(tmp, "\u00b1", "\n\u00b1")
    } else {
        tmp
    }
}


#' Print the result of a mean comparison test
#'
#' @param x Mean comparison test object among `anova_test`, `kruskal_test` or
#' `wilcox_test`.
#' @param digits Integer for the number of decimal places of the test value.
#' @param digits_p Integer for the number of decimal places of the p-value.
#'
#' @return String
#' @export
#'
#' @examples
#' library(rstatix)
#' data("ToothGrowth")
#' df <- ToothGrowth
#' res <- anova_test(df, len ~ dose)
#' print_mean_test(res)
#' res <- kruskal_test(df, len ~ dose)
#' print_mean_test(res)
#' res <- wilcox_test(df, len ~ supp)
#' print_mean_test(res)
print_mean_test <- function(x, digits = 1, digits_p = 2) {
    method <- class(x)[2] %>% str_remove_all("_test")
    if (method == "data.frame") {
        method <- "anova"
    }
    if (is.null(x$p.signif) %>% suppressWarnings()) {
        x <- x %>% add_significance0()
        if (x$p.signif == "ns") {
            x$p.signif <- ""
        }
    }
    if (x$p < 0.001) {
        x$p <- "< 0.001"
    } else {
        x$p <- paste0("= ", round(x$p, digits_p))
    }

    if (method == "anova") {
        par <- paste0("(", x$DFn, ", ", x$DFd, ")")
        statistic <- x$F %>% round(digits)
        index <- "Anova, F"
    } else if (method %in% c("kruskal", "t")) {
        par <- paste0("(", round(x$df, 1), ")")
        statistic <- x$statistic %>% round(digits)
        index <- ifelse(method == "t", "T-test, F", "Kruskal-Wallis, K")
    } else if (method == "wilcox") {
        par <- ""
        statistic <- x$statistic %>% round(digits)
        index <- "W"
    }
    paste0(index, par, " = ", statistic, ",", " p ", x$p, x$p.signif)
}
#' @inherit rstatix::add_significance return title params
#' @description Add p-value significance symbols into a data frame.
#' This is an wrapper for the function [rstatix::add_significance()].
# @examples
# # Perform pairwise comparisons and adjust p-values
# library(magrittr)
# library(rstatix)
# data("ToothGrowth")
# ToothGrowth %>%
#     t_test(len ~ dose) %>%
#     adjust_pvalue() %>%
#     add_significance0("p.adj")
add_significance0 <- function(data, p.col = NULL, output.col = NULL) {
    add_significance(
        data,
        p.col = NULL,
        output.col = NULL,
        cutpoints = c(0, 1e-03, 1e-02, 5e-02, 1),
        symbols = c("***", "**", "*", "ns")
    )
}

#' Multiple correlation
#'
#' Tests correlation between several variables. This is an wrapper for the
#' function [stats::cor.test()].
#'
#' @inheritParams plot_violin
#' @param x Data.frame of numerical variables.
#' @param pval Boolean to return adjusted p-values rather than coefficients.
#' @param method Character for the test method ('pearson' or 'spearman').
#'
#' @return Data.frame symmetrical containing correlation test results
#' (coefficients and adjusted p-value) for each pair of variables.
#' @export
#'
#' @examples
#' library(magrittr)
#' x <- runif(20)
#' x <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20)))
#' mcor_test(x)
#' mcor_test(x, pval = TRUE, method = "pearson", method_adjust = "bonferroni")
mcor_test <- function(
    x,
    pval = FALSE,
    method = "spearman",
    method_adjust = "BH") {
    x <- as.data.frame(x)
    vars <- seq(ncol(x))
    res <- sapply(
        vars,
        function(i) {
            sapply(
                vars,
                function(j) {
                    if (is.numeric(x[, i]) & is.numeric(x[, j])) {
                        tryCatch(
                            {
                                res <- cor.test(
                                    x[, i],
                                    x[, j],
                                    method = method,
                                    na.rm = TRUE
                                )
                                if (!pval) {
                                    res$estimate
                                } else {
                                    res$p.value
                                }
                            },
                            error = function(e) NA
                        )
                    } else {
                        NA
                    }
                }
            )
        }
    )
    if (!pval && method_adjust != "none") {
        as.vector(res) %>%
            p.adjust(method_adjust) %>%
            matrix(nrow = sqrt(length(.)), ncol = sqrt(length(.)))
    }
    colnames(res) <- rownames(res) <- colnames(x)
    return(res)
}
