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
    x <- unlist(x)
    digits <- check_integer(digits, min = 0)
    width <- check_integer(width)
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
print_mean_test <- function(x, digits = 0, digits_p = 2) {
    check_type(x, paste0(c("anova", "kruskal", "wilcox"), "_test"))
    if (nrow(x) > 1) {
        stop("x must have a single row.")
    }
    digits <- check_integer(digits, min = 0)
    digits_p <- check_integer(digits_p)
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
#' @param method Character for the test method ('pearson', 'kendall', or
#' spearman').
#'
#' @return Data.frame symmetrical containing correlation test results
#' (coefficients and adjusted p-value) for each pair of variables.
#'
#' @examples
#' library(magrittr)
#' x <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20)))
#' mcor_test(x)
#' mcor_test(x, p.value = TRUE, method = "pearson", method_adjust = "bonferroni")
mcor_test <- function(
    x,
    estimate = TRUE,
    p.value = FALSE,
    method = "spearman",
    method_adjust = "BH") {
    x <- as.data.frame(x)
    p.value <- check_boolean(p.value)
    check_choices(method, c("pearson", "kendall", "spearman"))
    check_choices(method_adjust, p.adjust.methods)
    vars <- seq(ncol(x))
    res <- lapply(
        vars,
        function(i) {
            lapply(
                vars,
                function(j) {
                    if (is.numeric(x[, i]) & is.numeric(x[, j])) {
                        tryCatch(
                            {
                                cor.test(
                                    x[, i],
                                    x[, j],
                                    method = method,
                                    na.rm = TRUE
                                )
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

    if (estimate) {
        rho <- sapply(res, function(i) sapply(i, function(j) j$estimate))
        colnames(rho) <- rownames(rho) <- colnames(x)
    }
    if (p.value) {
       p <- sapply(res, function(i) sapply(i, function(j) j$p.value))
    }

    if (p.value && method_adjust != "none") {
        p <- as.vector(p) %>%
            p.adjust(method_adjust) %>%
            matrix(nrow = sqrt(length(.)), ncol = sqrt(length(.)))
    }
    colnames(p) <- rownames(p) <- colnames(x)

    if (estimate && p.value) {
        return(list(estimate = rho, p.value = p))
    } else if (estimate) {
        return(rho)
    } else {
        return(p)
    }
}

print_chi2_test <- function(x, dec = 3) {
    if ("chisq_test" %in% class(x)) {
        x$statistic <- paste0("X²(", x$df, ") = ", round(x$statistic, 1), ", ")
        x$method <- paste0(x$method, ", ")
    } else {
        x$method <- "" # Fisher's Exact test"
        x$statistic <- ""
    }
    if (x$p.signif == "ns") {
        x$p.signif <- ""
    }
    if (x$p < 0.001) {
        x$p <- "< 0.001"
    } else {
        x$p <- paste("=", round(x$p, dec))
    }
    x$p.signif[x$p.signif == "****"] <- "***"
    paste0(x$statistic, "p ", x$p, x$p.signif, ", N = ", x$n)
}


# x <- c(A = 100, B = 78, C = 25)
# x <- c(rep("A", 100), rep("B", 78), rep("C", 25))
# file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
# housetasks <- read.delim(file_path, row.names = 1)
post_hoc_chi2 <- function(
    x,
    method = "chisq",
    method_adjust = "BH",
    dec = 3,
    count = FALSE) {
  df0 <- as.data.frame(x)
  if (ncol(df0) > 1) {
    # df0 <- set_colnames(x0, c("var1", "var2"))
    if (count) {
      x <- colnames(df0)
    } else {
      x <- pull(df0, 2)
    }
  }
  comb <- combn(x %>% unique() %>% length() %>% seq(), 2)
  res <- lapply(
    seq(ncol(comb)),
    function(i) {
      if (ncol(df0) > 1) {
        if(!count) {
          x0 <- table(df0)
        } else {
          x0 <- df0
        }
        df <- x0[, comb[, i]]
        dimn <- colnames(df)
      } else {
        if (!count) {
          x0 <- table(x)
        } else {
          x0 <- x
        }
        df <- x0[comb[, i]]
        dimn <- names(df)
      }
      get(paste0(method, "_test"))(df) %>%
        mutate(group1 = dimn[1], group2 = dimn[2])
      # mutate(groups = colnames(df) %>% paste(collapse = " vs ")) %>%
      # relocate(groups, .before = n)
    }
  ) %>%
    Reduce(rbind, .) %>%
  mutate(FDR = round(p.adjust(p, method_adjust), dec)) %>%
  add_significance(p.col = "FDR", output = "fdr.signif") %>%
  mutate(
    p = ifelse(p < 0.001, "< 0.001", round(p, dec)),
    FDR = ifelse(FDR < 0.001, "< 0.001", FDR)
  ) %>%
  select(-matches(c("method")))
  res[res == "****"] <- "***"
  if (method == "chisq")
    relocate(res, df, .before = p)
  else
    res
}
