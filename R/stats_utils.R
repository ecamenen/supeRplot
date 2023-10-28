print_stats <- function(x, digits = 1, wrap = 10) {
    tmp <- paste0(
        median(x, na.rm = TRUE) %>% round(digits),
        "\u00b1",
        IQR(x, na.rm = TRUE) %>% round(digits)
    )
    if (str_length(tmp) > wrap) {
        str_replace_all(tmp, "\u00b1", "\n\u00b1")
    } else {
        tmp
    }
}

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

add_significance0 <- function(x, p.col = NULL) {
    add_significance(
        x,
        p.col = NULL,
        cutpoints = c(0, 1e-03, 1e-02, 5e-02, 1),
        symbols = c("***", "**", "*", "ns")
    )
}

mcor <- function(x, pval = FALSE, method = "pearson", method_adjust = "BH") {
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
                                res <- cor.test(x[, i], x[, j], method = method, na.rm = TRUE)
                                if (pval) {
                                    res$estimate
                                } else {
                                    res$p.value
                                }
                            },
                            error = function(e) NA
                        )
                        # return(res)
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
