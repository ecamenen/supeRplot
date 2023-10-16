print_stats0 <- function(x, dec = 1, wrap = 10) {
    tmp <- paste0(median(x, na.rm = TRUE) %>% round(dec), "\u00b1", IQR(x, na.rm = TRUE) %>% round(dec))
    if (str_length(tmp) > wrap) {
        str_replace_all(tmp, "\u00b1", "\n\u00b1")
    } else {
        tmp
    }
}


print_mean_test <- function(x, dec = 1, dec_p = 2) {
    method <- class(x)[2] %>% str_remove_all("_test")
    if (method == "data.frame") {
        method <- "anova"
    }

    # stopifnot(method %in% c("anova", "ks"))
    # e <- effectsize(x) %>% suppressMessages()
    if (is.null(x$p.signif) %>% suppressWarnings()) {
        x <- x %>% add_significance0()
        if (x$p.signif == "ns") {
            x$p.signif <- ""
        }
    }
    if (x$p < 0.001) {
        x$p <- "< 0.001"
    } else {
        x$p <- paste0("= ", round(x$p, dec_p))
    }

    if (method == "anova") {
        par <- paste0("(", x$DFn, ", ", x$DFd, ")")
        statistic <- x$F %>% round(dec)
        index <- "Anova, F"
    } else if (method %in% c("kruskal", "t")) {
        par <- paste0("(", round(x$df, 1), ")")
        statistic <- x$statistic %>% round(dec)
        index <- ifelse(method == "t", "T-test, F", "Kruskal-Wallis, K")
    } else if (method == "wilcox") {
        par <- ""
        statistic <- x$statistic %>% round(dec)
        # index <- "Wilcoxon, W"
        index <- "W"
    }
    paste0(index, par, " = ", statistic, ",", " p ", x$p, x$p.signif)
}
