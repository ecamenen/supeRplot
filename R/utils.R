get_melt <- function(x) {
    as.data.frame(x) %>%
        pivot_longer(everything())
}

get_colors <- function() {
    c(
        brewer.pal(9, "Set1"),
        brewer.pal(7, "Set2"),
        brewer.pal(8, "Pastel1"),
        brewer.pal(8, "Set2")[-5]
    )
}

count_cat <- function(
    x,
    width = 20,
    collapse = FALSE,
    label = NULL) {
    x0 <- as.data.frame(x)
    if (ncol(x0) > 1) {
        x <- sapply(
            colnames(x0),
            function(i) rep(i, pull(x0, i) %>% unlist() %>% sum(na.rm = TRUE))
        )
    }
    x <- unlist(x) %>%
        stri_trans_general("latin-ascii") %>%
        str_replace_all("\n", " ") %>%
        to_title()
    df <- factor(x) %>%
        # fct_relabel(~ str_replace_all(.x, "\\s*\\([^\\)]+\\)", "")) %>%
        fct_relabel(~ str_replace_all(.x, "\\$\\$[^\\)]+", "")) %>%
        fct_relabel(~ str_replace_all(.x, "^0$", "No")) %>%
        fct_relabel(
            ~ str_replace_all(
                .x,
                "^1$",
                ifelse(colnames(x0)[1] == "x", "Yes", colnames(x0)[1])
            )
        ) %>%
        str_wrap(width) %>%
        fct_infreq() %>%
        fct_rev() %>%
        fct_count()
    if (collapse) {
        df <- group_by(df, n) %>%
            summarise(f = paste(f, collapse = ", ") %>% str_wrap(width)) %>%
            mutate(f = factor(f))
        df$f <- reorder(df$f, df$n)
    }
    if (!is.null(label)) {
        df$f <- factor(df$f, labels = rev(str_wrap(label, width)))
    }
    return(df)
}
