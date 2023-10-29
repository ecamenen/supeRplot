# @description pivot_longer0() "lengthens" data, increasing the number of rows
# and decreasing the number of columns. This is an wrapper for the function
# [tidyr::pivot_longer()].
# @inherit tidyr::pivot_longer
pivot_longer0 <- function(data) {
    as.data.frame(data) %>%
        pivot_longer(everything())
}

#' Color palette
#'
#' This is an wrapper for the function [RColorBrewer::brewer.pal()].
#' @return Color vector
palette_discrete <- function() {
    c(
        brewer.pal(9, "Set1")[-c(6:7, 9)],
        rev(brewer.pal(7, "Set2")[c(6:5)]),
        brewer.pal(8, "Pastel1")[-c(3, 6:7, 9)]
    )
}

#' Color palette
#'
#' This is an wrapper for the function [RColorBrewer::brewer.pal()].
#' @param x Integer for the length of the palette
#' @return Color vector
palette_continuous <- function(x) {
    colorRampPalette(
        c(
            brewer.pal(9, "YlOrBr")[c(3, 5, 7)],
            brewer.pal(9, "RdBu")[1:4],
            brewer.pal(11, "PiYG")[4:1],
            brewer.pal(11, "PRGn")[1:4],
            brewer.pal(11, "RdYlBu")[7:11],
            brewer.pal(11, "BrBG")[10:8],
            brewer.pal(11, "PiYG")[7:11],
            rev(brewer.pal(7, "Greys")[-1])
        )
    )
}

count_cat <- function(x, width = 20, collapse = FALSE) {
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
        fct_relabel(~ str_replace_all(.x, "\\s*\\([^\\)]+\\)", "")) %>%
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
    return(df)
}

to_title <- function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


# @export
# Cuts a sentence to a given number of characters and leaves the words as integers
# @example str_trunc1("Hi there, I'm a sentence to format.")
str_trunc1 <- function(x, n = 20, w = " ") {
    x0 <- strsplit(x, w)[[1]]
    lapply(seq(length(x0)), function(i) str_trunc0(x, i, w)) %>%
        detect(function(x) str_length(x) <= n, .dir = "backward")
}

# Cuts a sentence to a given number of words
# @example str_trunc0("Hi there, I'm a sentence to format.")
str_trunc0 <- function(x, n = 5, w = " ") {
    strsplit(x, w)[[1]] %>%
        .[seq(n)] %>%
        paste(collapse = w)
}

str_trunc2 <- function(x, n = 20) {
    sapply(
        x,
        function(i) {
            i <- str_trim(i)
            res <- str_trunc1(i, n)
            if (is.null(res)) {
                res <- str_trunc1(i, n, "-")
            }
            if (is.null(res)) {
                return(res)
            }
            if (str_width(res) < str_width(i)) {
                paste0(res, "...")
            } else {
                res
            }
        }
    ) %>% unname()
}
