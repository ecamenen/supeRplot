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
            brewer.pal(9, "RdBu")[seq(4)],
            brewer.pal(11, "PiYG")[4:1],
            brewer.pal(11, "PRGn")[seq(4)],
            brewer.pal(11, "RdYlBu")[7:11],
            brewer.pal(11, "BrBG")[10:8],
            brewer.pal(11, "PiYG")[7:11],
            rev(brewer.pal(7, "Greys")[-1])
        )
    )
}

#' Frequency of categorical variable
#'
#' Formats a data.frame containing categorical variables and calculates the
#' frequency of each category.
#'
#' @inheritParams plot_pie
#' @inheritParams str_pretty
#' @param x Vector or data.frame of categorical variables.
#'
#' @return Data.frame with two columns (f and n).
#'
# @examples
# library(magrittr)
# # Vector of categorical variable
# k <- 10
# n <- runif(k, 1, 10) %>% round()
# x <- paste("Level", seq(k)) %>%
#     mapply(function(x, y) rep(x, y), ., n) %>%
#     unlist()
# count_cat(x)
#
# # Data.frame of categorical variable
# df <- sapply(seq(10), function(x) runif(10) %>% round()) %>% as.data.frame()
# colnames(df) <- paste("Level", seq(10))
# plot_bar_mcat(df)
# count_cat(df)
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
            summarise(
                f = paste(f, collapse = ", ") %>%
                    str_wrap(width)
            ) %>%
            mutate(f = factor(f))
        df$f <- reorder(df$f, df$n)
    }
    return(df)
}

#' Capitalize  only the first letter
#' @inherit str_trunc0 return params
#' @examples
#' to_title("hi there, I'm a sentence to format.")
#' @export
to_title <- function(x) {
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


#' @inherit str_pretty
#' @param sep Character to separate the terms.
# @examples
# str_trunc1("Hi there, I'm a sentence to format.")
str_trunc1 <- function(x, width = 20, sep = " ") {
    x <- check_character(x)
    width <- check_integer(width)
    sep <- check_character(sep)
    x0 <- strsplit(x, sep)[[1]]
    n <- str_length(x0[[1]])
    if (width < n) {
        width <- n
    }
    lapply(seq(length(x0)), function(i) str_trunc0(x, i, sep)) %>%
        detect(function(x) str_length(x) <= width, .dir = "backward")
}

#' Truncate a string to maximum number of words.
#'
#' @inherit str_pretty return params
#' @inheritParams str_trunc1
#' @param n Maximum number of words.
#'
# @examples
# str_trunc0("Hi there, I'm a sentence to format.")
str_trunc0 <- function(x, n = 5, sep = " ") {
    x <- check_character(x)
    n <- check_integer(n)
    sep <- check_character(sep)
    if (n < 1) {
        n <- 1
    }
    res <- strsplit(x, sep)[[1]]
    if (n > length(res)) {
        n <- length(res)
    }
    res[seq(n)] %>%
        paste(collapse = sep)
}

#' Truncate a string to maximum width
#'
#' Truncate a string to maximum width while ensuring that whole words are
#' retained
#' @param x String
#' @param width Maximum width of string.
#' @return String
#' @export
#'
#' @examples
#' str_pretty("Hi there, I'm a sentence to format.")
str_pretty <- function(x, width = 20) {
    sapply(
        x,
        function(i) {
            i <- str_trim(i) %>% to_title()
            res <- str_trunc1(i, width)
            if (is.null(res)) {
                res <- str_trunc1(i, width, "-")
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

check_length <- function(par, val, l = 1) {
    res <- unlist(val)
    if (length(res) > l) {
        res <- res[seq(l)]
        warning(paste0(par, " must be of length ", l, "."))
    }
    return(res)
}

check_integer <- function(x, l = 1) {
    par <- deparse(substitute(x))
    x <- check_length(par, x, l)
    if (!is.numeric(x)) {
        x <- as.numeric(x) %>% suppressWarnings()
        if (any(is.na(x))) {
            stop(paste(par, "is not an integer."))
        }
    }
    round(x)
}

check_colors <- function(x, l = .Machine$integer.max) {
    par <- deparse(substitute(x))
    x <- check_length(par, x, l)
    f <- function() {
      stop(paste(par, "must be in colors() or in an hexadecimal format."))
    }
    if (!is.null(x)) {
        for (i in x) {
            if (is.na(i) | (
              !(i %in% colors()) &&
              (as.numeric(i) %>% suppressWarnings() %>% is.na()) &&
                !grepl("^#{1}[a-zA-Z0-9]{6,8}$", i))
              ) {
                f()
            }
        }
    } else {
        f()
    }
    return(x)
}

check_data_frame <- function(x) {
    if (is.null(x)) {
        stop(paste(deparse(substitute(x)), "is NULL."))
    }
    as.data.frame(x)
}

check_boolean <- function(x, l = 1) {
    par <- deparse(substitute(x))
    x <- check_length(par, x, l)
    if (any(!is(x, "logical") | is.na(x))) {
        stop(paste(par, "is not a boolean."))
    }
    return(x)
}

check_character <- function(x, l = 1) {
    check_length(deparse(substitute(x)), x, l) %>% paste()
}
