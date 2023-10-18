#' Barplot for categorical variables
#'
#' Visualize the proportions of multiple categorical variables using a
#' barplot
#'
#' @inheritParams plot_violin
#' @inheritParams plot_pie
#' @param x Data.frame of character values visualized on the plot.
#' @param ratio Integer for the width scale
#' @param n_collapse Integer for the maximum number of merged categories to show
#' @param n_max Integer for the maximum number of categories to show
#' (prioritizing those with the largest sample size)
#' @param hjust_title Integer for the horizontal justification of the title
#' (in \[0, 1\]).
#' @param hjust_text Integer for the horizontal justification of the text
#' (in \[0, 1\]).
#' @param vjust_text Integer for the vertical justification of the text
#' (in \[0, 1\]).
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' # Default parameters
#' df <- sapply(seq(10), function(x) runif(10) %>% round()) %>% as.data.frame()
#' colnames(df) <- paste("Level", seq(10))
#' plot_bar_mcat(df)
#'
#' # Advanced parameters
#' plot_bar_mcat(
#'     df,
#'     sample_size = 15,
#'     title = "Some categorical variable",
#'     width_text = 30,
#'     width_title = 50,
#'     colour = brewer.pal(9, "Reds"),
#'     color_title = "red",
#'     cex = 8,
#'     digits = 1,
#'     collapse = TRUE,
#'     ratio = 2,
#'     hjust_title = 1,
#'     n_max = 4,
#'     n_collapse = 3
#' )
#' @return A ggplot object.
#' @export
plot_bar_mcat <- function(
    x,
    sample_size = NULL,
    title = NULL,
    width_text = 20,
    width_title = width_text,
    colour = c("blue", "gray", "#cd5b45"),
    color_title = "black",
    cex = 10,
    digits = 0,
    collapse = FALSE,
    ratio = 5,
    # label = NULL,
    n_collapse = 5,
    n_max = Inf,
    threshold = 1,
    hjust_title = -0.5,
    hjust_text = -0.1,
    vjust_text = 0.5) {
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    x0 <- as.data.frame(x)
    df <- count_cat(
        x0,
        width = width_text,
        collapse = collapse
        # label = label
    ) %>%
        data.frame(., order = as.numeric(rownames(.))) %>%
        tail(n_max)
    if (is.null(sample_size)) {
        sample_size <- nrow(x0)
    }
    colour <- colorRampPalette(tail(colour, n_max))(nrow(df))
    x_lab <- (round(df$n, 2) / sample_size * 100) %>%
        round(digits) %>%
        paste0("%")
    df$x_lab <- x_lab
    df$y_lab <- df$n / 2
    y_lab0 <- as.character(df$f)
    y_lab0[(str_count(y_lab0, "\\,") + 1) >= n_collapse] <- "..."
    i <- df$y_lab < threshold
    df$x_lab[i] <- ""
    (ggplot(df, aes(f, n, fill = order, label = n)) +
        geom_bar(stat = "identity") +
        expand_limits(y = max(df$n) + max(df$n) / ratio) +
        coord_flip() +
        scale_x_discrete(labels = y_lab0)
    ) %>%
        theme_bar(colors = colour, cat = FALSE) +
        geom_text(
            aes(color = I("white"), y = y_lab),
            size = cex
        ) +
        geom_text(
            aes(label = x_lab, color = colour),
            data = df,
            hjust = hjust_text,
            vjust = vjust_text,
            size = cex
        ) +
        ggtitle(str_wrap(title, width_title)) +
        theme(
            plot.title = element_text(
                hjust = hjust_title,
                vjust = 0,
                size = cex * 4,
                face = "bold",
                color = color_title
            ),
            axis.text.y = element_text(colour = colour, size = cex * 3),
            axis.text.x = element_text(size = cex * 2.2),
            plot.margin = unit(c(-0, 0, 0, 0.5), "cm"),
            panel.grid.major.y = element_blank()
        ) %>%
        suppressWarnings() +
        scale_y_continuous(
            breaks = breaks_pretty(2),
            labels = label_number_auto()
        )
}
