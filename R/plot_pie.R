#' Piechart
#'
#' Visualize the proportions of a categorical variable using a piechart
#'
#' @inheritParams plot_violin
#' @inheritParams ggplot2::margin
#' @param x Vector of character values visualized on the plot.
#' @param hsize Integer for the size of the central hole in the pie chart
#' (in \[1, 2\]).
#' @param legend Boolean to toggle the display of the legend.
#' @param n Integer for the sample size of the dataset to calculate percentages
#'  (if different from the length of the variable).
#' @param collapse Boolean to merge categories with identical proportions.
#' @param threshold Integer for the minimal percentage value before being
#'  hidden on the plot.
#'
#' @examples
#' library(magrittr)
#' library(RColorBrewer)
#'
#' # Default plot
#' x <- c(rep("A", 5), rep("B", 4))
#' plot_pie(x)
#'
#' # Advanced parameters
#' k <- 10
#' n <- runif(k, 1, 10) %>% round()
#' x <- paste("Level", seq(k)) %>%
#'     mapply(function(x, y) rep(x, y), ., n) %>%
#'     unlist()
#' plot_pie(
#'     x,
#'     title = "Some random variable",
#'     width_text = 5,
#'     width_title = 15,
#'     colour = brewer.pal(9, "Reds"),
#'     cex = 20,
#'     digits = 1,
#'     hsize = 1.5,
#'     collapse = TRUE,
#'     b = 3
#' )
#'
#' @return A ggplot object.
#' @export
plot_pie <- function(
    x,
    title = NULL,
    width_text = 5,
    width_title = 20,
    colour = get_colors(),
    digits = .1,
    cex = 15,
    cex_main = cex * 1.5,
    hsize = 1.2,
    legend = TRUE,
    n = NULL,
    collapse = FALSE,
    threshold = 5,
    # label_name = NULL,
    # legend_name = NULL,
    t = -0.5,
    l = -1,
    r = -1,
    b = -1) {
    df <- count_cat(
        x,
        width = width_text,
        collapse = collapse
        # label = label_name
    )
    if (!is.null(n)) {
        df <- rbind(df, data.frame(f = NA, n = c(n - sum(df$n))))
    }
    if (!is.null(legend) && !is.logical(legend)) {
        df$f <- factor(df$f, labels = legend)
    }
    df <- mutate(
        df,
        hsize = hsize,
        pos = rev(cumsum(rev(n))),
        pos = n / 2 + lead(pos, 1),
        pos = if_else(is.na(pos), n / 2, pos),
        label = str_wrap(str_glue("{f}"), width_text),
        text = scales::percent(n / sum(n), digits)
    )
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    i <- df$n / sum(df$n) <= threshold / 100
    df$text[i] <- ""
    # if (!is.null(legend_name)) {
    #     df$legend <- str_replace_all(legend_name, "\\(", "\\\\(") %>%
    #         str_replace_all("\\)", "\\\\)") %>%
    #         c("NA")
    #     i <- sapply(df$legend, function(i) {
    #         paste(df$f) %>%
    #             str_replace_all("\n", " ") %>%
    #             str_which(i)
    #     })
    #     colour0 <- c(rev(colour), "gray")[i]
    #     colour <- colour0
    #     df$legend <- df$f[i]
    # } else {
    df$legend <- df$f
    colour <- sort(levels(df$f)) %>%
        match(., levels(df$f)) %>%
        colour[.] %>%
        c("gray")
    colour0 <- colour
    # }
    df$legend0 <- paste0(df$legend, ": ", df$n)
    df$legend[df$legend == "NA"] <- NA
    p <- ggplot(df, aes(x = hsize, y = n, fill = f)) +
        geom_col(width = 1, color = NA) +
        geom_text(
            color = "white",
            size = cex / 2.5,
            aes(label = text),
            position = position_stack(vjust = 0.5)
        ) +
        coord_polar(theta = "y", clip = "off") +
        scale_fill_manual(
            values = colour,
            na.value = "gray",
            labels = df$legend0,
            breaks = df$legend,
            name = ""
        ) +
        scale_y_continuous(breaks = df$pos, labels = df$label) +
        ggtitle(str_wrap(title, width_title)) +
        theme(
            plot.title = element_text(
                hjust = 0.5,
                vjust = -4,
                size = cex_main,
                face = "bold"
            ),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            legend.text = element_text(size = cex),
            legend.key = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.margin = unit(c(t, r, b, l), "cm")
        ) +
        xlim(0.5, hsize + 0.5)
    if (is.null(legend) || legend == FALSE) {
        p + theme(legend.position = "none")
    } else {
        p
    }
}
