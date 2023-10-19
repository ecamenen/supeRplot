#' Barplot
#'
#' Display each numerical value separately using a barplot
#'
#' @inheritParams plot_violin
#' @inheritParams plot_bar_mcat
#' @param x Vector of numerical values visualized on the plot.
#' @param colour Color or vector of colors for the gradient of the bars.
#' @param threshold Double for the minimal percentage value before being
#'  hidden on the plot.
#'
#' @examples
#' library(magrittr)
#'
#' # Default parameters
#' x <- runif(10, 1, 10) %>%
#'     set_names(paste("Sample", LETTERS[seq(10)]))
#' plot_bar(x)
#'
#' # Advanced parameters
#' plot_bar(
#'     x = x,
#'     title = "Some numerical variable",
#'     width_title = 30,
#'     colour = c("yellow", "gray", "red"),
#'     color_title = "blue",
#'     cex = 1.2,
#'     digits = 1,
#'     n_max = 5,
#'     ratio = 15,
#'     hjust_title = 1
#' )
#' @export plot_bar
plot_bar <- function(
    x = NULL,
    title = NULL,
    width_title = 20,
    colour = c("blue", "gray", "#cd5b45"),
    color_title = "black",
    cex = 1,
    cex_main = cex * 30,
    digits = 0,
    n_max = 100,
    ratio = 5,
    threshold = 2) {
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    df0 <- as.data.frame(x) %>%
        set_colnames("val")
    if (n_max > nrow(df0)) {
        n_max <- nrow(df0)
    }
    df <- (
        df0 %>%
            mutate(name = rownames(.)) %>%
            arrange(desc(val)) %>%
            mutate(order = rev(seq(nrow(df0))))
    ) %>%
        head(n_max) %>%
        set_rownames(.$name)
    p <- ggplot(df, aes(order, val, fill = order)) +
        theme_minimal()
    colors <- colorRampPalette(colour)(length(p$data$val))
    y_lab <- p$data$val / 2
    x_lab <- round(p$data$val, digits)
    x_lab[p$data$val < threshold] <- ""
    p +
        geom_bar(stat = "identity") +
        expand_limits(y = max(p$data$val) + max(p$data$val) / ratio) +
        coord_flip() +
        scale_x_continuous(breaks = df$order, labels = rownames(df)) +
        scale_y_continuous(
            breaks = pretty_breaks(n = 4),
            labels = label_number_auto()
        ) +
        labs(title = str_wrap(title, width_title)) +
        geom_text(
            aes(color = I("white"), y = y_lab, label = x_lab),
            size = cex * 7
        ) +
        theme(
            axis.text.y = element_text(size = cex * 20, face = "italic", color = colors),
            axis.text.x = element_text(size = cex * 20, face = "italic", color = "darkgrey"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(size = cex_main, face = "bold", color = color_title),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank()
        ) %>%
      suppressWarnings() +
        theme(legend.position = "none") +
        scale_fill_gradientn(colours = rev(colors), na.value = "black") %>%
      suppressWarnings()
}
