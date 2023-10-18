#' Histogram settings
#'
#' Default font for a vertical barplot.
#'
#' @param p A ggplot object.
#' @param df A dataframe with a column named "order"
#' @param title A character string giving a graphic title
#' @param color A vector of character giving the colors for the rows
#' @examples
#' df <- data.frame(x = runif(30), order = 30:1)
#' library("ggplot2")
#' p <- ggplot(df, aes(order, x))
#' plotHistogram(p, df, "This is my title", "red")
#' # Add colors per levels of a variable
#' df$color <- rep(c(1, 2, 3), each = 10)
#' p <- ggplot(df, aes(order, x, fill = color))
#' plot_bar(p, df, "Histogram", as.character(df$color))
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
