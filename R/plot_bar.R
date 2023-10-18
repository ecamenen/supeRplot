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
#' plotHistogram(p, df, "Histogram", as.character(df$color))
#' @export plotHistogram
plotHistogram <- function(p = NULL, df = NULL, hjust = 0, vjust = 0.5, n = 100, title = "", color_title = "black", color_gradient = c("gray", "#99000D"), cex = 1, ratio = 15, dec = 0, rows = NULL, colors = NULL) {
  if (is.null(p)) {
    df0 <- as.data.frame(df)
    colnames(df0)[1] <- "val"
    if (n > nrow(df0)) {
      n <- nrow(df0)
    }
    df <- (
      df0 %>%
        mutate(name = rownames(.)) %>%
        arrange(desc(val)) %>%
        mutate(order = rev(seq(nrow(df0))))
    ) %>% head(n) %>%
      set_rownames(.$name)
    p <- ggplot(df, aes(order, val, fill = order)) +
      theme_minimal()
  }
  if (is.null(colors))
    colors <- rev(colorRampPalette(color_gradient)(length(p$data$val)))
  y_lab <- p$data$val / 2
  x_lab <- ""
  if (!is.null(rows))
    x_lab <- (round(p$data$val, dec) / rows * 100) %>%
    round(dec) %>%
    paste("%")
  x_lab[p$data$val < 2] <- ""
  p +
    # TODO: if NB_ROW > X, uncomment this
    # geom_hline(yintercept = c(-.5,.5), col="grey", linetype="dotted", size=1) +
    geom_hline(yintercept = 0, col = "grey", size = 1) +
    geom_bar(stat = "identity") +
    expand_limits(y = max(p$data$val) + max(p$data$val) / ratio) +
    coord_flip() +
    scale_x_continuous(breaks = df$order, labels = rownames(df)) +
    labs(
      title = title,
      x = "",
      y = ""
    ) +
    geom_text(
      aes(color = I("white"), y = y_lab, label = x_lab),
      size = cex * 3.5
    ) +
    # theme_classic() +
    # theme_perso() +
    theme(
      axis.text.y = element_text(size = cex * 10, face = "italic", color = colors),
      axis.text.x = element_text(size = cex * 10, face = "italic", color = "darkgrey"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = cex * 16, face = "bold", color = color_title),
      plot.subtitle = element_text(hjust = 0.5, size = cex * 16, face = "italic"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    # label = round(..y.., dec) %>% paste0("%")
    geom_text(aes(label = round(..y.., dec) %>% paste0("%")), hjust = hjust, vjust = vjust, size = cex * 4, color = colors) +
    theme(legend.position = "none") +
    scale_fill_gradient(low = color_gradient[1], high = color_gradient[2])
}
