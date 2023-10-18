plot_histogram <- function(
    x,
    color = "red",
    color_title = color,
    color_stats = "black",
    title = NULL,
    cex = 1,
    cex_axis = 17 * cex,
    cex_main = 21 * cex,
    cex_sub = 15 * cex,
    digits = 0,
    binwidth = 1.5,
    # dotsize = 1,
    # method = "histodot",
    probs = c(.25, .75),
    subtitle = TRUE,
    ...
) {
  df <- data.frame(x) %>% get_melt()
  quant <- quantile(x, probs, na.rm = TRUE)
  if (is.null(title)) {
    title <- deparse(substitute(x))
  }
  if (!is.null(subtitle)) {
    subtitle <- paste0(print_stats(df$value, digits = digits), ", N=", length(na.omit(df$value)))
  }
  p <- gghistogram(
    df,
    x = "value",
    color = NA,
    fill = "name",
    palette = color,
    binwidth = binwidth,
    add = "median",
    rug = TRUE,
    add.params = list(linetype = "solid", size = 1),
    ...
  ) +
    geom_density(
      lwd = cex * 1.1,
      colour = color_stats,
      fill = color,
      alpha = 0.25
      # ) +
      # geom_dotplot(
      #     fill = color,
      #     color = color,
      #     binwidth = binwidth,
      #     dotsize = dotsize,
      #     method = method
    ) +
    geom_vline(xintercept = quant, color = color_stats, lty = 2, lwd = cex * 1.25) +
    labs(subtitle = subtitle, title = title) +
    theme_minimal() +
    scale_x_continuous(expand = c(0.01, 0), labels = function(x) paste0(x)) +
    guides(color = "none", fill = "none")
  theme_histogram(
    p,
                  cex = cex,
                  cex_main = cex_main,
                  cex_sub = cex_sub,
                  cex_axis = cex_axis,
                  guide = TRUE,
                  color_title = color_title
                )
}
