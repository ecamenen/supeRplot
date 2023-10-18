plot_histogram <- function(
    x,
    color = "red",
    dotsize = 1,
    binwidth = 1.5,
    method = "histodot",
    probs = c(.25, .75),
    subtitle = NULL
) {
  df <- data.frame(x) %>% get_melt()
  quant <- quantile(x, probs, na.rm = TRUE)
  if (!is.null(subtitle)) {
    subtitle <- paste0(print_stats0(df$value), ", N=", length(na.omit(df$value)))
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
  ) +
    geom_density(
      lwd = 1,
      colour = color,
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
    geom_vline(xintercept = quant, color = "red", lty = 2) +
    labs(subtitle = subtitle, title = title) +
    theme_minimal() +
    scale_x_continuous(expand = c(0.01, 0), labels = function(x) paste0(x)) +
    guides(color = "none", fill = "none")
  theme_violin1(p, guide = TRUE)
}
