plot_bar_mcat <- function(
    x,
    colors = c("blue", "gray", "#cd5b45"),
    hjust = -0.1,
    vjust = 0.5,
    ratio = 5,
    cex = 10,
    title = NULL,
    wrap = 20,
    parse = FALSE,
    collapse = FALSE,
    label = NULL,
    n = Inf,
    n_collapse = 5,
    dec = 0,
    color_title = "black",
    df0 = NULL,
    wrap_title = wrap,
    hjust_title = -0.5
) {
  if (is.null(title)) {
    title <- deparse(substitute(x))
  }
  x0 <- as.data.frame(x)
  df <- count_cat(x0, parse = parse, wrap = wrap, collapse = collapse, label = label) %>% data.frame(., order = as.numeric(rownames(.))) %>% tail(n)
  if (!is.null(df0)) {
    x0 <- df0
  }
  colors <- colorRampPalette(tail(colors, n))(nrow(df))
  x_lab <- (round(df$n, 2) / nrow(x0) * 100) %>%
    round(dec) %>%
    paste0("%")
  df$x_lab <- x_lab # paste0(df$n, "\n   (", x_lab, ")")
  df$y_lab <- df$n / 2
  y_lab0 <- as.character(df$f)
  y_lab0[(str_count(y_lab0, "\\,") + 1) > n_collapse] <- "..."
  # i <- df$y_lab < threshold
  # df$x_lab[i] <- ""
  (ggplot(df, aes(f, n, fill = order, label = n)) +
      geom_bar(stat = "identity") +
      expand_limits(y = max(df$n) + max(df$n) / ratio) +
      coord_flip() +
      scale_x_discrete(labels = y_lab0)
  ) %>% theme_perso_bar(colors = colors, cat = FALSE) +
    geom_text(
      aes(color = I("white"), y = y_lab),
      size = cex
    ) +
    geom_text(aes(label = x_lab, color = colors), data = df, hjust = hjust, vjust = vjust, size = cex) +
    ggtitle(str_wrap(title, wrap_title)) +
    theme(
      plot.title = element_text(hjust = hjust_title, vjust = 0, size = cex * 4, face = "bold", color = color_title),
      axis.text.y = element_text(colour = colors, size = cex * 3),
      axis.text.x = element_text(size = cex * 2.2),
      plot.margin = unit(c(-0, 0, 0, 0.5), "cm"),
      panel.grid.major.y = element_blank()
    )  +
    scale_y_continuous(breaks = breaks_pretty(2), labels = label_number_auto())
}
