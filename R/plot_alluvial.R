plot_alluvial <- function(df, col_stratum = NULL, col_alluvium = NULL, label_stratum = NULL, label = NULL) {
  if (is.null(col_stratum)) {
    col_stratum <- c(rep(get_colors()[seq(2)], 2))
  }
  if (is.null(col_alluvium)) {
    col_alluvium <- c(rep(c(get_colors()[c(2, 4, 8, 1)], "gray40"), 2))
  }
  if (is.null(label)) {
    label <- str_to_title(colnames(df)[seq(2)])
  }
  df <- count(df, pull(df, 1), pull(df, 2))
  fct_count(pull(df, 1))
  ggplot(df, aes(y = n, axis1 = pull(df, 1), axis2 = pull(df, 2))) +
    geom_alluvium(fill = col_alluvium, width = 0.5, knot.pos = 0) +
    geom_stratum(width = 0.5, fill = col_stratum, color = "grey") +
    geom_text(
      stat = "stratum",
      color = "white",
      aes(label = after_stat(stratum)),
      size = 8
    ) +
    scale_x_discrete(
      limits = label,
      expand = c(.05, .05)
    ) +
    scale_fill_manual(values = get_colors()[seq(2)]) +
    scale_y_continuous(
      breaks = seq(0, 40, 5),
      minor_breaks = seq(36),
      sec.axis = sec_axis(trans = ~., name = "n", breaks = seq(0, 40, 5))
    ) +
    labs(y = "n") +
    theme_minimal() +
    theme(legend.position = "none", axis.ticks.x = element_blank(), panel.grid.major.x = element_blank()) %>%
    theme_perso0(1.5, show_axis = FALSE)
}
