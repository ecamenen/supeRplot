plot_radar <- function(x, maxmin = FALSE, perc = TRUE, dec = 0, seg = 2, col_alpha = 0.25) {
  if (maxmin) {
    v_max <- 100
    caxislabels <- c("50%", "100%")
  } else {
    v_max <- unlist(x) %>% as.numeric() %>% max(na.rm = TRUE) %>% round(dec)
    caxislabels <- seq(0, v_max, length = seg + 1) %>% .[-1] %>% round(dec)
    if (perc) {
      caxislabels <- paste0(caxislabels, "%")
    }
  }
  x[is.na(x)] <- 0
  x0 <- rbind(
    rep(v_max, ncol(x)),
    rep(0, ncol(x))
  ) %>%
    set_rownames(c("Max", "Min")) %>%
    set_colnames(colnames(x)) %>%
    rbind(x)

  apply(x0, 2, as.numeric) %>%
    as.data.frame() %>%
    radarchart(
      cglwd = 2,
      cglty = 5,
      cglcol = "gray",
      plty = 1,
      pcol = get_colors1()[seq(nrow(x))],
      pfcol = get_colors1()[seq(nrow(x))] %>% alpha(col_alpha),
      plwd = 4,
      axistype = 1,
      axislabcol = "black",
      caxislabels = c("", caxislabels),
      seg = seg,
      vlcex = 1.5,
      calcex = 1.25,
      centerzero = TRUE
    )
  legend(
    "bottomleft",
    legend = rownames(x),
    bty = "n",
    pch = 20,
    col = get_colors1()[seq(nrow(x))],
    text.col = "black",
    pt.cex = 2
  )
}
