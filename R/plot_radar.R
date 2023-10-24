plot_radar <- function(
    x,
    colour = get_colors(),
    maxmin = FALSE,
    perc = TRUE,
    dec = 0,
    seg = 2,
    col_alpha = 0.25) {
  if (!is.null(ncol(x))) {
      x <- t(df)
      n <- ncol(x)
      colors <- colour[seq(n)]
    } else {
      n <- length(x)
      colors <- colour[1]
    }
    if (maxmin) {
        v_max <- 100
        caxislabels <- c("50%", "100%")
    } else {
        v_max <- unlist(x) %>%
            as.numeric() %>%
            max(na.rm = TRUE) %>%
            round(dec)
        caxislabels <- seq(0, v_max, length = seg + 1) %>%
            .[-1] %>%
            round(dec)
        if (perc) {
            caxislabels <- paste0(caxislabels, "%")
        }
    }
    x[is.na(x)] <- 0
    x0 <- rbind(
        rep(v_max, n),
        rep(0, n)
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
            pcol = colors,
            pfcol = alpha(colors, col_alpha),
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
        col = colors,
        text.col = "black",
        pt.cex = 2
    )
}
