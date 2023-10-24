plot_radar <- function(
    x,
    colour = get_colors(),
    cex = 1,
    n_max = NULL,
    add_percent = FALSE,
    digits = 0,
    n_interval = 2,
    alpha = 0.25) {
    if (!is.null(ncol(x))) {
        x <- t(df)
        n <- ncol(x)
        colors <- colour[seq(n)]
    } else {
        n <- length(x)
        colors <- colour[1]
    }
    if (is.null(n_max)) {
        n_max <- unlist(x) %>%
            as.numeric() %>%
            max(na.rm = TRUE) %>%
            round(digits)
    }
    caxislabels <- seq(0, n_max, length = n_interval + 1) %>%
        .[-1] %>%
        round(digits)
    if (add_percent) {
        caxislabels <- paste0(caxislabels, "%")
    }
    x[is.na(x)] <- 0
    x0 <- rbind(
        rep(n_max, n),
        rep(0, n)
    ) %>%
        set_rownames(c("Max", "Min")) %>%
        set_colnames(colnames(x)) %>%
        rbind(x)

    apply(x0, 2, as.numeric) %>%
        as.data.frame() %>%
        radarchart(
            cglwd = 3,
            cglty = 2,
            cglcol = "gray",
            plty = 1,
            pcol = colors,
            pfcol = alpha(colors, alpha),
            plwd = 4,
            axistype = 1,
            axislabcol = colors[1],
            caxislabels = c("", caxislabels),
            seg = n_interval,
            vlcex = cex * 1.5,
            calcex = cex * 1.25,
            centerzero = TRUE
        )
    if (!is.null(ncol(x))) {
        legend(
            "bottomleft",
            legend = rownames(x),
            bty = "n",
            pch = 15,
            col = colors,
            text.col = "black",
            pt.cex = cex * 2.5,
            cex = cex * 1.5
        )
    }
}
