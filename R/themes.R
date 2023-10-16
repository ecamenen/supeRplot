theme_violin <- function(
    p,
    colors = get_colors(),
    cex = 1,
    cex_main = 15 * cex,
    cex_sub = 13 * cex,
    cex_axis = 17 * cex,
    guide = FALSE,
    grid = FALSE,
    color_title = "black",
    title_center = 0.5,
    x_axis = FALSE,
    color_subtitle = "gray50",
    legend = FALSE) {
    p <- p +
        theme_minimal() +
        theme(
            plot.title = element_text(
                hjust = title_center,
                size = cex_main,
                face = "bold",
                color = color_title
            ),
            plot.subtitle = element_text(
                hjust = title_center,
                size = cex_sub,
                color = "gray50"
            ),
            plot.caption = element_text(
                hjust = 1,
                size = cex * 13,
                color = color_subtitle
            ),
            axis.title.x = element_blank(),
            axis.text.y = element_text(colour = "gray50")
        ) +
        theme_perso(cex, cex_main, cex_sub, cex_axis)
    if (!x_axis) {
        p <- p + theme(axis.title.x = element_blank())
    }
    # if (!isTRUE(guide)) {
    #     p <- p + guides(x = "none")
    # } else {
    p <- p +
        theme(
            axis.text.x = element_text(
                hjust = title_center,
                size = cex * 15,
                color = color_subtitle,
                angle = 45, vjust = 1
            )
        )
    # }
    if (!isTRUE(grid)) {
        p <- p + theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    }
    if (!legend) {
        p <- p +
            guides(
                color = "none",
                fill = "none"
            )
    }
    return(p)
}

theme_perso <- function(
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 15 * cex,
    cex_axis = 10 * cex) {
    theme(
        axis.text = element_text(size = 13 * cex, color = "gray50"),
        axis.title = element_text(face = "bold.italic", size = cex_axis),
        strip.text = element_text(
            size = cex_main,
            face = "bold",
            hjust = 0.5,
            margin = margin(0.5, 0.5, 0.5, 0.5)
        )
    )
}
