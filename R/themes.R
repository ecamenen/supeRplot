theme_violin <- function(
    p,
    colors = palette_discrete(),
    cex = 1,
    cex_main = 15 * cex,
    cex_sub = 13 * cex,
    cex_axis = 17 * cex,
    guide = FALSE,
    grid = FALSE,
    color_title = "black",
    hjust = 0.5,
    x_axis = FALSE,
    color_subtitle = "gray50",
    legend = FALSE) {
    p <- p +
        theme_minimal() +
        theme(
            plot.title = element_text(
                hjust = hjust,
                size = cex_main,
                face = "bold",
                color = color_title
            ),
            plot.subtitle = element_text(
                hjust = hjust,
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
        theme_custom(cex, cex_main, cex_sub, cex_axis)
    if (!x_axis) {
        p <- p + theme(axis.title.x = element_blank())
    }
    p <- p +
        theme(
            axis.text.x = element_text(
                hjust = hjust,
                size = cex * 15,
                color = color_subtitle,
                angle = 45, vjust = 1
            )
        )
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

theme_custom <- function(
    cex = 1,
    cex_main = 17 * cex,
    cex_sub = 15 * cex,
    cex_axis = 15 * cex) {
    theme(
        axis.text = element_text(size = 13 * cex, color = "gray50"),
        axis.title = element_text(face = "bold.italic", size = cex_axis),
        strip.text = element_text(
            size = cex_main,
            face = "bold",
            hjust = 0.5,
            margin = margin(0.5, 0.5, 0.5, 0.5)
        ),
        plot.title = element_text(face = "bold", size = 22 * cex, hjust = 0.5),
        legend.title = element_text(face = "italic", size = 12 * cex),
        legend.text = element_text(colour = "black", size = 10 * cex)
    )
}


theme_bar <- function(
    p,
    y = NULL,
    colors = c(
        brewer.pal(n = 9, name = "Set1")[-6],
        brewer.pal(n = 9, name = "Pastel2")
    ),
    cat = TRUE) {
    p <- p +
        theme_minimal() +
        theme_custom() +
        theme(legend.position = "none") +
        labs(x = "", y = "")
    if (cat) {
        p + scale_fill_manual(p, values = colors, na.value = "black")
    } else {
        p + scale_fill_gradientn(colors = colors, na.value = "black")
    }
}

theme_histogram <- function(
    p,
    colors = palette_discrete(),
    cex = 1,
    cex_main = 15 * cex,
    cex_sub = 13 * cex,
    cex_axis = 17 * cex,
    guide = FALSE,
    grid = FALSE,
    color_title = "black",
    title_center = 0.5,
    x_axis = FALSE,
    color_subtitle = "gray50") {
    p <- p +
        ylab("Count") +
        theme_minimal() +
        guides(
            color = "none",
            fill = "none"
        ) +
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
                size = cex * 11,
                color = "gray"
            ),
            axis.title.x = element_blank(),
            axis.text.y = element_text(colour = "gray50")
        ) +
        theme_custom(cex, cex_main, cex_sub, cex_axis)
    if (!isTRUE(guide)) {
        p <- p + guides(x = "none")
    } else {
        p <- p +
            theme(
                axis.text.x = element_text(
                    size = cex * 13,
                    color = color_subtitle,
                    vjust = 1, hjust = 1
                )
            )
    }
    if (!isTRUE(grid)) {
        p <- p + theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    }
    return(p)
}
