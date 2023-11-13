plot_bar_2cat <- function(
    x,
    colour1 = NULL,
    colour2 = "gray50",
    cex = 1,
    cex_main = 21 * cex,
    cex_sub = 15 * cex,
    cex_axis = 21 * cex,
    method = "chisq",
    method_adjust = "BH",
    wrap = 10,
    ratio = 7,
    stats = FALSE,
    title = NULL) {
    df0 <- set_colnames(x, c("var1", "var2"))
    if (is.null(colour1)) {
        colour1 <- get_colors()[seq(unique(pull(x, 1)))]
    }
    if (is.null(colour2)) {
        colour2 <- get_colors()[seq(unique(pull(x, 1))) + 9]
    }
    counts <- data.frame(table(df0)) %>%
        mutate(label = ifelse(Freq > 0, Freq, ""))

    max_val <- group_by(df0, var2) %>%
        summarise(label = length(var2)) %>%
        pull(label) %>%
        max(na.rm = TRUE)
    if (stats) {
        stats <- post_hoc_chi2(
            x,
            method = method,
            method_adjust = method_adjust
        ) %>%
            filter(p <= 0.05) %>%
            mutate(
                var1 = df0$var1[1],
                y.position = max_val +
                    (as.numeric(rownames(.)) * max_val / ratio)
            )
    }

    p <- ggplot(data = counts, aes(x = var2, y = Freq, fill = var1)) +
        geom_bar(stat = "identity", position = "stack") +
        xlab(to_title(colnames(x)[2])) +
        ylab("Count") +
        scale_fill_manual(values = colour1, name = to_title(colnames(x)[1])) +
        geom_text(
            aes(label = label, y = Freq),
            position = position_stack(vjust = 0.5),
            colour = I("white"),
            size = cex * 7
        ) +
        scale_y_continuous(breaks = pretty_breaks(n = 3)) +
        # scale_x_discrete(
        #   labels = group_by(df0, var2) %>%
        #     summarise(label = length(var2)) %>%
        #     mutate(label = paste0(var2, " (N = ", label, ")")) %>%
        #     pull(label) %>%
        #     str_wrap(wrap)
        # ) +
        labs(
            subtitle = table(df0) %>%
                get(paste0(method, "_test"))() %>%
                print_chi2_test()
        )
    if (!is.null(title)) {
        p <- p + labs(title = str_wrap(title, wrap))
    }
    if (stats) {
        p <- p + ggpubr::stat_pvalue_manual(
            stats,
            label = " ",
            color = colour2,
            bracket.size = 0.7,
            size = cex * 6,
            hide.ns = TRUE,
            tip.length = 0
        )
    }
    theme_violin1(
        p,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub,
        cex_axis = cex_axis,
        guide = TRUE,
        color_subtitle = colour2,
        x_axis = TRUE,
        legend = TRUE,
    ) +
        theme(
            axis.text.y = element_text(
                size = cex * 15,
                color = colour2
            )
        ) +
        theme(
            legend.title = element_text(face = "bold.italic", size = cex_axis),
            legend.text = element_text(colour = colour2, size = cex * 15)
        )
}
