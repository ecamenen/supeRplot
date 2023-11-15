plot_bar_2cat <- function(
    x,
    colour1 = NULL,
    colour2 = "gray50",
    colour3 = "white",
    cex = 1,
    cex_main = 21 * cex,
    cex_sub = 15 * cex,
    cex_axis = 21 * cex,
    method = "chisq",
    method_adjust = "BH",
    width_title = 10,
    width_legend = 10,
    ratio = 7,
    stats = TRUE,
    title = NULL,
    sort = FALSE,
    width_text = 30,
    threshold = 0) {
    x <- as.data.frame(x)
    df0 <- set_colnames(x, c("var1", "var2"))
    df0$var1 <- str_wrap(df0$var1, width =  width_text) %>% factor(levels = unique(.))
    df0$var2 <- str_wrap(df0$var2, width =  width_text) %>% factor(levels = unique(.))
    if (is.null(colour1)) {
        colour1 <- palette_discrete()[seq(unique(pull(x, 1)))]
    }
    counts <- data.frame(table(df0, useNA = "ifany")) %>%
        mutate(label = ifelse(Freq > threshold, Freq, ""))

    max_val <- group_by(df0, var2) %>%
        summarise(label = length(var2)) %>%
        pull(label) %>%
        max(na.rm = TRUE)
    if (stats) {
        res <- post_hoc_chi2(
            x,
            method = method,
            method_adjust = method_adjust
        ) %>%
            filter(FDR <= 0.05) %>%
            mutate(
                var1 = df0$var1[1],
                y.position = max_val +
                    (as.numeric(rownames(.)) * max_val / ratio)
            )
    }
    if (isFALSE(sort)) {
      counts$var2 <- counts$var2 %>% factor(levels = unique(.))
    }
    p <- ggplot(data = counts, aes(x = var2, y = Freq, fill = var1)) +
        geom_bar(stat = "identity", position = "stack") +
        xlab(to_title(colnames(x)[2])) +
        ylab("Count") +
        scale_fill_manual(
          values = colour1,
          name = to_title(colnames(x)[1]) %>% str_wrap(width_legend),
          na.value = "gray"
          ) +
        geom_text(
            aes(label = label, y = Freq),
            position = position_stack(vjust = 0.5),
            colour = I(colour3),
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
        p <- p + labs(title = str_wrap(title, width_title))
    }
    if (stats && nrow(res) > 0) {
        p <- p + stat_pvalue_manual(
            res,
            label = "fdr.signif",
            color = colour2,
            bracket.size = 0.7,
            size = cex * 6,
            hide.ns = TRUE,
            tip.length = 0
        )
    }
    theme_violin(
        p,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub,
        cex_axis = cex_axis,
        guide = TRUE,
        color_subtitle = colour2,
        x_axis = TRUE,
        legend = TRUE
    ) +
        theme(
            axis.text.y = element_text(size = cex * 15, color = colour2),
            axis.text.x = element_text(hjust = 1, vjust = 1),
            legend.title = element_text(face = "bold.italic", size = cex_axis),
            legend.text = element_text(colour = colour2, size = cex * 15)
        )
}
