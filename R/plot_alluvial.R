plot_alluvial <- function(
    x,
    col_stratum = NULL,
    col_alluvium = NULL,
    label_stratum = NULL,
    label = NULL) {
    df <- sapply(x, function(x) as.character(x)) %>% as.data.frame()
    n <- unlist(df) %>%
        unique() %>%
        na.omit() %>%
        seq()
    df[is.na(df)] <- "zz"
    df0 <- df %>%
        mutate(id = rownames(.)) %>%
        pivot_longer(-id)

    label_stratum <- df %>%
        select(sort(colnames(.))) %>%
        lapply(
            function(i) {
                unique(i) %>%
                    sort(decreasing = TRUE) %>%
                    str_replace_all("zz", "NA")
            }
        ) %>%
        unlist()
    ggplot(
        df0,
        aes(
            x = name,
            stratum = value,
            alluvium = id,
            fill = value,
            label = value
        )
    ) +
        geom_flow() +
        geom_stratum(width = 0.5, color = "grey") +
        geom_text(
            stat = "stratum",
            color = "white",
            label = label_stratum,
            size = 8
        ) +
        scale_x_discrete(expand = c(.05, .05)) +
        scale_fill_manual(values = c(get_colors()[n], "gray")) +
        # scale_y_continuous(
        #     breaks = seq(0, 40, 5),
        #     minor_breaks = seq(36),
        #     sec.axis = sec_axis(trans = ~., name = "n", breaks = seq(0, 40, 5))
        # ) +
        labs(y = "n") +
        theme_minimal() %>%
        theme_custom(cex = 1.5, show_axis = FALSE) +
      theme(
        legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank()
      )
}
