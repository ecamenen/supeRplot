plot_venn_diagram <- function(
    x,
    n = 4,
    color = get_colors()[-6],
    cex = 1,
    vjust = 0.5,
    label = TRUE,
    ratio = 0.1,
    wrap = 30,
    wrap_title = 30,
    percent = TRUE,
    item = TRUE,
    cex_main = cex * 6) {
    data <- x %>%
        set_names(names(.) %>% str_wrap(wrap_title)) %>%
        Venn() %>%
        process_data()
    data@region <- data@region %>%
        mutate(
            percent = (count * 100 / sum(count)) %>%
                round(digits = 0) %>%
                paste0("%")
        ) %>%
        mutate(
            label = paste0("(", percent, ")") %>%
                paste(count, ., sep = "\n")
        )
    data@region$label[data@region$item %>% list.which(length(.) == 0)] <- ""
    if (item) {
        i <- data@region$item %>% list.which(length(.) <= n & length(.) > 0)
        data@region$label[unlist(i)] <- data@region$item %>%
            list.search(length(.) <= n & length(.) > 0) %>%
            list.mapv(str_trunc2(., wrap) %>% paste(., collapse = "\n"))
    }
    p <- ggplot() +
        # geom_sf(aes(fill = count), data = venn_region(data)) +
        geom_sf(
            aes(color = id),
            data = venn_setedge(data),
            show.legend = FALSE,
            lwd = 1.5
        ) +
        geom_sf_label(
            aes(label = to_title(label)),
            data = venn_region(data),
            alpha = 0.5,
            label.size = NA,
            size = cex * 4
        ) +
        theme_void() +
        # scale_fill_gradient(low = "white", high = "red") +
        scale_color_manual(values = color) +
        theme(
            legend.title = element_text(face = "bold.italic", size = cex * 13),
            legend.text = element_text(size = cex * 9)
        ) +
        scale_x_continuous(expand = expansion(mult = ratio))

    if (label) {
        p <- p + geom_sf_text(
            aes(label = name),
            color = color[seq(length(x))],
            vjust = vjust,
            data = venn_setlabel(data),
            size = cex_main
        )
    }
    p$labels$fill <- "N"
    return(p)
}
