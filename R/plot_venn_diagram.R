plot_venn_diagram <- function(
    x,
    width_text = 30,
    width_label = 30,
    colour = get_colors(),
    cex = 1,
    cex_main = cex * 6,
    n_max = 4,
    vjust_label = 0.5,
    ratio = 0.1,
    label = TRUE,
    percent = TRUE,
    element = TRUE) {
    data <- x %>%
        set_names(names(.) %>% str_wrap(width_label)) %>%
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
    if (element) {
        i <- data@region$item %>% list.which(length(.) <= n_max & length(.) > 0)
        data@region$label[unlist(i)] <- data@region$item %>%
            list.search(length(.) <= n_max & length(.) > 0) %>%
            list.mapv(str_trunc2(., width_text) %>% paste(., collapse = "\n"))
    }
    p <- ggplot() +
        # geom_sf(aes(fill = count), data = venn_region(data)) +
        geom_sf(
            aes(colour = id),
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
        scale_color_manual(values = colour) +
        theme(
            legend.title = element_text(face = "bold.italic", size = cex * 13),
            legend.text = element_text(size = cex * 9)
        ) +
        scale_x_continuous(expand = expansion(mult = ratio))

    if (label) {
        p <- p + geom_sf_text(
            aes(label = name),
            colour = colour[seq(length(x))],
            vjust = vjust_label,
            data = venn_setlabel(data),
            size = cex_main
        )
    }
    p$labels$fill <- "N"
    return(p)
}
