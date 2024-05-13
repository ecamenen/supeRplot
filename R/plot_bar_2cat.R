# x <- sapply(
#     list(letters, LETTERS),
#     function(x) runif(10, 1, 4) %>% x[.]
# )
# plot_bar_2cat(x)
#
# x <- cbind(
#   mapply(function(x, y) rep(x, y), letters[seq(3)], c(7, 5, 8)) %>% unlist(),
#   mapply(function(x, y) rep(x, y), LETTERS[seq(3)], c(6, 6, 8)) %>% unlist()
# )

# V1 V2
# 1   a  B
# 2   c  C
# 3   b  A
# 4   c  B
# 5   b  B
# 6   a  C
# 7   a  C
# 8   a  C
# 9   a  C
# 10  c  B
# 11  b  A
# 12  a  C
# 13  c  A
# 14  b  A
# 15  a  C
# 16  b  A
# 17  c  C
# 18  a  B
# 19  c  B
# 20  a  B
# file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
# housetasks <- read.delim(file_path, row.names = 1)
# sub_housetasks <- housetasks[c(1:2, 12:13), c("Wife", "Husband")]
# plot_bar_2cat(t(sub_housetasks), count = TRUE, workspace = 1e6)
#' @export
plot_bar_2cat <- function(
        x,
        colour1 = NULL,
        colour2 = "gray50",
        colour3 = "white",
        cex = 1,
        cex_main = 21 * cex,
        cex_sub = 15 * cex,
        cex_axis = 21 * cex,
        method = "fisher",
        method_adjust = "BH",
        width_title = 10,
        width_legend = 10,
        ratio = 7,
        stats = TRUE,
        title = NULL,
        sort = FALSE,
        width_text = 30,
        threshold = 0,
        legend = NULL,
        count = FALSE,
        ...
) {
    x <- as.data.frame(x)
    if (count) {
        tmp <- x %>%
            mutate(Category = rownames(.)) %>%
            gather("var1", "var2", -Category)
        x <- lapply(
            seq(nrow(tmp)),
            function(x) {
                slice(tmp, x) %>%
                    data.frame(pull(., 3) %>% seq()) %>%
                    select(1, 2)
            }
        ) %>%
            Reduce(rbind, .)
    }
    df0 <- set_colnames(x, c("var1", "var2"))
    df0$var2 <- str_wrap(df0$var2, width =  width_text)
    if (isFALSE(sort)) {
        df0$var2 <- df0$var2 %>% factor(levels = unique(.))
    } else {
        df0$var2 <- factor(df0$var2)
    }
    df0$var1 <- str_wrap(df0$var1, width =  width_text)
    if (is.null(legend)) {
        df0$var1 <- factor(df0$var1)
    } else {
        legend <- str_wrap(legend, width =  width_text)
        stopifnot(legend %in% unique(df0$var1) %>% all())
        df0$var1 <- df0$var1 %>% factor(levels = legend)
    }
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
            method_adjust = method_adjust,
            ...
        ) %>%
            filter(FDR <= 0.05) %>%
            mutate(
                var1 = df0$var1[1],
                y.position = max_val +
                    (as.numeric(rownames(.)) * max_val / ratio)
            )
    }
    # if (isFALSE(sort)) {
    #   counts$var2 <- counts$var2 %>% factor(levels = unique(.))
    # }
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
            size = cex * 6
        ) +
        scale_y_continuous(breaks = pretty_breaks(n = 3)) # +
        # scale_x_discrete(
        #   labels = group_by(df0, var2) %>%
        #     summarise(label = length(var2)) %>%
        #     mutate(label = paste0(var2, " (N = ", label, ")")) %>%
        #     pull(label) %>%
        #     str_wrap(width_legend)
        # )
    if (!is.null(title)) {
        p <- p + labs(title = str_wrap(title, width_title))
    }
    if(stats) {
        p <- p +
            labs(
                subtitle = table(df0) %>%
                    get(paste0(method, "_test"))(...) %>%
                    print_chi2_test()
            )
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
