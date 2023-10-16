#' Violin plot
#'
#' Ditribution of one or more variables in the form of violin plot, boxplot and sina plot.
#'
#' @examples
#' library(magrittr)
#' plot_violin(runif(10))
#' data.frame()
#' lapply(seq(2), function(x) runif(10)) %>% as.data.frame() %>% plot_violin()
#' @export
plot_violin <- function(
    x,
    colour = "red",
    lwd = 1,
    cex = 1,
    size = cex,
    cex_main = 21 * cex,
    cex_sub = 15 * cex,
    cex_axis = 17 * cex,
    alpha = 0.3,
    title = NULL,
    wrap_title = 20,
    probs = c(.25, .75),
    coef = 1.5,
    pch_colour = "gray50",
    pch_alpha = 1,
    class = 1,
    value = 1,
    subtitle = FALSE,
    caption = NULL,
    color_title = colour,
    ratio = 5,
    title_center = 0.5,
    lim1 = NULL,
    lim2 = NULL,
    method = "anova",
    ylab = NULL,
    method_adjust = "BH",
    wrap = 20,
    ratio_y = 7,
    digits = 0,
    stat = TRUE) {
    set.seed(1)
    if (is.null(title)) {
        title <- paste0(deparse(substitute(x)))
    }
    if (isFALSE(subtitle)) {
        if (!(class(x) %in% c("data.frame", "tibble"))) {
            subtitle <- paste0(
                print_stats(x, digits = digits),
                ", N=",
                length(na.omit(x))
            )
        } else {
            if (stat) {
                subtitle <- get_melt(x) %>%
                    get(paste0(method, "_test"))(value ~ name) %>%
                    print_mean_test(digits_p = 3)
            } else {
                subtitle <- NULL
            }
        }
    }
    color_subtitle <- colour
    if (!(class(x) %in% c("data.frame", "tibble"))) {
        df <- data.frame(value = x, name = class)
        colour_fill <- colour
        sub_labs <- ""
        guide <- FALSE
    } else {
        df <- get_melt(x)
        # df$name <- factor(df$name, labels = colnames(x)) %>% as.numeric()
        # x <- df$value
        colour_fill <- sort(colnames(x)) %>%
            match(., colnames(x)) %>%
            colour[.]
        colour <- factor(df$name, labels = colour)
        sub_labs <- group_by(df, name) %>%
            dplyr::summarise(
                label = paste0(
                    print_stats(value, digits = digits),
                    ", N=",
                    length(na.omit(value))
                )
            ) %>%
            mutate(
                label = paste(
                    name, label,
                    sep = "\n"
                ) %>%
                    str_wrap(wrap)
            ) %>%
            pull(label)
        sub_labs <- sort(colnames(x)) %>%
            match(colnames(x), .) %>%
            sub_labs[.]
        guide <- TRUE
    }
    colour_fill1 <- factor(df$name, labels = colour_fill)
    colour_fill0 <- colour
    if (is.null(lim1)) {
        lim1 <- min(df$value, na.rm = TRUE)
    }
    if (is.null(lim2)) {
        lim2 <- max(df$value, na.rm = TRUE)
    }
    quant <- group_by(df, name) %>%
        dplyr::summarise(quantile(value, probs, na.rm = TRUE)) %>%
        pull(2)
    quant0 <- group_by(df, name) %>%
        dplyr::summarise(quantile(value, c(0, 1), na.rm = TRUE)) %>%
        pull(2)
    iqr <- quant - c(-1, 1) * (1 - coef) * quant
    iqr_even <- function(x) {
        which(seq(length(iqr)) %% 2 == x)
    }
    for (i in iqr_even(1)) {
        if (iqr[i] < quant0[i]) {
            iqr[i] <- quant0[i]
        }
    }
    for (i in iqr_even(0)) {
        if (iqr[i] > quant0[i]) {
            iqr[i] <- quant0[i]
        }
    }
    get_iqr <- function(x) {
        factor(df$name, labels = iqr[iqr_even(x)]) %>%
            as.character() %>%
            as.numeric()
    }
    p <- ggplot(df, aes(x = as.character(name), y = as.numeric(value))) +
        # expand_limits(x = 1 + 1 / ratio) +
        geom_errorbar(
            width = .1,
            lwd = lwd,
            colour = colour_fill1,
            aes(
                ymin = get_iqr(1),
                ymax = get_iqr(0)
            )
        ) +
        geom_boxplot(
            coef = 0,
            outlier.shape = NA,
            colour = "white",
            aes(fill = colour_fill0),
            lwd = lwd * 0.25
        ) +
        geom_violin(alpha = alpha, aes(fill = colour_fill0), colour = NA) +
        theme_minimal() +
        labs(
            title = str_wrap(title, wrap_title),
            subtitle = subtitle, # str_wrap(subtitle, wrap_subtitle),
            caption = caption,
            y = ylab
        ) +
        scale_fill_manual(values = colour_fill) +
        scale_x_discrete(limits = colnames(x), labels = sub_labs) +
        scale_y_continuous(breaks = pretty_breaks(n = 3)) +
        xlab("")
    if ((class(x) %in% c("data.frame", "tibble")) && ncol(x) > 2 && stat) {
        stats <- dunn_test(
            df,
            value ~ name,
            p.adjust.method = method_adjust
        ) %>%
            filter(p.adj.signif <= 0.05) %>%
            mutate(
                y.position = max(value, na.rm = TRUE) +
                    as.numeric(rownames(.)) *
                        max(value, na.rm = TRUE) /
                        ratio_y
            )
        p <- p + ggpubr::stat_pvalue_manual(
            stats,
            label = "p.adj.signif",
            color = "gray50",
            bracket.size = 0.7,
            size = cex * 6,
            hide.ns = TRUE,
            tip.length = 0
        )
        max_stats <- pull(stats, y.position) %>% max()
        if (lim2 < max_stats) {
            lim2 <- max_stats
        }
    }
    # p <- p + scale_y_continuous(limits = c(lim1, lim2))
    p <- p + geom_sina(
        size = size,
        colour = pch_colour,
        alpha = pch_alpha,
        seed = 1
    )
    theme_violin(
        p,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub,
        cex_axis = cex_axis,
        guide = guide,
        color_title = color_title,
        title_center = title_center,
        color_subtitle = color_subtitle
    ) %>% suppressMessages()
}
