#' Violin plot
#'
#' Visualize the distribution of single or multiple variables using violin
#' plots, boxplots, and sina plots
#'
#' @param x Vector or data.frame of numerical values visualized on the plot.
#' @param method Character for the test method ('anova', 'kruskal', or
#' 'wilcox').
#' @param method_adjust Character for the multiple correction test among
#' `r paste0("'", paste0(sort(p.adjust.methods), collapse = "', '"), "'")`
#' @param title Character for the title.
#' @param width_text Integer for the maximum length of the subtitle(s).
#' @param width_title Integer for the maximum length of the title.
#' @param color_title Color for the title.
#' @param colour Color or vector of colors for the violin and boxplot.
#' @param alpha Integer for the transparency of the violin plot (ranging from 0
#' to 1 for maximum opacity).
#' @param pch_alpha Integer for the transparency of the points (ranging from 0
#' to 1 for maximum opacity).
#' @param pch_colour Color for the sina points.
#' @param pch_size Integer for the magnification factor for the points relative
#' to the default.
#' @param cex Integer for the magnification factor for the text relative to the
#' default.
#' @param cex_axis Integer for the magnification factor for the axis labels
#' relative to the default.
#' @param cex_main Integer for the magnification factor for the subtitles
#' relative to the default.
#' @param cex_sub Integer for the magnification factor for the main title
#' relative to the default.
#' @param stats Boolean to display the results of statistical tests.
#' @param digits Integer for the number of decimals.
#' @param alpha Integer for the transparency of the violin plot
#' (ranging from 0 to 1 for maximum opacity)
#' @param coef Integer to multiply the quantiles by.
#' @param hjust Integer for the horizontal justification (in \[0,1\]).
#' @param lwd Integer for the line width.
#' @param probs Integer vector of probabilities (in \[0,1\]).
#' @param subtitle Character for the subtitle.
#' @param ylab Character for the title of the Y-axis.
#'
#' @examples
#' library(RColorBrewer)
#'
#' # Default plot
#' x <- runif(10)
#' plot_violin(x)
#'
#' # Advanced parameters
#' df <- lapply(seq(2), function(x) runif(10))
#' df <- as.data.frame(df)
#' df[, 3] <- runif(10, 1, 2)
#' colnames(df) <- paste0("X", seq(3))
#' plot_violin(
#'     df,
#'     title = "Some random variables",
#'     color_title = brewer.pal(9, "Set1")[5],
#'     ylab = "Y-values",
#'     colour = brewer.pal(9, "Set1")[seq(3)],
#'     method = "kruskal",
#'     method_adjust = "none",
#'     cex = 1.2,
#'     pch_size = 3,
#'     width_text = 5,
#'     pch_colour = "gray30",
#'     pch_alpha = 0.5,
#'     width_title = 30,
#'     lwd = 1.25,
#'     digits = 2
#' )
#'
#' @return A ggplot object.
#' @export
plot_violin <- function(
    x,
    method = "anova",
    method_adjust = "BH",
    title = NULL,
    width_text = 20,
    width_title = 20,
    colour = "red",
    color_title = colour,
    pch_alpha = 1,
    pch_colour = "gray50",
    pch_size = cex,
    cex = 1,
    cex_axis = 17 * cex,
    cex_main = 21 * cex,
    cex_sub = 15 * cex,
    stats = TRUE,
    digits = 0,
    alpha = 0.3,
    coef = 1.5,
    hjust = 0.5,
    lwd = 1,
    probs = c(.25, .75),
    subtitle = FALSE,
    ylab = NULL) {
    if (is.null(title)) {
        title <- paste0(deparse(substitute(x)))
    }
    value <- 1
    if (isFALSE(subtitle)) {
        if (!(class(x) %in% c("data.frame", "tibble"))) {
            subtitle <- paste0(
                print_stats(x, digits = digits),
                ", N=",
                length(na.omit(x))
            )
        } else {
            if (stats) {
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
        df <- data.frame(value = x, name = 1)
        colour_fill <- colour
        sub_labs <- ""
        guide <- FALSE
    } else {
        df <- get_melt(x)
        colour_fill <- sort(colnames(x)) %>%
            match(., colnames(x)) %>%
            colour[.]
        colour <- factor(df$name, labels = colour)
        sub_labs <- group_by(df, name) %>%
            reframe(
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
                    str_wrap(width_text)
            ) %>%
            pull(label)
        sub_labs <- sort(colnames(x)) %>%
            match(colnames(x), .) %>%
            sub_labs[.]
        guide <- TRUE
    }
    colour_fill1 <- factor(df$name, labels = colour_fill)
    colour_fill0 <- colour
    quant <- group_by(df, name) %>%
        reframe(quantile(value, probs, na.rm = TRUE)) %>%
        pull(2)
    quant0 <- group_by(df, name) %>%
        reframe(quantile(value, c(0, 1), na.rm = TRUE)) %>%
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
            title = str_wrap(title, width_title),
            subtitle = subtitle, # str_wrap(subtitle, wrap_subtitle),
            y = ylab
        ) +
        scale_fill_manual(values = colour_fill) +
        scale_x_discrete(limits = colnames(x), labels = sub_labs) +
        scale_y_continuous(breaks = pretty_breaks(n = 3)) +
        xlab("")
    if ((class(x) %in% c("data.frame", "tibble")) && ncol(x) > 2 && stats) {
        post_hoc <- dunn_test(
            df,
            value ~ name,
            p.adjust.method = method_adjust
        ) %>%
            filter(p.adj.signif <= 0.05) %>%
            mutate(
                y.position = max(value, na.rm = TRUE) +
                    as.numeric(rownames(.)) *
                        max(value, na.rm = TRUE) / 7
            )
        p <- p + ggpubr::stat_pvalue_manual(
            post_hoc,
            label = "p.adj.signif",
            color = "gray50",
            bracket.size = lwd * 0.7,
            size = cex * 6,
            hide.ns = TRUE,
            tip.length = 0
        )
        max_stats <- pull(post_hoc, y.position) %>% max()
    }
    p <- p + geom_sina(
        size = pch_size,
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
        hjust = hjust,
        color_subtitle = color_subtitle
    ) %>% suppressWarnings()
}
