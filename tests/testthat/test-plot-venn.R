x <- lapply(
    seq(3),
    function(x) {
        runif(10, 1, 10) %>%
            round() %>%
            letters[.] %>%
            unique() %>%
            paste("Element", .)
    }
) %>%
    set_names(paste("Dataset", LETTERS[seq(3)]))

test_that("venn default works", {
    expect_s3_class(plot_venn(x), "ggplot")
})

test_that("venn advanced works", {
    p <- plot_venn(
        x,
        width_text = 8,
        width_label = 5,
        colour = brewer.pal(3, "Reds"),
        color_gradient = FALSE,
        cex = 1.2,
        cex_main = 1.2 * 6,
        cex_line = 2,
        n_max = 2,
        vjust_label = 0.75,
        ratio = 0.5,
        label = FALSE,
        element = FALSE,
        percent = FALSE
    )
    expect_s3_class(p, "ggplot")
})
