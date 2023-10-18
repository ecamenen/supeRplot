test_that("violin default works", {
    x <- runif(10)
    expect_s3_class(plot_violin(x), "ggplot")
})

test_that("violin advanced works", {
    df <- lapply(seq(2), function(x) runif(10))
    df <- as.data.frame(df)
    df[, 3] <- runif(10, 1, 2)
    colnames(df) <- paste0("X", seq(3))
    p <- plot_violin(
        df,
        title = "Some numerical variables",
        color_title = brewer.pal(9, "Set1")[5],
        ylab = "Y-values",
        colour = brewer.pal(9, "Set1")[seq(3)],
        method = "kruskal",
        method_adjust = "none",
        cex = 1.2,
        pch_size = 3,
        width_text = 5,
        pch_colour = "gray30",
        pch_alpha = 0.5,
        width_title = 30,
        lwd = 1.25,
        digits = 2
    )
    expect_s3_class(p, "ggplot")
})
