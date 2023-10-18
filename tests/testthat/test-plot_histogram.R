x <- rnorm(100)

test_that("histogram default works", {
    expect_s3_class(plot_histogram(x), "ggplot")
})

test_that("histogram advanced works", {
    p <- plot_histogram(
        x,
        title = "Some numerical variable",
        color = "blue",
        color_title = "orange",
        color_stats = "orange",
        width_title = 15,
        cex = 1.2,
        digits = 1,
        binwidth = 0.5
    )
    expect_s3_class(p, "ggplot")
})
