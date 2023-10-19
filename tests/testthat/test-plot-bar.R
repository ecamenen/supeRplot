x <- runif(10, 1, 10) %>%
    set_names(paste("Sample", LETTERS[seq(10)]))

test_that("bar default works", {
    expect_s3_class(plot_bar(x), "ggplot")
})

test_that("bar advanced works", {
    p <- plot_bar(
        x = x,
        title = "Some numerical variable",
        width_title = 30,
        colour = c("yellow", "gray", "red"),
        color_title = "blue",
        cex = 1.2,
        digits = 1,
        n_max = 5,
        ratio = 15,
        hjust_title = 1
    )
    expect_s3_class(p, "ggplot")
})
