test_that("piechart default works", {
    x <- c(rep("A", 5), rep("B", 4))
    expect_s3_class(plot_pie(x), "ggplot")
})

test_that("piechart default works", {
    k <- 10
    n <- runif(k, 1, 10) %>% round()
    x <- paste("Level", seq(k)) %>%
        mapply(function(x, y) rep(x, y), ., n) %>%
        unlist()
    p <- plot_pie(
        x,
        title = "Some random variable",
        width_text = 5,
        width_title = 15,
        colour = brewer.pal(9, "Reds"),
        cex = 20,
        digits = 1,
        hsize = 1.5,
        collapse = TRUE,
        b = 3
    )
    expect_s3_class(p, "ggplot")
})
