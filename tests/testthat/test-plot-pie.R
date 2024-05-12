x <- c(rep("A", 5), rep("B", 4))

test_that("piechart default works", {
    expect_s3_class(plot_pie(x), "ggplot")
})

test_that("piechart advanced works", {
    p <- plot_pie(
        x,
        label = TRUE,
        legend = FALSE,
        sort = FALSE,
        percent = FALSE
    )
    expect_s3_class(p, "ggplot")
    p <- plot_pie(
        x,
        sort = c("A", "B"),
        legend = paste("Level", seq(2)),
        sample_size = 11,
        threshold = 20
    )
    expect_s3_class(p, "ggplot")

    k <- 10
    n <- runif(k, 1, 10) %>% round()
    x <- paste("Level", seq(k)) %>%
        mapply(function(x, y) rep(x, y), ., n) %>%
        unlist()
    p <- plot_pie(
        x,
        title = "Some categorical variable",
        width_text = 5,
        width_title = 20,
        colour = brewer.pal(9, "Reds"),
        cex = 20,
        digits = 1,
        hsize = 1.5,
        collapse = TRUE,
        b = 3
    )
    expect_s3_class(p, "ggplot")
})
