test_that("bar_mcat default works", {
    df <- sapply(seq(10), function(x) runif(10) %>% round()) %>% as.data.frame()
    colnames(df) <- paste("Level", seq(10))
    expect_s3_class(plot_bar_mcat(df), "ggplot")
})

test_that("bar_mcat advanced works", {
    df <- lapply(seq(2), function(x) runif(10))
    df <- as.data.frame(df)
    df[, 3] <- runif(10, 1, 2)
    colnames(df) <- paste0("X", seq(3))
    p <- plot_bar_mcat(
        df,
        sample_size = 15,
        title = "Some categorical variable",
        width_text = 30,
        width_title = 50,
        colour = brewer.pal(9, "Reds"),
        color_title = "red",
        cex = 8,
        digits = 1,
        collapse = TRUE,
        ratio = 2,
        n_collapse = 3,
        n_max = 4,
        hjust_title = 1
    )
    expect_s3_class(p, "ggplot")
})
