test_that("violin works", {
    # Test the default plot
    x <- runif(10)
    expect_s3_class(plot_violin(x), "ggplot")
    # Test the advanced parameters
    df <- lapply(seq(2), function(x) runif(10))
    df <- as.data.frame(df)
    df[, 3] <- runif(10, 1, 2)
    colnames(df) <- paste0("X", seq(3))
    expect_s3_class(plot_violin(df), "ggplot")
})
