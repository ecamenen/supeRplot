df <- sapply(
    seq(2),
    function(x) {
        runif(10, 1, 100) %>%
            set_names(paste("Variable", letters[seq(10)]))
    }
) %>% set_colnames(paste("Sample", LETTERS[seq(2)]))

test_that("radar default works", {
    expect_null(plot_radar(df[, 1]))
})

test_that("radar error", {
    expect_output(
        plot_radar(df[1, ]),
        "The number of variables must be 3 or more."
    )
})

test_that("radar advanced works", {
    p <- plot_radar(
        df,
        colour = c("orange", "forestgreen"),
        cex = 0.9,
        n_max = 100,
        digits = 1,
        n_interval = 4,
        alpha = 0.5
    )
    expect_type(p, "list")
})
