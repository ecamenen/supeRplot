x <- runif(20)
x <- lapply(
    c(1, -1),
    function(i) sapply(seq(10), function(j) x * i + runif(10, max = 1))
) %>%
    Reduce(cbind, .) %>%
    set_colnames(paste("Variable", seq(20)))

test_that("correlate default works", {
    expect_type(correlate(x, method_adjust = "none"), "list")
    res <- correlate(x)
    expect_type(res, "list")
    expect_named(res, c("r", "p"))
    expect_equal(unique(c(dim(res[[1]]), dim(res[[2]]))), 20)
    expect_equal(
        unique(
            c(
                rownames(res[[1]]),
                rownames(res[[2]]),
                colnames(res[[1]]),
                colnames(res[[2]])
            )
        ),
        paste("Variable", seq(20))
    )
    expect_lte(max(res[["r"]]), 1)
    expect_gte(min(res[["r"]]), -1)
    expect_lte(max(res[["p"]]), 1)
    expect_gte(min(res[["p"]]), 0)
})

res <- correlate(
    x,
    method = "pearson",
    method_adjust = "none",
    cutoff = 0.7
)
test_that("correlate advanced works", {
    expect_type(res, "list")
})

test_that("correlation network default works", {
    expect_s3_class(plot_cor_network(x), "visNetwork")
})

test_that("correlation network advanced works", {
    p <- plot_cor_network(
        x,
        colour_edge = c(
            brewer.pal(3, "Pastel1")[3],
            brewer.pal(3, "Pastel1")[1]
        ),
        colour_node = c("white", "black"),
        cex = 1.5,
        method = "pearson",
        method_adjust = "none",
        cutoff = 0.7,
        digits = 1
    )
    expect_s3_class(p, "visNetwork")

    p <- plot_cor_network(
        res,
        is_cor = TRUE
    )
    expect_s3_class(p, "visNetwork")
})
