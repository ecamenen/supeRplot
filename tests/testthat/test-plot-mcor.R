x <- sapply(seq(20), function(x) runif(20)) %>%
  set_colnames(paste("Variable", seq(20)))

test_that("mcor default works", {
  expect_type(plot_mcor(x), "list")
})

test_that("mcor advanced works", {
  p <- plot_mcor(
    x,
    colour = c("black", brewer.pal(n = 6, name = "RdBu"), 1),
    method = "spearman",
    method_adjust = "none",
    cex = 0.8
  )
  expect_type(p, "list")
})
