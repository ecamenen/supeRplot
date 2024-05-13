test_that("bar_2cat default works", {
    x <- cbind(
        mapply(function(x, y) rep(x, y), letters[seq(3)], c(7, 5, 8)) %>% unlist(),
        mapply(function(x, y) rep(x, y), LETTERS[seq(3)], c(6, 6, 8)) %>% unlist()
    )
    expect_s3_class(plot_bar_2cat(x), "ggplot")
})

test_that("bar_2cat advanced works", {
    file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
    housetasks <- read.delim(file_path, row.names = 1)
    sub_housetasks <- housetasks[c(1:2, 12:13), c("Wife", "Husband")]
    p <- plot_bar_2cat(
        t(sub_housetasks),
        colour = palette_discrete()[3:4],
        sort = TRUE,
        threshold = 10,
        count = TRUE,
        workspace = 1e6
    )
    expect_s3_class(p, "ggplot")
})
