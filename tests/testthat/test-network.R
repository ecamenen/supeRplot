x <- x0 <- sapply(seq(5), function(j) runif(5))
x[x < 0.5] <- diag(x) <- 0
colnames(x) <- rownames(x) <- paste("Variable", seq(5))
x[lower.tri(x)] <- t(x)[lower.tri(x)]

test_that("edge default works", {
    e <- Edge(x)
    expect_s3_class(e, "Edge")
    expect_named(e, c("from", "to", "weight", "label", "title"))
})

e <- Edge(x, x, 2)
test_that("edge advanced works", {
    expect_s3_class(e, "Edge")
    expect_s3_class(e, "data.frame")
    expect_named(e, c("from", "to", "weight", "p", "label", "title"))
    expect_type(e$weight, "double")
    expect_type(e$p, "double")
    expect_type(e$label, "double")
    expect_type(e$title, "double")
    expect_match(e$from, "Variable \\d{1,2}")
    expect_match(e$to, "Variable \\d{1,2}")
    expect_gt(min(e$weight), 0)
    expect_setequal(e$weight, e$p)
    expect_setequal(e$label, e$title)
})

test_that("edge fails", {
    expect_error(Edge(x0))
})

n <- Node(e, 12)

test_that("node works", {
    expect_s3_class(n, "Node")
    expect_named(n, c("id", "size"))
    expect_match(n$id, "Variable \\d{1,2}")
    expect_type(n$size, "double")
    expect_equal(max(n$size), 12)
    expect_gt(min(n$size), 0)
})

test_that("network default works", {
    expect_null(plot_network(x))
})

test_that("network advanced works", {
    p <- plot_network(
        title = "Gimme a network",
        cex = 1.5,
        color = c("white", "black"),
        shape = "square",
        dashed = FALSE,
        edge = e,
        node = n,
        dist = 6,
        label = TRUE
    )
    expect_null(p)
})

test_that("network fails", {
    expect_error(plot_network(x0, edge = x))
    expect_error(plot_network(x0, node = x))
})


test_that("dynamic network default works", {
    expect_s3_class(plot_network_dyn(x), "visNetwork")
})

test_that("dynamic network advanced works", {
    p <- plot_network_dyn(
        title = "Gimme a network",
        cex = 1.5,
        color = c("white", "black"),
        shape = "square",
        dashed = FALSE,
        edge = e,
        node = n
    )
    expect_s3_class(p, "visNetwork")
})

test_that("dynamic network fails", {
    expect_error(plot_network_dyn(x0, edge = x))
    expect_error(plot_network_dyn(x0, node = x))
})
