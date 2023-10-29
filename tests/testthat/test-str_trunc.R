x <- "hi there, I'm a sentence to format."

test_that("to_title works", {
    res <- to_title(x)
    expect_identical(res, paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
})

test_that("str_trunc1 works", {
    res <- str_trunc1(x)
    expect_identical(res, "hi there, I'm a")
})

test_that("str_trunc0 works", {
    res <- str_trunc0(x)
    expect_identical(res, "hi there, I'm a sentence")
})

test_that("str_pretty works", {
    res <- str_pretty(x)
    expect_identical(res, "Hi there, I'm a...")
})
