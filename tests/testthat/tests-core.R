# set comparison tolerance
tol <- 0.0001

library("datasets")

context("Test `prediction()` behavior")
test_that("Test prediction()", {
    expect_true(inherits(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars), "data.frame"), label = "prediction() works w data arg (LM)")
    expect_true(inherits(prediction(glm(mpg ~ cyl, data = mtcars), data = mtcars), "data.frame"), label = "prediction() works w data arg (GLM)")
    expect_true(inherits(prediction(lm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "prediction() works w/o data arg (LM)")
    expect_true(inherits(prediction(glm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "prediction() works w/o data arg (GLM)")
})
test_that("Test prediction(at = )", {
    m <- lm(mpg ~ cyl, data = mtcars)
    p1 <- prediction(m, at = list(cyl = 4))
    expect_true(inherits(p1, "data.frame"), label = "prediction(at = list(cyl = 4)) works")
    expect_true(nrow(p1) == nrow(mtcars), label = "prediction(at = list(cyl = 4)) works")
    
    p2 <- prediction(m, at = list(cyl = c(4, 6)))
    expect_true(inherits(p2, "data.frame"), label = "prediction(at = list(cyl = c(4, 6))) works")
    expect_true(nrow(p2) == 2*nrow(mtcars), label = "prediction(at = list(cyl = c(4, 6))) works")
    
    p3 <- prediction(m, at = list(cyl = c(4, 6), wt = 2:3))
    expect_true(inherits(p3, "data.frame"), label = "prediction(at = list(cyl = c(4, 6), wt = 2:3)) works")
    expect_true(nrow(p3) == 4*nrow(mtcars), label = "prediction(at = list(cyl = c(4, 6), wt = 2:3)) works")
    
    mtcars$cyl <- factor(mtcars$cyl)
    expect_error(prediction(m, at = list(cyl = 3)), label = "prediction(at = list(cyl = 3)) errors")
})

context("Test behavior of 'prediction' class methods")
test_that("Test print()", {
    expect_true(inherits(print(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars)), "data.frame"), 
                label = "print() works with numeric outcome")
})
test_that("Test head() and tail()", {
    expect_true(inherits(head(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars)), "data.frame"), 
                label = "head() works")
    expect_true(inherits(tail(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars)), "data.frame"), 
                label = "tail() works")
})

context("Test utilities")
test_that("Test seq_range()", {
    expect_true(identical(range(mtcars$wt), seq_range(mtcars$wt, 2)), label = "seq_range() is correct")
    expect_true(length(seq_range(mtcars$wt, 5)) == 5, label = "seq_range() length is correct")
})

test_that("Test mean_or_mode()/median_or_mode()", {
    expect_true(mean_or_mode(mtcars$wt) == mean(mtcars$wt), label = "mean_or_mode.numeric() is correct")
    expect_true(median_or_mode(mtcars$wt) == median(mtcars$wt), label = "median_or_mode.numeric() is correct")

    mtcars$cyl <- factor(mtcars$cyl)
    expect_true(mean_or_mode(mtcars$cyl) == 8, label = "mean_or_mode.default() is correct")
    expect_true(median_or_mode(mtcars$cyl) == 8, label = "mean_or_mode.default() is correct")
})
