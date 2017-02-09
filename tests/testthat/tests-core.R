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

context("Test behavior of 'prediction' class methods")
test_that("Test print()", {
    expect_true(inherits(print(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars)), "data.frame"), 
                label = "print() works")
})
test_that("Test head() and tail()", {
    expect_true(inherits(head(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars)), "data.frame"), 
                label = "head() works")
    expect_true(inherits(tail(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars)), "data.frame"), 
                label = "tail() works")
})


context("Test `build_data_list()` behavior")
test_that("Test build_datalist()", {
    expect_true(inherits(build_datalist(mtcars, at = list(cyl = 4)), "list"), label = "build_datalist() works")
})
