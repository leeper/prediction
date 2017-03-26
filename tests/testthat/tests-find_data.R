library("datasets")

context("Test `find_data()` behavior")

test_that("Test find_data.default()", {
    expect_true(inherits(find_data(lm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "find_data.default() works")
    
    m1 <- lm(mpg ~ cyl, data = mtcars, subset = am == 1)
    expect_true(nrow(find_data(m1)) == nrow(mtcars[mtcars$am == 1, ]), label = "find_data.default(data, subset) works")
    
    mtcars2 <- mtcars
    mtcars2[1:3,] <- NA_real_
    
    m2 <- lm(mpg ~ cyl, data = mtcars2)
    expect_true(nrow(find_data(m2)) == nrow(mtcars2[-c(1:3), ]), label = "find_data.default(data, na.action) works")
    
    m3 <- lm(mpg ~ cyl, data = mtcars2, subset = am == 1)
    expect_true(nrow(find_data(m3)) == nrow(mtcars2[mtcars2$am == 1, ]), label = "find_data.default(data, subset, na.action) works")
    
    expect_error(find_data(StructTS(log10(UKgas), type = "BSM")), label = "find_data.default([no formula]) errors")
})

test_that("Test find_data.llm()", {
    expect_true(inherits(find_data(lm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "find_data.lm() works")
})

test_that("Test find_data.glm()", {
    expect_true(inherits(find_data(glm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "find_data.glm() works")
})

test_that("Test find_data.formula()", {
    expect_true(inherits(find_data(mpg ~ cyl, data = mtcars), "data.frame"), label = "find_data.formula() works")
})

test_that("Test find_data.data.frame()", {
    expect_true(inherits(find_data(mtcars), "data.frame"), label = "find_data.data.frame() works")
})
