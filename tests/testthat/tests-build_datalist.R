context("Test `build_data_list()` behavior")

test_that("Test build_datalist()", {
    expect_true(inherits(build_datalist(mtcars, at = NULL), "list"), label = "build_datalist(at = NULL) works")
    expect_true(inherits(build_datalist(mtcars, at = list(cyl = 4)), "list"), label = "build_datalist(at = ) works")
    
    expect_error(build_datalist(mtcars, at = list(foo = 1)), label = "build_datalist(at = foo) errors")
})

test_that("Factors in build_datalist()", {
    mtcars$cyl <- factor(mtcars$cyl)
    expect_true(inherits(build_datalist(mtcars, at = list(cyl = 4)), "list"), label = "build_datalist(at = factor()) works")
    
    expect_error(build_datalist(mtcars, at = list(cyl = 7)), label = "build_datalist(at = ) errors on illegal factor level")
    
    mtcars$cyl <- as.character(mtcars$cyl)
    expect_true(inherits(build_datalist(mtcars, at = list(cyl = 4)), "list"), label = "build_datalist(at = ) works")
})
