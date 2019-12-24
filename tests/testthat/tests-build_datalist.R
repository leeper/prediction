context("Test `build_data_list()` behavior")

test_that("Test build_datalist()", {
    expect_true(inherits(build_datalist(mtcars, at = NULL), "list"), label = "build_datalist(at = NULL) works")
    expect_true(inherits(build_datalist(mtcars, at = list(cyl = 4)), "list"), label = "build_datalist(at = ) works")
    
    expect_true(length(build_datalist(mtcars, at = list(cyl = c(4, 6), wt = 2:3))) == 4, label = "build_datalist() length")
    
    expect_error(build_datalist(mtcars, at = list(foo = 1)), label = "build_datalist(at = foo) errors")
    expect_error(build_datalist(mtcars, at = list(1)), label = "build_datalist() unnamed list errors")
    expect_warning(build_datalist(mtcars, at = list(cyl = 2)), label = "build_datalist() range warning")
})

test_that("Factors in build_datalist()", {
    mtcars$cyl <- factor(mtcars$cyl)
    e <- build_datalist(mtcars, at = list(cyl = 4))
    expect_true(inherits(e, "list"), label = "build_datalist(at = factor()) works")
    expect_true(identical(levels(mtcars$cyl), levels(e[[1L]][["cyl"]])), label = "build_datalist(at = factor()) preserves factor levels")
    
    expect_error(build_datalist(mtcars, at = list(cyl = 7)), label = "build_datalist(at = ) errors on illegal factor level")
    
    mtcars$cyl <- as.character(mtcars$cyl)
    expect_true(inherits(build_datalist(mtcars, at = list(cyl = 4)), "list"), label = "build_datalist(at = ) works")
})
