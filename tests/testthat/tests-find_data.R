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
    expect_true(nrow(find_data(m3)) == nrow(na.omit(mtcars2[mtcars2$am == 1, ])), label = "find_data.default(data, subset, na.action) works")
    
    expect_error(find_data(StructTS(log10(UKgas), type = "BSM")), label = "find_data.default([no formula]) errors")
})

test_that("Test find_data.lm()", {
    expect_true(inherits(find_data(lm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "find_data.lm() works")
})

test_that("Test find_data.glm()", {
    expect_true(inherits(find_data(glm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "find_data.glm() works")
})

test_that("Test find_data.data.frame()", {
    expect_true(inherits(find_data(mtcars), "data.frame"), label = "find_data.data.frame() works")
})

test_that("Test find_data.lm() and prediction.lm() with missing data", {
    mtcars2 <- mtcars
    mtcars2$mpg[1:4] <- NA_real_
    
    # na.omit
    m1 <- lm(mpg ~ cyl, data = mtcars2, na.action = na.omit)
    expect_true(identical(dim(find_data(m1)), dim(na.omit(mtcars2))),
                label = "find_data.lm() drops missing data when 'na.action = na.omit'")
    expect_true(nrow(prediction(m1)) == nrow(na.omit(mtcars2)),
                label = "prediction.lm() returns correct rows when 'na.action = na.omit'")
    
    # na.exclude
    m2 <- lm(mpg ~ cyl, data = mtcars2, na.action = na.exclude)
    expect_true(identical(dim(find_data(m2)), dim(na.omit(mtcars2))),
                label = "find_data.lm() drops missing data when 'na.action = na.exclude'")
    expect_true(nrow(prediction(m2)) == nrow(na.omit(mtcars2)),
                label = "prediction.lm() returns correct rows when 'na.action = na.exclude'")
    
    # prediction with missing data passed explicitly
    m3 <- lm(mpg ~ cyl, data = mtcars) # missing outcome
    p3 <- prediction(m3, mtcars2, na.action = na.pass)
    expect_true(nrow(p3) == nrow(mtcars),
                label = "prediction.lm() returns correct rows when prediction(na.action = na.pass) for missing outcome")
    expect_true(all(!is.na(p3$fitted)[1:4]),
                label = "prediction.lm() returns numeric predictions when prediction(na.action = na.pass) for missing outcome")
    expect_true(nrow(prediction(m3, mtcars2, na.action = na.omit)) == nrow(mtcars2),
                label = "prediction.lm() returns correct rows when prediction(na.action = na.omit) for missing outcome")
    expect_true(nrow(prediction(m3, mtcars2, na.action = na.exclude)) == nrow(mtcars2),
                label = "prediction.lm() returns correct rows when prediction(na.action = na.exclude) for missing outcome")
    
    m4 <- lm(cyl ~ mpg, data = mtcars) # missing covariate
    p4 <- prediction(m4, mtcars2, na.action = na.pass)
    expect_true(nrow(p4) == nrow(mtcars),
                label = "prediction.lm() returns correct rows when prediction(na.action = na.pass) for missing covariate")
    expect_true(all(is.na(p4$fitted)[1:4]),
                label = "prediction.lm() returns NA predictions when prediction(na.action = na.pass) for missing covariate")
    expect_error(prediction(m4, mtcars2, na.action = na.omit),
                label = "prediction.lm() fails when prediction(na.action = na.omit) for missing covariate")
    expect_error(prediction(m4, mtcars2, na.action = na.exclude),
                label = "prediction.lm() fails when prediction(na.action = na.exclude) for missing covariate")
    
    rm(mtcars2)
})

test_that("Test find_data.lm() with subsetted data", {
    mtcars2 <- mtcars
    mtcars2$mpg[1:4] <- NA_real_
    m1 <- lm(mpg ~ cyl, data = mtcars2, subset = !is.na(mpg))
    expect_true(identical(dim(find_data(m1)), dim(na.omit(mtcars2))),
                label = "find_data.lm() has correct dimensions when subsetting")
    expect_true(nrow(prediction(m1)) == nrow(na.omit(mtcars2)),
                label = "prediction.lm() returns correct rows when subsetting")
    x <- c(rep(TRUE, 30), FALSE, FALSE)
    m2 <- lm(mpg ~ cyl, data = mtcars2, subset = x)
    expect_true(identical(nrow(find_data(m2)), nrow(na.omit(mtcars2))-2L),
                label = "find_data.lm() subsets correctly when subsetting variable is global")
    expect_true(identical(rownames(find_data(m2)), head(rownames(na.omit(mtcars2)), 26)),
                label = "find_data.lm() returns correct rows when subsetting and missing data are present")
    rm(mtcars2)
})
