# set comparison tolerance
tol <- 0.0001

library("datasets")

context("Test `prediction()` behavior")
test_that("Test prediction()", {
    mod1 <- lm(mpg ~ cyl, data = mtcars)
    mod2 <- glm(mpg ~ cyl, data = mtcars)
    expect_true(inherits(prediction(mod1, data = mtcars), "data.frame"), label = "prediction() works w data arg (LM)")
    expect_true(inherits(prediction(mod2, data = mtcars), "data.frame"), label = "prediction() works w data arg (GLM)")
    expect_true(inherits(prediction(mod1), "data.frame"), label = "prediction() works w/o data arg (LM)")
    expect_true(inherits(prediction(mod2), "data.frame"), label = "prediction() works w/o data arg (GLM)")
    expect_error(inherits(prediction(mod1, data = NULL), "data.frame"), label = "prediction() errors w/ NULL data arg (LM)")
    expect_error(inherits(prediction(mod2, data = NULL), "data.frame"), label = "prediction() errors w/ NULL data arg (GLM)")
    expect_true(all.equal(prediction(mod1, data = mtcars)$fitted, predict(mod1), check.attributes = FALSE),
                label = "prediction() matches predict() (LM)")
    expect_true(all.equal(prediction(mod2, data = mtcars)$fitted, predict(mod2, type = "response"), check.attributes = FALSE),
                label = "prediction() matches predict() (GLM)")
})

test_that("Test prediction(at = )", {
    m <- lm(mpg ~ cyl, data = mtcars)
    p1 <- prediction(m, at = list(cyl = 4))
    expect_true(inherits(p1, "data.frame"), label = "prediction(at = list(cyl = 4)) works")
    expect_true(nrow(p1) == nrow(mtcars), label = "prediction(at = list(cyl = 4)) works")
    expect_true(all.equal(p1$fitted, predict(m, within(mtcars, cyl <- 4)), check.attributes = FALSE),
                label = "prediction(at = list(cyl = 4)) matches predict()")
    
    p2 <- prediction(m, at = list(cyl = c(4, 6)))
    expect_true(inherits(p2, "data.frame"), label = "prediction(at = list(cyl = c(4, 6))) works")
    expect_true(nrow(p2) == 2*nrow(mtcars), label = "prediction(at = list(cyl = c(4, 6))) works")
    expect_true(all.equal(p2$fitted,
                          predict(m, rbind(within(mtcars, cyl <- 4), within(mtcars, cyl <- 6))),
                          check.attributes = FALSE),
                label = "prediction(at = list(cyl = c(4, 6))) matches predict()")
    
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
    expect_true(inherits(print(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars, at = list(cyl = c(4,6,8)))), "data.frame"), 
                label = "print() works with numeric outcome and at()")
})

test_that("Test summary() w/o at()", {
    m1 <- lm(mpg ~ cyl, data = mtcars)
    
    # prediction w/o at()
    p1 <- prediction(m1)
    s1 <- summary(p1)
    expect_true(inherits(summary(p1), "data.frame"), 
                label = "summary() works with numeric outcome")
    expect_true(all(c("Prediction", "SE", "z", "p", "lower", "upper") %in% names(s1)),
                label = "summary() has correct columns w/o at()")
    expect_true(nrow(s1) == 1L, label = "summary() has correct rows w/o at()")
    
    ## numerical correctness
    expect_true(all.equal(s1[["Prediction"]][1L], mean(predict(m1)), tolerance = tol),
                label = "summary() returns numerically correct mean prediction")
    test_se <- sqrt(colMeans(cbind(1, mtcars$cyl)) %*% vcov(m1) %*% colMeans(cbind(1, mtcars$cyl)))[1,1,drop=TRUE]
    expect_true(all.equal(s1[["SE"]][1L], test_se, tolerance = tol),
                label = "summary() returns numerically correct SE of mean prediction")
})

test_that("Test summary() w at()", {
    # prediction w/ at()
    m1 <- lm(mpg ~ cyl, data = mtcars)
    p2 <- prediction(m1, data = mtcars, at = list(cyl = c(4,6,8)))
    s2 <- summary(p2)
    expect_true(inherits(s2, "data.frame"),
                label = "summary() works with numeric outcome and at()")
    expect_true(all(c("at(cyl)", "Prediction", "SE", "z", "p", "lower", "upper") %in% names(s2)),
                label = "summary() has correct columns with at()")
    expect_true(nrow(s2) == 3L, label = "summary() has correct rows w/o at()")
    
    ## numerical correctness
    expect_true(all.equal(s2[["Prediction"]][1L], mean(predict(m1, newdata = within(mtcars, cyl <- 4))), tolerance = tol),
                label = "summary() returns numerically correct mean prediction with at()")
    test_se <- sqrt(colMeans(cbind(1, 4)) %*% vcov(m1) %*% colMeans(cbind(1, 4)))[1,1,drop=TRUE]
    expect_true(all.equal(s2[["SE"]][1L], test_se, tolerance = tol),
                label = "summary() returns numerically correct SE of mean prediction with at()")
})

test_that("Test prediction_summary()", {
    m1 <- lm(mpg ~ cyl, data = mtcars)
    p1 <- prediction(m1)
    s1 <- summary(p1)
    expect_true(identical(s1, prediction_summary(m1)), label = "prediction_summary() is correct")
})

test_that("Test head() and tail()", {
    p1 <- prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars)
    expect_true(inherits(head(p1), "data.frame"), label = "head() works")
    expect_true(nrow(head(p1, 5L)) == 5L, label = "head() has correct rows")
    expect_true(inherits(tail(p1), "data.frame"), label = "tail() works")
    expect_true(nrow(tail(p1, 5L)) == 5L, label = "tail() has correct rows")
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
    
    expect_true(identical(mean_or_mode(mtcars), lapply(mtcars, mean_or_mode)), label = "mean_or_mode.data.frame() is correct")
    expect_true(identical(median_or_mode(mtcars), lapply(mtcars, median_or_mode)), label = "median_or_mode.data.frame() is correct")
})
