library("datasets")

# test all prediction() methods, conditional on availability of package
# this file is organized alphabetically by package

context("Test `prediction()` methods, conditional on package availability")

if (requireNamespace("AER")) {
    test_that("Test prediction() for 'ivreg'", {
        data("CigarettesSW", package = "AER")
        CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
        CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
        CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
        fm <- AER::ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
                         data = CigarettesSW, subset = year == "1995")
        expect_true(inherits(prediction(fm), "prediction"))
    })
}

if (requireNamespace("betareg")) {
    test_that("Test prediction() for 'betareg'", {
        data("GasolineYield", package = "betareg")
        gy <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
        expect_true(inherits(prediction(gy), "prediction"))
    })
}

if (requireNamespace("brglm")) {
    test_that("Test prediction() for 'brglm'", {
        data("lizards", package = "brglm")
        lizards.brglm <- brglm::brglm(cbind(grahami, opalinus) ~ height + diameter +
                                      light + time, family = binomial(logit), data=lizards,
                                      method = "brglm.fit")
        expect_true(inherits(prediction(lizards.brglm), "prediction"))
    })
}

if (requireNamespace("crch")) {
    test_that("Test prediction() for 'crch'", {
        e <- new.env()
        data("RainIbk", package = "crch", envir = e)
        RainIbk <- e$RainIbk
        RainIbk$sqrtensmean <- apply(sqrt(RainIbk[,grep('^rainfc',names(RainIbk))]), 1, mean)
        CRCH2 <- crch::crch(sqrt(rain) ~ sqrtensmean, data = RainIbk, 
                            dist = "gaussian", left = 0)
        expect_true(inherits(prediction(CRCH2, data = RainIbk), "prediction"))
    })
    test_that("Test prediction() for 'hxlr'", {
        e <- new.env()
        data("RainIbk", package = "crch", envir = e)
        RainIbk <- e$RainIbk
        RainIbk$sqrtensmean <- 
          apply(sqrt(RainIbk[,grep('^rainfc',names(RainIbk))]), 1, mean)
        q <- unique(quantile(RainIbk$rain, seq(0.1, 0.9, 0.1)))
        XLR <- crch::hxlr(sqrt(rain) ~ sqrtensmean, data = RainIbk, thresholds = sqrt(q))
        expect_true(inherits(prediction(XLR, data = RainIbk), "prediction"))
    })
}

if (requireNamespace("e1071")) {
    test_that("Test prediction() for 'naiveBayes'", {
        data("Titanic")
        model <- e1071::naiveBayes(Survived ~ ., data = Titanic)
        expect_true(inherits(prediction(model), "prediction"))
    })
    test_that("Test prediction() for 'svm'", {
        model <- e1071::svm(Species ~ ., data = iris)
        expect_true(inherits(prediction(model), "prediction"))
    })
}

if (requireNamespace("gam")) {
    test_that("Test prediction() for 'gam'", {
        data("gam.data", package = "gam")
        gam.object <- gam::gam(y ~ gam::s(x,6) + z,data=gam.data)
        expect_true(inherits(prediction(gam.object), "prediction"))
    })
}

if (requireNamespace("gee")) {
    test_that("Test prediction() for 'gee'", {
        data("warpbreaks")
        g <- gee::gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="exchangeable")
        expect_true(inherits(prediction(g), "prediction"))
    })
}

if (requireNamespace("glmx") ) {
    test_that("Test prediction() for 'glmx()'", {
        d <- data.frame(x = runif(200, -1, 1))
        d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
        m_nb1 <- glmx::glmx(y ~ x, data = d, family = MASS::negative.binomial, xlink = "log", xstart = 0)
        expect_true(inherits(prediction(m_nb1), "prediction"))
    })
    test_that("Test prediction() for 'hetglm()'", {
        n <- 200
        x <- rnorm(n)
        ystar <- 1 + x +  rnorm(n, sd = exp(x))
        y  <- factor(ystar > 0)
        m0b <- glmx::hetglm(y ~ x | 1)
        expect_true(inherits(prediction(m0b), "prediction"))
    })
}

if (requireNamespace("MASS")) {
    #test_that("Test prediction() for 'glm.nb'", {})
    #test_that("Test prediction() for 'lda'", {})
    #test_that("Test prediction() for 'lqs'", {})
    #test_that("Test prediction() for 'mca'", {})
    #test_that("Test prediction() for 'polr'", {})
    #test_that("Test prediction() for 'qda'", {})
    #test_that("Test prediction() for 'rlm'", {})
}

if (requireNamespace("mclogit")) {
    test_that("Test prediction() for 'mclogit'", {
        data("Transport", package = "mclogit")
        m <- mclogit::mclogit(cbind(resp,suburb)~distance+cost, data = Transport, trace = FALSE)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (requireNamespace("mnlogit")) {
    test_that("Test prediction() for 'mnlogit'", {
        data("Fish", package = "mnlogit")
        fit <- mnlogit::mnlogit(mode ~ price | income | catch, Fish, ncores = 1)
        expect_true(inherits(prediction(fit), "prediction"))
    })
}

if (requireNamespace("MNP")) {
    test_that("Test prediction() for 'mnp'", {
        data("japan", package = "MNP")
        res2 <- MNP::mnp(cbind(LDP, NFP, SKG, JCP) ~ gender + education + age, data = head(japan, 100), verbose = FALSE)
        expect_true(inherits(prediction(res2), "prediction"))
    })
}

if (requireNamespace("nlme")) {
    test_that("Test prediction() for 'gls'", {
        data("Ovary", package = "nlme")
        fm1 <- nlme::gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
                         correlation = nlme::corAR1(form = ~ 1 | Mare), verbose = FALSE)
        expect_true(inherits(prediction(fm1), "prediction"))
    })
}

if (requireNamespace("nnet")) {
    #test_that("Test prediction() for 'multinom'", { })
    test_that("Test prediction() for 'nnet'", {
        data("iris3", package = "datasets")
        ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                          species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
        samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
        ir.nn2 <- nnet::nnet(species ~ ., data = ird, subset = samp, size = 2, rang = 0.1,
                             decay = 5e-4, maxit = 200, trace = FALSE)
        expect_true(inherits(prediction(ir.nn2), "prediction"))
    })
}

if (requireNamespace("ordinal")) {
    test_that("Test prediction() for 'clm'", {
        data("wine", package = "ordinal")
        fm1 <- ordinal::clm(rating ~ temp * contact, data = wine)
        expect_true(inherits(prediction(fm1), "prediction"))
    })
}

if (requireNamespace("quantreg")) {
    #test_that("Test prediction() for 'rq'", {})
}

if (requireNamespace("sampleSelection")) {
    test_that("Test prediction() for 'selection'", {
        data("Mroz87", package = "sampleSelection")
        Mroz87$kids  <- (Mroz87$kids5 + Mroz87$kids618 > 0)
        m <- sampleSelection::heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ, 
                                     wage ~ exper + I( exper^2 ) + educ + city, Mroz87)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (requireNamespace("stats")) {
    #test_that("Test prediction() for 'ar'", {})
    #test_that("Test prediction() for 'arima'", {})
    #test_that("Test prediction() for 'arima0'", {})
    #test_that("Test prediction() for 'loess'", {})
    #test_that("Test prediction() for 'nls'", {})
    #test_that("Test prediction() for 'ppr'", {})
    #test_that("Test prediction() for 'princomp'", {})
}

if (requireNamespace("survey")) {
    test_that("Test prediction() for 'svyglm'", {
        data("api", package = "survey")
        dstrat <- survey::svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
        api.reg <- survey::svyglm(api.stu~enroll, design=dstrat)
        expect_true(inherits(prediction(api.reg), "prediction"))
    })
}

if (requireNamespace("survival")) {
    test_that("Test prediction() for 'coxph'", {
        test1 <- list(time=c(4,3,1,1,2,2,3), 
              status=c(1,1,1,0,1,1,0), 
              x=c(0,2,1,1,1,0,0), 
              sex=c(0,0,0,0,1,1,1)) 
        m <- survival::coxph(survival::Surv(time, status) ~ x + survival::strata(sex), test1) 
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'survreg'", {
        data("ovarian", package = "survival")
        m <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist='weibull', scale=1)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

