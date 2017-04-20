library("datasets")

# test all prediction() methods, conditional on availability of package
# this file is organized alphabetically by package

context("Test `prediction()` methods, conditional on package availability")

if (require("AER", quietly = TRUE)) {
    test_that("Test prediction() for 'ivreg'", {
        data("CigarettesSW", package = "AER")
        CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
        CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
        CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)
        m <- AER::ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
                        data = CigarettesSW, subset = year == "1995")
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("betareg", quietly = TRUE)) {
    test_that("Test prediction() for 'betareg'", {
        data("GasolineYield", package = "betareg")
        m <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("brglm", quietly = TRUE)) {
    test_that("Test prediction() for 'brglm'", {
        data("lizards", package = "brglm")
        m <- brglm::brglm(cbind(grahami, opalinus) ~ height + diameter +
                          light + time, family = binomial(logit), data=lizards,
                          method = "brglm.fit")
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("crch", quietly = TRUE)) {
    test_that("Test prediction() for 'crch'", {
        e <- new.env()
        data("RainIbk", package = "crch", envir = e)
        RainIbk <- e$RainIbk
        RainIbk$sqrtensmean <- apply(sqrt(RainIbk[,grep('^rainfc',names(RainIbk))]), 1, mean)
        m <- crch::crch(sqrt(rain) ~ sqrtensmean, data = RainIbk, dist = "gaussian", left = 0)
        expect_true(inherits(prediction(m, data = RainIbk), "prediction"))
    })
#    test_that("Test prediction() for 'hxlr'", {
#        e <- new.env()
#        data("RainIbk", package = "crch", envir = e)
#        RainIbk <- e$RainIbk
#        RainIbk$sqrtensmean <- 
#          apply(sqrt(RainIbk[,grep('^rainfc',names(RainIbk))]), 1, mean)
#        q <- unique(quantile(RainIbk$rain, seq(0.1, 0.9, 0.1)))
#        m <- crch::hxlr(sqrt(rain) ~ sqrtensmean, data = RainIbk, thresholds = sqrt(q))
#        expect_true(inherits(prediction(m, data = RainIbk), "prediction"))
#    })
}

if (require("e1071", quietly = TRUE)) {
    test_that("Test prediction() for 'naiveBayes'", {
        data("Titanic")
        m <- e1071::naiveBayes(Survived ~ ., data = Titanic)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'svm'", {
        m <- e1071::svm(Species ~ ., data = iris)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("gam", quietly = TRUE)) {
    test_that("Test prediction() for 'gam'", {
        data("gam.data", package = "gam")
        m <- gam::gam(y ~ gam::s(x,6) + z,data=gam.data)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("gee", quietly = TRUE)) {
    test_that("Test prediction() for 'gee'", {
        data("warpbreaks")
        m <- gee::gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="exchangeable")
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("glmx", quietly = TRUE) ) {
    test_that("Test prediction() for 'glmx()'", {
        d <- data.frame(x = runif(200, -1, 1))
        d$y <- rnbinom(200, mu = exp(0 + 3 * d$x), size = 1)
        m <- glmx::glmx(y ~ x, data = d, family = MASS::negative.binomial, xlink = "log", xstart = 0)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'hetglm()'", {
        n <- 200
        x <- rnorm(n)
        ystar <- 1 + x +  rnorm(n, sd = exp(x))
        y  <- factor(ystar > 0)
        m <- glmx::hetglm(y ~ x | 1)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("lme4", quietly = TRUE)) {
    test_that("Test prediction() for 'merMod'", {
        data("cbpp", package = "lme4")
        m <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 |herd), cbpp, binomial)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("MASS", quietly = TRUE)) {
    test_that("Test prediction() for 'glm.nb'", {
        data("quine", package = "MASS")
        m <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'lda'", {
        data("iris3", package = "datasets")
        tr <- sample(1:50, 25)
        train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
        cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
        m <- MASS::lda(train, cl)
        expect_true(inherits(prediction(m, data = train), "prediction"))
    })
    test_that("Test prediction() for 'lqs'", {
        data("stackloss", package = "datasets")
        m <- MASS::lqs(stack.loss ~ ., data = stackloss, method = "S", nsamp = "exact")
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'mca'", {
        data("farms", package = "MASS")
        m <- MASS::mca(farms, abbrev=TRUE)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'polr'", {
        data("housing", package = "MASS")
        m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'qda'", {
        data("iris3", package = "datasets")
        tr <- sample(1:50, 25)
        train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
        cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
        m <- MASS::qda(train, cl)
        expect_true(inherits(prediction(m, data = train), "prediction"))
    })
    test_that("Test prediction() for 'rlm'", {
        data("stackloss", package = "datasets")
        m <- MASS::rlm(stack.loss ~ ., stackloss)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("mclogit", quietly = TRUE)) {
    test_that("Test prediction() for 'mclogit'", {
        data("Transport", package = "mclogit")
        m <- mclogit::mclogit(cbind(resp,suburb)~distance+cost, data = Transport, trace = FALSE)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("mnlogit", quietly = TRUE)) {
    test_that("Test prediction() for 'mnlogit'", {
        data("Fish", package = "mnlogit")
        m <- mnlogit::mnlogit(mode ~ price | income | catch, Fish, ncores = 1)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("MNP", quietly = TRUE)) {
    test_that("Test prediction() for 'mnp'", {
        data("japan", package = "MNP")
        m <- MNP::mnp(cbind(LDP, NFP, SKG, JCP) ~ gender + education + age, data = head(japan, 100), verbose = FALSE)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("nlme", quietly = TRUE)) {
    test_that("Test prediction() for 'gls'", {
        data("Ovary", package = "nlme")
        m <- nlme::gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
                         correlation = nlme::corAR1(form = ~ 1 | Mare), verbose = FALSE)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'lme'", {
        data("Orthodont", package = "nlme")
        m <- nlme::lme(distance ~ age, Orthodont, random = ~ age | Subject)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("nnet", quietly = TRUE)) {
    #test_that("Test prediction() for 'multinom'", { })
    test_that("Test prediction() for 'nnet'", {
        data("iris3", package = "datasets")
        ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                          species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
        samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
        m <- nnet::nnet(species ~ ., data = ird, subset = samp, size = 2, rang = 0.1,
                        decay = 5e-4, maxit = 200, trace = FALSE)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("ordinal", quietly = TRUE)) {
    test_that("Test prediction() for 'clm'", {
        data("wine", package = "ordinal")
        m <- ordinal::clm(rating ~ temp * contact, data = wine)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("plm", quietly = TRUE)) {
    test_that("Test prediction() for 'plm'", {
        data("Grunfeld", package = "plm")
        m <- plm::plm(inv ~ value + capital, data = Grunfeld, model = "pooling")
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("pscl", quietly = TRUE)) {
    test_that("Test prediction() for 'hurdle'", {
        data("bioChemists", package = "pscl")
        m <- pscl::hurdle(art ~ ., data = bioChemists)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'zeroinfl'", {
        data("bioChemists", package = "pscl")
        m <- pscl::zeroinfl(art ~ ., data = bioChemists)
        expect_true(inherits(prediction(m), "prediction"))
    })
    #test_that("Test prediction() for 'ideal'", {
    #    expect_true(inherits(prediction(m), "prediction"))
    #})
}

#if (require("quantreg", quietly = TRUE)) {
#    test_that("Test prediction() for 'rq'", {})
#}

if (require("rpart", quietly = TRUE)) {
    test_that("Test prediction() for 'rpart'", {
        data("kyphosis", package = "rpart")
        m <- rpart::rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("sampleSelection", quietly = TRUE)) {
    test_that("Test prediction() for 'selection'", {
        data("Mroz87", package = "sampleSelection")
        Mroz87$kids  <- (Mroz87$kids5 + Mroz87$kids618 > 0)
        m <- sampleSelection::heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ, 
                                     wage ~ exper + I( exper^2 ) + educ + city, Mroz87)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("stats", quietly = TRUE)) {
    test_that("Test prediction() for 'ar'", {
        data("sunspot.year", package = "datasets")
        m <- stats::ar(sunspot.year)
        expect_true(inherits(prediction(m, data = sunspot.year), "prediction"))
    })
    test_that("Test prediction() for 'Arima'", {
        expect_true(inherits(prediction(stats::arima(lh, order = c(3,0,0)), n.ahead = 12), "prediction"))
    })
    test_that("Test prediction() for 'arima0'", {
        m <- stats::arima0(lh, order = c(1,0,1))
        expect_true(inherits(prediction(m, data = lh, n.ahead = 12), "prediction"))
    })
    test_that("Test prediction() for 'loess'", {
        m <- stats::loess(dist ~ speed, cars)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'nls'", {
        m <- stats::nls(demand ~ SSasympOrig(Time, A, lrc), data = BOD)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'ppr'", {
        data("rock", package = "datasets")
        rock$area1 <- rock$area/10000
        rock$peri1 <- rock$peri/10000
        m <- stats::ppr(log(perm) ~ area1 + peri1 + shape,
                        data = rock, nterms = 2, max.terms = 5)
        expect_true(inherits(prediction(m), "prediction"))
    })
    test_that("Test prediction() for 'princomp'", {
        data("USArrests", package = "datasets")
        m <- stats::princomp(~ ., data = USArrests, cor = TRUE)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("survey", quietly = TRUE)) {
    test_that("Test prediction() for 'svyglm'", {
        data("api", package = "survey")
        dstrat <- survey::svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
        m <- survey::svyglm(api.stu~enroll, design=dstrat)
        expect_true(inherits(prediction(m), "prediction"))
    })
}

if (require("survival", quietly = TRUE)) {
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

