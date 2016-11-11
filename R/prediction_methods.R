#' @rdname prediction
#' @export
prediction.default <- function(model, data = find_data(model, parent.frame()), type = "response", ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              type = type)
}

#' @rdname prediction
#' @export
prediction.lm <- prediction.default

#' @rdname prediction
#' @export
prediction.glm <- function(model, data = find_data(model, parent.frame()), type = c("response", "link"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.svyglm <- function(model, data = find_data(model, parent.frame()), type = c("response", "link"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    pred <- list(fitted = unclass(pred), 
                 se.fitted = sqrt(unname(attributes(pred)[["var"]])))
    attributes(pred[["fitted"]]) <- NULL
    class(pred[["fitted"]]) <- c("fit", "numeric")
    class(pred[["se.fitted"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fitted"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.loess <- function(model, data = find_data(model, parent.frame()), type = "response", ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.ar <- function(model, data, ...) {
    # setup data
    data <- data
    
    # extract predicted value at input value
    pred <- predict(object = model, newdata = data, se.fit = TRUE, ...)
    names(pred) <- c("fit", "se.fit")
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.arima0 <- prediction.ar

#' @rdname prediction
#' @export
prediction.Arima <- function(model, data, ...) {
    # setup data
    if (!missing(data)) {
        stop("There is no 'data' argument for objects of class 'Arima'")
    }
    
    # extract predicted value at input value
    pred <- predict(object = model, newdata = data, se.fit = TRUE, ...)
    names(pred) <- c("fit", "se.fit")
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.ivreg <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # extract predicted value at input values
    pred <- data.frame(fit = predict(model, newdata = data, ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.nls <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # extract predicted value at input values
    pred <- data.frame(fit = predict(model, newdata = data, ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.survreg <- 
function(model, 
         data = find_data(model, parent.frame()), 
         type = c("response", "lp", "quantile", "uquantile"), 
         ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.coxph <- function(model, data = find_data(model, parent.frame()), type = c("risk", "expected", "lp"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.gls <- function(model, data, ...) {
    # setup data
    data <- data
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fit = predict(model, newdata = data, type = "class", ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.svm <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # extract predicted value
    if (any(grepl("prob.+", names(model)))) {
        pred <- data.frame(fitted = predict(model, newdata = data, probability = TRUE, ...))
        pred[["se.fit"]] <- NA_real_
        pred <- cbind(pred, attributes(pred[["fitted"]])[["probabilities"]])
        attr(pred[["fitted"]], "probabilities") <- NULL
        names(pred) <- c("fitted", "se.fit", paste0("Pr(", names(pred)[-c(1L:2L)], ")"))
    } else {
        pred <- data.frame(fitted = predict(model, newdata = data, probability = FALSE, ...))
        pred[["se.fit"]] <- NA_real_
    }
    class(pred[["fitted"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-(2+nlevels) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.polr <- function(model, data = find_data(model, parent.frame()), type = NULL, ...) {
    # setup data
    data <- data
    
    if (!is.null(type)) {
        warning("'type' is ignored for models of class 'polr'")
    }
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fit = predict(model, newdata = data, type = "class", ...))
    pred[["se.fit"]] <- rep(NA_real_, length(pred[["fit"]]))
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    probs <- as.data.frame(predict(model, newdata = data, type = "probs", ...))
    names(probs) <- paste0("Pr(", names(probs), ")")
    
    # obs-x-2 data.frame of predictions
    structure(cbind(list(fitted = pred[["fit"]], 
                         se.fitted = pred[["se.fit"]]),
                    probs),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.clm <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # extract predicted value at input value
    pred <- predict(model, newdata = data, type = "class", se.fit = FALSE, ...)
    pred[["se.fit"]] <- rep(NA_real_, length(pred[["fit"]]))
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    problist <- predict(model, newdata = data, type = "prob", se.fit = TRUE, ...)
    probs <- as.data.frame(problist[["fit"]])
    probs.se <- as.data.frame(problist[["fit"]])
    names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
    names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")
    
    # obs-x-2 data.frame of predictions
    structure(cbind(list(fitted = pred[["fit"]], 
                         se.fitted = pred[["se.fit"]]),
                    probs,
                    probs.se),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}
