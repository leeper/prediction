#' @rdname prediction
#' @export
prediction.svm <- 
function(model, 
         data = NULL, 
         at = NULL, 
         category, 
         ...) {

    # extract predicted value
    data <- data
    anyp <- grep("prob.+", names(model))
    if (length(anyp) && !is.null(model[[ anyp[1L] ]])) {
        probability <- TRUE
    } else {
        probability <- FALSE
    }
    if (missing(data) || is.null(data)) {
        tmp <- predict(model, decision.values = TRUE, probability = probability, ...)
        pred <- data.frame(fitted.class = tmp)
        attributes(pred[["fitted.class"]]) <- NULL
        if (!is.null(attributes(tmp)[["probabilities"]])) {
            probs <- data.frame(attributes(tmp)[["probabilities"]])
            names(probs) <- paste0("Pr(", names(probs), ")")
            pred <- cbind(pred, probs)
        }
        if (!is.null(attributes(tmp)[["decision.values"]])) {
            dvs <- data.frame(attributes(tmp)[["decision.values"]])
            names(dvs) <- paste0("dv(", names(dvs), ")")
            pred <- cbind(pred, dvs)
        }
    } else {
        tmp <- predict(model, newdata = data, decision.values = TRUE, probability = probability, ...)
        pred <- cbind(data, fitted.class = tmp)
        attributes(pred[["fitted.class"]]) <- NULL
        if (!is.null(attributes(tmp)[["probabilities"]])) {
            probs <- data.frame(attributes(tmp)[["probabilities"]])
            names(probs) <- paste0("Pr(", names(probs), ")")
            pred <- cbind(pred, probs)
        }
        if (!is.null(attributes(tmp)[["decision.values"]])) {
            dvs <- data.frame(attributes(tmp)[["decision.values"]])
            names(dvs) <- paste0("dv(", names(dvs), ")")
            pred <- cbind(pred, dvs)
        }
    }
    
    # handle category argument
    if (missing(category)) {
        w <- grep("^Pr\\(", names(pred))[1L]
        category <- names(pred)[w]
        pred[["fitted"]] <- pred[[w]]
    } else {
        w <- which(names(pred) == paste0("Pr(", category, ")"))
        if (!length(w)) {
            stop(sprintf("category %s not found", category))
        }
        pred[["fitted"]] <- pred[[ w[1L] ]]
    }
    
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NULL,
              category = category)
}
