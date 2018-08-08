#' @rdname prediction
#' @export
prediction.svm <- 
function(model, 
         data = NULL, 
         at = NULL, 
         calculate_se = TRUE,
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
            pred <- make_data_frame(pred, probs)
        }
        if (!is.null(attributes(tmp)[["decision.values"]])) {
            dvs <- data.frame(attributes(tmp)[["decision.values"]])
            names(dvs) <- paste0("dv(", names(dvs), ")")
            pred <- make_data_frame(pred, dvs)
        }
    } else {
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        tmp <- predict(model, newdata = out, decision.values = TRUE, probability = probability, ...)
        pred <- make_data_frame(out, fitted.class = tmp)
        attributes(pred[["fitted.class"]]) <- NULL
        if (!is.null(attributes(tmp)[["probabilities"]])) {
            probs <- data.frame(attributes(tmp)[["probabilities"]])
            names(probs) <- paste0("Pr(", names(probs), ")")
            pred <- make_data_frame(pred, probs)
        }
        if (!is.null(attributes(tmp)[["decision.values"]])) {
            dvs <- data.frame(attributes(tmp)[["decision.values"]])
            names(dvs) <- paste0("dv(", names(dvs), ")")
            pred <- make_data_frame(pred, dvs)
        }
    }
    
    # handle category argument
    if (missing(category)) {
        w <- grep("^Pr\\(", names(pred))[1L]
        if (is.na(w)) {
            pred[["fitted"]] <- NA_real_
            category <- NULL
        } else {
            category <- names(pred)[w]
            pred[["fitted"]] <- pred[[w]]
        }
    } else {
        w <- which(names(pred) == paste0("Pr(", category, ")"))
        if (!length(w)) {
            stop(sprintf("category %s not found", category))
        }
        pred[["fitted"]] <- pred[[ w[1L] ]]
    }
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data.frame of predictions
    # variance(s) of average predictions
    vc <- NA_real_
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = if (is.null(at)) at else at_specification,
              type = NA_character_,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = NULL,
              category = category,
              weighted = FALSE)
}
