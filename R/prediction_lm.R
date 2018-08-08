#' @rdname prediction
#' @export
prediction.lm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = "response",
         vcov = stats::vcov(model),
         calculate_se = TRUE,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        if (isTRUE(calculate_se)) {
            pred <- predict(model, type = type, se.fit = TRUE, ...)
            pred <- make_data_frame(fitted = pred[["fit"]], se.fitted = pred[["se.fit"]])
        } else {
            pred <- predict(model, type = type, se.fit = FALSE, ...)
            pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
        }
    } else {
        # reduce memory profile
        model[["model"]] <- NULL
        
        # setup data
        datalist <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(datalist, "at_specification")
        # calculate predictions
        if (isTRUE(calculate_se)) {
            tmp <- predict(model, newdata = datalist, type = type, se.fit = TRUE, ...)
            # cbind back together
            pred <- make_data_frame(datalist, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
        } else {
            tmp <- predict(model, newdata = datalist, type = type, se.fit = FALSE, ...)
            # cbind back together
            pred <- make_data_frame(datalist, fitted = tmp, se.fitted = rep(NA_real_, nrow(datalist)))
        }
    }
    
    # variance(s) of average predictions
    if (isTRUE(calculate_se)) {
        # handle case where SEs are calculated
        J <- NULL
        model_terms <- delete.response(terms(model))
        if (is.null(at)) {
            # no 'at_specification', so calculate variance of overall average prediction
            model_frame <- model.frame(model_terms, data, na.action = na.pass, xlev = model$xlevels)
            model_mat <- model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)
            means_for_prediction <- colMeans(model_mat)
            vc <- (means_for_prediction %*% vcov %*% means_for_prediction)[1L, 1L, drop = TRUE]
        } else {
            # with 'at_specification', calculate variance of all counterfactual predictions
            datalist <- build_datalist(data, at = at, as.data.frame = FALSE)
            vc <- unlist(lapply(datalist, function(one) {
                model_frame <- model.frame(model_terms, one, na.action = na.pass, xlev = model$xlevels)
                model_mat <- model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)
                means_for_prediction <- colMeans(model_mat)
                means_for_prediction %*% vcov %*% means_for_prediction
            }))
        }
    } else {
        # handle case where SEs are *not* calculated
        J <- NULL
        if (length(at)) {
            vc <- rep(NA_real_, nrow(at_specification))
        } else {
            vc <- NA_real_
        }
    }
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = if (is.null(at)) at else at_specification,
              type = type,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = J,
              weighted = FALSE)
}
