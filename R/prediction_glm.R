#' @rdname prediction
#' @export
prediction.glm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link"), 
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
        out <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(out, "at_specification")
        # calculate predictions
        if (isTRUE(calculate_se)) {
            tmp <- predict(model, newdata = out, type = type, se.fit = TRUE, ...)
            # cbind back together
            pred <- make_data_frame(out, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
        } else {
            tmp <- predict(model, newdata = out, type = type, se.fit = FALSE, ...)
            # cbind back together
            pred <- make_data_frame(out, fitted = tmp, se.fitted = rep(NA_real_, nrow(out)))
        }
    }
    
    # variance(s) of average predictions
    if (isTRUE(calculate_se)) {
        # handle case where SEs are calculated
        model_terms <- delete.response(terms(model))
        if (is.null(at)) {
            # no 'at_specification', so calculate variance of overall average prediction
            model_frame <- model.frame(model_terms, data, na.action = na.pass, xlev = model$xlevels)
            model_mat <- model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)
            if (type == "link") {
                means_for_prediction <- colMeans(model_mat)
            } else if (type == "response") {
                predictions_link <- predict(model, data = data, type = "link", se.fit = FALSE, ...)
                means_for_prediction <- colMeans(model$family$mu.eta(predictions_link) * model_mat)
            }
            J <- matrix(means_for_prediction, nrow = 1L)
        } else {
            # with 'at_specification', calculate variance of all counterfactual predictions
            datalist <- build_datalist(data, at = at, as.data.frame = FALSE)
            jacobian_list <- lapply(datalist, function(one) {
                model_frame <- model.frame(model_terms, one, na.action = na.pass, xlev = model$xlevels)
                model_mat <- model.matrix(model_terms, model_frame, contrasts.arg = model$contrasts)
                if (type == "link") {
                    means_for_prediction <- colMeans(model_mat)
                } else if (type == "response") {
                    predictions_link <- predict(model, data = one, type = "link", se.fit = FALSE, ...)
                    means_for_prediction <- colMeans(model$family$mu.eta(predictions_link) * model_mat)
                }
                means_for_prediction
            })
            J <- do.call("rbind", jacobian_list)
        }
        vc <- diag(J %*% vcov %*% t(J))
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
