#' @rdname prediction
#' @export
prediction.gausspr <- 
function(model, 
         data, 
         at = NULL, 
         type = NULL, 
         calculate_se = TRUE,
         category, 
         ...) {
    
    requireNamespace("kernlab")
    
    if (!is.null(type)) {
        warning(sprintf("'type' is ignored for models of class '%s'", class(model)))
    }
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- make_data_frame(fitted.class = kernlab::predict(model, type = "response", ...))
        probs <- make_data_frame(kernlab::predict(model, type = "probabilities", ...))
        names(probs) <- paste0("Pr(", names(probs), ")")
        pred <- cbind(pred, probs)
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        tmp <- kernlab::predict(model, newdata = out, type = "response", ...)
        tmp_probs <- make_data_frame(kernlab::predict(model, newdata = out, type = "probabilities", ...))
        names(tmp_probs) <- paste0("Pr(", names(tmp_probs), ")")
        # cbind back together
        pred <- make_data_frame(out, fitted.class = tmp, tmp_probs)
        rm(tmp, tmp_probs)
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
