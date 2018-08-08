#' @rdname prediction
#' @export
prediction.mnlogit <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         calculate_se = FALSE,
         category, 
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- make_data_frame(fitted.class = predict(model, probability = FALSE, ...))
        probs <- make_data_frame(predict(model, probability = TRUE, ...))
        names(probs) <- paste0("Pr(", names(probs), ")")
        pred <- make_data_frame(pred, probs)
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        tmp <- predict(model, newdata = out, probability = FALSE, ...)
        tmp_probs <- make_data_frame(predict(model, newdata = out, probability = TRUE, ...))
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
