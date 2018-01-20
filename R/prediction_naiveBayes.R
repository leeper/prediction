# @rdname prediction
# @export
prediction.naiveBayes <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = NULL, 
         calculate_se = FALSE,
         category, 
         ...) {
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        warning(sprintf("'data' is ignored for models of class '%s'", class(model)))
    }
    if (!is.null(type)) {
        warning(sprintf("'type' is ignored for models of class '%s'", class(model)))
    }
    
    # setup data
    if (is.null(at)) {
        out <- data
    } else {
        out <- build_datalist(data, at = at, as.data.frame = TRUE)
    }
    # calculate predictions
    pred <- predict(model, newdata = out, type = "class", ...)
    probs <- make_data_frame(predict(model, newdata = out, type = "raw", ...))
    names(probs) <- paste0("Pr(", names(probs), ")")
    # cbind back together
    pred <- make_data_frame(out, probs, fitted.class = pred, se.fitted = rep(NA_real_, nrow(out)))
    
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
    
    # obs-x-(ncol(data)+2+nlevels(outcome)) data frame
    structure(pred,
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = NA_character_,
              category = category)
}
