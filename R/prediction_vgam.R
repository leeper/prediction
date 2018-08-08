# @rdname prediction
# @export
prediction.vgam <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link"), 
         calculate_se = FALSE,
         category,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- make_data_frame(predict(model, type = type, se.fit = FALSE, ...))
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        tmp <- predict(model, newdata = out, type = type, se.fit = FALSE, ...)
        if (!is.null(dim(tmp))) {
            tmp <- as.matrix(tmp, ncol = 1)
        }
        # cbind back together
        pred <- make_data_frame(out, fitted = make_data_frame(tmp), se.fitted = rep(NA_real_, nrow(out)))
    }
    
    # handle category argument
    if (missing(category)) {
        category <- names(pred)[!names(pred) %in% names(data)][1L]
        pred[["fitted"]] <- pred[[category]]
    } else {
        w <- grep(category, names(pred))
        if (!length(w)) {
            stop(sprintf("category %s not found", category))
        }
        pred[["fitted"]] <- pred[[ w[1L] ]]
    }
    
    # variance(s) of average predictions
    vc <- NA_real_
    
    # output
    structure(pred, 
              class = c("prediction", "data.frame"),
              at = if (is.null(at)) at else at_specification,
              type = type,
              call = if ("call" %in% names(model)) model[["call"]] else NULL,
              model_class = class(model),
              row.names = seq_len(nrow(pred)),
              vcov = vc,
              jacobian = NULL,
              category = category,
              weighted = FALSE)
}
