# @rdname prediction
# @export
prediction.vglm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = c("response", "link"), 
         calculate_se = TRUE,
         category,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    arg <- list(...)
    if (missing(data) || is.null(data)) {
        if ("se.fit" %in% names(arg)) {
            tmp <- predict(model, type = type, ...)
            pred <- make_data_frame(tmp[["fitted.values"]], se.fitted = tmp[["se.fit"]])
        } else {
            pred <- make_data_frame(predict(model, type = type, se.fit = FALSE, ...))
        }
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
        }
        # calculate predictions
        if ("se.fit" %in% names(arg)) {
            tmp <- predict(model, newdata = out, type = type, ...)
            # cbind back together
            pred <- make_data_frame(out, tmp[["fitted.values"]], se.fitted = tmp[["se.fit"]])
        } else {
            tmp <- predict(model, newdata = out, type = type, ...)
            # cbind back together
            pred <- make_data_frame(out, tmp[["fitted.values"]], se.fitted = rep(NA_real_, nrow(out)))
        }
        rm(tmp)
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
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else names(at), 
              model.class = class(model),
              type = type,
              category = category)
}
