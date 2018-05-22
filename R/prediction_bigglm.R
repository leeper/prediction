# @rdname prediction
# @export
prediction.bigglm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = "response",
         calculate_se = TRUE,
         ...) {
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        stop("prediction() for objects of class 'bigglm' only work when 'data' is specified")
    } else {
        # reduce memory profile
        model[["model"]] <- NULL
        
        # setup data
        data <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(data, "at_specification")
        # calculate predictions
        if (isTRUE(calculate_se)) {
            tmp <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp[["fit"]], se.fitted = tmp[["se.fit"]])
        } else {
            tmp <- predict(model, newdata = data, type = type, se.fit = FALSE, ...)
            # cbind back together
            pred <- make_data_frame(data, fitted = tmp, se.fitted = rep(NA_real_, nrow(data)))
        }
    }
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"),
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else at_specification,
              model.class = class(model),
              type = type)
}
