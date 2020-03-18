## Like prediction.default, but without calculate_se

#' @rdname prediction
#' @export
prediction.ridgeLinear <- 
  function(model, 
           data = find_data(model, parent.frame()), 
           at = NULL, 
           type = "response", 
           vcov = stats::vcov(model),
           calculate_se = FALSE,
           ...) {
    if (calculate_se) {
      stop(paste0("calculate_se not implemented"))
    }
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
      pred <- predict(model, type = type, ...)
      pred <- make_data_frame(fitted = pred, se.fitted = rep(NA_real_, length(pred)))
    } else {
      # setup data
      if (!is.null(at)) {
        data <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(data, "at_specification")
      }
      # calculate predictions
      tmp <- predict(model, newdata = data, type = type, ...)
      # cbind back together
      pred <- make_data_frame(data, fitted = tmp, se.fitted = rep(NA_real_, nrow(data)))
    }
    
    # variance(s) of average predictions
    J <- NULL
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
              jacobian = J,
              weighted = FALSE)
  }
