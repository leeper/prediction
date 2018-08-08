#' @rdname prediction
#' @param lambda For models of class \dQuote{glmnet}, a value of the penalty parameter at which predictions are required.
#' @export
prediction.glmnet <- 
function(
  model, 
  data,
  lambda = model[["lambda"]][1L],
  at = NULL, 
  type = c("response", "link"),
  calculate_se = FALSE,
  ...
) {
    
    # glmnet models only operate with a matrix interface
    
    type <- match.arg(type)
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        warning(sprintf("'data' is required for models of class '%s'", class(model)))
    } else {
        # setup data
        out <- build_datalist(data, at = at, as.data.frame = TRUE)
        at_specification <- attr(out, "at_specification")
        # calculate predictions
        tmp <- predict(model, newx = out, type = type, s = lambda, ...)
        # cbind back together
        pred <- make_data_frame(out, fitted = tmp[, 1L, drop = TRUE], se.fitted = rep(NA_real_, nrow(out)))
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
              weighted = FALSE)
}
