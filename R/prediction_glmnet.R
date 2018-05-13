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
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else at_specification,
              model.class = class(model),
              type = type)
}
