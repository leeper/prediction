#' @rdname prediction
#' @export
prediction.kqr <- function(model, data, at = NULL, calculate_se = FALSE, ...) {
    
    requireNamespace("kernlab")
    
    # extract predicted values
    if (missing(data) || is.null(data)) {
        pred <- make_data_frame(fitted = kernlab::predict(object = model, ...)[,1L])
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        pred <- make_data_frame(fitted = kernlab::predict(model, newdata = data,...)[,1L])
    }
    pred[["se.fitted"]] <- NA_real_
    
    # obs-x-(ncol(data)+2) data frame
    structure(pred, 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(nrow(pred)),
              at = if (is.null(at)) at else at_specification,
              model.class = class(model),
              type = NA_character_)
}
