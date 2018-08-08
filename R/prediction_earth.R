#' @rdname prediction
#' @export
prediction.earth <- 
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
    if (missing(data) || is.null(data)) {
        pred <- make_data_frame(fitted.class = predict(model, type = "class", ...)[,1L])
        probs <- make_data_frame(predict(model, type = type, ...))
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
        tmp <- predict(model, 
                       newdata = out, 
                       type = "class", 
                       ...)
        colnames(tmp) <- "fitted.class"
        tmp_probs <- make_data_frame(predict(model, newdata = out, type = type, ...))
        names(tmp_probs) <- paste0("Pr(", names(tmp_probs), ")")
        # cbind back together
        pred <- make_data_frame(out, tmp, tmp_probs)
        pred[["se.fitted"]] <- NA_real_
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
