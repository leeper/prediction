#' @rdname prediction
#' @export
prediction.mnp <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = NULL, 
         calculate_se = FALSE,
         category, 
         ...) {
    
    if (!is.null(type)) {
        warning(sprintf("'type' is ignored for models of class '%s'", class(model)))
    }
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        probs <- make_data_frame(predict(model, type = "prob", ...)[["p"]])
        names(probs) <- paste0("Pr(", names(probs), ")")
        tmp <- predict(model, type = "choice", ...)[["y"]]
        d <- dim(tmp)
        if (length(d) == 3) {
            stop("'prediction.mnp' only works when 'n.draws = 1'")
        }
        probs[["fitted.class"]] <- lapply(seq_len(d[1L]), function(i) tmp[i,])
        pred <- probs
        rm(probs, tmp)
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        tmp_probs <- make_data_frame(predict(model, newdata = out, type = "prob", ...)[["p"]])
        names(tmp_probs) <- paste0("Pr(", names(tmp_probs), ")")
        tmp <- predict(model, newdata = out, type = "choice", ...)[["y"]]
        d <- dim(tmp)
        if (length(d) == 3) {
            stop("'prediction.mnp' only works when 'n.draws = 1'")
        }
        tmp_probs[["fitted.class"]] <- lapply(seq_len(d[1L]), function(i) tmp[i,])
        # cbind back together
        pred <- make_data_frame(out, tmp_probs)
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
