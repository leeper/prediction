#' @rdname prediction
#' @export
prediction.clm <- 
function(model, 
         data = find_data(model, parent.frame()), 
         at = NULL, 
         type = NULL, 
         calculate_se = TRUE,
         category, 
         ...) {
    
    if (!is.null(type)) {
        warning(sprintf("'type' is ignored for models of class '%s'", class(model)))
    }
    
    # extract predicted values
    data <- data
    if (missing(data) || is.null(data)) {
        pred <- make_data_frame(fitted.class = predict(model, type = "class", se.fit = FALSE, ...)[["fit"]])
        if (isTRUE(calculate_se)) {
            problist <- predict(model, newdata = data, type = "prob", se.fit = TRUE, ...)
            probs <- make_data_frame(problist[["fit"]])
            probs.se <- make_data_frame(problist[["se.fit"]])
            names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
            names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")
            pred <- make_data_frame(pred, probs, probs.se)
        } else {
            problist <- predict(model, newdata = data, type = "prob", se.fit = FALSE, ...)
            probs <- make_data_frame(problist[["fit"]])
            names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
            pred <- make_data_frame(pred, probs)
        }
    } else {
        # setup data
        if (is.null(at)) {
            out <- data
        } else {
            out <- build_datalist(data, at = at, as.data.frame = TRUE)
            at_specification <- attr(out, "at_specification")
        }
        # calculate predictions
        pred <- predict(model, newdata = out, type = "class", se.fit = FALSE, ...)[["fit"]]
        if (isTRUE(calculate_se)) {
            problist <- predict(model, newdata = out, type = "prob", se.fit = TRUE, ...)
            probs <- make_data_frame(problist[["fit"]])
            probs.se <- make_data_frame(problist[["se.fit"]])
            names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
            names(probs.se) <- paste0("se.Pr(", seq_len(ncol(probs)), ")")    
            pred <- make_data_frame(out, fitted.class = pred, probs, probs.se)
        } else {
            problist <- predict(model, newdata = out, type = "prob", se.fit = FALSE, ...)
            probs <- make_data_frame(problist[["fit"]])
            names(probs) <- paste0("Pr(", seq_len(ncol(probs)), ")")
            pred <- make_data_frame(out, fitted.class = pred, probs)
        }
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
