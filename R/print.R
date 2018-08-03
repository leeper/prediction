#' @export
print.prediction <- function(x, digits = 4, ...) {
    
    # gather metadata
    f <- x[["fitted"]]
    fc <- x[["fitted.class"]]
    ## at
    at <- attributes(x)[["at"]]
    at_names <- setdiff(names(attr(x, "at")), "index")
    
    ## weights
    is_weighted <- attr(x, "weighted")
    if (isTRUE(is_weighted)) {
        wts <- x[["_weights"]]
    }
    
    # calculate overall predictions
    ## if no 'at_specification', simply calculate overall average/mode and print
    if (is.null(at)) {
        # object is a single replication with no 'at' specification
        if ("fitted.class" %in% names(x) || is.list(fc)) {
            # factor outcome
            m <- sort(table(x[["fitted.class"]]), decreasing = TRUE)[1L]
            message(
              sprintf("Data frame with %d %s%swith modal prediction (of %d %s):",
                      length(fc),
                      ngettext(length(fc), "prediction", "predictions"),
                      if (!is.null(attr(x, "call"))) sprintf(" from\n %s\n", paste0(deparse(attr(x, "call")), collapse = "\n")) else "",
                      nlevels(factor(fc)),
                      ngettext(nlevels(f), "level", "levels"),
                      shQuote(names(m))
              )
            )
        } else {
            # numeric outcome
            message(
              sprintf("Data frame with %d %s%swith average prediction: %s",
                      length(f),
                      ngettext(length(fc), "prediction", "predictions"),
                      if (!is.null(attr(x, "call"))) sprintf(" from\n %s\n", paste0(deparse(attr(x, "call")), collapse = "\n")) else "",
                      sprintf(paste0("%0.", digits, "f"), mean(f, na.rm = TRUE))
              )
            )
        }
    } else {
        # otherwise, object has an 'at' specification, reflecting multiple requested predictions
        
        # convert 'at_specification' into data frame
        xby <- x[ , setdiff(names(at), "index"), drop = FALSE]
        
        if ("fitted.class" %in% names(x) || is.list(fc)) {
            # factor outcome
            out <- aggregate(x[["fitted.class"]], xby, FUN = function(set) names(sort(table(set), decreasing = TRUE))[1L])
            message(
              sprintf("Data frame with %d %s%swith modal %s (of %d %s):",
                      nrow(x),
                      ngettext(nrow(x), "prediction", "predictions"),
                      if (!is.null(attr(x, "call"))) sprintf(" from\n %s\n", paste0(deparse(attr(x, "call")), collapse = "\n")) else "",
                      ngettext(nrow(out), "prediction", "predictions"),
                      nlevels(factor(fc)),
                      ngettext(nlevels(fc), "level", "levels")
              )
            )
        } else {
            # numeric outcome
            out <- aggregate(x[["fitted"]], xby, FUN = mean, na.rm = TRUE)
            message(
              sprintf("Data frame with %d %s%swith average %s:",
                      nrow(x),
                      ngettext(nrow(x), "prediction", "predictions"),
                      if (!is.null(attr(x, "call"))) sprintf(" from\n %s\n", paste0(deparse(attr(x, "call")), collapse = "\n")) else "",
                      ngettext(nrow(out), "prediction", "predictions")
              )
            )
        }
        print(out, digits = digits, row.names = FALSE, ...)
    }
    
    # return invisibly
    invisible(x)
}
