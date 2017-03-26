#' @importFrom utils head
#' @export
print.prediction <- function(x, digits = 4, ...) {
    f <- x[["fitted"]]
    if (is.null(attributes(x)[["at"]])) {
        if (is.numeric(f)) {
            cat(paste0("Average prediction for ", length(f), " ", ngettext(length(f), "observation", "observations")))
            print(aggregate(fitted ~ 1L, data = x, FUN = mean, na.rm = TRUE), digits = digits, row.names = FALSE, ...)
        } else if (is.factor(f)) {
            m <- sort(table(x[["fitted"]]), decreasing = TRUE)[1]
            cat(paste0("Modal prediction: ", shQuote(names(m)), " for ", m, " of ", length(f), " ", 
                ngettext(length(f), "observation", "observations"),
                " with total ", nlevels(f), " ", ngettext(nlevels(f), "level", "levels") ))
        } else {
            print(head(x), ...)
        }
    } else {
        if (is.numeric(f)) {
            cat(paste0("Average predictions for ", length(f), " ", ngettext(length(f), "observation", "observations")))
        } else if (is.factor(f)) {
            m <- sort(table(x[["fitted"]]), decreasing = TRUE)[1]
            cat(paste0("Modal prediction: ", shQuote(names(m)), " for ", m, " of ", length(f), " ", 
                ngettext(length(f), "observation", "observations"),
                " with total ", nlevels(f), " ", ngettext(nlevels(f), "level", "levels") ))
        } else {
            print(head(x), ...)
        }
    }
    invisible(x)
}

