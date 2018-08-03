#' @rdname mean_or_mode
#' @title Class-dependent variable aggregation
#' @description Summarize a vector/variable into a single number, either a mean (median) for numeric vectors or the mode for categorical (character, factor, ordered, or logical) vectors. Useful for aggregation.
#' @param x A vector.
#' @return A numeric or factor vector of length 1.
#' @examples
#' require("datasets")
#' # mean for numerics
#' mean_or_mode(iris)
#' mean_or_mode(iris[["Sepal.Length"]])
#' mean_or_mode(iris[["Species"]])
#'
#' # median for numerics
#' median_or_mode(iris)
#' 
#' @seealso \code{\link{prediction}}, \code{\link{build_datalist}}, \code{\link{seq_range}}
#' @import stats
#' @export
mean_or_mode <- function(x) {
    UseMethod("mean_or_mode")
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.default <- function(x) {
    if (!is.factor(x)) {
        x <- as.factor(x)
    }
    factor(names(sort(table(x), decreasing = TRUE))[1L], levels = levels(x))
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.numeric <- function(x) {
    mean(x, na.rm = TRUE)
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.data.frame <- function(x) {
    setNames(lapply(x, mean_or_mode), names(x))
}

#' @rdname mean_or_mode
#' @export
median_or_mode <- function(x) {
    UseMethod("median_or_mode")
}

#' @rdname mean_or_mode
#' @export
median_or_mode.default <- function(x) {
    if (!is.factor(x)) {
        x <- as.factor(x)
    }
    factor(names(sort(table(x), decreasing = TRUE))[1L], levels = levels(x))
}

#' @rdname mean_or_mode
#' @export
median_or_mode.numeric <- function(x) {
    median(x, na.rm = TRUE)
}

#' @rdname mean_or_mode
#' @export
median_or_mode.data.frame <- function(x) {
    setNames(lapply(x, median_or_mode), names(x))
}
