#' @title Extract data from a model object
#' @description Attempt to reconstruct the data used to create a model object
#' @param model The model object.
#' @param \dots Additional arguments passed to methods.
#' @param env An environment in which to look for the \code{data} argument to the modelling call.
#' @details This typically requires that a model object be specified using a formula interface and an explicit \code{data} argument. Models that can be specified using variables from the \code{.GlobalEnv} or with a non-formula interface (e.g., a matrix of data) will tend to generate errors.
#' @return A data frame, typically with one column unless the variable is a factor with more than two levels.
#' @examples
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
#' find_data(x)
#' 
#' @seealso \code{\link{prediction}}, \code{\link{build_datalist}}, \code{\link{mean_or_mode}}, \code{\link{seq_range}}
#' @export
find_data <- function(model, ...) {
    UseMethod("find_data")
}

#' @rdname find_data
#' @importFrom stats terms
#' @export
find_data.default <- function(model, env = parent.frame(), ...) {
    form <- try(terms(model), silent = TRUE)
    if (inherits(form, "try-error")) {
        # if no terms, then model was created without a formula interface
        stop("'find_data()' requires a formula call")
    } else if (is.null(model[["call"]])) {
        stop("'find_data()' requires a formula call")
    } else  {
        dat <- eval(model[["call"]][["data"]], env)
        if (inherits(dat, "try-error")) {
            dat <- get_all_vars(model, data = model[["call"]][["data"]])
        }
        # handle subset
        if (!is.null(model[["call"]][["subset"]])) {
            dat <- dat[eval(model[["call"]][["subset"]], dat), , drop = FALSE]
        }
        # handle na.action
        if (!is.null(model[["na.action"]])) {
            dat <- dat[-model[["na.action"]], , drop = FALSE]
        }
    }
    dat
}

#' @rdname find_data
#' @export
find_data.data.frame <- function(model, ...) {
    model
}

#' @rdname find_data
#' @export
find_data.lm <- find_data.default

#' @rdname find_data
#' @export
find_data.glm <- find_data.default

#' @rdname find_data
#' @export
find_data.svyglm <- function(model, ...) {
    data <- model[["data"]]
    data
}
#' @rdname find_data
#' @importFrom stats model.frame
#' @export
find_data.merMod <- function(model, env = parent.frame(), ...) {
    model.frame(model)
}

#' @rdname find_data
#' @export
find_data.crch <- find_data.default

#' @rdname find_data
#' @export
find_data.hxlr <- find_data.default
