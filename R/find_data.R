#' @title Extract data from a model object
#' @description Find a model frame in a model object or try to reconstruct one
#' @param model The model object.
#' @param \dots Additional arguments passed to methods.
#' @param env An environment in which to look for the \code{data} argument to the modelling call.
#' @return A data.frame, typically with one column unless the variable is a factor with more than two levels.
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
#' @export
find_data.default <- function(model, env = parent.frame(), ...) {
    if (!is.null(model[["call"]][["data"]])) {
        data <- eval(model[["call"]][["data"]], env) 
    } else { 
        data <- get_all_vars(model[["terms"]], data = model[["model"]])
    }
    data
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
