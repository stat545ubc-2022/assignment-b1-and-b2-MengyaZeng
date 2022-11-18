#' @description
#' This function will return the standard error of the linear model fitted for vector x, y. Since we are fitting a linear model, none-numeric vectors will not be accepted, also we would expect the length of these two vectors to match. The input vector can have NA as long as we have enough numeric elements to fit our model.
#'
#' @title linear model standard error
#' @import broom
#' @param x - corresponds to is the "X" vector that we want to fit our linear regression model for,
#' @param y - corresponds to is the "Y" vector that we want to fit our linear regression model for
#' @param dataset - the dataset that contains the x, y variable, this dataset variable is set as an empty vector by default if the user wishes to input the x, y vector directly.
#' @return return the standard error of the linear model fitted for vector x, y.
#' @export
#'
#' @examples
#' lm_std(c(1,2,3), c(4,5,7))
#' # should return an integer which is the standard error of the linear model
#' # fitted for (c(4,5,7) ~ c(1,2,3))
#' lm_std(datateachr::cancer_sample$radius_mean, datateachr::cancer_sample$perimeter_mean)
#' # should return an integer which is the standard error of the linear model
#' # fitted for variable (perimeter_mean ~ radius_mean) inside dataset cancer_sample
lm_std <- function( x, y, dataset=c()) {
  stopifnot(is.numeric(x) && is.numeric(y))
  stopifnot(length(x) == length(y))
  my_lm <- lm(y ~ x, data = dataset)
  tidy_lm <- broom::tidy(my_lm)
  tidy_lm$std.error[2]# 2 is for extracting the standard error row corresponding to our x variable
}





