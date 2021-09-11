#' Estimate the height of trees.
#'
#' Estimate the height of trees using one of the twelve hypsometric models in the `timbeR` package.
#'
#' @param data tree measurement data.
#' @param model one of the twelve hypsometric models present in the `fit_hypso` function.
#' @param transform transformation to be performed on the predicted values. Use the `transform` element from the list resulting from the `best_hypso_model` function.
#' @param dbh_col the name of the column containing the values from diameter to breast height. Character. Required for Naslund and Prodan functions.
#'
#' @details check the `fit_hypso` function help for more details.
#'
#' @return a numerical vector containing the height estimates.
#'
#' @export
predict_h <- function(data, model, transform, dbh_col) {
  if (dplyr::is.grouped_df(data)) {
    message('Ungrouping the data.')
    data <- data %>% dplyr::ungroup()
  }

  data <- data %>% tibble::as.tibble()

  if (transform == 'none') {
    pred <- predict(model, data)
  }
  else{
    if (transform == 'log') {
      pred <- exp(predict(model, data))
    }
    else{
      if (transform == 'sqrt(1/y)') {
        pred <- (1 / predict(model, data))^2
      }
      else{
        if (transform == 'x^2/y+1.3') {

          if (missing(dbh_col)) {
            stop('`dbh_col` argument is needed to transform Naslund functions predictions.')
          }

          pred <- data %>%
            dplyr::mutate(pred_h = predict(model, data)) %>%
            dplyr::mutate_(.dots = setNames(list(
              lazyeval::interp(
                quote(dbh ^ 2 / pred_h + 1.3),
                dbh = as.name(dbh_col),
                pred_h = as.name('pred_h')
              )
            ), 'y')) %>%
            dplyr::pull(y)
        }
        else{
          if (missing(dbh_col)) {
            stop('`dbh_col` argument is needed to transform Prodan functions predictions.')
          }

          pred <- data %>%
            dplyr::mutate(pred_h = predict(model, data)) %>%
            dplyr::mutate_(.dots = setNames(list(
              lazyeval::interp(
                quote(dbh ^ 2 / pred_h),
                dbh = as.name(dbh_col),
                pred_h = as.name('pred_h')
              )
            ), 'y')) %>%
            dplyr::pull(y)
        }
      }
    }
  }
  return(pred)
}
