#' Get the best hypsometric model.
#'
#' Get the best model from a list generated from the `fit_hypsometric` function.
#'
#' @param fit_hypso_list list resulting from the `fit_hypsometric` function.
#' @param criteria criteria for selecting the best model. Valid options are `syx`, `adj_r_squared` or `both`. Optional. Default is `syx`. Check the details for more information.
#' @param model_name force the selection of a specific model. Character. Optional. Check the details for more information.
#'
#' @details When the selection criterion is `syx`, the model that resulted in the smallest standard error of the estimates will be selected. If the criterion selected by `adj_r_squared`, the selected model will be the one that resulted in the highest value for the adjusted coefficient of determination. If the chosen criterion is `both`, the best ranked model considering both criteria will be selected.
#'
#' To force the use of a specific model, declare one of the twelve models in the `model_name` argument. Valid options are: Linear', 'Trorey', 'Cubic', 'Prodan', 'Prodan2', 'Assman', 'Henricksen', 'Stoffel', 'Curtis', 'Petterson', 'Naslund' and 'Mean'.
#'
#' @return a list containing three elements. The element `name` is the first one, containing the name of the selected model. The second, named `model` is the `lm` object. The third, named `transform`, is a string to be used in the `predict_h` function.
#'
#' @export
best_hypsometric_model <- function(fit_hypsometric_list, criteria, model_name){

  if (missing(criteria))
    criteria <- 'syx'

  if (missing(model_name)) {
    if (!criteria %in% c('syx', 'adj_r_squared', 'both')) {
      stop(
        paste0(
          '`',
          criteria,
          '` is not a valid value for `criteria`. Valid options are `syx`, `adj_r_squared` and `both`.'
        )
      )
    }

    model_df <- fit_hypsometric_list[[1]][, c(1, 3:4)]

    if (criteria == 'syx') {
      model <-
        model_df %>% dplyr::arrange(syx_perc) %>% dplyr::slice_head(n = 1) %>% dplyr::pull(name)
    }

    if (criteria == 'adj_r_squared') {
      model <-
        model_df %>% dplyr::arrange(-adj_r_squared) %>% dplyr::slice_head(n = 1) %>% dplyr::pull(name)
    }

    if (criteria == 'both') {
      model <- model_df %>%
        dplyr::arrange(syx_perc) %>%
        dplyr::mutate(score1 = dplyr::row_number()) %>%
        dplyr::arrange(-adj_r_squared) %>%
        dplyr::mutate(score2 = dplyr::row_number()) %>%
        dplyr::mutate(score = score1 + score2) %>%
        dplyr::arrange(score) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::pull(name)
    }

  }

  else{
    if (!model_name %in% c(
      'Linear',
      'Trorey',
      'Cubic',
      'Prodan',
      'Prodan2',
      'Assman',
      'Henricksen',
      'Stoffel',
      'Curtis',
      'Petterson',
      'Naslund',
      'Mean'
    )) {
      stop(
        paste0(
          '`',
          model_name,
          '` is not a valid name for `model_name` argument.\nThe available models are: `Linear`, `Trorey`, `Cubic`, `Prodan`, `Prodan2`, `Assman`, `Henricksen`, `Stoffel`, `Curtis`, `Petterson`, `Naslund` and `Mean`.'
        )
      )
    }
    else{
      model <-  fit_hypsometric_list[[1]][, c(1, 3:4)] %>%
        dplyr::filter(name == model_name) %>%
        dplyr::pull(name)
    }

  }

  return(list(name = fit_hypsometric_list[[1]] %>% dplyr::filter(name==model) %>% dplyr::pull(name),
         model = fit_hypsometric_list[[2]][[model]],
         transform = fit_hypsometric_list[[1]] %>% dplyr::filter(name==model) %>% dplyr::pull(transform)))

}
