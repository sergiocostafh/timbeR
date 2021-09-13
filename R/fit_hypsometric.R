#' Fit of hypsometric models.
#'
#' Fit and evaluation of twelve hypsometric models.
#'
#' @param data tree measurement data.
#' @param h_col the name of the column containing the height values. Character.
#' @param dbh_col the name of the column containing the diameters at breast height. Character.
#'
#' @details Twelve models are fitted and evaluated using this function. The models are 'Linear', 'Trorey', 'Cubic', 'Prodan', 'Prodan2', 'Assman', 'Henricksen', 'Stoffel', 'Curtis', 'Petterson', 'Naslund' and 'Mean'.
#'
#' The fit statistics calculated to evaluate the models are the adjusted R² (`adj_r_squared`) and the standard error of the estimates in percentage (`syx_perc`).
#'
#' @return a list containing four elements. The first one, named `model_evaluation` is a data.frame containing twelve rows with the names, formulas and evaluation parameters of the fitted models. The second, named `models`, is a list containing the twelve `lm` models. The third, named `curves`, is a data.frame containing the heights predicted by each of the twelve fitted models, as well as the observed dbh values and the name of the models. The last, named `obs_data` is a data.frame with two columns referring to the observed values of dbh (`dbh_col`) and height (`h_col`).
#'
#' @export
fit_hypsometric <- function(data, h_col, dbh_col){


  if(missing(dbh_col)){
    stop('Missing argument: `dbh_col`')
  }

  if(missing(h_col)){
    stop('Missing argument: `h_col`')
  }

  if(min(data %>% dplyr::pull(!!h_col)) <= 0) {
    stop('Check the data. It must not contain heights less than or equal to zero.')
  }

  if(is.grouped_df(data)){
    message('Ungrouping the data.')
    data <- data %>% dplyr::ungroup()
  }

  df_hypsometrics <- data.frame(
    name = c('Linear',
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
             'Mean'),
    formula = c('y~x',
                'y~x+I(x^2)',
                'y~x+I(x^2)+I(x^3)',
                'I(x^2/y)~x+I(x^2)',
                'I(x^2/(y-1.3))~x+I(x^2)',
                'y~I(1/x^2)',
                'y~I(log(x))',
                'I(log(y))~I(log(x))',
                'I(log(y))~I(1/x)',
                'I(sqrt(1/(y)))~I(1/x)',
                'I(x^2/(y-1.3))~I(x^2)',
                'y~1'
    ),
    transform = c('none',
                  'none',
                  'none',
                  'x^2/',
                  'x^2/y+1.3',
                  'none',
                  'none',
                  'log',
                  'log',
                  'sqrt(1/y)',
                  'x^2/y+1.3',
                  'none')
  ) %>%
    dplyr::mutate(formula = gsub('y',h_col,formula)) %>%
    dplyr::mutate(formula = gsub('x',dbh_col,formula))

  formulas <- df_hypsometrics$formula
  transform <- df_hypsometrics$transform

  df_fits <-
    mapply(function(formula, transform, data) {

      f.hypso <- paste(formula, collapse = " ")

      model <- lm(formula = formula(f.hypso), data = data)

      pred <- timbeR::predict_h(data,model,transform,dbh_col)
      pred1mm <- timbeR::predict_h(data.frame(setNames(list(0.1), dbh_col)),model,transform,dbh_col)

      h <- data %>% dplyr::pull(!!h_col)
      dbh <- data %>% dplyr::pull(!!dbh_col)

      n <- nrow(data)

      k <- length(coef(model))-1

      syx_perc <- sqrt(sum((pred-h)^2)/(n-length(coef(model))))/
        mean(h)

      r_squared <- 1-sum((h-unname(pred))^2)/sum((h-mean(h))^2)

      adj_r_squared <- 1 - ((1-r_squared)*(n-1)/(n-k-1))

      pred_data <- data.frame(dbh, h = pred) %>%
        dplyr::arrange(dbh)

      decr_pred_lower_range <- pred_data$h[1] > pred_data$h[2]
      decr_pred_upper_range <- pred_data$h[nrow(pred_data)] < pred_data$h[nrow(pred_data)-1]
      neg_pred_obs_range <- any(pred_data$h <= 0)
      neg_pred_close_zero <- pred1mm<0

      name <- df_hypsometrics$name[match(f.hypso,formulas)]
      transform <- df_hypsometrics$transform[match(f.hypso,formulas)]

      formula <- f.hypso

      curve_dbh <- set_names(list(seq(min(round(dbh,1), na.rm = T),max(round(dbh,1), na.rm = T),.1)), dbh_col)
      curve_h <- setNames(list(timbeR::predict_h(curve_dbh,model,transform, dbh_col)), h_col)
      curve <- dplyr::bind_cols(curve_dbh, curve_h) %>%
        dplyr::mutate(name = name)

      model_tbl <- data.frame(name, formula, transform, syx_perc, adj_r_squared, decr_pred_lower_range, decr_pred_upper_range, neg_pred_obs_range, neg_pred_close_zero)

      return(list(list(model_tbl, model, curve)))
    },
    formula = df_hypsometrics$formula,
    transform = df_hypsometrics$transform,
    MoreArgs = list(data=data))

  model_evaluation <- dplyr::bind_rows(lapply(df_fits, `[[`, 1))
  models <- lapply(df_fits, `[[`, 2)
  names(models) <- df_hypsometrics$name
  curves <- dplyr::bind_rows(lapply(df_fits, `[[`, 3))

  obs_data <- data %>% dplyr::select(!!h_col,!!dbh_col)

  return(list(model_evaluation = model_evaluation, models = models, curves = curves, obs_data = obs_data))

}
