#' Visualize the simulation of log cutting along the stem using a 5th degree polynomial that describes the tree taper.
#'
#' Plot the shape of the tree and visualize the extracted logs based on the tree measurements, assortments data.frame, and the 5th degree polynomial function that describes the tree's taper.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param coef numerical vector containing six coefficients of the 5th degree polynomial function that describes the tree's taper.
#' @param assortments a data.frame with five columns and n rows, where n is the number of different wood assortments to be obtained from the tree stem. The first column must contain the names of the assortments, the second, numerical, contains the minimum diameters at the small end of the logs, in centimeters. The third column, numerical, contains the minimum lengths of the logs, in meters. The fourth column, numerical, contains the maximum lengths of the logs, in meters. The fifth column, numerical, contains the values in centimeters referring to the loss of wood due to cutting logs. The algorithm prioritizes the extraction of assortments along the stem in the order presented in the data.frame, starting from the first line, to the last.
#' @param stump_height tree cutting height, in meters. Default is 0.
#' @param downgrade if TRUE, the algorithm,from the defect_height onwards, simulates log extraction only for the last assortment in the assortments data.frame. Default is FALSE.
#' @param broken if TRUE, the algorithm will simulate the extraction of logs only up to the defect_height. Default is FALSE.
#' @param defect_height the height, in meters, from which the logs will be downgraded (if downgrade is TRUE) or log extraction simulation will be stopped (if broken is TRUE). Default is h * 0.5.
#' @param lang language in which plot labels will be displayed. Current options are 'en' and 'pt-BR'. Default is 'en'.
#'
#' @return a ggplot object.
#'
#' @details check the `poly5_logs` function help for more details.
#'
#' @examples
#'
#' library(dplyr)
#' library(minpack.lm)
#' library(timbeR)
#'
#' tree_scaling <- tree_scaling %>%
#' mutate(did = di/dbh,
#'        hih = hi/h)
#'
#' poli5 <- lm(did~hih+I(hih^2)+I(hih^3)+I(hih^4)+I(hih^5),tree_scaling)
#'
#' coef_poli <- coef(poli5)
#'
#' dbh <- 25
#' h <- 20
#'
#' assortments <- data.frame(
#'   NAME = c('15-25','4-15'),
#'   SED = c(15,4),
#'   MINLENGTH = c(2.65,2),
#'   MAXLENGTH = c(2.65,4.2),
#'   LOSS = c(5,5)
#' )
#'
#' poly5_logs_plot(dbh, h, coef_poli, assortments)
#'
#' @export
poly5_logs_plot <- function(dbh, h, coef, assortments, stump_height, downgrade, broken, defect_height, lang) {

  if (missing(lang)) {
    lang <- 'eng'
    message("Plot labels will be in english. Supported languages are 'en' and 'pt-BR'. See the `lang` argument.")
  }
  if (missing(stump_height)) {
    stump_height <- 0
  }
  if (missing(downgrade)) {
    downgrade <- FALSE
  }
  if (missing(broken)) {
    broken <- FALSE
  }

  if(!missing(defect_height) & !downgrade & !broken){
    message('The `broken` and `downgrade` arguments are FALSE. The value entered for `defect_height` will be discarded')
  }

  if(broken & downgrade){
    message('The `broken` and `downgrade` arguments are TRUE. The whole tree will be downgraded from `stump_height` to `defect_height`.')
    if(!missing(defect_height)){
      break_height <- defect_height
      defect_height <- 0}
  }

  if (missing(defect_height)) {
    if(downgrade & !broken){
      defect_height <- 0
      message('No value defined for `defect_height`. The whole stem of the tree will be downgraded.')}

    if(broken & !downgrade){
      defect_height <- h * 0.5
      break_height <- defect_height
      message('No value defined for `defect_height`. h * 0.5 will be considered as the break height of the tree.')}

    if(broken & downgrade){
      defect_height <- 0
      break_height <- h * 0.5
      message('No value defined for `defect_height`. h * 0.5 will be considered as the break height of the tree and the whole stem will be downgraded.')
    }

    if(!broken & !downgrade){
      defect_height <- h
    }

  }

  if(!exists('break_height') & !missing(defect_height) & broken){
    break_height <- defect_height
    defect_height <- 0
  }

  if(!exists('break_height')){break_height <- h}

  suppressMessages(
    nlogs <-
      timbeR::poly5_logs(dbh, h, coef, assortments, stump_height, downgrade, broken, defect_height, F)
  )

  stump_label <- dplyr::case_when(lang=='eng'~'Stump',
                                  lang=='pt-BR'~'Toco')
  loss_label <- dplyr::case_when(lang=='eng'~'Loss',
                                 lang=='pt-BR'~'Perda')
  tip_label <- dplyr::case_when(lang=='eng'~'Tip',
                                lang=='pt-BR'~'Ponteira')
  ptitle <- dplyr::case_when(lang=='eng'~'Timber assortments',
                             lang=='pt-BR'~'Processamento de multiplos produtos da madeira')

  psubtitle <- dplyr::case_when(lang=='eng'~
                                  paste0(ifelse(broken,'Broken tree measurements: ','Tree measurements: '), dbh, ' cm in dbh and ',h, ' m tall.'),
                                lang=='pt-BR'~
                                  paste0('Arvore ',ifelse(broken,paste0('quebrada aos ',break_height,' m'),''),'com DAP de ', dbh, ' cm',ifelse(broken,'.',paste0(' e altura total de ', h, ' m.'))))

  pylabel <- dplyr::case_when(lang=='eng'~ 'Height (m)',
                              lang=='pt-BR'~ 'Altura (m)')

  nlogs_assortments <- assortments %>%
    dplyr::mutate(Nlogs = as.vector(t(nlogs$logs)))

  tree <- tibble::tibble(hi = seq(0, ifelse(broken,break_height,h), 0.01),
                         di = timbeR::poly5_di(dbh, h, seq(0, ifelse(broken,break_height,h), 0.01), coef))


  tree_sections <- tibble::tibble(hi = stump_height, description = paste0(stump_label,' (',stump_height*100,'cm)'))

  for (i in 1:nrow(nlogs_assortments)) {
    sort <- nlogs_assortments[i, ]
    if (sort$Nlogs > 0) {
      hi_dpf <- timbeR::poly5_hi(dbh, h, sort[[2]], coef)
      for (j in 1:sort$Nlogs) {
        h0 <- tree_sections %>% dplyr::slice_tail(n = 1) %>% dplyr::pull(hi)
        tree_sections <- tree_sections %>%
          tibble::add_row(
            hi = ifelse(unlist(sort[,4]) + h0 < hi_dpf, unlist(sort[,4]) + h0, round(hi_dpf - h0,2) + h0),
            description = paste0(sort[,1], ' (', ifelse(unlist(sort[,4]) + h0 < hi_dpf, unlist(sort[,4]), round(hi_dpf - h0,2)), 'm)')
          )

        h0 <- tree_sections %>% dplyr::slice_tail(n = 1) %>% dplyr::pull(hi)

        tree_sections <- tree_sections %>%
          dplyr::add_row(
            hi = unlist(sort[,5]) / 100 + h0,
            description = paste0(loss_label, ' (', sort[,5], 'cm)')
          )
      }
    }
  }

  tree_sections <- tree_sections %>%
    tibble::add_row(hi = ifelse(broken,break_height,h),
                    description = paste0(tip_label, ' (', round(h - h0,2), 'm)')) %>%
    dplyr::mutate(
      desc_pos_y = dplyr::case_when(grepl(stump_label, description)~ifelse(hi>0,0.3,NA_real_),
                                    TRUE~(hi + dplyr::lag(hi, 1)) / 2)) %>%
    dplyr::mutate(desc_pos_x = dplyr::case_when(grepl(paste0(loss_label,'|',tip_label,'|',stump_label), description) ~ - timbeR::poly5_di(dbh,h,desc_pos_y,coef),
                                                TRUE ~ timbeR::poly5_di(dbh,h,desc_pos_y,coef)),
                  fontsize = dplyr::case_when(grepl(paste0(loss_label,'|',tip_label,'|',stump_label), description) ~ 3,
                                              TRUE ~ 4),
                  section = timbeR::poly5_di(dbh,h,hi,coef))

  tree %>%
    dplyr::mutate(ri_right = di / 2,
                  ri_left = di / -2) %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(xmin = ri_left, xmax = ri_right, y = hi)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = hi,
        yend = hi,
        x = -section*.5,
        xend = section*.5
      ),
      color = 'darkgray',
      linetype = 'dashed',
      size = .5,
      data = tree_sections %>% dplyr::slice_head(n = nrow(.) - 1)
    ) +

    ggplot2::geom_text(
      ggplot2::aes(
        y = desc_pos_y,
        x = desc_pos_x*.6,
        label = description,
        size = fontsize
      ),
      hjust = 0,
      show.legend = F,
      data = tree_sections %>% dplyr::filter(!grepl(paste0(loss_label,'|',tip_label,'|',stump_label),description))
    )+

    ggplot2::geom_text(
      ggplot2::aes(
        y = desc_pos_y,
        x = desc_pos_x*.6,
        label = description,
        size = fontsize
      ),
      hjust = 1,
      show.legend = F,
      data = tree_sections %>%
        dplyr::filter(grepl(paste0(loss_label,'|',tip_label,'|',stump_label),description))
    )+

    ggplot2::geom_text(
      ggplot2::aes(
        y = desc_pos_y,
        x = 0,
        label = paste0(round(section), 'cm'),
        size = 3
      ),
      hjust = .5,
      vjust=-.5,
      show.legend = F,
      color='white',
      data = tree_sections %>% dplyr::filter(grepl(paste0(loss_label,'|',stump_label),description))
    )+

    ggplot2::scale_size_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(-dbh * 3, dbh * 4)) +
    ggplot2::scale_y_continuous(limits = c(0, h), expand = c(0, 0), breaks = c(0, 1.3, ifelse(broken,break_height,h))) +
    ggplot2::labs(
      title = ptitle,
      subtitle = psubtitle,
      y = pylabel
    ) +
    cowplot::theme_cowplot() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_line(color = 'black', linetype = 'solid'),
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = 'darkgray', linetype =
                                                   'dashed'),
      plot.title.position = 'plot'
    )
}



