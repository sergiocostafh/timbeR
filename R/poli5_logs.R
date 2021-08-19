#' Simulate log extraction using a 5th degree polynomial that describes the taper of the tree.
#'
#' Simulate the extraction of logs from a tree from its measurements, taper function (5th degree polynomial), trunk quality characteristics and harvest parameters such as stump height and assortments.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param coef numerical vector containing six coefficients of the 5th degree polynomial function that describes the tree's taper.
#' @param assortments a data.frame with four columns and n rows, where n is the number of different wood assortments to be obtained from the tree stem. The first column must contain the names of the assortments, the second, numerical, contains the minimum diameters at the small end of the logs, in centimeters. The third column, numerical, contains the lengths of the logs, in meters. The fourth column, numerical, contains the values in centimeters referring to the loss of wood due to cutting logs. The algorithm prioritizes the extraction of assortments along the stem in the order presented in the data.frame, starting from the first line, to the last.
#' @param stump_height tree cutting height, in meters. Default is 0.
#' @param downgrade if TRUE, the algorithm,from the defect_height onwards, simulates log extraction only for the last assortment in the assortments data.frame. Default is FALSE.
#' @param broken if TRUE, the algorithm will simulate the extraction of logs only up to the defect_height. Default is FALSE.
#' @param defect_height the height, in meters, from which the logs will be downgraded (if downgrade is TRUE) or log extraction simulation will be stopped (if broken is TRUE). Default is 0 for downgrade = TRUE (the whole tree is downgraded) and h * 0.5 for broken = TRUE (the tree is broken from half its original/estimated total height).
#' @param eliminate if TRUE, the algorithm does not get logs for any assortment present in the assortments table. All will be zero. Default is FALSE.
#' @param total_volume if TRUE, it adds an additional column to the results data.frame with the estimate of the total volume of the tree, from the ground height to h if broken argument is FALSE, or to defect_height if broken is TRUE. Default is FALSE.
#'
#' @return a list of two data.frames, the first (volumes) with the calculated volumes per assortment, and the second (logs) with the number of logs per assortment.
#'
#' @export
poli5_logs <-
  function(dbh,
           h,
           coef,
           assortments,
           stump_height,
           downgrade,
           broken,
           defect_height,
           eliminate,
           total_volume) {

    if (missing(stump_height)) {
      stump_height <- 0
    }
    if (missing(downgrade)) {
      downgrade <- FALSE
    }
    if (missing(eliminate)) {
      eliminate <- FALSE
    }
    if (missing(broken)) {
      broken <- FALSE
    }
    if(!missing(defect_height) & !downgrade & !broken){
      message('The `broken` and `downgrade` arguments are FALSE. The value entered for `defect_height` will be discarded')
    }

    if(broken & !missing(defect_height)){break_height <- defect_height}

    if(broken & downgrade){
      message('The `broken` and `downgrade` arguments are TRUE. The whole tree will be downgraded from `stump_height` to `defect_height`.')
      if(!missing(defect_height)){break_height <- defect_height}
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

    if(!exists('break_height')){break_height <- h}

    if (missing(total_volume)) {
      total_volume <- F
    }

    h0 <- stump_height

    colnames(assortments) <- c("Assortment", "SED", "Length",
                               "Loss")
    tab_sort <- (assortments %>% dplyr::select(Assortment, SED) %>%
                   tidyr::pivot_wider(names_from = Assortment, values_from = SED))[0,
                   ]
    tab_sort_n <- (assortments %>% dplyr::select(Assortment,
                                                 SED) %>% tidyr::pivot_wider(names_from = Assortment,
                                                                             values_from = SED))[0, ]

    if (any(c(is.na(dbh), is.na(h), eliminate))) {
      tab_sort[1, ] <- 0
      tab_sort <- tab_sort %>% tibble::add_column(`Volume Total` = 0)
    }
    else {
      for (i in seq_along(assortments$Assortment)) {
        dsort <- assortments[[i, 2]]
        csort <- assortments[[i, 3]]
        psort <- assortments[[i, 4]]/100
        harv_dsort <- poli5_hi(dbh, h, dsort, coef)
        if ((downgrade & !broken & i < nrow(assortments)) &
            harv_dsort > defect_height) {
          harv_dsort <- defect_height
        }

        if(broken & !downgrade & harv_dsort > break_height){
          harv_dsort <- break_height
        }

        if(broken & downgrade){
          if(i < nrow(assortments)){
            harv_dsort <- defect_height
          }
          else{
            harv_dsort <- break_height
          }
        }

        nlogs <- 0
        vsort <- 0
        while (h0 <= harv_dsort - csort) {
          nlogs <- nlogs + 1
          h0 <- h0 + csort + psort
          vsort <- vsort + timbeR::poli5_vol(dbh, h, coef, h0 - psort, h0 - (psort + csort))
        }
        tab_sort[1, i] <- vsort
        tab_sort_n[1, i] <- nlogs
      }

      if (total_volume) {
        tab_sort <- tab_sort %>% tibble::add_column(Total = ifelse(broken,
                                                                   timbeR::poli5_vol(dbh, h, coef, break_height),
                                                                   timbeR::poli5_vol(dbh, h, coef, h)))
      }
    }
    return(list(volumes = tab_sort,
                logs = tab_sort_n))
  }
