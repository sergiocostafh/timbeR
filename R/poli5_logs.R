#' Estimate the total or partial volume of the tree, based on a 5th degree polynomial function that describes the taper of the tree.
#'
#' Estimates the total or partial volume of the tree from the diameter at breast height, total height, initial section height, final section height and coefficients of the 5th degree polynomial function that describes the tree's taper.
#'
#' @param dbh tree diameter at breast height, in centimeters.
#' @param h total tree height, in meters.
#' @param coef numerical vector containing six coefficients of the 5th degree polynomial function that describes the tree's taper.
#' @param hi final height of the tree section whose volume will be calculated, in meters. Default is the total tree height (h).
#' @param h0 initial height of the tree section whose volume will be calculated, in meters. Default is 0 (ground height).
#'
#' @export
poli5_logs <-
  function(dbh,
           h,
           coef,
           stump_height,
           assortments,
           downgrade,
           eliminate,
           broken,
           defect_height) {

    # Define a altura do toco e calcula o volume do toco
    h0 <- stump_height
    vdiscount <- poli5_vol(dbh, h, h0, coef)

    # Prepara a tabela que receberá os registros de assortments
    tab_sort <-
      (
        assortments %>% select(Sortimento, DPF) %>% pivot_wider(names_from = Sortimento, values_from =
                                                                  DPF)
      )[0, ]

    tab_sort_n <-
      (
        assortments %>% select(Sortimento, DPF) %>% pivot_wider(names_from = Sortimento, values_from =
                                                                  DPF)
      )[0, ]
    # Zera volumes de registros sem dbh, altura, ou que devem ser eliminados do cálculo de volume
    if (any(c(is.na(dbh), is.na(h), eliminate))) {
      tab_sort[1, ] <-
        0
      tab_sort <- tab_sort %>% add_column(`Volume Total` = 0)
    }

    # Início do cálculo de volumes
    else{

      for (i in seq_along(assortments$Sortimento)) {

        # Define diâmetros, comprimentos e perda das toras
        dsort <- assortments[[i, 2]]
        csort <- assortments[[i, 3]]
        psort <- assortments[[i,4]]/100

        # Cálcula o altura em que o DPF do sortimento ocorre na árvore
        harv_dsort <- poli5_hi(dbh, h, dsort, coef)

        # Define a altura máxima em que não ocorre downgrade devido a algum defeito
        if(((downgrade & i<nrow(assortments)) | broken) & harv_dsort>defect_heght){
          harv_dsort <- defect_heght
        }

        nlogs <- 0
        vsort <- 0
        while(h0 <= harv_dsort-csort){
          nlogs <- nlogs+1
          h0 <- h0 + csort + psort
          vsort <- vsort + poli5_vol(dbh, h, h0-psort, coef) - vdiscount
          vloss <- poli5_vol(dbh, h, h0, coef) - (vdiscount + vsort)
          vdiscount <- vdiscount + vsort + vloss
        }


        tab_sort[1, i] <- vsort
        tab_sort_n[1, i] <- nlogs
      }

      tab_sort <- tab_sort %>% add_column(`Volume Total` = ifelse(broken, poli5_vol(dbh, h, defect_heght, coef),poli5_vol(dbh, h, h, coef)))
    }
    return(list(volumes = tab_sort,
                logs = tab_sort_n))
  }
