fit_multi = function(time_col,
                        intensity_col,
                        data,
                        strata_cols ,
                        starting_par = list(y0 = 0.01, r = 0.03, K =  0.8),
                        maxiter=500,
                        nlin = FALSE,
                        estimate_K = FALSE){
  if (missing(data)) {
    stop(gettextf("Missing 'data' argument"))
  }
  if (missing(intensity_col)) {
    stop(gettextf("Missing 'intensity_col' argument"))
  }
  if (missing(time_col)) {
    stop(gettextf("Missing 'time_col' argument"))
  }
  # if (nlin==T & missing(guess_y0)) {
  #   stop(gettextf("Missing 'guess_y0' value"))
  # }
  # if (nlin==T & missing(guess_r)) {
  #   stop(gettextf("Missing 'guess_r' value"))
  # }
  # if (estimate_K == T & missing(guess_K)) {
  #   stop(gettextf("Missing 'guess_K' value"))
  # }




  box = data.frame()
  pred_box = data.frame()

  strata_col = strata_cols
  if(is.null(strata_col)){
    data_uni=data %>%
      dplyr::mutate(strata = "")
    strata_col= "strata"
  }else{
    data_uni = data %>%
      tidyr::unite(strata, strata_col, sep = "---")
  }

  STRATA = data_uni[["strata"]]
  strata = as.character(unique(STRATA))

  for(i in 1:length(strata)){
    rowi = data_uni[["strata"]]==strata[i]
    datai = data_uni[rowi,]

    if(nlin == T & estimate_K == T ){
      model = fit_nlin2(time = datai[[time_col]],
                        y = datai[[intensity_col]],
                        starting_par = starting_par,
                        maxiter=maxiter)
    }
    if(nlin == T & estimate_K == F ){
      model = fit_nlin(time = datai[[time_col]],
                       y = datai[[intensity_col]],
                       starting_par = starting_par[1:2],
                       maxiter = maxiter)}

    if(nlin == F & estimate_K == F){
      model = fit_lin(time = datai[[time_col]],
                                 y = datai[[intensity_col]])
    }
    if(nlin == F & estimate_K == T){
      model = fit_lin(time = datai[[time_col]],
                                 y = datai[[intensity_col]])
      gettextf("'K' is not estimated when nlin = F. To estimate K, use nlin = T and estimate_K = T ")
    }

# Predictions
    lil_pred_box= model$data %>%
      dplyr::mutate(strata = strata[i])
    pred_box = pred_box %>%
      dplyr::bind_rows(lil_pred_box)

#Parameters
    lil_box = model$stats_all %>%
      dplyr::mutate(strata = strata[i])

    box = box %>%
      dplyr::bind_rows(lil_box)

  }
  colnames = colnames(lil_box)[colnames(lil_box)!="strata"]
  colnames_prbox = colnames(lil_pred_box)[colnames(lil_pred_box)!="strata"]


  box2 = box %>%
    dplyr::select("strata",colnames) %>%
    tidyr::separate(strata,into = strata_col, sep = "---")

  pred_box2 = pred_box %>%
    dplyr::select("strata",colnames_prbox) %>%
    tidyr::separate(strata,into = strata_col, sep = "---")
  if(nlin == F & estimate_K == T){
    message("'K' is not estimated when nlin = F. To estimate K, use nlin = T and estimate_K = T ")
  }


  a = list(Parameters = box2,
           Data = pred_box2)

  return(a)


}
