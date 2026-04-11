#' Plot fitted epidemic models
#'
#' Create a faceted `ggplot2` panel showing observed and fitted values for the
#' selected epidemic models.
#'
#' @param object A fitted object returned by `fit_lin()`, `fit_nlin()`, or
#'   `fit_nlin2()`.
#' @param point_size Point size for observed values.
#' @param line_size Line width for fitted curves.
#' @param models Character vector with the models to display.
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' epi <- sim_logistic(N = 30, y0 = 0.01, dt = 5, r = 0.3, alpha = 0.2, n = 4)
#' fit <- fit_lin(time = epi$time, y = epi$random_y)
#' plot_fit(fit)
#'
#' @export
plot_fit <- function(object,
                     point_size = 1.2,
                     line_size = 1,
                     models = c("Exponential", "Monomolecular", "Logistic", "Gompertz")) {
  if (missing(object)) {
    stop(gettextf("Missing 'object'"))
  }

  model = time = y = NULL

  if(!is.matrix(object$`Maximum disease intensity`)) {

    models2 <- c("Exponential", "Monomolecular", "Logistic", "Gompertz")
    selected_models <- models

    model_inf <- tibble::rownames_to_column(as.data.frame(object$`Infection rate`), var = "model")
    model_inoc <- tibble::rownames_to_column(as.data.frame(object$`Initial inoculum`), var = "model")

    df <- data.frame(model = models2) %>%
      dplyr::mutate(model = as.character(model))



    df$fun <- c(
      Exponential = function(y0, r, time) y0 * exp(r * time),
      Monomolecular = function(y0, r, time) 1 - (1 - y0) * exp(-r * time),
      Logistic = function(y0, r, time) 1 / (1 + ((1 - y0) / y0) * exp(-r * time)),
      Gompertz = function(y0, r, time) 1 * exp(log(y0 / 1) * exp(-r * time))
    )

    plot_data <- object$data %>%
      dplyr::left_join(model_inf, by = "model") %>%
      dplyr::left_join(model_inoc, by = "model") %>%
      dplyr::left_join(df, by = "model")


    base <- ggplot2::ggplot()

    for (i in 1:length(selected_models)) {
      plot_data_sep <- plot_data %>%
        dplyr::filter(model == selected_models[i])
      r_par <- base::unique(plot_data_sep$Estimate.x)
      y0_par <- base::unique(plot_data_sep$Estimate.y)

      base <- base +
        ggplot2::geom_point(data = plot_data_sep,
                            ggplot2::aes(x = time, y = y, color = model),
                            size = point_size) +
        # stat_function(data = plot_data_sep,aes(x = time, color = model),
        # fun = df$fun[[selected_models[i]]],args = list(r = r_par ,y0 =y0_par), size =line_size)+
        ggplot2::geom_line(
          stat = "function", data = plot_data_sep, ggplot2::aes(x = time, color = model),
          fun = df$fun[[selected_models[i]]], args = list(r = r_par, y0 = y0_par), linewidth = line_size
        ) +
        ggplot2::facet_wrap(~model)
    }





  }else{

    models2 <- c("Monomolecular", "Logistic", "Gompertz")
    selected_models <- models

    model_inf <- tibble::rownames_to_column(as.data.frame(object$`Infection rate`), var = "model")
    model_inoc <- tibble::rownames_to_column(as.data.frame(object$`Initial inoculum`), var = "model")
    model_K <- tibble::rownames_to_column(as.data.frame(object$`Maximum disease intensity`), var = "model")

    df <- data.frame(model = models2) %>%
      dplyr::mutate(model = as.character(model))






    df$fun <- c(
      Monomolecular = function(y0, r, K, time) K*(1 - ((K - y0)/K) * exp(-r * time)),
      Logistic = function(y0, r, K, time) K / (1 + ((K - y0) / y0) * exp(-r * time)),
      Gompertz = function(y0, r, K, time) K * exp(log(y0 / K) * exp(-r * time))
    )




    plot_data <- object$data %>%
      dplyr::left_join(model_inf, by = "model") %>%
      dplyr::left_join(model_inoc, by = "model") %>%
      dplyr::left_join(model_K, by = "model") %>%
      dplyr::left_join(df, by = "model")


    base <- ggplot2::ggplot()

    for (i in 1:length(selected_models)) {
      plot_data_sep <- plot_data %>%
        dplyr::filter(model == selected_models[i])
      r_par <- base::unique(plot_data_sep$Estimate.x)
      y0_par <- base::unique(plot_data_sep$Estimate.y)
      K_par <-  base::unique(plot_data_sep$Estimate)
      base <- base +
        ggplot2::geom_point(data = plot_data_sep,
                            ggplot2::aes(x = time, y = y, color = model),
                            size = point_size) +
        # stat_function(data = plot_data_sep,aes(x = time, color = model),
        # fun = df$fun[[selected_models[i]]],args = list(r = r_par ,y0 =y0_par), size =line_size)+
        ggplot2::geom_line(
          stat = "function", data = plot_data_sep, ggplot2::aes(x = time, color = model),
          fun = df$fun[[selected_models[i]]], args = list(r = r_par, y0 = y0_par, K = K_par), linewidth = line_size
        ) +
        ggplot2::facet_wrap(~model)
    }
  }

  base <- base +
    cowplot::theme_half_open(font_size = 12) +
    cowplot::background_grid(major = "y", minor = "none")

  return(base)
}
