#' Extract values of used or fitted history parameter
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param param Paramtere name: \code{"tau"}, \code{"mixed_state"},
#' or \code{"history_mix"}.
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param includePopulationLevel Logical, for pooled random effect only. Whether to include
#' population mean as a separate \code{"_population} level, default to \code{TRUE}.
#' @param ...
#'
#' @return A vector, if summary was not requested. Or a tibble with a summary or if a fixed value was used.
#'
#' @export
#'
#' @examples
#' #' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' history_parameter(br_fit, "tau")
#' }

history_parameter.cumhist <- function(object, param, summary=TRUE, probs=c(0.055, 0.945), includePopulationLevel = TRUE, ...){
  if (is.null(object$stanfit)) stop("The object has no fitted stan model")

  # link functions
  lf <- list("tau" = exp, "mixed_state"=boot::inv.logit, "history_mix"=boot::inv.logit)

  # sanity check
  if (!param %in% c("tau", "mixed_state", "history_mix")) stop("Unknown history parameter")

  # trivial case of user-supplied single constant value
  if (object$data[[paste0(param, '_option')]] == 1) {
    return(tibble(Estimate = object$data[[paste0('fixed_', param)]]))
  }

  # single fitted value
  if (object$data[[paste0(param, '_option')]] == 2){
    param_mu <- rstan::extract(object$stanfit, pars=paste0(param, "_mu"))[[paste0(param, "_mu")]]
    param_mu <- lf[[param]](param_mu)

    # row samples
    if (!summary) return(param_mu)

    # just the mean
    if (is.null(probs)) return(tibble(Estimate = mean(param_mu)))

    # average and quantiles
    avg <- tibble::tibble(Estimate = mean(param_mu))
    return(bind_cols(avg, as_tibble(t(quantile(param_mu, probs = c(0.055, 0.945))))))
    return(avg)
  }

  # independent random
  if (object$data[[paste0(param, '_option')]] == 3){
    param_rnd <- rstan::extract(object$stanfit, pars=paste0(param, "_rnd"))[[paste0(param, "_rnd")]]
    param_rnd <- lf[[param]](param_rnd)

    # putting data and random factor levels into a single table
    df <- tibble::tibble(Random = rep(levels(factor(object$data$random)), each=nrow(param_rnd)),
                         Estimate = c(param_rnd))

    # raw samples
    if (!summary) return(df)
  }

  # pooled random
  if (object$data[[paste0(param, '_option')]] == 4){
    # getting population parameters and a z-score
    param_mu <- rstan::extract(object$stanfit, pars=paste0(param, "_mu"))[[paste0(param, "_mu")]]
    param_sigma <- rstan::extract(object$stanfit, pars=paste0(param, "_sigma"))[[paste0(param, "_sigma")]]
    param_z <- rstan::extract(object$stanfit, pars=paste0(param, "_rnd"))[[paste0(param, "_rnd")]]

    # computing individual values
    param_random <- rep(param_mu, ncol(param_z)) + rep(param_sigma, ncol(param_z)) * c(param_z)

    if (includePopulationLevel){
      # putting data and random factor levels into a single table
      df <- tibble::tibble(Random = rep(c("_population", levels(factor(object$data$random))), each=nrow(param_z)),
                           Estimate = lf[[param]](c(c(param_mu), param_random)))
    }
    else {
      df <- tibble::tibble(Random = rep(levels(factor(object$data$random)), each=nrow(param_z)),
                           Estimate = lf[[param]](c(param_random)))
    }

    # raw samples
    if (!summary){
      colnames(df) <- c(object$random_effect, param)
      return(df)
    }
  }

  # averages only
  avg <-
    df %>%
    dplyr::group_by(Random) %>%
    dplyr::summarise(Estimate = mean(Estimate), .groups="keep") %>%
    dplyr::ungroup()

  # quantiles, if requested
  if (!is.null(probs)){
    quantiles <-
      df %>%
      dplyr::group_by(Random) %>%
      tidyr::nest() %>%
      dplyr::mutate(CI = purrr::map(data, ~tibble::as_tibble(t(apply(as.matrix(.$Estimate), MARGIN=2, FUN=quantile, probs=probs))))) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(cols=CI)

    avg <-dplyr::left_join(avg, quantiles, by="Random")
  }

  # returning summary for random independent and pooled
  colnames(avg)[1] <- object$random_effect
  return(avg)
}

#' @export
#' @keywords internal
history_parameter <- function(object, ...) { UseMethod("history_parameter") }

#' Extract values of used or fitted history parameter tau
#'
#' A short-cut for \code{history_parameter(object, "tau", ...)}.
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param ...
#'
#' @return A single value, if fixed value was used. A vector or a tibble, depending on the
#' option used (single intercept, independent or random intercepts), and whether summary was
#' requested.
#' @export
#'
#' @examples
#' #' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' history_tau(br_fit)
#' }
history_tau.cumhist <- function(object, summary=TRUE, probs=c(0.055, 0.945), ...){
  bistablehistory::history_parameter.cumhist(object, "tau", summary, probs)
}

#' @export
#' @keywords internal
history_tau <- function(object, ...) { UseMethod("history_tau") }


#' Extract values of used or fitted history parameter mixed_state
#'
#' A short-cut for \code{history_parameter(object, "mixed_state", ...)}.
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param ...
#'
#' @return A single value, if fixed value was used. A vector or a tibble, depending on the
#' option used (single intercept, independent or random intercepts), and whether summary was
#' requested.
#' @export
#'
#' @examples
#' #' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' history_tau(br_fit)
#' }
history_mixed_state.cumhist <- function(object, summary=TRUE, probs=c(0.055, 0.945), ...){
  bistablehistory::history_parameter.cumhist(object, "mixed_state", summary, probs)
}

#' @export
#' @keywords internal
history_mixed_state <- function(object, ...) { UseMethod("history_mixed_state") }


#' Extract values of used or fitted history parameter history_mix
#'
#' A short-cut for \code{history_parameter(object, "history_mix", ...)}.
#'
#' @param object An object of class [cumhist][cumhist-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param ...
#'
#' @return A single value, if fixed value was used. A vector or a tibble, depending on the
#' option used (single intercept, independent or random intercepts), and whether summary was
#' requested.
#' @export
#'
#' @examples
#' #' \donttest{
#' br_fit <- fit_cumhist(br_singleblock, state="State", duration="Duration")
#' history_tau(br_fit)
#' }
history_mix.cumhist <- function(object, summary=TRUE, probs=c(0.055, 0.945), ...){
  bistablehistory::history_parameter.cumhist(object, "history_mix", summary, probs)
}

#' @export
#' @keywords internal
history_mix <- function(object, ...) { UseMethod("history_mix") }
