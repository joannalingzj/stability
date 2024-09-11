# ------------------------------------------------------------------------------
# Stability Platform
# ------------------------------------------------------------------------------
# Data requirements:
#   Variables:
#     time = time - can be transformed or untransformed
#     time_months = Time (Months)
#     batch = Batch / Lots
#     label = Quality Attribute (Unit)
#     lowerlimit & upperlimit = NA if not required
#     backtransform = string with back transformation
#         "time" is the identity backtransformation of time_months
#         "time^2" is the backtransformation of sqrt(time_months)
#         "2^time" is the backtransformation of log(time_months)
# ------------------------------------------------------------------------------
# Time transformation can be applied
# Model diagnostics: Residuals vs Fitted, Residuals vs Time
# ------------------------------------------------------------------------------


# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom rlang :=
#' @importFrom rlang .data
## usethis namespace: end
NULL

# Seems like this but doesn't work? only moving the chunk to the bottom works
# But then check() throws errors because its not being read in the beginning
# https://github.com/r-lib/roxygen2/issues/1409

# What it means is to add the word NULL after the @importFrom lines!
# https://stackoverflow.com/questions/48817232/how-do-i-use-importfrom-so-that-it-applies-to-a-whole-r-package
# ------------------------------------------------------------------------------


# check() still throws notes


#' Test qq plot
#' Draw a QQ plot of residuals based on JMP formula
#'
#' @param df A dataframe containing a continuous variable to graph qq plot.
#' @param variable Variable name to graph qq plot in quotation marks.
#'
#' @return A qq plot.
#' @export
#'
#' @examples
#'   # Draw qq plot of residuals
#'   # m1_qq <- qq_jmp(df = m1_diag, variable = "residuals")

qq_jmp <- function(df, variable){
  df <- df[order(df[[variable]]),]
  df$qq <- stats::qnorm(stats::ppoints(nrow(df)))

  df <- df %>%
    dplyr::mutate(std = (!!rlang::sym(variable) - mean(!!rlang::sym(variable)))/stats::sd(!!rlang::sym(variable)))

  # Get slope and intercept for line
  m <- stats::lm(stats::as.formula(paste(variable, "~ std")), df)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = .data$qq, y = .data[[variable]])) +
    ggplot2::geom_abline(slope = m$coefficients[[2]], intercept = m$coefficients[[1]], color = "red") +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Residuals") +
    ggplot2::ggtitle("Normal Q-Q Plot") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  return(p)
}

# ------------------------------------------------------------------------------

#' Test poolability of model
#'
#' This uses the Type 1 Sum of Squares ANOVA results from the time*batch interaction model
#'
#' @param data A dataframe containing variables "time" and "batch" to be analysed.
#' @param outcome Outcome variable name in quotation marks.
#' @param poolMSE Use pooled MSE? To turn on, poolMSE = 1.
#' @param debug Debugging. To turn on, debug = 1.
#'
#' @return A list of (i. Chosen model as an integer from 1-4, ii. String description of chosen model).
#' @export
#'
#' @examples
#'   # Choose model to use by p values and whether MSE is to be pooled or not
#'   # model <- choosemodel(data = data, outcome = "result_value", poolMSE = 0, debug = 0)

choosemodel <- function(data, outcome, poolMSE, debug) {

  choice <- stats::lm(stats::as.formula(paste(outcome, " ~ time*batch")), data = data)
  if (debug == 1) print(stats::anova(choice))

  P_B <- stats::anova(choice)$`Pr(>F)`[2]
  P_C <- stats::anova(choice)$`Pr(>F)`[3]

  if (P_B < 0.25 & P_C < 0.25) {

    if (poolMSE == 1) {
      chosenmodel <- 1
      chosenmodelstring <- "Different intercepts and Different slopes with pooled MSE"
      if (debug == 1) print("P_B < 0.25 & P_C < 0.25, Model 1 Chosen")
    } else {
      chosenmodel <- 4
      chosenmodelstring <- "Different intercepts and Different slopes with unpooled MSE"
      if (debug == 1) print("P_B < 0.25 & P_C < 0.25, Model 4 Chosen")
    }
  }
  else if (P_B < 0.25 & P_C >= 0.25) {
    if (debug == 1) print("P_B < 0.25 & P_C >= 0.25, Model 2 Chosen")
    chosenmodel <- 2
    chosenmodelstring <- "Different intercepts and Common slopes"
  }
  else if (P_B >= 0.25 & P_C >= 0.25) {
    if (debug == 1) print("P_B >= 0.25 & P_C >= 0.25, Model 3 Chosen")
    chosenmodel <- 3
    chosenmodelstring <- "Common intercepts and Common slopes"
  }

  chosen <- list(chosenmodel,chosenmodelstring)
  return(chosen)

}


# ------------------------------------------------------------------------------

#' Get all models and results
#'
#' Model numbering following JMP output. SAS Macro uses reverse numbering
#'
#' @param data A dataframe containing variables "time" and "batch" to be analysed.
#' @param outcome Outcome variable name in quotation marks.
#' @param colours_named A list of named colours to use for plotting each batch.
#' @param debug Debugging. To turn on, debug = 1.
#'
#' @return A list of (i. Model coefficients, ii. 90% and 95% confidence intervals, iii. Model diagnostics - residuals and fitted values).
#' @export
#'
#' @examples
#'   # Choose model to use by p values and whether MSE is to be pooled or not
#'   # model <- choosemodel(data = data, outcome = "result_value", poolMSE = 0, debug = 0)

models <- function(data, outcome, colours_named, debug) {

  # Create tibbles for predicting values up to 180 months (arbitrary maximum of 15 years)
  x_val <- tibble::tibble(time = seq(0,180,0.001))

  b <- data %>%
    dplyr::distinct(.data$batch)

  xb_val <- tidyr::crossing(b,x_val)


  # Model 1: Different Intercept, Different Slope - Pooled MSE
  m1 <- stats::lm(stats::as.formula(paste(outcome, " ~ 0 + batch + time:batch")), data = data)

  # Get table coefficients
  m1_coef <- tibble::as_tibble(summary(m1)$coefficients, rownames = "name") %>%
    dplyr::mutate(model = 1,
           slope = stringr::str_detect(.data$name, "time"),
           parameter = dplyr::case_when(slope == FALSE ~ substr(.data$name,nchar("batch")+1,nchar(.data$name)),
                                 TRUE ~ stringr::str_extract(substr(.data$name,nchar("batch")+1,nchar(.data$name)),"^[^\\.:]+"))) %>%
    dplyr::arrange(.data$parameter) %>%
    dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ paste0("Intercept: ",.data$parameter),
                                 TRUE ~ paste0("Slope: ",.data$parameter)))

  # Get predicted confidence intervals
  m1_conf90 <- tibble::as_tibble(stats::predict(m1, newdata = xb_val, interval = "confidence", level = 0.90)) %>%
    dplyr::rename(lwr90 = .data$lwr, upr90 = .data$upr)
  m1_conf95 <- tibble::as_tibble(stats::predict(m1, newdata = xb_val, interval = "confidence", level = 0.95)[,-1]) %>%
    dplyr::rename(lwr95 = .data$lwr, upr95 = .data$upr)

  m1_conf <- cbind(xb_val, m1_conf90, m1_conf95) %>%
    dplyr::mutate(model = 1)

  # Diagnostics
  m1_diag <- tibble::tibble(residuals = m1$residuals,
                    fitted = m1$fitted.values,
                    time = m1$model$time,
                    batch = m1$model$batch)

  ## QQ plot
  m1_qq <- qq_jmp(df = m1_diag, variable = "residuals")

  # Residuals vs Time
  m1_p_rvt <- ggplot2::ggplot(m1_diag,
                              ggplot2::aes(x = .data$time, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Time") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  # Residuals vs Fitted
  m1_p_rvf <- ggplot2::ggplot(m1_diag,
                              ggplot2::aes(x = .data$fitted, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Fitted") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Fitted") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  m1_diagnostics <- ggpubr::ggarrange(m1_p_rvt,m1_p_rvf,m1_qq, nrow = 1,  common.legend = TRUE)


  # Model 2: Different Intercept, Common Slope
  m2 <- stats::lm(stats::as.formula(paste(outcome, " ~ 0 + time + batch")), data = data)

  # Get table coefficients
  m2_coef <- tibble::as_tibble(summary(m2)$coefficients, rownames = "name") %>%
    dplyr::mutate(model = 2,
           slope = stringr::str_detect(.data$name, "time"),
           parameter = dplyr::case_when(slope == FALSE ~ substr(.data$name,nchar("batch")+1,nchar(.data$name)),
                                 TRUE ~ "")) %>%
    dplyr::arrange(.data$parameter) %>%
    dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ paste0("Intercept: ",.data$parameter),
                                 TRUE ~ "Slope"))
  # m2_conf <- tibble::as_tibble(stats::predict(m2, interval = "confidence", level = 0.95)) %>%
  #   dplyr::mutate(model = 2)

  # Get predicted confidence intervals
  m2_conf90 <- tibble::as_tibble(stats::predict(m2, newdata = xb_val, interval = "confidence", level = 0.90)) %>%
    dplyr::rename(lwr90 = .data$lwr, upr90 = .data$upr)
  m2_conf95 <- tibble::as_tibble(stats::predict(m2, newdata = xb_val, interval = "confidence", level = 0.95)[,-1]) %>%
    dplyr::rename(lwr95 = .data$lwr, upr95 = .data$upr)

  m2_conf <- cbind(xb_val, m2_conf90, m2_conf95) %>%
    dplyr::mutate(model = 2)

  # Diagnostics
  m2_diag <- tibble::tibble(residuals = m2$residuals,
                    fitted = m2$fitted.values,
                    time = m2$model$time,
                    batch = m2$model$batch)

  ## QQ plot
  m2_qq <- qq_jmp(df = m2_diag, variable = "residuals")

  # Residuals vs Time
  m2_p_rvt <- ggplot2::ggplot(m2_diag,
                              ggplot2::aes(x = .data$time, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Time") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  # Residuals vs Fitted
  m2_p_rvf <- ggplot2::ggplot(m2_diag,
                              ggplot2::aes(x = .data$fitted, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Fitted") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Fitted") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  m2_diagnostics <- ggpubr::ggarrange(m2_p_rvt,m2_p_rvf,m2_qq, nrow = 1,  common.legend = TRUE)


  # Model 3: Common Intercept, Common Slope
  m3 <- stats::lm(stats::as.formula(paste(outcome, " ~ time")), data = data)

  # Get table coefficients
  m3_coef <- tibble::as_tibble(summary(m3)$coefficients, rownames = "name") %>%
    dplyr::mutate(model = 3,
           slope = stringr::str_detect(.data$name, "time"),
           parameter = dplyr::case_when(slope == FALSE ~ substr(.data$name,nchar("batch")+1,nchar(.data$name)),
                                 TRUE ~ "")) %>%
    dplyr::arrange(.data$parameter) %>%
    dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ "Intercept",
                                 TRUE ~ "Slope"))
  # m3_conf <- tibble::tibble(stats::predict(m3, interval = "confidence", level = 0.95))%>%
  #   dplyr::mutate(model = 3)

  # Get predicted confidence intervals
  m3_conf90 <- tibble::as_tibble(stats::predict(m3, newdata = x_val, interval = "confidence", level = 0.90)) %>%
    dplyr::rename(lwr90 = .data$lwr, upr90 = .data$upr)
  m3_conf95 <- tibble::as_tibble(stats::predict(m3, newdata = x_val, interval = "confidence", level = 0.95)[,-1]) %>%
    dplyr::rename(lwr95 = .data$lwr, upr95 = .data$upr)

  b_zeros <- tibble::tibble(batch = rep(0,180001))
  m3_conf <- cbind(x_val, b_zeros, m3_conf90, m3_conf95) %>%
    dplyr::mutate(model = 3)

  # Get the right number of batches - in case there are missing values in the outcome
  bb <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(outcome)))
  # Diagnostics
  m3_diag <- tibble::tibble(residuals = m3$residuals,
                    fitted = m3$fitted.values,
                    time = m3$model$time,
                    batch = bb$batch)

  ## QQ plot
  m3_qq <- qq_jmp(df = m3_diag, variable = "residuals")

  # Residuals vs Time
  m3_p_rvt <- ggplot2::ggplot(m3_diag,
                              ggplot2::aes(x = .data$time, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Time") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  # Residuals vs Fitted
  m3_p_rvf <- ggplot2::ggplot(m3_diag,
                              ggplot2::aes(x = .data$fitted, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Fitted") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Fitted") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  m3_diagnostics <- ggpubr::ggarrange(m3_p_rvt,m3_p_rvf,m3_qq, nrow = 1,  common.legend = TRUE)


  # Model 4: Different Intercept, Different Slope - Nonpoolable MSE (Stratified by Batch)
  b <- data %>%
    dplyr::select(.data$batch)
  b <- as.list(unique(b))

  m4_coef <- data.frame()
  m4_conf <- data.frame()
  m4_diag <- data.frame()

  for (i in 1:length(b[[1]])) {

    subs <- data %>%
      dplyr::filter(.data$batch == b[[1]][i])

    m4 <- stats::lm(stats::as.formula(paste(outcome, " ~ time")),
             data = subs)

    # Get table coefficients
    m4_coef_tmp <- tibble::as_tibble(summary(m4)$coefficients, rownames = "name") %>%
      dplyr::mutate(model = 4,
             slope = stringr::str_detect(.data$name, "time"),
             parameter = dplyr::case_when(slope == FALSE ~ substr(.data$name,nchar("batch")+1,nchar(.data$name)),
                                   TRUE ~ "")) %>%
      dplyr::arrange(.data$parameter) %>%
      dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ paste0("Intercept: ",b[[1]][i]),
                                   TRUE ~ paste0("Slope: ",b[[1]][i])))

    m4_coef <- rbind(m4_coef,m4_coef_tmp)

    # Get predicted confidence intervals
    m4_conf90 <- tibble::as_tibble(stats::predict(m4, newdata = x_val, interval = "confidence", level = 0.90)) %>%
      dplyr::rename(lwr90 = .data$lwr, upr90 = .data$upr)
    m4_conf95 <- tibble::as_tibble(stats::predict(m4, newdata = x_val, interval = "confidence", level = 0.95)[,-1]) %>%
      dplyr::rename(lwr95 = .data$lwr, upr95 = .data$upr)

    b_num <- tibble::tibble(batch = rep(b[[1]][i],180001))
    m4_conf_tmp <- cbind(x_val, b_num, m4_conf90, m4_conf95) %>%
      dplyr::mutate(model = 4)

    m4_conf <- rbind(m4_conf,m4_conf_tmp)

    # Gather diagnostics
    m4_diag_tmp <- tibble::tibble(residuals = m4$residuals,
                          fitted = m4$fitted.values,
                          time = m4$model$time,
                          batch = b[[1]][i])
    m4_diag <- rbind(m4_diag,m4_diag_tmp)

  }
  # Diagnostics

  ## QQ plot
  m4_qq <- qq_jmp(df = m4_diag, variable = "residuals")

  # Residuals vs Time
  m4_p_rvt <- ggplot2::ggplot(m4_diag,
                              ggplot2::aes(x = .data$time, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Time") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  # Residuals vs Fitted
  m4_p_rvf <- ggplot2::ggplot(m4_diag,
                              ggplot2::aes(x = .data$fitted, y = .data$residuals, group = .data$batch, colour = .data$batch)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::xlab("Fitted") +
    ggplot2::ylab("Residuals") +
    ggplot2::scale_color_manual(values=colours_named) +
    ggplot2::labs(color = "Batch", title = "Residuals vs Fitted") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  m4_diagnostics <- ggpubr::ggarrange(m4_p_rvt,m4_p_rvf,m4_qq, nrow = 1, common.legend = TRUE)

  # Save all results and return as a list
  coef <- rbind(m1_coef, m2_coef, m3_coef, m4_coef)
  coef <- coef[, c("model", "parameter", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

  conf <- rbind(m1_conf, m2_conf, m3_conf, m4_conf)

  diag <- list(m1_diagnostics, m2_diagnostics, m3_diagnostics, m4_diagnostics)

  results <- list(coef,conf,diag)
  return(results)

}

# ------------------------------------------------------------------------------

#' Calculate expiration dates from all batches and models based on limits provided
#'
#' Crossing time of 90%CI used to get one-sided 95%CI if only one limit is provided
#' Crossing time of 95%CI used to get two-sided 95%CI if both limits are provided
#' Crossing time is defined as the value just before it crosses the lower / upper limits
#' Expiry date for a batch / model is not missing if it is within 15 years of time 0
#'
#' @param data A result dataset of predicted values from the chosen model.
#' @param lowerlimit Lower limit to calculate lower confidence interval.
#' @param upperlimit Upper limit to calculate upper confidence interval.
#' @param debug Debugging. To turn on, debug = 1.
#'
#' @return Dataset containing estimated expiration date by model and batch.
#' @export
#'
#' @examples
#'   # Save confidence interval results from all models
#'   # results <- models(data = data, outcome = outcome, colours_named = colours_named, debug = debug)
#'
#'   # Get expiry date of all batches from all models by number of limits provided
#'   # exp <- expiry(data = results[[2]], lowerlimit = 90, upperlimit = 110, debug = 0)

expiry <- function(data, lowerlimit, upperlimit, debug) {

  # Lower & Upper limits, use two-sided 95% CI
  if (!is.na(lowerlimit) & !is.na(upperlimit)) {
    if (debug == 1) print("both true")

    expiration <- data %>%
      dplyr::mutate(diff_lwr = .data$lwr95 - lowerlimit,
             flag_lwr = dplyr::lead(ifelse(.data$diff_lwr <= 0,1,0)),
             diff_upr = upperlimit - .data$upr95,
             flag_upr = dplyr::lead(ifelse(.data$diff_upr <= 0,1,0)),
             flag = dplyr::case_when(.data$flag_lwr == 1 | .data$flag_upr == 1 ~ paste0(.data$flag_lwr,.data$flag_upr),
                              TRUE ~ "00")) %>%
      dplyr::group_by(.data$model,.data$batch,.data$flag) %>%
      dplyr::mutate(rn = dplyr::row_number(),
             level = 95) %>%
      dplyr::filter(.data$flag != "00" & .data$rn == 1)

  }
  # Lower limit only, use one-sided 95% CI (i.e. lower 90%CI)
  else if (!is.na(lowerlimit) & is.na(upperlimit)) {
    if (debug == 1) print("lowerlimit true")

    # Get expiration date just before it crosses below the lower limit
    expiration <- data %>%
      dplyr::mutate(diff = .data$lwr90 - lowerlimit,
             flag = dplyr::lead(ifelse(.data$diff <= 0,1,0))) %>%
      dplyr::group_by(.data$model,.data$batch,.data$flag) %>%
      dplyr::mutate(rn = dplyr::row_number(),
             level = 90) %>%
      dplyr::filter(.data$flag == 1 & .data$rn == 1)

  }
  # Upper limit only, use one-sided 95% CI (i.e. upper 90%CI)
  else if (is.na(lowerlimit) & !is.na(upperlimit)) {
    if (debug == 1) print("upperlimit true")

    # Get expiration date just before it crosses above the upper limit
    expiration <- data %>%
      dplyr::mutate(diff = upperlimit - .data$upr90,
             flag = dplyr::lead(ifelse(.data$diff <= 0,1,0))) %>%
      dplyr::group_by(.data$model,.data$batch,.data$flag) %>%
      dplyr::mutate(rn = dplyr::row_number(),
             level = 90) %>%
      dplyr::filter(.data$flag == 1 & .data$rn == 1)

  }
  else {
    print("error")
  }

  return(expiration)
}

# ------------------------------------------------------------------------------

#' Plot observed values and expiry date based on chosen model
#'
#' Variables should be named "time" and "batch"
#'
#' @param data A result dataset of observed quality attribute values with variables "time" (in months or transformed) and "batch".
#' @param outcome Quality attribute outcome variable name in quotation marks.
#' @param label y-axis label.
#' @param lowerlimit Lower limit to plot on graph.
#' @param upperlimit Upper limit to plot on graph.
#' @param chosenmodel A numeric integer (1-4) of the chosen model from choosemodel()\[\[1\]\].
#' @param confRes A result dataset of predicted values from models()\[\[2\]\].
#' @param expiryRes A result dataset of expiry dates by model and batch from expiry().
#' @param backtransf Formula for backtransformation of outcome variable. Supported formula strings are c("Time","log2","Time^2","Time^3","Time^4","Time^5") corresponding to the indicator formula for c(no transformation, Log 2 transformation, square root of time, cube root of time, fourth root of time, fifth root of time).
#' @param colours_named A list of named colours to use for plotting each batch.
#'
#' @return List of (i. Expiry date in months, ii. Plot of all observed values with estimated model mean and one-/two-sided 95%CI)
#' @export
#'
#' @examples
#'     # Choose model to use by p values and whether MSE is to be pooled or not
#'     # model <- choosemodel(data = data, outcome = "result_value", poolMSE = 0, debug = 0)
#'
#'     # Set colours using all batches in dataset
#'     # colours <- c("#FC1921","#0E56A5","#F5C017","#975DA2","#00A28A",
#'     #              "#DA2877","#F06125","#03B3BE","#C6D92D","#572A7B")
#'     # colours_named <- setNames(object = colours,
#'     #                           nm = c(unique(data_transf$batch),"Common Intercept"))
#'
#'     # Save confidence interval results from all models
#'     # results <- models(data = data, outcome = "result_value",
#'     #                   colours_named = colours_named, debug = 0)
#'
#'     # Get expiry date of all batches from all models by number of limits provided
#'     # exp <- expiry(data = results[[2]], lowerlimit = 90, upperlimit = 110, debug = 0)
#'
#'     # Plot results from chosen model
#'     # expiryplot <- expiryplot(data = data, outcome = "result_value", label = "Potency (%)",
#'     #                    lowerlimit = lowerlimit, upperlimit = upperlimit,
#'     #                    chosenmodel = model[[1]], confRes = results[[2]],
#'     #                    expiryRes = exp, backtransf = backtransform, colours_named = colours_named)

expiryplot <- function(data, outcome, label, lowerlimit, upperlimit,
                 chosenmodel, confRes, expiryRes, backtransf, colours_named) {
  # print(colours_named)

  # Use time months only, backtransform CIs.

  # Get expiry date from minimum crossing time of chosen model
  exp_model <- expiryRes %>%
    dplyr::filter(.data$model == chosenmodel) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(minx = min(.data$time)) %>%
    dplyr::filter(.data$time == .data$minx)

  # backtransform time variable before comparing it to 180 months time limit
  # backtransf is the transformation with the value to be transformed denoted by "time"
  # e.g. "time^2" is the back transformation of sqrt(time)
  minexpdt_months <- eval(parse(text = gsub("time",min(exp_model$time),tolower(backtransf))))
  expirydate_months <- min(minexpdt_months,180)
  if (min(exp_model$time) < 180) expirydate <- min(exp_model$time)

  # Set end date of x-axis as the longer of expiry date or time of available data, but with a maximum of 180
  xend <- max(min(exp_model$time),max(data$time))
  xend_months <- min(eval(parse(text = gsub("time",xend,tolower(backtransf)))),180)

  # beautify x-axis (transformed) breaks depending on when they end
  if (xend < 12) {
    steps = 1
  } else if (xend < 36) {
    steps = 6
  } else {
    steps = 12
  }
  # beautify x-axis (months) breaks depending on when they end
  if (xend_months < 12) {
    steps_months = 1
  } else if (xend_months < 36) {
    steps_months = 6
  } else {
    steps_months = 12
  }

  # only plot a subset of fit & 90/95% CIs up till expiry date
  # - This is transformed back to time (months)
  # - THEN only take every 100th value to plot AND last value to ensure graph reaches the expiry date

  conf_sub <- confRes %>%
    dplyr::filter(.data$model == chosenmodel) %>%
    dplyr::mutate(# backtransform all results to original scale
      time = dplyr::case_when(tolower(backtransf) == "log2" ~ 2^time,
                       tolower(backtransf) == "time^2" ~ time^2,
                       tolower(backtransf) == "time^3" ~ time^3,
                       tolower(backtransf) == "time^4" ~ time^4,
                       tolower(backtransf) == "time^5" ~ time^5,
                       TRUE ~ time) ) %>%
    dplyr::filter(.data$time <= xend_months) %>%
    dplyr::group_by(.data$batch) %>%
    dplyr::mutate(nrow = dplyr::row_number(),
           rem100 = nrow %% 100,
           lastrow = dplyr::if_else(dplyr::row_number() == dplyr::n(), 1, 0),
           batch = dplyr::case_when(batch == 0 ~ "Common Intercept",
                             TRUE ~ batch)) %>%
    dplyr::filter(.data$rem100 == 1 | .data$lastrow == 1)

  # only plot a subset of observed values up till expiry date
  # this is only to replicate JMP which cuts off the graph at the expiry date
  # it is much better to have all observed values
  # made redundant for now
  # data_sub <- subset(data, time_months <= xend)
  data_sub <- data

  # Lower & Upper limits, use two-sided 95% CI
  if (!is.na(lowerlimit) & !is.na(upperlimit)) {
    # print("both true")

    # ylimits with 2% buffer of lower and upper limits OR largest/smallest confidence intervals or largest/smallest observed results
    min_lci <- min(conf_sub$lwr95)
    max_uci <- max(conf_sub$upr95)
    min_obs <- min(data_sub[,outcome])
    max_obs <- max(data_sub[,outcome])
    ystart <- min(lowerlimit - 0.02*lowerlimit, min_lci - 0.02*min_lci, min_obs)
    yend <- max(upperlimit + 0.02*upperlimit, max_uci + 0.02*max_uci, max_obs)

    # ystart <- lowerlimit - 0.02*lowerlimit
    # yend <- upperlimit + 0.02*upperlimit

    p <- ggplot2::ggplot(data = data_sub,
                         ggplot2::aes(x = .data$time_months, group = .data$batch, color = .data$batch)) +
      ggplot2::geom_ribbon(data = conf_sub,
                           ggplot2::aes(x = .data$time, ymin = .data$lwr95, ymax = .data$upr95, group = .data$batch, color = .data$batch), alpha = 0.2) +
      ggplot2::geom_line(data = conf_sub,
                         ggplot2::aes(x = .data$time, y = .data$fit, group = .data$batch, color = .data$batch), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = .data[[outcome]]), position = ggplot2::position_jitter()) +
      ggplot2::geom_hline(yintercept = lowerlimit) +
      ggplot2::geom_hline(yintercept = upperlimit) +
      ggplot2::geom_vline(xintercept = expirydate_months) +
      ggplot2::scale_x_continuous(breaks = seq(0,xend_months,steps_months)) +
      ggplot2::scale_color_manual(values=colours_named) +
      ggplot2::ylim(ystart,yend) +
      ggplot2::xlab("Time (months)") +
      ggplot2::ylab(label) +
      ggplot2::labs(color = "Batch") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  }
  # Lower limit only, use one-sided 95% CI (i.e. lower 90%CI)
  else if (!is.na(lowerlimit) & is.na(upperlimit)) {
    # print("lowerlimit true")

    # ylimits with 2% buffer of lower limits and upper limit = max(upper 90%CI)
    min_lci <- min(conf_sub$lwr90)
    max_uci <- max(conf_sub$upr90)
    min_obs <- min(data_sub[,outcome])
    max_obs <- max(data_sub[,outcome])
    ystart <- min(lowerlimit - 0.02*lowerlimit, min_lci - 0.02*min_lci, min_obs)
    yend <- max(max(subset(conf_sub, nrow == 1)$upr90), max_uci + 0.02*max_uci, max_obs)

    # yend <- max(subset(conf_sub, nrow == 1)$upr90)
    # ystart <- lowerlimit - 0.02*lowerlimit

    p <- ggplot2::ggplot(data = data_sub,
                         ggplot2::aes(x = .data$time_months, group = .data$batch, color = .data$batch)) +
      ggplot2::geom_ribbon(data = conf_sub,
                           ggplot2::aes(x = .data$time, ymin = .data$lwr90, ymax = .data$upr90, group = .data$batch, color = .data$batch), alpha = 0.2) +
      ggplot2::geom_line(data = conf_sub,
                         ggplot2::aes(x = .data$time, y = .data$fit, group = .data$batch, color = .data$batch), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = .data[[outcome]]), position = ggplot2::position_jitter()) +
      ggplot2::geom_hline(yintercept = lowerlimit) +
      ggplot2::geom_vline(xintercept = expirydate_months) +
      ggplot2::scale_x_continuous(breaks = seq(0,xend_months,steps_months)) +
      ggplot2::scale_color_manual(values=colours_named) +
      ggplot2::ylim(ystart,yend) +
      ggplot2::xlab("Time (months)") +
      ggplot2::ylab(label) +
      ggplot2::labs(color = "Batch") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  }
  # Upper limit only, use one-sided 95% CI (i.e. upper 90%CI)
  else if (is.na(lowerlimit) & !is.na(upperlimit)) {
    # print("upperlimit true")

    # ylimits with 2% buffer of upper limits and lower limit = min(lower 90%CI)
    min_lci <- min(conf_sub$lwr90)
    max_uci <- max(conf_sub$upr90)
    min_obs <- min(data_sub[,outcome])
    max_obs <- max(data_sub[,outcome])
    ystart <- min(min(subset(conf_sub, nrow == 1)$lwr90), min_lci - 0.02*min_lci, min_obs)
    yend <- max(upperlimit + 0.02*upperlimit, max_uci + 0.02*max_uci, max_obs)

    # ystart <- min(subset(conf_sub, nrow == 1)$lwr90)
    # yend <- upperlimit + 0.02*upperlimit

    p <- ggplot2::ggplot(data = data_sub,
                         ggplot2::aes(x = .data$time_months, group = .data$batch, color = .data$batch)) +
      ggplot2::geom_ribbon(data = conf_sub,
                           ggplot2::aes(x = .data$time, ymin = .data$lwr90, ymax = .data$upr90, group = .data$batch, color = .data$batch), alpha = 0.2) +
      ggplot2::geom_line(data = conf_sub,
                         ggplot2::aes(x = .data$time, y = .data$fit, group = .data$batch, color = .data$batch), linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = .data[[outcome]]), position = ggplot2::position_jitter()) +
      ggplot2::geom_hline(yintercept = upperlimit) +
      ggplot2::geom_vline(xintercept = expirydate_months) +
      ggplot2::scale_x_continuous(breaks = seq(0,xend_months,steps_months)) +
      ggplot2::scale_color_manual(values=colours_named) +
      ggplot2::ylim(ystart,yend) +
      ggplot2::xlab("Time (months)") +
      ggplot2::ylab(label) +
      ggplot2::labs(color = "Batch") +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
  }
  else {
    print("error")
  }

  results <- list(expirydate_months, p)
  return(results)
}

# ------------------------------------------------------------------------------

#' Packaging all helper functions into one function call
#'
#'
#'
#' @param data A result dataset of observed quality attribute values with variables "time" (in months or transformed) and "batch".
#' @param outcome Quality attribute outcome variable name in quotation marks.
#' @param label y-axis label.
#' @param lowerlimit Lower limit to plot on graph.
#' @param upperlimit Upper limit to plot on graph.
#' @param poolMSE Use pooled MSE? To turn on, poolMSE = 1.
#' @param backtransform Formula for backtransformation of outcome variable. Supported formula strings are c("Time","log2","Time^2","Time^3","Time^4","Time^5") corresponding to the indicator formula for c(no transformation, Log 2 transformation, square root of time, cube root of time, fourth root of time, fifth root of time).
#' @param colours_named A list of named colours to use for plotting each batch.
#' @param debug Debugging. To turn on, debug = 1.
#'
#' @return List of (i. Expiry date in months, ii. Plot of all observed values with estimated model mean and one-/two-sided 95%CI)
#' @export
#'
#' @examples
#'
#'     # Create dataset to be analysed in long format with time variable with
#'     # transformed time where necessary and corresponding backtransform formula variable
#'
#'     # data_transf <- data %>%
#'     #   mutate(time = case_when(quality_attribute == "SE HPLC Monomer" ~ sqrt(time),
#'     #                           quality_attribute == "SE HPLC HMWS" ~ sqrt(time),
#'     #                           quality_attribute == "CE reducing" ~ sqrt(time),
#'     #                           quality_attribute == "CE non reducing LMWS" ~ sqrt(time),
#'     #                           TRUE ~ time),
#'     #           backtransform = case_when(quality_attribute == "SE HPLC Monomer" ~ "Time^2",
#'     #                                     quality_attribute == "SE HPLC HMWS" ~ "Time^2",
#'     #                                     quality_attribute == "CE reducing" ~ "Time^2",
#'     #                                     quality_attribute == "CE non reducing LMWS" ~ "Time^2",
#'     #                                     TRUE ~ "Time"))
#'
#'     # Set colours using all batches in dataset
#'     # colours <- c("#FC1921","#0E56A5","#F5C017","#975DA2","#00A28A",
#'     #              "#DA2877","#F06125","#03B3BE","#C6D92D","#572A7B")
#'     # colours_named <- setNames(object = colours,
#'     #                           nm = c(unique(data_transf$batch),"Common Intercept"))
#'
#'     # Get vectors of quality attributes, labels, lower limits and upper limits
#'     # qa <- data_dist$quality_attribute
#'     # lbl <- data_dist$label
#'     # ll <- data_dist$lower_limit
#'     # ul <- data_dist$upper_limit
#'
#'     # Initialise empty results list
#'     # stab <- vector(mode = "list", length = length(qa))
#'
#'     # for (i in 1:1) {
#'
#'        # Get subset of data containing only one quality attribute
#'        # data_qa <- subset(data_transf, quality_attribute == qa[i])
#'
#'        # Get backtransform formula string
#'        # bt <- unique(data_qa$backtransform)
#'
#'        # Call stability function and save results to list
#'        # stab[[i]] <- stability(data = data_qa, outcome = "result_value", label = lbl[i],
#'        #                        lowerlimit = ll[i], upperlimit = ul[i], poolMSE = 0,
#'        #                        backtransform = bt, colours_named = colours_named, debug = 0)
#'     # }

stability <- function(data, outcome, label, lowerlimit, upperlimit, poolMSE, backtransform, colours_named, debug) { #transform,

  # Choose model to use by p values and whether MSE is to be pooled or not
  model <- choosemodel(data = data, outcome = outcome, poolMSE = poolMSE, debug = debug)

  # Save confidence interval results from all models
  results <- models(data = data, outcome = outcome, colours_named = colours_named, debug = debug)

  # Get expiry date of all batches from all models by number of limits provided
  exp <- expiry(data = results[[2]], lowerlimit = lowerlimit, upperlimit = upperlimit, debug = debug)

  # Plot results from chosen model
  expiryplot <- expiryplot(data = data, outcome = outcome, label = label,
                     lowerlimit = lowerlimit, upperlimit = upperlimit,
                     chosenmodel = model[[1]], confRes = results[[2]],
                     expiryRes = exp, backtransf = backtransform, colours_named = colours_named)

  # Return main results as a list
  results <- list(model[[1]], model[[2]], exp, results[[1]], expiryplot[[1]], expiryplot[[2]], results[[3]])

}
