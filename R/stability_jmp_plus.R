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
# To add: QQ Plot
# ------------------------------------------------------------------------------


# library(tidyverse)
# library(ggplot2)
# library(readxl)X
# library(janitor)X
# library(stringr)
# library(ggpubr)


# QQ Plot
qq_jmp <- function(df, variable){
  df <- df[order(df[[variable]]), ]
  df$qq <- stats::qnorm(stats::ppoints(nrow(df)))

  df <- df %>%
    dplyr::mutate(std := (!!rlang::sym(variable) - mean(!!rlang::sym(variable)))/stats::sd(!!rlang::sym(variable)))

  # Get slope and intercept for line
  m <- stats::lm(stats::as.formula(paste(variable, "~ std")), df)

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_point(ggplot2::aes(x = qq, y = .data[[variable]])) +
    ggplot2::geom_abline(slope = m$coefficients[[2]], intercept = m$coefficients[[1]], color = "red") +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Residuals") +
    ggplot2::ggtitle("Normal Q-Q Plot") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  return(p)
}

# Test poolability of model
# This uses the anova results from the time*batch interaction model
# Type 1 Sum of Squares
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

# Get all models and results
# Model numbering following JMP output. SAS Macro uses reverse numbering
models <- function(data, outcome, colours_named, debug) {

  # Create tibbles for predicting values up to 180 months (arbitrary maximum of 15 years)
  x_val <- tibble::tibble(time = seq(0,180,0.001))

  b <- tibble::tibble(unique(data$batch)) %>%
    dplyr::rename(batch = value)

  xb_val <- tidyr::crossing(b,x_val)


  # Model 1: Different Intercept, Different Slope - Pooled MSE
  m1 <- stats::lm(stats::as.formula(paste(outcome, " ~ 0 + batch + time:batch")), data = data)

  # Get table coefficients
  m1_coef <- tibble::tibble(summary(m1)$coefficients, rownames = "name") %>%
    dplyr::mutate(model = 1,
           slope = stringr::str_detect(name, "time"),
           parameter = dplyr::case_when(slope == FALSE ~ substr(name,nchar("batch")+1,nchar(name)),
                                 TRUE ~ stringr::str_extract(substr(name,nchar("batch")+1,nchar(name)),"^[^\\.:]+"))) %>%
    dplyr::arrange(parameter) %>%
    dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ paste0("Intercept: ",parameter),
                                 TRUE ~ paste0("Slope: ",parameter)))

  # Get predicted confidence intervals
  m1_conf90 <- tibble::tibble(stats::predict(m1, newdata = xb_val, interval = "confidence", level = 0.90)) %>%
    dplyr::rename(lwr90 = lwr, upr90 = upr)
  m1_conf95 <- tibble::tibble(stats::predict(m1, newdata = xb_val, interval = "confidence", level = 0.95)[,-1]) %>%
    dplyr::rename(lwr95 = lwr, upr95 = upr)

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
                              ggplot2::aes(x = time, y = residuals, group = batch, colour = batch)) +
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
                              ggplot2::aes(x = fitted, y = residuals, group = batch, colour = batch)) +
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
  m2_coef <- tibble::tibble(summary(m2)$coefficients, rownames = "name") %>%
    dplyr::mutate(model = 2,
           slope = stringr::str_detect(name, "time"),
           parameter = dplyr::case_when(slope == FALSE ~ substr(name,nchar("batch")+1,nchar(name)),
                                 TRUE ~ "")) %>%
    dplyr::arrange(parameter) %>%
    dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ paste0("Intercept: ",parameter),
                                 TRUE ~ "Slope"))
  m2_conf <- tibble::tibble(stats::predict(m2, interval = "confidence", level = 0.95)) %>%
    dplyr::mutate(model = 2)

  # Get predicted confidence intervals
  m2_conf90 <- tibble::tibble(stats::predict(m2, newdata = xb_val, interval = "confidence", level = 0.90)) %>%
    dplyr::rename(lwr90 = lwr, upr90 = upr)
  m2_conf95 <- tibble::tibble(stats::predict(m2, newdata = xb_val, interval = "confidence", level = 0.95)[,-1]) %>%
    dplyr::rename(lwr95 = lwr, upr95 = upr)

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
                              ggplot2::aes(x = time, y = residuals, group = batch, colour = batch)) +
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
                              ggplot2::aes(x = fitted, y = residuals, group = batch, colour = batch)) +
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
  m3_coef <- tibble::tibble(summary(m3)$coefficients, rownames = "name") %>%
    dplyr::mutate(model = 3,
           slope = stringr::str_detect(name, "time"),
           parameter = dplyr::case_when(slope == FALSE ~ substr(name,nchar("batch")+1,nchar(name)),
                                 TRUE ~ "")) %>%
    dplyr::arrange(parameter) %>%
    dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ "Intercept",
                                 TRUE ~ "Slope"))
  m3_conf <- tibble::tibble(stats::predict(m3, interval = "confidence", level = 0.95))%>%
    dplyr::mutate(model = 3)

  # Get predicted confidence intervals
  m3_conf90 <- tibble::tibble(stats::predict(m3, newdata = x_val, interval = "confidence", level = 0.90)) %>%
    dplyr::rename(lwr90 = lwr, upr90 = upr)
  m3_conf95 <- tibble::tibble(stats::predict(m3, newdata = x_val, interval = "confidence", level = 0.95)[,-1]) %>%
    dplyr::rename(lwr95 = lwr, upr95 = upr)

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
                              ggplot2::aes(x = time, y = residuals, group = batch, colour = batch)) +
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
                              ggplot2::aes(x = fitted, y = residuals, group = batch, colour = batch)) +
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
    dplyr::select(batch)
  b <- as.list(unique(b))

  m4_coef <- data.frame()
  m4_conf <- data.frame()
  m4_diag <- data.frame()

  for (i in 1:length(b[[1]])) {

    subs <- data %>%
      dplyr::filter(batch == b[[1]][i])

    m4 <- stats::lm(stats::as.formula(paste(outcome, " ~ time")),
             data = subs)

    # Get table coefficients
    m4_coef_tmp <- tibble::tibble(summary(m4)$coefficients, rownames = "name") %>%
      dplyr::mutate(model = 4,
             slope = stringr::str_detect(name, "time"),
             parameter = dplyr::case_when(slope == FALSE ~ substr(name,nchar("batch")+1,nchar(name)),
                                   TRUE ~ "")) %>%
      dplyr::arrange(parameter) %>%
      dplyr::mutate(parameter = dplyr::case_when(slope == FALSE ~ paste0("Intercept: ",b[[1]][i]),
                                   TRUE ~ paste0("Slope: ",b[[1]][i])))

    m4_coef <- rbind(m4_coef,m4_coef_tmp)

    # Get predicted confidence intervals
    m4_conf90 <- tibble::tibble(stats::predict(m4, newdata = x_val, interval = "confidence", level = 0.90)) %>%
      dplyr::rename(lwr90 = lwr, upr90 = upr)
    m4_conf95 <- tibble::tibble(stats::predict(m4, newdata = x_val, interval = "confidence", level = 0.95)[,-1]) %>%
      dplyr::rename(lwr95 = lwr, upr95 = upr)

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
                              ggplot2::aes(x = time, y = residuals, group = batch, colour = batch)) +
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
                              ggplot2::aes(x = fitted, y = residuals, group = batch, colour = batch)) +
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

# Get expiration dates from all batches and models based on limits provided
# Crossing time of 90%CI used to get one-sided 95%CI if only one limit is provided
# Crossing time of 95%CI used to get two-sided 95%CI if both limits are provided
# Crossing time is defined as the value just before it crosses the lower / upper limits
# Expiry date for a batch / model is not missing if it is within 15 years of time 0
# data = result dataset of predicted values
limits <- function(data, lowerlimit, upperlimit, debug) {

  # Lower & Upper limits, use two-sided 95% CI
  if (!is.na(lowerlimit) & !is.na(upperlimit)) {
    if (debug == 1) print("both true")

    expiration <- data %>%
      dplyr::mutate(diff_lwr = lwr95 - lowerlimit,
             flag_lwr = lead(ifelse(diff_lwr <= 0,1,0)),
             diff_upr = upperlimit - upr95,
             flag_upr = lead(ifelse(diff_upr <= 0,1,0)),
             flag = dplyr::case_when(flag_lwr == 1 | flag_upr == 1 ~ paste0(flag_lwr,flag_upr),
                              TRUE ~ "00")) %>%
      dplyr::group_by(model,batch,flag) %>%
      dplyr::mutate(rn = dplyr::row_number(),
             level = 95) %>%
      subset(flag != "00" & rn == 1)

  }
  # Lower limit only, use one-sided 95% CI (i.e. lower 90%CI)
  else if (!is.na(lowerlimit) & is.na(upperlimit)) {
    if (debug == 1) print("lowerlimit true")

    # Get expiration date just before it crosses below the lower limit
    expiration <- data %>%
      dplyr::mutate(diff = lwr90 - lowerlimit,
             flag = lead(ifelse(diff <= 0,1,0))) %>%
      dplyr::group_by(model,batch,flag) %>%
      dplyr::mutate(rn = dplyr::row_number(),
             level = 90) %>%
      subset(flag == 1 & rn == 1)

  }
  # Upper limit only, use one-sided 95% CI (i.e. upper 90%CI)
  else if (is.na(lowerlimit) & !is.na(upperlimit)) {
    if (debug == 1) print("upperlimit true")

    # Get expiration date just before it crosses above the upper limit
    expiration <- data %>%
      dplyr::mutate(diff = upperlimit - upr90,
             flag = lead(ifelse(diff <= 0,1,0))) %>%
      dplyr::group_by(model,batch,flag) %>%
      dplyr::mutate(rn = dplyr::row_number(),
             level = 90) %>%
      subset(flag == 1 & rn == 1)

  }
  else {
    print("error")
  }

  return(expiration)
}


# Plot results from chosen model
# data = result dataset of observed values
# confRes = result dataset of predicted values
# expiryRes = result dataset of expiry dates
plot <- function(data, outcome, time, batch, label, lowerlimit, upperlimit,
                 chosenmodel, confRes, expiryRes, backtransf, colours_named) {
  # print(colours_named)

  # Use time months only, backtransform CIs.

  # Get expiry date from minimum crossing time of chosen model
  exp_model <- subset(expiryRes, model == chosenmodel) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(minx = min(time)) %>%
    subset(time == minx)

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

  conf_sub <- subset(confRes, model == chosenmodel) %>%
    dplyr::mutate(# backtransform all results to original scale
      time = dplyr::case_when(tolower(backtransf) == "log2" ~ 2^time,
                       tolower(backtransf) == "time^2" ~ time^2,
                       tolower(backtransf) == "time^3" ~ time^3,
                       tolower(backtransf) == "time^4" ~ time^4,
                       tolower(backtransf) == "time^5" ~ time^5,
                       TRUE ~ time) ) %>%
    subset(time <= xend_months) %>%
    dplyr::group_by(batch) %>%
    dplyr::mutate(nrow = dplyr::row_number(),
           rem100 = nrow %% 100,
           lastrow = if_else(dplyr::row_number() == n(), 1, 0),
           batch = dplyr::case_when(batch == 0 ~ "Common Intercept",
                             TRUE ~ batch)) %>%
    subset(rem100 == 1 | lastrow == 1)

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

    p <- ggplot2::ggplot(data = data_sub, ggplot2::aes(x = time_months, group = batch, color = batch)) +
      ggplot2::geom_ribbon(data = conf_sub, ggplot2::aes(x = time, ymin = lwr95, ymax = upr95, group = batch, color = batch), alpha = 0.2) +
      ggplot2::geom_line(data = conf_sub, ggplot2::aes(x = time, y = fit, group = batch, color = batch), linewidth = 1) +
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

    p <- ggplot2::ggplot(data = data_sub, ggplot2::aes(x = time_months, group = batch, color = batch)) +
      ggplot2::geom_ribbon(data = conf_sub, ggplot2::aes(x = time, ymin = lwr90, ymax = upr90, group = batch, color = batch), alpha = 0.2) +
      ggplot2::geom_line(data = conf_sub, ggplot2::aes(x = time, y = fit, group = batch, color = batch), linewidth = 1) +
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

    p <- ggplot2::ggplot(data = data_sub, ggplot2::aes(x = time_months, group = batch, color = batch)) +
      ggplot2::geom_ribbon(data = conf_sub, ggplot2::aes(x = time, ymin = lwr90, ymax = upr90, group = batch, color = batch), alpha = 0.2) +
      ggplot2::geom_line(data = conf_sub, ggplot2::aes(x = time, y = fit, group = batch, color = batch), linewidth = 1) +
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


# Packaging everything into one function call
stability <- function(data, outcome, label, lowerlimit, upperlimit, poolMSE, transform, backtransform, colours_named, debug) {

  # Choose model to use by p values and whether MSE is to be pooled or not
  model <- choosemodel(data = data, outcome = outcome, poolMSE = poolMSE, debug = debug)

  # Save confidence interval results from all models
  results <- models(data = data, outcome = outcome, colours_named = colours_named, debug = debug)

  # Get expiry date of all batches from all models by number of limits provided
  exp <- limits(data = results[[2]], lowerlimit = lowerlimit, upperlimit = upperlimit, debug = debug)

  # Plot results from chosen model
  expiryplot <- plot(data = data, outcome = outcome, label = label,
                     lowerlimit = lowerlimit, upperlimit = upperlimit,
                     chosenmodel = model[[1]], confRes = results[[2]],
                     expiryRes = exp, backtransf = backtransform, colours_named = colours_named)

  # Return main results as a list
  results <- list(model[[1]], model[[2]], exp, results[[1]], expiryplot[[1]], expiryplot[[2]], results[[3]])

}



