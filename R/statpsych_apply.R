apply_ci_mdiff <- function(
  myrow,
  assume_equal_variance,
  conf_level
) {

  has_r <- FALSE
  if ("r" %in% names(myrow)) {
    has_r <- !is.na(myrow[["r"]])
  }

  if (!has_r) {
      res <- as.data.frame(
        statpsych::ci.mean2(
          alpha = 1 - conf_level,
          m1 = myrow[["comparison_mean"]],
          m2 = myrow[["reference_mean"]],
          sd1 = myrow[["comparison_sd"]],
          sd2 = myrow[["reference_sd"]],
          n1 = myrow[["comparison_n"]],
          n2 = myrow[["reference_n"]]
        )
      )

    res_row <- if(assume_equal_variance) 1 else 2

  } else {

    res <- as.data.frame(
      statpsych::ci.mean.ps(
        alpha = 1 - conf_level,
        m1 = myrow[["comparison_mean"]],
        m2 = myrow[["reference_mean"]],
        sd1 = myrow[["comparison_sd"]],
        sd2 = myrow[["reference_sd"]],
        n = myrow[["reference_n"]],
        cor = myrow[["r"]]
      )
    )

    res_row <- 1
  }

  res <- c(
    res[res_row, "Estimate"],
    res[res_row, "SE"]^2,
    res[res_row, "LL"],
    res[res_row, "UL"],
    res[res_row, "df"],
    res[res_row, "p"]
  )
  names(res) <- c("yi", "vi", "LL", "UL", "df", "p")

  return(res)
}



apply_ci_stdmean_two <- function(
  myrow,
  assume_equal_variance,
  correct_bias,
  conf_level
) {

  has_r <- FALSE
  if ("r" %in% names(myrow)) {
    has_r <- !is.na(myrow[["r"]])
  }

  if (!has_r) {
    res <- as.data.frame(
      CI_smd_ind_contrast(
        means = c(myrow[["comparison_mean"]], myrow[["reference_mean"]]),
        sds = c(myrow[["comparison_sd"]], myrow[["reference_sd"]]),
        ns = c(myrow[["comparison_n"]], myrow[["reference_n"]]),
        contrast = c(1, -1),
        assume_equal_variance = assume_equal_variance,
        correct_bias = correct_bias,
        conf_level = conf_level
      )
    )

    n_sum <- myrow[["comparison_n"]] + myrow[["reference_n"]]
    n_prod <- myrow[["comparison_n"]] * myrow[["reference_n"]]
    vi_alt <- ( n_sum/n_prod + res$d_biased^2 / (2*n_sum)  )

    if (correct_bias) {
      J <- res$effect_size / res$d_biased
      vi_alt <- J^2 * vi_alt
    }

    res_raw <- as.data.frame(
      statpsych::ci.mean2(
        alpha = 1 - conf_level,
        m1 = myrow[["comparison_mean"]],
        m2 = myrow[["reference_mean"]],
        sd1 = myrow[["comparison_sd"]],
        sd2 = myrow[["reference_sd"]],
        n1 = myrow[["comparison_n"]],
        n2 = myrow[["reference_n"]]
      )
    )

    res_raw_row <- if(assume_equal_variance) 1 else 2


  } else {

    m1 <- myrow[["comparison_mean"]]
    m2 <- myrow[["reference_mean"]]
    df <- myrow[["reference_n"]] - 1
    s <- sqrt((myrow[["comparison_sd"]]^2 + myrow[["reference_sd"]]^2)/2)

    if(!correct_bias) {
      adj1 <- sqrt((df-1)/df)
      est1 <- (m1 - m2)/s
      desired_est1 <- est1/adj1
      desired_diff <- desired_est1*s
      m1 <- desired_diff
      m2 <- 0
    }


    res <- as.data.frame(
      statpsych::ci.stdmean.ps(
        alpha = 1 - conf_level,
        m1 = m1,
        m2 = m2,
        sd1 = myrow[["comparison_sd"]],
        sd2 = myrow[["reference_sd"]],
        cor= myrow[["r"]],
        n = myrow[["reference_n"]]
      )
    )

    res$df <- df

    res_raw <- as.data.frame(
      statpsych::ci.mean.ps(
        alpha = 1 - conf_level,
        m1 = myrow[["comparison_mean"]],
        m2 = myrow[["reference_mean"]],
        sd1 = myrow[["comparison_sd"]],
        sd2 = myrow[["reference_sd"]],
        cor= myrow[["r"]],
        n = myrow[["reference_n"]]
      )
    )

    res_raw_row <- 1
    vi_alt <- NA

  }


  res <- c(
    res[1, if (has_r) "Estimate" else "effect_size"],
    res[1, "SE"]^2,
    res[1, "LL"],
    res[1, "UL"],
    vi_alt,
    res[1, "df"],
    p = res_raw[res_raw_row, "p"]
  )
  names(res) <- c("yi", "vi", "LL", "UL", "vi_alt", "df", "p")


  return(res)
}


apply_ci_cor <- function(
  myrow,
  conf_level
) {

  res <- as.data.frame(
    statpsych::ci.cor(
      alpha = 1 - conf_level,
      cor = myrow[["r"]],
      s = 0,
      n = myrow[["N"]]
    )
  )

  res <- c(
    res[1, "Estimate"],
    res[1, "SE"]^2,
    res[1, "LL"],
    res[1, "UL"]
  )
  names(res) <- c("yi", "vi", "LL", "UL")

  return(res)
}


apply_ci_prop1 <- function(
  myrow,
  conf_level
) {

  res <- as.data.frame(
    statpsych::ci.prop1(
      alpha = 1 - conf_level,
      f = myrow[["cases"]],
      n = myrow[["N"]]
    )
  )

  res <- c(
    res[2, "Estimate"],
    res[1, "SE"]^2,
    res[1, "LL"],
    res[1, "UL"],
    res[1, "Estimate"]
  )
  names(res) <- c("yi", "vi", "LL", "UL", "P_adjusted")

  return(res)
}

apply_ci_prop2 <- function(
  myrow,
  conf_level
) {
  f1 <- myrow[["comparison_cases"]]
  f2 <- myrow[["reference_cases"]]
  n1 <- myrow[["comparison_N"]]
  n2 <- myrow[["reference_N"]]

  res <- as.data.frame(
    statpsych::ci.prop2(
      alpha = 1 - conf_level,
      f1 = f1,
      f2 = f2,
      n1 = n1,
      n2 = n2
    )
  )

  res <- c(
    f1/n1 - f2/n2,
    res[1, "SE"]^2,
    res[1, "LL"],
    res[1, "UL"]
  )
  names(res) <- c("yi", "vi", "LL", "UL")

  return(res)
}


apply_ci_mean1 <- function(
  myrow,
  reference_mean,
  conf_level
) {

  res <- as.data.frame(
    statpsych::ci.mean1(
      alpha = 1 - conf_level,
      m = myrow[["mean"]] - reference_mean,
      sd = myrow[["sd"]],
      n = myrow[["n"]]
    )
  )

  res_row <- 1


  t <- abs((myrow[["mean"]] - reference_mean) / res[res_row, "SE"])
  p <- 2*stats::pt(
    q = t,
    df = myrow[["n"]] -1,
    lower.tail = FALSE
  )

  res <- c(
    res[res_row, "Estimate"],
    res[res_row, "SE"]^2,
    res[res_row, "LL"],
    res[res_row, "UL"],
    p
  )
  names(res) <- c("yi", "vi", "LL", "UL", "p")

  return(res)
}

apply_ci_stdmean1 <- function(
  myrow,
  reference_mean,
  correct_bias,
  conf_level
) {


  res <- as.data.frame(
    CI_smd_one(
      mean = myrow[["mean"]],
      sd = myrow[["sd"]],
      n = myrow[["n"]],
      reference_mean = reference_mean,
      correct_bias = correct_bias,
      conf_level = conf_level
    )
  )

  sem <- myrow[["sd"]] / sqrt(myrow[["n"]])
  t <- abs((myrow[["mean"]] - reference_mean) / sem)
  p <- 2*stats::pt(
    q = t,
    df = myrow[["n"]] -1,
    lower.tail = FALSE
  )

  res <- c(
    res[1, "effect_size"],
    res[1, "SE"]^2,
    res[1, "LL"],
    res[1, "UL"],
    p
  )
  names(res) <- c("yi", "vi", "LL", "UL", "p")


  return(res)
}
