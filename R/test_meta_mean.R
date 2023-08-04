test_estimate_meta_mean <- function() {
  rt_mean <- rt_sd <- rt_n <- study_name <- d1_unbiased <- NULL

  original_7 <- data.frame(
    study_name = c(
      "Aden (1993)"	,
      "Buggs (1995)"	,
      "Crazed (1999)"	,
      "Dudley (2003)"	,
      "Evers (2005)"	,
      "Fox (2009)",
      "Mine (2011)"
    ),
    rt_mean = c(
      454	,
      317	,
      430	,
      525	,
      479	,
      387,
      531
    ),
    rt_sd = c(
      142	,
      158	,
      137	,
      260	,
      144	,
      165,
      233
    ),
    rt_n = c(
      24	,
      7	,
      20	,
      8	,
      14	,
      13,
      18
    ),
    subset = as.factor(
      c(
        "90s",
        "90s",
        "90s",
        "00s",
        "00s",
        "00s",
        "00s"
      )
    ),
    d1_unbiased = c(
      3.091587,
      1.742751,
      3.012857,
      1.793487,
      3.130074,
      2.195209,
      2.17667
    )
  )

  estimate <- meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name
  )
  estimate$raw_data
  estimate


  estimate <- meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    subset,
    reported_effect_size = "smd_unbiased"
  )
  destimate <- meta_d1(
    original_7,
    d1_unbiased,
    rt_n,
    moderator = "subset",
    study_name
  )
  estimate$raw_data
  destimate$raw_data
  estimate
  destimate


  means <- "rt_mean"
  sds <- "rt_sd"
  ns <- "rt_n"
  labels <- "study_name"
  moderator <- "subset"

  estimate <- meta_mean(
    data = original_7,
    means = !!means,
    sds = !!sds,
    ns = !!ns,
    labels = !!labels,
    moderator = subset
  )

  estimate <- do.call(
    what = meta_mean,
    args = list(
      data = original_7,
      means = means,
      sds = sds,
      ns = ns,
      labels = labels,
      moderator = "subset"
    )
  )

  estimate <- meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    conf_level = 0.99
  )
  estimate$raw_data
  estimate

  estimate <- meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    random_effects = FALSE,
    conf_level = 0.99
  )
  estimate$raw_data
  estimate


  estimate <- meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    subset
  )
  estimate$raw_data
  estimate


  estimate <- meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    reference_mean = 300
  )
  estimate$raw_data
  estimate


  estimate <- meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    reported_effect_size = "smd",
    reference_mean = 300
  )
  estimate$raw_data
  estimate

}

