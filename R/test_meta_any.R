test_estimate_meta_any <- function() {

  effect_size <- variance <- mymod <- bad_mod <- NULL

  my_meta <- data.frame(
    labels = paste("Study", seq(from = 1, to = 30, by = 1), sep = ""),
    effect_size = c(
      rnorm(n = 10, mean = 100, sd = 15),
      rnorm(n = 10, mean = 115, sd = 15),
      rnorm(n = 10, mean = 130, sd = 15)
    ),
    variance = (rnorm(n = 30, mean = 9, sd = 15*sqrt(2/24)))^2,
    mymod = as.factor(
      c(
        rep(x = "Normal", times = 10),
        rep(x = "Online", times = 10),
        rep(x = "Biased", times = 10)
      )
    )
  )


  estimate <- meta_any(my_meta, effect_size, variance)

  meta_any(my_meta, "effect_size", "variance")

  meta_any(my_meta, effect_size, variance, labels, mymod)

  meta_any(
    data = my_meta,
    yi = effect_size,
    vi = variance,
    labels = labels,
    moderator = mymod,
    contrast = c(1/2, -1, 1/2),
    effect_label = "Mean IQ",
    moderator_variable_name = "Participant Pool",
  )



  # Bad versions -------------------------------------------
  # Moderator is not a factor
  meta_any(
    data = cbind(
      my_meta, data.frame(
        bad_mod = c(
         rep(x = "Normal", times = 10),
         rep(x = "Online", times = 10),
         rep(x = "Biased", times = 10)
        )
      )
    ),
    yi = effect_size,
    vi = variance,
    labels = labels,
    moderator = bad_mod,
    contrast = c(1/2, -1, 1/2),
    effect_label = "Mean IQ",
    moderator_variable_name = "Participant Pool",
    random_effects = TRUE,
    conf_level = 0.95
  )

  # A moderator has only 1 instance
  meta_any(
    data = cbind(
      my_meta, data.frame(
        bad_mod = as.factor(
          c(
            rep(x = "Normal", times = 15),
            rep(x = "Online", times = 14),
            rep(x = "Biased", times = 1)
          )
        )
      )
    ),
    yi = effect_size,
    vi = variance,
    labels = labels,
    moderator = bad_mod,
    contrast = c(1/2, -1, 1/2),
    effect_label = "Mean IQ",
    moderator_variable_name = "Participant Pool",
    random_effects = TRUE,
    conf_level = 0.95
  )

  # Contrast doesn't match number of valid moderator levels
  bdata <- cbind(
    my_meta,
    data.frame(
      bad_mod = as.factor(
        c(
          rep(x = "Normal", times = 10),
          rep(x = "Online", times = 10),
          rep(x = "Biased", times = 10)
        )
      )
    )
  )

  # Level that is not used
  levels(bdata$bad_mod) <- c(levels(bdata$bad_mod), "Unused")
  meta_any(
    bdata,
    yi = effect_size,
    vi = variance,
    labels = labels,
    moderator = bad_mod,
    contrast = c(1/2, -1, 1/2, 0),
    effect_label = "Mean IQ",
    moderator_variable_name = "Participant Pool",
    random_effects = TRUE,
    conf_level = 0.95
  )

  # NAs
  bad_meta <- my_meta
  bad_meta[c(2, 3, 10), "effect_size"] <- NA
  bad_meta[c(12, 13, 10), "variance"] <- NA
  bad_meta[c(12, 13, 21), "labels"] <- NA
  bad_meta[c(22, 23, 21), "mymod"] <- NA
  meta_any(
    data = bad_meta,
    yi = effect_size,
    vi = variance,
    labels = labels,
    moderator = mymod,
    contrast = c(1/2, -1, 1/2),
    effect_label = "Mean IQ",
    moderator_variable_name = "Participant Pool",
  )


}

