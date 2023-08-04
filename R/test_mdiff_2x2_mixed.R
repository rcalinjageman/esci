test_mdiff_2x2_mixed <- function() {
  pre <- post <- condition <- NULL

  mydf <- data.frame(
    pre = c(
      18, 19, 20, 17, 20, 16,
      19, 16, 16, 14, 16, 18
    ),
    post = c(
      19, 18, 19, 20, 17, 16,
      16, 10, 12,  9, 13, 15
    ),
    condition = as.factor(
      c(
        "Ctl", "Ctl", "Ctl", "Ctl", "Ctl", "Ctl",
        "Exp", "Exp", "Exp", "Exp", "Exp", "Exp"
      )
    )
  )


  mestimate <- estimate_mdiff_2x2_mixed(
    mydf,
    outcome_variable_level1 = pre,
    outcome_variable_level2 = post,
    grouping_variable = condition,
    outcome_variable_name = "Reaction time",
    repeated_measures_name = "Phase",
    save_raw_data = TRUE
  )

  plot_mdiff(mestimate$interaction)

}
