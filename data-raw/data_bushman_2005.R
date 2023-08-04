## code to prepare `data_penlaptop2` dataset goes here

data_bushman_2005 <- data.frame(
  condition = as.factor(
    c(
      "Neutral",
      "Violent",
      "Sexual"
    )
  ),

  n = c(
    84,
    84,
    84
  ),

  m = c(
    5.76,
    4.36,
    3.54
  ),

  s = c(
    2.47,
    2.57,
    2.57
  )
)

usethis::use_data(data_bushman_2005, overwrite = TRUE)

