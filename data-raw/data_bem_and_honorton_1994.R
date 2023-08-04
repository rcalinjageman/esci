## code to prepare `data_bem_and_honorton_1994` dataset goes here

data_bem_and_honorton_1994 <- data.frame(
  study = as.factor(
    c(
      "Pilot 1",
      "Pilot 2",
      "Pilot 3",
      "Study 1",
      "Study 2",
      "Study 3",
      "Study 4",
      "Study 5",
      "Study 6",
      "Study 7"
    )
  ),

  hits = c(
    8,
    3,
    10,
    12,
    18,
    15,
    12,
    10,
    3,
    15

  ),

  trials = c(
    22,
    9,
    35,
    50,
    50,
    50,
    36,
    20,
    7,
    50

  )
)

usethis::use_data(data_bem_and_honorton_1994, overwrite = TRUE)
