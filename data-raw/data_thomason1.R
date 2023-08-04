## code to prepare `data_thmason1` dataset goes here

data_thomason1 <- data.frame(
  pretest = c(
    13,
    12,
    12,
    9,
    14,
    17,
    14,
    9,
    6,
    7,
    11,
    15

  ),
  posttest = c(
    14,
    13,
    16,
    12,
    15,
    18,
    13,
    10,
    10,
    8,
    14,
    16

  )
)

usethis::use_data(data_thomason1, overwrite = TRUE)
