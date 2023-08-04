## code to prepare `data_halagappa_et_al_2007` dataset goes here

data_halagappa_et_al_2007 <- data.frame(
  condition = as.factor(
    c(
      "NFree10",	"AFree10",	"ADiet10",	"NFree17",	"AFree17",	"ADiet17"
    )
  ),

  n = c(
    19,	19,	19,	19,	19,	19
  ),

  m = c(
    37.5,	31.9,	41.2,	33.4,	29.9,	38.3

  ),

  s = c(
    10,	13.5,	14.8,	10,	8.7,	10
  )
)

usethis::use_data(data_halagappa_et_al_2007, overwrite = TRUE)

