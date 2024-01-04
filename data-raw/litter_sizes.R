## code to prepare `litters` dataset goes here
litters <- rep(1:12, c(7, 33, 58, 116, 125, 126, 121, 107, 56, 37, 25, 4))
usethis::use_data(litters, overwrite = TRUE)
