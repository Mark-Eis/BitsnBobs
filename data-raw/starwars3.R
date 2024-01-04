## code to prepare `starwars3` dataset goes here
starwars3 <- dplyr::starwars |> dplyr::select(c(name, skin_color)) |>
  dplyr::arrange(stringr::str_length(skin_color) |> desc()) |> head(10) |> dplyr::arrange(name)

usethis::use_data(starwars3, overwrite = TRUE)
