# Generated from create-rhasdata.Rmd: do not edit by hand

ages <- readr::read_csv("data-raw/my-raw-data.csv")
ages <- ages |> 
  dplyr::transmute(id = dplyr::row_number(), age = ages)
usethis::use_data(ages)
