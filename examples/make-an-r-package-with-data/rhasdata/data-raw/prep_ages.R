# Generated from create-rhasdata.Rmd: do not edit by hand

ages <- read.csv("data-raw/my-raw-data.csv")
ages <- data.frame(id = seq_len(nrow(ages)), age = ages$ages)
usethis::use_data(ages)
