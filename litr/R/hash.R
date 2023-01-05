# Generated from create-litr.Rmd: do not edit by hand

#' Hash package directory
#' 
#' Gets an identifier that can be used to uniquely (whp) identify the current 
#' state of the package. This is formed by ignoring the `LitrId` field of the
#' DESCRIPTION file, which is the location where the output of this function is 
#' stored when `litr::render` generates the package.
#' 
#' @param package_dir Path to package
#' @keywords internal
hash_package_directory <- function(package_dir) {
  pkg_files <- fs::dir_ls(package_dir, recurse = TRUE, all = TRUE, type = "file")
  pkg_files <- stringr::str_subset(pkg_files, ".DS_Store$", negate = TRUE)
  pkg_files <- normalizePath(pkg_files)
  descr_file <- normalizePath(file.path(package_dir, "DESCRIPTION"))
  i_descr <- which(pkg_files == descr_file)
  if (length(i_descr) == 0) stop("Cannot find DESCRIPTION file.")
  txt_descr <- readLines(pkg_files[i_descr])
  txt_descr_mod <- stringr::str_subset(
    txt_descr, 
    stringr::str_glue("{description_litr_hash_field_name()}: .+$"),
    negate = TRUE)
  hashes <- as.character(tools::md5sum(pkg_files[-i_descr]))
  digest::digest(c(hashes, list(txt_descr_mod)))
}

#' Generate litr hash field name for DESCRIPTION file
#' @keywords internal
description_litr_hash_field_name <- function() return("LitrId")

#' Write the hash of the package to the DESCRIPTION file
#' 
#' @param package_dir Path to package
#' @keywords internal
write_hash_to_description <- function(package_dir) {
  hash <- hash_package_directory(package_dir)
  desc::desc_set(description_litr_hash_field_name(), hash, 
                 file = file.path(package_dir, "DESCRIPTION"))
}

#' Get the hash of the package from the DESCRIPTION file
#' 
#' @param package_dir Path to package
#' @keywords internal
read_hash_from_description <- function(package_dir) {
  descr <- file.path(package_dir, "DESCRIPTION")
  if (!file.exists(descr)) stop("Cannot find DESCRIPTION file.")
  txt <- stringr::str_subset(
    readLines(descr), 
    stringr::str_glue("{description_litr_hash_field_name()}: .+$"))
  if (length(txt) > 1) stop("More than one hash found in DESCRIPTION.")
  if (length(txt) == 0) stop("No hash found in DESCRIPTION.")
  stringr::str_extract(txt, "\\S+$")
}

#' Check if package directory is the unedited output of litr::render()
#' 
#' Uses hash stored in a special `litr` field of DESCRIPTION file to check that 
#' the current state of the R package directory is identical to its state at the
#' time that it was created by `litr::render()`.
#' 
#' @param package_dir Path to package
#' @keywords internal
check_unedited <- function(package_dir) {
  hash <- hash_package_directory(package_dir)
  hash == read_hash_from_description(package_dir)
}
