# Shared helper functions for the Stuff+ workflow.

load_stuff_packages <- function() {
  needed <- c("dplyr", "tidyr", "stringr", "readr", "purrr")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    stop(
      "Missing required package(s): ", paste(missing, collapse = ", "),
      "\nInstall with: install.packages(c(",
      paste0('"', missing, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(readr)
    library(purrr)
  })
}

pick_col <- function(df, choices) {
  choices <- choices[!is.na(choices) & nzchar(choices)]
  found <- choices[choices %in% names(df)]

  if (length(found) == 0) {
    return(NULL)
  }

  found[[1]]
}

add_missing_cols <- function(df, cols, value = NA) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- value
    }
  }

  df
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  mean(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_real_)
  }

  max(x)
}

safe_z <- function(x) {
  n <- sum(!is.na(x))

  if (n == 0) {
    return(rep(NA_real_, length(x)))
  }

  if (n == 1) {
    return(rep(0, length(x)))
  }

  s <- sd(x, na.rm = TRUE)

  if (is.na(s) || s == 0) {
    return(rep(0, length(x)))
  }

  (x - mean(x, na.rm = TRUE)) / s
}

safe_percentile <- function(x) {
  if (sum(!is.na(x)) <= 1) {
    return(rep(NA_real_, length(x)))
  }

  dplyr::percent_rank(x) * 100
}

to_logical_flag <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  x_chr <- tolower(as.character(x))

  dplyr::case_when(
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

stuff_grade_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x >= 130 ~ "Elite",
    x >= 120 ~ "Plus-plus",
    x >= 110 ~ "Plus",
    x >= 105 ~ "Above avg",
    x >= 95 ~ "Average",
    x >= 90 ~ "Below avg",
    TRUE ~ "Needs work"
  )
}
