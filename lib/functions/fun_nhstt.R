library(tidyverse)
library(here)
library(janitor)
library(archive)
library(fs)
library(cli)

get_nhstt_yearly <- function(url, cleanup = TRUE) {

  file_ext <- tolower(tools::file_ext(url))
  temp_archive <- tempfile(fileext = paste0(".", file_ext))
  temp_dir <- tempfile()

  dir.create(temp_dir)
  download.file(url, temp_archive, mode = "wb")
  
  if (file_ext == "zip") {
    unzip(temp_archive, exdir = temp_dir)
  } else if (file_ext == "rar") {
    archive_extract(temp_archive, dir = temp_dir)
  } else {
    stop("Unsupported file type: ", file_ext, ". Only .zip and .rar supported.")
  }
  
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  data_list <- map(csv_files, ~read_csv(.x, name_repair = janitor::make_clean_names, show_col_types = FALSE))
  names(data_list) <- janitor::make_clean_names(basename(csv_files))
  
  if (cleanup) {
    unlink(temp_archive)
    unlink(temp_dir, recursive = TRUE)
  }

  return(data_list)
}

write_nested_csvs <- function(dfs, base_path = here()) {
  iwalk(dfs, function(tables, year_dir) {
    dir_path <- here(base_path, year_dir)
    dir_create(dir_path)
    iwalk(tables, ~write_csv(.x, here(dir_path, paste0(.y, ".csv"))))
  })
}

select_datasets <- function(
  dfs, 
  datasets = c(
    "main",
    "effect_size",
    "employment_status",
    "iet_ltc",
    "peq",
    "meds",
    "therapist_role",
    "therapy_type"
  )
) {

  datasets <- match.arg(datasets, several.ok = TRUE)
  
  dataset_patterns <- list(
    effect_size = "effect_size",
    employment_status = "emp(loyment)?_status",
    iet_ltc = "iet_ltc",
    main = "main",
    peq = "peq",
    meds = "psych_med",
    therapist_role = "therapist_role",
    therapy_type = "therapy_type"
  )
    
  if (!is.null(datasets)) {
    patterns <- dataset_patterns[datasets]
    dfs_return <- map(dfs, function(tables) {
      keep_idx <- map_lgl(names(tables), function(nm) {
        any(map_lgl(patterns, ~str_detect(nm, .x)))
      })
      tables[keep_idx]
    })
  }
  
  dfs_return
}

check_var_names <- function(dfs) {
  all_vars <- map(dfs, function(year_list) {
    map(year_list, names) |>
      flatten_chr() |>
      unique()
  })
  
  all_unique_unsorted <- all_vars |> flatten_chr() |> unique()
  all_unique <- all_unique_unsorted |> sort()
  years <- names(all_vars)
  
  presence <- map(all_unique, function(var) {
    map_lgl(all_vars, ~var %in% .x)
  }) |>
    set_names(all_unique) |>
    as_tibble()
  
  complete_vars_sorted <- all_unique[map_lgl(all_unique, ~all(presence[[.x]]))]
  complete_vars <- all_unique_unsorted[all_unique_unsorted %in% complete_vars_sorted]
  partial_vars <- all_unique[map_lgl(all_unique, ~!all(presence[[.x]]))]
  
  max_var_length <- max(nchar(c(complete_vars, partial_vars)))
  n_years <- length(years)
  header_width <- max_var_length + 2 + (n_years * 3)
  
  cat(cli::rule(left = "Variable comparison across years", col = "cyan", line = 2))
  cat(cli::col_green(paste0(cli::symbol$tick, " ", length(complete_vars), " variables present in all years")), "\n")
  cat(cli::col_red(paste0(cli::symbol$cross, " ", length(partial_vars), " variables missing in some years")), "\n")
  cat(cli::rule(col = "cyan", line = 2), "\n")
  
  cat(cli::col_green(cli::style_bold("Variables present in all years:")), "\n")
  cat(strrep("─", header_width), "\n")
  
  walk(complete_vars, function(var) {
    status_symbols <- map_chr(years, ~cli::col_green(cli::symbol$tick))
    var_padded <- str_pad(var, max_var_length + 2, "right")
    year_status <- paste(status_symbols, collapse = "  ")
    cat(cli::col_green(var_padded), year_status, "\n")
  })
  cat("\n")
  
  cat(cli::col_yellow(cli::style_bold("Variables missing in some years:")), "\n")
  cat(strrep("─", header_width), "\n")
  
  walk(partial_vars, function(var) {
    present_in <- presence[[var]]
    status_symbols <- map_chr(present_in, function(p) {
      if (p) cli::col_green(cli::symbol$tick) else cli::col_red(cli::symbol$cross)
    })
    
    var_padded <- str_pad(var, max_var_length + 2, "right")
    year_status <- paste(status_symbols, collapse = "  ")
    cat(cli::col_yellow(var_padded), year_status, "\n")
  })
  cat("\n")
  
  invisible(
    list(
      complete = complete_vars,
      partial = partial_vars,
      presence = presence
    )
  )
}

flatten_one_level <- function(dfs) {
  map(dfs, ~.x[[1]])
}

standardise_variables <- function(dfs, var_mapping) {
  map(dfs, function(year_list) {
    map(year_list, function(df) {
      df |>
        rename_with(
          ~map_chr(.x, function(col) {
            if (col %in% names(var_mapping)) {
              var_mapping[[col]]
            } else {
              col
            }
          })
        )
    })
  })
}
