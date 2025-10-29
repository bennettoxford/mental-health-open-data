library(readxl)
library(janitor)
library(purrr)

source(here::here("lib/functions/fun_nhstt.R"))

nhstt_metadata_xlsx_specs <- list(
  measures = list(
    url = "https://digital.nhs.uk/binaries/content/assets/website-assets/data-and-information/datasets/nhs-talking-therapies/reports/nhs_talking_therapies-publications-list-10042025.xlsx",
    sheets = list(
      list(sheet = 3, range = "A3:I534"),
      list(sheet = 4, range = "A3:D334")
    )
  ),
  fy2223 = list(
    url = "https://files.digital.nhs.uk/5D/1BE581/psych-ther-ann-2022-23-meta-V1.1.xlsx",
    sheets = list(
      list(sheet = 3, range = "A10:D57"),
      list(sheet = 4, range = "A10:E137"),
      list(sheet = 5, range = "A10:G271"),
      list(sheet = 6, range = "A10:H308")
    )
  ),
  fy2324 = list(
    url = "https://files.digital.nhs.uk/6C/47D124/psych-ther-ann-2023-24-meta.xlsx",
    sheets = list(
      list(sheet = 3, range = "A10:D57"),
      list(sheet = 4, range = "A10:E137"),
      list(sheet = 5, range = "A10:G271"),
      list(sheet = 6, range = "A10:H308")
    )
  ),
  fy2425 = list(
    url = "https://files.digital.nhs.uk/BE/AAF6D0/psych-ther-ann-2024-25-meta.xlsx",
    sheets = list(
      list(sheet = 3, range = "A10:D57"),
      list(sheet = 4, range = "A10:E137"),
      list(sheet = 5, range = "A10:G271"),
      list(sheet = 6, range = "A10:H308")
    )
  )
)

read_xlsx_sheets <- function(file_spec) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(file_spec$url, temp_file, mode = "wb")
  
  data_list <- map(file_spec$sheets, ~read_excel(temp_file, sheet = .x$sheet, range = .x$range) |> clean_names())
  sheet_names <- map_chr(file_spec$sheets, ~excel_sheets(temp_file)[.x$sheet]) |> make_clean_names()
  names(data_list) <- paste0("metadata_", sheet_names)
  
  unlink(temp_file)
  data_list
}

all_meta_data <- map(
  nhstt_metadata_xlsx_specs,
  read_xlsx_sheets
)

all_meta_data

# Write all raw data
write_nested_csvs(
  all_meta_data,
  base_path = here("lib/data_raw")
)
