source(here::here("lib/functions/fun_nhstt.R"))

nhstt_annual_tbo_links <- list(
  "fy2223" = "https://files.digital.nhs.uk/70/F00866/ther-based-outcomes2223.zip",
  "fy2324" = "https://files.digital.nhs.uk/FB/021DCD/ther-based-outcomes2324.zip",
  "fy2425" = "https://files.digital.nhs.uk/B7/464F54/ther-based-outcomes2425-2.zip"
)

dfs <- nhstt_annual_tbo_links |>
  map(get_nhstt_yearly)

# Write all raw data
write_nested_csvs(
  dfs,
  base_path = here("lib/data_raw")
)

dfs_selected <- select_datasets(dfs, dataset = c("table1"))

dfs_flat <- flatten_one_level(dfs_selected)

df_main <- dfs_flat |>
  bind_rows(.id = "nhs_fy") %>%
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = str_remove(measure, "^count_"),
    start_year = as.integer(str_sub(nhs_fy, 3, 4)) + 2000,
    end_year = as.integer(str_sub(nhs_fy, 5, 6)) + 2000,
    start_date = as.Date(paste0(start_year, "-04-01")),
    end_date = as.Date(paste0(end_year, "-03-31")),
    statistic = "count"
  ) %>%
  select(
    start_date,
    end_date,
    nhs_fy,
    therapy_type,
    statistic,
    measure,
    value
  )

fs::dir_create(here("lib", "data", "yearly", "tbo"))

write_csv(
  df_main,
  here("lib/data/yearly/tbo/nhstt_tbo_england_table1.csv")
)
