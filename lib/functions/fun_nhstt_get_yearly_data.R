source(here::here("lib/functions/fun_nhstt.R"))

# Until FY 21/22 get data from IAPT website
# https://digital.nhs.uk/data-and-information/publications/statistical/psychological-therapies-annual-reports-on-the-use-of-iapt-services
# From FY 22/23 get data from NHS Talking Therapies website
# https://digital.nhs.uk/data-and-information/publications/statistical/nhs-talking-therapies-for-anxiety-and-depression-annual-reports

# Data set archived guidance documents
# Version 2.1 - Release date July 2021
# https://digital.nhs.uk/data-and-information/information-standards/governance/latest-activity/standards-and-collections/dapb-1520-improving-access-to-psychological-therapies-data-set
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/improving-access-to-psychological-therapies-data-set/iapt-data-set-v2.0-archived-guidance-documents

# Version 2.0 - Release date September 2019
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/improving-access-to-psychological-therapies-data-set/iapt-data-set-v2.0-archived-guidance-documents

# Version 1.5 - Release date June 2014
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/improving-access-to-psychological-therapies-data-set/iapt-data-set-v1.5-archived-guidance-documents
# https://ppn.nhs.uk/attachments/article/53/IAPTDataSetSummaryofChangesJune2014.pdf

# Version 1.0 - Release date August 2010
# https://digital.nhs.uk/data-and-information/data-collections-and-data-sets/data-sets/improving-access-to-psychological-therapies-data-set/iapt-pilot-collections
# https://doczz.net/doc/3780051/the-iapt-data-handbook-appendices

nhstt_annual_reports_links <- list(

  # IAPT Data Set Version 1.5 
  # "fy1314" = https://files.digital.nhs.uk/publicationimport/pub14xxx/pub14899/psych-ther-ann-rep-tab-2013-14.xlsx
  # "fy1415" = https://files.digital.nhs.uk/publicationimport/pub19xxx/pub19098/psych-ther-ann-rep-tab-2014-15.xlsx

  # IAPT
  "fy1516" = "https://files.digital.nhs.uk/zip/2/j/psych-ther-ann-rep-csv-pack-2015-16.zip",
  "fy1617" = "https://files.digital.nhs.uk/F8/5B17FF/psych-ther-ann-rep-csv-pack-2016-17_v3.0.zip",
  "fy1718" = "https://files.digital.nhs.uk/82/2CCBED/psych-ther-ann-2017-18-csvs.zip",
  "fy1819" = "https://files.digital.nhs.uk/88/EBA9A6/psych-ther-ann-2018-19-csvs.rar",
  "fy1920" = "https://files.digital.nhs.uk/1A/F2ABB3/psych-ther-ann-2019-20-csvs.zip",
  "fy2021" = "https://files.digital.nhs.uk/62/DBF395/psych-ther-ann-rep-csvs-2020-21.zip",
  "fy2122" = "https://files.digital.nhs.uk/DC/9E8751/psych-ther-ann-rep-csvs-2021-22.zip",

  # NHS Talking Therapies
  "fy2223" = "https://files.digital.nhs.uk/CA/8F53D1/psych-ther-ann-rep-csvs-2022-23.zip",
  "fy2324" = "https://files.digital.nhs.uk/4E/6D88C2/psych-ther-ann-rep-csvs-2023-24.zip",
  "fy2425" = "https://files.digital.nhs.uk/4E/2104A0/psych-ther-ann-rep-csv-2024-25-2.zip"
)

dfs <- nhstt_annual_reports_links |> 
  map(get_nhstt_yearly)

# Write all raw data
write_nested_csvs(
  dfs,
  base_path = here("lib/data_raw")
)

dfs |> 
  map(names)

# Main dataset
dfs_main <- select_datasets(
  dfs,
  datasets = "main",
  remove_empty = TRUE
)

dfs_main |> 
  map(names)

df_main_variables <- check_names_across_years(dfs_main)
df_main_variables$complete

main_vars_list <- c(
  "org_type",
  "org_code",
  "org_name",
  "variable_type",
  "variable_a",
  "variable_b",

  "count_referrals_received",
  "count_finished_course_treatment",

  "count_at_caseness",
  "count_not_at_caseness",

  "count_improvement",
  "percentage_improvement",

  "count_deterioration",
  "percentage_deterioration",

  "count_no_reliable_change",
  "percentage_no_reliable_change",

  "count_reliable_recovery",
  "percentage_reliable_recovery",

  "count_recovery",
  "percentage_recovery"
)


dfs_main_wide <- flatten_one_level(dfs_main) |> 
  map(~filter(.x, 
    org_type == "England",
    variable_type %in% c(
      "Problem Descriptor",
      "Presenting Complaint"
    )
  )) |> 
  map(~select(.x, all_of(main_vars_list))) |> 
  map(~mutate(.x, across(everything(), ~na_if(.x, "NULL")))) |> 
  map(~mutate(.x, across(matches("^(count|mean|median|percentage|sum)_"), ~as.numeric(.x))))

# Check data
dfs_main$fy1718
dfs_main$fy2324

df_main <- dfs_main_wide |> 
  bind_rows(.id = "nhs_fy") |> 
  pivot_longer(
    cols = matches("^(count|mean|median|percentage|sum)_"),
    values_to = "value",
    values_ptypes = numeric(),
    names_to = "measure"
  ) |> 
  extract(
  col = measure,
  into = c("statistic", "measure"),
  regex = "^(count|mean|median|percentage|sum)_(.+)$"
  ) |> 
  mutate(
    start_year = as.integer(str_sub(nhs_fy, 3, 4)) + 2000,
    end_year = as.integer(str_sub(nhs_fy, 5, 6)) + 2000,
    start_date = as.Date(paste0(start_year, "-04-01")),
    end_date = as.Date(paste0(end_year, "-03-31"))
  ) |>
  select(
    start_date,
    end_date,
    nhs_fy,
    org_type,
    org_code,
    org_name,
    variable_type,
    variable_a,
    variable_b,
    statistic,
    measure,
    value
  )

df_main

unique(df_main$variable_type)
unique(df_main$variable_a)
unique(df_main$variable_b)
unique(df_main$statistic)
unique(df_main$measure)

write_csv(
  df_main,
  here("lib/data/nhstt_problem_descriptor_england_per_year.csv")
)

# Meds dataset
dfs_meds <- select_datasets(
  dfs,
  datasets = "meds"
)

dfs_meds |> 
  map(names)

df_meds_variables <- check_names_across_years(dfs_meds)
df_meds_variables$complete

meds_var_mapping <- c(
  "countstatus_end_prescribed_but_not_taking" = "count_status_end_prescribed_but_not_taking",
  "not_prescribed" = "count_status_end_not_prescribed",
  "not_stated_known_invalid" = "count_status_end_not_stated_not_known_invalid",
  "prescribed_not_taking" = "count_status_end_prescribed_but_not_taking",
  "prescribed_taking" = "count_status_end_prescribed_and_taking",
  "total" = "count_status_end_total"
)

dfs_meds <- standardise_variables(dfs_meds, meds_var_mapping)
check_names_across_years(dfs_meds)

meds_vars_list <- c(
  "org_type",
  "org_code",
  "org_name",
  "variable_type",
  "variable_a",
  "variable_b",

  "count_status_end_total",
  "count_status_end_prescribed_but_not_taking",
  "count_status_end_prescribed_and_taking",
  "count_status_end_not_prescribed",
  "count_status_end_not_stated_not_known_invalid"
)

dfs_meds_wide <- flatten_one_level(dfs_meds) |> 
  map(~filter(.x, 
    org_type == "England",
    variable_type %in% c(
      "Psychotropic Medication Status"
    )
  )) |> 
  map(~select(.x, all_of(meds_vars_list))) |> 
  map(~mutate(.x, across(everything(), ~na_if(.x, "NULL")))) |> 
  map(~mutate(.x, across(matches("^(count|mean|median|percentage|sum)_"), ~as.numeric(.x))))

# Check data
dfs_meds$fy1718
dfs_meds$fy2324

df_meds <- dfs_meds_wide |> 
  bind_rows(.id = "nhs_fy") |> 
  pivot_longer(
    cols = matches("^(count|mean|median|percentage|sum)_"),
    values_to = "value",
    values_ptypes = numeric(),
    names_to = "measure"
  ) |> 
  extract(
  col = measure,
  into = c("statistic", "measure"),
  regex = "^(count|mean|median|percentage|sum)_(.+)$"
  ) |> 
  mutate(
    start_year = as.integer(str_sub(nhs_fy, 3, 4)) + 2000,
    end_year = as.integer(str_sub(nhs_fy, 5, 6)) + 2000,
    start_date = as.Date(paste0(start_year, "-04-01")),
    end_date = as.Date(paste0(end_year, "-03-31"))
  ) |>
  select(
    start_date,
    end_date,
    nhs_fy,
    org_type,
    org_code,
    org_name,
    variable_type,
    variable_a,
    variable_b,
    statistic,
    measure,
    value
  )

df_meds

unique(df_meds$variable_type)
unique(df_meds$variable_a)
unique(df_meds$variable_b)
unique(df_meds$statistic)
unique(df_meds$measure)

write_csv(
  df_meds,
  here("lib/data/nhstt_psych_meds_england_per_year.csv")
)
