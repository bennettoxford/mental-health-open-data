# Function to create table using gt package grouped by semantic tags
# The input data need to follow a specific data structure, e.g.,:
#    snomed_code      description_short                             semantic_tag         usage ratio_usage
#    <chr>            <chr>                                         <chr>                <int>       <dbl>
#  1 720433000        Patient Health Questionnaire Nine Item score  observable entity 16229920  0.912
#  2 836541000000100  Patient health questionnaire 2 score          observable entity  1295170  0.0727
#  3 836521000000107  Patient health questionnaire 2                assessment scale    103210  0.00580
#  4 758711000000105  Patient health questionnaire 9                assessment scale     65440  0.00368

create_table_sem_tag <- function(data, title = NULL, subtitle = NULL) {

  # Check that all variables that are needed to create the table are available
  required_cols <- c(
    "snomed_code",
    "description_short",
    "usage",
    "ratio_usage",
    "semantic_tag"
  )
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Create table
  table <- data |>
    mutate(semantic_tag = str_to_sentence(semantic_tag)) |>
    group_by(semantic_tag) |>
    gt() |>
    fmt_percent(
      columns = c(ratio_usage),
      decimals = 3
    ) |>
    tab_style(
      style = list(
        cell_text(font = "Courier New")
      ),
      locations = cells_body(columns = snomed_code)
    ) |>
    cols_label(
      snomed_code = "SNOMED code",
      description_short = "Description",
      usage = "Usage",
      ratio_usage = "%"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) |>
    cols_align(
      align = "left",
      columns = snomed_code
    ) |>
    tab_options(table.width = "100%") |>
    tab_source_note(
      source_note = md(
        "Data scource: NHS England SNOMED Usage in Primary Care."
      )
    )

  # Add titles if specified by user
  if (!is.null(title)) {
    table <- table |>
      tab_header(
        title = md(title),
        subtitle = md(subtitle)
      )
  }

  # Return table
  table
}