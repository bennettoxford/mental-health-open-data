library(reactable)
library(htmlwidgets)


create_category_regex_pattern <- function(category_terms) {
  str_c("(?i)", str_c(category_terms, collapse = "|"))
}

identify_matching_terms <- function(text, category_terms) {
  matches_term <- function(term) {
    str_detect(text, regex(term, ignore_case = TRUE))
  }
  
  category_terms[map_lgl(category_terms, matches_term)]
}

format_matching_terms <- function(text, category_terms) {
  matching_terms <- identify_matching_terms(text, category_terms)
  
  if (length(matching_terms) == 0) {
    return(NA_character_)
  }
  
  str_c(matching_terms, collapse = "; ")
}

add_category_columns <- function(matched_rows, category_name, category_terms) {
  matched_rows |>
    mutate(
      search_category = category_name,
      matching_term = map_chr(description, ~format_matching_terms(.x, category_terms)),
      .before = 1
    )
}

search_category <- function(data, category_name, category_pattern, category_terms) {
  matched_idx <- str_detect(data$description, category_pattern)
  
  if (!any(matched_idx)) {
    return(NULL)
  }
  
  data[matched_idx, ] |>
    add_category_columns(category_name, category_terms)
}

find_matching_codes <- function(data, search_terms) {
  patterns <- map(search_terms, create_category_regex_pattern)
  
  map_dfr(names(patterns), ~search_category(
    data,
    category_name = .x,
    category_pattern = patterns[[.x]],
    category_terms = search_terms[[.x]]
  ))
}

create_search_summary_df <- function(matching_codes_tibble, snomed_usage) {
  matching_codes_tibble |>
    left_join(
      snomed_usage |> 
        group_by(snomed_code) |> 
        summarise(total_usage = sum(usage, na.rm = TRUE)),
      by = "snomed_code"
    ) |>
    separate_rows(matching_term, sep = "; ") |>
    group_by(search_category, matching_term) |>
    summarise(
      n_codes = n_distinct(snomed_code),
      total_usage = sum(total_usage, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(search_category, desc(n_codes))
}

create_gt_table <- function(summary_df) {
  summary_df |>
    gt(groupname_col = "search_category") |>
    cols_label(
      matching_term = "Search Term",
      n_codes = "Unique Codes",
      total_usage = "Total Usage"
    ) |>
    fmt_number(
      columns = c(n_codes, total_usage),
      decimals = 0,
      use_seps = TRUE
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    )
}

process_matching_codes <- function(
  matching_codes_tibble,
  snomed_usage,
  condition_names,
  exclude_semantic_tags = NULL
) {
  processed_codes <- matching_codes_tibble |>
    mutate(
      semantic_tag = extract_semantic_tag(description),
      description = strip_semantic_tag(description)
    ) |>
    left_join(
      snomed_usage |>
        group_by(snomed_code) |>
        summarise(code_usage = sum(usage, na.rm = TRUE)),
      by = "snomed_code"
    ) |>
    select(
      search_category,
      matching_term,
      snomed_code,
      description,
      semantic_tag,
      code_usage
    ) |>
    distinct() |>
    mutate(
      search_category = recode(search_category, !!!condition_names),
      selected = FALSE
    )

  if (!is.null(exclude_semantic_tags) && length(exclude_semantic_tags) > 0) {
    processed_codes <- processed_codes |>
      filter(!semantic_tag %in% exclude_semantic_tags)
  }

  processed_codes
}

create_interactive_table_with_export <- function(processed_codes_tibble) {
  table_id <- paste0("table_", sample(1000:9999, 1))

  summary_df <- processed_codes_tibble |>
    separate_rows(matching_term, sep = "; ") |>
    rename(condition = search_category) |>
    group_by(condition, matching_term) |>
    summarise(
      n_codes = n_distinct(snomed_code),
      total_usage = sum(code_usage, na.rm = TRUE),
      codes = list(
        tibble(
          snomed_code = snomed_code,
          description = description,
          semantic_tag = semantic_tag,
          usage = code_usage
        ) |>
          distinct() |>
          arrange(desc(usage))
      ),
      .groups = "drop"
    ) |>
    arrange(condition, desc(n_codes))

  table_widget <- reactable(
    summary_df,
    elementId = table_id,
    width = "100%",
    columns = list(
      condition = colDef(
        name = "Condition",
        minWidth = 250,
        style = list(fontFamily = "sans-serif")
      ),
      matching_term = colDef(
        name = "Search Term",
        minWidth = 150,
        style = list(fontFamily = "sans-serif")
      ),
      n_codes = colDef(
        name = "Unique Codes",
        format = colFormat(separators = TRUE),
        align = "right",
        width = 120,
        style = list(fontFamily = "sans-serif")
      ),
      total_usage = colDef(
        name = "Total Usage",
        format = colFormat(separators = TRUE),
        align = "right",
        width = 120,
        style = list(fontFamily = "sans-serif")
      ),
      codes = colDef(show = FALSE)
    ),
    details = function(index) {
      code_data <- summary_df$codes[[index]]
      current_condition <- summary_df$condition[index]
      current_search_term <- summary_df$matching_term[index]

      htmltools::div(
        style = "padding: 1rem",
        reactable(
          code_data |> mutate(selected = FALSE),
          width = "100%",
          columns = list(
            snomed_code = colDef(
              name = "SNOMED Code",
              width = 180,
              style = function(value) {
                list(
                  fontFamily = "monospace",
                  fontSize = "1em",
                  backgroundColor = "#f5f5f5",
                  whiteSpace = "nowrap",
                  overflow = "visible",
                  wordWrap = "normal",
                  wordBreak = "normal"
                )
              }
            ),
            description = colDef(
              name = "Description",
              style = list(fontFamily = "sans-serif", whiteSpace = "normal", wordWrap = "break-word")
            ),
            semantic_tag = colDef(
              name = "Semantic Tag",
              width = 130,
              style = list(fontFamily = "sans-serif", fontSize = "1em")
            ),
            usage = colDef(
              name = "Usage",
              format = colFormat(separators = TRUE),
              align = "right",
              width = 130,
              style = list(fontFamily = "sans-serif")
            ),
            selected = colDef(
              name = "Select",
              cell = function(value, index) {
                htmltools::tags$input(
                  type = "checkbox",
                  class = "code-checkbox",
                  `data-condition` = current_condition,
                  `data-search-term` = current_search_term,
                  `data-code` = code_data$snomed_code[index],
                  `data-description` = code_data$description[index],
                  `data-semantic-tag` = code_data$semantic_tag[index],
                  `data-usage` = code_data$usage[index],
                  onchange = paste0("updateCheckboxState('", table_id, "', this)")
                )
              },
              width = 70,
              align = "center"
            )
          ),
          outlined = TRUE,
          pagination = FALSE
        )
      )
    },
    searchable = TRUE,
    pagination = FALSE,
    style = list(fontFamily = "sans-serif")
  )

  htmltools::tagList(
    htmltools::tags$button(
      id = paste0("download_", table_id),
      class = "btn btn-primary",
      style = "margin-bottom: 10px;",
      onclick = paste0("downloadSelectedCodes('", table_id, "')"),
      "Download selected codes as CSV"
    ),
    table_widget,
    htmltools::tags$script(htmltools::HTML(paste0(
      "
      // Global object to store checkbox states
      if (typeof window.checkboxStates === 'undefined') {
        window.checkboxStates = {};
      }
      if (typeof window.checkboxStates['", table_id, "'] === 'undefined') {
        window.checkboxStates['", table_id, "'] = {};
      }

      function updateCheckboxState(tableId, checkbox) {
        const code = checkbox.dataset.code;
        window.checkboxStates[tableId][code] = checkbox.checked;
      }

      function restoreCheckboxStates(tableId) {
        const checkboxes = document.querySelectorAll('#' + tableId + ' .code-checkbox');
        checkboxes.forEach(cb => {
          const code = cb.dataset.code;
          if (window.checkboxStates[tableId][code]) {
            cb.checked = true;
          }
        });
      }

      // Observer to restore states when detail rows are expanded
      const observer", table_id, " = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
            setTimeout(() => restoreCheckboxStates('", table_id, "'), 10);
          }
        });
      });

      // Start observing when DOM is ready
      document.addEventListener('DOMContentLoaded', function() {
        const table = document.getElementById('", table_id, "');
        if (table) {
          observer", table_id, ".observe(table, { childList: true, subtree: true });
        }
      });

      // If DOM is already loaded, start observing immediately
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', function() {
          const table = document.getElementById('", table_id, "');
          if (table) {
            observer", table_id, ".observe(table, { childList: true, subtree: true });
          }
        });
      } else {
        const table = document.getElementById('", table_id, "');
        if (table) {
          observer", table_id, ".observe(table, { childList: true, subtree: true });
        }
      }
      
      function downloadSelectedCodes(tableId) {
        const checkboxes = document.querySelectorAll('#' + tableId + ' .code-checkbox:checked');
        const selected = Array.from(checkboxes).map(cb => ({
          condition: cb.dataset.condition,
          search_term: cb.dataset.searchTerm,
          snomed_code: cb.dataset.code,
          description: cb.dataset.description,
          semantic_tag: cb.dataset.semanticTag,
          usage: cb.dataset.usage,
          selected: true
        }));
        
        if (selected.length === 0) {
          alert('Please select at least one code');
          return;
        }
        
        const headers = ['condition', 'search_term', 'snomed_code', 'description', 'semantic_tag', 'usage', 'selected'];
        const csvContent = [
          headers.join(','),
          ...selected.map(row => 
            headers.map(h => '\"' + String(row[h]).replace(/\"/g, '\"\"') + '\"').join(',')
          )
        ].join('\\n');
        
        const blob = new Blob([csvContent], { type: 'text/csv' });
        const url = window.URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'selected_snomed_codes.csv';
        a.click();
        window.URL.revokeObjectURL(url);
      }
    ")))
  )
}

# Code here 
search_terms <- list(
  gad = c(
    "generalised anxiety",
    "generalized anxiety",
    "GAD",
    "anxiety disorder",
    "persistent anxiety",
    "excessive worry",
    "chronic anxiety"
  ),
  cmhc_nos = c(
    "mental disorder",
    "psychiatric disorder",
    "mental health condition",
    "psychological disorder",
    "behavioural disorder",
    "emotional disorder",
    "unspecified mental"
  ),
  ocd = c(
    "obsessive compulsive",
    "OCD",
    "obsession",
    "compulsion",
    "repetitive behaviour",
    "intrusive thought",
    "ritualistic behaviour"
  ),
  dep = c(
    "depression",
    "depressive episode",
    "major depression",
    "unipolar depression",
    "mood disorder",
    "depressive disorder",
    "melancholia",
    "dysthymia"
  ),
  pd = c(
    "panic disorder",
    "panic attack",
    "agoraphobia",
    "acute anxiety",
    "episodic anxiety"
  ),
  phobia = c(
    "phobia",
    "phobic disorder",
    "specific phobia",
    "social phobia",
    "agoraphobia",
    "fear disorder",
    "anxiety phobia"
  ),
  ptsd = c(
    "posttraumatic stress",
    "post-traumatic stress",
    "PTSD",
    "trauma disorder",
    "stress reaction",
    "acute stress",
    "combat stress",
    "post-trauma"
  )
)

# Create smaller dataframe with unique SNOMED CT code/description mapping
snomed_code_desc <- snomed_usage |> 
  select(end_date, snomed_code, description) |> 
  group_by(snomed_code) |> 
  arrange(desc(end_date), .by_group = TRUE) |> 
  mutate(description = first(description)) |> 
  ungroup() |> 
  arrange(desc(end_date), snomed_code) |> 
  select(-end_date) |> 
  distinct()

matching_codes_tibble <- find_matching_codes(snomed_code_desc, search_terms)

# matching_codes_tibble |> 
#   mutate(
#     semantic_tag = extract_semantic_tag(description),
#     description = strip_semantic_tag(description)
#   )

extract_codes_by_category <- function(matching_codes_tibble) {
  matching_codes_tibble |>
    group_by(search_category) |>
    summarise(codes = list(snomed_code)) |>
    deframe()
}

codes_by_category <- extract_codes_by_category(matching_codes_tibble)
# codes_by_category


add_category_flags <- function(data, codes_by_category) {
  existing_cols <- colnames(data)
  new_cols <- names(codes_by_category)
  conflicts <- intersect(existing_cols, new_cols)
  
  if (length(conflicts) > 0) {
    stop("Column names already exist in data: ", 
         str_c(conflicts, collapse = ", "))
  }

  df_with_category_flags <- data |>
    bind_cols(
      map_dfc(codes_by_category, ~data$snomed_code %in% .x) |>
        set_names(names(codes_by_category))
    )
  
  df_with_category_flags |>
    filter(if_any(all_of(new_cols))) |> 
    arrange(across(all_of(new_cols), desc), snomed_code, desc(start_date))
}

# snomed_usage |> 
#   add_category_flags(codes_by_category)
  

# search_summary_df <- create_search_summary_df(matching_codes_tibble, snomed_usage)
# search_summary_df <- search_summary_df |> 
#   mutate(search_category = recode(search_category, !!!condition_names))

# search_summary_table <- create_gt_table(search_summary_df)
# search_summary_table

processed_matching_codes <- process_matching_codes(
  matching_codes_tibble,
  snomed_usage,
  condition_names,
  exclude_semantic_tags = c(
    "observable entity",
    "assessment scale",
    "regime/therapy",
    "procedure",
    "qualifier value",
    "physical object"
  )
)

interactive_table <- create_interactive_table_with_export(
  processed_matching_codes
)

interactive_table
