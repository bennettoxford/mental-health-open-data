# Function to extract the semantic tag from SNOMED CT descriptions
# The input data need to follow a specific data structure:
# start_date end_date   snomed_code      description                                                         usage active_at_start active_at_end
# <date>     <date>     <chr>            <chr>                                                               <int> <lgl>           <lgl>        
# 2023-08-01 2024-07-31 720433000        Patient Health Questionnaire Nine Item score (observable entity)  1669350 TRUE            TRUE         
# 2023-08-01 2024-07-31 836541000000100  Patient health questionnaire 2 score (observable entity)           566790 TRUE            TRUE         
# 2023-08-01 2024-07-31 758711000000105  Patient health questionnaire 9 (assessment scale)                   18060 TRUE            TRUE         

get_semantic_tag <- function(data, add_short_description = TRUE) {
  
  # Check that all variables that are needed to create the table are available
  required_cols <- c(
    "description"
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  data_tag <- data |> 
    mutate(
      semantic_tag = str_extract(description, "\\(([^()]+)\\)$"),
      semantic_tag = str_replace_all(semantic_tag, "[()]", "")
    )
  
  # Add short description (without the semantic tag) by default
  if (add_short_description) {
    data_tag <- data_tag |> 
      mutate(
        description_short = str_remove(description, " \\([^()]+\\)$")
      )
  }

  data_tag

}

# Function to summarise code counts grouped by semantic tag
# The input data need to follow a specific data structure:
# start_date end_date   snomed_code      description                                                                           usage active_at_start active_at_end semantic_tag      description_short                                                      
# <date>     <date>     <chr>            <chr>                                                                                 <int> <lgl>           <lgl>         <chr>             <chr>                                                                  
# 2023-08-01 2024-07-31 720433000        Patient Health Questionnaire Nine Item score (observable entity)                    1669350 TRUE            TRUE          observable entity Patient Health Questionnaire Nine Item score                           
# 2023-08-01 2024-07-31 836541000000100  Patient health questionnaire 2 score (observable entity)                             566790 TRUE            TRUE          observable entity Patient health questionnaire 2 score                                   
# 2023-08-01 2024-07-31 758711000000105  Patient health questionnaire 9 (assessment scale)                                     18060 TRUE            TRUE          assessment scale  Patient health questionnaire 9                                         
summarise_code_counts <- function(data) {

    # Check that all variables that are needed to create the table are available
  required_cols <- c(
    "start_date",
    "end_date",
    "snomed_code",
    "description_short",
    "usage",
    "semantic_tag"
  )
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  data |> 
    group_by(snomed_code, description_short, semantic_tag) |> 
    summarise(usage = sum(usage), .groups = "drop") |>
    mutate(ratio_usage = usage / sum(usage)) |>
    arrange(-usage)

}