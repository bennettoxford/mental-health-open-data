# Function to extract the semantic tag from SNOMED CT descriptions
# The input data need to follow a specific data structure, e.g.,:
# start_date end_date   snomed_code      description                                                         usage active_at_start active_at_end
# <date>     <date>     <chr>            <chr>                                                               <int> <lgl>           <lgl>        
# 2023-08-01 2024-07-31 720433000        Patient Health Questionnaire Nine Item score (observable entity)  1669350 TRUE            TRUE         
# 2023-08-01 2024-07-31 836541000000100  Patient health questionnaire 2 score (observable entity)           566790 TRUE            TRUE         
# 2023-08-01 2024-07-31 758711000000105  Patient health questionnaire 9 (assessment scale)                   18060 TRUE            TRUE         

get_semantic_tag <- function(data, add_short_description = TRUE) {
  data_dem_tag <- data |> 
    mutate(
      semantic_tag = str_extract(description, "\\(([^()]+)\\)$"),
      semantic_tag = str_replace_all(semantic_tag, "[()]", "")
    )
  
  # Add short description (without the semantic tag) by default
  if (add_short_description) {
    data_dem_tag <- data_dem_tag |> 
      mutate(
        description_short = str_remove(description, " \\([^()]+\\)$")
      )
  }

  data_dem_tag

}