## file for all the functions

# FUNC1: remove_NAs, A function that deletes columns or rows if they are all NAs. (copied from Example "Abgabe KoCo19 Kinder")
remove_NAs <- function(df) {
  require(dplyr)
  df <- df[rowSums(is.na(df)) < ncol(df), ] %>%
    select_if(~ !all(is.na(.)))
  return(df)
}

# FUNC2: remove_Auspraegung, removes "(deutsch)", "(nichtdeutsch)" and "(insgesamt)" from values
remove_Auspraegung <- function(df) {
  require(dplyr)
  df <- df %>% 
    mutate_all(~ gsub("\\(deutsch\\)","",.)) %>%
    mutate_all(~ gsub("\\(nichtdeutsch\\)","",.)) %>%
    mutate_all(~ gsub("\\(insgesamt\\)","",.)) %>%
    mutate_all(~ gsub("\\(auch innerhalb der räumlichen Einheit\\)","",.)) %>%
  return(df)
}

# vectors for functions read_xls_matrix and read_xlsx_matrix
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")
Bezirke <- Mobilitaet_thin %>%
  select(Raumbezug) %>% 
  distinct() %>% 
  filter(Raumbezug != "Stadt München") %>%
  pull()
jahrbuch_spaltennamen <- c("Anfangsbezirk", Bezirke, "zusammen")
jahrbuch_spaltentypen <- c("text", rep("numeric", 26))

# FUNC3: read_xls_matrix, read_xls function with set parameters, for the given data
read_xls_matrix <- function(filepath) {
  require(readxl)
  require(dplyr)
  df <- read_xls(filepath, 
                 col_names = jahrbuch_spaltennamen,
                 skip = 4,
                 col_types = jahrbuch_spaltentypen,
                 n_max = 26)
  return(df)
}

# FUNC4: read_xlsx_matrix, read_xlsx function with set parameters, for the given data
read_xlsx_matrix <- function(filepath) {
  require(readxl)
  require(dplyr)
  df <- read_xlsx(filepath, 
                 col_names = jahrbuch_spaltennamen,
                 skip = 4,
                 col_types = jahrbuch_spaltentypen,
                 n_max = 26)
  return(df)
}

# FUNC5: matrix_to_long, pivots matrices to a long format for work with ggplot2
