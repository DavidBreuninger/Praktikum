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

# vectors of distinct Raumbezug 
Mobilitaet <- read.csv("Data/indikat2510_bevoelkerung_mobilitaetsziffer_28_10_25.csv")
Bezirke <- Mobilitaet %>%
  select(Raumbezug) %>% 
  distinct() %>% 
  filter(Raumbezug != "Stadt München") %>%
  pull()

# FUNC3: matrix_to_long, pivots data table to a long format for work with ggplot2, adds a column with one year
matrix_to_long <- function(df, year) {
  df <- df %>%
    filter(Anfangsbezirk_Nr %in% 1:25)
  df[2:25] <- as.data.frame(sapply(df[2:25], as.numeric))
  df <- df %>% 
    select(all_of(c("Anfangsbezirk_Nr", Bezirke))) %>% 
    mutate(Jahr = year) %>%
    cbind(Anfangsbezirk = Bezirke) %>%
    pivot_longer(
      cols = starts_with(c("0", "1", "2")),
      names_to = "Umzugsbezirk",
      values_to = "Anzahl") 
  return(df)
}