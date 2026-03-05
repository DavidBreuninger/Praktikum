## file for all the functions

# FUNC1: remove_NAs, A function that deletes columns or rows if they are all NAs. (copied from Example "Abgabe KoCo19 Kinder")
remove_NAs <- function(df) {
  require(dplyr)
  df <- df[rowSums(is.na(df)) < ncol(df), ] %>%
    select_if(~ !all(is.na(.)))
  return(df)
}