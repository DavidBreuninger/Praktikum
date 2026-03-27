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

# vectors of distinct Raumbezug, district numbers removed
Mobilitaet_thin <- read.csv("Clean_Data/Mobilitaet_thin.csv")
Bezirke <- Mobilitaet_thin %>%
  select(Raumbezug) %>% 
  distinct() %>% 
  filter(Raumbezug != "Stadt München") %>%
  pull() %>%
  str_remove_all("^[0-9]{2} ")%>%
  str_replace("Thalkirchen - Obersendling - Forstenried - Fürstenried - Solln", "Thalkirchen") %>%
  str_replace("Aubing - Lochhausen - Langwied", "Aubing")

# vector for setting column names
jahrbuch_spaltennamen <- c("Anfangsbezirk_Nr", Bezirke)
spaltennamen_ID <- as.character(c("Anfangsbezirk_Nr",1:25))

# get data frame with number of people, who moved outside of Munich
wegzug_außerstadt <- Mobilitaet_thin %>%
  filter(Ausprägung == "insgesamt") %>%
  select(Raumbezug, Jahr, außerstädtisch.Weggezogene.) %>% 
  distinct() %>% 
  filter(Raumbezug != "Stadt München") %>%
  rename(außerstaedtisch = außerstädtisch.Weggezogene.)

# FUNC3: colnames_to_ID, sets the colnames to district IDs, filters relevant rows
colnames_to_ID <- function(df) {
  df <- df %>%
    select(colnames(.)[1:26]) %>%
    setnames(new = spaltennamen_ID) %>%
    filter(Anfangsbezirk_Nr %in% 1:25)
  return(df)
}

#FUNC4: add_umzug_info, adds columns Jahr (passed as Variable year), 
#       innerstaedtisch, außerstaedtisch, insgesamt und Bezirke 
add_umzug_info <- function(df, year) {
  df[2:26] <- as.data.frame(sapply(df[2:26], as.numeric)) # turn district columns into numeric
  
  außerstadt <- wegzug_außerstadt %>%
    filter(Jahr == year) %>%
    mutate(Anfangsbezirk = Bezirke) %>%
    select(!Raumbezug)
  
  df <- df[,1:26] %>% 
    cbind(Anfangsbezirk = Bezirke) %>%
    mutate(innerstaedtisch = rowSums(df[,2:26])) %>%
    left_join(außerstadt) %>%
    mutate(insgesamt = innerstaedtisch + außerstaedtisch)
  return(df)
}

#FUNC5: get_district_sum, origin: numeric value, district of origin 
#                         moved_to: numeric vector, the districts that people moved to from the orignal district
get_district_sum <- function(df, origin, moved_to) {
  res <- df[,2:26]
  res <- res[origin, moved_to] %>%
         sum()
  return(res)
}
#FUNC6: sum_im_bezirk, creates column "selber Bezirk", which holds the number of people, 
#       that stayed in the sam district after moving
sum_im_bezirk <- function(df) {
  df <- df %>%
    mutate(selber_Bezirk = case_when(
      Anfangsbezirk_Nr == 1 ~ get_district_sum(df, 1, 1),
      Anfangsbezirk_Nr == 2 ~ get_district_sum(df, 2, 2),
      Anfangsbezirk_Nr == 3 ~ get_district_sum(df, 3, 3),
      Anfangsbezirk_Nr == 4 ~ get_district_sum(df, 4, 4),
      Anfangsbezirk_Nr == 5 ~ get_district_sum(df, 5, 5),
      Anfangsbezirk_Nr == 6 ~ get_district_sum(df, 6, 6),
      Anfangsbezirk_Nr == 7 ~ get_district_sum(df, 7, 7),
      Anfangsbezirk_Nr == 8 ~ get_district_sum(df, 8, 8),
      Anfangsbezirk_Nr == 9 ~ get_district_sum(df, 9, 9),
      Anfangsbezirk_Nr == 10 ~ get_district_sum(df, 10, 10),
      Anfangsbezirk_Nr == 11 ~ get_district_sum(df, 11, 11),
      Anfangsbezirk_Nr == 12 ~ get_district_sum(df, 12, 12),
      Anfangsbezirk_Nr == 13 ~ get_district_sum(df, 13, 13),
      Anfangsbezirk_Nr == 14 ~ get_district_sum(df, 14, 14),
      Anfangsbezirk_Nr == 15 ~ get_district_sum(df, 15, 15),
      Anfangsbezirk_Nr == 16 ~ get_district_sum(df, 16, 16),
      Anfangsbezirk_Nr == 17 ~ get_district_sum(df, 17, 17),
      Anfangsbezirk_Nr == 18 ~ get_district_sum(df, 18, 18),
      Anfangsbezirk_Nr == 19 ~ get_district_sum(df, 19, 19),
      Anfangsbezirk_Nr == 20 ~ get_district_sum(df, 20, 20),
      Anfangsbezirk_Nr == 21 ~ get_district_sum(df, 21, 21),
      Anfangsbezirk_Nr == 22 ~ get_district_sum(df, 22, 22),
      Anfangsbezirk_Nr == 23 ~ get_district_sum(df, 23, 23),
      Anfangsbezirk_Nr == 24 ~ get_district_sum(df, 24, 24),
      Anfangsbezirk_Nr == 25 ~ get_district_sum(df, 25, 25)))
    
    return(df)
}

#FUNC7: sum_Nachbarbezirke, creates column with sum of people, that moved to neigbouring districts
sum_Nachbarbezirke <- function(df) {
  df <- df %>%
  mutate(Nachbarbezirke = case_when(
    Anfangsbezirk_Nr == 1 ~ get_district_sum(df, 1, c(2, 3, 5, 13, 12)),
    Anfangsbezirk_Nr == 2 ~ get_district_sum(df, 2, c(1, 3, 8, 6, 18)),
    Anfangsbezirk_Nr == 3 ~ get_district_sum(df, 3, c(1, 2, 4, 8, 9)),
    Anfangsbezirk_Nr == 4 ~ get_district_sum(df, 4, c(3, 9, 11, 12)),
    Anfangsbezirk_Nr == 5 ~ get_district_sum(df, 5, c(1, 2, 13, 14, 17, 18)),
    Anfangsbezirk_Nr == 6 ~ get_district_sum(df, 6, c(2, 7, 8, 18, 19)),
    Anfangsbezirk_Nr == 7 ~ get_district_sum(df, 7, c(6, 8, 19, 20, 25)),
    Anfangsbezirk_Nr == 8 ~ get_district_sum(df, 8, c(2, 3, 6, 7, 9, 25)),
    Anfangsbezirk_Nr == 9 ~ get_district_sum(df, 9, c(3, 4, 8, 10, 11, 21, 25)),
    Anfangsbezirk_Nr == 10 ~ get_district_sum(df, 10, c(9, 11, 21, 23, 24)),
    Anfangsbezirk_Nr == 11 ~ get_district_sum(df, 11, c(4, 9, 10, 12, 24)),
    Anfangsbezirk_Nr == 12 ~ get_district_sum(df, 12, c(1, 3, 4, 11, 13)),
    Anfangsbezirk_Nr == 13 ~ get_district_sum(df, 13, c(1, 5, 12, 14, 15)),
    Anfangsbezirk_Nr == 14 ~ get_district_sum(df, 14, c(5, 13, 15, 16)),
    Anfangsbezirk_Nr == 15 ~ get_district_sum(df, 15, c(13, 14, 16)),
    Anfangsbezirk_Nr == 16 ~ get_district_sum(df, 16, c(5, 14, 15, 17)),
    Anfangsbezirk_Nr == 17 ~ get_district_sum(df, 17, c(5, 16, 18)),
    Anfangsbezirk_Nr == 18 ~ get_district_sum(df, 18, c(2, 5, 6, 17, 19)),
    Anfangsbezirk_Nr == 19 ~ get_district_sum(df, 19, c(6, 7, 18, 20)),
    Anfangsbezirk_Nr == 20 ~ get_district_sum(df, 20, c(7, 19, 21, 25)),
    Anfangsbezirk_Nr == 21 ~ get_district_sum(df, 21, c(9, 20, 22, 23, 25)),
    Anfangsbezirk_Nr == 22 ~ get_district_sum(df, 22, c(21, 23)),
    Anfangsbezirk_Nr == 23 ~ get_district_sum(df, 23, c(10, 21, 22, 24)),
    Anfangsbezirk_Nr == 24 ~ get_district_sum(df, 24, c(10, 11, 23)),
    Anfangsbezirk_Nr == 25 ~ get_district_sum(df, 25, c(7, 8, 9, 20, 21)))) 
  
  return(df)
}

#FUNC8: sum_Restbezirke, creates column with sum of people, 
#       that moved to districts not neighbouring their old one
sum_Restbezirke <- function(df) {
  ID <- c(1:25)
  df <- df %>%
    mutate(Restbezirke = case_when(
      Anfangsbezirk_Nr == 1 ~ get_district_sum(df, 1, ID[-c(1, 2, 3, 5, 13, 12)]),
      Anfangsbezirk_Nr == 2 ~ get_district_sum(df, 2, ID[-c(1, 2, 3, 8, 6, 18)]),
      Anfangsbezirk_Nr == 3 ~ get_district_sum(df, 3, ID[-c(1, 2, 3, 4, 8, 9)]),
      Anfangsbezirk_Nr == 4 ~ get_district_sum(df, 4, ID[-c(3, 4, 9, 11, 12)]),
      Anfangsbezirk_Nr == 5 ~ get_district_sum(df, 5, ID[-c(1, 2, 5, 13, 14, 17, 18)]),
      Anfangsbezirk_Nr == 6 ~ get_district_sum(df, 6, ID[-c(2, 6, 7, 8, 18, 19)]),
      Anfangsbezirk_Nr == 7 ~ get_district_sum(df, 7, ID[-c(6, 7, 8, 19, 20, 25)]),
      Anfangsbezirk_Nr == 8 ~ get_district_sum(df, 8, ID[-c(2, 3, 6, 7, 8, 9, 25)]),
      Anfangsbezirk_Nr == 9 ~ get_district_sum(df, 9, ID[-c(3, 4, 8, 9, 10, 11, 21, 25)]),
      Anfangsbezirk_Nr == 10 ~ get_district_sum(df, 10, ID[-c(9, 10, 11, 21, 23, 24)]),
      Anfangsbezirk_Nr == 11 ~ get_district_sum(df, 11, ID[-c(4, 9, 10, 11, 12, 24)]),
      Anfangsbezirk_Nr == 12 ~ get_district_sum(df, 12, ID[-c(1, 3, 4, 11, 12, 13)]),
      Anfangsbezirk_Nr == 13 ~ get_district_sum(df, 13, ID[-c(1, 5, 12, 13, 14, 15)]),
      Anfangsbezirk_Nr == 14 ~ get_district_sum(df, 14, ID[-c(5, 13, 14, 15, 16)]),
      Anfangsbezirk_Nr == 15 ~ get_district_sum(df, 15, ID[-c(13, 14, 15, 16)]),
      Anfangsbezirk_Nr == 16 ~ get_district_sum(df, 16, ID[-c(5, 14, 15, 16, 17)]),
      Anfangsbezirk_Nr == 17 ~ get_district_sum(df, 17, ID[-c(5, 16, 17, 18)]),
      Anfangsbezirk_Nr == 18 ~ get_district_sum(df, 18, ID[-c(2, 5, 6, 17, 18, 19)]),
      Anfangsbezirk_Nr == 19 ~ get_district_sum(df, 19, ID[-c(6, 7, 18, 19, 20)]),
      Anfangsbezirk_Nr == 20 ~ get_district_sum(df, 20, ID[-c(7, 19, 20, 21, 25)]),
      Anfangsbezirk_Nr == 21 ~ get_district_sum(df, 21, ID[-c(9, 20, 21, 22, 23, 25)]),
      Anfangsbezirk_Nr == 22 ~ get_district_sum(df, 22, ID[-c(21, 22, 23)]),
      Anfangsbezirk_Nr == 23 ~ get_district_sum(df, 23, ID[-c(10, 21, 22, 23, 24)]),
      Anfangsbezirk_Nr == 24 ~ get_district_sum(df, 24, ID[-c(10, 11, 23, 24)]),
      Anfangsbezirk_Nr == 25 ~ get_district_sum(df, 25, ID[-c(7, 8, 9, 20, 21, 25)]))) 
  
  return(df)
}