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
  
  df <- df[1:26] %>% 
    cbind(Anfangsbezirk = Bezirke) %>%
    mutate(innerstaedtisch = rowSums(df[2:26])) %>%
    left_join(außerstadt) %>%
    mutate(insgesamt = innerstaedtisch + außerstaedtisch)
  return(df)
}
