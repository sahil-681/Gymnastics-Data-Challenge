# Preparing d1

# Cleaning dates
# Define a mapping for months
months_mapping <- c("JUN" = "Jun", "JUL" = "Jul", "AUG" = "Aug", "Aug" = "Aug",
                    "July" = "Jul")

# Function to process the Date column for d1
process_date_column <- function(date_str) {
  # Split the date string by space
  parts <- strsplit(date_str, " ")[[1]]
  
  # Check if the first part is a valid day abbreviation, if so, remove it
  if (parts[1] %in% c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT")) {
    parts <- parts[-1]
  }
  
  # Convert the month part to its abbreviated form
  month <- parts[2]
  parts[2] <- months_mapping[month]
  
  # Combine the parts back into a string
  result <- paste(parts, collapse = " ")
  return(result)
}

# Fixing dates and removing duplicates
d1 <- d1 %>%
  mutate(Date = sapply(Date, process_date_column)) %>%
  distinct()

# Preparing d2

# Cleaning dates

# Only taking the start date as the Date column since the other dataframe 
# doesnt have an end date 

d2 <- d2 %>%
  mutate(Date = str_replace_all(Date, "[-,]", " ")) %>%
  mutate(Date = str_trim(Date)) %>%
  mutate(word_count = str_count(Date, "\\s+") + 1) %>%
  mutate(Date = case_when(
    word_count == 6 ~ str_replace(Date, "\\s+[^\\s]+\\s+[^\\s]+\\s+[^\\s]+$", ""), # "D1 M1 Y1 D2 M2 Y2" format, deleting D2 M2 Y2
    word_count == 4 ~ str_replace(Date, "\\s+[^\\s]+", ""), # "D1 D2 M Y" format, deleting D2
    TRUE ~ Date
  )) %>%
  select(-word_count)

# Special treatment for "D1 M1 D2 M2 Y" format, deleting D2 M2
for (i in (1:length(d2$Date))) {
  word_count = str_count(d2$Date[i], "\\s+") + 1
  d2$Date[i] = ifelse(word_count == 5, paste(strsplit(d2$Date[i], " ")[[1]][c(1,2,5)], collapse = " "), d2$Date[i])
}

# Combining dataframes
data <- as.data.frame(rbind(d1, d2))
data = data %>%
  mutate(LastName  = tolower(LastName),
         FirstName = tolower(FirstName),

         #if `FirstName` is empty then if `LastName` has two words, use 2nd word for `FirstName`
         FirstName = ifelse(
           FirstName == "" & grepl(" ", LastName),
           word(LastName, 1),
           FirstName),
         LastName = ifelse(
           FirstName == "" & grepl(" ", LastName),
           word(LastName, 2),
           LastName),
         
         #if `LastName` is empty then if `FirstName` has two words, use 2nd word for `LastName`
         FirstName = ifelse(
           LastName == "" & grepl("\\s", FirstName),  # Check if LastName is empty and FirstName has 2 words
           word(FirstName, 1),
           FirstName),
         LastName = ifelse(
           LastName == "" & grepl("\\s", FirstName),  # Check if LastName is empty and FirstName has 2 words
           word(FirstName, 2),
           LastName),
         
         Country   = ifelse(LastName == "akhmejanov" & FirstName == "emil", "KAZ", Country),
         FirstName = str_replace_all(FirstName, "-", " "), #replacing hyphens with space
         LastName  = str_replace_all(LastName, "-", " "),
         FirstName = str_replace_all(FirstName, "'", " "), #replacing apostrophe with space
         LastName  = str_replace_all(LastName, "'", " "),
         FirstName = ifelse(str_detect(LastName, "^elpitiya"), #FirsName starts with Elpitiya then LastName will be Badalge Dona
                            "badalge dona", FirstName), #there were 3-4 variations for Elpitiya Badalge Dona, so making all in one
         FirstName = str_replace_all(FirstName, "é", "e"), #replacing accents
         LastName  = str_replace_all(LastName, "é", "e"), 
         FirstName = str_replace_all(FirstName, "ö", "o"),
         LastName  = str_replace_all(LastName, "ö", "o"), 
         FirstName = str_replace_all(FirstName, "ä", "a"),
         LastName  = str_replace_all(LastName, "ä", "a"), 
         FirstName = ifelse(LastName == "frick" & Country == "AUS", "aiden", FirstName),
         FirstName = ifelse(LastName == "gawronski" & Country == "POL", "sebastian", FirstName),
         FirstName = ifelse(LastName == "gudmundsdottir" & Country == "ISL", "hildur", FirstName),
         FirstName = ifelse(LastName == "gudmundsdottir hm" & Country == "ISL", "hildur", FirstName),
         FirstName = ifelse(LastName == "gundogdu" & Country == "TUR", "yunus", FirstName),
         FirstName = ifelse(LastName == "habisreutinger" & Country == "SUI", "lilli", FirstName),
         FirstName = ifelse(LastName == "hatoguan" & Country == "INA", "joseph", FirstName), #checked till 4489
         LastName  = ifelse(FirstName == "hillary" & Country == "PAN", "heron", LastName),
         LastName  = ifelse(FirstName == "lana" & Country == "PAN", "herrera", LastName),
         LastName  = ifelse(FirstName == "vinzenz" & Country == "AUT", "hock", LastName),
         FirstName = ifelse(LastName == "hock" & Country == "AUT", "vinzenz", FirstName),
         FirstName = ifelse(LastName == "sant" & Country == "FRA", "melanie", FirstName),
         LastName  = ifelse(FirstName == "melanie" & Country == "FRA", "jesus santos", LastName),
         FirstName = ifelse(LastName == "aas" & Country == "NOR", "fredrick", FirstName),
         LastName  = ifelse(FirstName == "pau" & Country == "ESP" & LastName == "jimenez i fernandez", 
                            "jimenez", LastName),
         LastName  = ifelse(FirstName == "nicolau" & Country == "ESP" & LastName == "mir rossello", 
                            "mir", LastName),
         LastName  = ifelse(FirstName == "adria" & Country == "ESP" & LastName == "vera mora", 
                            "vera", LastName),
         LastName  = ifelse(FirstName == "oriol" & Country == "ESP" & LastName == "rifa pedreno", 
                            "rifa", LastName),
         LastName  = ifelse(FirstName == "jan" & Country == "ESP" & LastName == "carreres macia", 
                            "carreres", LastName),
         LastName  = ifelse(FirstName == "dietmar v." & Country == "ESP" & LastName == "reinhardt codina", 
                            "reinhardt", LastName),
         FirstName = ifelse(LastName == "reinhardt" & Country == "ESP" & FirstName == "dietmar v.", 
                            "dietmar", FirstName),
         FirstName = ifelse(LastName == "zakutney" & Country == "CAN" & FirstName == "samuel", 
                             "sam", FirstName),
         FirstName = ifelse(LastName == "stickler" & FirstName == "poppy grace", 
                             "poppy", FirstName),
         Country   = ifelse(LastName == "stickler" & FirstName == "poppy", 
                          "GBR", Country)
  ) %>% 
  arrange(LastName) %>%
  mutate(Apparatus = case_when(
            Apparatus == "VT1"  ~ "VT",
            Apparatus == "VT2"  ~ "VT",
            Apparatus == "VT_1" ~ "VT",
            Apparatus == "VT_2" ~ "VT",
            Apparatus == "hb"   ~ "HB",
            Apparatus == "UE"   ~ "UB",
            TRUE ~ Apparatus),
         Country = case_when(
            Country == "GE1" ~ "GER",
            Country == "GE2" ~ "GER",
            TRUE ~ Country),
         ID = paste(LastName, FirstName, sep = "_"),
         ID = gsub(" ", "_", ID),
         Date = as.Date(Date, format = "%d %b %Y"),
         Country = ifelse(ID == "achampong_ondine", 
                                      "GBR", Country),
         Gender = ifelse(ID == "godwin_georgia", 
                          "w", Gender),
         ID = ifelse(ID == "de_jesus" & Country == "FRA", "jesus_santos_melanie", ID),
         Country = ifelse(ID == "chasazyrovas_gytis" & Gender == "m", "LTU", Country),
         Country = ifelse(ID == "schmidt_casimir" & Gender == "m", "NED", Country),
         Gender = ifelse(ID == "godwin_georgia" & Country == "AUS", "w", Gender),
         Country = ifelse(Country == "ENG", "GBR", Country),
         Country = ifelse(Country == "SCO", "GBR", Country),
         Country = ifelse(ID == "aleksandrov_yordan" & Gender == "m", "BUL", Country),
         ID = ifelse(ID == "richard_frederick_nathaniel" & Country == "USA", "richard_frederick", ID),
         ID = ifelse(ID == "skirkey_ian_hunter" & Country == "USA", "skirkey_ian", ID),
         ID = ifelse(ID == "callum_mc_grace" & Country == "USA", "mc_callum", ID)
  )

data <- data[!(data$ID == "whitehead_emily" & data$Gender == "m"), ]
data <- data[!(data$ID == "achampong_ondine" & data$Gender == "m"), ]


# output data (though fit.model.R doesn't reread data, but instead just uses the data object)
write.csv(data, "data/cleaned_combined_data.csv", row.names = F)
