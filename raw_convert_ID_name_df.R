data <- readRDS("data/best.teams.mens.rds")
d2 <- readRDS("data/best.teams.womens.rds")$interestedteam
key <- readRDS("data/name_ID_key.rds")
d1 <- data$interestedteam

paste(get_names_for_IDs(lapply(strsplit(as.character(d1$VT[1]), ","), function(x) gsub("\\s+", "", x))[[1]], key), collapse = ", ")

men_app <- c('VT', 'FX', 'HB', 'PB', 'PH', 'SR')
women_app <- c('VT', 'BB', 'UB', 'FX')

for (app in men_app) {
  d1[[app]] <- sapply(lapply(d1$app, function(entry) {
    names_vector <- lapply(strsplit(entry, ","), function(x) gsub("\\s+", "", x))[[1]]
    get_names_for_IDs(names_vector, key)
  }), function(x) paste(x, collapse = ", "))
}

