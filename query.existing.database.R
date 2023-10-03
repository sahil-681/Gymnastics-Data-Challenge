weights.modification <- function(data, gold, silver, bronze) {
  weights <- c(gold, silver, bronze)
  data$customscore <- apply(data[, c("Gold", "Silver", "Bronze")], 1, 
                            function(row) sum(row * weights))
  data <- data[order(data$customscore, decreasing = TRUE), ]
  
  return(data)
}

exclude.people <- function(data, exc) {
  for (i in 1:length(exc)) {
    data <-  data %>% filter((P1 != exc[i]) & (P2 != exc[i])  & (P3 != exc[i])  
                    & (P4 != exc[i])  & (P5 != exc[i]))
  }
  return(data)
}

include.people <- function(data, inc) {
  for (i in 1:length(inc)) {
    data <-  data %>% filter((P1 == inc[i]) | (P2 == inc[i])  | (P3 == inc[i])  
                             | (P4 == inc[i])  | (P5 == inc[i]))
  }
  return(data)
}
