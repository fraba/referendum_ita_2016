setwd('/Users/francesco/public_git/referendum_ita_2016')
load('data/polls.RData')

# Average last 30 days
guessResult <- function(df, t0) {
  t0 <- as.Date(t0, origin = '1970-01-01')
  i = 0.05
  df <- subset(df, date >= t0 - 30 & date <= t0  & !is.na(sample) & !is.na(yes_wtout_undecided))
  df$precision <- 
    1/(sqrt((df$yes_wtout_undecided/100)*(1-df$yes_wtout_undecided/100)/df$sample)^2)
  df$precision_disc <- df$precision / ((1 + i)^as.numeric((t0 - df$date)))
  return(weighted.mean(df$yes_wtout_undecided, df$precision_disc))
}

expected_results <- data.frame() 

for (t0 in seq(min(polls$date)+31, max(polls$date), by = 1)) {
  print(t0)
  expected_results <- 
    rbind(expected_results, 
          data.frame(date = as.Date(t0, origin = '1970-01-01'),
                     expected_result = guessResult(polls, t0)))
}

save(expected_results, file = 'data/expected_results.RData')
