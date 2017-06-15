# clean R workspace beforehand
rm(list = ls())

# load libraries
library(ggplot2)

# create empty dataframe
df <- data.frame(people = rep(0, 367), expected_prob = rep(0, 367))

# compute probabilities
for(people in 1:367) {
  n_pairs       <- choose(people, 2)
  expected_prob <- 1 - (364/365)**n_pairs
  
  # save the result in a dataframe
  df[people, ]  <- c(people, expected_prob)
}

# visualisation
ggplot(df[15:30, ], aes(x = people, y = expected_prob)) +
  geom_line() +
  geom_text(aes(label = sprintf("%1.0f%%", expected_prob*100)),
            vjust = -0.2,
            hjust = 0.8) +
  theme_light()


