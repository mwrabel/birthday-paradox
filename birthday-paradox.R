# clean R workspace beforehand
rm(list = ls())

# load libraries
library(ggplot2)
library(reshape2)
library(Rmisc)

# create empty dataframe
yr_days <- 365
df <- data.frame(people = rep(0, yr_days), expected_prob = rep(0, yr_days))

# compute probabilities
for(people in 1:yr_days) {
  n_pairs       <- choose(people, 2)
  expected_prob <- 1 - ((yr_days - 1)/yr_days)**n_pairs
  
  # save the result in a dataframe
  df[people, ]  <- c(people, expected_prob)
}

# visualisation
ggplot(df[1:60, ], aes(x = people, y = expected_prob)) +
  geom_line() +
  geom_text(aes(label = sprintf("%1.0f%%", expected_prob*100)),
            vjust = -0.4,
            hjust = 0) +
  labs(title = "teoretical probability", x = "people") +
  theme_minimal()

ggplot(df[15:30, ], aes(x = people, y = expected_prob)) +
  geom_line() +
  geom_text(aes(label = sprintf("%1.0f%%", expected_prob*100)),
            vjust = -0.2,
            hjust = 0.8) +
  labs(title = "teoretical probability - zoomed", x = "people") +
  theme_minimal()

# simulation
p <- data.frame()
i_max <- 1000
j_max <- 60

for (j in 1:j_max) {

  for (i in 1:i_max) {
    
    v <- floor(runif(j, min = 1, max = yr_days + 1))
    p[i, j] <- ifelse(length(v) - length(unique(v)) > 0, 1, 0)
    
  }
  
}
sapply(p, mean)
colnames(p) <- 1:60

# simulation - summary
ci = 0.999
p_summary <- summarySE(melt(p), measurevar = "value", groupvars = c("variable"), conf.interval = ci)

ggplot(p_summary, aes(x = variable, y = value)) +
  geom_line(group = 1) +
  geom_text(aes(label = sprintf("%1.0f%%", value * 100)),
            vjust = -0.2,
            hjust = 0.8) +
  labs(title = "empiric probability", x = "people") +
  theme_minimal()

ggplot(p_summary, aes(x = variable, y = value)) +
  geom_crossbar(aes(ymin = value - ci, ymax = value + ci), width = 0.75, fatten = 2.5) +
  geom_line(group = 1) +
  labs(title = paste0("empiric probability with ", sprintf("%1.1f%%", ci * 100), " confidence intervals"), x = "people") +
  theme_minimal()
