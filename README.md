Birthday Paradox in R
================
Michał Wrąbel

Did you know that it takes only 24 people to be over 50% sure that there's a lucky pair who share the same day of birth? That's what birthday paradox is all about. The more counterintuitive it feels, the more astounding is the fact that actually it is easily provable.

Here I'd like to show you, my Dearest Reader, R implementation of birthday paradox. Enjoy!

Step 1: Prepare R environment
-----------------------------

``` r
# clean R workspace beforehand
rm(list = ls())

# load libraries
library(ggplot2)
library(reshape2)
library(Rmisc)
```

    ## Loading required package: lattice

    ## Loading required package: plyr

For the sake of simplicity, I'll introduce assumption that each year consists of 365 days. Secondly, I could safely assume that distribution of birthdays across all days is uniform (that's not true, but very close to true).

In fact those assumptions won't heavily influence final results. In additional read there's an article in which author lifts those two assumptions - I recommend it wholeheartedly for further read on the topic!

Step 2: Create dataframe
------------------------

``` r
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

# first few probabilities (starting from 1 person in a room with probability equal 0.00)
head(df)
```

    ##   people expected_prob
    ## 1      1   0.000000000
    ## 2      2   0.002739726
    ## 3      3   0.008196680
    ## 4      4   0.016326175
    ## 5      5   0.027061942
    ## 6      6   0.040317031

Step 3: Visualize!
------------------

``` r
# Theoretical probability of an event of having two people in the same room sharing same date of birth
ggplot(df[1:60, ], aes(x = people, y = expected_prob)) +
  geom_line(size = 1, colour = "#235372") +
  #geom_text(aes(label = sprintf("%1.0f%%", expected_prob*100)),
  #          vjust = -0.4,
  #          hjust = 0) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "theoretical probability of an event", x = "people") +
  theme_minimal()
```

![](birthday-paradox_files/figure-markdown_github/unnamed-chunk-3-1.png)

After decomposing the paradox, it does not seem that counter-intuitive, doesn't it?

However, what would have become of statistics if it were lacking confidence intervals? I would not dare not to answer this question hence I include also a small simulation.

Is the paradox visible only in 'means' and 'aggregated estimations'? Or maybe it is observable roughly every time with similar probabilities? The simulation will help find the correct answer.

Step 4: Simulation
------------------

``` r
# simple simulation function definitions
birthday_paradox_simulation <- function(iterations, people_max) {
  p <- data.frame()

  # for people_max... (to speed up the computations)
  for (j in 1:people_max) {
    # ...make many iterations 
    for (i in 1:iterations) {
      v <- floor(runif(j, min = 1, max = yr_days + 1))
      p[i, j] <- ifelse(length(v) - length(unique(v)) > 0, 1, 0)
      }
    }
sapply(p, mean)

return(p)
}

p <- birthday_paradox_simulation(iterations = 1000, people_max = 60)
```

So far so good. Simulated means are very similar to theoretical values. But what about aforementioned confidence intervals?

``` r
# simulation - summary
calculate_ci <- function(ci, people_max) {
  
  colnames(p) <- 1:people_max
  p_summary <- summarySE(melt(p), measurevar = "value", groupvars = c("variable"), conf.interval = ci)
  p_summary$variable <- as.numeric(p_summary$variable)
  
  return(p_summary)
}

p_summary <- calculate_ci(ci = 0.999, people_max = 60)
```

    ## No id variables; using all as measure variables

``` r
confidence_plot <- function(df, ci) {
  # Theoretical probability of an event of having two people in the same room sharing same date of birth with confidence intervals
  plot <- ggplot(p_summary, aes(x = variable, y = value)) +
    geom_crossbar(aes(ymin = value - ci, ymax = value + ci), width = 0.75, fatten = 2.5, colour = "#459ad1") +
    geom_line(size = 1, colour = "#235372", group = 1) +
    labs(title = paste0("empirical probability with ", sprintf("%1.1f%%", ci * 100), " confidence intervals"), x = "people", y = "expected_prob") +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal()
  
  return(plot)
}

confidence_plot(df = p_summary, ci = 0.999)
```

![](birthday-paradox_files/figure-markdown_github/unnamed-chunk-5-1.png)

The interpretation is pretty straightforward. For example, in case of 12 people in the same room, there is 17.3% (+/- 3.9%) chance that there is a lucky pair of people sharing the same day of birth. That's pretty stable, even for a paradox. How would it looked after 50 iterations? And what to expect from 5000 repeats? Take a look:

``` r
# After 5000 iterations
p <- birthday_paradox_simulation(iterations = 5000, people_max = 60)
p_summary <- calculate_ci(ci = 0.999, people_max = 60)
```

    ## No id variables; using all as measure variables

``` r
confidence_plot(df = p_summary, ci = 0.999)
```

![](birthday-paradox_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
# After 50 iterations
p <- birthday_paradox_simulation(iterations = 50, people_max = 60)
p_summary <- calculate_ci(ci = 0.999, people_max = 60)
```

    ## No id variables; using all as measure variables

``` r
confidence_plot(df = p_summary, ci = 0.999)
```

![](birthday-paradox_files/figure-markdown_github/unnamed-chunk-6-2.png)

Increasing number of iteration should lead to even narrower confidence intervals and that's precisely what happens. However, with 50 iterations confidence intervals expand substantialy.There's 99.9% chance that in case of having 50 rooms filled with 20 people each, mean number of birthday matches will lie somewhere between 0.19 and 0.69 what is quite wide.

With 10 rooms filled with 20 people each, we'd be 95% sure that the mean range would extend to 0.13-0.88.

Conclusion?
-----------

Although with the power of R birthday paradox has been proved again, there is absolute no certainty that if we gathered about 20 friends in our room, there would be a birthday match indeed. Instead, there would be a mere probability of a that event. Also, there would be a pack of friends wondering where's the cake and schnapps. And who's in charge of that party?

Additional resources
--------------------

-   <http://www.panix.com/~murphy/bday.html>
-   <https://betterexplained.com/articles/understanding-the-birthday-paradox/>
-   <http://blog.revolutionanalytics.com/2012/06/simulating-the-birthday-problem-with-data-derived-probabilities.html>
