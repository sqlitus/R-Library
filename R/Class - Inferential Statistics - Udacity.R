#### INFERENTIAL STATISTICS ####

# CLASS: https://classroom.udacity.com/courses/ud201/lessons/1234788951/concepts/1177087270923

# Lesson 1: introduction and lesson 7 review

# Key topics: mean, stdev, std error, z score, p value

# copy data from clipboard to data frame
klout.scores <- read.delim("clipboard")
names(klout.scores) <- "scores"
str(klout.scores)
summary(klout.scores)  

klout.scores$scores %>% mean()
klout.scores$scores %>% sd()

TeachingDemos::z.test(klout.scores$scores, 40, stdev = 16.04724)


#EDA - karma scores
library(ggplot2)
karma.scores <- read.delim("clipboard", header = FALSE)
str(karma.scores)
summary(karma.scores)
ggplot(karma.scores, aes(V1)) + geom_histogram()
ggplot(karma.scores, aes("karma scores", V1)) + geom_boxplot()

karma.scores$z_score_V1 <- (karma.scores$V1 - mean(karma.scores$V1)) / sd(karma.scores$V1)

karma.scores$p_value_V <- 2 * pnorm(abs(karma.scores$z_score_V1))


# how many karma points to be in top 5%? (work backwards)
1.645 * 4.8 + 13

#### Lesson 2: Estimation ####
library(tidyverse)
engagement.ratio <- read.delim("clipboard")
names(engagement.ratio)[1] <- "er"
ggplot(engagement.ratio, aes(er)) + geom_histogram()
summary(engagement.ratio)
sd(engagement.ratio$er)
sd(engagement.ratio[["er"]])

# EDA function - take in dataframe and column and return analysis
f_estimation <- function(df, col, sample.size, sample.mean){
  o1 <- summary(df[[col]])
  o2 <- sd(df[[col]])
  o3 <- ggplot(df, aes_string(col)) + geom_histogram()
  o4 <- sd(df[[col]]) / sqrt(sample.size)
  o5 <- c(sample.mean-1.96*o4, sample.mean+1.96*o4)
  o6 <- c(sample.mean-2.576*o4, sample.mean+2.576*o4)
  return(list(summary = o1, sd = o2, hist = o3, standard.error = o4, 
              conf_int_95 = o5, conf_int_99 = o6))
}
f_estimation(engagement.ratio, "er", 20, .13)




# estimate probabilities from a sample
f_estimation_sample <- function(xbar, mu, stdev, sample.size, tails = NULL){
  
  # 'standard error of the mean'
  std.err <- stdev/sqrt(sample.size)
  
  # num of standard deviations the sample {point | mean} is from mu
  z.score.sample <- (xbar - mu) / std.err
  
  # Cumulative Distribution Function: area under standard normal curve _
  # to left of z score (flip if xbar is below mu)
  cdf <- pnorm(abs(z.score.sample))
  p.value = pnorm(abs(z.score.sample), lower.tail = FALSE)
  two.sided.p.value <- 2 * pnorm(-abs(z.score.sample))
  
  conf.int.95 <- c(xbar - 1.96 * std.err, xbar + 1.96 * std.err)
  interval <- conf.int.95[2] - conf.int.95[1]
  margin.of.err <- interval / 2
  
  conf.int.99 <- c(xbar - 2.576 * std.err, xbar + 2.576 * std.err)
  interval.99 <- conf.int.99[2] - conf.int.99[1]
  margin.of.err.99 <- interval.99 / 2
  
  # if (!missing(conf.int)) point.conf.int <- qnorm((1-conf.int)/2, lower.tail = FALSE) 
  
  if (!missing(tails)){
    if (tails == 2){
      critical.region.1.96 <- "test"
    }
  }
  
  return(list(standard.error = std.err, z.score = z.score.sample, cdf = cdf, p.value = p.value, 
              two.sided.p.value = two.sided.p.value, 
              conf.int.95 = conf.int.95, interval = interval, margin.of.err = margin.of.err,
              conf.int.99 = conf.int.99, interval.99 = interval.99, margin.of.err.99 = margin.of.err.99)
         )
}

f_estimation_sample(xbar = .75,mu = .68, stdev = .1, sample.size = 25)
f_estimation_sample(.75, .68, .1, 1)
f_estimation_sample(.78, .68, .1, 1)
f_estimation_sample(175, 180, 18, 9)

# additions to make:
# input desired conf interval
# input num tails

# tails. if tails = 2 then return this, else that. 



#### Chapter 3: Hypothesis Testing ####

# DEFINITIONS:
# p value: area to the right of the z-score (if positive); (probability of the occurrence of a given event)
# alpha level (critical region probability): chosen significance level; standard to compare to p-value
# z critical value: z score for alpha level
# value in z score table (CDF?): proportion of values less than a given Z score, aka percentile

  # Single Tailed Critical Values
  # critical region a=.05, z score is ~1.645 (cdf = .95)
  # critical region a=.01, z score is ~2.33 (cdf = .99)
  # critical region a=.001, z score is ~3.08 (cdf = .999)

  # Two tailed critical values
  # critical region a=.05, z score is ~1.96 (cdf = .975)
  # critical region a=.01, z score is ~2.576 (cdf = .995)
  # critical region a=.001, z score is ~3.291 (cdf = .9995)

# xbar is significant at p < .05
# split alpha level in half: two tailed. Z-critical values become pos/neg of same number



# engagement / learning results
e.l.results <- read.delim("clipboard")
names(e.l.results)[2] <- "engagement"
names(e.l.results)[3] <- "learning"

summary(e.l.results)
apply(e.l.results, 2, sd)
sd(e.l.results$engagement)
e.l.results$engagement %>% sd() 

## R SD FUNCTION USES BESSEL'S CORRECTION; CREATE STANDARD DEVIATION FUNCTION WITHOUT CORRECTION
# reference: http://rstudio-pubs-static.s3.amazonaws.com/331368_abe76b437ef94b36a013560b6d8a593a.html
sd.pop <- function(x) sqrt(mean((x - mean(x))^2))

sd.pop(e.l.results$engagement)

f_estimation_sample(xbar = 8.3,mu = 7.47, stdev = 2.41, sample.size = 30)
f_estimation_sample(xbar = 8.3,mu = 7.47, stdev = 2.41, sample.size = 50)

# "at alpha level of .5, {reject | fail to reject} null hypothesis"
# "the probability of obtaining this {sample | sample mean} is less than our alpha level

# statistical decision errors

f_estimation_sample(xbar = 28, mu = 25, stdev = 6, sample.size = 36)
f_estimation_sample(xbar = 22.793,mu = 22.965, stdev = .360, sample.size = 16)
f_estimation_sample(xbar = 9640,mu = 7895, stdev = 230, sample.size = 5)


#### Interlude: plotting distributions ####


pnorm()
dnorm(3)
data_frame(values = rnorm(5000)) %>%
  ggplot( aes(x = values)) + geom_histogram()


myseq <- seq(-4,4,.1)
mydensities <- dnorm(myseq)
mycumulative <- pnorm(myseq)
myrandom <- rnorm(1000)

plot(myseq, mydensities)
plot(myseq, mycumulative)
hist(myrandom, breaks = 30)