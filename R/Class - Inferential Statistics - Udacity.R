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
  
# Lesson 2: Estimation
  library(tidyverse)
  engagement.ratio <- read.delim("clipboard")
  names(engagement.ratio)[1] <- "er"
  ggplot(engagement.ratio, aes(er)) + geom_histogram()
  summary(engagement.ratio)
  sd(engagement.ratio$er)
  sd(engagement.ratio[["er"]])
  
  # EDA function - take in dataframe and column and return analysis
  f_estimation <- function(df, col){
    o1 <- summary(df[[col]])
    o2 <- sd(df[[col]])
    o3 <- ggplot(df, aes_string(col)) + geom_histogram()
    return(list(summary = o1, sd = o2, plot = o3))
  }
  f_estimation(engagement.ratio, "er")