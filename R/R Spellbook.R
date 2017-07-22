
#### R SPELLBOOK ####


#### Legend ####

## [R] SPELLBOOK LEGEND ##

## GROUP - TITLE
# single function
# * single function + creates variable used for other functions

#### Utility Functions ####

# load and/or download packages workflow
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak("checkpoint")
checkpoint("2017-07-12")

packages <- c("ggplot2", "tm", "sqldf", "qdap", "scales")
ipak(packages)

#### Word Frequency Matrix ####

## WORD FREQUENCY MATRIX

library(qdap)
DATA 

# total words frequency
wfm(DATA$state)

# word frequency with grouping
wfm(DATA$state, list(DATA$person, DATA$adult))

# * word frequencies grouped by row
freqs <- as.data.frame(t(wfm(DATA$state, 1:nrow(DATA))))

# * word occurrences (flags) by row
flags <- sapply(freqs,function(i){
  ifelse(i>0,1,0)
})

# word [frequencies | flags] joined back to table - MUST USE CBIND TO PRESERVE CORRECT DF COLUMN NAMES
cbind(DATA,flags)


# total count word frequency
colSums(freqs)
# total count word occurrence (count of rows)
colSums(flags)

# * top X words by [frequency | occurrence]
topx <- rev(sort(colSums(flags)))[1:10]

# orig table with top X word [frequency | occurrence] appended
cbind(DATA,flags[,names(topx)])
# end ====