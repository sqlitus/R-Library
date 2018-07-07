library(tidyverse)

# Dplyr vs base data manipulation

# select rows
diamonds %>% slice(1)
diamonds[1,]

# select columns
diamonds %>% select(1)
diamonds %>% select("carat")
diamonds[,1]
diamonds[,"carat"]


diamonds[c(1,2),]
diamonds[,c(1,2)]


# stock trading charts & formulas; MACD; moving averages
library(lazyeval)
library(zoo)

# scrub way using a loop
closes <- c(11,12,13,14,15,16,17)
closes[2:6]
for (i in 1:3){
  closes[i:i+4] %>% print()
}


# Exponential Moving Average (EMA) using QuantTools vs TTR

install.packages("QuantTools")
library(QuantTools)

install.packages("TTR")
library(TTR)

closes.df %>% 
  mutate(ema.5 = ema(closes, 5))

closes.df %>% 
  mutate(sma.5 = SMA(closes, 5)) %>%
  mutate(ema.5 = EMA(closes, 5, wilder = TRUE)) %>%
  mutate(ema.again = EMA(closes, n=5,  ratio=2/(length(closes)+1)))



# fuzzy string matching with category list of words ----
library(tidyverse)

issue_string <- data_frame(name = c("handheld", "scanner", "latency", "scale", "bob-problem", "network outage"))

sample_tickets <- data_frame(name = c("was scanning items then the power went off", "need a new hand held replacement",
                    "experiencing high latency when turning off power", "my scale is broken.",
                    "the new scan gun isn't working", "the new scanning gun isn't working",
                    "more of these scan guns not working", "scann gun is borked.", "slow latencies at store"))


dist.name <- adist(issue_string$name, sample_tickets$name, partial = TRUE, ignore.case = TRUE)
dist.name.t <- t(dist.name) %>% as.data.frame()
names(dist.name.t) <- issue_string$name
sample_tickets_fuzzy <- cbind(sample_tickets, dist.name.t)

# problem: need to fuzzy match WHOLE WORDS ONLY, NOT STRINGS.

dist.name
adist(issue_string$name, sample_tickets$name, partial = TRUE, ignore.case = TRUE)
adist(issue_string$name, sample_tickets$name, fixed = FALSE)



# fuzzy matching spelling correction for text mining; spelling correections against a dictionary ----
library(tidytext); library(fuzzyjoin); library(tidyverse); library(qdapDictionaries)

my_dictionary <- DICTIONARY

set.seed(2016)
sub_misspellings <- misspellings %>% sample_n(1000)

joined <- sub_misspellings %>% stringdist_inner_join(my_dictionary, by = c(misspelling = "word"), max_dist = 1)
joined %>% count(misspelling, correct)

which_correct <- joined %>%
  group_by(misspelling, correct) %>%
  summarize(guesses = n(), one_correct = any(correct == word))

which_correct
mean(which_correct$one_correct)






## ovot home example: find count of 'things' active & assigned between [date|numeric] range  ----
## (referencing 2+ tables)
library(tidyverse)
base_table <- data_frame(base = seq(1:40))
ref_table <- data_frame(num = paste0("n", seq(10)),
                        start = sample(1:20, 10),
                        end = sample(21:40,10))

# find all tix open between start & end dates
base_table$assigned_on_date <- NA
for (i in 1:40){
  base_table$assigned_on_date[i] <- ref_table %>% filter(start <= base_table$base[i] & base_table$base[i] < end) %>% nrow()
}

# create status table - to cross-reference with start & end
ref_table_active <- data_frame(num = sample(ref_table$num, 18, replace = T))
ref_table_active$status <- sample(c("active","pending","resolved"))
ref_table_active$start <- sample(1:20, 18)
ref_table_active$end <- sample(21:40, 18)

# all tix active on the calendar date
base_table$active_on_date <- NA
for (i in 1:nrow(base_table)){
  base_table$active_on_date[i] <- ref_table_active %>% 
    filter(start <= base_table$base[i] & base_table$base[i] < end & status == "active") %>% nrow()
}

# all tix active & assigned on the date
base_table$assigned_and_active <- NA
for (i in 1:nrow(base_table)){
  a <- ref_table %>% filter(start <= base_table$base[i] & base_table$base[i] < end)  
  b <- ref_table_active %>% filter(start <= base_table$base[i] & base_table$base[i] < end & status == "active")
  base_table$assigned_and_active[i] <- inner_join(a, b, by = "num") %>% nrow()
}

# END OVOT HOME SAMPLE


#### Anomaly Detection (?) algorithm - detect trends extrapolating from attributes

ticket.data <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\Sample Ticket Data.csv")
ticket.data$Created <- as.character(ticket.data$Created)
ticket.data$Created <- as.POSIXct(ticket.data$Created, format="%m/%d/%Y %H:%M")
ticket.data$Resolved <- as.character(ticket.data$Resolved)
ticket.data$Resolved <- as.POSIXct(ticket.data$Resolved, format="%m/%d/%Y %H:%M")
# algorithm for creating all feature permutations
# feature engineer date columns...

# create columns from all factor permutations...
library(tidyverse)
unique(ticket.data$Store)
unique(ticket.data$Support.Group)
sample(unique(ticket.data$Support.Group), length(unique(ticket.data$Support.Group)))




# subsetting & creating dataframe columns programmatically
a <- head(ticket.data)[1:5]
a

a[,1] %>% str()
a[6] <- "a"
ncol(a)
a[,7] <- "b"
length(a)
a
a$Support.Group %>% str()
a[1]
a["ID"]

myColFunc <- function(df, column){
  unique(df[,column])
}
myColFunc(a, "Support.Goup")
a[,"Support.Group"] %>% unique()
unique(a[,"Support.Group"])
a
a[unique(a$Support.Group)] <- NA
unique(a$Support.Group)
a[c("my","columns")] <- NA
a[a$Support.Group] <- NA
a$Support.Group %>% str()
unique(a$Support.Group)
unique(a$Support.Group) %>% str()

# method 2
rm(a)
x <- runif(20)
a <- as.data.frame(x)
a
a[2:11] <- sapply(1:10, "+", a[[1]])
a

# solution - method 3: string vector
a[as.character(unique(a$Support.Group))] <- NA
a[as.character(unique(a$Support.Group))] <- 
  case_when(a$Support.Group == as.character(unique(a$Support.Group)) ~ 1)
a


#### cusom factor generation algorithm ----
library(tidyverse)
z <- data_frame(id = c(1,2,3,4,5,6), 
                category = sample(c("printer", "scanner", "tax", NA), 6, replace = TRUE),
                team = sample(c('L1', 'Aloha', 'L2', NA), 6, replace = TRUE)
)

my_factor_generation <- function(df, mycols){
  for (col in 1:length(mycols)){
    for (ele in 1:length(df[[mycols[col]]])){
      this_ele = toString(z[ele,mycols[col]])
      df.cols = names(df) # cannot use names(df) directly as this changes them
      if (!any(df.cols %in% this_ele)){  # convert names & values case later
        df <- df %>% mutate(!!quo_name(this_ele) := NA)
        print(paste("new column created for", this_ele)) # debugging
      }
      df[ele,this_ele] = 1  # (all NAs in single column)
    }
  }
  return(df)
}

z.new <- my_factor_generation(z, "team")
z.new <- my_factor_generation(z, c("team","category"))

# test w/ assoc rules
library(arules); library(arulesViz)
z.new %>% str()
z.new.cols <- names(z.new)
z.trans <- z.new %>% mutate_at(z.new.cols, factor) %>% select(2:3) # submit categories OR flag columns
z.trans <- as(object = z.trans, "transactions")
inspect(z.trans)
itemFrequency(z.trans)


# left off: plugging factor df into transactions...