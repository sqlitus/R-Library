#### Workspace #####

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak("tidyverse")
# ipak(c("ggplot2", "tm", "sqldf", "scales","dplyr", "tidyr", "tibble", "mailR","RColorBrewer","stringr","tidyverse", 
#        "tidytext", "plotly", "wordcloud"))
packages <- c("googleVis", "plotly", "ggthemes", "officer", "RDCOMClient") # slidy?
ipak(packages)

# import data & transformations (from library) - sample ticket data
df <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\Sample Ticket Data.csv")
df$Title <- as.character(df$Title)
df$Created <- as.character(df$Created)
df$Created <- as.POSIXct(df$Created, format="%m/%d/%Y %H:%M")
df$Priority <- paste("P",df$Priority, sep = "")
df$Priority <- as.factor(df$Priority)
df$Created.Week <- as.Date(cut(df$Created, breaks = "week", start.on.monday = T))

convert_to_date <- function(v){as.character(v) %>% as.POSIXct(format = "%m/%d/%Y %H:%M")}
df$FirstAssigned <- convert_to_date(df$FirstAssigned)
df$Resolved <- convert_to_date(df$Resolved)

# Time Difference in units = minutes
df$Time.To.Response <- difftime(df$FirstAssigned, df$Created, units = "mins")
df$Time.To.Restore.Service <- difftime(df$Resolved, df$Created, units = "mins")
df$Assigned.To.Resolve <- difftime(df$Resolved, df$FirstAssigned, units = "mins")

#### Webscrape Project ####
# regularly scrape websites to df
# tm on df
# plot
# shiny dashboard choose inputs






#### which column names are numeric, factor, etc ####
col.choice <- names(df)[sapply(df, is.numeric)]
col.choice <- names(df)[sapply(df, is.factor)]
select(df, col.choice) %>% str()
df[,col.choice] %>% str() # subset df returns vector if just 1 column



#### word trend over time linechart entry; include unnest_tokens into function ####
library(tidyverse); library(tidytext); library(plotly)

# unigrams; removing duplicate words per IR
df.unigram <- df %>% unnest_tokens(output = word, input = Title, drop = F) %>% distinct()
df.unigrams.byweek <- df.unigram %>% group_by(Created.Week, word) %>% count()
p <- ggplot(df.unigrams.byweek, aes(Created.Week, n, fill = word)) + geom_area() +
  theme(legend.position = "none")
ggplotly(p)
ggplot(df[df$Priority %in% c("P3", "P4") ,], aes(Created.Week, fill = Priority)) + geom_bar()

test <- df %>% group_by(Created.Week) %>% count()
ggplot(test, aes(Created.Week, n)) + geom_area()


## new example
a <- data_frame(value = round(rnorm(10, mean = 20, sd = 6)),
                date = seq.Date(as.Date("2017-05-05"), as.Date("2017-05-14"), by = 1),
                category = rep(c("cat a", "cat b"),5))
a
ggplot(a, aes(date, value)) + geom_line()


## parameter dataset and ggplot; choose word example; plot function keeps same axis
LinePlotForWord <- function(myword){
  df.word.choose <- df.unigram %>% filter(word == myword) %>% group_by(Created.Week) %>% count()
  ggplot(df.word.choose, aes(Created.Week, n)) + geom_line() +
    expand_limits(x = c(min(df.unigram$Created.Week),max(df.unigram$Created.Week)))
}
LinePlotForWord("lane")
LinePlotForWord("the")
LinePlotForWord("froze")
LinePlotForWord("defect")


#### RSelenium First Try ####

install.packages("RSelenium")
library(RSelenium)
checkForServer() # search for and download Selenium Server java binary.  Only need to run once.
startServer() # run Selenium Server binary
remDr <- remoteDriver(browserName="firefox", port=4444) # instantiate remote driver to connect to Selenium Server
remDr$open(silent=T) # open web browser




#### custom functions for package ####

# for monday-sunday weeks; week starts monday (day 1)

x_mondays_ago <- function(x){
  (Sys.Date() - 7 * x) + (1 - as.integer(format(Sys.Date(), format = "%u")))
}
x_mondays_ago(4)

x_sundays_ago <- function(x){
  (Sys.Date() - 7 * x) + (7 - as.integer(format(Sys.Date(), format = "%u")))
}
x_sundays_ago(1)

x_weeks_y_days_ago <- function(x,y){
  (Sys.Date() - 7 * x) + (y - as.integer(format(Sys.Date(), format = "%u")))
}
x_weeks_y_days_ago(2,7)


#### testing making named vectors from df values ####
test <- df %>% group_by(Store) %>% count(Assigned.To)
nrow(test)

x <- test$Assigned.To
names(x) <- test$Store


  
  


#### Data Analysis with R ####

# gonna use my sample ticket dataset (df)
# 10/27/2017

histogram_this <- function(df, v) ggplot(df, aes(as.numeric(v))) + geom_histogram()

qplot(Time.To.Response, data = df)
histogram_this(df, df$Time.To.Response)
histogram_this(df, df$Time.To.Restore.Service)
# plan: funct that identifies numeric vars, then histograms them, then plots these on a grid
df %>% str()
if (cla)
class(df$ID)
df[,1] %>% str()
df$ID %>% str()
df[,1] %>% class()
df[,2] %>% class()
df[,2][[1]] %>% class()
df[,2][[1]] %>% str()
class(df[,1]) == "factor"


for (i in 1:length(names(df))){
  paste(names(df)[i], class(df[,i])) %>% print()
  paste(names(df)[i], class(df[,i]), if_else(class(df[,1] == "factor", "is a factor", "NOT FAC"))) %>% print()
  if (class(df[,1]) == "factor") {print("is a factor")}
}

# 10/28/2017 - ggplot vs qplot
ggplot(df, aes(Time.To.Response)) + geom_histogram()
qplot(Time.To.Response, data = df) + scale_x_continuous(limits = c(0,2000))

ggplot(df, aes(x = Priority)) + geom_bar()

# plot change range automatically based on percentile; change binwidth, axis label intervals
quantile(df$Time.To.Response, .5) # percentile
df$Time.To.Response[df$Time.To.Response < quantile(df$Time.To.Response, .5)] # vector of percentile
max(df$Time.To.Response); median(df$Time.To.Response); quantile(df$Time.To.Response, .5)
ggplot(df, aes(Time.To.Response)) + geom_histogram(binwidth = 50)
ggplot(df, aes(Time.To.Response)) + geom_histogram(binwidth = 50) + scale_x_continuous(limits = c(0,quantile(df$Time.To.Response, .9)))
ggplot(df, aes(Time.To.Response)) + geom_histogram(binwidth = 50, fill = "#5760AB", color = "black") + scale_x_continuous(limits = c(0,1500), breaks = seq(0, 1500, 100))
  
ggplot(df, aes(Time.To.Response)) + geom_histogram(binwidth = 50) + facet_grid(.~Priority)

filter(df, Store == "Lamar")
subset(df, Store == "Lamar") # keeps row numbers 

by(as.numeric(df$Time.To.Response), df$Store, summary)

# 1 continuous var graph #2: frequency poly
ggplot(df, aes(Time.To.Response)) + geom_histogram(binwidth = 50)
ggplot(df, aes(Time.To.Response, color = Priority)) + geom_freqpoly()
ggplot(df, aes(Time.To.Response, color = Priority)) + geom_density()
qplot(x = Time.To.Response, data = df, color = Priority, geom = 'freqpoly')
qplot(x = Time.To.Response, data = df, fill = Priority)
qplot(x = as.numeric(Time.To.Response), data = df, fill = Priority) + scale_x_log10()

# what priority has higher TTR?
by(df$Time.To.Response, df$Priority, sum)
df %>% group_by(Priority) %>% summarise(sum(Time.To.Response))


# 1 category + 1 cont var graph #3: boxplot
ggplot(df, aes(x = Priority, y = Time.To.Response)) + geom_boxplot()
qplot(x = Priority, y = Time.To.Response, data = df, geom = 'boxplot')

# incorrect way: this removes data points from the calculation; use above to slice the axis
ggplot(df, aes(x = Priority, y = Time.To.Response)) + geom_boxplot() + scale_y_continuous(limits = c(0,2000))

# correct way to change view of graph
ggplot(df, aes(x = Priority, y = Time.To.Response)) + geom_boxplot() + coord_cartesian(ylim = c(0,2000))

# by inspection is great for continuous vs category; complements box plot
by(as.numeric(df$Time.To.Response), df$Priority, summary)
by(as.numeric(df$Time.To.Response), df$Priority, sum)
by(df, df$Priority, summary)

# similar to manipulating to find individual values
df %>% filter(Priority == "P1") %>% summarise(min(Created))
df %>% filter(Priority == "P4") %>% summarise(mean(FirstAssigned))


# ordered factor - re sort factor levels, and find min/max of factor
factor(df$Location, levels = sort(levels(df$Location), decreasing = TRUE)) %>% levels()
factor(df$Location, levels = rev(levels(df$Location))) %>% levels()
factor(df$Location, levels = rev(levels(df$Location))) %>% levels() %>% min()
factor(df$Location, levels = rev(levels(df$Location))) %>% levels() %>% max()
factor(df$Location, levels = rev(levels(df$Location))) %>% levels() %>% median()





#### 10/28/2017 - multiplot function ####

# method 1 (preferred): gridExtra package. Native/tidyverse?
library(gridExtra)
grid.arrange(plot.1, plot.2, ncol = 2)


#### 10/16/2017 - word cloud ####
wordcloud(df$Title, max.words = 100, random.order = FALSE, colors = df$Num.Assigns)
wordcloud(df$Title, max.words = 50, colors = brewer.pal(8, "Dark2")[factor(df$Support.Group)], random.order = FALSE)

# custom color ramp palette...
colfunc <- colorRampPalette(c("red", "blue"))
df$colors <- lookup(df$Num.Assigns, levels(df$Num.Assigns),
                           rev(colfunc(length(levels(df$Num.Assigns)))))
head(word.freq, 10)





#### 10/14/2017 - which in ####
a <- data_frame(x = c("one","two","three","four"))
b <- c("two")
filter(a, x %in% b)
filter(a, !x %in% b)

# match makes no sense; use %in%
match(b, a)
match(a,b)
match("three", a)
match(a, "three")

#### 10/11/2017 - send outlook email with image ####

# example: https://blog.mdneuzerling.com/2017/03/19/using-r-to-send-an-outlook-email-with-an-inline-image/


#### 10/10/2017 - Randomizing Data ####

# generating random data
mynum <- 22
mysample <- sample(1:mynum)
myrnorm <- rnorm(mynum)
myletters <- sample(LETTERS[1:4], mynum, replace = TRUE)
mydates <- seq(as.Date("2017-01-01"), as.Date("2017-02-01"), by = "day") %>% sample(mynum, replace = TRUE)

test <- data_frame(sample = mysample, rnorm = myrnorm, letters = myletters, date = mydates)
ggplot(test, aes(letters)) + geom_bar()

ggplot(test, aes(date, fill = as.factor(date))) + geom_bar()

#### 10/10/2017 - ggplot color palettes ####
ggplot(df, aes(Created.Week, fill = Support.Group)) +
  geom_bar() +
  facet_wrap(~Store) + 
  ggtitle("PDF PICTURE")
  # scale_fill_brewer(palette = "Set1")
  scale_fill_manual(values = rainbow(22))
ggsave(filename = "myplot.pdf")


# base palette
palette()
colors()[1:10]
sample(colors(), 5)

# custom color palettes - discrete
ggplot(df, aes(Store, fill = Store)) + geom_bar()
ggplot(df, aes(Store, fill = Store)) + geom_bar() + scale_fill_brewer("Set2")
ggplot(df, aes(Store, fill = Store)) + geom_bar() + scale_fill_manual(values = rainbow(10))
ggplot(df, aes(Store, fill = Store)) + geom_bar() + scale_fill_manual(values = colors()[1:5])
ggplot(df, aes(Created.Week, fill = as.factor(Created.Week))) + geom_bar() + 
  scale_fill_manual(values = colors()[1:222])
ggplot(df, aes(Created.Week, fill = as.factor(Created.Week))) + geom_bar() + scale_alpha()

# continuous

ggplot(df, aes(as.factor(Created.Week), y = Num.Assigns, fill = Num.Assigns)) + 
  geom_bar(stat = "identity") + 
  scale_color_gradient(low = "red", high = "blue")
ggplot(df, aes(Location, sum(Num.Assigns))) + geom_point()


#### 10/1/2017 - R for Data Science ch5: data transformation ####

str(1/49 * 49) 
str(1)
1/2*2 == 1

test <- data_frame(c = c("a","a","a","b","b","b"), a = c(1,2,3,4,5,NA), b = c(5,10,15,20,NA,25))

# na.rum = TRUE removes missinnnnnnnng values prior to computation, so that calcs do not = NULL (NA)
test %>%
  group_by(c) %>%
  summarize(le.count = n(),
            dist = mean(a, na.rm = TRUE),
            delay = mean(b, na.rm = TRUE),
            std = sd(a, na.rm = TRUE))
test %>% 
  count(c) %>%
  mutate(dist = mean(a))



#### 9/29/2017 - beginning ngrams ####

# bigrams - count (tf)
df.bigram.tf <- df %>%
  unnest_tokens(output = bigram, input = Title, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

# bigrams - tf - window function
df.bigram.tf.window <- df %>%
  unnest_tokens(output = bigram, input = Title, token = "ngrams", n = 2, drop = FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  group_by(word1, word2) %>%
  mutate(total.bigram = n()) %>%
  arrange(desc(total.bigram))
  
# bigrams - tf-idf by [category], [category], word
df.bigram.tf.idf <- df %>%
  unnest_tokens(output = bigram, input = Title, token = "ngrams", n = 2) %>%
  count(Classification, bigram, sort = TRUE) %>%
  bind_tf_idf(Classification, bigram, n) 
# plot it
test <- df.bigram.tf.idf %>%
  group_by(Classification) %>%
  top_n(5, wt = tf_idf) %>%
  ungroup %>%
  mutate(Classification = factor(Classification) %>% forcats::fct_rev()) %>%
  ggplot(aes(bigram, tf_idf, fill = Classification)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  facet_wrap(~Classification, ncol = 2, scales = "free")
test



#### 9/28/2017 - officer (Powerpoint) ####

library(officer)
# REFERENCE: https://davidgohel.github.io/officer/articles/powerpoint.html

pp.plot <- ggplot(df, aes(Created.Week)) +
  geom_bar()

pp.plot

# get layout details to use
layout_summary(mydoc)
layout_properties(mydoc)
slide_summary(mydoc)

# make OR read pp deck
mydoc <- read_pptx(path = "C:\\Users\\Chris\\Downloads\\econ_update.pptx")

# add slide, add text, select slide to manipulate
mydoc <- mydoc %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = "here is some Title text") %>%
    ph_with_text(type = "ftr", str = "ftr text") %>%
    ph_with_text(type = "dt", str = "dt text") %>%
    ph_with_text(type = "sldNum", str = "sldNum text") %>%
    ph_with_text(str = "Hello world body text", type = "body") %>%
  add_slide(layout = "Section Header", master = "Office Theme") %>%
    ph_with_text(type = "title", str = "another add slide with title text") %>%
    ph_with_text(type = "ftr", str = "ftr text") %>%
    ph_with_text(type = "dt", str = "dt text") %>%
    ph_with_text(type = "sldNum", str = "sldNum text") %>%
    ph_with_text(str = "Hello world body text", type = "body") %>%
  on_slide(index = 3) %>%
    ph_remove(type = "title") %>%
    ph_with_text(type = "title", str = "REPLACED OLD TITLE") %>%
    ph_with_table(type = "body", value = head(mtcars)) %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", str = "last added slide title") %>%
  ph_add_par() %>%
    ph_add_text(str = "ph_add_text to the paragraph") 
    # ph_add_par(level = 2) %>%
    # ph_add_text(str = "Level 2") %>% 
    # ph_add_par(level = 3) %>%
    # ph_add_text(str = "Level 3")


# write (print) the power point doc; will write over
print(mydoc, target = "myNewPowerPoint.pptx") 



#### 9/24/2017 - diff ways to subset dataframe/vectors ####

# factor vectors
df[,"ID"] 
df[,1] 
df[,c("ID")] 
df[,c(1)] 

# dataframe
df[,1:2]
df[,c(1:2)]
df[,c("ID", "Created")]
df %>% select(1)
df %>% select(ID)



#### handy functions #####

  # sequence of numbers
  seq(5)
  seq(length.out = 5)
  seq(1,2, length.out = 5)
  
  # 9/23/2017 - for loops vs apply functions
  # get all columns of [type] ... sapply instead of for loop
  df[,sapply(X = df, FUN = is.integer)]
  df[,sapply(df, is.factor)]
  
  numeric_col <- rep(FALSE, ncol(df))
  for (i in 1:ncol(df)){
    is.numeric(df[,i])
  }
  

#### 9/23/2017 - read excel data ####
  
  library(tidyverse) # library(readxl)
  # see all functions in package
  ls("package:readxl")
  
  test <- read_excel("D:\\Datasets\\Food budget\\Food_2016.xlsx")
  test %>% select(1:5)
  test <- read_excel("D:\\Datasets\\Food budget\\Food_2016.xlsx", 5)

  

  

  

#### 9/19/2017 - tidy text mining ch3 - analyzing word & document frequencies ####
  
  # summary table: total words per [store], and count by [store] and [word]
  store_words <- df %>%
    unnest_tokens(word, Title, drop = F) %>%
    count(Store, word, sort = T) %>%
    ungroup()
    
  total_words <- store_words %>%
    group_by(Store) %>%
    summarise(total = sum(n))
  
  store_words <- left_join(store_words, total_words)
  
  # this method doesn't summarize the whole table
    mywords <- df %>%
      unnest_tokens(word, Title) %>%
      group_by(Store, word) %>%
      mutate(store.word.count = n()) %>%
      group_by(Store) %>%
      mutate(store.words.total = n()) %>% ungroup()
      
    mywords %>% filter(word == "the", Store == "Hyde Park") %>% select(store.word.count, store.words.total)
  
  # plot not right
  p <- ggplot(store_words, aes(n/total, fill = Store))+
    geom_histogram(show.legend = F)+
    xlim(NA, .0009)+
    facet_wrap(~Store, ncol = 2)
  ggplotly(p)
  
  p <- ggplot()+geom_histogram(
    mapping = aes(n/total, fill = Store),
    data = store_words
  )+
    facet_wrap(~Store, ncol = 2)
  p
  
  ## !!! best way to ggplot IMO, data in ggplot, aes in layers !!!
  p <- ggplot(store_words)+
    geom_histogram(mapping = aes(n/total, fill = Store))+
    facet_wrap(~Store, ncol = 2)
  p
  ggplotly(p)
  
#### 9/17/2017 - tidy text mining ch 1 - tidy text ####
  
  text <- c("Because I could not stop for Death -",
            "He kindly stopped for me -",
            "The Carriage held but just Ourselves -",
            "and Immortality")
  
  text
  text_df <- data_frame(line = seq(length(text)), textStuff = text)
  text_df %>%
    unnest_tokens(word, textStuff)
  
  
  ## personal example - sample ticket data
  df.tm <- data_frame(ID = df$ID, Created = df$Created, Title = df$Title)
  df.tm$Title <- as.character(df.tm$Title)
  
  # splits Title column into words, removes punc, converts lowercase, and turns into long dataframe (check args)
  df.tm <- df.tm %>% unnest_tokens(output = title.words, input = Title, drop = F)
  
  # not in stopword list - with ! %in% or anti_join
  df.tm.tidy <- df.tm %>%
    filter(!(title.words %in% stop_words$word))
  
  df.tm.tidy <- df.tm %>%
    anti_join(stop_words, by = c("title.words" = "word"))
  
  word.blacklist <- c("r10", "lane")
  df.tm.tidy <- df.tm.tidy %>% filter(!(title.words %in% word.blacklist))

  # count words, reorder word factor by level, top X, plot
  df.tm.tidy %>%
    count(title.words, sort = T) %>%
    slice(1:5) %>%
    mutate(title.words = reorder(title.words, n)) %>%
    ggplot(aes(title.words, n, fill = title.words))+geom_bar(stat = "identity")+coord_flip()
  ### find way to reverse x axis order ###
    
    # (quick compare)
    df.tm.tidy %>%
    count(title.words, sort = T) %>%
    slice(1:5) 
  
  
  ## jane austin
    library(janeaustenr)
    d <- data_frame(txt = prideprejudice)
    d
    
    d %>%
      unnest_tokens(word, txt)
    
    d %>%
      unnest_tokens(sentence, txt, token = "sentences")
    
    # tokenize HTML
    h <- data_frame(row = 1:2,
                    text = c("<h1>Text <b>is<b>", "<a href='example.com'>here</a>"))
    h %>%
      unnest_tokens(word, text, format = "html")
    
  ## 1.5 - word frequency comparison
    
    tidy.df <- df %>%
      unnest_tokens(word, Title, drop = F) %>%
      anti_join(stop_words) 
    
    frequency <- tidy.df %>%
      count(Store, word) %>%
      group_by(Store) %>%
      mutate(proportion = n / sum(n)) %>%
      select(-n) 
      
    frequency.p <- ggplot(frequency, aes(proportion, "Store"))+geom_abline(color = "gray40", lty = 2)+
      geom_jitter(alpha = .05, size = 2.5, width = .3, height = .3)+
      geom_text(aes(label = word), check_overlap = T, vjust = 1.5)+
      # scale_x_log10(labels = percent_format())+
      # scale_y_log10(labels = percent_format())+
      scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
      facet_wrap(~Store)
    
    ggplotly(frequency.p)
  
seq(5, length.out = 10)


#### 9/14/2017 - more ggplotly - add text to tooltip ####

  p <- ggplot(df, aes(Location, fill=Store, text = paste("Number of reassigns: ", Num.Assigns)))+geom_bar()
  ggplotly(p, tooltip = c("text","x"))
  
  # badass example: https://data.nozav.org/app/scatterD3/
  # hover over legend
  

#### 9/14/2017 - regex extract/replace text ####

  entries <- c ("Trial 1 58 cervicornis (52 match)", "Trial 2 60 terrestris (51 matched)", "Trial 8 109 flavicollis (101 matches)")
  entries <- c(entries, "some more /\ text n' stuff", "ra ri ru re ro rarirurero ", "text not in (text in parenths) and (more in paren)",
               "trial 5: the trip to trinity", "trial 6 - trinidad and trinity triangles tip trips, from trial 5")
  # extract content in parenthesis; replace "(" OR ")"
  str_extract_all(entries, "\\(.*\\)$") %>% str_replace_all("\\(|\\)", "")
  substr(entries, start = str_locate(entries, "\\(.*"), stop = str_locate(entries, "\\)"))
  
  # extract the other text (not parenthesis)
  str_replace_all(entries, "\\(.*\\)$", "") %>%
    str_replace(" $", "")
  
  # find location & length of match (int vector)
  r.a <- entries %>% str_locate("rr+")
  r.b <- regexpr("rr+", entries)
  
  attributes(r.b)
  attr(r.b, "match.length")
  
  # test - other chars
  str_locate(entries, "\\bs")
  str_extract_all(entries, "\\bs")
  str_extract_all(entries, "r.{1}") # match r, then any other char after
  str_extract_all(entries, "r[ei]") # match r, then e or i
  
  # find char matches at end of word / within word
  str_locate_all(entries, "ra\\B")
  str_locate_all(entries, "ra\\b")
  regexpr("ra\\b", entries)
  str_locate_all(entries, "ial\\b")
  
  # case insensitive
  str_detect(entries, "(?i)tri")
  str_locate_all(entries, "(?i)tri")
  str_extract_all(entries, "(?i)tri")
  
  
  # list all records with case-insensitive "trial", find trial & num, final all mentions of trials & nums, remove first trial mention
  entries[grep("^(?i)TRIAL", entries)]
  entries[grep("^trial", entries)]
  str_extract(entries, "^(?i)trial [0-9]*")
  str_extract_all(entries, "^(?i)trial [0-9]*")
  str_replace(entries, "^(?i)trial [0-9]*", "")
  
  ## test regex with sample data
  
  # regex anchors. string at boundary of word.
  df$Assigned.To
  df.regex <- df %>% select(Assigned.To) %>% filter(grepl("A", df$Assigned.To))
  df$Assigned.To[grep("A", df$Assigned.To)]
  df$Assigned.To[grep("\\b(?i)A", df$Assigned.To)]
  df$Assigned.To[grep("\\b(?i)e", df$Assigned.To)]
  df$Assigned.To[grep("\\B(?i)e", df$Assigned.To)]
  df$Assigned.To[grep("\\b(?i)e", df$Assigned.To)]

#### 9/13/2017 - The R book 2.12 - Text, char strings ####

# 2.12
a <- c("one","two","three","f","f","f","f","f","f","f","f","f","f","f","f","f","f","f")
b <- as.factor(a)
length(a)
str(length(a[3]))
str(length(a[11]))
length("lalala")
length(b[1])
attributes(b)
nchar(a)
which(a == "f")
??concat

# 2.12.1 - paste character strings together
a.2 <- c("lala","blabla")
a.3 <- c(a,a.2)
a.3
str(a.3)
paste("alol","bbb")
paste(a,a.2)







#### 9/11/2017 - assign colors to factors ####

  function(EXCELLENT_COLOR_REFERENCE){
    # https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
    # https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  }

  #Some test data
  dat <- data.frame(x=runif(10),y=runif(10),
                    grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)
  
  #Create a custom color scale. create vector of color values, then change index names to the category...
  library(RColorBrewer)
  myColors <- brewer.pal(5,"Set1")
  # change names of vector to DISTINCT values of a factor (levels)
  names(myColors) <- levels(dat$grp) 
  # ggplot argument for scale colors...
  colScale <- scale_colour_manual(name = "grp",values = myColors)

  #One plot with all the data
  p <- ggplot(dat,aes(x,y,colour = grp)) + geom_point()
  p1 <- p + colScale
  
  #A second plot with only four of the levels
  p2 <- p %+% droplevels(subset(dat[4:10,])) + colScale
  
  
  # unique color sclae workflow
  # get levels of factors, get palette of equiv length ... (this will change colors if more are added on...)
  
  # get unique list - factor levels
  analyst.list <- levels(df$Assigned.To)
  analyst.list.2 <- paste(analyst.list, "lol")
  analyst.list.3 <- c(analyst.list, analyst.list.2)

  # diff colors tho with rainbow.
  analyst.colors <- rainbow(length(analyst.list))
  names(analyst.colors) <- analyst.list
  analyst.colors.3 <- rainbow(length(analyst.list.3))
  
  # aes(color = axis)
  analyst.plot <- df %>% ggplot(aes(x=Assigned.To, fill = Assigned.To))+geom_bar()
  
  # assign color to x axis with "..x.."
  analyst.plot.2 <- df %>% ggplot(aes(x=Assigned.To, fill = ..x..))+geom_bar()
  
  colors.1 <- scale_fill_brewer(c(palette = "Set3"))
  colors.2 <- scale_fill_continuous(low = "Red", high = "Blue")
  colors.3 <- scale_fill_gradient(low = "Red", high = "Blue")
  analyst.plot.2 + colors.2
  
  # easy stacked bar chart using fill
  df %>% ggplot(aes(x=Store, fill = Assigned.To))+geom_bar()+scale_color_manual(values = myColors)
  
#### 9/10/2017 - mailR; import csv to vector

  # import data & transformations (from library) - sample ticket data
  df <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\Sample Ticket Data.csv")
  df$Created <- as.character(df$Created) %>%
    as.POSIXct(format="%m/%d/%Y %H:%M")
  
  
  sender <- "@gmail.com"
  recipients <- c("@gmail.com")
  send.mail(from = sender,
            to = recipients,
            subject = "Subject of the email",
            body = "Body of the email",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = sender,            
                        passwd = "", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
  
  # import csv list, rename column, convert to char vector
  importList <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\test list.csv", header = F)
  names(importList) <- "person.list"
  personList <- as.character(importList$`person List`)
  
  persons <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\test list.csv", header = F)$V1
  