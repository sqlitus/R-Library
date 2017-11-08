#### R LIBRARY ####
#### Help ####

# search all help pages mentioning items
help.search(pattern = "optimisation|optimization", fields = c("title","concept"), package = "stats")

#### Log ####

## 7/17/2017 - created Library as normal R script
## ctrl+alt+O to see chapters








#### 11/7/2017 - Kelley Kickstarter Analysis ####

library(readxl)
library(tidyverse)
library(plotly)
kk <- read_excel(path = "C:\\Users\\Chris\\Downloads\\KS_pinstats.xlsx")
str(kk)
kk$`Reward Tiers` <- as.factor(kk$`Reward Tiers`)
kk$`Stretch Goals` <- as.factor(kk$`Stretch Goals`)

ggplot(kk, aes(Goal)) + geom_density()
ggplot(kk, aes(Goal)) + geom_density() + scale_x_continuous(breaks = seq(0,3000,100))
ggplot(kk, aes(`Reward Tiers`, Goal)) + geom_boxplot()
summary(kk$Goal)

summary(kk$Funded)
ggplot(kk, aes(Funded, Goal, color = `Reward Tiers`, size = Backers)) + geom_point()
p <- ggplot(kk, aes(Backers, `Pins Pre-Ordered`, size = `Price per pin`)) + geom_point(alpha = .5)
ggplotly(p)
ggplot(kk, aes(Funded, Backers)) + geom_point(size = 12, alpha = .5, color = "blue")

ggplot(kk, aes(`Campaign Length (days)`, Backers)) + geom_line()
ggplot(kk, aes(`Campaign Length (days)`, Backers)) + geom_bar(stat = "identity")
ggplot(kk, aes(Backers)) + geom_histogram()

ggplot(kk, aes(`Stretch Goals`)) + geom_bar()

cor(kk$Goal, kk$Funded)

kk$funded.beyond.goal <- kk$Funded/kk$Goal * 100
ggplot(kk, aes(kk$`Stretch Goals`, kk$funded.beyond.goal)) + geom_bar(stat = "identity")
ggplot(kk, aes(funded.beyond.goal, fill = `Stretch Goals`)) + geom_histogram()



#### 10/3/2017 - !! more regex - extract defect number, import csv of valid stores / regions & find ####
text <- data_frame(text = c("lane down due to defect # 4444", 
                            "issue with scale, DEFECT 4444",
                            "lane down due to defect. the new defect 4444",
                            "pumps and bumps #1234"),
                   title = c("R10 blah blah MW LOL", "mPos NE CIR stuff doing things", 
                             "lane down region MA PRD house", "SP 10101 scale broke"))


region <- data_frame(region = c("NE", "MA", "SP", "MW"))

text %>%
  mutate(defect.num = str_extract(str_extract(text$text, "(?i)defect.{0,5}\\d\\d\\d\\d"), "\\d\\d\\d\\d"),
         smart.region = str_extract(text$title, paste0("(",region$region, ")", collapse = "|")))

paste0("(", region$region, collapse = "|")


#### 9/10/2017 - POSIX dates & timezone conversions ####

# import data & transformations (from library) - sample ticket data
df <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\Sample Ticket Data.csv")
df$Created <- as.character(df$Created)
# next two import the given time as the timezone
df$Created.utc <- as.POSIXct(df$Created, tz="UTC", format="%m/%d/%Y %H:%M")
df$Created.cdt <- as.POSIXct(df$Created, tz="America/Chicago", format="%m/%d/%Y %H:%M")
# change a timezone and change the 'time' reflected (new col example)
df$created.utc.to.cdt <- df$Created.utc
attributes(df$created.utc.to.cdt)$tzone <- "America/Chicago"

# confirmed - timezone conversion proper 5 or 6 hours for DLS start/end
df2 <- df %>%
  select(ID, contains("create")) %>%
  mutate(datediff = difftime(Created.cdt, Created.utc, "hours")) %>%
  arrange(Created.cdt)

#### 9/10/2017 - Dplyr deeper exercises; window functions & more ####

# (refer to cheat sheets for dplyr / data wrangling)

# import data & transformations (from library) - sample ticket data
df <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\Sample Ticket Data.csv")
df$Created <- as.character(df$Created)
df$Created <- as.POSIXct(df$Created, format="%m/%d/%Y %H:%M")
df$Priority <- paste("P",df$Priority, sep = "")
df$Priority <- as.factor(df$Priority)
df$Created.Week <- as.Date(cut(df$Created, breaks = "week", start.on.monday = T))

# mutate, arrange, group_by - window functions example. row_number, count, n_distinct, if_else. Avg # per group
df.2 <- df %>%
  select(ID, Created, Created.Week, Store) %>%
  mutate(ticketTotal = n()) %>%
  mutate(origRowNum = row_number()) %>%
  mutate(numStores = n_distinct(Store)) %>%
  mutate(numWeeks = n_distinct(Created.Week)) %>%
  mutate(conditional = if_else(Created.Week < "2016-06-01", "Phase 1", "Phase 2")) %>%
  mutate(avgTicketsPerWeek = ticketTotal / numWeeks) %>%
  mutate(avgTicketsPerStore = ticketTotal / numStores) %>%
  mutate(middleOfPeriod = median(Created.Week)) %>% # median gets middle of rows's value, NOT the middle value of that column. aggregate it first
  group_by(Created.Week) %>% # total by week
  mutate(weekTotal = n())  %>%
  arrange(Created.Week) %>%
  mutate(weekNum = row_number()) %>%
  group_by(Store) %>% # total by store
  mutate(storeTotal = n()) %>%
  group_by(Created.Week, Store) %>% # totals by week and store grouping
  mutate(weekStoreTotal = n()) %>%
  arrange(desc(weekStoreTotal)) %>%
  mutate(weekStoreTotalRank = row_number()) %>%
  ungroup() %>%
  arrange(origRowNum)


# summary, RENAME
df.2.summary <- df.2 %>%
  group_by(Store) %>%
  summarise(storeCount = n()) %>%
  rename(COUNT = storeCount)

# summary & mutate - summary & vector functions. week total -> avg tickets per week
df.2.summary.weeks <- df.2 %>%
  group_by(Created.Week) %>%
  summarise(weekTotal = n()) %>% # summarise gets rid of other columns
  ungroup() %>%
  mutate(weekAvg = mean(weekTotal)) %>%
  mutate(weekSd = sd(weekTotal)) %>%
  mutate(weekVar = var(weekTotal)) %>%
  mutate(weekMax = max(weekTotal)) %>%
  mutate(weekMin = min(weekTotal)) %>%
  mutate(weekIQR = IQR(weekTotal)) %>%
  mutate(weekFirst = first(Created.Week)) %>%
  mutate(cumMean = cummean(weekTotal)) %>%
  mutate(cumSum = cumsum(weekTotal)) %>%
  mutate(middleWeek = median(Created.Week)) %>% # median is true middle if odd n(), averaged middle if even n()
  mutate(firstWeek = first(Created.Week)) %>%
  mutate(lastWeek = last(Created.Week)) %>%
  mutate(dayRange = lastWeek - firstWeek) %>%
  mutate(topDayRange = lastWeek - middleWeek) %>%
  mutate(botDayRange = middleWeek - firstWeek) %>%
  slice(-48) %>%
  mutate(newMiddleWeek = median(Created.Week)) # median is true middle if odd n(), averaged middle if even n()
## take off last col then find median again


# extract - top/bot store count per week
df.2.top2.store.per.week <- df.2 %>%
  arrange(Created.Week, desc(weekStoreTotal)) %>%
  filter(weekStoreTotalRank <= 2)
df.2.bot2.store.per.week <- df.2 %>%
  arrange(Created.Week, desc(weekStoreTotalRank)) %>%
  filter(weekStoreTotalRank > 2)

# extract - data for first x weeks of data, "top x", dense rank .... [order by date], dense_rank created.week, filter first 2 created.week
df.2.top2.weeks <- df.2 %>%
  arrange(Created) %>%
  mutate(weekOrder = dense_rank(Created.Week)) %>%
  filter(weekOrder <= 2)

# extract - distinct values of ... something
df.2.distinct.store <- df.2 %>%
  distinct(Store)

# extract - random sample of rows, by # or %, with/without replacement
df.2.rand.num.samp <- df.2 %>%
  sample_n(5) %>%
  arrange(origRowNum)
df.2.rand.pct.samp <- df.2 %>%
  sample_frac(.1) %>%
  arrange(origRowNum)

# extract - slice which # rows I want
df.2.slice <- df.2 %>%
  slice(5:10)
df.2.slice <- df.2 %>%
  slice(c(5,10,15))    

# extract - top n
df.2.order.rownum <- df.2 %>%
  top_n(2, origRowNum)
df.2.order.Weeks <- df.2 %>% # top_n on repeated row value is not working
  group_by(Created.Week) %>%
  arrange(Created.Week) %>%
  top_n(1, Created.Week)

# group by aggregation; summary functions
df.2.count <- df.2 %>% 
  group_by(Store) %>% 
  summarise(count = n())

# select columns based on literal string OR regular expression
df.2.sel.cols <- df.2 %>%
  select(contains("store"))
df.2.sel.cols2 <- df.2 %>%
  select(contains("."))
df.2.sel.cols3 <- df.2 %>%
  select(matches("."))
df.2.sel.cols4 <- df.2 %>%
  select(matches("[tasdf]o"))

#### 9/9/2017 - Dplyr first look & sqldf workaround ####


# find term sums / number of time period for average per period
# find term sum in period / total term sums for term % during period.
df.weekly.store.total <- sqldf("select df.[Created.week], store, count(*) as weekStoreTotal, avg(*) as weekStoreAvg 
                               from df group by [created.week], store")

df.weekly.total <- sqldf("select df.[created.week], count(*) as weekTotal from df group by [created.week]")

df.store.total <- sqldf("select df.store, count(*) as overallStoreTotal, df2.totalCount,
                        round(cast(count(*) as float) / cast(df2.totalCount as foat), 2) as baseStorePercent 
                        from df join (select count(*) as totalCount from df) df2 group by store")

df.calcs <- sqldf("select df.*, df2.weekStoreTotal, df3.weekTotal, 
                  round(cast(df2.weekStoreTotal as float) / cast(df3.weekTotal as float), 2) as 'weeklyStorePercent'
                  , df4.overallStoreTotal, df4.totalCount, df4.baseStorePercent
                  from df join [df.weekly.store.total] df2 on df.[created.week] = df2.[created.week] and df.[store] = df2.store
                  join [df.weekly.total] df3 on df.[created.week] = df3.[created.week]
                  join [df.store.total] df4 on df.store = df4.store")

# confirm
df.test <- sqldf("select * from [df.calcs] order by created")
# compare weekly store % to base store %, see what is a certain pct above normal.....
# sort & plot those  



# test mutate & filter
df.t2 <- df.test %>%
  mutate(newcol = round(overallStoreTotal / totalCount, digits = 3)) %>%
  # filter(weeklyStorePercent > newcol * 1.7 & Store == "Lamar") %>%
  mutate(prev = lag(Location, 1, order_by = Created.Date)) %>%
  mutate(nextcol = lead(Location, 1, order_by = Created.Date))

# group by
df.t2 <- df.test %>%
  count(Store)
df.t2 <- df.test %>%
  group_by(Store)
df.t2 <- df.test %>%
  filter()

# window aggregate
df.t2 <- df.test %>%
  filter(weeklyStorePercent > mean(weeklyStorePercent))


## Dplyr w/ window function aggregates. NOTE: USE RANK-BASED AGGREGATES BEFORE GROUPING
df.t2 <- df.test %>%
  select(ID, Store, Created.Week, Created.Date, Location) %>%
  group_by(Store) %>%
  mutate(storeTotal = n()) %>%
  group_by(Created.Week, Store) %>%
  mutate(storeWeekTotal = n()) %>%
  mutate(prev = lag(Location, 1, order_by = Created.Date)) %>%
  mutate(nextcol = lead(Location, 1, order_by = Created.Date))

df.t4 <- df.test %>%
  select(ID, Store, Created.Week, Created.Date, Location) %>%
  group_by(Store) %>%
  mutate(prev = lag(Location, 1, order_by = Created.Date)) %>%
  mutate(nextcol = lead(Location, 1, order_by = Created.Date))

df.t5 <- df.test %>%
  select(ID, Store, Created.Week, Created.Date, Location) %>%
  mutate(prev = lag(Location, 1, order_by = Created.Date)) %>%
  mutate(nextcol = lead(Location, 1, order_by = Created.Date))
group_by(Store) %>%
  
  
  df.t3 <- df.test %>%
  select(ID, Store, Created.Week, Created.Date, Location) %>%
  mutate(prev = lag(Location, 1, order_by = Created.Date)) %>%
  mutate(nextcol = lead(Location, 1, order_by = Created.Date))

# group by - does not change the data - groups in memory for aggregations
df.t2 <- df.test %>%
  select(ID, Store, Created.Week) %>%
  group_by(Store) %>%
  group_by(Created.Week) %>%
  ungroup()
df.t3 <- df.test %>%
  select(ID, Store, Created.Week) 
identical(df.t3, df.t2)


#### 8/12/2017 Text mining vignette ####

data("crude")
tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))


dtm <- DocumentTermMatrix(crude,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))

dtm2 <- t(tdm)

inspect(tdm[202:205, 1:5])
inspect(tdm[c("price", "texas"), c("127", "144", "191", "194")])
inspect(dtm[1:5, 273:276])


# convert the wfm to a dataframe
install.packages("tidytext")
library(tidytext)
d <- tidy(dtm2)









#### 7/22/2017 - POLAR COORDINATES ####
t <- seq(0,10, len=100)  # the parametric index
# Then convert ( sqrt(t), 2*pi*t ) to rectilinear coordinates
x = sqrt(t)* cos(2*pi*t) 
y = sqrt(t)* sin(2*pi*t)


plot(sample(50))

help(plot)
plot(df,y)


plot(df,y, type="b")
plot(df,y, type = "c")

polar2cart<-function(df,y,dist,bearing,as.deg=FALSE){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing
  ## as.deg indicates if the bearing is in degrees (T) or radians (F)
  
  if(as.deg){
    ##if bearing is in degrees, convert to radians
    bearing=bearing*pi/180
  }
  
  newx<-x+dist*sin(bearing)  ##X
  newy<-y+dist*cos(bearing)  ##Y
  return(list("x"=newx,"y"=newy))
}


##Example

oldloc=c(0,5) 
bearing=200 #degrees
dist=5

newloc<-polar2cart(oldloc[1],oldloc[2],dist,bearing,TRUE)
plot(oldloc[1],oldloc[2],xlim=c(-10,10),ylim=c(-10,10))
points(newloc$x,newloc$y,col="red")







#### 7/21/2017 - !! Sample Ticket Data - INITIAL TEXT MINING RESEARCH !!####

# import packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr"))



## Import & Clean data process. Convert char to datetimes. Convert text factors to char. Create calculated columns with datetime arithmatic.

df <- read.csv("D:\\Work\\Libraries\\R Library\\Data\\Sample Ticket Data.csv")

# altering strings and converting to factors
df$Priority <- paste("P",df$Priority, sep = "")
df$Priority <- as.factor(df$Priority)
# df$Priority <- gsub(" ", "", df$Priority)

# convert the string dates to datetimes -> convert to character, then match the format of the character field to time values
df$Created <- as.character(df$Created)
df$Created <- as.POSIXct(df$Created, format="%m/%d/%Y %H:%M")
df$Resolved <- as.character(df$Resolved)
df$Resolved <- as.POSIXct(df$Resolved, format="%m/%d/%Y %H:%M")
df$FirstAssigned <- as.character(df$FirstAssigned)
df$FirstAssigned <- as.POSIXct(df$FirstAssigned, format="%m/%d/%Y %H:%M")

# skip
function(date.conversion.and.datediffs){
  # Convert to simple date and find datediff
  df$Created.Date <- as.Date(df$Created,format = "%m/%d/%Y")
  df$Resolved.Date <- as.Date(df$Resolved,format = "%m/%d/%Y")
  df$Time.To.Response <- df$FirstAssigned - df$Created
  df$Days.To.Resolve <- df$Resolved - df$Created
}

# Time Difference in units = minutes
df$Time.To.Response <- difftime(df$FirstAssigned, df$Created, units = "hours")
df$Time.To.Restore.Service <- difftime(df$Resolved, df$Created, units = "hours")
df$Assigned.To.Resolve <- difftime(df$Resolved, df$FirstAssigned, units = "hours")


# week and month values of dates - for POSIX dates
df$Created.Week <- as.Date(cut(df$Created,
                               breaks = "week",
                               start.on.monday = T)) # changes weekly break point to Monday
df$Created.Week2 <- as.Date(cut(df$Created,
                                breaks = "week",
                                start.on.monday = F)) # changes weekly break point to Sunday


df$Created.Month <-as.Date(cut(df$Created, breaks = "month"))




## 8/12/2017 Text mining sample data - append document term matrix to original data frame ##

function(REFERENCES){
  # https://stackoverflow.com/questions/44014097/convert-document-term-matrix-dtm-to-data-frame-r-programming
  # https://stackoverflow.com/questions/26711423/how-can-i-convert-an-r-data-frame-with-a-single-column-into-a-corpus-for-tm-such
  # https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus
}

# convert title column to character, isolate title column in data frame, convert to data frame, change title, remove all punctuation
df$Title <- as.character(df$Title)
df.titles <- as.data.frame(df[,c("Title")], col.names = "Title")
colnames(df.titles) <- "Title"
df.titles$Title <- gsub("[^a-zA-Z0-9 ]"," ",df.titles$Title)

# turn sliced dataframe to corpus, then get document term matrix
corpus <- Corpus(DataframeSource(df.titles))
dtm <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))


# !!! convert dtm to matrix, create flags, and append to orig data frame
word.m <- as.matrix(dtm)
word.m <- as.data.frame(word.m)
word.m[word.m > 1] <- 1
df.w.words <- cbind(df,word.m)

# write output - to test size
write.csv(df.w.words, file = "D:\\Work\\Libraries\\R Library\\Data\\sample data with words.csv")







## Document Term Matrix - Long Dataframe for EDA ##

ticketsandtitles <- df[,c("ID","Title","Location")]
ticketsandtitles$Title <- gsub("[^a-zA-Z0-9 ]"," ",ticketsandtitles$Title)

# long dataframe with ids, words, counts, and flags for more text-specific analysis
corpus2 <- VCorpus(DataframeSource(ticketsandtitles), readerControl = list(reader = readTabular(mapping = list(content = "Title", id = "ID", Location = "Location"))))
dtm2 <- DocumentTermMatrix(corpus2,
                           control = list(removePunctuation = TRUE,
                                          stopwords = TRUE))
dtm.df <- tidy(dtm2)
dtm.df$flag <- ifelse(dtm.df$count > 0, 1, 0)

# additional summary tables
dtm.df.withloc <- sqldf("select [dtm.df].*,ticketsandtitles.Location 
                        from [dtm.df] join ticketsandtitles on [dtm.df].document = ticketsandtitles.ID")


location.sum <- sqldf("select Location, term, count(*) as 'total'
                      , sum(count) as 'word.mentions', sum(flag) as 'count2'
                      , count(distinct document) as 'totalTickets', count(*) over(partition by Location) as 'loctictotal' 
                      from [dtm.df.withloc] group by Location, term")

# location word matrix, with ticket sum column
location.sum.matrix <- spread(location.sum, term, total)
location.sum.matrix$location.total <- rowSums(location.sum.matrix)
location.ticket.sum <- sqldf("select Location, count(distinct document) as [numtickets] from [dtm.df.withloc] group by Location", drv = "SQLite")
location.sum.matrix <- sqldf("select a.*, b.numtickets from [location.sum.matrix] a join [location.ticket.sum] b on a.Location = b.Location")


write.csv(dtm.df.withloc, file = "D:\\Work\\Libraries\\R Library\\Data\\tickets and word frequencies.csv", row.names = FALSE)
write.csv(location.sum.matrix, file = "D:\\Work\\Libraries\\R Library\\Data\\Locations and Counts.csv", row.names = FALSE)




# !!! long dataframe with all other datapoints

# subset dataframe selecting only columns that are factors or dates. subset dataframe by column type
z.df.prep <- df[,sapply(df,is.factor) | sapply(df,function(x){inherits(x, 'Date')})]

# subset dataframe by columns not in given list
df[,-which(names(df) %in% c("Title", "Created.Week"))]


dtm.full <- sqldf("select [dtm.df].*, [z.df.prep].*
                  from [dtm.df] join [z.df.prep] on [dtm.df].document = [z.df.prep].ID")

write.csv(dtm.full, file = "D:\\Work\\Libraries\\R Library\\Data\\dtm full.csv", row.names = FALSE)


# inspect corpus elements
corpus2[[1]]
corpus2[[1]]$content
corpus2[[1]]$meta
meta(corpus2[[1]])


# next steps - get # of columns we want from matrix (top x occurring words)

# need analysis on most frequent words (by doc occurrence)
# need to see which docs match a particular word
# need to be able to slice and dice based on word, and other fields from base dataset.
# need to APPEND dtm to orig df for analysis



#### ggplot2 experimenting ####
# mailR stuff / Rshiny

#do: stacked bar chart, facet bar, color scale by X, point by Y...
# volume
ggplot(df, aes(Created.Month))+geom_bar()

ggplot(df, aes(x=Created.Month, y=Time.To.Response))+stat_summary(fun.y = "mean", geom="bar")+
  facet_grid(facets=Support.Group~.) # rows ~ columns


ggplot(data=df, aes(x=Created.Month)) + geom_bar(aes(fill=..count..))+
  facet_grid(Classification~.)





########### GGPLOT SPELLBOOK ###########

########### Part 1 - Basic Bar Plots; Colors; ####
#### requires Sample Ticket Data - cleaned

# Count
ggplot(df, aes(Created.Month))+
  geom_bar()

# Sum
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  stat_summary(fun.y = "sum", geom = "bar")
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  geom_bar(stat = "identity", fill = "Purple")
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  geom_bar(stat = "summary", fun.y = "sum", fill = "gold")

# Avg
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  stat_summary(fun.y = "mean", geom = "bar")
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue")

# Median
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  stat_summary(fun.y = "median", geom = "bar")
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  geom_bar(stat = "summary", fun.y = "median", fill = "green")


## Custom colors

# 1 - uniform color  -> border / fill
ggplot(df, aes(Created.Month))+
  geom_bar(color = "blue", fill = rgb(.1,.4,.5,.7))

ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  geom_bar(stat = "summary", fun.y = "mean", color = "blue", fill = rgb(.1,.4,.5,.3))+
  scale_x_date(date_breaks = "2 month")

# 2 - hue
ggplot(df, aes(Created.Month, fill = Priority))+ # fill = as.factor(Created_Month_R)
  geom_bar()+scale_fill_hue(c = 40)

# 3 - Rcolorbrewer
ggplot(df, aes(Created.Month, fill = Support.Group))+geom_bar()+
  scale_fill_brewer(palette = "Set1")

# 4 - greyscale
ggplot(df, aes(Created.Month, fill = Priority))+geom_bar()+
  scale_fill_grey(start = .25, end = .75)

# 5 - manual
ggplot(df, aes(Created.Month, fill = Priority))+geom_bar()+
  scale_fill_manual(values = c("red","orange","blue","green","black"))



###  Minor Tweaks

# Facet
ggplot(df, aes(x = Created.Month, y = Time.To.Restore.Service, color = Created.Month))+
  stat_summary(fun.y = "mean", geom = "bar")+
  facet_wrap(~Support.Group)


# label intervals
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  stat_summary(fun.y = "sum", geom = "bar")+
  scale_x_date(date_breaks = "2 month")

# Missing dates are filled in by default; alter width
df.sample <- df[sample(nrow(df), 11),]
ggplot(df.sample, aes(Created.Month))+geom_bar()
ggplot(df.sample, aes(Created.Month))+
  geom_bar(fill = "pink", width = 22, color = "black")


# Extras 1 - remove legend, change labels, flip axis (horizontal bar plot), bar width
ggplot(df, aes(Priority, fill = Priority))+geom_bar(width = 1)+
  theme(legend.position = "none")+
  labs(x = "my x axis", y = "my y axis", title = "my title", ggtitle = "ggtitle here?")+
  coord_flip()



########### Part 2 - Stacked / Grouped bars ####
# Stacked Bar
ggplot(df, aes(x = Created.Week, fill = as.factor(Priority)))+
  geom_bar()

# Stacked Percent
ggplot(df, aes(x = Created.Week, fill = as.factor(Priority)))+
  geom_bar(position = "fill")

# Grouped Bar
ggplot(df, aes(x = Created.Week, fill = Priority))+
  geom_bar(position = "dodge")

# Add RcolorBrewer
ggplot(df, aes(x = Created.Week, fill = Priority))+
  geom_bar(position = "fill") + scale_fill_brewer(palette = "Pastel1")

# Faceting
ggplot(df, aes(x = Created.Week, fill = Priority))+
  geom_bar()+facet_wrap(~Support.Group)





#### Master template
ggplot(df, aes(Created.Month, Time.To.Restore.Service))+
  geom_bar(stat = "summary", fun.y = "mean", fill = "blue")+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week", date_labels = "%Y-%b")+
  facet_grid(Support.Group~.)+
  xlab("Month")+
  ylab("Avg Time Restore Service")+
  ggtitle("MTRS by SG and month")+
  theme_classic()


# do next: grep, regexp, get factor month name from date!

#### Nulls, NAs ####

# 7/18/2017

x <- c(1,2,3)
y <- c(5,6,NA)
t <- data.frame(x,y)

sqldf("select count(*) from t")
sqldf("select sum(y) from t")
sqldf("select avg(y) from t")
# sqldf aggregates properly

mean(x)
mean(y)
mean(y, na.rm = T)
sum(y)
sum(y, na.rm = T)
# NAs MUST BE ACCOUNTED FOR BEFORE PERFORMING AGGREGATES ON A SOLE VECTOR

z <- c(7,NULL,8)
z
y
length(z)
length(y)
# NULLS ARE NOT INCLUDED IN THE VECTOR OR ITS LENGTH. NAs are.
sum(z)
mean(z)
# HOWEVER, NULLS ARE PROPERLY EXCLUDED FROM AGGREGATE FUNCTIONS

# DROP A DATAFRAME COLUMN -> SET IT TO NULL
t$y <- NULL
t

#### Pluralsight - R Fundamentals ####

## R Programming Fundamentals

getwd()
save.image("R Fundamentals")
load("D:/RStudio/R Studio Files/Practice/R Fundamentals")
# everything workspace: http://www.statmethods.net/interface/workspace.html


# Getting Help


# Web Search
RSiteSearch("mean")
RSiteSearch("{mean}") # case sensitive

# Internal search
library(sos)
findFn("{arithmatic mean}")
findFn("{arithmatic mean}",maxPages = 2)
???"{arithmatic mean}"(2)

# Search engine tips
# Use [R] instead of R
# Mailing Lists
# Asking questions



## Variables & Operators


# use brackets for sqldf variable names;
format(10^5, scientific = FALSE)

# Arithmatic functions
10%%3
10%/%3 #integer division
10/3

# Mathematical functions
abs(-5)
log(2)
log(2,base=10)
exp(5)
factorial(5)

# Special constants
pi
options()
options(digits = 7)

# Special numbers
Inf
-Inf
NaN # Not a number
NA # not available

# Infinity
is.finite(1/0)
is.infinite(1/0)

# Undefined
is.nan(Inf/Inf)

# Missing Values
is.na(NA)

# NaN is NA, but NA is not NaN
is.na(NaN)
is.nan(NA)

# Logical
1==5
1!=5
!(TRUE)
!TRUE
"b"<"a"

# Vectorized operation
x <- c(1,2,3)
mean(x) # scalar result vector input, scalar output
sqrt(x) # vector input, vector output 
x+x # multiple vector input+output




## Operations on Atomic Vectors


# extract elements
x[1]
x[1:2]
x[x>2]

# Typecasting
as.numeric(x)
as.character(x)
as.integer(x)
as.data.frame(x)
as.ts(x)

# Factors
factor(x)
factor(x,labels = c("One","Two","Three"))
factor(x,levels = c("One","Two","Three")) #levels: all possible
factor(x,levels = c("One","Two","Three"), labels=c("O","T","Th"))

# Lists
names <- c("Chris","Kelley","Puss")
weights <- c(80,50,5)
genders <- factor(c("M","F","F"))

student1 <- list(names[1],weights[1],genders[1])

# Named list
student1 <- list(name=names[1]
                 ,weight=c(weights[1],165)
                 ,gender=genders[1])

# Operations on lists
student1[2]
typeof(student1[1])
student1[[2]]
typeof(student1[[2]])
student1[1:3]
class(student1[1:3])

student1[["name"]]
student1$name
student1[c("name","weight")]



## Data structures pt2


students <- data.frame(names,weights,genders
                       ,stringsAsFactors = FALSE)
typeof(students)
class(students)
str(students)
students

library(sqldf)
sqldf("select names from students")

students[1:3]
students$names
typeof(students$names)
students[c("names")]
typeof(students[c("names")])
students[1,]
students[1:2,2:3]
students[names="Chris",]
sqldf("select * from students where weights>40")

# Arrays
a1 <- c(1,2,3)
a2 <- c(11,22,33)
arraydemo <- cbind(a1,a2)
a3 <- c(50,60,70)
a4 <- c(101,102,103)
arraydemo2 <- cbind(a3,a4)

array <- array(c(arraydemo,arraydemo2)
               ,dim=c(3,2,2))
array[,,2]
array[1,1,]

## Functions


Totals <- function(a,b){
  a+b
}
Totals(a1,a2)
Totals(a=a2,b=a1)

# Function with default arguments
Totals2 <- function(a,b=c(1,2,3)){
  a+b
}
Totals2(a1)
Totals2(a2)
Totals2(a2,a3)

# Function arguments with ellipsis
Totals3 <- function(a,b,...){
  thing <- a+b*sum(...)
  thing
  print(a+b)
}
TotalsFunction <- function(v1,v2,v3,...){
  totals <- v1+v2+v3*sum(...)
  extra <- list(...)
  print(extra)
  totals
}
TotalsFunction(a1,a2,a3,a4)

# Function lazy evaluation
LazyFunction <- function(a,b,c=d){
  d <- a*b
  (a+b)/c
}
LazyFunction(2,4)

# Function - return multiple values
GetMarksSummary <- function(marks1,marks2){
  total.marks <- sum(marks1,marks2)
  avg.marks <- mean(c(marks1,marks2))
  return(list(total = total.marks,average = avg.marks))
}

GetMarksSummary(c(1,2,3),c(50,51,52))
GetMarksSummary(1,50)$total
GetMarksSummary(1,50)[1]

# Functions as objects
GetMarksSummary
formals(GetMarksSummary) 
body(GetMarksSummary)

MyGetMarksSummary <- GetMarksSummary

do.call(GetMarksSummary,list(a1,a2))

# Anonymous functions
do.call(function(marks1,marks2){
  total <- marks1+marks2
  average <- mean(c(marks1,marks2))
  return(list(tot=total,avg=average))
},list(a1,a2))




## R - Flow Control

# If Else
Performance <- function(marks){
  average.marks <- mean(marks)
  print(paste("average marks: ",average.marks))
  if(average.marks>25){
    print("High performance")
  }
  else if(average.marks>15){
    print("Medium performance")
  }
  else{
    print("Overall performance average or less")
  }
  print("Performance test completed")
}

Performance(a3)
Performance(a2)
Performance(a1)


# SWitch
GetMarksSummarySwitch <- function(marks,summary.type){
  result <- switch(summary.type,
                   "mean"={
                     mean(marks)
                   },
                   "median"={
                     median(marks)
                   },
                   "variance"={
                     var(marks)
                   },
                   "Not Implemented"
  )
  result
}

GetMarksSummarySwitch(a3,"mean")
GetMarksSummarySwitch(a3,"blork")


# Vectorized If
a3
a3>50

ifelse(a3>50,c("Yay","Good"),
       c("Keep going","Almost"))

# Repeat
WriteOnNotebook <- function(total.page.count)
{
  count <- 0
  repeat{
    count <- count+1
    if(count>total.page.count){
      print("Page finished")
      break
    }
    if(count %% 2==0){
      print(paste("Skipping page number",count))
      next
    }
    print(paste("writing on page number",count))
  }
}

WriteOnNotebook(10)

# While
WriteOnNotebook.while <- function(total.page.count)
{
  count <- 0
  while(count<total.page.count){
    count <- count+1
    print(paste("Writing on page number",count)) 
  }
  print("Page finished")
}

WriteOnNotebook.while(10)

# For
WriteOnNotebook.for <- function(total.page.count){
  for (count in 1:total.page.count){
    print(paste("writing on page number",count))
  }
  print("Page finished")
}
WriteOnNotebook.for(10)

# Apply
m <- matrix(c(1:10,11:20),nrow=10,ncol=2)
apply(m,1,sum)
apply(m,2,sum)
apply(m,1,max)
apply(m,1,which.max)
colnames(m)[apply(m,1,which.max)]
apply(m,2,mean)

library(dplyr)
m.df <- as.data.frame(m)
summarise(m.df,NewAvgColumn=sum(m.df[,1:2]))
summarise_each(m.df,funs(mean))

rownames(m.df)[apply(m.df,2,which.max)]
apply(m.df,1:2,function(x) x+2)




## Packages


library()
packages <- installed.packages()

# loaded/unload packages
search()
detach(package:sqldf, unload = TRUE)

# Download package if required
if(!require("newpackage"))
{
  #download and install
}

install.package("tidyr")
setRepositories() #which repos. to look at to download

# installing from github
install.packages("devtools")
library(devtools)
# install_github("slidify","ramnathv")

# Managing Packages
update.packages()
update.packages(ask = FALSE)
remove.packages("packagename")



## Importing Data


getwd()
setwd()

# CSV
read.csv(filepath,header=TRUE,sep = ",",quote="\"")

# Table
read.table(filepath,header=FALSE,sep = "",dec="."
           ,colClasses = c("character","factor","numeric"))
## skip number of lines before reading data
## nrows to read as data...

# URL
read.table(url)
download.file(url,localDirectory.Filename)
mydata <- read.table(localDirectory,sep=",")

# XML
library(XML)
mydata <- xmlToDataFrame(file,
                         colClasses = c("integer","string")
                         ,stringsAsFactors=FALSE)

# Excel
# requires java installed...
mydata <- readWorksheetFromFile(file,sheet=1,
                                startRow=2) # or sheet="name"
# does not support factor or integer...?
mydata <- transform(mydata,
                    student.gender=as.factor(student.gender),
                    student.marks=as.integer(student.marks))
# also other excel import files (probably better)


# Can also import spss, stata, minitab, sas
# library(foreign)

# Import package datasets
library(datasets)
data(package="datasets")
data(datasetofchoice)

# Import from database
# any db - rodbc
# install.packages("RODBC")
# library(RODBC)
# connect <- odbcConnect("sqlserverconnection")
# mydata <- sqlQuery(connect,"select * from table")



## Exploring Data


library(datasets)
data(iris)

# Overall structure
str(iris)
head(iris)

# Continuous data - Central Tendency
mean(iris$Sepal.Length)
median(iris$Sepal.Length)

# Continuous data - Spread
range(iris$Sepal.Length)
diff(range(iris$Sepal.Length))

# Continuous data - Spread - Quartiles
summary(iris$Sepal.Length)
summary(iris)

# Continuous data - Spread - Quartiles - Boxplot
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length,horizontal=TRUE)
boxplot.stats(iris$Sepal.Length) #summary,obs,conf?,outliers
boxplot(iris[1:4])

# Continuous data - Spread - Histogram
hist(iris$Sepal.Length)
hist(iris$Sepal.Length
     ,main="Length"
     ,xlab="length")

# Continuous data - Spread - Variance + Standard Deviation
var(iris$Sepal.Length)
sd(iris$Sepal.Length)


# Categorical data - Frequency
table(iris$Species)
barplot(table(iris$Species))
prop.table(table(iris$Species))

# Categorical data - Category Statistics
library(psych)
describeBy(iris,group=iris$Species)

# histogram + boxplot
library(lattice)
histogram(~Sepal.Length | Species,
          data=iris,
          layout=c(1,3),
          col="black")

boxplot(Sepal.Length~Species,
        data=iris)

## ggplot version
iris.df <- iris
iris.df$Species <- factor(iris.df$Species,
                          levels=sort(unique(iris.df$Species),
                                      decreasing=TRUE))

ggplot(
  data=iris.df,
  aes(x=Sepal.Length)
)+geom_histogram(bins = 8)+
  facet_grid(facets=Species~.)+
  theme_bw()

ggplot(data=iris.df, aes(x=Sepal.Length))+
  geom_histogram(bins=8)+
  facet_grid(facets = Species~.)+
  theme(plot.title=element_text(face="bold"))




#### Swirl - EDA ####


#### Data analysis 

### Swirl - EDA - 12: K Means Clustering ###


cmat
points(cx, cy, col = c("red", "orange", "purple"), pch = 3, cex = 2, lwd = 2)
mdist(x,y,cx,cy)
which.min(distTmp, 2, which.min)
which(distTmp, 2, which.min)
history()
sapply(distTmp, 2, which.min)
sapply(distTmp, 2, which.min())
info()
apply(distTmp, 2, which.min)
points(x,y, pch = 19, cex = 2, col = cols1[newClust])
tapply(x, newClust, mean)
tapply(y, newClust, mean)
points(newCx, newCy, col = cols1, pch = 8, cex = 2, lwd = 2)
mdist(x,y,newCx,newCy)
apply(distTmp2,2,which.min)
points(x,y,pch = 19, cex =2, col = cols1[newClust2])
tapply(x,newClust2,mean)
tapply(y,newClust2,mean)

install_from_swirl("Exploratory Data Analysis")


# 11/5/2016
# Data analysis - 1

myMPG <- cars$mpgCity
mean(myMPG)
median(myMPG)
mode(myMPG)
table(myMPG)


# html shiny
library(shiny)
names(tags)

install_from_swirl("Getting_and_Cleaning_Data")
install_from_swirl("Regression_Models")

# 11/8/2016
# Data analysis - 2 
range(cars$price)
max(cars$price)-min(cars$price)
var(cars$price)
sd(cars$price)



# The importance of spread / dispersion
rnorm(100)
rnorm.2 <- function(n,mean,sd){mean+sd*scale(rnorm(n))}

r1 <- rnorm.2(100,4,1)
summary(r1)

r2 <- rnorm.2(500,4,1)
summary(r2)
boxplot(r1)
boxplot(r2)
boxplot(c(r1,r2))
hist(r1)
hist(r2)


# Want to learn?
# Cool shit to master

# web scraping
# scrape text
# loop through webpages
# then text analysis with NLP
# visualized with ggvis/more ggplot
# then put into a repeatable dashboard with shiny
# rmarkdown shiny


install.packages("rvest")


# 11/19/2016 - data cleaning tidyr script




# Swirl - Base Package 
{r}

#### SWirl - Base Package ####



# Swirl Training + Reference

# Workspace reference
# http://www.statmethods.net/interface/workspace.html

install.packages("swirl")
install.packages("mtcars")

install.packages("WWWusage")


data <-
  read.csv("C:/Users/Kellis/Downloads/userssharedsdfleaachvrsltsassessrlasy201112.csv")

summary(data)

install.packages("ggplot2")

??ggplot2
methods(plot)

ggplot2(data=data, aes(x=stnam, y=leanm11)) +
  geom_bar(stat="identity")

demo()




## 8/21/2016
## Lesson 2: Workspace and Files
## DIRECTORIES

getwd()
setwd("C:/Users/Kellis/Documents/R/Practice")
ls()
dir()
dir.create("testdir")
file.create("mytest.R")
file.exists("mytest.R")
file.info("mytest.R")
## case sensitive
dir.create(file.path("testdir2","testdir3"),recursive=TRUE)

## save settings from before analysis, then reset to them at end
unlink("testdir",recursive=TRUE)



## 8/24/2016
## Lesson 3: Sequences of Numbers
1:20
pi:10
15:1
?`:`

seq(1,20)
seq(0,10,by=.5)
my_seq<-seq(5,10,length=30)
length(my_seq)
my_seq
1:length(my_seq)
seq(along.with=my_seq)
seq_along(my_seq)
rep(0,times=40)
rep(c(0,1,2),times=10)
rep(c(0,1,2),each=10)
1
4

## Lesson 4: Vectors?
num_vect<-c(.5,55,-10,6)
tf<-num_vect<1
1
tf
num_vect>=6
my_char<-c("My","name","is")
my_char
paste(my_char,collapse=" ")
my_name<-c(my_char,"Chris")
my_name
paste(my_name,collapse = " ")
paste("Hello","world!",sep=" ")
paste(c(1:3),c("X","Y","Z"),sep = "")
paste(LETTERS,1:4,sep="-")

# 8/25/2016
# Lesson 5: Missing Values
x<-c(44,NA,5,NA)
x*3
y<-rnorm(1000)
z<-rep(NA,1000)
my_data<-sample(c(y,z),100)
my_na<-is.na(my_data)
my_na
my_data==NA
sum(my_na)
my_data
0/0
Inf-Inf

# Lesson 6: Subsetting a Vector
x[1:10]
# returns all nulls
x[is.na(x)]
y <- x[!is.na(x)]
y
3
y[y>0]
x[x>0]
x[!is.na(x)&x>0]
# R uses 1 based indexing
x[c(3,5,7)]
x[0]
x[3000]
x[c(-2,-10)]
x[-c(2,10)]
vect <- c(foo=11,bar=2,norf=NA)
vect
names(vect)
vect2 <- c(11,2,NA)
names(vect2) <- c("foo","bar","norf")
identical(vect,vect2)
1
1
vect["bar"]
vect[c("foo","bar")]



# Clear variables in workspace
library(swirl)
ls()
rm(list=ls())


# 8/29/2016
# Lesson 7: Data Frames and Matricies

my_vector <- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4,5) # gives the vector 4 rows 5 columns
attributes(my_vector)
my_vector
class(my_vector)
my_matrix <- my_vector
my_matrix2 <- matrix( data=c(1:20), nrow=4, ncol=5 )

# check if two arguments/variables are exact same
identical(my_matrix,my_matrix2)
patients <- c("Bill","Gina","Kelly","Sean")

#combine columns
cbind(patients,my_matrix)

#convert to data frame - preserves data types of matrixes / vectors
my_data <- data.frame(patients,my_matrix)
my_data
class(my_data)
cnames <- c("patient","age","weight","bp","rating","test")
?colnames
colnames(my_data) <- cnames
my_data


## 9/10/2016
## Lesson 8: Logic
TRUE==TRUE
(FALSE==TRUE)==FALSE
6==7
6<7
10<=10
5!=7 # NOT == !
!(5==7)
FALSE & FALSE
# && ONLY EVALUATES FIRST MEMBER OF A VECTOR
TRUE & c(TRUE,FALSE,FALSE)
TRUE && c(TRUE,FALSE,FALSE)
# | evaluates whole vector, || evaluates first memeber of vector
TRUE | c(TRUE,FALSE,FALSE)
TRUE || c(TRUE,FALSE,FALSE)
5 > 8 || 6 != 8 && 4 > 3.9
#THE TWO CONDITIONS ON EITHER SIDE OF AND EVALUATED BEFORE OR
isTRUE(6>4)
identical('twins','twins')
xor(5==6,!FALSE) # EXCLUSIVE OR EVALUATES TO TRUE IF CONTIANS 1. FALSE
2.TRUE
ints <- sample(10) # random sampling of integers from 1 to 10
ints
ints > 5
which(ints>7) # evaluates which elements are true (gives indicies)
any(ints<0) #evaluates if ANY elements are true
all(ints>0) #evaluates if ALL elements are true

# FOLLOW UP - LOOK AT CONTROL FLOW FOR MORE LOGIC


# 9/13/2016
# Lesson 9: Functions

library(swirl)
ls()
rm(list=ls())
swirl()
Sys.Date()
mean(c(2,4,5))

options(editor = "internal") #fix the script editor?

boring_function('My first function!')
boring_function
my_mean(c(4,5,10))
remainder(5)
remainder(11,5)
remainder(divisor=11,num=5)
remainder(4,div=2)
args(remainder)
evaluate(stdev(),c(1.4,3.6,7.9,8.8))
evaluate(sd,c(1.4,3.6,7.9,8.8))
evaluate(function(x){x+1},6)
evaluate(function(x){first(x)},c(8,4,0))
evaluate(function(x){x[1]},c(8,4,0))
evaluate(function(x){x[length(x)]},c(8,4,0))
?paste
paste("Programming","is","fun!")
telegram("girl from walgreens")
mad_libs("pla","adj","nou")






# Lesson 10: lapply and sapply





# 10/31/2016

# 11 - vapply and tapply

vapply(flags,unique,numeric(1))
ok()
sapply(flags, class)
vapply(flags, class, FUN.VALUE = character(1))
?tapply
table(flags$landmass)
table(flags$animate)
tapply(flags$animate,flags$landmass,mean)
tapply(flags$population,flags$red,summary)
tapply(flags$population,flags$landmass,summary)

tapply(flags$population,flags$zone,sum)
library(sqldf)
sqldf("select zone, sum(population) from flags group by zone")
sqldf("select avg(population),red from flags group by red")

sapply(flags, unique)
sapply(flags,length)

# !!!! Number of unique values in each column
sapply(flags,function(x){length(unique(x))})
apply(flags, 2, function(x){length(unique(x))})
# !!!! USE CUSTOM FUNCTIONS FOR sAPPLY & OTHERS



# Uniques with sql. Table function
sqldf("select distinct landmass, sum(population) from flags
      group by landmass")
sqldf("select landmass, sum(population) from flags
      group by landmass")

sqldf("select botright, count(*) from flags group by botright")
sqldf("select count(distinct botright) from flags")
length(unique(flags$botright))


table(flags$landmass)
table(flags$landmass,flags$botright)


# More summary stats
library(psych)
describeBy(flags$population)




# 11/1/2016

# 12: Looking at data; inspection; inspecting data
ls()
class(plants)
dim(plants)
nrow(plants)
ncol(plants)
object.size(plants)
names(plants)
head(plants)
head(plants, 10)
tail(plants, 15)
summary(plants)
table(plants$Active_Growth_Period)
str(plants)


# 11/2/2016
# 13: Simulations

?sample
sample(1:6,4, replace = TRUE)
sample(1:20,10)


LETTERS
sample(LETTERS)
flips <- sample(c(0,1),100,replace = TRUE,prob = c(.3,.7))
flips
sum(flips[flips==1])
sum(flips)
x <- c(1,2,3,4,5,1,2)
sum(x[x==3])
sum(c(1,2,3,1,2)[c(1,2,3,1,2)==2])
#sum where lips=x

?rbinom
rbinom(1,100,.7)
flips2 <- rbinom(100,1,.7)
flips2
rbinom(1,5,.7)
# multi trials
sum(flips2)
?rnorm
rnorm(10)
rnorm(10,100,25)
?rpois
rpois(5,10)
my_pois <- replicate(100,rpois(5,10))
my_pois
cm <- colMeans(my_pois)
hist(cm)


# 11/3/2016
# 14 - Dates and Times

d1 <- Sys.Date()
class(d1)
unclass(d1)
d1
d2 <- as.Date("1969-01-01")
unclass(d2)
class(d2)
t1 <- Sys.time()
t1
class(t1)
unclass(t1)
t2 <- as.POSIXlt(Sys.time())
class(t2)
t2
unclass(t2)
str(unclass(t2))
t2$min
weekdays(d1)
months(t1)
quarters(t2)
t3 <- "October 17, 1986 08:24"
t4 <- strptime(t3,"%B %d, %Y %H:%M")
t4
class(t4)
Sys.time()>t1
Sys.time()-t1
difftime()
difftime(Sys.time(),t1,units = 'days')
# CHECK OUT LUBRIDATE PACKAGE FOR MORE DATE STUFF

# 11/4/2016
# 15 - Base graphics
data(cars)
?cars
head(cars)
plot(cars)
?plot

plot(x=cars$speed,y=cars$dist)
plot(x=cars$dist,y=cars$speed)
plot(x=cars$speed,y=cars$dist,xlab="Speed"
     ,ylab = "Stopping Distance")
plot(cars,main="My Plot")
?plot
plot(cars,sub="My Plot Subtitle")
?par
plot(cars,col=2)
plot(cars,xlim=c(10,15))
?points
plot(cars,pch=2)
data(mtcars)
nxt()
?boxplot
boxplot(formula = mpg~cyl, data=mtcars)
hist(mtcars$mpg)



#### 7/15/2017 - word frequency matrix ####
{r}
library(qdap)

# check ?wfm for more examples


## Not run: 
## word frequency matrix (wfm) example:
DATA

a <- wfm(DATA$state, list(DATA$sex, DATA$adult))
b <- with(DATA, wfm(state, person))
b.2 <- as.data.frame(b)
identical(a,b)

with(DATA, wfm(state, list(sex, adult, person)))[1:15, ]

Filter(with(DATA, wfm(state, list(sex, adult))), 5)
with(DATA, wfm(state, list(sex, adult)))



#### PREV CODE BEFORE 7/15/2017; Sample ticket data ####

# PREV CODE BEFORE 7/15/2017

# date format field
sample.data <- read.csv("D:/Work/Sample Ticket Data.csv")
sample.data$Created <- as.Date(sample.data$Created,format = "%m/%d/%Y")

# plot with counts
ggplot(sample.data,aes(x=Store))+geom_bar()
ggplot(sample.data,aes(x=Store))+geom_bar()+theme_classic()
ggplot(sample.data,aes(x=Store))+geom_bar()+theme_minimal()
ggplot(sample.data,aes(x=Store))+geom_bar()+theme_void()


# Create the data frame
element <- rep("apples", 5)
element
qty <- c(2, 1, 4, 3, 6)
qty
category <- c("Red", "Green", "Red", "Green", "Yellow")
d <- data.frame(element=element, qty=qty, category=category)
d
# chart with sums
ggplot(d, aes(x=category, y=qty)) + geom_bar(stat = "identity")



# 6/27/2017
# practice understanding functions, documentation

x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(1,1,1,2,2,2,3,4,5,11)

mean(x)
mean(x,trim = .5)

mean(y)
mean(y,.5)
mean(y,trim=.5)



# subset remove nulls
z <- c(1,1,1,2,2,2,3,4,5,NA,6)
z.1 <- !is.na(z)
z.2 <- z[!is.na(z)]
mean(z)
mean(z,na.rm = FALSE)
mean(z,na.rm = TRUE)
mean(z.2)


library(sqldf)

#  ud functions

zfunction <- function(a,b=5){
  a+b
}

zfunction(1,2)

zfunction(a = 4)


# returns last "returN" item, or last line
zfunction2 <- function(a,b,...){
  x <- a+b+sum(...)
  print(a)
  # return(x)
  extra <- list(...)
  print(extra)
  print("test")
  # return(list(x,extra))
  return(list(x,extra))
  # return(x)
}

a <- zfunction2(1,2,6,1,11)
a
str(a)
summary(a)


# the majority is always wrong when it comes to high performance
# 6/28/2017
# 7/12/2017

library(ggplot2)
library(sqldf)

## Working with ticket data

# load and inspect data
sample.data <- read.csv("D:/Work/Sample Ticket Data.csv")


str(sample.data)
head(sample.data); tail(sample.data); summary(sample.data)

# clean data and calc column
sample.data$Created <- as.Date(sample.data$Created,format = "%m/%d/%Y")
sample.data$Resolved <- as.Date(sample.data$Resolved,format = "%m/%d/%Y")
sample.data$Days.To.Resolve <- sample.data$Resolved - sample.data$Created

# plot with counts
ggplot(sample.data,aes(x=Store))+geom_bar()


function(example){
  # Create the data frame
  element <- rep("apples", 5)
  element
  qty <- c(2, 1, 4, 3, 6)
  qty
  category <- c("Red", "Green", "Red", "Green", "Yellow")
  d <- data.frame(element=element, qty=qty, category=category)
  d
  # chart with sums
  ggplot(d, aes(x=category, y=qty)) + geom_bar(stat = "identity")
  
  
  ## ended session by messing around with the sample.data, getting the dates to convert, trying to make calc field
}

# themes
ggplot(sample.data,aes(x=Store))+geom_bar()
ggplot(sample.data,aes(x=Store))+geom_bar()+theme_classic()
ggplot(sample.data,aes(x=Store))+geom_bar()+theme_minimal()
ggplot(sample.data,aes(x=Store))+geom_bar()+theme_void()



# 7/1/2017 - added extra data & etc

plot(sample.data$Days.To.Resolve)

# system date / current date
Sys.Date()
date()

## GGPLOT GUIDE
#bar chart average
ggplot(sample.data, aes(x=Store, y=Days.To.Resolve)) + stat_summary(fun.y="mean", geom="bar")

ggplot(sample.data, aes(x=Priority, y=Days.To.Resolve))+stat_summary(fun.y = "mean", geom="bar")+
  facet_grid(facets=.~Store) # rows ~ columns

ggplot(sample.data, aes(x=Priority, y=Days.To.Resolve))+stat_summary(fun.y = "mean", geom="bar")+
  facet_wrap(facets=~Store) # rows ~ columns

ggplot(sample.data, aes(x=Priority, y=Days.To.Resolve))+stat_summary(fun.y = "mean", geom="bar")+
  facet_grid(facets=Support.Group~Store) # rows ~ columns




# 7/4/2017

# Time series charts

# Bar chart over time
ggplot(sample.data, aes(Created))+geom_bar()
ggplot(sample.data, aes(Created))+geom_area(stat = "count")
ggplot(sample.data, aes(Created))+geom_density()
ggplot(sample.data, aes(Created))+geom_freqpoly()
ggplot(sample.data, aes(Created))+geom_histogram()

ggplot(data=sample.data, aes(x=Created)) + geom_bar(aes(fill=..count..)) 
ggplot(data=sample.data, aes(x=Created)) + geom_bar(aes(fill=..count..)) 

# line chart over time
ggplot(data=sample.data, aes(x=Created)) + geom_line(aes(fill=..count..), stat="bin", binwidth=1)




# 7/13/2017
# text mining stuff - reference github

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


ipak(c("qdap","ggplot2","tm"))


DATA 

freqs <- t(wfm(DATA$state, 1:nrow(DATA))) 
df <- data.frame(DATA, freqs)
df.flags <- data.frame(df[c(1:5)],
                       sapply(df[-c(1:5)],
                              function(i){
                                ifelse(i>0,1,0)
                              }))

colSums(freqs)
wfm(DATA$state, 1:nrow(DATA))

x <- c("R10 acd bd terminal not working *MSR - customer says the thing is the bidoof",
       "R10 ALOHA CE CEN power outage contact Charlotte")
str(wfm(x))
wfm(x)
Corpus(x)

write.csv(df.flags, file = "\\Exports\\df_flags.csv")




# Data frame with top 9 word counts
top.x <- rev(sort(colSums(freqs)))[1:9] #top 9 words 
top.x.cols <- freqs[, names(top.x)] #grab those columns from freqs
DATA.top9 <- data.frame(DATA, top.x.cols, check.names = FALSE) #put it together

pairs(DATA.top9[-c(1:5)])

# bar chart by sum. bar chart count
ggplot(DATA.top9, aes(sex,you))+stat_summary(geom = "bar", fun.y = "sum")
ggplot(DATA.top9, aes(sex))+geom_bar()+facet_grid(facets = ~fun)

colSums(freqs)
colSums(DATA.top9[-c(1:5)])
plot(colSums(freqs))
plot(colSums(DATA.top9[-c(1:5)]))

ggplot(DATA.top9, aes(c(DATA.top9[-c(1:5)])))+geom_bar()



# Using sample ticket data

ggplot(sample.data, aes(Support.Group))+geom_bar()+facet_wrap(~Store)

# Super plots
ggplot(sample.data, aes(Support.Group, Days.To.Resolve, color=Priority, shape=Store))+
  geom_point()
ggplot(sample.data, aes(Support.Group, color=Priority, shape=Store))+
  geom_point(stat = "count")
ggplot(sample.data, aes(Num.Assigns, Days.To.Resolve, color=Priority, shape=Store))+
  geom_point()




### End ###

# end ====


#### Additional General Notes ####

# create data frame with column names
# create data frame sample
docs <- data.frame(someText = c("This is a text.", "This another one."), secondColumn = c(1:2))
(ds <- DataframeSource(docs))
inspect(VCorpus(ds))