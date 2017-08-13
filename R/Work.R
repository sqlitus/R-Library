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







#### various ggplots on ticket data 7/21/2017 ####

# import packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext"))



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


# week and month values of dates
df$Created.Week <- as.Date(cut(df$Created,
                               breaks = "week",
                               start.on.monday = T)) # changes weekly break point to Sunday
df$Created.Week2 <- as.Date(cut(df$Created,
                                breaks = "week",
                                start.on.monday = F)) # changes weekly break point to Sunday


df$Created.Month <-as.Date(cut(df$Created, breaks = "month"))




#### 8/12/2017 Text mining sample data ####

function(REFERENCES){
  # https://stackoverflow.com/questions/44014097/convert-document-term-matrix-dtm-to-data-frame-r-programming
  # https://stackoverflow.com/questions/26711423/how-can-i-convert-an-r-data-frame-with-a-single-column-into-a-corpus-for-tm-such
  # https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus
}

# get title column, convert to data frame, change title
df$Title <- as.character(df$Title)
df.titles <- as.data.frame(df[,c("Title")], col.names = "Title")
colnames(df.titles) <- "Title"

# turn sliced dataframe to corpus, then get document term matrix
corpus <- Corpus(DataframeSource(df.titles))
dtm <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))
inspect(dtm)

# more text processing after this probably
# calculate flag column
library(tidytext)
dtm.df <- tidy(dtm)
dtm.df$flag <- ifelse(dtm.df$count > 0, 1, 0)


# need analysis on most frequent words (by doc occurrence)
# need to see which docs match a particular word
# need to be able to slice and dice based on word, and other fields from base dataset.



# need to APPEND dtm to orig df for analysis



function(trying.to.remove.more.characters.not.working){
# removeSpecialChars <- function(x) gsub("“•”","",x)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
corpus2 <- tm_map(corpus, removeSpecialChars)
dtm2 <- DocumentTermMatrix(corpus2,
                           control = list(removePunctuation = TRUE,
                                          stopwords = TRUE))
}




#### Visualize with ggplot2 ####
# mailR stuff / Rshiny

#do: stacked bar chart, facet bar, color scale by X, point by Y...
# volume
ggplot(df, aes(Created.Month))+geom_bar()

ggplot(df, aes(x=Created.Month, y=Time.To.Response))+stat_summary(fun.y = "mean", geom="bar")+
  facet_grid(facets=Support.Group~.) # rows ~ columns


ggplot(data=df, aes(x=Created.Month)) + geom_bar(aes(fill=..count..))+
  facet_grid(Classification~.)





########### GGPLOT SPELLBOOK ###########

#### Part 1 - Basic Bar Plots; Colors; ####
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



#### Part 2 - Stacked / Grouped bars ####
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
