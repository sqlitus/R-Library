#### various ggplots on ticket data ####
#### 7/21/2017 

# import packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("ggplot2", "tm", "sqldf", "scales","chron"))



## Import & Clean data process. Convert char to datetimes. Convert text factors to char. Create calculated columns with datetime arithmatic.

df <- read.csv("D:/Work/Sample Ticket Data.csv")



# convert the string dates to datetimes -> convert to character, then match the format of the character field to time values
df$Created <- as.character(df$Created)
df$Created <- as.POSIXct(df$Created, format="%m/%d/%Y %H:%M")
df$Resolved <- as.character(df$Resolved)
df$Resolved <- as.POSIXct(df$Resolved, format="%m/%d/%Y %H:%M")
df$FirstAssigned <- as.character(df$FirstAssigned)
df$FirstAssigned <- as.POSIXct(df$FirstAssigned, format="%m/%d/%Y %H:%M")


function(date.conversion.and.datediffs){
# Convert to simple date and find datediff
df$Created.Date <- as.Date(df$Created,format = "%m/%d/%Y")
df$Resolved.Date <- as.Date(df$Resolved,format = "%m/%d/%Y")
df$Time.To.Response <- df$FirstAssigned - df$Created
df$Days.To.Resolve <- df$Resolved - df$Created
}

# Time Difference in units = minutes
df$Time.To.Response <- difftime(df$FirstAssigned, df$Created)
df$Time.To.Restore.Service <- difftime(df$Resolved, df$Created)


# Visualize with ggplot2
# mailR stuff / Rshiny