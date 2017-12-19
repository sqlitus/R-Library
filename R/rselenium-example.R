# install.packages("RSelenium")
library(RSelenium)

# Create an automated testing driver
rD = rsDriver()
remDr = rD[["client"]]

# Navigate to course website
remDr$navigate("http://stat385.thecoatlessprofessor.com")

# Find the top link
webElem = remDr$findElement(using = 'class', value = "archive-item-link")

# Show the link
webElem$highlightElement()

# .. then Extract the text
webElem$getElementAttribute("text")

# Navigate to Google
remDr$navigate("http://www.google.com/ncr")

# Manipulate a webpage
webElem = remDr$findElement(using = "css", "[name = 'q']")
webElem = remDr$findElement(using = "id", value = "lst-ib")

# Send content to search
webElem$sendKeysToElement(list("enter here"))

# Trigger the Search
webElem$sendKeysToElement(list(key = "enter"))

# Navigate
remDr$goBack()
remDr$goForward()

remDr$close()
# stop the selenium server
rD[["server"]]$stop() 
