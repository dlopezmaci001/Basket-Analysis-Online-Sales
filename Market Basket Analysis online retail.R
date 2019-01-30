
# Libraries

library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)


# Load File

retail <- read.csv2("C:/Users/Daniel/Desktop/Nimerya/Online Retail/Online.Retail.csv",dec=".")

# Transform variables

retail <- retail[complete.cases(retail), ]

retail <- retail %>% mutate(Description = as.factor(Description))

retail <- retail %>% mutate(Country = as.factor(Country))

retail$InvoiceDate <- as.character(retail$InvoiceDate)

retail$Date <- as.POSIXct(retail$InvoiceDate, format="%d/%m/%Y%H:%M")

retail$Time <- format(retail$Date,"%H:%M")

retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

# When do people purchase

retail$Time <- as.factor(retail$Time)

a <- hms(as.character(retail$Time))

retail$Time = hour(a)

retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")

# Between 10 and 15 we get the most purchases. 

# Items purchased by customer

detach("package:plyr", unload=TRUE)

retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

# Most customers buy les than 20 items

# TOP 10 sellers

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

tmp <- head(tmp, n=10)

tmp

tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

# We need to transform the df into a format to pass it in the apriori algorith where 
# purchases must be in columns

retail_sorted <- retail[order(retail$CustomerID),]

library(plyr)

itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

# We only need item transactions so we remove the rest

itemList$CustomerID <- NULL

itemList$Date <- NULL

colnames(itemList) <- c("items")

# Save file 

write.csv2(itemList,"market_basket.csv", quote = FALSE, row.names = FALSE)

# Load the file

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')

tr

summary(tr)

# Let's look at the most frequent items

itemFrequencyPlot(tr, topN=20, type='absolute')

# let's find some rules

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))

rules <- sort(rules, by='confidence', decreasing = TRUE)

rules_lift <- sort(rules, by='lift', decreasing = TRUE)

summary(rules)

# Number of rules generated: 34.647
# The most rules is in combination of 5 items

# Let's analyse top 10 rules

inspect(rules[1:10])

inspect(rules_lift[1:10])


# Let's analyse these rules

topRules <- rules[1:10]

plot(topRules, method="graph") 

# Let's export the rules 

write(rules, file="Rules Online Retail.csv",sep=";",quote= TRUE,row.names=FALSE)

# Let's now focus on a given single product for example Decortation

decorationRules <- subset(rules, items %in% "DECORATION")

# let's analyse all the transactions that had decortions purchased

inspect(decorationRules)

# In all the DECORATION purchases 3 items have ALWAYS been bought:
# DECORATIONS 
# WOBBLY CHICKEN
# WOBBLY RABBIT 
# METAL

# That's why the confidence is always 1 


