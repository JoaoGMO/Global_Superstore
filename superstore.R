#   Technical Challenge - Data Analyst 
#
#   This was a test to evaluate the data manipulation of the candidates for a
# job in data analysis. Our task was to answer a multiple choice survey with 
# 7 questions. The last one was a specific SQL question to pick the command
# line which matched the result in the statement, so I won't dispose it here.
#
#   All the questions are referred to the Global Super Store dataset  
# which is available in: 
# http://www.tableau.com/sites/default/files/training/global_superstore.zip
#


#   First step is to download the file for RStudio and get a glance of it 
# with colnames(), str(), head() and summary()

library(readxl)
gss <- read_excel("Global Superstore.xls") 
gss <- as.data.frame(gss)

colnames(gss)
str(gss)
head(gss, n = 5)
summary(gss)


# 1) How many sales where realized in 2012?


library(tidyverse)

gss$`Order Date` <- as.Date(gss$`Order Date`)
s2012 <- subset(gss, `Order Date`> as.Date("2011-12-31") & 
                  `Order Date` < as.Date("2013-01-01"))

s2012 %>%                       #   Since a single costumer can
  group_by(`Order ID`) %>%      # purchase more than one item in 
  summarise(n = n()) %>%        # one single order, it's more reasonable
  nrow()                        # to group by the Order ID column.


# 2) Which region has the biggest sales value in 2013?

r2013 <- subset(gss, `Order Date`> as.Date("2012-12-31") & 
                  `Order Date` < as.Date("2014-01-01"))

r2013 %>%
  group_by(Region) %>%                #   Here I grouped by the Product ID
  summarise(Value = sum(Sales)) %>%   # column and took a sum of the Sales
  arrange(desc(Value)) %>%            # column revenue, arranged by the Value
  head(n = 1)                         # and took the first row.


# 3) What was the product ID with the most sales, in quantity, in all 4 years?

gss %>%
  group_by(`Product ID`) %>%            
  summarise(Qnt = sum(Quantity)) %>%    
  arrange(desc(Qnt)) %>%                
  head(n = 1)


# 4) Which sub-categories sold the most in the last yeas of sales, in terms of
# value and quantity, respectively? (EASY)
#                                   (yes, they really added this comment)

s2014 <- subset(gss, `Order Date`> as.Date("2013-12-31") & 
                  `Order Date` < as.Date("2015-01-01"))

s2014 %>%
  group_by(`Sub-Category`) %>%
  summarise(Value = sum(Sales)) %>%
  arrange(desc(Value)) %>%
  head(n = 1)   # value

s2014 %>%
  group_by(`Sub-Category`) %>%
  summarise(Qnt = sum(Quantity)) %>%
  arrange(desc(Qnt)) %>%
  head(n = 1)   # Quantity


# 5) What was the growth rate for the "Consumer" segment between 2013 and 2014?

r2013 %>%
  group_by(Segment) %>%
  summarise(n = n()) 
  
Consumer2013 <- 7091  # Copy and paste from the previous code

s2014 %>% 
  group_by(Segment) %>%
  summarise(n = n())

consumer2014 <- 8935  # Same

(consumer2014/Consumer2013 - 1)*100

# 6) How many orders were returned in 2013?

#   This was the most tricky question because actually I'm not 100% sure
# if my analysis is completely right. What I've thought was that the
# absence of a postal code would make impossible to reach it's destiny.
# So I've calculated how many orders ~ with `Postal Code` == NA ~
# were done, just like the 1st question. 

na <- which(is.na(gss$`Postal Code`))     #   Which rows in gss/post code 
                                          # are NA?

# Sub-setting the rows which are NA in gss to the time interval
na2013 <- subset(gss[na,], `Order Date`> as.Date("2012-12-31") & 
                   `Order Date` < as.Date("2014-01-01"))

na2013 %>%                    
  group_by(`Order ID`) %>%    
  summarise(n = n()) %>%      
  nrow()







