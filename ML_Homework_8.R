library(tidyverse)
library(dplyr)
library(igraph)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

#Question: How related on a grocery list, is the second item to the first?

#In order to solve this we only want groceries list with 2 or more items 
#So we will drop anything with just one item in it
groceries <- subset(groceries, V2 != "")


str(groceries)
summary(groceries)

summary_data <- groceries %>%
  group_by(V1) %>%
  summarise(count = n())

# Sort the summary data
sorted_data <- summary_data %>%
  arrange(desc(count))

# Select the top 20 artists
top_priority_groceries <- sorted_data %>%
  head(20)

# Create a barplot
barplot(top_priority_groceries$count, names.arg = top_priority_groceries$V1, las = 2, cex.names = 0.6)
#Once we remove just single item groceries list, whole milk is no longer the 
#top purchased or sought after item, sausage is with whole milk coming in second
#People certainly love the meat!


groceries_lists = split(x=groceries$V1, f=groceries$V2)
## Remove duplicates ("de-dupe")
groceries_lists = lapply(groceries_lists, unique)


groceriestrans = as(groceries_lists, "transactions")
summary(groceriestrans)

#I have choosen a support of .15 an confidence of .7 and maxlen of 3 
#to reduce the computation time as before it was taking too long. This will 
#help simplify it.

musicrules = apriori(groceriestrans, 
                     parameter=list(support=.15, confidence=.7, maxlen=3))


# Look at the output... so many rules!
inspect(musicrules)

#Milk is an extremely common staple. Milk has a support of .16-.21 in general based on
#the first 100 rows. It's confidence goes up with related dairy items like butter milk
#But it's confidence also rises when purchasing berries, bread or eggs, which 
#are also staples but not to the extent of milk.


#Let's look at other relationships
#Meat purchase gives a lift to other meat. E.g. frankfurter, and sausage 
#receive a lift of nearly 2 when someone picks up ham 


#Now let's try to plot the overall relationships
plot(musicrules)

#From this initial plot we can see that there isn't a clear cut relationship
#Between support and confidence, only that is goes up but that the lift isn't
#super big, this primarily due to our support threshold we chose to minimize 
#computational cost.

# can swap the axes and color scales
plot(musicrules, measure = c("support", "lift"), shading = "confidence")

# can now look at subsets driven by the plot
inspect(subset(musicrules, support > 0.3))

#Based on this result, diary products have the highest support, such as whole milk
#But when purchased with other dairy products, the confidence level goes way above
#E.G. When yogurt is purchased, the confidence of whole milk goes from .38 to .82

