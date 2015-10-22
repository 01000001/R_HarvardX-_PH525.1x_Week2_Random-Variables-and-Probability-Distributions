#Description####
#HarvardX: PH525.1x Data Analysis for Life Sciences 1: Statistics and R
#
#Introduction to Random variables and population distribution
#
#recommended packaged
#library(devtools)
#install_github("genomicsclass/dagdata")

#Introduction####
#location of the packeges
dir <- system.file(package="dagdata")
list.files(dir)
list.files(file.path(dir,"extdata")) #external data is in this directory

filename <- file.path(dir,"extdata/femaleMiceWeights.csv")

library(dplyr)
dat <- read.csv(filename)

#select control group data
control <- filter(dat, Diet =="chow") %>% select(Bodyweight) %>% unlist

#select treatment group data
treatment <- filter(dat, Diet =="hf") %>% select(Bodyweight) %>% unlist

mean(treatment)- mean(control)

population <- read.csv(file = "/Library/Frameworks/R.framework/Versions/3.2/Resources/library/dagdata/extdata/femaleControlsPopulation.csv")

#turn dataframe into numeric vector

population <- unlist(population)

#mean of a sample from the population
mean(sample(population,12))

#Exercises####
#RANDOM VARIABLES EXERCISES #1  (1 point possible)
#What is the average of these weights?
mean(population)

#RANDOM VARIABLES EXERCISES #2  (1 point possible)
#After setting the seed at 1, set.seed(1) take a random sample of size 5. What is the absolute value (use abs) of the difference between the average of the sample and the average of all the values?
set.seed(1)
abs(mean(sample(population,5)) - mean(population))

#RANDOM VARIABLES EXERCISES #3  (1 point possible)
#After setting the seed at 5, set.seed(5) take a random sample of size 5. What is the absolute value of the difference between the average of the sample and the average of all the values?
set.seed(5)
abs(mean(sample(population,5)) - mean(population))

#Null Distribution####
obs <- mean(treatment) - mean(control)

#is the difference due to chance?
#if the 0 hypothesis is true, we can assign control mice to the treatment group, because there is no difference
control <- sample ( population, 12 )
treatmet <- sample (population, 12)
mean(treatment)- mean(control)

#record the differences we see under the 0 hypothesis
n = 10000
nulls <- vector("numeric", n)
for( i in 1:n){
  control <- sample ( population, 12 )
  treatmet <- sample (population, 12)
  nulls[i] <- mean(treatment)- mean(control)
  }

max(nulls)

hist(nulls)

#So now we can get a sense of how likely it is to see values as big as 3, the observation, under the null hypothesis.
#And that'll help us provide scientific support based on statistical inference for our finding.

#P-value####
#We're going to report the proportion of times that the null value, the null distribution, was larger than what we observed.
#So let's first count how often it happens.
sum(nulls > obs) / n
# equal to
mean(nulls > obs)

mean(abs(nulls) > obs)

#NULL DISTRIBUTIONS EXERCISES####
population <- read.csv(file = "/Library/Frameworks/R.framework/Versions/3.2/Resources/library/dagdata/extdata/femaleControlsPopulation.csv")
population <- unlist(population)

#NULL DISTRIBUTIONS EXERCISES #1  (1 point possible)
#Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages. What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
set.seed(1)

n = 1000
nulls <- vector(mode = "numeric", length = n)

for(i in 1:n){
  X <- sample ( population, 5 )
  nulls[i] <- mean(X)
#  treatmet <- sample (population, 5)
#  nulls[i] <- mean(treatment)- mean(control)
  }

hist(nulls)

mean(abs(nulls - mean(population)) > 1)

#solution
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(population,5)
  averages5[i] <- mean(X)
}
hist(averages5) ##take a look
mean( abs( averages5 - mean(population) ) > 1)

#NULL DISTRIBUTIONS EXERCISES #2  (1 point possible)
#We are now going to increase the number of times we redo the sample from 1,000 to 10,000. Set the seed at 1, then using a for-loop take a random sample of 5 mice 10,000 times. Save these averages. What proportion of these 10,000 averages are more than 1 gram away from the average of x ?
set.seed(1)

n = 10000
averages <- vector(mode = "numeric", length = n)

for(i in 1:n){
  X <- sample ( population, 5 )
  averages[i] <- mean(X)
}

hist(averages)

mean(abs(averages - mean(population)) > 1)

#NULL DISTRIBUTIONS EXERCISES #3  (1 point possible)
#Note that the answers to 1 and 2 barely changed. This is expected. The way we think about the random value distributions is as the distribution of the list of values obtained if we repeated the experiment an infinite number of times. On a computer, we can't perform an infinite number of iterations so instead, for our examples, we consider 1,000 to be large enough, thus 10,000 is as well. Now if instead we change the sample size, then we change the random variable and thus its distribution.

#Set the seed at 1, then using a for-loop take a random sample of 50 mice 1,000 times. Save these averages. What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
set.seed(1)

n = 1000
averages <- vector(mode = "numeric", length = n)

for(i in 1:n){
  X <- sample ( population, 50 )
  averages[i] <- mean(X)
}

hist(averages)

mean(abs(averages - mean(population)) > 1)

#PROBABILITY DISTRIBUTIONS EXERCISES####
"
In the video you just watched, Rafa looked at distributions of heights, and asked what was the probability of someone being shorter than a given height. In this assessment, we are going to ask the same question, but instead of people and heights, we are going to look at whole countries and the average life expectancy in those countries.

We will use the data set called \"Gapminder\" which is available as an R-package on Github. This data set contains the life expectancy, GDP per capita, and population by country, every five years, from 1952 to 2007. It is an excerpt of a larger and more comprehensive set of data available on Gapminder.org, and the R package of this dataset was created by the statistics professor Jennifer Bryan.

First, install the gapminder data using:

install.packages(\"gapminder\")

Next, load the gapminder data set. To find out more information about the data set, use ?gapminder which will bring up a help file. To return the first few lines of the data set, use the function head().

library(gapminder)
data(gapminder)
head(gapminder)

Create a vector 'x' of the life expectancies of each country for the year 1952. Plot a histogram of these life expectancies to see the spread of the different countries.
"
library(gapminder)
data(gapminder)
head(gapminder)

x <- filter(gapminder, year ==1952) %>% select(lifeExp) %>% unlist
hist(x)

#PROBABILITY DISTRIBUTIONS EXERCISES #1  (1 point possible)
"
In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the function F(a) for any a, which tells you the proportion of the values which are less than or equal to a.

We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are less than or equal a, divided by the total number of values in x, in other words the proportion of values less than or equal to a.

The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn't return a value, but a function.

Let's continue, using the simpler, mean() function.

What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
"
mean( x <= 40)

"
EXPLANATION

dat1952 = gapminder[ gapminder$year == 1952, ]

x = dat1952$lifeExp

mean(x <= 40)
"

#PROBABILITY DISTRIBUTIONS EXERCISES #2  (1 point possible)
#What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years? Hint: this is the proportion that have a life expectancy less than or equal to 60 years, minus the proportion that have a life expectancy less than or equal to 40 years.

mean(x <= 60) - mean(x <= 40)

"
SAPPLY() ON A CUSTOM FUNCTION

Suppose we want to plot the proportions of countries with life expectancy 'q' for a range of different years. R has a built in function for this, plot(ecdf(x)), but suppose we didn't know this. The function is quite easy to build, by turning the code from question 1.1 into a custom function, and then using sapply(). Our custom function will take an input variable 'q', and return the proportion of countries in 'x' less than or equal to q. The curly brackets { and }, allow us to write an R function which spans multiple lines:
"
prop = function(q) {
mean(x <= q)
}

"
Try this out for a value of 'q':  
"
prop(40)

"
Now let's build a range of q's that we can apply the function to:"

qs = seq(from=min(x), to=max(x), length=20)
qs
"Print 'qs' to the R console to see what the seq() function gave us. Now we can use sapply() to apply the 'prop' function to each element of 'qs':"

props = sapply(qs, prop)
props

"Take a look at 'props', either by printing to the console, or by plotting it over qs:"

plot(qs, props)

"Note that we could also have written this in one line, by defining the 'prop' function but without naming it:"

props = sapply(qs, function(q) mean(x <= q))

"This last style is called using an \"inline\" function or an \"anonymous\" function. Let's compare our homemade plot with the pre-built one in R:"

plot(ecdf(x))

