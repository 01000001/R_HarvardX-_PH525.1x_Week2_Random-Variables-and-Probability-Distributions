population <- read.csv(file = "/Library/Frameworks/R.framework/Versions/3.2/Resources/library/dagdata/extdata/femaleControlsPopulation.csv")

#turn dataframe into numeric vector
population <- unlist(population)

n <- 10000
null <- vector("numeric",n) 
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}

mean(null >= obsdiff)

#Let???s repeat the loop above, but this time let???s add a point to the figure every time we re-run the experiment. If you run this code, you can see the null distribution forming as the observed values stack on top of each other.

n <- 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency") 

totals <- vector("numeric",11)

for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulldiff <- mean(treatment) - mean(control)
  
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j] <- totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
  if(i < 15) Sys.sleep(1) ##You can add this line to see values appear slowly 
}

#The figure above amounts to a histogram. From a histogram of the null vector we calculated earlier, we can see that values as large as obsdiff are relatively rare:

hist(null, freq=TRUE)
abline(v=3.020833, col="red", lwd=2)
