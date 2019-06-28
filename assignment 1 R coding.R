install.packages("plotrix")

par(mfrow =c(1,2))
slices <- c(650, 1000,900,300,14900)
lbls <- c("UK","Canada","India","Australia","US")
pct <- round(slices/(sum(slices))*100)
lbls2 <- paste(lbls," ", pct,"%", sep = " ")
pie(slices, labels = lbls2, col = rainbow(length(lbls2)), main = "percentage plots")

library(plotrix)
pie3D(slices, labels = lbls2, explode = 0.1 ,col = rainbow
      
standardize <- function(x){
  return((x -mean(x))/sd(x))
  
}


NDATA.NORM$A <- standardize(Rawdata$A)
NDATA.NORM$B <- standardize(Rawdata$B)
NDATA.NORM$C <- standardize(Rawdata$C)
NDATA.NORM$D <- standardize(Rawdata$D)

attach(NDATA.NORM)
par(mfrow = c(2,2))
boxplot(A , main ="Variable A")
boxplot(B , main ="Variable B")
boxplot(C , main ="Variable C")
boxplot(D , main ="Variable D")

attach(Rawdata)
plot(A,B, main = "Scatterplot of A vs B", xlab = "A")

lines(B)


par(mfrow=[c(2,2)])
boxplot(forestfires$wind, xlab="wind", ylab="area")
boxplot(forestfires$ISI, xlab="ISI", ylab="area")
boxplot(forestfires$DC, xlab="DC", ylab="area")



diagnose_outlier(Twitter,friends_count) %>% 
  filter(outliers_ratio > 5) %>% 
  mutate(rate = outliers_mean / with_mean) %>% 
  arrange(desc(rate)) %>% 
  select(-outliers_cnt)



par(mfrow=c(2,4))
hist(Rawdata$A, xlab="Sustainabilty", probability = "TRUE")
lines(density(Rawdata$A), lwd = 4, col= "Blue")
hist(Rawdata$B, xlab="Carbon footprint")
hist(Rawdata$C, xlab="Weight")
hist(Rawdata$D, xlab="Required Power")
hist(A, xlab="Sustainabilty")
hist(B, xlab="Carbon footprint")
hist(C, xlab="Weight")
hist(D, xlab="Required Power")

lines(density(Rawdata$A, lwd = 3, col= "Blue"))


library(GGally)

ggpairs(Rawdata[,c("A","B")])


hist(forestfires$wind,
     main = "Histogram of Wind Speed",
     xlab = "Wind Speed",
     ylab = "Frequency",
     #freq=FALSE,
     probability =TRUE,
     col = "pink",
     breaks = 15)
lines(density(forestfires$wind),col="blue",lwd=2)


      

