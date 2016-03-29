set.seed(1)

script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir,"/GMeans.R", sep=""))

d <- read.csv(paste(script.dir,"/data.csv", sep=""), head=T)

plot.new()

# Analysis for getting best values of k
pdf(paste(script.dir,"/BestFit.pdf", sep=""), width = 10, height = 6, paper="a4r")
par(mfrow= c(1,2))
mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main= "SS Error vs Number of Clusters")

#plot(y, alpha, type="l", main = "Sig. Level vs  Num Clusters", xlab="Number of clusters", xlim=rev(range(y)))
#points(y,alpha)


gmeans_model <- gmeans(d, alpha = 0.0001)
gmeans_visualize(d, gmeans_model, alpha = 0.0001, sig = 0.95)


dev.off()

plot.new()

pdf(paste(script.dir,"/AlphaVsFit.pdf", sep=""), width = 10, height = 6, paper="a4r")
par(mfrow= c(2,2))
alpha <- c(0.005, 0.001, 0.0005, 0.0001)
for(i in 1:length(alpha))
{
  gmeans_model <- gmeans(d, alpha = alpha[i])
  gmeans_visualize(d, gmeans_model, alpha = alpha[i], sig = 0.95)
  print(paste(dim(gmeans_model$centers)[1], alpha[i]))
} 

dev.off()
