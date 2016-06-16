sensorydata <- read.csv("data/sensory.csv", sep=",", dec=".")
sensory <- data.frame(sensorydata[,-1], row.names=sensorydata[,1])
round(cor(sensory[,1:15]), 2)
pc <- princomp(samp.with.rownames[,1:15], cor=TRUE, scores=TRUE)
summary(pc)
plot(pc,type="lines")
biplot(pc)

thecor = round(cor(sensorydata),2)
colnames(thecor)<-colnames(sensorydata)
rownames(thecor)<-colnames(sensorydata)
## printing the result in a clean-slate table
#cat(renderHeatmapX(thecor, border=1,center = 0,vrange_up = 1, vrange_down = 1))
cat(renderHeatmapX(thecor, border=1))

#plot3d(pc$scores[,1:3], col=iris$Species)
#Error in col2rgb(colors) : invalid color name 'setosa'
#> text3d(pc$scores[,1:3],texts=rownames(iris))
#> coords <- NULL
#> text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
 # +     coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
#> for (i in 1:nrow(pc$loadings)) {
  #+ }
#> lines3d(coords, col="red", lwd=4)
#> set.seed(42)
#> cl <- kmeans(iris[,1:4],3)
#> iris$cluster <- as.factor(cl$cluster)
#> plot3d(pc$scores[,1:3], col=iris$cluster, main="k-means clusters")
#> plot3d(pc$scores[,1:3], col=iris$Species, main="actual species")