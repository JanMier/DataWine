  local({
      .PC <- 
        princomp(~X1.carmel+X2.spice+X3.veg+X4.oak+X5.hot+X6.floral+X7.chem+X8.trop+X9.citrus+X10.sweet+X11.sour+X12.bitter+X13.astrMF+X14.hotMF+X15.viscMF,cor=TRUE, data=wine_std)
      cat("\nComponent loadings:\n")
      print(unclass(loadings(.PC)))
      cat("\nComponent variances:\n")
      print(.PC$sd^2)
      cat("\n")
      print(summary(.PC))
      screeplot(.PC) })
  wine_std <- 
      read.table("C:/Users/oskarfeng/Documents/R/BDA/BigData/CourseSessions/Project/data/standardized.csv",
                       header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

  HClust.1 <- hclust(dist(model.matrix(~-1 + X1.carmel+X2.spice+X3.veg+X4.oak+X5.hot+X6.floral+X7.chem+X8.trop+X9.citrus+X10.sweet+X11.sour+X12.bitter+X13.astrMF+X14.hotMF+X15.viscMF,wine_std)) , method= "ward")

  plot(HClust.1, main= "Cluster Dendrogram for Solution HClust.1", xlab= 
                   "Observation Number in Data Set wine_std", 
         sub="Method=ward; Distance=euclidian")

  HClust.2 <- hclust(dist(model.matrix(~-1 + 
                                                   X16.viscosity+X17.density+X18.ethanol+X19.phenolics+X20.Citrate+X21.Tartrate+X22.Malate+X23.Succinate+X24.Lactate+X25.Acetic+X26.Total.Sugars+X27.Glycerol+X28.P+X29.K+X30.Ca+X31.Mg+X32.Na+X33.Cl+X34.pH+X35.TA,
                                                  wine_std)) , method= "ward")

  plot(HClust.2, main= "Cluster Dendrogram for Solution HClust.2", xlab= 
                   "Observation Number in Data Set wine_std", 
      sub="Method=ward; Distance=euclidian")

  HClust.4 <- hclust(dist(model.matrix(~-1 + 
                                                   X1.carmel+X2.spice+X3.veg+X4.oak+X5.hot+X6.floral+X7.chem+X8.trop+X9.citrus+X10.sweet+X11.sour+X12.bitter+X13.astrMF+X14.hotMF+X15.viscMF+X16.viscosity+X17.density+X18.ethanol+X19.phenolics+X20.Citrate+X21.Tartrate+X22.Malate+X23.Succinate+X24.Lactate+X25.Acetic+X26.Total.Sugars+X27.Glycerol+X28.P+X29.K+X30.Ca+X31.Mg+X32.Na+X33.Cl+X34.pH+X35.TA,
                                                  wine_std)) , method= "ward")

  plot(HClust.4, main= "Cluster Dendrogram for Solution HClust.4", 
        xlab= "Observation Number in Data Set wine_std", 
   sub="Method=ward; Distance=euclidian")
