rm(list=(setdiff(ls(), ls(pattern="_Cell_wise"))))

library(sm)
library(MASS)
library(rgl)
library(ggplot2)

#### density plots ####

PCA_density_plotter_all(fact = "H2az", PC = 2, adjust=5, xlim = c("-10", "10"), titleadjust=0.2, ysize=20, xsize = 20)
PCA_density_plotter_3D_all(fact = "DHS_len", expression_type = "n", PC1 = 1, PC2 = 2 ,grid_size= 100)
PCA_density_plotter_3D_all_static(fact = "H2az", expression_type = "r",PC1 = 1, PC2 = 2 ,grid_size= 100, theta = -30, phi = 20, d = 10)


#### correlation plots ####

############################### heatmap style 1 ##################################
a <- "comb1"
b <- "H2az"
c <- "r"
e <- paste(b,c,sep="_")
xyz <- PCA_correlation_calculator_all_2(a,b,c,ncol(eval(parse(text=e))))
colnames(xyz) <- 1:ncol(xyz)
heatmap.2(xyz,trace="none",density = "none", Colv = FALSE,
          Rowv = FALSE,dendrogram = "none",
          xlab = "principal components",
          adjRow = c(0.01,1),srtRow = 0,offsetRow = 0.8, 
          margin = c(8,8), symm =TRUE,
          main = paste("Correlation of PCA_", a," with ",b," in RETs ",sep=""),
          col=rev(heat.colors(5)), notecol = ("black"),
          lmat=rbind( c(0, 3), c(2,1), c(0,4)),
          lhei=c(0.9, 6.8, 1.5), lwid=c(0.1,5),
          cellnote= round(xyz,2),cexRow = 1.5,
          cexCol = 1.5, notecex = 1.5
          )
##################################################################################

############################### heatmap style 2 ##################################
a <- "H3k9ac"
b <- paste(a,"Cell_wise",sep="_")
xyz <- PCA_correlation_calculator_all(a,ncol(eval(parse(text=b)))-1)
colnames(xyz) <- 1:ncol(xyz)

heatmap.2(xyz,trace="none",density = "none", Colv = FALSE,
          Rowv = FALSE,dendrogram = "none", xlab = "principal components",
          adjRow = c(0.16,1),srtRow = 0,offsetRow = 0, margin=c(6,6), symm =TRUE,
          main = paste("Overall correlation of PCA_", a," with ", a,sep=""),
          col=rev(heat.colors(5)), notecol = ("black"),
          lmat=rbind( c(0, 3), c(2,1), c(0,4)), lhei=c(0.9, 6.8, 1.5), lwid=c(0.1,5),
          cellnote= round(xyz,2),cexRow = 1, cexCol = 0.9, notecex = 0.9
)
###################################################################################

############################### heatmap style 3 ##################################
a <- "H3k4me3"
b <- paste(a,"Cell_wise",sep="_")
#xyz <- PCA_correlation_calculator_all_3(comb="comb1",fact=a, no_of_PCs = ncol(eval(parse(text=b)))-1)
xyz <- PCA_correlation_calculator_all_3(comb="comb1",fact=a, no_of_PCs = 4)
colnames(xyz) <- 1:ncol(xyz)

heatmap.2(xyz,trace="none",density = "none", Colv = FALSE,
          Rowv = FALSE,dendrogram = "none", xlab = "principal components",
          adjRow = c(0.01,1),srtRow = 0,offsetRow = 0, margin=c(7,15), symm =TRUE,
          main = paste("Overall correlation of " ,comb," with ", a,sep=""),
          col=rev(heat.colors(5)), notecol = ("black"),
          lmat=rbind( c(0, 3), c(2,1), c(0,4)), lhei=c(0.9, 6.8, 1.5), lwid=c(0.5,5),
          cellnote= round(xyz,2),cexRow = 1, cexCol = 1, notecex = 1
)
#######################################################################################
