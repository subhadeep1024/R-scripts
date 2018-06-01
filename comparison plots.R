rm(list=(setdiff(ls(), ls(pattern="_Cell_wise"))))

library(sm)
library(MASS)
library(rgl)
library(ggplot2)

#### plots####

PCA_density_plotter_all(fact = "H2az", PC = 2, adjust=5, xlim = c("-10", "10"), titleadjust=0.2, ysize=20, xsize = 20)
PCA_density_plotter_3D_all(fact = "DHS_len", expression_type = "n", PC1 = 1, PC2 = 2 ,grid_size= 100)
PCA_density_plotter_3D_all_static(fact = "H2az", expression_type = "r",PC1 = 1, PC2 = 2 ,grid_size= 100, theta = -30, phi = 20, d = 10)
