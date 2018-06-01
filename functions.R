######################################## function definition #################################################

############################################################ density_plotter ###########################################################
density_plotter <- function(cell, fact){
  
  #### for rarely expressed ##
  x1 <- paste(fact,"r",sep="_")
  x2 <- paste(x1,cell,sep="$")
  x3 <- eval(parse(text=x2))
  
  #### for widely expressed ##
  x4 <- paste(fact,"w",sep="_")
  x5 <- paste(x4,cell,sep="$")
  x6 <- eval(parse(text=x5))
  
  #### for not expressed ##
  x7 <- paste(fact,"n",sep="_")
  x8 <- paste(x7,cell,sep="$")
  x9 <- eval(parse(text=x8))
  
  z <- c(x3,x6,x9)
  z1 <- data.frame(factor = z, 
                   expression_group = rep(c("Rarely expressed","Widely expressed","Not expressed"),
                                          c(nrow(eval(parse(text=x1))), 
                                            nrow(eval(parse(text=x4))), 
                                            nrow(eval(parse(text=x7))))))
  sm.density.compare(z1$factor,z1$expression_group, 
                     col= c("Red","Green", "black"), 
                     lty=c(1,1,1), xlab=fact, pch = 5)
  legend("topright",levels(as.factor(z1$expression_group)), 
         fill=c("Red","Green","Black"),
         title=cell)
}
############################################################# PCA_density_plotter ######################################################
PCA_density_plotter <- function(fact, PC){
  
  #### for rarely expressed ##
  x1 <- paste("PCA",fact,"r",sep="_")
  x2 <- paste(x1,"scores",sep="$")
  x3 <- paste(x2,"[,",PC,"]",sep="")
  x4 <- eval(parse(text=x3))
  
  #### for widely expressed ##
  x5 <- paste("PCA",fact,"w",sep="_")
  x6 <- paste(x5,"scores",sep="$")
  x7 <- paste(x6,"[,",PC,"]",sep="")
  x8 <- eval(parse(text=x7))
  
  #### for not expressed ##
  x9 <- paste("PCA",fact,"n",sep="_")
  x10 <- paste(x9,"scores",sep="$")
  x11 <- paste(x10,"[,",PC,"]",sep="")
  x12 <- eval(parse(text=x11))
  
  #### for intermediately expressed ####
  x13 <- paste("PCA",fact,"int",sep="_")
  x14 <- paste(x13,"scores",sep="$")
  x15 <- paste(x14,"[,",PC,"]",sep="")
  x16 <- eval(parse(text=x15))
  
  z <- c(x4,x8,x12,x16)
  z1 <- data.frame(PCs = z, 
                   expression_group = rep(c("Rarely expressed","Widely expressed",
                                            "Not expressed", "Intermediately expressed"),
                                          c(length(x4), 
                                            length(x8), 
                                            length(x12),
                                            length(x16))))
  sm.density.compare(z1$PCs, z1$expression_group, 
                     col= c("Red","Green", "black","blue"), 
                     lty=c(1,1,1,1), xlab=paste(fact,"PC",PC,sep="_"))
  
  legend("topright",levels(as.factor(z1$expression_group)), 
         fill=c("Red","Green","Black","Blue"),
         title=paste("PC",PC))
}

############################################################## PCA_density_plotter_all #################################################
PCA_density_plotter_all <- function(fact, PC, adjust){
  
  #### for rarely expressed ##
  x1 <- paste("PCA",fact,"r",sep="_")
  x2 <- paste(x1,"[,",PC,"]",sep="")
  x3 <- eval(parse(text=x2))
  
  #### for widely expressed ##
  x4 <- paste("PCA",fact,"w",sep="_")
  x5 <- paste(x4,"[,",PC,"]",sep="")
  x6 <- eval(parse(text=x5))
  
  #### for not expressed ##
 # x7 <- paste("PCA",fact,"n",sep="_")
  #x8 <- paste(x7,"[,",PC,"]",sep="")
  #x9 <- eval(parse(text=x8))
  
  #### for intermediately expressed ####
  #x10 <- paste("PCA",fact,"int",sep="_")
  #x11 <- paste(x10,"[,",PC,"]",sep="")
  #x12 <- eval(parse(text=x11))
  
  #z <- c(x3,x6,x9,x12)
  z <- c(x3,x6)
  z1 <- data.frame(PCs = z, 
                   exp_type= c(rep("RETs",nrow(PCA_H3k9ac_r)),
                               rep("WETs",nrow(PCA_H3k9ac_w))
                               #rep("NETs",nrow(PCA_H3k9ac_n)),
                               #rep("Intermediate",nrow(PCA_H3k9ac_int))
                               ))
  
  label <- paste(fact,"PC",PC,sep="_")
  
  ggplot(z1, aes(PCs,colour = exp_type))+ geom_density(size=1, adjust = adjust) 
  + ggtitle(paste(fact,"Density plot of various expression groups over PC",PC)) 
  + labs(x=label)+xlim(-1000,1000)
  
  #sm.density.compare(z1$PCs, z1$exp_type, 
   #                  col= c("Red","Green", "black","blue"), 
    #                 lty=c(1,1,1,1), xlab=paste(fact,"PC",PC,sep="_"))
  
  #legend("top",levels(as.factor(z1$exp_type)), 
   #      fill=c("Red","Green","Black","Blue"),
    #     title=paste("PC",PC))
}

############################################################## PCA_density_plotter_3D ################################################

PCA_density_plotter_3D <- function(fact, expression_type, PC1, PC2, grid_size){
  switch(expression_type,
         r={
           x1 <- paste("PCA",fact,"r",sep="_")
           x2 <- paste(x1,"scores",sep="$")
           x3 <- paste(x2,"[,",PC1,"]",sep="")
           x4 <- paste(x2,"[,",PC2,"]",sep="")
           x5 <- eval(parse(text=x3))
           x6 <- eval(parse(text=x4))
           bivn.kde <- kde2d(x5, x6, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1)
         },
         w={
           x1 <- paste("PCA",fact,"w",sep="_")
           x2 <- paste(x1,"scores",sep="$")
           x3 <- paste(x2,"[,",PC1,"]",sep="")
           x4 <- paste(x2,"[,",PC2,"]",sep="")
           x5 <- eval(parse(text=x3))
           x6 <- eval(parse(text=x4))
           bivn.kde <- kde2d(x5, x6, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1)
         },
         n={
           x1 <- paste("PCA",fact,"n",sep="_")
           x2 <- paste(x1,"scores",sep="$")
           x3 <- paste(x2,"[,",PC1,"]",sep="")
           x4 <- paste(x2,"[,",PC2,"]",sep="")
           x5 <- eval(parse(text=x3))
           x6 <- eval(parse(text=x4))
           bivn.kde <- kde2d(x5, x6, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1)
         },
         #w_r={
         # x1 <- paste("PCA",fact,"w",sep="_")
         #x2 <- paste(x1,"scores",sep="$")
         #x3 <- paste(x2,"[,",PC1,"]",sep="")
         #x4 <- paste(x2,"[,",PC2,"]",sep="")
         #x5 <- eval(parse(text=x3))
         #x6 <- eval(parse(text=x4))
         
         #x7 <- paste("PCA",fact,"r",sep="_")
         #x8 <- paste(x7,"scores",sep="$")
         #x9 <- paste(x8,"[,",PC1,"]",sep="")
         #x10 <- paste(x8,"[,",PC2,"]",sep="")
         #x11 <- eval(parse(text=x9))
         #x12 <- eval(parse(text=x10))
         
         #x13 <- c(x5,x11)
         #x14 <- c(x6,x12)
         
         #bivn.kde <- kde2d(x13, x14, n = grid_size)
         
         #col1 = c(rep(2,length(x5)), rep(3, length(x11)))
         #col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
         #persp3d(x=bivn.kde, col = col1)
         #},
         int = {
           x1 <- paste("PCA",fact,"int",sep="_")
           x2 <- paste(x1,"scores",sep="$")
           x3 <- paste(x2,"[,",PC1,"]",sep="")
           x4 <- paste(x2,"[,",PC2,"]",sep="")
           x5 <- eval(parse(text=x3))
           x6 <- eval(parse(text=x4))
           bivn.kde <- kde2d(x5, x6, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1)
         }
  )
}

###################################################### PCA_density_plotter_3D_all ######################################################
PCA_density_plotter_3D_all <- function(fact, expression_type, PC1, PC2, grid_size){
  switch(expression_type,
         r={
           x1 <- paste("PCA",fact,"r",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of RETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         w={
           x1 <- paste("PCA",fact,"w",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of WETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         n={
           x1 <- paste("PCA",fact,"n",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of NETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         w_r={
           x1 <- paste("PCA",fact,"w",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           
           x7 <- paste("PCA",fact,"r",sep="_")
           x8 <- paste(x7,"[,",PC1,"]",sep="")
           x9 <- paste(x7,"[,",PC2,"]",sep="")
           x10 <- eval(parse(text=x8))
           x11 <- eval(parse(text=x9))
           
           x12 <- c(x4,x10)
           x13 <- c(x5,x11)
           
           bivn.kde <- kde2d(x12, x13, n = grid_size)
           
           #col1 = c(rep(2,length(x5)), rep(3, length(x11)))
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of WETs and RETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         int={
           x1 <- paste("PCA",fact,"int",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp3d(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of IETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         }
  )
}

#################################################### PCA_density_plotter_3D_all_static ####################################################
PCA_density_plotter_3D_all_static <- function(fact, expression_type, PC1, PC2, grid_size){
  switch(expression_type,
         r={
           x1 <- paste("PCA",fact,"r",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of RETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         w={
           x1 <- paste("PCA",fact,"w",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of WETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         n={
           x1 <- paste("PCA",fact,"n",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of NETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         w_r={
           x1 <- paste("PCA",fact,"w",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           
           x7 <- paste("PCA",fact,"r",sep="_")
           x8 <- paste(x7,"[,",PC1,"]",sep="")
           x9 <- paste(x7,"[,",PC2,"]",sep="")
           x10 <- eval(parse(text=x8))
           x11 <- eval(parse(text=x9))
           
           x12 <- c(x4,x10)
           x13 <- c(x5,x11)
           
           bivn.kde <- kde2d(x12, x13, n = grid_size)
           
           #col1 = c(rep(2,length(x5)), rep(3, length(x11)))
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of WETs and RETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         },
         int={
           x1 <- paste("PCA",fact,"int",sep="_")
           x2 <- paste(x1,"[,",PC1,"]",sep="")
           x3 <- paste(x1,"[,",PC2,"]",sep="")
           x4 <- eval(parse(text=x2))
           x5 <- eval(parse(text=x3))
           bivn.kde <- kde2d(x4, x5, n = grid_size)
           col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]
           persp(x=bivn.kde, col = col1,
                   main = paste(fact," density plot of IETs over PC",PC1,"and PC",PC2, sep=" "),
                   xlab = paste("PC",PC1), ylab = paste("PC",PC2), zlab = "density")
         }
  )
}


################################################### PCA_correlation_calculator ########################################################
PCA_correlation_calculator <- function(fact,type,no_of_PCs){
  #x0 <- paste(fact,type,"cor_table",sep="_")
  x1 <- paste(fact,type,sep="_")
  x1_eval <- eval(parse(text=x1))
  len <- ncol(x1_eval)
  cor <- rep(0,len)
  x2 <- paste("PCA",x1,sep="_")
  x3 <- paste(x2,"scores",sep="$")
  names <- ""
  for(i in 1:no_of_PCs){
    x4 <- paste(x3,"[,",i,"]",sep="")
    x4_eval <- eval(parse(text=x4))
    cor <- rbind(cor,cor(x4_eval,x1_eval))
  }
  cor <- t(cor[-1,])
  cor
}

###################################################### PCA_correlation_calculator_all #################################################


PCA_correlation_calculator_all <- function(fact,no_of_PCs){
  #x0 <- paste(fact,type,"cor_table",sep="_")
  x1 <- paste(fact,"Cell_wise",sep="_")
  x1_eval <- eval(parse(text=x1))
  x2_eval=x1_eval[,-ncol(x1_eval)]
  len <- ncol(x2_eval)
  cor <- rep(0,len)
  x3 <- paste("PCA",fact,sep="_")
  names <- ""
  for(i in 1:no_of_PCs){
    
    x4 <- paste(x3,"scores",sep="$")
    x5 <- paste(x4,"[,",i,"]",sep="")
    x5_eval <- eval(parse(text=x5))
    cor <- rbind(cor,cor(x5_eval,x2_eval))
  }
  cor <- t(cor[-1,])
  cor
}

######################################################## PCA_correlation_calculator_all_2 ###############################################
PCA_correlation_calculator_all_2 <- function(comb,fact,type,no_of_PCs){
  x0 <- paste(fact,type,sep="_")
  x0_eval <- eval(parse(text=x0))
  x1 <- paste(comb,type,sep="_")
  #x1_eval <- eval(parse(text=x1))
  len <- ncol(x0_eval)
  cor <- rep(0,len)
  x2 <- paste("PCA",x1,sep="_")
  names <- ""
  for(i in 1:no_of_PCs){
    x3 <- paste(x2,"[,",i,"]",sep="")
    x3_eval <- eval(parse(text=x3))
    cor <- rbind(cor,cor(x3_eval,x0_eval))
  }
  cor <- t(cor[-1,])
  cor
}

################################################## PCA_correlation_calculator_all_3 #####################################################
PCA_correlation_calculator_all_3 <- function(comb,fact,no_of_PCs){
  x00 <- paste(fact,"Cell_wise",sep="_")
  #x0<- eval(parse(text=paste(x00,"[-ncol",fact,"]")))
  x0<- paste(x00,"[,-ncol(",fact,"_Cell_wise)]", sep="")
  x0_eval <- eval(parse(text=x0))
  x1 <- paste(comb,"$scores",sep="")
  #x1_eval <- eval(parse(text=x1))
  len <- ncol(x0_eval)
  cor <- rep(0,len)
  x2 <- paste("PCA",x1,sep="_")
  names <- ""
  for(i in 1:no_of_PCs){
    x3 <- paste(x2,"[,",i,"]",sep="")
    x3_eval <- eval(parse(text=x3))
    cor <- rbind(cor,cor(x3_eval,x0_eval))
  }
  cor <- t(cor[-1,])
  cor
}
