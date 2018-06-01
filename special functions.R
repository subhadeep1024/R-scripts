
################################################ PCA_density_plotter_3D_all_static #####################################################
PCA_density_plotter_3D_all_static <- function(fact, expression_type, PC1, PC2, grid_size, r, theta, phi , d){
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
                 xlab = paste("PC",PC1), ylab = paste("PC",PC2), theta=theta,phi=phi,border= NA, 
                 box=TRUE, shade=0.1, ticktype = "detailed", nticks = 4,d = d, zlab=NA
                 )
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
                 xlab = paste("PC",PC1), ylab = paste("PC",PC2), theta=theta,phi=phi,border= NA, 
                 box=TRUE, shade=0.1, ticktype = "detailed", nticks = 4,d = d, zlab=NA
                 )
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
                 xlab = paste("PC",PC1), ylab = paste("PC",PC2),theta=theta,phi=phi,border= NA, 
                 box=TRUE, shade=0.1, ticktype = "detailed", nticks = 4,d = d, zlab=NA
                 )
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
                 xlab = paste("PC",PC1), ylab = paste("PC",PC2),theta=theta,phi=phi,border= NA, 
                 box=TRUE, shade=0.1, ticktype = "detailed", nticks = 4,d = d, zlab=NA
                 )
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
                 xlab = paste("PC",PC1), ylab = paste("PC",PC2),theta=theta,phi=phi,border= NA, 
                 box=TRUE, shade=0.1, ticktype = "detailed", nticks = 4,d = d, zlab=NA
                 )
         }
  )
}

###################################################### PCA_density_plotter_all ##############################################################################

PCA_density_plotter_all <- function(fact, PC, adjust, xlim = c("-100","50"), titleadjust=0.5, ysize=40, xsize= 40){
  
  my.limits = as.numeric(xlim)
  #### for rarely expressed ##
  x1 <- paste("PCA",fact,"r",sep="_")
  x2 <- paste(x1,"[,",PC,"]",sep="")
  x3 <- eval(parse(text=x2))
  
  #### for widely expressed ##
  x4 <- paste("PCA",fact,"w",sep="_")
  x5 <- paste(x4,"[,",PC,"]",sep="")
  x6 <- eval(parse(text=x5))
  
  #### for not expressed ##
  x7 <- paste("PCA",fact,"n",sep="_")
  x8 <- paste(x7,"[,",PC,"]",sep="")
  x9 <- eval(parse(text=x8))
  
  #### for intermediately expressed ####
  x10 <- paste("PCA",fact,"int",sep="_")
  x11 <- paste(x10,"[,",PC,"]",sep="")
  x12 <- eval(parse(text=x11))
  
  z <- c(x3,x6,x9,x12)
  #z <- c(x3,x6)
  z1 <- data.frame(PCs = z, 
                   exp_type= c(rep("RETs",nrow(PCA_H3k9ac_r)),
                               rep("WETs",nrow(PCA_H3k9ac_w)),
                               rep("NETs",nrow(PCA_H3k9ac_n)),
                               rep("Intermediate",nrow(PCA_H3k9ac_int))
                   ))
  
  label <- paste(fact,"PC",PC,sep="_")
  
  p <- ggplot(z1, aes(PCs,colour = exp_type)) + geom_density(
    size=1, adjust = adjust)+labs(x=label, y=NULL)+xlim(my.limits)
  
  p+theme(
        axis.text.x = element_text(size=xsize), axis.text.y = element_text(size=ysize),
        axis.title.x = element_text(size=25),title = element_text(size = 20, face = "bold"), 
        legend.text = element_text(size=40),
        legend.title = element_text(size = 40, vjust = -20),
        legend.key.height = unit(4, 'lines'),
        legend.key.width = unit(4, 'lines'),
        plot.title = element_text(hjust= titleadjust))
  #sm.density.compare(z1$PCs, z1$exp_type, 
  #                  col= c("Red","Green", "black","blue"), 
  #                 lty=c(1,1,1,1), xlab=paste(fact,"PC",PC,sep="_"))
  
  #legend("top",levels(as.factor(z1$exp_type)), 
  #      fill=c("Red","Green","Black","Blue"),
  #     title=paste("PC",PC))
}
