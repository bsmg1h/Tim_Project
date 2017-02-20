library(dplyr)
library(data.table)
library(ggplot2)
library(jpeg)
library(png)
library(ReadImages)


plot_jpg = function(jpg, add=FALSE){
    res = dim(jpg)[1:2] # get the resolution
    if (!add) # initialize an empty plot area if add==FALSE
        plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',
             yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
}

make_jpg_from_vector = function(v,nolayer=c()){
    v <- as.numeric(v)
    len <- length(v)
    res <- sqrt(len/3)
    output <- array(dim = c(res,res,3))
    output[,,1]<-matrix(v[1:res^2],nrow = res,byrow = T)
    output[,,2]<-matrix(v[(res^2+1):(2*res^2)],nrow = res,byrow = T)
    output[,,3]<-matrix(v[(res^2*2+1):(3*res^2)],nrow = res,byrow = T)
    output[,,nolayer] <- 1
    output
}

Xtr <- fread("Xtr.csv",data.table = F)
Xtr <- Xtr[,1:3072]

transform_jpg = function(Xtr,strength){
    Xtr <- abs(Xtr)^(1/strength)*sign(Xtr)
    Xtr <- (Xtr-min(Xtr))/(max(Xtr)-min(Xtr))
    Xtr
}

Xtr[5000,] %>%
    make_jpg_from_vector(nolayer = c()) %>% 
    plot_jpg()




