
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# Basic non-parameric Bootstrap
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
library(scales) # For plotting
# Setwd
setwd(" ")
# read data
dat.raw = read.csv("plastic.density.csv")
samples = unique(dat.raw$sample)
samples
results = NULL

# choose sample
nboots = 10000
for(i in 1:length(samples)){
  dat = subset(dat.raw,sample==samples[i]) 
  dat$type <- as.factor(dat$type)
  types = unique(dat$type) 
  
  #-----------------------------
  # Do non-parametric bootsrap
  #-----------------------------
  
  boots  =  NULL # objects
  mean_diff <- vector(,nboots)
  median_diff <- vector(,nboots)
  for(k in 1:nboots){
    # Subset type j
    mu.temp = NULL # random data object
    dat.samp = dat
    dat.samp$type <- sample(dat.samp$type,size = length(dat.samp$type),replace = F)
    mean_diff[k] <- diff(tapply(dat.samp$density,dat.samp$type,mean))
    median_diff[k] <- diff(tapply(dat.samp$density,dat.samp$type,median))
    
    for(j in 1:length(types)){
      # resample vector by type with replace and take mean
      mu.temp =  c(mu.temp,mean(sample(dat[dat$type==types[j],3],replace=TRUE)))
    }
    boots = rbind(boots,as.numeric(mu.temp))
  }
  boots= data.frame(boots)
  colnames(boots) = paste(types)
  
  # get stats 
  median = apply(boots,2,median)
  mean = apply(boots,2,mean)
  se = apply(boots,2,sd)
  lci = apply(boots,2,quantile,c(0.025))
  uci = apply(boots,2,quantile,c(0.975))
  
  org_diff_mean <- diff(tapply(dat$density,dat$type,mean))
  org_diff_median <- diff(tapply(dat$density,dat$type,median))
  
  out = data.frame(sample=samples[i],type=types, mean,median,se,lci,uci,`p(out<inside)` = c(mean(median_diff > org_diff_median)))
  results = rbind(results,out)
  
  cols = c(4,2,3,5,6,7)
  Par = list(mfrow=c(1,1),mar = c(5, 5, 1, 1), mgp =c(3,1,0),mai = c(0.7, 0.7, 0.1, 0.1),mex=0.8, tck = -0.02,cex=0.7)
  png(file = paste0("Bootstrap_",samples[i],".png"), width = 4.5, height = 4, 
      res = 200, units = "in")
  par(Par)
  Ymax = Xlim = NULL
  for(j in 1:2){
    dens = stats:::density(boots[,j],adjust=2)
    Ymax = max(c(Ymax,dens$y))
  }
  xlim = range(boots)
  plot(1,1,type="n",xlab=samples[i],ylab="Density",ylim=c(0,Ymax),xlim=xlim,log='x')
  for(j in 1:length(types)){
    dens = stats:::density(boots[,j],adjust=2)
    polygon(c(dens$x,rev(dens$x)),c(dens$y,rep(0,length(dens$y))),col=alpha(cols[j],0.3))
    lines(rep(mean[j],2),c(0,max(dens$y)),col=cols[j],lwd=2)
  }
  legend("topright",paste(types),pch=22,pt.bg=cols,pt.cex=2,cex=1.1,bty="n")
  dev.off()
  write.csv(boots, paste0(getwd(),"/",samples[i],"_boots.csv"),row.names=F)
}

# confirm with R bootstrap package
bootmean <- function(data,index){
  d <- data[index,]
  tapply(d$density,d$type,mean)
}

bootres <- boot::boot(dat,bootmean,R=10000)
boot::boot.ci(bootres,index=c(1,1))
boot::boot.ci(bootres,index=c(2,2))

write.csv(results,"boot.results.csv",row.names = F)

### 
