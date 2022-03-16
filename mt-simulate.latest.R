mtSimulate=function(dense=5){
  mtNum=ceiling(dense+rnorm(1)+runif(1,-1*dense*0.5,dense*0.5))
  interval=0.005
  count=500
  sd=interval/50
  d=0
  for(i in 1:mtNum){
    d=c(d,rep(i,count))
  }
  d=d[-1]
  d=data.frame(x=0,y=0,mt=d)
  for(i in 1:mtNum){
    a=runif(1,0,2*pi)
    d$x[d$mt==i][1]=sqrt(2)*cos(a)
    d$y[d$mt==i][1]=sqrt(2)*sin(a)
    # a=runif(1,0,2*pi)
    d$x[d$mt==i][2]=cos(a)*(sqrt(2)-interval)
    d$y[d$mt==i][2]=sin(a)*(sqrt(2)-interval)
    for(c in 3:count){
      d$x[d$mt==i][c]=2*d$x[d$mt==i][c-1]-d$x[d$mt==i][c-2]
      d$y[d$mt==i][c]=2*d$y[d$mt==i][c-1]-d$y[d$mt==i][c-2]
      d$x[d$mt==i][c]=d$x[d$mt==i][c]+rnorm(1,0,sd)
      d$y[d$mt==i][c]=d$y[d$mt==i][c]+rnorm(1,0,sd)
    }
  }
  # d$x=d$x-min(d$x)
  # d$y=d$y-min(d$y)
  # d$x=d$x/max(d$x)
  # d$y=d$y/max(d$y)
  # plot(d$x[-1<=d$x & d$x <=1 & -1<=d$y & d$y<=1],d$y[-1<=d$x & d$x <=1 & -1<=d$y & d$y<=1],pch='.',asp=1)
  plot(d$x,d$y,pch='.',asp=1)
  data.frame(x=d$x[-1<=d$x & d$x <=1 & -1<=d$y & d$y<=1],y=d$y[-1<=d$x & d$x <=1 & -1<=d$y & d$y<=1])
}

setwd("/media/chin/Chin/ExpData/WF-PAINT/SimulatePAINT/20220216/simulate")
library("EBImage")
source("render.R")
source("resizeandmove.R")
setwd("/media/chin/Chin/ExpData/WF-PAINT/SimulatePAINT/20220216/complex4")
gb=160/128
for(i in 30559:40001){
  if(dir.exists(as.character(i)))
    break()
  dir.create(as.character(i))
  print(paste("Processing",i,"."))
  d=mtSimulate(runif(1,5,20))
  im=render(d$x,d$y,256,256)
  EBImage::writeImage(im,file.path(i,"gt.tif"))
  sd=0.003
  im=render(d$x+rnorm(length(d$x),0,sd),d$y+rnorm(length(d$y),0,sd),256,256)
  im=EBImage::gblur(im,gb)
  EBImage::writeImage(im,file.path(i,"30000R.tif"))
  im=EBImage::resize(im,16)
  EBImage::writeImage(resize(im,256),file.path(i,"WF.tif"))
  im=EBImage::resize(im,256)
  EBImage::writeImage(im,file.path(i,"WF-chazhi.tif"))
  # 3000R
  a=round(runif(length(d$x)/10,1,length(d$x)))
  d=data.frame(x=d$x[a],y=d$y[a])
  im=render(d$x+rnorm(length(d$x),0,sd),d$y+rnorm(length(d$y),0,sd),256,256)
  EBImage::writeImage(EBImage::gblur(im,gb),file.path(i,"3000R.tif"))
  a=round(runif(length(d$x)/3*2,1,length(d$x)))
  d=data.frame(x=d$x[a],y=d$y[a])
  im=render(d$x+rnorm(length(d$x),0,sd),d$y+rnorm(length(d$y),0,sd),256,256)
  EBImage::writeImage(EBImage::gblur(im,gb),file.path(i,"2000R.tif"))
  a=round(runif(length(d$x)/2,1,length(d$x)))
  d=data.frame(x=d$x[a],y=d$y[a])
  im=render(d$x+rnorm(length(d$x),0,sd),d$y+rnorm(length(d$y),0,sd),256,256)
  EBImage::writeImage(EBImage::gblur(im,gb),file.path(i,"1000R.tif"))
}
