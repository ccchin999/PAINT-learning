args=commandArgs(TRUE)
vpat=args[1]
vopat=args[2]
print("Please give raw_dirctory_path and output_path.")

# pat,frame,scale,outpat
PicassoOfFrames=function(pat,frames,scale,
hdf5File=dir(pat,'.*_render.hdf5',full.names=T)[1],
yamlFile=dir(pat,'.*_render.yaml',full.names=T)[1],
#wfdriftFile=tail(dir(file.path(pat,'..'),'.*WFdrift.yaml',full.names=T),1),
wfdriftFile=file.path(pat,'..','WFdrift.yaml'),
driftFile=tail(dir(pat,'.*_drift.txt',full.names=T),1),
wfFile=gtools::mixedsort(dir(file.path(pat,'..'),'[0-9]+.tif',full.names=T))[1],
srrfFile=dir(file.path(pat,'..'),recursive=T,full.names=T)[grep(' - SRRF',dir(file.path(pat,'..'),recursive=T))],
wforiFile=dir(file.path(pat,'..'),recursive=T,full.names=T)[sum(grep('(WideField|WF).*\\.tif$',dir(file.path(pat,'..'),recursive=T)))-sum(grep('(WideField|WF).*SRRF\\.tif$',dir(file.path(pat,'..'),recursive=T)))],
picassoFile=dir(pat,'.*_render.*.png',full.names=T),
outpat=pat,
throw=0,
startfs=0,
gb=10*scale/128,
tiffFile=file.path(outpat,paste0('R-',startfs,'-',frames,'-',scale,'.tif'))){
source("/media/chin/Chin/ExpData/ExpdataTemp/格式化/resizeandmove.R")
dir.create(outpat,showWarnings = FALSE, recursive = TRUE)
library('hdf5r')
yaml=configr::read.config(file=yamlFile)
w=yaml$Width*scale
h=yaml$Height*scale
wfn=as.numeric(tail(strsplit(wfFile,'[/.]')[[1]],2)[1])
drift=read.table(file=driftFile,header=FALSE,sep=' ')
drift=data.frame(x=drift$V1[wfn+1],y=drift$V2[wfn+1])
hdf5=H5File$new(hdf5File,mode="r")
hdf5$open("locs")->locs
print(paste0("Precessing ",hdf5File," now."))
if(length(frames)==1 && length(startfs)==1){
    locs=locs[startfs[1] <= locs[]$frame & locs[]$frame < (startfs[1]+frames[1])]
    locs=data.frame(f=locs$frame,x=locs$x,y=locs$y)
} else{
    locs=data.frame(f=locs[]$frame,x=locs[]$x,y=locs[]$y)
}
hdf5$close()
locs$x=scale*(locs$x+drift$x[1])
locs$y=scale*(locs$y+drift$y[1])
locs=data.frame(f=locs$f[w>= locs$x & locs$x > 0 & h>= locs$y & locs$y > 0],x=locs$x[w>= locs$x & locs$x > 0 & h>= locs$y & locs$y > 0],y=locs$y[w>= locs$x & locs$x > 0 & h>= locs$y & locs$y > 0])
for(startf in startfs){
    for(frame in frames){
        tiffFile=file.path(outpat,paste0('R-',startf,'-',frame,'-',scale,'.tif'))
        print(paste0("Precessing ",tiffFile," now."))
        dlocx=locs$x[startf <= locs$f & locs$f < (startf+frame)]
        dlocy=locs$y[startf <= locs$f & locs$f < (startf+frame)]        
        im=matrix(0,w,h)
        for(i in 1:length(dlocx)){
            im[ceiling(dlocx[i]),ceiling(dlocy[i])]=im[ceiling(dlocx[i]),ceiling(dlocy[i])]+1
        }
        ord=order(im)
        zero=length(im[im==0])
        im[ord[(zero+1):(zero+throw*(length(im)-zero))]]=0
        im[ord[ceiling(zero+(1-throw)*(length(im)-zero)):length(im)]]=im[ord[floor(zero+(1-throw)*(length(im)-zero))]]
        bl=max(im)
        #bl=2*mean(im[im>0])
        print(paste0("Drawing image."))
        EBImage::writeImage(EBImage::gblur(im,gb)/bl,tiffFile)
    }
}
wfpdx=0
wfpdy=0
wfdx=0
wfdy=0
wff=1
if(file.exists(wfdriftFile)){
    wfdrift=configr::read.config(file=wfdriftFile)
    if(scale==as.numeric(wfdrift$scale)){
        wfpdx=as.numeric(wfdrift[which(names(wfdrift)==paste0(wfn,'dx'))])
        wfpdy=as.numeric(wfdrift[which(names(wfdrift)==paste0(wfn,'dy'))])
        wfdx=as.numeric(wfdrift$WideFielddx)
        wfdy=as.numeric(wfdrift$WideFielddy)
        wff=as.numeric(wfdrift$WideFieldframe)
    }
}
print("Precessing WideField images now.")
WideField=EBImage::readImage(wfFile)
EBImage::writeImage(move(resize(WideField,w=w,h=h),wfpdx,wfpdy),file.path(outpat,'WideField.tif'))
EBImage::writeImage(move(EBImage::resize(WideField,w=w,h=h),wfpdx,wfpdy),file.path(outpat,'WideField-chazhi.tif'))
EBImage::writeImage(move(EBImage::resize(EBImage::readImage(srrfFile),w=w,h=h),wfdx,wfdy),file.path(outpat,'SRRF.tif'))
EBImage::writeImage(move(EBImage::resize(EBImage::channel(EBImage::readImage(picassoFile),'grey'),w=w,h=h),-drift$x[1]*scale,-drift$y[1]*scale),file.path(outpat,'picasso.ori.tif'))
WideField=EBImage::readImage(wforiFile)[,,wff]
EBImage::writeImage(move(resize(WideField,w=w,h=h),wfdx,wfdy),file.path(outpat,'WideField.ori.tif'))
EBImage::writeImage(move(EBImage::resize(WideField,w=w,h=h),wfdx,wfdy),file.path(outpat,'WideField.ori-chazhi.tif'))
cat(paste0("WideField image from ",wfFile," and ",wforiFile," was scaled to ",scale," times. WideField(.ori)-chazhi was scaled using EBImage. SRRF image from ",srrfFile," and picasso.ori file was from",picassoFile," . They was scaled using EBImage. ",throw," of PAINT pixels was thrown when rendering ",tiffFile," . Drift file of WideField image and PAINT images is ",wfdriftFile," ."),file=file.path(outpat,'Statements.txt'))
}


frames=30000
PicassoOfFrames(pat=vpat,frame=frames,scale=16,outpat=vopat,throw=0.01)
frames=c(10,100,1000,3000)
startframes=c(0,5000,10000,15000,20000,25000)
PicassoOfFrames(pat=vpat,startfs=startframes,frames=frames,scale=16,outpat=vopat)

