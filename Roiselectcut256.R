# pat,grid_x
Roiselectcut=function(pat,
grid_x,
start_x=1,
start_y=1,
w=0,
h=0,
end_x=w-grid_x+1,
end_y=h-grid_y+1,
grid_y=grid_x,
outpat=pat,
roipath=file.path(pat,paste0('RoiSelectOf',grid_x,'x',grid_y,'.csv')),
startcount=1){
    nam=tail(strsplit(pat,'/')[[1]],1)[1]
    tif=dir(pat,'*.tif')
    as.numeric(gsub('[A-Za-z.]+','0',gsub('R-[0-9]+-([0-9]+)-[0-9]+.tif','\\1',tif)))->f
    imax=which.max(f)
    if(w == 0 || h ==0){
        im=EBImage::readImage(file.path(pat,tif[imax]))
        w=nrow(im)
        h=ncol(im)
    }
    if(end_x <= 0 || end_y <=0){
        end_x=w-grid_x+1
        end_y=h-grid_y+1
    }
    im=array(0,c(w,h,length(tif)))
    for(i in 1:length(tif)){
        im[,,i]=EBImage::resize(EBImage::readImage(file.path(pat,tif[i])),w=w,h=h)
    }
    pmean=1.2*mean(im[,,imax])
    pmax=0.8*max(im[,,imax])
    roi=data.frame(x1=0,x2=0,y1=0,y2=0)
    for(i_x in seq(start_x,end_x,grid_x)){
        for(i_y in seq(start_y,end_y,grid_y)){
            tim=im[i_x:(i_x+grid_x-1),i_y:(i_y+grid_y-1),imax]
            mmean=mean(tim)
            mmedian=median(tim)
            if(mmean>=pmean && mmean<=pmax && 0.8*mmean>mmedian && mmedian>= 0.1*mmean){
                x1=i_x
                x2=(i_x+grid_x-1)
                y1=i_y
                y2=i_y+grid_y-1
                roi=rbind(roi,c(x1,x2,y1,y2))
            }
        }
    }
    roi=roi[-1,]
    write.table(roi,roipath,sep=',',row.names=FALSE)
    
    for(i in 1:length(roi$x1)){
        outp=file.path(outpat,as.character(startcount+i-1))
        dir.create(outp,recursive = TRUE)
        for(f in 1:length(tif)){
            EBImage::writeImage(im[roi$x1[i]:roi$x2[i],roi$y1[i]:roi$y2[i],f],file.path(outp,tif[f]))
        }
        cat(paste0("Tiff files from ",pat," , roi index is ",i," ."),file=file.path(outp,paste0(nam,"-statements.txt")))
    }
    startcount+i-1
}

grid_x=256
grid_y=grid_x
pat="/media/chin/Chin/ExpData/ExpdataTemp/格式化/ToLHJ/data/20220113/Uncut/COS-7-1nMCy3B-sphere-Deoxy-2"
out="/media/chin/Chin/ExpData/ExpdataTemp/格式化/ToLHJ/data/20220113/Cut/COS-7-1nMCy3B-sphere-Deoxy-2"
startcount=1770

    startcount=1+Roiselectcut(pat=file.path(pat),grid_x=256,grid_y=256,start_x=grid_x/4+1,start_y=grid_y/4+1,outpat=out,startcount=startcount)

