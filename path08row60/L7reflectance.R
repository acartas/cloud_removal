#!/usr/bin/env Rscript

#############################################################################
#Convert Landsat DNs to TOA reflectance
#############################################################################
library(sp)
library(methods)
library(raster)
library(multicore)

########### Set directories and get shell variables ########################

dir<-sprintf("/home/sam/Landsat_data/")
setwd(dir)
date<-as.integer(Sys.getenv("date"))
sun_el<-as.numeric(Sys.getenv("sun_el"))
write_dir<-Sys.getenv("dir_name")
scenes<-as.integer(Sys.getenv("scenes"))
current<-as.integer(Sys.getenv("count"))
current<-current+1
julian<-as.matrix(read.table(sprintf("%sjulian.txt",dir)))
calibration<-as.matrix(read.table(sprintf("%sL7calib.txt",dir)))
b=matrix(nrow=8,ncol=1)
  for(i in 0:7){
    c=i+1
    a=sprintf("b%s",i)
    b[c,1]=Sys.getenv(a)
    }
d<-as.numeric(julian[date,2])
d<-d*d
sza<-cos((pi/180)*(90-sun_el))
name<-Sys.getenv("name")

###############################################################################
sprintf("...processing scene %d of %d",current,scenes)

channel1<-raster(sprintf("%stemp/%s",dir,b[1]))
channel2<-raster(sprintf("%stemp/%s",dir,b[2]))
channel3<-raster(sprintf("%stemp/%s",dir,b[3]))
channel4<-raster(sprintf("%stemp/%s",dir,b[4]))
channel5<-raster(sprintf("%stemp/%s",dir,b[5]))
channel6<-raster(sprintf("%stemp/%s",dir,b[6]))
channel7<-raster(sprintf("%stemp/%s",dir,b[7]))
channel8<-raster(sprintf("%stemp/%s",dir,b[8]))

#### Function for converting DNs to TOA reflectance for 6 spectral bands ####
ref<-function(x,g,b,d2,s,Esun,filename) {
	out<-raster(x)
	bs<-blockSize(out)
	out<-writeStart(out, filename, overwrite=TRUE, format="GTiff", datatype="INT2U", options=c("COMPRESS=NONE", "TFW=NO"))
	for (i in 1:bs$n) {
		v<-getValues(x,row=bs$row[i], nrows=bs$nrows[i])
		v<-(g*v+b)
		c<-(v>0)
		v<-(((pi*v*d2)/(Esun*s))*c)*10000
		writeValues(out,v,bs$row[i])
	}
	out<-writeStop(out)
	return(out)
}


### Function for computing thermal band with high gain ###
tempH<-function(x,filename) {
	out<-raster(x)
	bs<-blockSize(out)
	out<-writeStart(out, filename, overwrite=TRUE, format="GTiff", datatype="INT1U", options=c("COMPRESS=NONE", "TFW=NO"))
	for (i in 1:bs$n) {
		v<-getValues(x,row=bs$row[i], nrows=bs$nrows[i])
		c<-(v>0)
		v<-(((1282.71/log(666.09/((0.037205*v+3.16)+1)))-100))*c
		writeValues(out,v,bs$row[i])
	}
	out<-writeStop(out)
	return(out)
	
}

### Function for computing thermal band with low gain ###
tempL<-function(x,filename) {
	out<-raster(x)
	bs<-blockSize(out)
	out<-writeStart(out, filename, overwrite=TRUE, format="GTiff", datatype="INT1U", options=c("COMPRESS=NONE", "TFW=NO"))
	for (i in 1:bs$n) {
		v<-getValues(x,row=bs$row[i], nrows=bs$nrows[i])
		c<-(v>0)
		v<-(((1282.71/log(666.09/((0.067087*v-0.07)+1)))-100))*c
		writeValues(out,v,bs$row[i])
	}
	out<-writeStop(out)
	return(out)
	
}

### Average thermal bands ###
avgtemp<-function(x,y,filename) {
	out<-raster(x)
	bs<-blockSize(out)
	out<-writeStart(out, filename, overwrite=TRUE, format="GTiff", datatype="INT1U", options=c("COMPRESS=NONE", "TFW=NO"))
	for (i in 1:bs$n) {
		v1<-getValues(x,row=bs$row[i], nrows=bs$nrows[i])
		v2<-getValues(y,row=bs$row[i], nrows=bs$nrows[i])
		c<-(v>0)
		v<-((v1+v2)/2)*c
		writeValues(out,v,bs$row[i])
	}
	out<-writeStop(out)
	return(out)
	
}	


### Call functions for multi-processing ###
o<-parallel(tempH(channel6,filename=sprintf("%sprocessed/%s/band6H_%s",dir,write_dir,name)))
p<-parallel(ref(channel1,calibration[1,1],calibration[2,1],d,sza,calibration[3,1],filename=sprintf("%sprocessed/%s/band1_%s",dir,write_dir,name)))
q<-parallel(ref(channel2,calibration[1,2],calibration[2,2],d,sza,calibration[3,2],filename=sprintf("%sprocessed/%s/band2_%s",dir,write_dir,name)))
r<-parallel(ref(channel3,calibration[1,3],calibration[2,3],d,sza,calibration[3,3],filename=sprintf("%sprocessed/%s/band3_%s",dir,write_dir,name)))
collect(list(o,p,q,r))

o<-parallel(ref(channel4,calibration[1,4],calibration[2,4],d,sza,calibration[3,4],filename=sprintf("%sprocessed/%s/band4_%s",dir,write_dir,name)))
p<-parallel(ref(channel5,calibration[1,5],calibration[2,5],d,sza,calibration[3,5],filename=sprintf("%sprocessed/%s/band5_%s",dir,write_dir,name)))
q<-parallel(ref(channel8,calibration[1,8],calibration[2,8],d,sza,calibration[3,8],filename=sprintf("%sprocessed/%s/band7_%s",dir,write_dir,name)))
r<-parallel(tempL(channel7,filename=sprintf("%sprocessed/%s/band6L_%s",dir,write_dir,name)))
collect(list(o,p,q,r))