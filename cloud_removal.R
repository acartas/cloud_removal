library(raster)

filenames <- list.files("path08row60", full.names=TRUE)

#try to read in the .jpg 3 bands #stack read multiple files, brick only reads one.
r1 <- stack(filenames[1]) 
r2 <- stack(filenames[2])

rStack <- sapply(filenames,stack)


keep <- c(1)
nokeep <- vector()
numFiles <- length(rStack)

#can't figure out stupid apply function!!! loops it is!
for (cur in c(2:numFiles)){
  if (rStack[[1]]@extent == rStack[[cur]]@extent)
    keep <- c(keep,cur)
}

#discard any image with different extent
rStack <- rStack[keep]
sprintf("Using %d files for image collage.",num)

#display all images being used
dispRow <- floor(sqrt(length(rStack)))
dispCol <- ceiling(length(rStack)/nRow)
par(mfrow=c(dispRow,dispCol))
lapply(rStack,plotRGB)
dev.off()

#create blank stack for results
outR <- raster(ext=rStack[[1]]@extent)
outG <- raster(ext=rStack[[1]]@extent)
outB <- raster(ext=rStack[[1]]@extent)

#row by row, grab the second-highest value and put it into output.
nRow = nrow(rStack[[1]])
nCol = ncol(rStack[[1]])
for (i in c(1:nRow)){
  curRowValues <- lapply(rStack,values,row=i) #stores RGB for row 1 for all images  
  #proceed through the row
  for (j in c(1:nCol)){
    #curRowValues[[j]] is an nCol x 3 matrix
      
  }
}