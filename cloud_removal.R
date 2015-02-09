library(raster)

filenames <- list.files("path08row60", full.names=TRUE)

#try to read in the .jpg 3 bands #stack read multiple files, brick only reads one.
rStack <- sapply(filenames,stack)


keep <- c(1)
numFiles <- length(rStack)

#can't figure out stupid apply function!!! loops it is!
for (cur in c(2:numFiles)){
  if (rStack[[1]]@extent == rStack[[cur]]@extent)
    keep <- c(keep,cur)
}

#discard any image with different extent
rStack <- rStack[keep]
numFiles <- length(rStack)
sprintf("Using %d files for image collage.",numFiles)

#display all images being used (just for information, not needed!)
#dispRow <- floor(sqrt(numFiles))
#dispCol <- ceiling(numFiles/dispRow)
#par(mfrow=c(dispRow,dispCol))
#invisible(sapply(rStack,plotRGB))
#dev.off()

#row by row, grab the second-highest value and put it into output.
nRow = nrow(rStack[[1]])
nCol = ncol(rStack[[1]])

#create blank stack for results
outR <- matrix(nrow=nRow,ncol=nCol)#raster(ext=rStack[[1]]@extent, nrows=nRow, ncols=nCol)
outG <- matrix(nrow=nRow,ncol=nCol)#raster(ext=rStack[[1]]@extent, nrows=nRow, ncols=nCol)
outB <- matrix(nrow=nRow,ncol=nCol)#raster(ext=rStack[[1]]@extent, nrows=nRow, ncols=nCol)

#should be 1:nRow and 1:nCol, set smaller for testing!
for (curRow in c(4000:4020)){
  
  curRowValues <- lapply(rStack,values,row=curRow) #stores RGB for row 1 for all images  
  for (curCol in c(2000:2200)){#proceed through column-by-column
    red <- vector(mode="integer")
    blue <- vector(mode="integer")
    green <- vector(mode="integer")
    for (curFile in c(1:numFiles)){ #gather values for current cell for all images
      curRed   = curRowValues[[curFile]][[curCol,1]]
      red <- c(red,curRed)
      curGreen = curRowValues[[curFile]][[curCol,2]]
      green <- c(green,curGreen)
      curBlue  = curRowValues[[curFile]][[curCol,3]]
      blue <- c(blue,curBlue)
    }
    outR[curRow,curCol] <- sort(red,partial=numFiles-1)[numFiles-1]
    outG[curRow,curCol] <- sort(green,partial=numFiles-1)[numFiles-1]
    outB[curRow,curCol] <- sort(blue, partial=numFiles-1)[numFiles-1]
    cat("Row ",curRow," Col ",curCol,"\ --> "); 
    cat("Red: ", outR[curRow,curCol], " Green: ", outG[curRow,curCol], " Blue: ", outB[curRow,curCol],"\n")
  }
}

#convert final output to plot/file (currently only testing with subset)
outfilename = paste("path08row60/output_",as.integer(Sys.time()),".jpg",sep="")
file.copy(filenames[1], oufilename)
rasR = raster(outR,template=rStack[[1]])
rasG = raster(outG,template=rStack[[1]])
rasB = raster(outB,template=rStack[[1]])

