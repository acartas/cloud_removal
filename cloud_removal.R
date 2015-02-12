library(raster)

filenames <- list.files("path08row60", full.names=TRUE)

#try to read in the .jpg 3 bands #stack read multiple files, brick only reads one.
rStack <- sapply(filenames,stack)

#outputfile that I will be making changes to. Copy of first file for simplicity!
outfilename = paste("path08row60/output_",as.integer(Sys.time()),".jpg",sep="")
file.copy(filenames[1], outfilename)
outStack <- stack(outfilename)

keep <- c(1)
numFiles <- length(rStack)

#Use only files with matching extents (easier than lining them up for now)
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

outValues = array(dim=c(nRow,nCol,3))

#should be 1:nRow and 1:nCol, set smaller for testing!
for (curRow in c(4000:4050)){
  
  curRowValues <- lapply(rStack,values,row=curRow) #stores RGB for row 1 for all images  
  #newRowValues <- matrix(ncol=3, nrow=nCol) #output row in RGB

  for (curCol in c(2000:2500)){#proceed through column-by-column
    
    red <- vector(mode="integer")
    blue <- vector(mode="integer")
    green <- vector(mode="integer")
    
    for (curFile in c(1:numFiles)){ #gather values for current cell for all images
      curRed   <- curRowValues[[curFile]][[curCol,1]]
      red <- c(red,curRed)
      curGreen <- curRowValues[[curFile]][[curCol,2]]
      green <- c(green,curGreen)
      curBlue  <- curRowValues[[curFile]][[curCol,3]]
      blue <- c(blue,curBlue)
    }
    
    #find second highest value for output
    outValues[curRow,curCol,1] <- sort(red,partial=numFiles-1)[numFiles-1]
    outValues[curRow,curCol,2] <- sort(green,partial=numFiles-1)[numFiles-1]
    outValues[curRow,curCol,3] <- sort(blue,partial=numFiles-1)[numFiles-1]
#    newRowValues[curCol,1] <- sort(red,partial=numFiles-1)[numFiles-1]
#    newRowValues[curCol,2] <-  sort(green,partial=numFiles-1)[numFiles-1]
#    newRowValues[curCol,3] <- sort(blue, partial=numFiles-1)[numFiles-1]
        
    #cat("Row ",curRow," Col ",curCol,"\ --> "); 
    #cat("Red: ", newRowValues[curCol,1], " Green: ", newRowValues[curCol,2], " Blue: ", newRowValues[curCol,3],"\n")
  }
  cat("Completed row",curRow,"\n")
}

#convert final output to plot/file (currently only testing with subset)

