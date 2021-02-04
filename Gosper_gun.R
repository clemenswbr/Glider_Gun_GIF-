#Gosper's gliding gun

library(animation)

#Create matrix
x <- matrix(0,nrow=50,ncol=50)

#Implement gliding gun
x[6:7,2:3] <- x[4:5,36:37] <- x[6:8,12] <- x[4,14:15] <- x[10,14:15] <- 
  x[5,13] <- x[9,13] <- x[7,16] <- x[5,17] <- x[9,17] <- x[6:8,18] <-
  x[7,19] <- x[4:6,22:23] <- x[3,24] <- x[7,24] <- x[2:3,26] <- x[7:8,26] <- 1
x <- t(x)
image(x)

#Implement Game of life loop
life_step <- function(d) { 
  nrow <- dim(d)[[1]] 
  ncol <- dim(d)[[2]] 
  d_eu <- rbind(d[-1, , drop = FALSE], 0)
  d_ed <- rbind(0, d[-nrow, , drop = FALSE])
  d_le <- cbind(d[ , -1, drop = FALSE], 0)
  d_re <- cbind(0, d[ , -ncol, drop = FALSE])
  d_lu <- cbind(d_eu[ , -1, drop = FALSE], 0)
  d_ru <- cbind(0, d_eu[ , -ncol, drop = FALSE])
  d_ld <- cbind(d_ed[ , -1, drop = FALSE], 0)
  d_rd <- cbind(0, d_ed[ , -ncol, drop = FALSE])
  pop <- d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd
  d <- (pop==3) | (d & (pop>=2) & (pop<=3))
  d
}

#List with all images
ls <- list()
for (i in 1:200) {
  x <- life_step(x);
  ls[[length(ls)+1]] <- x;
}

#Function to extract current image
current <- function(step){
  mat <- ls[[step]]
  return(mat)
}

#Creating GIF
saveGIF(for (i in 1:200) {
  image(current(i),col=c('black','white'),axes=FALSE)
  }
  ,movie.name='Gosper_gun.gif',clean=TRUE,interval=0.05)


