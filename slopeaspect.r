# Calculate slope and aspect of a digital terrain model

slope<-function(r, res=1){
  # returns a slope surface of elevation raster r of resolution res,
  # according to Burrough et McDonnell 2000, p. 191
  newr<-matrix(0, nrow=nrow(r), ncol=ncol(r))
  h<-nrow(r)
  w<-ncol(r)
  for(i in 2:(h-1)){
    for(j in 2:(w-1)){
      newr[i,j]<-sqrt( ((r[i+1,j]-r[i-1,j])/(2*res))^2 + ((r[i,j+1]-r[i,j-1])/(2*res))^2 )
    }
  }
  newr[1,]<-0; newr[h,]<-0; newr[,1]<-0; newr[,w]<-0; #adjust border
  return(newr)
}

aspect<-function(r, res=1){
  # returns a aspect surface of elevation raster r of resolution res,
  # according to Burrough et McDonnell 2000, p. 191
  newr<-matrix(0, nrow=nrow(r), ncol=ncol(r))
  h<-nrow(r)
  w<-ncol(r)
  for(i in 2:(h-1)){
    for(j in 2:(w-1)){
      newr[i,j]<-atan( (-(r[i,j-1]-r[i,j+1])/(2*res)) / (-(r[i-1,j]-r[i+1,j])/(2*res)) )
    }
  }
  newr[1,]<-0; newr[h,]<-0; newr[,1]<-0; newr[,w]<-0; #adjust border
  return(newr)
}

# parameters
input_terrain <- 'c:/temp/terrain.txt'

# read data
m<-as.matrix(read.table(input_terrain))

# calculate slope and aspect surfaces
s<-slope(m)
a<-aspect(m)

# plot results
image(m, main="elevation")
image(s, main="slope")
image(a, main="aspect")
