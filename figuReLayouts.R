#### Contributors:
#### John Urban, 2019


uppercase <- 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
lowercase <- 'abcdefghijklmnopqrstuvwxyz'


layouts.square <- function(n, fill.in.byrow=TRUE){
  # n = ncol and nrow
  # hor
  mat <- matrix(data=1:(n^2), nrow = n, ncol = n, byrow = fill.in.byrow) 
  layout(mat)
}

layouts.square.ABC <- function(n, thickness.factor=3, fill.in.byrow=TRUE){
  # Leaves a sliver for lettering A, B, C where the thickness is 1/thickness.factor
  # So a 2x2 with TF=1 will really be a 2TF x (2TF+2) = 2 x 4
  #    a 2x2 with TF=9 will be 2TF x (2TF+2) = 2*9 + 2*9+2 = 18x20
  layouts.ABC(nr=n, nc=n, thickness.factor = thickness.factor, fill.in.byrow = fill.in.byrow)
}


layouts.ABC <- function(nr, nc, thickness.factor=3, fill.in.byrow=TRUE){
  # Leaves a sliver for lettering A, B, C where the thickness is 1/thickness.factor
  # So a 2x2 with TF=1 will really be a 2TF x (2TF+2) = 2 x 4
  #    a 2x2 with TF=9 will be 2TF x (2TF+2) = 2*9 + 2*9+2 = 18x20
  nr <- nr*thickness.factor
  nc <- nc*thickness.factor+nc
  cwidth <- thickness.factor+1
  rowseq <- seq(1,nr-thickness.factor+1, thickness.factor)
  colseq1 <- seq(1,nc-cwidth+1,cwidth)
  colseq2 <- seq(2,nc-cwidth+2,cwidth)
  mat <- matrix(nrow=nr, ncol = nc)
  fn <- 1
  if(fill.in.byrow){
    ## First add LETTER boards to MULTIPLOT
    for(i in rowseq){for(j in colseq1){
      mat[i,j] <- fn; fn <- fn+1
      if(thickness.factor>1){mat[(i+1):(i+thickness.factor-1),j] <- fn; fn <- fn+1}
    }}
    ## Next add plot spaces
    for(i in rowseq){for(j in colseq2){mat[i:(i+thickness.factor-1),j:(j+thickness.factor-1)] <- fn; fn <- fn+1}}
    
  } else {
    ## First add LETTER boards to MULTIPLOT
    for(j in colseq1){for(i in rowseq){
      mat[i,j] <- fn; fn <- fn+1
      if(thickness.factor>1){mat[(i+1):(i+thickness.factor-1),j] <- fn; fn <- fn+1}
    }}
    ## Next add plot spaces
    for(j in colseq2){for(i in rowseq){mat[i:(i+thickness.factor-1),j:(j+thickness.factor-1)] <- fn; fn <- fn+1}}
    
  }
  layout(mat)
  #print(mat)
}



plots.layouts.square.ABC <- function(plotfxns, alphabet=uppercase, thickness.factor=9, fill.in.byrow=TRUE, mgp=c(3,0.6,0), lettermar=rep(0.1,4), figuremar=c(4,2,1,4)){
  # Plot fxns is a list containing pre-made BASE plot functions - typically with empty args
  n <- length(plotfxns)
  if(as.integer(sqrt(n))-sqrt(n) != 0){return("The number of plot functions needs to have an integer square root.")}
  layouts.square.ABC(n=sqrt(n), thickness.factor = thickness.factor, fill.in.byrow = fill.in.byrow)
  # First add letters
  par(mgp=mgp, mar=lettermar)
  for(idx in 1:n){
    add.figure.letter(alphabet,idx)
    if(thickness.factor>1){emptyplot()}
  }
  par(mgp=mgp, mar=figuremar)
  # Next add plots
  for(i in 1:n){
    plotfxns[[i]]()
  }
}

plots.layouts.ABC <- function(plotfxns, nr, nc, alphabet=uppercase, thickness.factor=9, fill.in.byrow=TRUE, mgp=c(3,0.6,0), lettermar=rep(0.1,4), figuremar=c(4,2,1,4)){
  # Plot fxns is a list containing pre-made BASE plot functions - typically with empty args
  n <- length(plotfxns)
  if(nr*nc<n){return("nr*nc must exceed the number of plotfxns, n.")}
  layouts.ABC(nr=nr, nc=nc, thickness.factor = thickness.factor, fill.in.byrow = fill.in.byrow)
  
  # LETTERS
  # First add letters
  par(mgp=mgp, mar=lettermar)
  for(idx in 1:n){
    add.figure.letter(alphabet,idx)
    if(thickness.factor>1){emptyplot()}
  }
  # Empty for remaining letter slots
  if(nr*nc > n){
    #nfigslots <- length( (n+1):(nr*nc) )
    for(leftover in (n+1):(nr*nc)){emptyplot(); if(thickness.factor>1){emptyplot()}} 
  }
  # FIGS
  par(mgp=mgp, mar=figuremar)
  # Next add plots
  for(i in 1:n){
    plotfxns[[i]]()
  }
  
  ## Empty the remaining plots - may not be nec...
  if(nr*nc > n){
    #nfigslots <- length( (n+1):(nr*nc) )
    for(leftover in (n+1):(nr*nc)){emptyplot()} 
  }
}




add.figure.letter <- function(alphabet,idx){
  textplot(msg=substr(x = alphabet, start = idx, stop = idx))
}


emptyplot <- function(x=1,y=1){
  plot(x,y,type="n", bty="n", xlab="", ylab="", xaxt="n",yaxt="n")
}


textplot<- function(msg="",x=1,y=1,cex=2, rot=0, pol=FALSE){
  emptyplot(x=x,y=y)
  text(x,y,msg, font=2, cex=cex, srt=rot)
  if(pol){
    l<-x+0.3
    r<-l+0.01
    b=y-0.35
    t <- y+0.35
    polygon(x = c(l,r,r,l,l), y = c(b,b,t,t,b), col = "black", border = "black")
  }
}












### SPECIAL 1 ABC ###################
# layouts.special1 <- function(nr=4, nc=3, h=4, nhead=2, nlab=2){
#   if(nr <= 0 | nc <= 0 | nr%%2 != 0){return("nr and nc must be > 0 and nr must be even.")}
#   # h is like thickness.factor in others... 
#   mat <- matrix(nrow = nr*h+nhead, ncol=nc*h+nlab); dim(mat)
#   fn <- 1
#   
#   ## Empty space at top left corner
#   mat[1:nhead,1:nlab] <- fn; fn<-fn+1
#   
#   ## Title
#   mat[1,(nlab+1):dim(mat)[2]] <- fn; fn<-fn+1
#   
#   ## Subtitles
#   for(subhead in 2:nhead){
#     for (j in seq((nlab+1), (nlab+1)+h*(nc-1)+1, h)){
#       mat[subhead, j:(j+h-1)] <- fn; fn<-fn+1
#     }
#   }
#   
#   idx <- nhead+1 ## this idx to simplify code
#   for (i in seq(idx, idx+h*(nr-1)+1, 2*h)){
#     # Outer label
#     mat[i:(i+2*h-1),1] <- fn; fn<-fn+1
#     # Inner labels
#     mat[i:(i+h-1),2] <- fn; fn<-fn+1
#     mat[(i+h):(i+2*h-1),2] <- fn; fn<-fn+1
#     # First row of plots
#     for (j in seq(nlab+1, (nlab+1)+h*(nc-1)+1, h)){
#       mat[i:(i+h-1),j:(j+h-1)] <- fn; fn<-fn+1
#     }
#     # Next row of plots
#     for (j in seq(nlab+1, (nlab+1)+h*(nc-1)+1, h)){
#       mat[(i+h):(i+2*h-1), j:(j+h-1)] <- fn; fn<-fn+1
#     }
#     
#   };
#   layout(mat)
# }
# plots.layouts.special1 <- function(nr=4, nc=3, h=4, nhead=2, nlab=2, header="Header", subtitles=c("Subhead1","Subnhead2","Subhead3"), outside.labels=c("Outside1","Outside2"), inside.labels=c("Inside1","Inside2"), plotfxns){
#   layouts.special1(nr=nr, nc=nc, h=h, nhead=nhead, nlab = nlab)
#   rot=90
#   # Plot empty space at top left
#   par(mar=rep(0.1,4)); emptyplot()
#   
#   # Plot header
#   par(mar=rep(0.1,4)); textplot(header,cex=3) 
#   
#   # Plot sub-headings...
#   par(mar=c(0.1,5,1,1)); 
#   for(i in 1:((nhead-1)*nc)){
#     textplot(subtitles[i])
#   }
#   
#   ## Plot side labels and figures
#   fig.num <- 1
#   for(i in 1:(nr/2)){
#     par(mar=rep(0.1,4)); 
#     #OPutside label
#     textplot(outside.labels[i], rot=rot, pol=TRUE); 
#     
#     # Inside labels
#     textplot(inside.labels[1],cex=1.5, rot=rot); 
#     textplot(inside.labels[2],cex=1.5, rot=rot)
#     
#     ## Figures -- plotting 2 rows x nc
#     par(mar=c(4,5,1,1))
#     for(i in fig.num:(fig.num+2*nc-1)){
#       plotfxns[[i]]()
#     }
#     fig.num <- i + 1 ## Add 1 to get the next starting value when this loop is encountered again
#   }
# }

layouts.special1 <- function(nr=4, nc=3, h=4, nhead=2, nlab=2, addletters=FALSE){
  if(nr <= 0 | nc <= 0 | nr%%2 != 0){return("nr and nc must be > 0 and nr must be even.")}
  # h is like thickness.factor in others... 
  if(addletters){ltr<-nc; ltr.indicator<-1}else{ltr<-0; ltr.indicator<-0}
  mat <- matrix(nrow = nr*h+nhead, ncol=nc*h+nlab+ltr); dim(mat)
  fn <- 1
  print(1)
  
  ## Empty space at top left corner
  mat[1:nhead,1:nlab] <- fn; fn<-fn+1
  print(2)
  
  ## Title
  mat[1,(nlab+1):dim(mat)[2]] <- fn; fn<-fn+1
  print(3)
  
  ## Subtitles
  for(subhead in 2:nhead){
    for (j in seq((nlab+1), (nlab+1)+(h+ltr.indicator)*(nc-1)+1, h+ltr.indicator)){
      if(addletters){mat[subhead, j] <- fn; fn<-fn+1}
      mat[subhead, (j+ltr.indicator):(j+ltr.indicator+h-1)] <- fn; fn<-fn+1
    }
  }
  
  print(4)
  idx <- nhead+1 ## this idx to simplify code
  for (i in seq(idx, idx+h*(nr-1)+1, 2*h)){
    # Outer label
    print(5)
    mat[i:(i+2*h-1),1] <- fn; fn<-fn+1
    # Inner labels
    print(6)
    mat[i:(i+h-1),2] <- fn; fn<-fn+1
    mat[(i+h):(i+2*h-1),2] <- fn; fn<-fn+1
    # First row of plots
    print(7)
    print(dim(mat))
    for (j in seq(nlab+1, (nlab+1)+(h+ltr.indicator)*(nc-1)+1, h+(ltr/nc))){
      print(c("j",j))
      if(addletters){
        print(71)
        mat[i,j] <- fn; fn<-fn+1 
        print(72)
        if(h>1){
          mat[(i+1):(i+h-1),j] <- fn; fn<-fn+1 
        }
        print(73)
        mat[i:(i+h-1),(j+1):(j+1+h-1)] <- fn; fn<-fn+1 
      } else{
        mat[i:(i+h-1),j:(j+h-1)] <- fn; fn<-fn+1 
      }
    }
    # Next row of plots
    print(8)
    for (j in seq(nlab+1, (nlab+1)+(h+ltr.indicator)*(nc-1)+1, h+(ltr/nc))){
      if(addletters){
        mat[i+h,j] <- fn; fn<-fn+1 
        if(h>1){
          mat[(i+h+1):(i+2*h-1),j] <- fn; fn<-fn+1 
        }
        mat[(i+h):(i+2*h-1), (j+1):(j+1+h-1)] <- fn; fn<-fn+1
      }else{
        mat[(i+h):(i+2*h-1), j:(j+h-1)] <- fn; fn<-fn+1
      }
    }
  };
  print(mat)
  layout(mat)
  
}


plots.layouts.special1 <- function(plotfxns, nr=4, nc=3, h=4, nhead=2, nlab=2, header="Header", subtitles=c("Subhead1","Subhead2","Subhead3"), outside.labels=c("Outside1","Outside2"), inside.labels=c("Inside1","Inside2"), addletters=FALSE, alphabet=uppercase){
  layouts.special1(nr=nr, nc=nc, h=h, nhead=nhead, nlab = nlab, addletters = addletters)
  rot=90
  print("HW")
  # Plot empty space at top left
  par(mar=rep(0.1,4)); emptyplot()
  
  # Plot header
  par(mar=rep(0.1,4)); textplot(header,cex=3) 
  
  # Plot sub-headings...
  par(mar=c(0.1,5,1,1)); 
  for(i in 1:((nhead-1)*nc)){
    if(addletters){par(mar=rep(0.1,4)); emptyplot(); par(mar=c(0.1,5,1,1))}
    textplot(subtitles[i])
  }
  
  ## Plot side labels and figures
  fig.num <- 1
  for(i in 1:(nr/2)){
    par(mar=rep(0.1,4)); 
    #OPutside label
    textplot(outside.labels[i], rot=rot, pol=TRUE); 
    
    # Inside labels
    textplot(inside.labels[1],cex=1.5, rot=rot); 
    textplot(inside.labels[2],cex=1.5, rot=rot)
    
    ## Figures -- plotting 2 rows x nc
    par(mar=c(4,5,1,1))
    for(idx in fig.num:(fig.num+2*nc-1)){
      if(addletters){
        par(mar=c(0.1,1,0.1,0.1))
        add.figure.letter(alphabet,idx)
        if(h>1){emptyplot()}
        par(mar=c(4,5,1,1))
      }
      #print(idx)
      plotfxns[[idx]]()
    }
    fig.num <- idx + 1 ## Add 1 to get the next starting value when this loop is encountered again
  }
}


## FUNCTIONS FOR EXAMPLE
## Defining some boring functions
p1 <- function(){
  plot(1:10)
}
p2 <- function(){
  plot(1:20)
}
p3 <- function(){
  p1()
}
p4 <- function(){
  p2()
}
plotpng <- function(preserveAspectRatio=FALSE){
  emptyplot(1:10,1:10)
  img <- readPNG(system.file("img", "Rlogo.png", package="png"))
  #grid::grid.raster(img)
  rasterImage(img, xleft = 1, xright = 10, ybottom = 1, ytop = 10)
}

plotpng.preserveAspectRatio <- function(preserveAspectRatio=FALSE){
  emptyplot(1:10,1:10)
  img <- readPNG(system.file("img", "Rlogo.png", package="png"))
  addImg(obj = img, x=5.5, y=5.5, width=3)
}


addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  ## Stolen from: https://stackoverflow.com/questions/27800307/adding-a-picture-to-plot-in-r
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}
