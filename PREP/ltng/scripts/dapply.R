dapply <- function(x,by,ncol,f, colnames=c(NA)) {

  j <- matrix(NA,nrow=length(x[[1]]), ncol=ncol)
  for (i in 1:length(x[[1]])) {
#    print(j[i,])
#    print(subset(x,select=by)[i,seq(1,length(by))])
    j[i,] <- f(subset(x,select=by)[i,seq(1,length(by))])
#    print(j[i,])
  }
  if(is.na(colnames[1])) {
    df.j <- as.data.frame(j)
  } else {
    df.j <- as.data.frame(j)
    colnames(df.j) <- colnames

  }
  df.j
}
