load.data <- function(Z)
{
  targZ <- as.numeric(Z[1,2:NCOL(Z)])
  tranZ <- as.numeric(Z[2,2:NCOL(Z)])
  lablZ <- as.character(Z[3,2:NCOL(Z)])
  dateZ <- as.Date(Z[4:NROW(Z),1])
  valsZ <- apply(as.matrix(Z[4:NROW(Z),2:NCOL(Z)]), 2, as.numeric)
  # colnames(valsZ) <- lablZ
  rownames(valsZ) <- as.character(dateZ)
  return(list(target=targZ, transf=tranZ, labels=lablZ, dates=dateZ, values=valsZ))
}

crspn.dates.QtoM <- function(zz)
{
  months.in.q1 <- c("01", "02", "03")
  months.in.q2 <- c("04", "05", "06")
  months.in.q3 <- c("07", "08", "09")
  months.in.q4 <- c("10", "11", "12")
  
  newseqd <- NULL
  for(j in 1:NROW(zz))
  {
    jyear <- year(zz[j])
    jquar <- quarter(zz[j])
    if(jquar==1){jmo <- months.in.q1}
    if(jquar==2){jmo <- months.in.q2}
    if(jquar==3){jmo <- months.in.q3}
    if(jquar==4){jmo <- months.in.q4}
    
    jmo2 <- paste(jyear, "-", jmo, "-01", sep="")
    jmo2 <- as.Date(jmo2)
    jmo2 <- ceiling_date(jmo2, "month") - days(1)
    newseqd <- c(newseqd, as.character(jmo2))
  }
  return(as.Date(newseqd))
}

make.data.DWtoM <- function(zds, zu)
{
  ju <- as.Date(rownames(zu))
  newzu <- matrix(NA, NROW(zds), NCOL(zu))
  colnames(newzu) <- colnames(zu)
  rownames(newzu) <- as.character(zds)
  
  for(j in 1:NROW(zds))
  {
    jdate <- zds[j]
    ztemp1 <- as.matrix(zu[which(year(ju)==year(jdate)),])
    ztemp2 <- as.matrix(ztemp1[which(month(as.Date(rownames(ztemp1)))==month(jdate)),])
    if(NCOL(ztemp2)>1){
      newzu[j,] <- colMeans(as.matrix(ztemp2), na.rm=TRUE)
    }else{
      newzu[j,] <- mean(ztemp2, na.rm=TRUE)
    }
  }
  return(newzu)
}

transform.vars <- function(zwd, zwt)
{
  trmat <- matrix(NA, NROW(zwd), NCOL(zwd))
  colnames(trmat) <- colnames(zwd)
  rownames(trmat) <- rownames(zwd)
  
  for(j in 1:NCOL(zwd))
  {
    ztemp <- zwd[,j]
    if(zwt[j]==0){zfo <- ztemp}
    if(zwt[j]==1){zfo <- log(ztemp) }
    if(zwt[j]==2){zfo <- c(NA, diff(log(ztemp))) }
    if(zwt[j]==3){zfo <- c(NA, diff(ztemp)) }
    if(zwt[j]==4){ zfo <- c(NA, (ztemp[2:NROW(ztemp)]/ztemp[1:(NROW(ztemp)-1)])-1)}
    # if(zwt[j]==4){zfo <- c(NA, exp(diff(log(ztemp)))-1) }
    trmat[,j] <- zfo
  }
  return(trmat)
}

getPCA <- function(ZZ)
{
  pc.out <- prcomp(ZZ,scale.=TRUE,retx=TRUE)
  pev <- cumsum((pc.out$sdev^2)/sum(pc.out$sdev^2))
  #nci <- max(which(pev <= 0.9))
  nci <- 1
  pcx <- matrix((pc.out$x)[,1:nci],NROW(ZZ),ncol=nci)
  return(pcx)
}

make.data.MtoQ1 <- function(zds, zu)
{
  ju <- as.Date(rownames(zu))
  newzu <- matrix(NA, NROW(zds), NCOL(zu))
  colnames(newzu) <- colnames(zu)
  rownames(newzu) <- as.character(zds)
  
  for(j in 1:NROW(zds))
  {
    jdate <- zds[j]
    ztemp1 <- as.matrix(zu[which(year(ju)==year(jdate)),])
    ztemp2 <- as.matrix(ztemp1[which(quarter(as.Date(rownames(ztemp1)))==quarter(jdate)),])
    if(NCOL(ztemp2)>1){
      newzu[j,] <- colMeans(as.matrix(ztemp2), na.rm=TRUE)
    }else{
      newzu[j,] <- mean(ztemp2, na.rm=TRUE)
    }
  }
  return(newzu) 
}

make.data.MtoQ2 <- function(zds, zu)
{
  ju <- as.Date(rownames(zu))
  newzu1 <- matrix(NA, NROW(zds), NCOL(zu))
  colnames(newzu1) <- colnames(zu)
  rownames(newzu1) <- as.character(zds)
  
  newzu2 <- newzu3 <- newzu1
  
  for(j in 1:NROW(zds))
  {
    jdate <- zds[j]
    ztemp1 <- as.matrix(zu[which(year(ju)==year(jdate)),])
    ztemp2 <- as.matrix(ztemp1[which(quarter(as.Date(rownames(ztemp1)))==quarter(jdate)),])
    
    if(quarter(jdate)==1){
      jpt <- which(month(as.Date(rownames(ztemp2)))==1)
      if(NROW(jpt)>0){
        newzu1[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==2)
      if(NROW(jpt)>0){
        newzu2[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==3)
      if(NROW(jpt)>0){
        newzu3[j,] <- ztemp2[jpt,]
      }
    }
    
    if(quarter(jdate)==2){
      jpt <- which(month(as.Date(rownames(ztemp2)))==4)
      if(NROW(jpt)>0){
        newzu1[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==5)
      if(NROW(jpt)>0){
        newzu2[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==6)
      if(NROW(jpt)>0){
        newzu3[j,] <- ztemp2[jpt,]
      }
    }
    
    if(quarter(jdate)==3){
      jpt <- which(month(as.Date(rownames(ztemp2)))==7)
      if(NROW(jpt)>0){
        newzu1[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==8)
      if(NROW(jpt)>0){
        newzu2[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==9)
      if(NROW(jpt)>0){
        newzu3[j,] <- ztemp2[jpt,]
      }
    }
    
    if(quarter(jdate)==4){
      jpt <- which(month(as.Date(rownames(ztemp2)))==10)
      if(NROW(jpt)>0){
        newzu1[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==11)
      if(NROW(jpt)>0){
        newzu2[j,] <- ztemp2[jpt,]
      }
      jpt <- which(month(as.Date(rownames(ztemp2)))==12)
      if(NROW(jpt)>0){
        newzu3[j,] <- ztemp2[jpt,]
      }
    }
  }
  return(cbind(newzu1, newzu2, newzu3))
}

make.data.WtoM1 <- function(zds, zu)
{
  ju <- as.Date(rownames(zu))
  newzu <- matrix(NA, NROW(zds), NCOL(zu))
  colnames(newzu) <- colnames(zu)
  rownames(newzu) <- as.character(zds)
  
  for(j in 1:NROW(zds))
  {
    jdate <- zds[j]
    ztemp1 <- as.matrix(zu[which(year(ju)==year(jdate)),])
    ztemp2ind <- which(month(as.Date(rownames(ztemp1)))==month(jdate))
    ztemp2 <- as.matrix(ztemp1[ztemp2ind,])
    if(NROW(ztemp2ind)==1){
      ztemp2 <- t(ztemp2)
    }
    if(NCOL(ztemp2)>1){
      newzu[j,] <- colMeans(as.matrix(ztemp2), na.rm=TRUE)
    }else{
      newzu[j,] <- mean(ztemp2, na.rm=TRUE)
    }
  }
  return(newzu) 
}

make.data.WtoM2 <- function(zds, zu)
{
  ju <- as.Date(rownames(zu))
  newzu1 <- matrix(NA, NROW(zds), NCOL(zu))
  colnames(newzu1) <- colnames(zu)
  rownames(newzu1) <- as.character(zds)
  
  newzu2 <- newzu3 <- newzu4 <- newzu1
  
  for(j in 1:NROW(zds))
  {
    jdate <- zds[j]
    ztemp1 <- as.matrix(zu[which(year(ju)==year(jdate)),])
    ztemp2 <- as.matrix(ztemp1[which(month(as.Date(rownames(ztemp1)))==month(jdate)),])
    
    ztds <- as.Date(rownames(ztemp2))
    ztws <- ceiling(day(ztds)/7)
    
    iweek1 <- which(ztws==1)
    iweek2 <- which(ztws==2)
    iweek3 <- which(ztws==3)
    iweek4 <- c(which(ztws==4), which(ztws==5))
    
    if(NROW(iweek1)==1){ newzu1[j,] <- ztemp2[iweek1,] }
    if(NROW(iweek2)==1){ newzu2[j,] <- ztemp2[iweek2,] }
    if(NROW(iweek3)==1){ newzu3[j,] <- ztemp2[iweek3,] }
    if(NROW(iweek4)==1){ newzu4[j,] <- ztemp2[iweek4,] }
    
    if(NROW(iweek1)>1){ newzu1[j,] <- colMeans(as.matrix(ztemp2[iweek1,]), na.rm=TRUE) }
    if(NROW(iweek2)>1){ newzu2[j,] <- colMeans(as.matrix(ztemp2[iweek2,]), na.rm=TRUE) }
    if(NROW(iweek3)>1){ newzu3[j,] <- colMeans(as.matrix(ztemp2[iweek3,]), na.rm=TRUE) }
    if(NROW(iweek4)>1){ newzu4[j,] <- colMeans(as.matrix(ztemp2[iweek4,]), na.rm=TRUE) }
  }
  return(cbind(newzu1, newzu2, newzu3, newzu4))
}

MBB <- function(x, b)
{
  xboot <- NULL
  t <- NROW(x)
  k <- NROW(xboot)
  while(k<=(t+5)){
    length <- b
    point <- runif(1, 1, t-b)
    xstart <- point;
    xend <- point+length
    x_new <- x[(xstart+1):(xend)]
    xboot <- c(xboot, x_new)
    k <- NROW(xboot)
  }
  xboot <- xboot[1:t]
  return(xboot)
}

xstd <- function(x)
{
  xf <- x
  for(i in 1:NCOL(xf))
  {
    xf[,i] <- (xf[,i]-mean(xf[,i], na.rm=T))/sd(xf[,i], na.rm=T)
  }
  return(xf)
}

print.backplots <- function(give.names, yyf, xxf, qqf, mfnam)
{
  zzf <- cbind(yyf, xxf, qqf)
  # zzf <- na.omit(zzf)
  yf <- as.matrix(zzf[,1])
  xf <- as.matrix(zzf[,2])
  qf <- as.matrix(zzf[,3:NCOL(qqf)])
  
  sdd <- as.Date(rownames(as.matrix(yf)))
  dlims <- c(min(c(yf,xf,qf), na.rm=TRUE), max(c(yf,xf,qf), na.rm=TRUE))
  snam <- paste(give.names, "-", mfnam, ".pdf", sep="")
  
  subxx <- paste("h=", h, sep="")
  if(itarg==1){ subvar <- "GDP"}
  if(itarg==2){ subvar <- "UNR"}
  if(itarg==3){ subvar <- "IP"}
  if(itarg==4){ subvar <- "CPI"}
  suco <- unlist(strsplit(give.names, split="-"))[1]
  submm <- paste(suco, subvar, sep="-")
  
  pdf(snam, width=11.69, height=8.27)
  plot(sdd, yf, type="l", xlab=subxx, ylab="", main=submm, sub=mfnam, col="white", ylim=dlims)
  box(which="plot", lty = "solid")
  colfunc <- colorRampPalette(c("#3a92af", "#d1eff9"))
  cols <- colfunc(NCOL(qf))
  cols <- c(rev(cols), cols)
  cols <- cols[1:NCOL(qf)]
  jjj <- NCOL(qf)
    for(jj in 1:round(NCOL(qf)/2))
  {
    jnapos <- as.matrix(which(is.na(qf[,jj])==FALSE))
    jnapos <- jnapos[1]-1
    jnadate <- rownames(qf)[jnapos]
    qf[jnapos,jj] <- qf[jnapos,jjj] <- yf[jnadate,]
    polygon(c(sdd, rev(sdd)), c(qf[,jj], rev(qf[,jjj])), col=cols[jj], border = NA)
    jjj <- jjj-1
  }
  lines(sdd, yf, col="black", lwd=2)
  
  jnapos <- as.matrix(which(is.na(xf)==FALSE))
  jnapos <- jnapos[1]-1
  jnadate <- rownames(xf)[jnapos]
  xf[jnapos] <- yf[jnadate,]
  lines(sdd, xf, col="red", lwd=2)
  legend("bottomright", legend=c("Actual", "Estimate"), col=c("black", "red"),
         lwd=c(2,2), lty=c(1,1), bg="white")
  dev.off()
}

make.data.DWtoW <- function(z, ZV)
{
  zn <- seq(z[1]-months(1)+days(1), z[NROW(z)], by="day")
  zw <- which(weekdays(zn)=="Sunday")
  zw <- zn[zw]
  ZV2 <- matrix(NA, NROW(zw), NCOL(ZV))
  zu <- as.Date(rownames(ZV))
  for(j in 1:NROW(zw))
  {
    zzw <- zw[j]
    ztemp1 <- as.matrix(ZV[which(year(zu)==year(zzw)),])
    ztemp2ind <- which(week(as.Date(rownames(ztemp1)))==week(zzw))
    ztemp2 <- as.matrix(ztemp1[ztemp2ind,])
    if(NROW(ztemp2ind)==1){
      ztemp2 <- t(ztemp2)
    }
    if(NROW(ztemp2)>0){
      ZV2[j,] <- colMeans(ztemp2, na.rm=TRUE)
    }
  }
  rownames(ZV2) <- as.character(zw)
  colnames(ZV2) <- colnames(ZV)
  return(ZV2)
}

# 1st diff
fdiff <- function(x){xx <- c(NA, diff(x))}
kfdiff <- function(x,k){xx <- c(rep(NA,k), diff(x,lag=k, differences=1))}

# Requires that S is a matrix with colnames and rownames defined
lag.uni <- function(S, Slags)
{
  Sl <- matrix(NA, NROW(S), 0)
  for(jj in 1:Slags)
  {
    Slt <- c(rep(NA, jj), S[1:(NROW(S)-jj),])
    Sl <- cbind(Sl, Slt)
  }
  colnames(Sl) <- paste(colnames(S), "_L", 1:Slags, sep="")
  return(Sl)
}

# Requires that S is a matrix with colnames and rownames defined
lag.multi <- function(S, Slags)
{
  if(NCOL(S)>1)
  {
    Sl <- matrix(NA, NROW(S), 0)
    for(jj in 1:NCOL(S))
    {
      Slt <- as.matrix(S[,jj])
      rownames(Slt) <- rownames(S)
      colnames(Slt) <- colnames(S)[jj]
      Sl <- cbind(Sl, lag.uni(Slt, Slags))
    }
    kls <- strsplit(colnames(Sl), split="_")
    klsu <- NULL
    for(ik in 1:NROW(kls))
    {
      klsu <- c(klsu, unlist(kls[ik])[2])
    }
    klsu.u <- unique(klsu)
    Ss <- matrix(NA, NROW(Sl), 0)
    for(iju in 1:NROW(klsu.u))
    {
      iwc <- which(klsu==klsu.u[iju])
      Ss <- cbind(Ss, Sl[,iwc])
    }
  }else{
    Sl <- lag.uni(S, Slags)
    Ss <- Sl
  }
  return(Ss)
}

take.unique.lag <- function(ZZ, zlag)
{
  ZZlag <- matrix(NA, NROW(ZZ), NCOL(ZZ))
  for(j in 1:NCOL(ZZ))
  {
    ZZtemp <- lag.uni(as.matrix(ZZ[,j]), zlag)
    ZZtemp <- ZZtemp[,NCOL(ZZtemp)]
    ZZlag[,j] <- ZZtemp
  }
  return(ZZlag)
}
