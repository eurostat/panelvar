if(system_constant.p==TRUE){
  if(steps.p=="mstep"){ b <- PVARe$m_step }
  if(steps.p=="twostep"){ b <- PVARe$second_step }
  if(steps.p=="onestep"){ b <- PVARe$first_step }
  bcw <- which(colnames(b)=="const")
  bc <- b[,bcw]
  b <- b[,-bcw]
}else{
  if(steps.p=="mstep"){ b <- PVARe$m_step }
  if(steps.p=="twostep"){ b <- PVARe$second_step }
  if(steps.p=="onestep"){ b <- PVARe$first_step }
  bc <- rep(0, 3)
}

if(lags.p==1){
  # Calculate e so you can calculate sigma
  if(flag.exog==TRUE){
    ctemp <- cbind(c1y[1:(NROW(c1y)-1),], exog1[1:(NROW(c1y)-1),])
    e1.y1 <- c1y[2:NROW(c1y),1]-ctemp%*%b[1,]-bc[1]
    e1.y2 <- c1y[2:NROW(c1y),2]-ctemp%*%b[2,]-bc[2]
    e1.y3 <- c1y[2:NROW(c1y),3]-ctemp%*%b[3,]-bc[3]

    ctemp <- cbind(c2y[1:(NROW(c2y)-1),], exog2[1:(NROW(c2y)-1),])
    e2.y1 <- c2y[2:NROW(c2y),1]-ctemp%*%b[1,]-bc[1]
    e2.y2 <- c2y[2:NROW(c2y),2]-ctemp%*%b[2,]-bc[2]
    e2.y3 <- c2y[2:NROW(c2y),3]-ctemp%*%b[3,]-bc[3]
    
    ctemp <- cbind(c3y[1:(NROW(c3y)-1),], exog3[1:(NROW(c3y)-1),])
    e3.y1 <- c3y[2:NROW(c3y),1]-ctemp%*%b[1,]-bc[1]
    e3.y2 <- c3y[2:NROW(c3y),2]-ctemp%*%b[2,]-bc[2]
    e3.y3 <- c3y[2:NROW(c3y),3]-ctemp%*%b[3,]-bc[3]
    
    ctemp <- cbind(c4y[1:(NROW(c4y)-1),], exog4[1:(NROW(c4y)-1),])
    e4.y1 <- c4y[2:NROW(c4y),1]-ctemp%*%b[1,]-bc[1]
    e4.y2 <- c4y[2:NROW(c4y),2]-ctemp%*%b[2,]-bc[2]
    e4.y3 <- c4y[2:NROW(c4y),3]-ctemp%*%b[3,]-bc[3]
  }else{
    e1.y1 <- c1y[2:NROW(c1y),1]-c1y[1:(NROW(c1y)-1),]%*%b[1,]-bc[1]
    e2.y1 <- c2y[2:NROW(c2y),1]-c2y[1:(NROW(c2y)-1),]%*%b[1,]-bc[1]
    e3.y1 <- c3y[2:NROW(c3y),1]-c3y[1:(NROW(c3y)-1),]%*%b[1,]-bc[1]
    e4.y1 <- c4y[2:NROW(c4y),1]-c4y[1:(NROW(c4y)-1),]%*%b[1,]-bc[1]
    
    e1.y2 <- c1y[2:NROW(c1y),2]-c1y[1:(NROW(c1y)-1),]%*%b[2,]-bc[2]
    e2.y2 <- c2y[2:NROW(c2y),2]-c2y[1:(NROW(c2y)-1),]%*%b[2,]-bc[2]
    e3.y2 <- c3y[2:NROW(c3y),2]-c3y[1:(NROW(c3y)-1),]%*%b[2,]-bc[2]
    e4.y2 <- c4y[2:NROW(c4y),2]-c4y[1:(NROW(c4y)-1),]%*%b[2,]-bc[2]
    
    e1.y3 <- c1y[2:NROW(c1y),3]-c1y[1:(NROW(c1y)-1),]%*%b[3,]-bc[3]
    e2.y3 <- c2y[2:NROW(c2y),3]-c2y[1:(NROW(c2y)-1),]%*%b[3,]-bc[3]
    e3.y3 <- c3y[2:NROW(c3y),3]-c3y[1:(NROW(c3y)-1),]%*%b[3,]-bc[3]
    e4.y3 <- c4y[2:NROW(c4y),3]-c4y[1:(NROW(c4y)-1),]%*%b[3,]-bc[3]
  }
}
if(lags.p>1){
  if(flag.exog==TRUE){
    
  }else{
    ctemp <- c1y; colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep=""); ctemp <- lag.multi(ctemp, lags.p)
    e1.y1 <- c1y[,1]-ctemp%*%b[1,]-bc[1]
    e1.y2 <- c1y[,2]-ctemp%*%b[2,]-bc[2]
    e1.y3 <- c1y[,3]-ctemp%*%b[3,]-bc[3]
    
    ctemp <- c2y; colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep=""); ctemp <- lag.multi(ctemp, lags.p)
    e2.y1 <- c2y[,1]-ctemp%*%b[1,]-bc[1]
    e2.y2 <- c2y[,2]-ctemp%*%b[2,]-bc[2]
    e2.y3 <- c2y[,3]-ctemp%*%b[3,]-bc[3]
    
    ctemp <- c3y; colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep=""); ctemp <- lag.multi(ctemp, lags.p)
    e3.y1 <- c3y[,1]-ctemp%*%b[1,]-bc[1]
    e3.y2 <- c3y[,2]-ctemp%*%b[2,]-bc[2]
    e3.y3 <- c3y[,3]-ctemp%*%b[3,]-bc[3]
    
    ctemp <- c4y; colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep=""); ctemp <- lag.multi(ctemp, lags.p)
    e4.y1 <- c4y[,1]-ctemp%*%b[1,]-bc[1]
    e4.y2 <- c4y[,2]-ctemp%*%b[2,]-bc[2]
    e4.y3 <- c4y[,3]-ctemp%*%b[3,]-bc[3]
  }
}

if(lags.p==1){
  # calculate forecasts
  if(flag.exog==TRUE){
    ctemp <- c(c1y[NROW(c1y),], exog1[NROW(exog1),])
    
  }else{
    for1.y1 <- c1y[NROW(c1y),]%*%b[1,]+bc[1]
    for2.y1 <- c2y[NROW(c2y),]%*%b[1,]+bc[1]
    for3.y1 <- c3y[NROW(c3y),]%*%b[1,]+bc[1]
    for4.y1 <- c4y[NROW(c4y),]%*%b[1,]+bc[1]
    
    for1.y2 <- c1y[NROW(c1y),]%*%b[2,]+bc[2]
    for2.y2 <- c2y[NROW(c2y),]%*%b[2,]+bc[2]
    for3.y2 <- c3y[NROW(c3y),]%*%b[2,]+bc[2]
    for4.y2 <- c4y[NROW(c4y),]%*%b[2,]+bc[2]
    
    for1.y3 <- c1y[NROW(c1y),]%*%b[3,]+bc[3]
    for2.y3 <- c2y[NROW(c2y),]%*%b[3,]+bc[3]
    for3.y3 <- c3y[NROW(c3y),]%*%b[3,]+bc[3]
    for4.y3 <- c4y[NROW(c4y),]%*%b[3,]+bc[3]
    
    if(h>1){
      for(gh in 2:h){
        cxu <- c(for1.y1[NROW(for1.y1)], for1.y2[NROW(for1.y2)], for1.y3[NROW(for1.y3)])
        for1.y1 <- c(for1.y1, cxu%*%b[1,]+bc[1])
        for1.y2 <- c(for1.y2, cxu%*%b[2,]+bc[2])
        for1.y3 <- c(for1.y3, cxu%*%b[3,]+bc[3])
        
        cxu <- c(for2.y1[NROW(for2.y1)], for2.y2[NROW(for2.y2)], for2.y3[NROW(for2.y3)])
        for2.y1 <- c(for2.y1, cxu%*%b[1,]+bc[1])
        for2.y2 <- c(for2.y2, cxu%*%b[2,]+bc[2])
        for2.y3 <- c(for2.y3, cxu%*%b[3,]+bc[3])
        
        cxu <- c(for3.y1[NROW(for3.y1)], for3.y2[NROW(for3.y2)], for3.y3[NROW(for3.y3)])
        for3.y1 <- c(for3.y1, cxu%*%b[1,]+bc[1])
        for3.y2 <- c(for3.y2, cxu%*%b[2,]+bc[2])
        for3.y3 <- c(for3.y3, cxu%*%b[3,]+bc[3])
        
        cxu <- c(for4.y1[NROW(for4.y1)], for4.y2[NROW(for4.y2)], for4.y3[NROW(for4.y3)])
        for4.y1 <- c(for4.y1, cxu%*%b[1,]+bc[1])
        for4.y2 <- c(for4.y2, cxu%*%b[2,]+bc[2])
        for4.y3 <- c(for4.y3, cxu%*%b[3,]+bc[3])
      }
    }
  }
}

if(lags.p>1){
  # calculate forecasts
  ctemp <- c1y
  colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep="")
  ctempL <- lag.multi(ctemp, lags.p-1); ctemp <- cbind(ctemp, ctempL); ctemp <- ctemp[NROW(ctemp),]
  for1.y1 <- ctemp%*%b[1,]+bc[1]
  for1.y2 <- ctemp%*%b[2,]+bc[2]
  for1.y3 <- ctemp%*%b[3,]+bc[3]
  
  ctemp <- c2y
  colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep="")
  ctempL <- lag.multi(ctemp, lags.p-1); ctemp <- cbind(ctemp, ctempL); ctemp <- ctemp[NROW(ctemp),]
  for2.y1 <- ctemp%*%b[1,]+bc[1]
  for2.y2 <- ctemp%*%b[2,]+bc[2]
  for2.y3 <- ctemp%*%b[3,]+bc[3]
  
  ctemp <- c3y
  colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep="")
  ctempL <- lag.multi(ctemp, lags.p-1); ctemp <- cbind(ctemp, ctempL); ctemp <- ctemp[NROW(ctemp),]
  for3.y1 <- ctemp%*%b[1,]+bc[1]
  for3.y2 <- ctemp%*%b[2,]+bc[2]
  for3.y3 <- ctemp%*%b[3,]+bc[3]
  
  ctemp <- c4y
  colnames(ctemp) <- paste("V", 1:NCOL(ctemp), sep=""); rownames(ctemp) <- paste("R", 1:NROW(ctemp), sep="")
  ctempL <- lag.multi(ctemp, lags.p-1); ctemp <- cbind(ctemp, ctempL); ctemp <- ctemp[NROW(ctemp),]
  for4.y1 <- ctemp%*%b[1,]+bc[1]
  for4.y2 <- ctemp%*%b[2,]+bc[2]
  for4.y3 <- ctemp%*%b[3,]+bc[3]
  
  c1y.app <- rbind(c1y, c(for1.y1, for1.y2, for1.y3))
  c2y.app <- rbind(c2y, c(for2.y1, for2.y2, for2.y3))
  c3y.app <- rbind(c3y, c(for3.y1, for3.y2, for3.y3))
  c4y.app <- rbind(c4y, c(for4.y1, for4.y2, for4.y3))
  
  if(h>1){
    for(gh in 2:h){
      cxu <- c1y.app; cxu <- cbind(cxu, lag.multi(cxu, lags.p-1)); cxu <- cxu[NROW(cxu),]
      for1.y1 <- c(for1.y1, cxu%*%b[1,]+bc[1])
      for1.y2 <- c(for1.y2, cxu%*%b[2,]+bc[2])
      for1.y3 <- c(for1.y3, cxu%*%b[3,]+bc[3])
      
      cxu <- c2y.app; cxu <- cbind(cxu, lag.multi(cxu, lags.p-1)); cxu <- cxu[NROW(cxu),]
      for2.y1 <- c(for2.y1, cxu%*%b[1,]+bc[1])
      for2.y2 <- c(for2.y2, cxu%*%b[2,]+bc[2])
      for2.y3 <- c(for2.y3, cxu%*%b[3,]+bc[3])
      
      cxu <- c3y.app; cxu <- cbind(cxu, lag.multi(cxu, lags.p-1)); cxu <- cxu[NROW(cxu),]
      for3.y1 <- c(for3.y1, cxu%*%b[1,]+bc[1])
      for3.y2 <- c(for3.y2, cxu%*%b[2,]+bc[2])
      for3.y3 <- c(for3.y3, cxu%*%b[3,]+bc[3])
      
      cxu <- c4y.app; cxu <- cbind(cxu, lag.multi(cxu, lags.p-1)); cxu <- cxu[NROW(cxu),]
      for4.y1 <- c(for4.y1, cxu%*%b[1,]+bc[1])
      for4.y2 <- c(for4.y2, cxu%*%b[2,]+bc[2])
      for4.y3 <- c(for4.y3, cxu%*%b[3,]+bc[3])
      
      c1y.app <- rbind(c1y.app, c(for1.y1[NROW(for1.y1)], for1.y2[NROW(for1.y2)], for1.y3[NROW(for1.y3)]))
      c2y.app <- rbind(c2y.app, c(for2.y1[NROW(for2.y1)], for2.y2[NROW(for2.y2)], for2.y3[NROW(for2.y3)]))
      c3y.app <- rbind(c3y.app, c(for3.y1[NROW(for3.y1)], for3.y2[NROW(for3.y2)], for3.y3[NROW(for3.y3)]))
      c4y.app <- rbind(c4y.app, c(for4.y1[NROW(for4.y1)], for4.y2[NROW(for4.y2)], for4.y3[NROW(for4.y3)]))
    }
  }
}

for1.y1 <- for1.y1[NROW(for1.y1)]
for2.y1 <- for2.y1[NROW(for2.y1)]
for3.y1 <- for3.y1[NROW(for3.y1)]
for4.y1 <- for4.y1[NROW(for4.y1)]

s1.y1 <- sd(e1.y1, na.rm=TRUE)
s2.y1 <- sd(e2.y1, na.rm=TRUE)
s3.y1 <- sd(e3.y1, na.rm=TRUE)
s4.y1 <- sd(e4.y1, na.rm=TRUE)

spseq1.y1 <- qnorm(seq(0.51, 0.99, 0.01))*s1.y1
spseq2.y1 <- qnorm(seq(0.51, 0.99, 0.01))*s2.y1
spseq3.y1 <- qnorm(seq(0.51, 0.99, 0.01))*s3.y1
spseq4.y1 <- qnorm(seq(0.51, 0.99, 0.01))*s4.y1

outf <- for1.y1
spseq <- spseq1.y1
zout1.y1 <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])

outf <- for2.y1
spseq <- spseq2.y1
zout2.y1 <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])

outf <- for3.y1
spseq <- spseq3.y1
zout3.y1 <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])

outf <- for4.y1
spseq <- spseq4.y1
zout4.y1 <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])
