b <- out.lm$coefficients
b[which(is.na(b))] <- 0

e1 <- c1[,1]-X1%*%b
sigmah <- sd(e1, na.rm=TRUE); if(hbounds==TRUE){sigmah <- sigmah*sqrt(h)}
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
outf <- as.numeric(X1[NROW(X1),]%*%b )
zout <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])
fest1[(i+h),imodel] <- zout[1]
fquant1[(i+h), ,imodel] <- zout[2:NROW(zout)]

e2 <- c2[,1]-X2%*%b 
sigmah <- sd(e2, na.rm=TRUE); if(hbounds==TRUE){sigmah <- sigmah*sqrt(h)}
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
outf <- as.numeric(X2[NROW(X2),]%*%b )
zout <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])
fest2[(i+h),imodel] <- zout[1]
fquant2[(i+h), ,imodel] <- zout[2:NROW(zout)]

e3 <- c3[,1]-X3%*%b 
sigmah <- sd(e3, na.rm=TRUE); if(hbounds==TRUE){sigmah <- sigmah*sqrt(h)}
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
outf <- as.numeric(X3[NROW(X3),]%*%b )
zout <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])
fest3[(i+h),imodel] <- zout[1]
fquant3[(i+h), ,imodel] <- zout[2:NROW(zout)]

e4 <- c4[,1]-X4%*%b 
sigmah <- sd(e4, na.rm=TRUE); if(hbounds==TRUE){sigmah <- sigmah*sqrt(h)}
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah
outf <- as.numeric(X4[NROW(X4),]%*%b )
zout <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])
fest4[(i+h),imodel] <- zout[1]
fquant4[(i+h), ,imodel] <- zout[2:NROW(zout)]
