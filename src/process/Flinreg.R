z <- YY
zsd <- sd(z, na.rm=TRUE)
zmu <- mean(z, na.rm=TRUE)
z <- xstd(z)
f <- XX

fout <- f[NROW(f),]

# common Y, X/F for h=0
if(NROW(z)<NROW(f)){
  zoutmiss <- which(as.Date(rownames(f))%in%as.Date(rownames(z))==FALSE)
  z <- rbind(z, matrix(NA, NROW(zoutmiss), 1))
  rownames(z) <- rownames(f)
}
if(NROW(z)>NROW(f)) {
  stop("There seems to be a problem in line 16 in Flinreg.R file")
}

zcom <- cbind(z, f)
zcom <- na.omit(zcom)
fcom <- as.matrix(zcom[,2:NCOL(zcom)])
zcom <- as.matrix(zcom[,1])
colnames(zcom) <- colnames(z)
colnames(fcom) <- colnames(f)

if(NROW(zcom)!=NROW(fcom)) {
  stop("There seems to be a problem in line 27 in Flinreg.R file")
}

jj <- h
zreg <- as.matrix(zcom[(jj+1):NROW(zcom),])
freg <- as.matrix(fcom[1:(NROW(fcom)-jj),])
out <- lm(zreg~freg)
b <- out$coefficients; 
outf <- ((fout%*%b[2:NROW(b)]) + b[1])*zsd+zmu
outf <- as.numeric(outf)

# For one-step forecasts for time series, the residual standard deviation
# provides a good estimate of the forecast standard deviation. 
# see https://www.otexts.org/fpp/2/7
# Hyndman
#
# Not entirely correct
# but the scaling in the first step causes problems
sigmah <- sd(out$residuals*zsd+zmu, na.rm=TRUE)
if(hbounds==TRUE){
  sigmah <- sigmah*sqrt(h)
}
spseq <- qnorm(seq(0.51, 0.99, 0.01))*sigmah

zout <- c(outf, outf-rev(spseq)[1], outf-rev(spseq), outf, outf+spseq, outf+spseq[NROW(spseq)])
