# Clear Memory
rm(list=ls(all=TRUE))

# Load functions
library("zoo")
library("lubridate")
library("forecast")
library("imputeTS")

# Define working directories
wd1 <- dirname(sys.frame(1)$ofile) # absolute path to current directory
wd2 <- "../data/input" # relative paths from wd1
wd3 <- "../data/output/"

# User settings
# for the quarterly file, you can only use GDP
# 1:GDP, 2: UNR, 3: IP, 4: CPI, 5:FF
# THREE-DIMENSIONAL MODEL ONLY
itarg <- 3
jtarg <- c(4,5)
hh <- c(1,3,6,12)  # > 0
ibench <- "AR(1)"
hbounds <- FALSE    # if TRUE, then sigmah*sqrt(h) is used in the bounds
                    # otherwise sigmah

# Load some additional functions
setwd(wd1)
source("functions.R")

# Load data
setwd(wd2)

ZM <- load.data(as.matrix(read.csv("IT-M.csv", header=FALSE)))
ZD <- load.data(as.matrix(read.csv("IT-D.csv", header=FALSE)))
ZG <- load.data(as.matrix(read.csv("IT-Google.csv", header=FALSE)))

# switch back so you can use other functions
setwd(wd1)

# We have different dates for macrofinance, google and reuters
# make sure we use the same dates everywhere
ds <- max(c(min(ZM$dates), min(ZD$dates),
          min(ZG$dates)))
de <- min(c(max(ZM$dates), max(ZD$dates),
            max(ZG$dates)))

xselM <- (ZM$dates>=ds) & (ZM$dates<=de)
xselD <- (ZD$dates>=ds) & (ZD$dates<=de)
xselG <- (ZG$dates>=ds) & (ZG$dates<=de)

XM <- ZM$values[xselM,]
XD <- ZD$values[xselD,]
XG <- ZG$values[xselG,]

for(jh in 1:NROW(hh))
{
  setwd(wd1)
  
  h <- hh[jh]
  # give name
  give.name <- paste("IT-M-itarg345", itarg, "-h", h, sep="")

  # make sure you start the loop on the same date
  iinfrom <- "2007-01-31"
  ifrom <- which(rownames(XM)==iinfrom)
  
  # prepare matrices for storage
  mnams <- c("Naive", "AR(1)",
             "LR-MF-T1", "LR-G-T1", "LR-MFGR-T1",
             "LR-MF-T2", "LR-G-T2", "LR-MFGR-T2",
             "AR(1)-MF-T1", "AR(1)-G-T1", "AR(1)-MFGR-T1",
             "AR(1)-MF-T2", "AR(1)-G-T2", "AR(1)-MFGR-T2",
             "VAR(1)",
             "VARX(1)-MF-T1", "VARX(1)-G-T1", "VARX(1)-MFGR-T1",
             "VARX(1)-MF-T2", "VARX(1)-G-T2", "VARX(1)-MFGR-T2")
  fest <- matrix(NA, NROW(XM), NROW(mnams))
  rownames(fest) <- rownames(XM)
  colnames(fest) <- mnams
  
  pintv <- seq(0, 1, 0.01)
  fquant <- array(NA, c(NROW(fest), NROW(pintv), NROW(mnams)),
                           dimnames=list(rownames(fest), as.character(pintv), mnams))
  
  for(i in ifrom:(NROW(XM)-h))
  {
    # in-sample
    xm <- XM[1:i, ]
    dm <- as.Date(rownames(xm))
    xd <- make.data.DWtoW(dm, XD)
    xg <- make.data.DWtoW(dm, XG)
    
    # if NAs use interpolation
    xd <- apply(xd, 2, na.interpolation, option="linear")
    xg <- apply(xg, 2, na.interpolation, option="linear")

    # Transform all variables
    xms <- transform.vars(xm, ZM$transf)
    xds <- transform.vars(xd, ZD$transf)
    xgs <- transform.vars(xg, ZG$transf)

    # Extract weekly factors using PCA
    Fw <- na.omit(xds); rnams <- rownames(Fw)
    Fw <- getPCA(Fw); rownames(Fw) <- rnams
    Fg <- na.omit(xgs); rnams <- rownames(Fg)
    Fg <- getPCA(Fg);  rownames(Fg) <- rnams
    
    # Extract target
    y <- as.matrix(xms[,ZM$target==itarg])
    
    # Also extract the other 2 variables for the simple VAR
    x1 <- as.matrix(xms[,ZM$target==jtarg[1]])
    x2 <- as.matrix(xms[,ZM$target==jtarg[2]])
    xx <- cbind(x1, x2)
    colnames(xx) <- paste("X", 1:NCOL(xx), sep="")
    
    # Transform weekly factors to monthly
    # according to the dates
    xF1 <- make.data.WtoM1(as.Date(rownames(y)), as.matrix(Fw[,1]))
    xG1 <- make.data.WtoM1(as.Date(rownames(y)), as.matrix(Fg[,1]))
    xFGR1 <- cbind(xF1, xG1)
    
    xF2 <- make.data.WtoM2(as.Date(rownames(y)), as.matrix(Fw[,1]))
    xG2 <- make.data.WtoM2(as.Date(rownames(y)), as.matrix(Fg[,1]))
    xFGR2 <- cbind(xF2, xG2)
    
    # make a lag
    yL1 <- as.matrix(c(NA, y[1:(NROW(y)-1),]))
    
    # make forecasts
    imodel <- "Naive"; z <- y; source("naive.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "AR(1)"; YY <- y; XX <- lag.uni(as.matrix(y), 1); source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "LR-MF-T1"; XX <- xF1; YY <- y; source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "LR-G-T1"; XX <- xG1; YY <- y; source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "LR-MFGR-T1"; XX <- xFGR1; YY <- y; source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "LR-MF-T2"; XX <- xF2; YY <- y; source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "LR-G-T2"; XX <- xG2; YY <- y; source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]

    imodel <- "LR-MFGR-T2"; XX <- xFGR2; YY <- y; source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "AR(1)-MF-T1"; YY <- y; XX <- cbind(lag.uni(as.matrix(y), 1), xF1); source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "AR(1)-G-T1"; YY <- y; XX <- cbind(lag.uni(as.matrix(y), 1), xG1); source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]

    imodel <- "AR(1)-MFGR-T1"; YY <- y; XX <- cbind(lag.uni(as.matrix(y), 1), xFGR1); source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "AR(1)-MF-T2"; YY <- y; XX <- cbind(lag.uni(as.matrix(y), 1), xF2); source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "AR(1)-G-T2"; YY <- y; XX <- cbind(lag.uni(as.matrix(y), 1), xG2); source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]

    imodel <- "AR(1)-MFGR-T2"; YY <- y; XX <- cbind(lag.uni(as.matrix(y), 1), xFGR2); source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "VAR(1)"; glag <- 1
    YY <- y; XX <- cbind(lag.uni(as.matrix(y), glag), lag.multi(xx, glag))
    source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "VARX(1)-MF-T1"; glag <- 1
    YY <- y; XX <- cbind(lag.uni(as.matrix(y), glag), lag.multi(xx, glag), xF1)
    source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "VARX(1)-G-T1"; glag <- 1
    YY <- y; XX <- cbind(lag.uni(as.matrix(y), glag), lag.multi(xx, glag), xG1)
    source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "VARX(1)-MFGR-T1"; glag <- 1
    YY <- y; XX <- cbind(lag.uni(as.matrix(y), glag), lag.multi(xx, glag), xFGR1)
    source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "VARX(1)-MF-T2"; glag <- 1
    YY <- y; XX <- cbind(lag.uni(as.matrix(y), glag), lag.multi(xx, glag), xF2)
    source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "VARX(1)-G-T2"; glag <- 1
    YY <- y; XX <- cbind(lag.uni(as.matrix(y), glag), lag.multi(xx, glag), xG2)
    source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    imodel <- "VARX(1)-MFGR-T2"; glag <- 1
    YY <- y; XX <- cbind(lag.uni(as.matrix(y), glag), lag.multi(xx, glag), xFGR2)
    source("Flinreg.R")
    fest[(i+h), imodel] <- zout[1]; fquant[(i+h), ,imodel] <- zout[2:NROW(zout)]
    
    cat("Now doing ", i, " of ", (NROW(XM)-h), " and h ", h, "\n")
  }
  
  fwhich <- which(ZM$target==itarg)
  ftrue <- as.matrix(XM[,fwhich])
  ftrue <- transform.vars(ftrue, ZM$transf[fwhich])
  
  e <- matrix(ftrue, NROW(ftrue), NCOL(fest))-fest
  rownames(e) <- rownames(fest)
  colnames(e) <- colnames(fest)
  e <- na.omit(e)
  
  ftrue.diff <- transform.vars(as.matrix(ftrue), 3)
  fest.diff <- transform.vars(fest, rep(3, NCOL(fest)))
  ss.diff <- sign(matrix(ftrue.diff, NROW(ftrue.diff), NCOL(fest.diff)))-sign(fest.diff)
  ss.diff <- na.omit(ss.diff)
  
  # Save the output
  setwd(wd3)
  
  ibn <- which(colnames(e)==ibench)
  
  MAE <- colMeans(abs(e))
  RMSFE <- sqrt(colMeans(e^2))
  SSR <- colMeans((ss.diff==0))
  
  stats1 <- cbind(MAE, RMSFE, SSR)
  stats2 <- cbind(MAE/MAE[ibn], RMSFE/RMSFE[ibn], SSR)
  
  DM <- matrix(NA, NROW(stats1), 2)
  for(j in 1:NCOL(e))
  {
    if(j!=ibn){
      DM[j,1] <- dm.test(e[,j], e[,ibn], alternative = c("two.sided"), h=h, power=1)$p.value
      DM[j,2] <- dm.test(e[,j], e[,ibn], alternative = c("two.sided"), h=h, power=2)$p.value
     }
  }
  rownames(stats1) <- rownames(stats2) <- rownames(DM) <- mnams
  colnames(stats1) <- colnames(stats2) <- c("MAE", "RMSFE", "SSR")
  colnames(DM) <- c("DMabs", "DMsq")
  
  write.csv(e, paste(give.name, "-error.csv", sep=""))
  write.csv(stats2, paste(give.name, "-stats2.csv", sep=""))
  write.csv(DM, paste(give.name, "-DMpval.csv", sep=""))
  
  # Print the density plots as well as export the graph data
  
  for(jj in 1:NCOL(fest))
  {
    print.backplots(give.name, ftrue, fest[,jj],
                    fquant[rownames(ftrue), ,mnams[jj]], mnams[jj])
  }
  
  save.image(paste(give.name, ".Rdata", sep=""))
  
}
