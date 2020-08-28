# Clear Memory
rm(list=ls(all=TRUE))

# Load functions
library("zoo")
library("lubridate")
library("forecast")
library("imputeTS")
library("panelvar")

# Define working directories
wd1 <- "../model" # relative path to model directory
wd2 <- "../data/input" # relative paths to wd1
wd3 <- "../data/output/

# User settings
# for the quarterly file, you can only use GDP
# 1:GDP, 2: UNR, 3: IP, 4: CPI, 5:FF
itarg <- 1
jtarg <- c(4,5)
hh <- c(1,2,3,4)  # > 0
ibench <- "AR(1)"
hbounds <- FALSE    # if TRUE, then sigmah*sqrt(h) is used in the bounds
                    # otherwise sigmah

# panel settings
system_instruments.p <- FALSE
system_constant.p <- FALSE
steps.p  <- "onestep"
lags.p <- 1
collapse.p <- TRUE
progress_bar.p <- FALSE
transfor.p <- "fd"
pca_instruments.p <- FALSE

# Load some additional functions
setwd(wd1)
source("functions.R")

# Load data
setwd(wd2)

# Load data for Country 1
ZQ1 <- load.data(as.matrix(read.csv("DE-Q.csv", header=FALSE)))
ZM1 <- load.data(as.matrix(read.csv("DE-M.csv", header=FALSE)))
ZD1 <- load.data(as.matrix(read.csv("DE-D.csv", header=FALSE)))
ZG1 <- load.data(as.matrix(read.csv("DE-Google.csv", header=FALSE)))

# Load data for Country 2
ZQ2 <- load.data(as.matrix(read.csv("FR-Q.csv", header=FALSE)))
ZM2 <- load.data(as.matrix(read.csv("FR-M.csv", header=FALSE)))
ZD2 <- load.data(as.matrix(read.csv("FR-D.csv", header=FALSE)))
ZG2 <- load.data(as.matrix(read.csv("FR-Google.csv", header=FALSE)))

# Load data for Country 3
ZQ3 <- load.data(as.matrix(read.csv("IT-Q.csv", header=FALSE)))
ZM3 <- load.data(as.matrix(read.csv("IT-M.csv", header=FALSE)))
ZD3 <- load.data(as.matrix(read.csv("IT-D.csv", header=FALSE)))
ZG3 <- load.data(as.matrix(read.csv("IT-Google.csv", header=FALSE)))

# Load data for Country 4
ZQ4 <- load.data(as.matrix(read.csv("UK-Q.csv", header=FALSE)))
ZM4 <- load.data(as.matrix(read.csv("UK-M.csv", header=FALSE)))
ZD4 <- load.data(as.matrix(read.csv("UK-D.csv", header=FALSE)))
ZG4 <- load.data(as.matrix(read.csv("UK-Google.csv", header=FALSE)))

# switch back so you can use other functions
setwd(wd1)

# We have different dates for macrofinance, google and reuters
# make sure we use the same dates everywhere and use same info across countries
ds1 <- max(c(min(ZQ1$dates), min(ZM1$dates), min(ZD1$dates),
          min(ZG1$dates)))
de1 <- min(c(max(ZQ1$dates), max(ZM1$dates), max(ZD1$dates),
            max(ZG1$dates)))

ds2 <- max(c(min(ZQ2$dates), min(ZM2$dates), min(ZD2$dates),
             min(ZG2$dates)))
de2 <- min(c(max(ZQ2$dates), max(ZM2$dates), max(ZD2$dates),
             max(ZG2$dates)))

ds3 <- max(c(min(ZQ3$dates), min(ZM3$dates), min(ZD3$dates),
             min(ZG3$dates)))
de3 <- min(c(max(ZQ3$dates), max(ZM3$dates), max(ZD3$dates),
             max(ZG3$dates)))

ds4 <- max(c(min(ZQ4$dates), min(ZM4$dates), min(ZD4$dates),
             min(ZG4$dates)))
de4 <- min(c(max(ZQ4$dates), max(ZM4$dates), max(ZD4$dates),
             max(ZG4$dates)))

ds <- max(c(ds1, ds2, ds3, ds4))
de <- min(c(de1, de2, de3, de4))

xselQ1 <- (ZQ1$dates>=ds) & (ZQ1$dates<=de)
xselM1 <- (ZM1$dates>=ds) & (ZM1$dates<=de)
xselD1 <- (ZD1$dates>=ds) & (ZD1$dates<=de)
xselG1 <- (ZG1$dates>=ds) & (ZG1$dates<=de)

xselQ2 <- (ZQ2$dates>=ds) & (ZQ2$dates<=de)
xselM2 <- (ZM2$dates>=ds) & (ZM2$dates<=de)
xselD2 <- (ZD2$dates>=ds) & (ZD2$dates<=de)
xselG2 <- (ZG2$dates>=ds) & (ZG2$dates<=de)

xselQ3 <- (ZQ3$dates>=ds) & (ZQ3$dates<=de)
xselM3 <- (ZM3$dates>=ds) & (ZM3$dates<=de)
xselD3 <- (ZD3$dates>=ds) & (ZD3$dates<=de)
xselG3 <- (ZG3$dates>=ds) & (ZG3$dates<=de)

xselQ4 <- (ZQ4$dates>=ds) & (ZQ4$dates<=de)
xselM4 <- (ZM4$dates>=ds) & (ZM4$dates<=de)
xselD4 <- (ZD4$dates>=ds) & (ZD4$dates<=de)
xselG4 <- (ZG4$dates>=ds) & (ZG4$dates<=de)

XQ1 <- ZQ1$values[xselQ1,]
XM1 <- ZM1$values[xselM1,]
XD1 <- ZD1$values[xselD1,]
XG1 <- ZG1$values[xselG1,]

XQ2 <- ZQ2$values[xselQ2,]
XM2 <- ZM2$values[xselM2,]
XD2 <- ZD2$values[xselD2,]
XG2 <- ZG2$values[xselG2,]

XQ3 <- ZQ3$values[xselQ3,]
XM3 <- ZM3$values[xselM3,]
XD3 <- ZD3$values[xselD3,]
XG3 <- ZG3$values[xselG3,]

XQ4 <- ZQ4$values[xselQ4,]
XM4 <- ZM4$values[xselM4,]
XD4 <- ZD4$values[xselD4,]
XG4 <- ZG4$values[xselG4,]

# MAKE A CHECK
if((NROW(XQ1)+NROW(XQ2)+NROW(XQ3)+NROW(XQ4))!=NROW(XQ1)*4){
  stop("Wrong dimensions in XQs! Check line 141")
}

for(jh in 1:NROW(hh))
{
  setwd(wd1)
  
  h <- hh[jh]
  # give name
  give.name <- paste("PVAR-Q-itarg", itarg, "-h", h, sep="")
  
  # make sure you start the loop on the same date
  iinfrom <- "2009-09-30"
  ifrom1 <- which(rownames(XQ1)==iinfrom)
  ifrom2 <- which(rownames(XQ2)==iinfrom)
  ifrom3 <- which(rownames(XQ3)==iinfrom)
  ifrom4 <- which(rownames(XQ4)==iinfrom)
  
  # MAKE A CHECK
  if((ifrom1+ifrom2+ifrom3+ifrom4)!=ifrom1*4){
    stop("Wrong dimensions in ifrom's! Check line 162")
  }
  ifrom <- ifrom1
  
  # prepare matrices for storage
  mnams <- c("PVAR(1)-GMM", "PVAR(1)-OLSCFE",
             "PVARX(1)-OLSCFE-MF-T1", "PVARX(1)-OLSCFE-G-T1",
              "PVARX(1)-OLSCFE-MFGR-T1",
             "PVARX(1)-OLSCFE-MF-T2", "PVARX(1)-OLSCFE-G-T2",
             "PVARX(1)-OLSCFE-MFGR-T2",
             "PVAR(1)-OLSCTFE",
             "PVARX(1)-OLSCTFE-MF-T1", "PVARX(1)-OLSCTFE-G-T1",
             "PVARX(1)-OLSCTFE-MFGR-T1",
             "PVARX(1)-OLSCTFE-MF-T2", "PVARX(1)-OLSCTFE-G-T2",
             "PVARX(1)-OLSCTFE-MFGR-T2")
  
  fest <- matrix(NA, NROW(XQ1), NROW(mnams))
  rownames(fest) <- rownames(XQ1)
  colnames(fest) <- mnams
  fest2 <- fest3 <- fest4 <- fest1 <- fest
  
  pintv <- seq(0, 1, 0.01)
  fquant <- array(NA, c(NROW(fest), NROW(pintv), NROW(mnams)),
                           dimnames=list(rownames(fest), as.character(pintv), mnams))
  fquant1 <- fquant2 <- fquant3 <- fquant4 <- fquant
  
  for(i in ifrom:(NROW(XQ1)-h))
  {
    # in-sample
    xq1 <- XQ1[1:i, ]
    xq2 <- XQ2[1:i, ]
    xq3 <- XQ3[1:i, ]
    xq4 <- XQ4[1:i, ]
    
    # Using the above xq, construct the corresponding monthly datasets
    dq1 <- as.Date(rownames(xq1))
    dm1 <- crspn.dates.QtoM(dq1)
    xm1 <- XM1[as.character(dm1),]
    xd1 <- make.data.DWtoM(dm1, as.matrix(XD1))
    xg1 <- make.data.DWtoM(dm1, as.matrix(XG1))

    dq2 <- as.Date(rownames(xq2))
    dm2 <- crspn.dates.QtoM(dq2)
    xm2 <- XM2[as.character(dm2),]
    xd2 <- make.data.DWtoM(dm2, as.matrix(XD2))
    xg2 <- make.data.DWtoM(dm2, as.matrix(XG2))

    dq3 <- as.Date(rownames(xq3))
    dm3 <- crspn.dates.QtoM(dq3)
    xm3 <- XM3[as.character(dm3),]
    xd3 <- make.data.DWtoM(dm3, as.matrix(XD3))
    xg3 <- make.data.DWtoM(dm3, as.matrix(XG3))

    dq4 <- as.Date(rownames(xq4))
    dm4 <- crspn.dates.QtoM(dq4)
    xm4 <- XM4[as.character(dm4),]
    xd4 <- make.data.DWtoM(dm4, as.matrix(XD4))
    xg4 <- make.data.DWtoM(dm4, as.matrix(XG4))

    # if NAs use interpolation
    xm1 <- apply(xm1, 2, na.interpolation, option="linear")
    xd1 <- apply(xd1, 2, na.interpolation, option="linear")
    xg1 <- apply(xg1, 2, na.interpolation, option="linear")

    xm2 <- apply(xm2, 2, na.interpolation, option="linear")
    xd2 <- apply(xd2, 2, na.interpolation, option="linear")
    xg2 <- apply(xg2, 2, na.interpolation, option="linear")

    xm3 <- apply(xm3, 2, na.interpolation, option="linear")
    xd3 <- apply(xd3, 2, na.interpolation, option="linear")
    xg3 <- apply(xg3, 2, na.interpolation, option="linear")

    xm4 <- apply(xm4, 2, na.interpolation, option="linear")
    xd4 <- apply(xd4, 2, na.interpolation, option="linear")
    xg4 <- apply(xg4, 2, na.interpolation, option="linear")

    # Transform all variables
    xqs1 <- transform.vars(xq1, ZQ1$transf)
    xms1 <- transform.vars(xm1, ZM1$transf)
    xds1 <- transform.vars(xd1, ZD1$transf)
    xgs1 <- transform.vars(xg1, ZG1$transf)

    xqs2 <- transform.vars(xq2, ZQ2$transf)
    xms2 <- transform.vars(xm2, ZM2$transf)
    xds2 <- transform.vars(xd2, ZD2$transf)
    xgs2 <- transform.vars(xg2, ZG2$transf)

    xqs3 <- transform.vars(xq3, ZQ3$transf)
    xms3 <- transform.vars(xm3, ZM3$transf)
    xds3 <- transform.vars(xd3, ZD3$transf)
    xgs3 <- transform.vars(xg3, ZG3$transf)

    xqs4 <- transform.vars(xq4, ZQ4$transf)
    xms4 <- transform.vars(xm4, ZM4$transf)
    xds4 <- transform.vars(xd4, ZD4$transf)
    xgs4 <- transform.vars(xg4, ZG4$transf)

    # Extract Monthly factors using PCA
    Fm1 <- na.omit(cbind(xms1, xds1)); rnams <- rownames(Fm1)
    Fm1 <- getPCA(Fm1); rownames(Fm1) <- rnams
    Fg1 <- na.omit(xgs1); rnams <- rownames(Fg1)
    Fg1 <- getPCA(Fg1);  rownames(Fg1) <- rnams
    
    Fm2 <- na.omit(cbind(xms2, xds2)); rnams <- rownames(Fm2)
    Fm2 <- getPCA(Fm2); rownames(Fm2) <- rnams
    Fg2 <- na.omit(xgs2); rnams <- rownames(Fg2)
    Fg2 <- getPCA(Fg2);  rownames(Fg2) <- rnams

    Fm3 <- na.omit(cbind(xms3, xds3)); rnams <- rownames(Fm3)
    Fm3 <- getPCA(Fm3); rownames(Fm3) <- rnams
    Fg3 <- na.omit(xgs3); rnams <- rownames(Fg3)
    Fg3 <- getPCA(Fg3);  rownames(Fg3) <- rnams

    Fm4 <- na.omit(cbind(xms4, xds4)); rnams <- rownames(Fm4)
    Fm4 <- getPCA(Fm4); rownames(Fm4) <- rnams
    Fg4 <- na.omit(xgs4); rnams <- rownames(Fg4)
    Fg4 <- getPCA(Fg4);  rownames(Fg4) <- rnams

    # Extract target
    y1 <- as.matrix(xqs1[,ZQ1$target==itarg])
    y2 <- as.matrix(xqs2[,ZQ2$target==itarg])
    y3 <- as.matrix(xqs3[,ZQ3$target==itarg])
    y4 <- as.matrix(xqs4[,ZQ4$target==itarg])

    # Also extract the other 2 variables for the simple VAR
    x1 <- as.matrix(xms1[,ZM1$target==jtarg[1]])
    x2 <- as.matrix(xms1[,ZM1$target==jtarg[2]])
    xx1 <- make.data.MtoQ1(as.Date(rownames(y1)), as.matrix(cbind(x1, x2)))
    colnames(xx1) <- paste("X1.", 1:NCOL(xx1), sep="")
    
    x1 <- as.matrix(xms2[,ZM2$target==jtarg[1]])
    x2 <- as.matrix(xms2[,ZM2$target==jtarg[2]])
    xx2 <- make.data.MtoQ1(as.Date(rownames(y2)), as.matrix(cbind(x1, x2)))
    colnames(xx2) <- paste("X2.", 1:NCOL(xx2), sep="")

    x1 <- as.matrix(xms3[,ZM3$target==jtarg[1]])
    x2 <- as.matrix(xms3[,ZM3$target==jtarg[2]])
    xx3 <- make.data.MtoQ1(as.Date(rownames(y3)), as.matrix(cbind(x1, x2)))
    colnames(xx3) <- paste("X3.", 1:NCOL(xx3), sep="")
    
    x1 <- as.matrix(xms4[,ZM4$target==jtarg[1]])
    x2 <- as.matrix(xms4[,ZM4$target==jtarg[2]])
    xx4 <- make.data.MtoQ1(as.Date(rownames(y4)), as.matrix(cbind(x1, x2)))
    colnames(xx4) <- paste("X4.", 1:NCOL(xx4), sep="")
    
    # Transform monthly factors to quarterly
    # according to the quarterly dates of the target
    x1F1 <- make.data.MtoQ1(as.Date(rownames(y1)), as.matrix(Fm1[,1]))
    x1G1 <- make.data.MtoQ1(as.Date(rownames(y1)), as.matrix(Fg1[,1]))
    x1FGR1 <- cbind(x1F1, x1G1)
    x1F2 <- make.data.MtoQ2(as.Date(rownames(y1)), as.matrix(Fm1[,1]))
    x1G2 <- make.data.MtoQ2(as.Date(rownames(y1)), as.matrix(Fg1[,1]))
    x1FGR2 <- cbind(x1F2, x1G2)
    
    x2F1 <- make.data.MtoQ1(as.Date(rownames(y2)), as.matrix(Fm2[,1]))
    x2G1 <- make.data.MtoQ1(as.Date(rownames(y2)), as.matrix(Fg2[,1]))
    x2FGR1 <- cbind(x2F1, x2G1)
    x2F2 <- make.data.MtoQ2(as.Date(rownames(y2)), as.matrix(Fm2[,1]))
    x2G2 <- make.data.MtoQ2(as.Date(rownames(y2)), as.matrix(Fg2[,1]))
    x2FGR2 <- cbind(x2F2, x2G2)
    
    x3F1 <- make.data.MtoQ1(as.Date(rownames(y3)), as.matrix(Fm3[,1]))
    x3G1 <- make.data.MtoQ1(as.Date(rownames(y3)), as.matrix(Fg3[,1]))
    x3FGR1 <- cbind(x3F1, x3G1)
    x3F2 <- make.data.MtoQ2(as.Date(rownames(y3)), as.matrix(Fm3[,1]))
    x3G2 <- make.data.MtoQ2(as.Date(rownames(y3)), as.matrix(Fg3[,1]))
    x3FGR2 <- cbind(x3F2, x3G2)
    
    x4F1 <- make.data.MtoQ1(as.Date(rownames(y4)), as.matrix(Fm4[,1]))
    x4G1 <- make.data.MtoQ1(as.Date(rownames(y4)), as.matrix(Fg4[,1]))
    x4FGR1 <- cbind(x4F1, x4G1)
    x4F2 <- make.data.MtoQ2(as.Date(rownames(y4)), as.matrix(Fm4[,1]))
    x4G2 <- make.data.MtoQ2(as.Date(rownames(y4)), as.matrix(Fg4[,1]))
    x4FGR2 <- cbind(x4F2, x4G2)
    
    # make a lag
    yL1 <- as.matrix(c(NA, y1[1:(NROW(y1)-1),]))
    yL2 <- as.matrix(c(NA, y2[1:(NROW(y1)-1),]))
    yL3 <- as.matrix(c(NA, y3[1:(NROW(y1)-1),]))
    yL4 <- as.matrix(c(NA, y4[1:(NROW(y1)-1),]))
 
    # First, bind together the y,xx
    vny <- c("y1", "y2", "y3")
    vni <- c("cid", "tid")
    
    Z <- cbind(y1, xx1); zz <- 1
    t <- NROW(Z); czi <- cbind(rep(zz, t), 1:t)
    c1y <- Z
    c1i <- czi
    c1ex1a <- x1F1;   c1ex1b <- x1G1; 
    c1ex1d <- x1FGR1; c1ex2a <- x1F2; c1ex2b <- x1G2 
     c1ex2d <- x1FGR2 

    Z <- cbind(y2, xx2); zz <- 2
    t <- NROW(Z); czi <- cbind(rep(zz, t), 1:t)
    c2y <- Z
    c2i <- czi
    c2ex1a <- x2F1
    c2ex1b <- x2G1
    c2ex1d <- x2FGR1
    c2ex2a <- x2F2
    c2ex2b <- x2G2 
    c2ex2d <- x2FGR2 

    Z <- cbind(y3, xx3); zz <- 3
    t <- NROW(Z); czi <- cbind(rep(zz, t), 1:t)
    c3y <- Z
    c3i <- czi
    c3ex1a <- x3F1
    c3ex1b <- x3G1
    c3ex1d <- x3FGR1
    c3ex2a <- x3F2
    c3ex2b <- x3G2 
    c3ex2d <- x3FGR2 
  
    Z <- cbind(y4, xx4); zz <- 4
    t <- NROW(Z); czi <- cbind(rep(zz, t), 1:t)
    c4y <- Z
    c4i <- czi
    c4ex1a <- x4F1
    c4ex1b <- x4G1
    c4ex1d <- x4FGR1
    c4ex2a <- x4F2
    c4ex2b <- x4G2 
    c4ex2d <- x4FGR2 

    # PVAR(1) - No exogenous
    imodel <- "PVAR(1)-GMM"
    c1 <- cbind(c1i, c1y)
    c2 <- cbind(c2i, c2y)
    c3 <- cbind(c3i, c3y)
    c4 <- cbind(c4i, c4y)
    C <- rbind(c1, c2, c3, c4)
    colnames(C) <- c(vni, vny)
    C <- as.data.frame(C)
    
    flag.exog <- FALSE
    PVARe <- pvargmm(dependent_vars=vny, lags=lags.p, transformation=transfor.p,
                     #exog_vars=NULL,
                     data=C, panel_identifier=vni, steps=steps.p,
                     system_instruments=system_instruments.p,
                     system_constant=system_constant.p,
                     pca_instruments=pca_instruments.p,
                     collapse=collapse.p, progressbar=progress_bar.p)
    source("PVARfor.R")
    fest1[(i+h),imodel] <- zout1.y1[1]
    fquant1[(i+h), ,imodel] <- zout1.y1[2:NROW(zout1.y1)]
    fest2[(i+h),imodel] <- zout2.y1[1]
    fquant2[(i+h), ,imodel] <- zout2.y1[2:NROW(zout2.y1)]
    fest3[(i+h),imodel] <- zout3.y1[1]
    fquant3[(i+h), ,imodel] <- zout3.y1[2:NROW(zout3.y1)]
    fest4[(i+h),imodel] <- zout4.y1[1]
    fquant4[(i+h), ,imodel] <- zout4.y1[2:NROW(zout4.y1)]

    # PVAR(1) - No exogenous
    imodel <- "PVAR(1)-OLSCFE"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
               c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    X1 <- lag.multi(c1y, glag); FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    X2 <- lag.multi(c2y, glag); FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    X3 <- lag.multi(c3y, glag); FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    X4 <- lag.multi(c4y, glag); FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")

    imodel <- "PVARX(1)-OLSCFE-MF-T1"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex1a
    extra2 <- c2ex1a
    extra3 <- c3ex1a
    extra4 <- c4ex1a
    X1 <- cbind(lag.multi(c1y, glag), extra1)
    X2 <- cbind(lag.multi(c2y, glag), extra2)
    X3 <- cbind(lag.multi(c3y, glag), extra3)
    X4 <- cbind(lag.multi(c4y, glag), extra4)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCFE-G-T1"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex1b
    extra2 <- c2ex1b
    extra3 <- c3ex1b
    extra4 <- c4ex1b
    X1 <- cbind(lag.multi(c1y, glag), extra1)
    X2 <- cbind(lag.multi(c2y, glag), extra2)
    X3 <- cbind(lag.multi(c3y, glag), extra3)
    X4 <- cbind(lag.multi(c4y, glag), extra4)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCFE-MFGR-T1"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex1d
    extra2 <- c2ex1d
    extra3 <- c3ex1d
    extra4 <- c4ex1d
    X1 <- cbind(lag.multi(c1y, glag), extra1)
    X2 <- cbind(lag.multi(c2y, glag), extra2)
    X3 <- cbind(lag.multi(c3y, glag), extra3)
    X4 <- cbind(lag.multi(c4y, glag), extra4)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCFE-MF-T2"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex2a
    extra2 <- c2ex2a
    extra3 <- c3ex2a
    extra4 <- c4ex2a
    X1 <- cbind(lag.multi(c1y, glag), extra1)
    X2 <- cbind(lag.multi(c2y, glag), extra2)
    X3 <- cbind(lag.multi(c3y, glag), extra3)
    X4 <- cbind(lag.multi(c4y, glag), extra4)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCFE-G-T2"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex2b
    extra2 <- c2ex2b
    extra3 <- c3ex2b
    extra4 <- c4ex2b
    X1 <- cbind(lag.multi(c1y, glag), extra1)
    X2 <- cbind(lag.multi(c2y, glag), extra2)
    X3 <- cbind(lag.multi(c3y, glag), extra3)
    X4 <- cbind(lag.multi(c4y, glag), extra4)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCFE-MFGR-T2"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex2d
    extra2 <- c2ex2d
    extra3 <- c3ex2d
    extra4 <- c4ex2d
    X1 <- cbind(lag.multi(c1y, glag), extra1)
    X2 <- cbind(lag.multi(c2y, glag), extra2)
    X3 <- cbind(lag.multi(c3y, glag), extra3)
    X4 <- cbind(lag.multi(c4y, glag), extra4)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVAR(1)-OLSCTFE"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    Dt <- diag(NROW(c1i))
    X1 <- cbind(lag.multi(c1y, glag), Dt)
    X2 <- cbind(lag.multi(c2y, glag), Dt)
    X3 <- cbind(lag.multi(c3y, glag), Dt)
    X4 <- cbind(lag.multi(c4y, glag), Dt)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCTFE-MF-T1"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex1a
    extra2 <- c2ex1a
    extra3 <- c3ex1a
    extra4 <- c4ex1a
    X1 <- cbind(lag.multi(c1y, glag), extra1, Dt)
    X2 <- cbind(lag.multi(c2y, glag), extra2, Dt)
    X3 <- cbind(lag.multi(c3y, glag), extra3, Dt)
    X4 <- cbind(lag.multi(c4y, glag), extra4, Dt)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCTFE-G-T1"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex1b
    extra2 <- c2ex1b
    extra3 <- c3ex1b
    extra4 <- c4ex1b
    X1 <- cbind(lag.multi(c1y, glag), extra1, Dt)
    X2 <- cbind(lag.multi(c2y, glag), extra2, Dt)
    X3 <- cbind(lag.multi(c3y, glag), extra3, Dt)
    X4 <- cbind(lag.multi(c4y, glag), extra4, Dt)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCTFE-MFGR-T1"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex1d
    extra2 <- c2ex1d
    extra3 <- c3ex1d
    extra4 <- c4ex1d
    X1 <- cbind(lag.multi(c1y, glag), extra1, Dt)
    X2 <- cbind(lag.multi(c2y, glag), extra2, Dt)
    X3 <- cbind(lag.multi(c3y, glag), extra3, Dt)
    X4 <- cbind(lag.multi(c4y, glag), extra4, Dt)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCTFE-MF-T2"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex2a
    extra2 <- c2ex2a
    extra3 <- c3ex2a
    extra4 <- c4ex2a
    X1 <- cbind(lag.multi(c1y, glag), extra1, Dt)
    X2 <- cbind(lag.multi(c2y, glag), extra2, Dt)
    X3 <- cbind(lag.multi(c3y, glag), extra3, Dt)
    X4 <- cbind(lag.multi(c4y, glag), extra4, Dt)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    imodel <- "PVARX(1)-OLSCTFE-G-T2"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex2b
    extra2 <- c2ex2b
    extra3 <- c3ex2b
    extra4 <- c4ex2b
    X1 <- cbind(lag.multi(c1y, glag), extra1, Dt)
    X2 <- cbind(lag.multi(c2y, glag), extra2, Dt)
    X3 <- cbind(lag.multi(c3y, glag), extra3, Dt)
    X4 <- cbind(lag.multi(c4y, glag), extra4, Dt)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
  
    imodel <- "PVARX(1)-OLSCTFE-MFGR-T2"
    c1 <- c1y
    c2 <- c2y
    c3 <- c3y
    c4 <- c4y
    ci <- rbind(c1i, c2i, c3i, c4i)
    Ch <- rbind(c1[(h+1):NROW(c1),], c2[(h+1):NROW(c2),],
                c3[(h+1):NROW(c3),], c4[(h+1):NROW(c4),])
    Chi <- rbind(c1i[(h+1):NROW(c1i),], c2i[(h+1):NROW(c2i),],
                 c3i[(h+1):NROW(c3i),], c4i[(h+1):NROW(c4i),])
    colnames(Ch) <- paste("Y", 1:NCOL(Ch), sep="")
    # create the lags
    glag <- 1
    extra1 <- c1ex2d
    extra2 <- c2ex2d
    extra3 <- c3ex2d
    extra4 <- c4ex2d
    X1 <- cbind(lag.multi(c1y, glag), extra1, Dt)
    X2 <- cbind(lag.multi(c2y, glag), extra2, Dt)
    X3 <- cbind(lag.multi(c3y, glag), extra3, Dt)
    X4 <- cbind(lag.multi(c4y, glag), extra4, Dt)
    FEm <- matrix(0, NROW(X1), 4); FEm[,1] <- 1; X1 <- cbind(X1, FEm)
    FEm <- matrix(0, NROW(X2), 4); FEm[,2] <- 1; X2 <- cbind(X2, FEm)
    FEm <- matrix(0, NROW(X3), 4); FEm[,3] <- 1; X3 <- cbind(X3, FEm)
    FEm <- matrix(0, NROW(X4), 4); FEm[,4] <- 1; X4 <- cbind(X4, FEm)
    CX <- rbind(X1[1:(NROW(X1)-h),], X2[1:(NROW(X2)-h),],
                X3[1:(NROW(X3)-h),], X4[1:(NROW(X4)-h),])
    colnames(CX) <- paste("X", 1:NCOL(CX), sep="")
    out.lm <- lm(Ch[,1]~CX-1)
    source("PVARfor-OLS.R")
    
    cat("Now doing ", i, " of ", (NROW(XQ1)-h), " and h ", h, "\n")
  }
  
  # Save the output
  setwd(wd3); KQ <- ZQ1; KQQ <- XQ1; Kfest <- fest1; Kquant <- fquant1; knams <- "DE"; setwd(wd1); source("PVAR-post.R"); setwd(wd3)
  setwd(wd3); KQ <- ZQ2; KQQ <- XQ2; Kfest <- fest2; Kquant <- fquant2; knams <- "FR"; setwd(wd1); source("PVAR-post.R"); setwd(wd3)
  setwd(wd3); KQ <- ZQ3; KQQ <- XQ3; Kfest <- fest3; Kquant <- fquant3; knams <- "IT"; setwd(wd1); source("PVAR-post.R"); setwd(wd3)
  setwd(wd3); KQ <- ZQ4; KQQ <- XQ4; Kfest <- fest4; Kquant <- fquant4; knams <- "UK"; setwd(wd1); source("PVAR-post.R"); setwd(wd3)
  
  save.image(paste(give.name, ".Rdata", sep=""))
}
