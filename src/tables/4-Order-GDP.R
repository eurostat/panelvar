# Clear Memory
rm(list=ls(all=TRUE))

fload.csv <- function(Z)
{
  xe <- as.matrix(read.csv(Z, header=TRUE))
  xed <- as.character(xe[,1])
  xe <- apply(xe[,2:NCOL(xe)], 2, as.numeric)
  rownames(xe) <- xed
  return(xe)
}

print.stats <- function(h1n, h1s, h1nsav, h, ibench)
{
  e1 <- fload.csv(h1n[1])
  e2 <- fload.csv(h1n[2])
  e <- cbind(e1, e2)
  
  s1 <- colMeans(abs(e), na.rm=TRUE)
  s2 <- sqrt(colMeans(e^2, na.rm=TRUE))
  
  ibn <- which(colnames(e)==ibench)
  
  DM <- matrix(NA, NROW(s1), 2)
  for(j in 1:NCOL(e))
  {
    if(j!=ibn){
      DM[j,1] <- dm.test(e[,j], e[,ibn], alternative = c("two.sided"), h, power=1)$p.value
      DM[j,2] <- dm.test(e[,j], e[,ibn], alternative = c("two.sided"), h, power=2)$p.value
    }
  }
  
  ssr1 <- fload.csv(h1s[1]); ssr1 <- ssr1[,NCOL(ssr1)]
  ssr1 <- as.matrix(ssr1); ssr1n <- rownames(ssr1)
  ssr2 <- as.matrix(read.csv(h1s[2]))
  ssr2n <- ssr2[,1]
  ssr2 <- as.numeric(ssr2[,2])
  
  ssr <- c(ssr1, ssr2)
  
  mnamsall <- c(ssr1n, ssr2n)
  stt <- cbind(s1/s1[ibn], s2/s2[ibn], ssr, DM)
  rownames(stt) <- mnamsall
  colnames(stt) <- c("MAE", "RMSFE", "SSR", "DM1", "DM2")
  
  # setwd("../data/tables/")  
  # write.csv(stt[1:37,], h1nsav)
  return(stt)
}

get.cq <- function(sers, num1, num2)
{
  xbt.l <- which(as.numeric(sers)==num1)
  xbt.h <- which(as.numeric(sers)==num2)
  xbt <- c(xbt.l, xbt.h)
  return(xbt)
}

get.CR <- function(z, zt)
{
  crt <- matrix(NA, NROW(z), 1)
  for(j in 1:NROW(z))
  {
    if(is.na(z[j])){next}
    if( (zt[j]<=z[j,2]) & (zt[j]>=z[j,1])){crt[j] <- 1}else{crt[j] <- 0}
  }
  return(mean(crt, na.rm=TRUE)) 
}

get.allCR <- function(hh1n, hh1s, hc)
{
  q1 <- loadToEnv(hh1n)[["fquant"]]
  if(hc=="DE"){ q2 <- loadToEnv(hh1s)[["fquant1"]] }
  if(hc=="FR"){ q2 <- loadToEnv(hh1s)[["fquant2"]] }
  if(hc=="IT"){ q2 <- loadToEnv(hh1s)[["fquant3"]] }
  if(hc=="UK"){ q2 <- loadToEnv(hh1s)[["fquant4"]] }
  qtr <- loadToEnv(hh1n)[["ftrue"]]
  
  nams1 <- dimnames(q1)[3][[1]]
  nams2 <- dimnames(q2)[3][[1]]
  allqn <- c(nams1, nams2[1:10])
  
  CR <- matrix(NA, NROW(allqn), 2)
  rownames(CR) <- allqn
  colnames(CR) <- crn
  
  for(i in 1:NROW(allqn))
  {
    im <- allqn[i]
    
    w1 <- which(nams1==im)
    w2 <- which(nams2==im)
    
    if(NROW(w1)>0){ xq <- q1[,,w1]}
    if(NROW(w2)>0){ xq <- q2[,,w2]}
    
    xq1 <- xq[, get.cq(colnames(xq), cr1[1], cr2[1])]
    xq2 <- xq[, get.cq(colnames(xq), cr1[2], cr2[2])]
    
    # CR[i,] <- c(get.CR(xq1, qtr), get.CR(xq2, qtr), get.CR(xq3, qtr),
    #             get.CR(xq4, qtr), get.CR(xq5, qtr))
    
    CR[i,] <- c(get.CR(xq1, qtr), get.CR(xq2, qtr))
  }
  return(CR)
}

get.ord <- function(hh1n)
{
  y <- loadToEnv(hh1n)[["y"]]
  x <- loadToEnv(hh1n)[["xx"]]
  xexog <- loadToEnv(hh1n)[["xFGR1"]]
  Y <- cbind(y, x, xexog)
  colnames(Y) <- paste("Y", 1:NCOL(Y), sep="")
  Y <- na.omit(Y)
  y <- Y[,1:3]
  x <- Y[,4:NCOL(Y)]
  y <- as.matrix(y)
  x <- as.matrix(x)
  colnames(y) <- paste("Y", 1:NCOL(y), sep="")
  colnames(x) <- paste("X", 1:NCOL(x), sep="")
  outv <- VARselect(y, lag.max=6, type = "none", exogen=x)$selection
  return(outv)
}

library("zoo"); library("lubridate"); library("forecast")
library("imputeTS"); library("panelvar"); library("moments")
library("R.utils"); library("vars")

iibench <- "AR.1."

setwd("../data/output")

wdd <- "../output/" # relative path
wds <- "../tables/"

c1 <- get.ord("DE-Q-itarg11-h1.Rdata")
c2 <- get.ord("FR-Q-itarg11-h1.Rdata")
c3 <- get.ord("IT-Q-itarg11-h1.Rdata")
c4 <- get.ord("UK-Q-itarg11-h1.Rdata")
I1 <- rbind(c1, c2, c3, c4)

c1 <- get.ord("DE-M-itarg3453-h1.Rdata")
c2 <- get.ord("FR-M-itarg3453-h1.Rdata")
c3 <- get.ord("IT-M-itarg3453-h1.Rdata")
c4 <- get.ord("UK-M-itarg3453-h1.Rdata")
I2 <- rbind(c1, c2, c3, c4)

c1 <- get.ord("DE-M-itarg2342-h1.Rdata")
c2 <- get.ord("FR-M-itarg2342-h1.Rdata")
c3 <- get.ord("IT-M-itarg2342-h1.Rdata")
c4 <- get.ord("UK-M-itarg2342-h1.Rdata")
I3 <- rbind(c1, c2, c3, c4)

c1 <- get.ord("DE-M-itarg4254-h1.Rdata")
c2 <- get.ord("FR-M-itarg4254-h1.Rdata")
c3 <- get.ord("IT-M-itarg4254-h1.Rdata")
c4 <- get.ord("UK-M-itarg4254-h1.Rdata")
I4 <- rbind(c1, c2, c3, c4)

I <- cbind(I1[,3], I2[,3], I3[,3], I4[,3])
colnames(I) <- c("GDP", "IP", "UNR", "CPI")
rownames(I) <- c("DE", "FR", "IT", "UK")

setwd(wds)
write.csv(I, "VARords.csv")
