#!/bin/bash
nohup R CMD BATCH ./src/PVAR-m1.R ./src/PVAR-m1.Rout &
nohup R CMD BATCH ./src/PVAR-m2.R ./src/PVAR-m2.Rout &
nohup R CMD BATCH ./src/PVAR-m3.R ./src/PVAR-m3.Rout &
nohup R CMD BATCH ./src/PVAR-q1.R ./src/PVAR-q1.Rout &

