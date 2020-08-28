#!/bin/bash
nohup R CMD BATCH ../src/DE-single-m1.R ../src/DE-single-m1.Rout &
nohup R CMD BATCH ../src/DE-single-m2.R ../src/DE-single-m2.Rout &
nohup R CMD BATCH ../src/DE-single-m3.R ../src/DE-single-m3.Rout &
nohup R CMD BATCH ../src/DE-single-q1.R ../src/DE-single-q1.Rout &
nohup R CMD BATCH ../src/FR-single-m1.R ../src/FR-single-m1.Rout &
nohup R CMD BATCH ../src/FR-single-m2.R ../src/FR-single-m2.Rout &
nohup R CMD BATCH ../src/FR-single-m3.R ../src/FR-single-m3.Rout &
nohup R CMD BATCH ../src/FR-single-q1.R ../src/FR-single-q1.Rout &
nohup R CMD BATCH ../src/IT-single-m1.R ../src/IT-single-m1.Rout &
nohup R CMD BATCH ../src/IT-single-m2.R ../src/IT-single-m2.Rout &
nohup R CMD BATCH ../src/IT-single-m3.R ../src/IT-single-m3.Rout &
nohup R CMD BATCH ../src/IT-single-q1.R ../src/IT-single-q1.Rout &
nohup R CMD BATCH ../src/PVAR-m1.R      ../src/PVAR-m1.Rout &
nohup R CMD BATCH ../src/PVAR-m2.R      ../src/PVAR-m2.Rout &
nohup R CMD BATCH ../src/PVAR-m3.R      ../src/PVAR-m3.Rout &
nohup R CMD BATCH ../src/PVAR-q1.R      ../src/PVAR-q1.Rout &
nohup R CMD BATCH ../src/UK-single-m1.R ../src/UK-single-m1.Rout &
nohup R CMD BATCH ../src/UK-single-m2.R ../src/UK-single-m2.Rout &
nohup R CMD BATCH ../src/UK-single-m3.R ../src/UK-single-m3.Rout &
nohup R CMD BATCH ../src/UK-single-q1.R ../src/UK-single-q1.Rout &

