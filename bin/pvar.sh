#!/bin/bash
nohup R CMD BATCH ../model/DE-single-m1.R ../model/DE-single-m1.Rout &
nohup R CMD BATCH ../model/DE-single-m2.R ../model/DE-single-m2.Rout &
nohup R CMD BATCH ../model/DE-single-m3.R ../model/DE-single-m3.Rout &
nohup R CMD BATCH ../model/DE-single-q1.R ../model/DE-single-q1.Rout &
nohup R CMD BATCH ../model/FR-single-m1.R ../model/FR-single-m1.Rout &
nohup R CMD BATCH ../model/FR-single-m2.R ../model/FR-single-m2.Rout &
nohup R CMD BATCH ../model/FR-single-m3.R ../model/FR-single-m3.Rout &
nohup R CMD BATCH ../model/FR-single-q1.R ../model/FR-single-q1.Rout &
nohup R CMD BATCH ../model/IT-single-m1.R ../model/IT-single-m1.Rout &
nohup R CMD BATCH ../model/IT-single-m2.R ../model/IT-single-m2.Rout &
nohup R CMD BATCH ../model/IT-single-m3.R ../model/IT-single-m3.Rout &
nohup R CMD BATCH ../model/IT-single-q1.R ../model/IT-single-q1.Rout &
nohup R CMD BATCH ../model/PVAR-m1.R      ../model/PVAR-m1.Rout &
nohup R CMD BATCH ../model/PVAR-m2.R      ../model/PVAR-m2.Rout &
nohup R CMD BATCH ../model/PVAR-m3.R      ../model/PVAR-m3.Rout &
nohup R CMD BATCH ../model/PVAR-q1.R      ../model/PVAR-q1.Rout &
nohup R CMD BATCH ../model/UK-single-m1.R ../model/UK-single-m1.Rout &
nohup R CMD BATCH ../model/UK-single-m2.R ../model/UK-single-m2.Rout &
nohup R CMD BATCH ../model/UK-single-m3.R ../model/UK-single-m3.Rout &
nohup R CMD BATCH ../model/UK-single-q1.R ../model/UK-single-q1.Rout &

