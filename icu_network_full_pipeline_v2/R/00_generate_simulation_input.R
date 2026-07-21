#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table); library(MASS)})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("Usage: Rscript 00_generate_simulation_input.R <data_csv> <dictionary_csv>")
data_file <- args[[1]]; dictionary_file <- args[[2]]
dir.create(dirname(data_file), recursive = TRUE, showWarnings = FALSE)

set.seed(20260713)
n_icu <- 450L; n_non <- 1800L; n <- n_icu + n_non
icu <- c(rep(1L, n_icu), rep(0L, n_non))
nodes <- c(paste0("ANX",1:7),paste0("DEP",1:9),paste0("STR",1:10),paste0("BO",1:22),paste0("FAT",1:14))
domains <- c(rep("Anxiety",7),rep("Depression",9),rep("Stress",10),rep("Burnout",22),rep("Fatigue",14))
dlevels <- c("Anxiety","Depression","Stress","Burnout","Fatigue")
dindex <- match(domains,dlevels)

make_precision <- function(icu_network = FALSE) {
  K <- diag(62); dimnames(K) <- list(nodes,nodes)
  add_edge <- function(a,b,w) {K[a,b] <<- K[b,a] <<- -w}
  for(dom in dlevels) {
    ids <- which(domains == dom)
    for(k in seq_len(length(ids)-1L)) add_edge(ids[k],ids[k+1L],if(icu_network).58 else .54)
    if(length(ids)>4L) for(k in seq_len(length(ids)-2L)[seq(1,length(ids)-2L,by=3L)]) add_edge(ids[k],ids[k+2L],.25)
  }
  cross <- list(c("ANX1","DEP2"),c("ANX6","STR3"),c("DEP4","FAT1"),c("DEP7","FAT9"),
                c("STR6","BO8"),c("STR9","FAT4"),c("BO5","FAT6"),c("BO13","DEP8"),
                c("ANX4","BO10"),c("STR2","DEP5"),c("BO19","FAT12"),c("ANX7","FAT14"))
  for(e in cross)add_edge(e[1],e[2],if(icu_network).38 else .28)
  for(e in list(c("ANX2","ANX3"),c("STR5","STR7"),c("BO2","BO3"),c("FAT7","FAT8")))add_edge(e[1],e[2],.68)
  diag(K) <- 0
  diag(K) <- apply(abs(K),1,sum) + .15
  S <- solve(K); D <- diag(1/sqrt(diag(S))); D %*% S %*% D
}
S0 <- make_precision(FALSE); S1 <- make_precision(TRUE)
Z <- matrix(NA_real_,n,62,dimnames=list(NULL,nodes))
Z[icu==0,] <- mvrnorm(sum(icu==0),rep(0,62),S0)
means_icu <- c(rep(.24,7),rep(.28,9),rep(.18,10),rep(.32,22),rep(.25,14))
Z[icu==1,] <- mvrnorm(sum(icu==1),means_icu,S1)

disc <- function(x,cuts) as.integer(cut(x,c(-Inf,cuts,Inf),labels=FALSE))-1L
X <- matrix(NA_integer_,n,62,dimnames=list(NULL,nodes))
for(j in seq_along(nodes)) {
  X[,j] <- if(domains[j] %in% c("Anxiety","Depression")) disc(Z[,j],c(-.55,.25,1.05)) else
    if(domains[j]=="Stress") disc(Z[,j],c(-1,-.35,.30,.95)) else
    if(domains[j]=="Burnout") disc(Z[,j],c(-1.25,-.8,-.35,.10,.55,1.05)) else as.integer(Z[,j]>-.05)
}

pss_rev<-c(4,5,7,8); mbi_rev<-c(4,7,9,12,17,18,19,21); fat_rev<-c(10,13,14)
raw <- list()
for(i in 1:7) raw[[paste0("D_q22_",i)]]<-X[,paste0("ANX",i)]
for(i in 1:9) raw[[paste0("D_q23_",i)]]<-X[,paste0("DEP",i)]
for(i in 1:10) raw[[paste0("D_q24_",i)]]<-if(i%in%pss_rev)4L-X[,paste0("STR",i)] else X[,paste0("STR",i)]
for(i in 1:22) raw[[paste0("F_q3_",i)]]<-if(i%in%mbi_rev)6L-X[,paste0("BO",i)] else X[,paste0("BO",i)]
for(i in 1:14) raw[[paste0("G_q3_",i)]]<-if(i%in%fat_rev)1L-X[,paste0("FAT",i)] else X[,paste0("FAT",i)]
raw <- as.data.table(raw)

age <- pmin(pmax(round(rnorm(n,ifelse(icu==1,34,35),6)),20),60)
tenure <- pmax(0,round(age-23+rnorm(n,0,3)))
sex <- ifelse(runif(n)<ifelse(icu==1,.07,.05),1L,2L)
edu <- sample(1:4,n,TRUE,c(.01,.12,.84,.03))
title <- pmax(1L,pmin(5L,as.integer(cut(tenure,c(-Inf,3,8,16,25,Inf),labels=FALSE))))
admin <- ifelse(runif(n)<pmin(.30,.03+tenure/90),sample(2:5,n,TRUE),1L)
dept <- integer(n); dept[icu==1]<-8L
dept[icu==0] <- sample(c(1:7,9:11),sum(icu==0),TRUE,c(.29,.20,.07,.03,.05,.07,.06,.05,.01,.17))
schedule <- ifelse(runif(n)<ifelse(icu==1,.84,.58),4L,1L)
shift <- ifelse(schedule==1,-3L,sample(1:3,n,TRUE,c(.45,.50,.05)))
night <- ifelse(schedule==1,-3L,sample(1:3,n,TRUE,c(.22,.61,.17)))
bmi <- round(rnorm(n,22.6,3.1),2)
times <- matrix(round(rlnorm(n*4,log(410),.35)),n,4,
                dimnames=list(NULL,c("timetaken.x","timetaken.y","timetaken.x.x","timetaken.y.y")))

d <- data.table(analysis_id=sprintf("SIMV2-%05d",1:n),participant_hash=sprintf("SIMV2HASH-%08d",1:n),
                A_q2=sex,A_age=age,A_BMI=bmi,A_gongzuoshichang=tenure,A_q5=edu,A_q9=dept,
                A_q11=title,A_q12=admin,C_q1=schedule,C_q2=shift,C_q8=night)
d <- cbind(d,as.data.table(times))
d[,D_jiaolv_all:=rowSums(X[,paste0("ANX",1:7),drop=FALSE])]
d[,D_yiyu_all:=rowSums(X[,paste0("DEP",1:9),drop=FALSE])]
d[,D_yali_all:=rowSums(X[,paste0("STR",1:10),drop=FALSE])]
d[,F_qingganshuaijie:=rowSums(X[,paste0("BO",c(1,2,3,6,8,13,14,16,20)),drop=FALSE])]
d[,F_qurengehua:=rowSums(X[,paste0("BO",c(5,10,11,15,22)),drop=FALSE])]
d[,F_gerenchengjiugan:=rowSums(6L-X[,paste0("BO",mbi_rev),drop=FALSE])]
d[,F_zhiyejuandai_all:=F_qingganshuaijie+F_qurengehua+F_gerenchengjiugan]
d[,G_pifa_all:=rowSums(X[,paste0("FAT",1:14),drop=FALSE])]
d[,G_qutipifa:=rowSums(X[,paste0("FAT",1:8),drop=FALSE])]
d[,G_naolipifa:=rowSums(X[,paste0("FAT",9:14),drop=FALSE])]
d <- cbind(d,raw)

# Explicit QC branches. Stored totals remain valid for all complete records.
set.seed(20260714)
d$participant_hash[n_icu] <- d$participant_hash[n_icu-1L]
miss_rows <- c(sample(setdiff(seq_len(n_icu),c(n_icu-1L,n_icu)),3L),sample((n_icu+1L):n,2L)); miss_vars <- sample(names(raw),5)
for(i in seq_along(miss_rows)) set(d,miss_rows[i],miss_vars[i],NA_integer_)
d$A_age[sample(setdiff(1:n,miss_rows),4)] <- c(0,0,99,99)
ten_bad <- sample(setdiff(1:n,miss_rows),4); d$A_gongzuoshichang[ten_bad] <- d$A_age[ten_bad]
d$A_BMI[sample(1:n,12)] <- NA_real_
d$A_BMI[sample(1:n,3)] <- 90
d$A_q9[sample(which(icu==0),1)] <- 99L
d$timetaken.x[1:2] <- -rowSums(d[1:2,.(timetaken.y,timetaken.x.x,timetaken.y.y)])

fwrite(d,data_file)

variables <- c(paste0("D_q22_",1:7),paste0("D_q23_",1:9),paste0("D_q24_",1:10),paste0("F_q3_",1:22),paste0("G_q3_",1:14))
reverse <- c(rep(FALSE,16),1:10%in%pss_rev,1:22%in%mbi_rev,1:14%in%fat_rev)
raw_max <- c(rep(3,16),rep(4,10),rep(6,22),rep(1,14))
dict <- data.table(variable_name=variables,node_id=nodes,domain=domains,
                   reverse_scored_for_network=reverse,source_min=0L,source_max=raw_max,
                   analysis_min=0L,analysis_max=raw_max,
                   transformation=ifelse(reverse,paste0(raw_max," - source value"),"source value"),
                   analysis_direction="higher = greater burden")
fwrite(dict,dictionary_file)
cat("Generated simulation input",nrow(d),"rows and",nrow(dict),"dictionary rows\n")
