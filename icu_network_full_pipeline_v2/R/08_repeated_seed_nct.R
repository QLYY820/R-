#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(NetworkComparisonTest);library(qgraph);library(Matrix);library(parallel)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<5L)stop("Usage: 08_repeated_seed_nct.R <downsampling_dir> <out> <permutations> <cores> <log>")
source_dir<-args[1];out<-args[2];iterations<-as.integer(args[3]);cores<-as.integer(args[4]);log_file<-args[5]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START repeated-seed NCT 20 x 2; permutations=",iterations,"paired=FALSE")
if(iterations<1000L)stop("Repeated-seed NCT requires at least 1000 permutations")
scenarios<-c("all_eligible_nonICU_pool","ordinary_inpatient_pool")
files<-unlist(lapply(scenarios,function(s)list.files(file.path(source_dir,s,"selected_seed_data"),pattern="\\.csv$",full.names=TRUE)))
if(length(files)!=40L)stop("Expected exactly 40 selected-seed datasets, found ",length(files))

estimator_fun<-function(dat,...){
  dat<-as.data.frame(dat);S<-cor(dat,method="spearman",use="complete.obs")
  if(!all(eigen(S,symmetric=TRUE,only.values=TRUE)$values>1e-8))S<-as.matrix(Matrix::nearPD(S,corr=TRUE,keepDiag=TRUE)$mat)
  W<-qgraph::EBICglasso(S,n=nrow(dat),gamma=.5,penalize.diagonal=FALSE,checkPD=TRUE,verbose=FALSE);dimnames(W)<-list(colnames(dat),colnames(dat));W
}
worker<-function(file){
  scenario<-basename(dirname(dirname(file)));tag<-tools::file_path_sans_ext(basename(file));parts<-strsplit(tag,"_",fixed=TRUE)[[1]]
  iter<-as.integer(parts[2]);seed<-as.integer(parts[4]);z<-fread(file);x1<-as.data.frame(z[icu==1L,..NODE_IDS]);x2<-as.data.frame(z[icu==0L,..NODE_IDS])
  if(nrow(x1)!=nrow(x2)||nrow(x1)==0L)stop("Selected-seed group size mismatch: ",file)
  start<-Sys.time();set.seed(seed+700000L)
  nct<-suppressWarnings(NetworkComparisonTest::NCT(x1,x2,gamma=.5,it=iterations,binary.data=FALSE,paired=FALSE,weighted=TRUE,
    AND=TRUE,abs=TRUE,test.edges=FALSE,progressbar=FALSE,make.positive.definite=TRUE,test.centrality=FALSE,
    estimator=estimator_fun,estimatorArgs=list(),verbose=FALSE))
  folder<-file.path(out,scenario,tag);dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  dist<-data.table(permutation=seq_len(iterations),global_strength_statistic=nct$glstrinv.perm,network_structure_statistic=nct$nwinv.perm)
  fwrite(dist,file.path(folder,"permutation_distributions.csv"))
  meta<-data.table(scenario=scenario,iteration=iter,seed=seed,n_ICU=nrow(x1),n_comparator=nrow(x2),paired=FALSE,permutations=iterations,
    global_strength_statistic=as.numeric(nct$glstrinv.real),global_strength_p=as.numeric(nct$glstrinv.pval),
    network_structure_statistic=as.numeric(nct$nwinv.real),network_structure_p=as.numeric(nct$nwinv.pval),
    runtime_seconds=as.numeric(difftime(Sys.time(),start,units="secs")))
  fwrite(meta,file.path(folder,"nct_summary.csv"));meta
}
cl<-makePSOCKcluster(min(cores,length(files)));on.exit(try(stopCluster(cl),silent=TRUE),add=TRUE)
clusterEvalQ(cl,{suppressPackageStartupMessages({library(data.table);library(NetworkComparisonTest);library(qgraph);library(Matrix)});NULL})
clusterExport(cl,c("files","out","iterations","NODE_IDS","worker","estimator_fun"),envir=environment())
results<-rbindlist(parLapply(cl,files,worker));stopCluster(cl)
fwrite(results,file.path(out,"all_40_repeated_seed_NCT_results.csv"))
agg<-results[,.(seeds_completed=.N,network_structure_p_lt_05_proportion=mean(network_structure_p<.05),
  global_strength_p_lt_05_proportion=mean(global_strength_p<.05),
  network_structure_statistic_mean=mean(network_structure_statistic),network_structure_statistic_SD=sd(network_structure_statistic),
  global_strength_statistic_mean=mean(global_strength_statistic),global_strength_statistic_SD=sd(global_strength_statistic),
  network_structure_p_median=median(network_structure_p),global_strength_p_median=median(global_strength_p)),by=scenario]
fwrite(agg,file.path(out,"repeated_seed_NCT_scenario_summary.csv"))
if(any(agg$seeds_completed!=20L))stop("Repeated-seed NCT completeness failure")
append_log(log_file,"PASS repeated-seed NCT 20 x 2, paired=FALSE, 1000 permutations each")
