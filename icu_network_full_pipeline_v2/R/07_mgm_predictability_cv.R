#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(mgm);library(parallel)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<5L)stop("Usage: 07_mgm_predictability_cv.R <PSM_A.csv> <out> <folds> <seed> <log>")
input<-args[1];out<-args[2];folds<-as.integer(args[3]);seed<-as.integer(args[4]);log_file<-args[5]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START mixed categorical mgm",folds,"fold CV")
if(folds<5L)stop("Predictability requires at least 5-fold CV")
d<-fread(input);assert_scored_input(d,"mgm CV input")

labels<-c("ICU PSM-A","Matched non-ICU PSM-A")
group_data<-list(as.matrix(d[icu==1L,..NODE_IDS]),as.matrix(d[icu==0L,..NODE_IDS]));lapply(group_data,function(x)storage.mode(x)<-"numeric")
group_hash<-list(d[icu==1L]$participant_hash,d[icu==0L]$participant_hash)
fold_ids<-vector("list",2)
for(g in 1:2){set.seed(seed+10000L*(g-1L));fold_ids[[g]]<-sample(rep(seq_len(folds),length.out=nrow(group_data[[g]])))
  folder<-file.path(out,gsub("[^A-Za-z0-9]+","_",labels[g]));dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  fwrite(data.table(participant_hash=group_hash[[g]],fold=fold_ids[[g]]),file.path(folder,"cross_validation_fold_assignment.csv"))}
jobs<-CJ(group_index=1:2,fold=seq_len(folds))
fit_one<-function(i){
  g<-jobs$group_index[i];k<-jobs$fold[i];x<-group_data[[g]];fid<-fold_ids[[g]];label<-labels[g]
  train<-x[fid!=k,,drop=FALSE];test<-x[fid==k,,drop=FALSE];lev<-vapply(seq_len(ncol(train)),function(j)as.integer(max(x[,j])+1L),integer(1))
  set.seed(seed+10000L*(g-1L)+100L*k)
  fit<-mgm(data=train,type=rep("c",ncol(train)),level=lev,k=2,lambdaSel="EBIC",lambdaGam=.25,ruleReg="AND",
           pbar=FALSE,warnings=FALSE,saveModels=TRUE,saveData=FALSE)
  pred<-predict(fit,data=test,errorCat=c("CC","nCC"),pbar=FALSE);er<-as.data.table(pred$errors);setnames(er,"Variable","node_id")
  er[,`:=`(network=label,fold=k,n_train=nrow(train),n_test=nrow(test),
    metric_interpretation="CC=classification accuracy; nCC=accuracy normalized against marginal-mode baseline; negative nCC=worse than baseline",
    model="Mixed categorical mgm, pairwise interactions, EBIC gamma 0.25, 5-fold cross-validation")]
  folder<-file.path(out,gsub("[^A-Za-z0-9]+","_",label));saveRDS(list(fit=fit,prediction=pred),file.path(folder,sprintf("fold_%02d_model_prediction.rds",k)),compress=TRUE);er
}
mgm_cores<-max(1L,min(as.integer(Sys.getenv("ICU_PIPELINE_CORES","3")),nrow(jobs)))
cl<-makePSOCKcluster(mgm_cores);on.exit(try(stopCluster(cl),silent=TRUE),add=TRUE)
clusterEvalQ(cl,{suppressPackageStartupMessages({library(data.table);library(mgm)});NULL})
clusterExport(cl,c("jobs","group_data","fold_ids","labels","seed","out","fit_one"),envir=environment())
raw_all<-rbindlist(parLapply(cl,seq_len(nrow(jobs)),fit_one),fill=TRUE);stopCluster(cl)
summary<-raw_all[,.(CC_mean=mean(CC,na.rm=TRUE),CC_SD=sd(CC,na.rm=TRUE),CC_q2_5=quantile(CC,.025,na.rm=TRUE),CC_q97_5=quantile(CC,.975,na.rm=TRUE),
  nCC_mean=mean(nCC,na.rm=TRUE),nCC_SD=sd(nCC,na.rm=TRUE),nCC_q2_5=quantile(nCC,.025,na.rm=TRUE),nCC_q97_5=quantile(nCC,.975,na.rm=TRUE),
  negative_nCC_count=sum(nCC<0,na.rm=TRUE),folds_completed=.N),by=.(network,node_id)]
for(label in labels){folder<-file.path(out,gsub("[^A-Za-z0-9]+","_",label));fwrite(raw_all[network==label],file.path(folder,"fold_level_CC_nCC.csv"));
  fwrite(summary[network==label],file.path(folder,"node_predictability_5fold_summary.csv"))}
fwrite(summary,file.path(out,"mgm_predictability_5fold_all_nodes.csv"))
audit<-summary[,.(nodes=.N,all_nodes_have_5_folds=all(folds_completed==folds),nodes_with_negative_nCC=sum(negative_nCC_count>0),
  median_CC=median(CC_mean),median_nCC=median(nCC_mean)),by=network]
fwrite(audit,file.path(out,"mgm_predictability_CV_audit.csv"))
if(any(audit$nodes!=62L)|!all(audit$all_nodes_have_5_folds))stop("mgm cross-validation completeness failure")
append_log(log_file,"PASS mgm 5-fold CV; 62 nodes x 2 groups; negative nCC node counts=",paste(audit$nodes_with_negative_nCC,collapse=","))
