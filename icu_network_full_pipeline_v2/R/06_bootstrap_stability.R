#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(bootnet);library(parallel);library(qgraph);library(networktools)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<6L)stop("Usage: 06_bootstrap_stability.R <PSM_A.csv> <PSM_B.csv> <out> <nBoots> <cores> <log>")
fileA<-args[1];fileB<-args[2];out<-args[3];nboot<-as.integer(args[4]);cores<-as.integer(args[5]);log_file<-args[6]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START bootstrap stability nBoots=",nboot)
if(nboot<1000L)stop("Bootstrap samples must be at least 1000")

aggregate_edge<-function(obj,label,model){
  b<-as.data.table(obj$bootTable)[type=="edge"]
  ss<-b[,.(bootstrap_mean=mean(value),bootstrap_sd=sd(value),ci95_low=quantile(value,.025),ci95_high=quantile(value,.975),proportion_nonzero=mean(value!=0)),by=.(node1,node2,id)]
  sm<-as.data.table(obj$sampleTable)[type=="edge",.(node1,node2,id,sample_weight=value)]
  z<-merge(sm,ss,by=c("node1","node2","id"),all.x=TRUE);z[,`:=`(model=model,network=label,bootstrap_samples=nboot,ci_excludes_zero=ci95_low>0|ci95_high<0)];z
}
run_bootnet<-function(dat,model,group,edge=TRUE){
  label<-paste(model,group);folder<-file.path(out,model,group);dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  assert_group_matrix(dat,NODE_IDS,label)
  if(edge){
    set.seed(202607130L+sum(utf8ToInt(label)))
    eb<-bootnet(as.data.frame(dat[,..NODE_IDS]),nBoots=nboot,default="EBICglasso",type="nonparametric",nCores=cores,
      statistics="edge",model="GGM",verbose=FALSE,labels=NODE_IDS,memorysaver=TRUE,corMethod="spearman",tuning=.5)
    saveRDS(eb,file.path(folder,paste0("edge_accuracy_bootstrap_",nboot,".rds")))
    fwrite(aggregate_edge(eb,group,model),file.path(folder,paste0("edge_bootstrap_ci_",nboot,".csv")))
  }
  set.seed(202607230L+sum(utf8ToInt(label)))
  cb<-bootnet(as.data.frame(dat[,..NODE_IDS]),nBoots=nboot,default="EBICglasso",type="case",nCores=cores,
    statistics=c("strength","expectedInfluence"),model="GGM",verbose=FALSE,labels=NODE_IDS,caseMin=.05,caseMax=.75,caseN=10,
    memorysaver=TRUE,corMethod="spearman",tuning=.5)
  saveRDS(cb,file.path(folder,paste0("case_dropping_bootstrap_",nboot,".rds")))
  cs<-corStability(cb,statistics=c("strength","expectedInfluence"),verbose=FALSE)
  csdt<-data.table(model=model,network=group,statistic=names(cs),cs_coefficient=as.numeric(cs),bootstrap_samples=nboot,
                   maximum_tested_drop_proportion=.75,reached_maximum_tested=abs(as.numeric(cs)-.75)<.011)
  fwrite(csdt,file.path(folder,paste0("case_dropping_CS_coefficients_",nboot,".csv")))
  csdt
}

A<-fread(fileA);B<-fread(fileB);assert_scored_input(A,"PSM-A bootstrap input");assert_scored_input(B,"PSM-B bootstrap input")
cs_all<-rbindlist(list(run_bootnet(A[icu==1L],"PSM_A","ICU",TRUE),run_bootnet(A[icu==0L],"PSM_A","Comparator",TRUE),
                          run_bootnet(B[icu==1L],"PSM_B","ICU",FALSE),run_bootnet(B[icu==0L],"PSM_B","Comparator",FALSE)))
fwrite(cs_all,file.path(out,"all_standard_centrality_CS_coefficients.csv"))

proportions<-c(.10,.20,.30,.40,.50,.60,.70,.75);reps<-100L
custom_group<-function(dat,model,group){
  folder<-file.path(out,model,group);orig<-estimate_network(as.data.frame(dat),NODE_IDS,"spearman",.5,FALSE)
  oct<-centrality_table(orig$weights,group,model)[match(NODE_IDS,node_id)]
  jobs<-CJ(drop_proportion=proportions,replicate=seq_len(reps));jobs[,seed:=2026071300L+.I+100000L*(model=="PSM_B")+10000L*(group=="Comparator")]
  worker<-function(i){set.seed(jobs$seed[i]);keep<-sample.int(nrow(dat),max(20L,floor(nrow(dat)*(1-jobs$drop_proportion[i]))),replace=FALSE)
    fit<-estimate_network(as.data.frame(dat[keep]),NODE_IDS,"spearman",.5,FALSE);ct<-centrality_table(fit$weights,group,model)[match(NODE_IDS,node_id)]
    data.table(drop_proportion=jobs$drop_proportion[i],replicate=jobs$replicate[i],seed=jobs$seed[i],retained_n=length(keep),
      bridge_strength=rank_correlation(oct$bridge_strength,ct$bridge_strength),
      bridge_expected_influence=rank_correlation(oct$bridge_expected_influence,ct$bridge_expected_influence),
      normalized_bridge_expected_influence=rank_correlation(oct$normalized_bridge_expected_influence,ct$normalized_bridge_expected_influence))}
  cl<-makePSOCKcluster(min(cores,nrow(jobs)));on.exit(try(stopCluster(cl),silent=TRUE),add=TRUE)
  clusterEvalQ(cl,{suppressPackageStartupMessages({library(data.table);library(qgraph);library(networktools);library(Matrix)});NULL})
  clusterExport(cl,c("jobs","dat","model","group","oct","worker","NODE_IDS","COMMUNITIES","NODE_DOMAINS","estimate_network",
    "centrality_table","rank_correlation","safe_pd"),envir=environment())
  raw<-rbindlist(parLapply(cl,seq_len(nrow(jobs)),worker));stopCluster(cl)
  fwrite(raw,file.path(folder,"custom_bridge_case_dropping_raw.csv"))
  metrics<-c("bridge_strength","bridge_expected_influence","normalized_bridge_expected_influence")
  sm<-rbindlist(lapply(metrics,function(v)raw[,.(model=model,network=group,metric=v,n_repetitions=.N,mean_correlation=mean(get(v),na.rm=TRUE),
    median_correlation=median(get(v),na.rm=TRUE),q05_correlation=quantile(get(v),.05,na.rm=TRUE),
    proportion_at_least_0_70=mean(get(v)>=.70,na.rm=TRUE)),by=drop_proportion]))
  sm[,passes_CS_rule:=q05_correlation>=.70&proportion_at_least_0_70>=.95]
  cs<-sm[passes_CS_rule==TRUE,.(cs_coefficient=max(drop_proportion)),by=.(model,network,metric)]
  allkeys<-CJ(model=model,network=group,metric=metrics,unique=TRUE);cs<-merge(allkeys,cs,by=c("model","network","metric"),all.x=TRUE);cs[is.na(cs_coefficient),cs_coefficient:=0]
  cs[,`:=`(correlation_threshold=.70,required_success_proportion=.95,repetitions_per_drop=reps,maximum_tested_drop_proportion=.75)]
  fwrite(sm,file.path(folder,"custom_bridge_case_dropping_summary.csv"));fwrite(cs,file.path(folder,"custom_bridge_CS_coefficients.csv"));cs
}
custom_cs<-rbindlist(list(custom_group(A[icu==1L],"PSM_A","ICU"),custom_group(A[icu==0L],"PSM_A","Comparator"),
                            custom_group(B[icu==1L],"PSM_B","ICU"),custom_group(B[icu==0L],"PSM_B","Comparator")))
fwrite(custom_cs,file.path(out,"all_custom_bridge_CS_coefficients.csv"))
append_log(log_file,"PASS bootstrap edge accuracy PSM-A 1000 x2; standard case PSM-A/PSM-B 1000 x4; custom bridge case-dropping completed")
