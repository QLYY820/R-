#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(parallel);library(qgraph);library(networktools)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<6L)stop("Usage: 05_repeated_downsampling.R <scored.csv> <PSM_A_matched.csv> <output_dir> <iterations> <cores> <log>")
scored_file<-args[1];psm_file<-args[2];out<-args[3];iterations<-as.integer(args[4]);cores<-as.integer(args[5]);log_file<-args[6]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START repeated downsampling iterations=",iterations,"scenarios=2")
if(iterations!=200L)stop("V2 requires exactly 200 downsampling iterations")
d<-fread(scored_file);eligible<-assert_scored_input(d,"downsampling source");eligible[,icu:=as.integer(A_q9==8L)]
icu<-eligible[icu==1L];all_pool<-eligible[icu==0L];ordinary_pool<-eligible[icu==0L&A_q9%in%c(1L,2L,4L,5L,6L)]
assert_group_matrix(icu,NODE_IDS,"eligible ICU downsampling fixed group",expected_n=sum(eligible$A_q9==8L))
if(nrow(all_pool)<nrow(icu)||nrow(ordinary_pool)<nrow(icu))stop("Comparator pool smaller than eligible ICU sample")
psm<-fread(psm_file);ref_comp<-estimate_network(as.data.frame(psm[icu==0L]),NODE_IDS,"spearman",.5,FALSE)
ref_icu<-estimate_network(as.data.frame(psm[icu==1L]),NODE_IDS,"spearman",.5,FALSE)
icu_fit<-estimate_network(as.data.frame(icu),NODE_IDS,"spearman",.5,FALSE);icu_ct<-centrality_table(icu_fit$weights,"ICU","eligible_ICU")
ref_comp_ct<-centrality_table(ref_comp$weights,"Comparator","ref")
fixed_iterations<-c(1L,11L,21L,31L,41L,51L,61L,71L,81L,91L,101L,111L,121L,131L,141L,151L,161L,171L,181L,191L)

run_scenario<-function(pool,label,offset){
  folder<-file.path(out,label);dir.create(folder,recursive=TRUE,showWarnings=FALSE);dir.create(file.path(folder,"selected_seed_data"),recursive=TRUE,showWarnings=FALSE)
  seeds<-2026071300L+offset+seq_len(iterations)
  worker<-function(i){
    set.seed(seeds[i]);idx<-sample.int(nrow(pool),nrow(icu),replace=FALSE);z<-pool[idx];fit<-estimate_network(as.data.frame(z),NODE_IDS,"spearman",.5,FALSE)
    ct<-centrality_table(fit$weights,"Comparator",paste0(label,"_",i));v<-fit$weights[upper.tri(fit$weights)]
    list(i=i,seed=seeds[i],ids=z$participant_hash,W=fit$weights,
      summary=network_summary(fit$weights,"Comparator",label,nrow(z)),centrality=ct,
      top=data.table(iteration=i,seed=seeds[i],metric=c(rep("strength",5),rep("expected_influence",5),rep("bridge_expected_influence",5)),
        rank=rep(1:5,3),node_id=c(top_nodes(setNames(ct$strength,ct$node_id)),top_nodes(setNames(ct$expected_influence,ct$node_id)),top_nodes(setNames(ct$bridge_expected_influence,ct$node_id)))))
  }
  cl<-makePSOCKcluster(min(cores,iterations));on.exit(try(stopCluster(cl),silent=TRUE),add=TRUE)
  clusterEvalQ(cl,{suppressPackageStartupMessages({library(data.table);library(qgraph);library(networktools);library(Matrix)});NULL})
  clusterExport(cl,c("pool","label","seeds","icu","worker","NODE_IDS","COMMUNITIES","NODE_DOMAINS",
    "estimate_network","centrality_table","network_summary","top_nodes","safe_pd"),envir=environment())
  ans<-parLapply(cl,seq_len(iterations),worker);stopCluster(cl)
  if(length(ans)!=iterations||any(vapply(ans,is.null,logical(1))))stop(label,": incomplete downsampling iterations")
  ix<-which(upper.tri(icu_fit$weights),arr.ind=TRUE);edge_names<-data.table(node1=rownames(icu_fit$weights)[ix[,1]],node2=colnames(icu_fit$weights)[ix[,2]])
  edge_dt<-rbindlist(lapply(ans,function(a)cbind(data.table(iteration=a$i,seed=a$seed),edge_names,
    weight=a$W[ix],ICU_minus_comparator=icu_fit$weights[ix]-a$W[ix])))
  cen_dt<-rbindlist(lapply(ans,function(a){x<-copy(a$centrality);x[,`:=`(iteration=a$i,seed=a$seed)];x}))
  sum_dt<-rbindlist(lapply(ans,function(a){x<-copy(a$summary);x[,`:=`(iteration=a$i,seed=a$seed)];x}))
  top_dt<-rbindlist(lapply(ans,`[[`,"top"))
  fwrite(edge_dt,file.path(folder,"all_iteration_edge_weights.csv"));fwrite(cen_dt,file.path(folder,"all_iteration_centrality_bridge.csv"))
  fwrite(sum_dt,file.path(folder,"all_iteration_network_summary.csv"));fwrite(top_dt,file.path(folder,"all_iteration_top5_nodes.csv"))
  fwrite(data.table(iteration=seq_len(iterations),seed=seeds),file.path(folder,"iteration_seeds.csv"))
  for(i in fixed_iterations){z<-pool[participant_hash%in%ans[[i]]$ids];combo<-rbindlist(list(icu,z),fill=TRUE);combo[,icu:=as.integer(A_q9==8L)]
    fwrite(combo,file.path(folder,"selected_seed_data",sprintf("iteration_%03d_seed_%d.csv",i,seeds[i])))}
  metric_vars<-c("global_strength","nonzero_edges","positive_edges","negative_edges","density")
  metric_summary<-rbindlist(lapply(metric_vars,function(v)data.table(metric=v,mean=mean(sum_dt[[v]]),median=median(sum_dt[[v]]),sd=sd(sum_dt[[v]]),
    q2_5=quantile(sum_dt[[v]],.025),q97_5=quantile(sum_dt[[v]],.975))))
  fwrite(metric_summary,file.path(folder,"network_metric_distribution_summary.csv"))
  top_freq<-top_dt[,.(top5_count=.N,top5_frequency=.N/iterations),by=.(metric,node_id)][order(metric,-top5_frequency)]
  fwrite(top_freq,file.path(folder,"top5_appearance_frequency.csv"))
  edge_freq<-edge_dt[,.(retention_frequency=mean(weight!=0),mean_weight=mean(weight),median_weight=median(weight),sd_weight=sd(weight),q2_5=quantile(weight,.025),q97_5=quantile(weight,.975)),by=.(node1,node2)]
  fwrite(edge_freq,file.path(folder,"edge_retention_frequency_summary.csv"))
  ref_ix<-upper.tri(ref_comp$weights);ref_direction<-sign(ref_icu$weights[ref_ix]-ref_comp$weights[ref_ix])
  iter_compare<-rbindlist(lapply(ans,function(a){ct<-a$centrality[match(NODE_IDS,node_id)];
    data.table(iteration=a$i,seed=a$seed,edge_weight_correlation_with_PSM_A=rank_correlation(a$W[ref_ix],ref_comp$weights[ref_ix]),
      strength_rank_correlation_with_PSM_A=rank_correlation(ct$strength,ref_comp_ct$strength),
      EI_rank_correlation_with_PSM_A=rank_correlation(ct$expected_influence,ref_comp_ct$expected_influence),
      bridge_EI_rank_correlation_with_PSM_A=rank_correlation(ct$bridge_expected_influence,ref_comp_ct$bridge_expected_influence),
      edge_difference_direction_consistency=mean(sign(icu_fit$weights[ref_ix]-a$W[ref_ix])==ref_direction))}))
  fwrite(iter_compare,file.path(folder,"iteration_comparison_with_PSM_A.csv"))
  data.table(scenario=label,eligible_ICU_n=nrow(icu),eligible_pool_n=nrow(pool),iterations=length(ans),selected_seed_NCT_datasets=length(fixed_iterations),
    mean_direction_consistency=mean(iter_compare$edge_difference_direction_consistency),mean_edge_rank_correlation=mean(iter_compare$edge_weight_correlation_with_PSM_A))
}

scenario_summary<-rbindlist(list(run_scenario(all_pool,"all_eligible_nonICU_pool",0L),run_scenario(ordinary_pool,"ordinary_inpatient_pool",100000L)))
fwrite(scenario_summary,file.path(out,"repeated_downsampling_scenario_summary.csv"))
if(any(scenario_summary$iterations!=200L)||any(scenario_summary$selected_seed_NCT_datasets!=20L))stop("Repeated downsampling completeness failure")
append_log(log_file,"PASS repeated downsampling 200 x 2; eligible ICU n=",nrow(icu))
