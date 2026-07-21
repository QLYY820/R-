#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(qgraph);library(networktools)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<4L)stop("Usage: 03_network_models.R <scored.csv> <psm_dir> <output_dir> <log>")
scored_file<-args[1];psm_dir<-args[2];out<-args[3];log_file<-args[4]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START network models")
d<-fread(scored_file);eligible<-assert_scored_input(d,"network source")

read_pair<-function(sub){
  x<-fread(file.path(psm_dir,sub,"matched_data.csv"));assert_scored_input(x,paste(sub,"matched data"))
  list(ICU=x[icu==1L],Comparator=x[icu==0L])
}
pA<-read_pair("PSM_A");pB<-read_pair("PSM_B");pO<-read_pair("PSM_A_ordinary")
unmatched<-list(ICU=eligible[A_q9==8L],Comparator=eligible[A_q9!=8L])
duration_cut<-quantile(eligible$total_response_seconds,.01,na.rm=TRUE)
qc<-eligible[!flag_age_implausible&!flag_tenure_implausible&!flag_age_tenure_inconsistent&!flag_bmi_implausible&
             !flag_duration_nonpositive&total_response_seconds>=duration_cut]
qc_pair<-list(ICU=qc[A_q9==8L],Comparator=qc[A_q9!=8L])

pooled_matched<-rbindlist(pA)
pooled_cor<-cor(as.data.frame(pooled_matched[,..NODE_IDS]),method="spearman")
redundant<-character();remaining<-NODE_IDS
repeat{
  A<-abs(pooled_cor[remaining,remaining,drop=FALSE]);diag(A)<-0;mx<-max(A)
  if(!is.finite(mx)||mx<.80)break
  pair<-which(A==mx,arr.ind=TRUE)[1,];remove<-remaining[max(pair)];redundant<-c(redundant,remove);remaining<-setdiff(remaining,remove)
}
if(length(remaining)==62L){
  A<-abs(pooled_cor);diag(A)<-0;ord<-order(apply(A,1,max),decreasing=TRUE);redundant<-NODE_IDS[ord[1:4]];remaining<-setdiff(NODE_IDS,redundant)
}
fwrite(data.table(removed_node=redundant,reason="Greedy redundancy reduction using pooled absolute Spearman association threshold 0.80; minimum four removed for sensitivity contrast"),file.path(out,"redundancy_removed_nodes.csv"))

specs<-list(
  PSM_A_full62=list(pair=pA,nodes=NODE_IDS,cor="spearman",gamma=.5,threshold=FALSE),
  PSM_B_full62=list(pair=pB,nodes=NODE_IDS,cor="spearman",gamma=.5,threshold=FALSE),
  Ordinary_PSM_A_full62=list(pair=pO,nodes=NODE_IDS,cor="spearman",gamma=.5,threshold=FALSE),
  Unmatched_eligible_full62=list(pair=unmatched,nodes=NODE_IDS,cor="spearman",gamma=.5,threshold=FALSE),
  PSM_A_cor_auto_full62=list(pair=pA,nodes=NODE_IDS,cor="cor_auto",gamma=.5,threshold=FALSE),
  PSM_A_redundancy_reduced=list(pair=pA,nodes=remaining,cor="spearman",gamma=.5,threshold=FALSE),
  PSM_A_legacy22=list(pair=pA,nodes=LEGACY22,cor="spearman",gamma=.5,threshold=FALSE),
  PSM_A_gamma025=list(pair=pA,nodes=NODE_IDS,cor="spearman",gamma=.25,threshold=FALSE),
  PSM_A_gamma075=list(pair=pA,nodes=NODE_IDS,cor="spearman",gamma=.75,threshold=FALSE),
  PSM_A_threshold_TRUE=list(pair=pA,nodes=NODE_IDS,cor="spearman",gamma=.5,threshold=TRUE),
  QC_locked_full62=list(pair=qc_pair,nodes=NODE_IDS,cor="spearman",gamma=.5,threshold=FALSE)
)

models<-list();summaries<-list();centrals<-list();edges<-list()
for(nm in names(specs)){
  s<-specs[[nm]];folder<-file.path(out,nm);dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  models[[nm]]<-list()
  for(g in c("ICU","Comparator")){
    z<-s$pair[[g]];assert_group_matrix(z,s$nodes,paste(nm,g))
    fit<-estimate_network(as.data.frame(z),s$nodes,s$cor,s$gamma,s$threshold);models[[nm]][[g]]<-fit
    write_matrix(fit$correlation,file.path(folder,paste0(g,"_correlation_matrix.csv")))
    write_matrix(fit$weights,file.path(folder,paste0(g,"_network_weight_matrix.csv")))
    el<-edge_long(fit$weights,g,nm);ct<-centrality_table(fit$weights,g,nm);sm<-network_summary(fit$weights,g,nm,nrow(z))
    fwrite(el,file.path(folder,paste0(g,"_edge_long_",nrow(el),".csv")));fwrite(ct,file.path(folder,paste0(g,"_centrality_bridge.csv")))
    summaries[[paste(nm,g)]]<-sm;centrals[[paste(nm,g)]]<-ct;edges[[paste(nm,g)]]<-el
  }
  saveRDS(models[[nm]],file.path(folder,"network_models.rds"))
}
saveRDS(models,file.path(out,"all_network_models.rds"))
summary_dt<-rbindlist(summaries);central_dt<-rbindlist(centrals);edge_dt<-rbindlist(edges)
fwrite(summary_dt,file.path(out,"all_network_summary.csv"));fwrite(central_dt,file.path(out,"all_network_centrality_bridge.csv"));fwrite(edge_dt,file.path(out,"all_network_edges_long.csv"))

ref<-models$PSM_A_full62
comparison<-rbindlist(lapply(names(models),function(nm)rbindlist(lapply(c("ICU","Comparator"),function(g){
  m<-models[[nm]][[g]];nodes<-intersect(m$nodes,ref[[g]]$nodes);ix<-upper.tri(m$weights[nodes,nodes]);rw<-ref[[g]]$weights[nodes,nodes]
  ct<-central_dt[analysis==nm&network==g];rc<-central_dt[analysis=="PSM_A_full62"&network==g][match(nodes,node_id)]
  ct<-ct[match(nodes,node_id)]
  data.table(analysis=nm,network=g,n_nodes=length(nodes),n=nrow(specs[[nm]]$pair[[g]]),global_strength=sum(abs(m$weights[upper.tri(m$weights)])),
    nonzero_edges=sum(m$weights[upper.tri(m$weights)]!=0),edge_weight_correlation_with_PSM_A=rank_correlation(m$weights[nodes,nodes][ix],rw[ix]),
    strength_rank_correlation=rank_correlation(ct$strength,rc$strength),EI_rank_correlation=rank_correlation(ct$expected_influence,rc$expected_influence),
    bridge_EI_rank_correlation=rank_correlation(ct$bridge_expected_influence,rc$bridge_expected_influence),
    top5_strength_overlap=length(intersect(top_nodes(setNames(ct$strength,nodes)),top_nodes(setNames(rc$strength,nodes)))),
    top5_EI_overlap=length(intersect(top_nodes(setNames(ct$expected_influence,nodes)),top_nodes(setNames(rc$expected_influence,nodes)))),
    top5_bridge_EI_overlap=length(intersect(top_nodes(setNames(ct$bridge_expected_influence,nodes)),top_nodes(setNames(rc$bridge_expected_influence,nodes))))
  )
}))))
fwrite(comparison,file.path(out,"network_sensitivity_comparison_matrix.csv"))

primary_summary<-summary_dt[analysis=="PSM_A_full62"]
if(any(primary_summary$n_nodes!=62L)||any(primary_summary$possible_edges!=1891L))stop("Primary full62 network dimension/edge count failure")
append_log(log_file,"PASS 11 network specifications; primary edge rows per group=1891; redundancy nodes=",length(remaining))
