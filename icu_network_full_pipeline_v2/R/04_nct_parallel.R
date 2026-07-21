#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(qgraph);library(NetworkComparisonTest);library(Matrix);library(parallel)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<11L)stop("Usage: 04_nct_parallel.R <data.csv> <out> <analysis> <nodes_csv|ALL> <correlation> <gamma> <permutations> <cores> <seed> <save_permutations TRUE/FALSE> <log>")
data_file<-args[1];out<-args[2];analysis<-args[3];node_arg<-args[4];cor_method<-args[5];gamma<-as.numeric(args[6]);iterations<-as.integer(args[7]);cores<-as.integer(args[8]);base_seed<-as.integer(args[9]);save_perm<-as.logical(args[10]);log_file<-args[11]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
start<-Sys.time();append_log(log_file,"START NCT",analysis,"permutations=",iterations,"paired=FALSE")
if(iterations<1000L)stop("NCT permutations must be at least 1000")
nodes<-if(node_arg=="ALL")NODE_IDS else strsplit(node_arg,",",fixed=TRUE)[[1]]
d<-fread(data_file);if(!"icu"%in%names(d))d[,icu:=as.integer(A_q9==8L)]
assert_group_matrix(d[icu==1L],nodes,paste(analysis,"ICU"));assert_group_matrix(d[icu==0L],nodes,paste(analysis,"comparator"))
x1<-as.data.frame(d[icu==1L,..nodes]);x2<-as.data.frame(d[icu==0L,..nodes])

estimator_fun<-function(dat,gamma=.5,cor_method="spearman",nodes=NULL,...){
  dat<-as.data.frame(dat);if(!is.null(nodes))colnames(dat)<-nodes
  if(cor_method=="spearman")S<-cor(dat,method="spearman",use="complete.obs") else
    S<-qgraph::cor_auto(dat,detectOrdinal=TRUE,ordinalLevelMax=7,forcePD=TRUE,missing="listwise",verbose=FALSE)
  if(!all(eigen(S,symmetric=TRUE,only.values=TRUE)$values>1e-8))S<-as.matrix(Matrix::nearPD(S,corr=TRUE,keepDiag=TRUE)$mat)
  W<-qgraph::EBICglasso(S,n=nrow(dat),gamma=gamma,penalize.diagonal=FALSE,checkPD=TRUE,verbose=FALSE)
  dimnames(W)<-list(colnames(dat),colnames(dat));W
}

cores<-min(max(1L,cores),iterations);sizes<-rep(iterations%/%cores,cores);if(iterations%%cores)sizes[seq_len(iterations%%cores)]<-sizes[seq_len(iterations%%cores)]+1L
cl<-makePSOCKcluster(length(sizes));on.exit(try(stopCluster(cl),silent=TRUE),add=TRUE)
clusterEvalQ(cl,{suppressPackageStartupMessages({library(qgraph);library(NetworkComparisonTest);library(Matrix)});NULL})
clusterExport(cl,c("x1","x2","nodes","gamma","cor_method","estimator_fun","sizes","base_seed"),envir=environment())
chunks<-parLapply(cl,seq_along(sizes),function(k){
  set.seed(base_seed+10000L*k)
  suppressWarnings(NetworkComparisonTest::NCT(data1=x1,data2=x2,gamma=gamma,it=sizes[k],binary.data=FALSE,
    paired=FALSE,weighted=TRUE,AND=TRUE,abs=TRUE,test.edges=TRUE,edges="all",progressbar=FALSE,
    make.positive.definite=TRUE,p.adjust.methods="none",test.centrality=TRUE,
    centrality=c("strength","expectedInfluence"),nodes="all",estimator=estimator_fun,
    estimatorArgs=list(gamma=gamma,cor_method=cor_method,nodes=nodes),verbose=FALSE))
})
stopCluster(cl)
ref<-chunks[[1]];gl_perm<-unlist(lapply(chunks,`[[`,"glstrinv.perm"));nw_perm<-unlist(lapply(chunks,`[[`,"nwinv.perm"))
p_perm<-function(real,perm)(sum(perm>=real,na.rm=TRUE)+1)/(sum(!is.na(perm))+1)
global_p<-p_perm(ref$glstrinv.real,gl_perm);structure_p<-p_perm(ref$nwinv.real,nw_perm)

p<-length(nodes);edge_perm<-array(NA_real_,c(p,p,iterations),dimnames=list(nodes,nodes,NULL));cursor<-1L
for(ch in chunks){k<-dim(ch$einv.perm)[3];edge_perm[,,cursor:(cursor+k-1L)]<-ch$einv.perm;cursor<-cursor+k}
upper<-which(upper.tri(ref$einv.real),arr.ind=TRUE)
edge_p<-rbindlist(lapply(seq_len(nrow(upper)),function(i){r<-upper[i,1];c<-upper[i,2]
  data.table(node1=rownames(ref$einv.real)[r],node2=colnames(ref$einv.real)[c],observed_absolute_difference=ref$einv.real[r,c],p_value=p_perm(ref$einv.real[r,c],edge_perm[r,c,]))}))
edge_p[,q_value_BH:=p.adjust(p_value,"BH")];edge_p[,`:=`(nominal_p_lt_05=p_value<.05,FDR_q_lt_05=q_value_BH<.05,permutations=iterations)]
setorder(edge_p,p_value,-observed_absolute_difference)

cen_perm<-do.call(rbind,lapply(chunks,`[[`,"diffcen.perm"));real_c<-as.vector(ref$diffcen.real)
cp<-vapply(seq_along(real_c),function(j)p_perm(real_c[j],cen_perm[,j]),numeric(1))
metric_names<-colnames(ref$diffcen.real);if(is.null(metric_names))metric_names<-c("strength","expectedInfluence")[seq_len(ncol(ref$diffcen.real))]
node_names<-rownames(ref$diffcen.real);if(is.null(node_names))node_names<-nodes
cen<-data.table(node_id=rep(node_names,times=length(metric_names)),centrality=rep(metric_names,each=length(node_names)),
  observed_absolute_difference=real_c,p_value=cp)
cen[,q_value_BH:=p.adjust(p_value,"BH"),by=centrality];cen[,`:=`(nominal_p_lt_05=p_value<.05,FDR_q_lt_05=q_value_BH<.05,permutations=iterations)]

runtime<-as.numeric(difftime(Sys.time(),start,units="secs"))
summary<-data.table(analysis=analysis,comparison="ICU vs independent comparator",paired=FALSE,node_count=p,possible_edges=p*(p-1)/2,
  correlation=cor_method,estimator="EBICglasso",gamma=gamma,permutations=iterations,n_ICU=nrow(x1),n_comparator=nrow(x2),
  test=c("global_strength","network_structure"),statistic=c(as.numeric(ref$glstrinv.real),as.numeric(ref$nwinv.real)),
  p_value=c(global_p,structure_p),ICU_global_strength=c(as.numeric(ref$glstrinv.sep[[1]]),NA_real_),
  comparator_global_strength=c(as.numeric(ref$glstrinv.sep[[2]]),NA_real_),runtime_seconds=runtime,seed=base_seed)
fwrite(summary,file.path(out,"network_comparison_test.csv"));fwrite(edge_p,file.path(out,"nct_edge_differences.csv"));fwrite(cen,file.path(out,"nct_node_centrality_differences.csv"))
fwrite(data.table(permutation=seq_len(iterations),global_strength_statistic=gl_perm,network_structure_statistic=nw_perm),file.path(out,"nct_global_permutation_distributions.csv"))
fwrite(data.table(chunk=seq_along(sizes),seed=base_seed+10000L*seq_along(sizes),permutations=sizes),file.path(out,"nct_chunk_seeds.csv"))
meta<-data.table(analysis=analysis,paired=FALSE,correlation=cor_method,gamma=gamma,permutations=iterations,cores=cores,
  n_ICU=nrow(x1),n_comparator=nrow(x2),node_count=p,edge_tests=nrow(edge_p),node_tests=nrow(cen),seed=base_seed,runtime_seconds=runtime,
  saved_complete_permutations=save_perm)
fwrite(meta,file.path(out,"nct_run_metadata.csv"))
if(save_perm)saveRDS(list(edge_permutations=edge_perm,centrality_permutations=cen_perm,global_strength=gl_perm,network_structure=nw_perm),
                     file.path(out,"complete_permutation_distributions.rds"),compress="xz")
append_log(log_file,"PASS NCT",analysis,"global p=",global_p,"structure p=",structure_p,"edge tests=",nrow(edge_p),"runtime sec=",round(runtime,1))
