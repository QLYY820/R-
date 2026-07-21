#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(qgraph);library(ggplot2);library(patchwork)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<7L)stop("Usage: 09_generate_figures.R <network_dir> <nct_dir> <bootstrap_dir> <predict_dir> <fig_dir> <mode> <log>")
netdir<-args[1];nctdir<-args[2];bootdir<-args[3];preddir<-args[4];out<-args[5];mode<-args[6];log_file<-args[7];dir.create(out,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START figures")
notice<-if(mode=="simulation")"SIMULATION ONLY—NOT FOR MANUSCRIPT" else ""
models<-readRDS(file.path(netdir,"PSM_A_full62","network_models.rds"));W1<-models$ICU$weights;W2<-models$Comparator$weights
palette<-c(Anxiety="#7EA6D8",Depression="#D59AA2",Stress="#9BC5A3",Burnout="#D9B66F",Fatigue="#A99AC6");cols<-unname(palette[NODE_DOMAINS[NODE_IDS]])
layout<-qgraph::averageLayout(W1,W2,layout="spring")
draw_main<-function(){par(mfrow=c(1,3),mar=c(2,2,3,1));qgraph(W1,layout=layout,labels=NODE_IDS,color=cols,groups=split(NODE_IDS,NODE_DOMAINS),
  vsize=4.2,label.cex=.55,edge.color=c("#D6604D","#4393C3"),title="A  ICU: full 62-node network",legend=FALSE)
  qgraph(W2,layout=layout,labels=NODE_IDS,color=cols,groups=split(NODE_IDS,NODE_DOMAINS),vsize=4.2,label.cex=.55,
    edge.color=c("#D6604D","#4393C3"),title="B  PSM-A comparator",legend=FALSE)
  sm<-fread(file.path(nctdir,"main_PSM_A","network_comparison_test.csv"));plot.new();title("C  Network comparison test\nFull modeled networks")
  text(.05,.75,paste0("Global strength p = ",format(sm[test=="global_strength"]$p_value,digits=3)),adj=0,cex=1.05)
  text(.05,.60,paste0("Network structure p = ",format(sm[test=="network_structure"]$p_value,digits=3)),adj=0,cex=1.05)
  ep<-fread(file.path(nctdir,"main_PSM_A","nct_edge_differences.csv"));text(.05,.45,paste0("FDR-significant edges = ",sum(ep$FDR_q_lt_05)," / ",nrow(ep)),adj=0,cex=1.05)
  if(nzchar(notice)){text(.5,.05,notice,col="#A23B3B",cex=.75);mtext(notice,outer=TRUE,line=-1,col="#A23B3B",cex=.7)}}
base_export<-function(stem,fun,w=15,h=6){cairo_pdf(file.path(out,paste0(stem,".pdf")),width=w,height=h);fun();dev.off();png(file.path(out,paste0(stem,".png")),width=w,height=h,units="in",res=600,bg="white");fun();dev.off();tiff(file.path(out,paste0(stem,".tiff")),width=w,height=h,units="in",res=600,compression="lzw",bg="white");fun();dev.off()}
base_export("Figure1_network_comparison_main",draw_main)

ct<-fread(file.path(netdir,"all_network_centrality_bridge.csv"))[analysis=="PSM_A_full62"]
top<-ct[,.(importance=max(abs(expected_influence))),by=node_id][order(-importance)][1:10,node_id]
z<-ct[node_id%in%top];z[,network:=factor(network,levels=c("Comparator","ICU"))];z[,node_id:=factor(node_id,levels=rev(top))]
p2<-ggplot(z,aes(expected_influence_z,node_id,colour=network))+geom_line(aes(group=node_id),colour="#BBBBBB",linewidth=.7)+geom_point(size=3)+
  scale_colour_manual(values=c(Comparator="#4393C3",ICU="#D6604D"))+theme_minimal(base_size=11)+labs(x="Standardized expected influence",y=NULL,colour=NULL,title="Expected influence comparison")+
  annotate("text",x=Inf,y=-Inf,label=notice,hjust=1.05,vjust=-.8,colour="#A23B3B",size=3)
z3<-ct[,.(importance=max(abs(bridge_expected_influence))),by=node_id][order(-importance)][1:10,node_id]
b<-ct[node_id%in%z3];b[,network:=factor(network,levels=c("Comparator","ICU"))];b[,node_id:=factor(node_id,levels=rev(z3))]
p3<-ggplot(b,aes(bridge_expected_influence_z,node_id,colour=network))+geom_line(aes(group=node_id),colour="#BBBBBB",linewidth=.7)+geom_point(size=3)+
  scale_colour_manual(values=c(Comparator="#4393C3",ICU="#D6604D"))+theme_minimal(base_size=11)+labs(x="Standardized bridge expected influence",y=NULL,colour=NULL,title="Bridge centrality across predefined domains")+
  annotate("text",x=Inf,y=-Inf,label=notice,hjust=1.05,vjust=-.8,colour="#A23B3B",size=3)
ggexport<-function(stem,p,w=8,h=6){ggsave(file.path(out,paste0(stem,".pdf")),p,width=w,height=h,device=cairo_pdf);ggsave(file.path(out,paste0(stem,".png")),p,width=w,height=h,dpi=600,bg="white");ggsave(file.path(out,paste0(stem,".tiff")),p,width=w,height=h,dpi=600,compression="lzw",bg="white")}
ggexport("Figure2_expected_influence_dumbbell",p2);ggexport("Figure3_bridge_expected_influence",p3)

csfile<-file.path(bootdir,"all_custom_bridge_CS_coefficients.csv")
if(file.exists(csfile)){cs<-fread(csfile);p<-ggplot(cs,aes(metric,cs_coefficient,fill=network))+geom_col(position=position_dodge(.8),width=.7)+facet_wrap(~model)+
  geom_hline(yintercept=.5,linetype=2,colour="#777777")+scale_fill_manual(values=c(ICU="#D6604D",Comparator="#4393C3"))+theme_minimal(base_size=10)+
  theme(axis.text.x=element_text(angle=25,hjust=1))+labs(x=NULL,y="CS coefficient",title=paste("Bridge case-dropping stability —",notice));ggexport("FigureS1_bridge_stability",p,10,6)}
ep<-fread(file.path(nctdir,"main_PSM_A","nct_edge_differences.csv"));p<-ggplot(ep,aes(p_value))+geom_histogram(binwidth=.05,fill="#7EA6D8",colour="white")+
  geom_vline(xintercept=.05,linetype=2,colour="#D6604D")+theme_minimal(base_size=11)+labs(title=paste("NCT edge-level p-value distribution —",notice),x="Raw permutation p value",y="Edge count")
ggexport("FigureS2_NCT_edge_pvalue_distribution",p)
sens<-fread(file.path(netdir,"network_sensitivity_comparison_matrix.csv"));p<-ggplot(sens,aes(analysis,network,fill=edge_weight_correlation_with_PSM_A))+geom_tile(colour="white")+
  scale_fill_gradient2(low="#D6604D",mid="white",high="#4393C3",midpoint=.5,limits=c(-1,1))+theme_minimal(base_size=9)+theme(axis.text.x=element_text(angle=55,hjust=1))+
  labs(title=paste("Network sensitivity concordance —",notice),x=NULL,y=NULL,fill="Edge rank r")
ggexport("FigureS3_network_sensitivity_matrix",p,11,5)

stable_draw<-function(){par(mfrow=c(1,2),mar=c(2,2,3,1));for(g in c("ICU","Comparator")){ci<-fread(file.path(bootdir,"PSM_A",g,"edge_bootstrap_ci_1000.csv"));W<-if(g=="ICU")W1 else W2;keep<-matrix(FALSE,62,62,dimnames=dimnames(W));for(i in seq_len(nrow(ci))){if(isTRUE(ci$ci_excludes_zero[i]))keep[ci$node1[i],ci$node2[i]]<-keep[ci$node2[i],ci$node1[i]]<-TRUE};Ws<-W*keep
  qgraph(Ws,layout=layout,labels=NODE_IDS,color=cols,vsize=4,label.cex=.5,title=paste(g,"bootstrap-stable edges"),legend=FALSE)};mtext(notice,outer=TRUE,line=-1,col="#A23B3B",cex=.7)}
if(file.exists(file.path(bootdir,"PSM_A","ICU","edge_bootstrap_ci_1000.csv")))base_export("FigureS4_stable_edge_sensitivity_network",stable_draw,11,6)
ggexport("FigureS5_traditional_centrality_plot",p2);ggexport("FigureS6_bridge_expected_influence_plot",p3)
pred<-fread(file.path(preddir,"mgm_predictability_5fold_all_nodes.csv"));p<-ggplot(pred,aes(nCC_mean,reorder(node_id,nCC_mean),colour=network))+geom_point(alpha=.8)+facet_wrap(~network,scales="free_y")+
  geom_vline(xintercept=0,linetype=2)+scale_colour_manual(values=c("ICU PSM-A"="#D6604D","Matched non-ICU PSM-A"="#4393C3"))+theme_minimal(base_size=8)+theme(legend.position="none")+
  labs(title=paste("Cross-validated normalized classification accuracy —",notice),x="Mean nCC",y=NULL)
ggexport("FigureS7_mgm_predictability_CV",p,11,9)
append_log(log_file,"PASS publication figures exported PDF/PNG/TIFF; mode=",mode)
