#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table); library(MatchIt); library(cobalt); library(ggplot2)
  library(sandwich); library(lmtest)
})

args<-commandArgs(trailingOnly=TRUE)
if(length(args)<5L)stop("Usage: 02_descriptive_psm.R <scored.csv> <output_dir> <figures_dir> <mode> <log>")
input<-args[1];out<-args[2];figdir<-args[3];mode<-args[4];log_file<-args[5]
dir.create(out,recursive=TRUE,showWarnings=FALSE);dir.create(figdir,recursive=TRUE,showWarnings=FALSE)
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))])
source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START descriptive statistics and matching")

d<-fread(input)
primary<-assert_scored_input(d,"descriptive/PSM source")
primary[,icu:=as.integer(A_q9==8L)]
primary[,group:=factor(ifelse(icu==1L,"ICU","Non-ICU"),levels=c("Non-ICU","ICU"))]

cont_summary<-function(dt,vars,sample){
  pieces<-lapply(vars,function(v){
    dt[,.(sample=sample,variable=v,level=NA_character_,n=sum(!is.na(get(v))),
      mean=mean(get(v),na.rm=TRUE),sd=sd(get(v),na.rm=TRUE),median=median(get(v),na.rm=TRUE),
      q1=quantile(get(v),.25,na.rm=TRUE),q3=quantile(get(v),.75,na.rm=TRUE),count=NA_integer_,percent=NA_real_),by=group]
  })
  rbindlist(pieces,fill=TRUE)
}
cat_summary<-function(dt,vars,sample){
  pieces<-lapply(vars,function(v){z<-dt[,.(count=.N),by=.(group,level=as.character(get(v)))];z[,percent:=100*count/sum(count),by=group]
    z[,`:=`(sample=sample,variable=v,n=NA_integer_,mean=NA_real_,sd=NA_real_,median=NA_real_,q1=NA_real_,q3=NA_real_)];z})
  rbindlist(pieces,fill=TRUE)
}
cont_vars<-c("A_age","A_gongzuoshichang","A_BMI")
cat_vars<-c("A_q2","A_q5","A_q11","A_q12","C_q1","C_q2","C_q8")
fwrite(rbindlist(list(cont_summary(primary,cont_vars,"Eligible primary network"),cat_summary(primary,cat_vars,"Eligible primary network")),fill=TRUE),
       file.path(out,"baseline_characteristics_eligible_long.csv"))

outcomes<-c("GAD7","PHQ9","PSS10","MBI_EE","MBI_DP","MBI_low_PA","FS14_total","FS14_physical","FS14_mental")
scale_pieces<-lapply(outcomes,function(v){
  rbindlist(list(
    primary[,.(sample="Overall",group="Overall",outcome=v,n=.N,mean=mean(get(v)),sd=sd(get(v)),median=median(get(v)),q1=quantile(get(v),.25),q3=quantile(get(v),.75))],
    primary[,.(sample="By ICU status",outcome=v,n=.N,mean=mean(get(v)),sd=sd(get(v)),median=median(get(v)),q1=quantile(get(v),.25),q3=quantile(get(v),.75)),by=group]
  ),fill=TRUE)
})
scale_summary<-rbindlist(scale_pieces,fill=TRUE)
fwrite(scale_summary,file.path(out,"scale_subscale_descriptive_statistics.csv"))

unadjusted<-rbindlist(lapply(outcomes,function(v){
  fit<-lm(reformulate("icu",v),data=primary);ct<-coeftest(fit,vcov.=vcovHC(fit,type="HC3"));b<-unname(ct["icu","Estimate"]);se<-unname(ct["icu","Std. Error"])
  data.table(outcome=v,n=nobs(fit),difference_icu_minus_non=b,HC3_se=se,ci95_low=b-qnorm(.975)*se,ci95_high=b+qnorm(.975)*se,
             p_value=unname(ct["icu","Pr(>|t|)"]),standardized_effect=b/sd(primary[[v]]))
}))
unadjusted[,q_value_BH:=p.adjust(p_value,"BH")]
fwrite(unadjusted,file.path(out,"unadjusted_group_differences_hc3.csv"))

primary[,`:=`(sex=factor(A_q2),education=factor(A_q5),title=factor(A_q11),administrative_role=factor(A_q12),
              schedule=factor(C_q1),shift_type=factor(C_q2),night_shift_frequency=factor(C_q8))]
adjust_covars<-c("A_age","A_gongzuoshichang","sex","education","title","administrative_role","schedule","shift_type","night_shift_frequency")
adjusted<-rbindlist(lapply(outcomes,function(v){
  fit<-lm(reformulate(c("icu",adjust_covars),v),data=primary);ct<-coeftest(fit,vcov.=vcovHC(fit,type="HC3"));b<-unname(ct["icu","Estimate"]);se<-unname(ct["icu","Std. Error"])
  data.table(outcome=v,n=nobs(fit),adjusted_difference_icu_minus_non=b,HC3_se=se,ci95_low=b-qnorm(.975)*se,ci95_high=b+qnorm(.975)*se,
             p_value=unname(ct["icu","Pr(>|t|)"]),standardized_effect=b/sd(primary[[v]]))
}))
adjusted[,q_value_BH:=p.adjust(p_value,"BH")]
fwrite(adjusted,file.path(out,"adjusted_linear_models_hc3.csv"))

run_match<-function(dt,covars,label,folder,exact_vars=NULL){
  dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  dt<-copy(dt);assert_scored_input(dt,paste(label,"matching source"));dt[,icu:=as.integer(A_q9==8L)]
  if(sum(dt$icu==1L)==0L||sum(dt$icu==0L)==0L)stop(label,": both exposure groups required")
  form<-reformulate(covars,response="icu")
  stronger_exact<-unique(c(exact_vars,intersect(c("A_q5"),covars)))
  attempts<-list(list(cal=.20,exact=NULL,mah=NULL,order="largest"),
                 list(cal=.10,exact=exact_vars,mah=c("A_age","A_gongzuoshichang"),order="largest"),
                 list(cal=.05,exact=exact_vars,mah=c("A_age","A_gongzuoshichang"),order="largest"),
                 list(cal=.05,exact=stronger_exact,mah=c("A_age","A_gongzuoshichang"),order="largest"),
                 list(cal=.03,exact=stronger_exact,mah=c("A_age","A_gongzuoshichang"),order="largest"))
  attempts<-c(attempts,lapply(seq_len(20L),function(i)list(cal=if(i%%2L).10 else .05,exact=exact_vars,
    mah=c("A_age","A_gongzuoshichang"),order="random")))
  chosen<-NULL;best<-Inf
  for(k in seq_along(attempts)){
    set.seed(20260713+k)
    aa<-attempts[[k]]
    m<-matchit(form,data=dt,method="nearest",distance="glm",estimand="ATT",ratio=1,replace=FALSE,
               caliper=aa$cal,std.caliper=TRUE,exact=if(length(aa$exact))reformulate(aa$exact) else NULL,
               mahvars=if(length(aa$mah))reformulate(intersect(aa$mah,covars)) else NULL,m.order=aa$order)
    bt<-bal.tab(m,un=TRUE,binary="std",continuous="std",s.d.denom="pooled")
    mx<-max(abs(bt$Balance$Diff.Adj),na.rm=TRUE)
    if(mx<best){chosen<-m;best<-mx}
    if(mx<.10)break
  }
  if(!is.finite(best)||best>=.10)stop(label,": matching failed max absolute SMD < 0.10; best=",best)
  md<-as.data.table(match.data(chosen,drop.unmatched=TRUE));md[,group:=factor(ifelse(icu==1L,"ICU","Matched non-ICU"))]
  assert_group_matrix(md[icu==1L],NODE_IDS,paste(label," ICU"));assert_group_matrix(md[icu==0L],NODE_IDS,paste(label," non-ICU"))
  if(anyDuplicated(md$participant_hash))stop(label,": duplicate participant after matching")
  bt<-bal.tab(chosen,un=TRUE,binary="std",continuous="std",s.d.denom="pooled")
  bal<-as.data.table(bt$Balance,keep.rownames="covariate");bal[,model:=label]
  fwrite(bal,file.path(folder,"covariate_balance.csv"));fwrite(md,file.path(folder,"matched_data.csv"))
  obs<-as.data.table(bt$Observations,keep.rownames="sample");fwrite(obs,file.path(folder,"sample_sizes.csv"))
  audit<-data.table(model=label,eligible_source_n=nrow(dt),eligible_ICU=sum(dt$icu==1L),eligible_comparator=sum(dt$icu==0L),
    matched_ICU=sum(md$icu==1L),matched_comparator=sum(md$icu==0L),maximum_absolute_SMD=best,all_absolute_SMD_below_0_10=best<.10)
  fwrite(audit,file.path(folder,"matching_audit.csv"))
  lp<-love.plot(chosen,stats="mean.diffs",abs=TRUE,binary="std",thresholds=c(m=.10),var.order="unadjusted",
    colors=c("#777777","#4393C3"),shapes=c(16,17),sample.names=c("Before matching","After matching"))+
    theme_minimal(base_size=10)+theme(panel.grid.minor=element_blank(),plot.background=element_rect(fill="white",colour=NA))+
    labs(title=if(mode=="simulation")paste0(label," — SIMULATION ONLY—NOT FOR MANUSCRIPT") else label,
         x="Absolute standardized mean difference",y=NULL)
  ggsave(file.path(folder,"love_plot.pdf"),lp,width=7.2,height=6.2,device=cairo_pdf)
  ggsave(file.path(folder,"love_plot.png"),lp,width=7.2,height=6.2,dpi=600,bg="white")
  invisible(md)
}

a_cov<-c("A_age","A_q2","A_gongzuoshichang","A_q5","A_q11","A_q12")
b_cov<-c(a_cov,"C_q1","C_q2","C_q8")
psmA<-run_match(primary,a_cov,"PSM-A",file.path(out,"PSM_A"),exact_vars=c("A_q2"))
psmB<-run_match(primary,b_cov,"PSM-B",file.path(out,"PSM_B"),exact_vars=c("A_q2","C_q1"))
ordinary_source<-primary[A_q9==8L|A_q9%in%c(1L,2L,4L,5L,6L)]
psmO<-run_match(ordinary_source,a_cov,"Ordinary-inpatient PSM-A",file.path(out,"PSM_A_ordinary"),exact_vars=c("A_q2"))

all_audits<-rbindlist(lapply(c("PSM_A","PSM_B","PSM_A_ordinary"),function(x)fread(file.path(out,x,"matching_audit.csv"))))
fwrite(all_audits,file.path(out,"matching_models_summary.csv"))
append_log(log_file,"PASS descriptives and three matching models; max SMDs=",paste(round(all_audits$maximum_absolute_SMD,4),collapse=","))
