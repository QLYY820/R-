#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(psych)})

args<-commandArgs(trailingOnly=TRUE)
if(length(args)<4)stop("Usage: 01_prepare_scored_eligible.R <raw.csv> <dictionary.csv> <output_dir> <log>")
raw_file<-args[1];dict_file<-args[2];out<-args[3];log_file<-args[4]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
# Robustly locate common.R when invoked with Rscript.
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))])
source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START scoring and eligibility")

d<-fread(raw_file,encoding="UTF-8")
dict<-fread(dict_file,encoding="UTF-8")
if(nrow(dict)!=62L)stop("Dictionary must contain exactly 62 rows")
if(anyDuplicated(dict$node_id)||!setequal(dict$node_id,NODE_IDS))stop("Dictionary node IDs are not the expected 62 unique nodes")
if(length(setdiff(dict$variable_name,names(d))))stop("Required source items missing: ",paste(setdiff(dict$variable_name,names(d)),collapse=","))

item_audit<-dict[,{
  x<-d[[variable_name]]
  .(n=length(x),n_missing=sum(is.na(x)),observed_min=min(x,na.rm=TRUE),observed_max=max(x,na.rm=TRUE),
    n_out_of_range=sum(!is.na(x)&(x<source_min|x>source_max)),unique_values=paste(sort(unique(x[!is.na(x)])),collapse=";"))
},by=.(variable_name,node_id,domain,source_min,source_max)]
fwrite(item_audit,file.path(out,"item_range_missingness_62.csv"))
if(any(item_audit$n_out_of_range>0))stop("Unexplained out-of-range source item values detected")

for(i in seq_len(nrow(dict))){
  x<-d[[dict$variable_name[i]]]
  value<-if(dict$reverse_scored_for_network[i])dict$analysis_max[i]-x else x
  d[,(dict$node_id[i]):=value]
}

d[,GAD7:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("ANX",1:7)]
d[,PHQ9:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("DEP",1:9)]
d[,PSS10:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("STR",1:10)]
d[,MBI_EE:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("BO",c(1,2,3,6,8,13,14,16,20))]
d[,MBI_DP:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("BO",c(5,10,11,15,22))]
d[,MBI_low_PA:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("BO",MBI_REVERSE)]
d[,MBI_positive_PA:=rowSums(6L-as.matrix(.SD),na.rm=FALSE),.SDcols=paste0("BO",MBI_REVERSE)]
d[,FS14_total:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("FAT",1:14)]
d[,FS14_physical:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("FAT",1:8)]
d[,FS14_mental:=rowSums(.SD,na.rm=FALSE),.SDcols=paste0("FAT",9:14)]

pairs<-list(c("GAD7","D_jiaolv_all"),c("PHQ9","D_yiyu_all"),c("PSS10","D_yali_all"),
            c("MBI_EE","F_qingganshuaijie"),c("MBI_DP","F_qurengehua"),
            c("MBI_positive_PA","F_gerenchengjiugan"),c("FS14_total","G_pifa_all"),
            c("FS14_physical","G_qutipifa"),c("FS14_mental","G_naolipifa"))
score_audit<-rbindlist(lapply(pairs,function(p){
  if(!p[2]%in%names(d))stop("Stored total missing: ",p[2])
  x<-d[[p[1]]];y<-d[[p[2]]];ok<-complete.cases(x,y);delta<-x[ok]-y[ok]
  data.table(recomputed=p[1],stored=p[2],n_complete=sum(ok),correlation=cor(x[ok],y[ok]),
             mean_difference=mean(delta),min_difference=min(delta),max_difference=max(delta),
             n_nonzero_difference=sum(delta!=0))
}))
score_audit[,verified:=abs(correlation-1)<1e-12&mean_difference==0&min_difference==0&max_difference==0&n_nonzero_difference==0]
fwrite(score_audit,file.path(out,"scoring_reconciliation.csv"))
if(!all(score_audit$verified))stop("Stored and recomputed scores are inconsistent")
dict[,scoring_verified_against_existing_total:=all(score_audit$verified)]
fwrite(dict,file.path(out,"verified_node_dictionary_62.csv"))

time_cols<-grep("^timetaken",names(d),value=TRUE)
d[,total_response_seconds:=rowSums(.SD,na.rm=FALSE),.SDcols=time_cols]
d[,flag_duplicate_source_id:=duplicated(participant_hash)|duplicated(participant_hash,fromLast=TRUE)]
d[,flag_department_invalid:=is.na(A_q9)|!(A_q9%in%1:11)]
d[,flag_item_invalid:=rowSums(is.na(.SD))>0,.SDcols=NODE_IDS]
d[,eligible_primary_network:=!flag_duplicate_source_id & !flag_department_invalid & !flag_item_invalid]
d[,flag_age_implausible:=is.na(A_age)|A_age<18|A_age>70]
d[,flag_tenure_implausible:=is.na(A_gongzuoshichang)|A_gongzuoshichang<0|A_gongzuoshichang>55]
d[,flag_age_tenure_inconsistent:=!is.na(A_age)&!is.na(A_gongzuoshichang)&A_gongzuoshichang>(A_age-15)]
d[,flag_bmi_implausible:=!is.na(A_BMI)&(A_BMI<12|A_BMI>60)]
d[,flag_duration_nonpositive:=is.na(total_response_seconds)|total_response_seconds<=0]

eligible<-assert_scored_input(d,"scored eligible data")
flow<-data.table(stage=c("Raw source rows","Duplicate-ID eligible","Valid department","Complete 62 nodes","Eligible primary network","Eligible ICU","Eligible non-ICU","Eligible ordinary inpatient non-ICU"),
                 n=c(nrow(d),sum(!d$flag_duplicate_source_id),sum(!d$flag_duplicate_source_id&!d$flag_department_invalid),
                     sum(!d$flag_duplicate_source_id&!d$flag_department_invalid&!d$flag_item_invalid),nrow(eligible),
                     sum(eligible$A_q9==8),sum(eligible$A_q9!=8),sum(eligible$A_q9%in%c(1,2,4,5,6))))
fwrite(flow,file.path(out,"sample_flow.csv"))
fwrite(d[,.(analysis_id,participant_hash,eligible_primary_network,flag_duplicate_source_id,flag_department_invalid,
             flag_item_invalid,flag_age_implausible,flag_tenure_implausible,flag_age_tenure_inconsistent,
             flag_bmi_implausible,flag_duration_nonpositive)],file.path(out,"participant_qc_flags.csv"))

missing_vars<-c("A_q2","A_age","A_BMI","A_gongzuoshichang","A_q5","A_q9","A_q11","A_q12","C_q1","C_q2","C_q8",NODE_IDS)
miss<-rbindlist(lapply(missing_vars,function(v)data.table(variable=v,n_missing=sum(is.na(d[[v]])),percent_missing=mean(is.na(d[[v]]))*100)))
fwrite(miss,file.path(out,"missingness_all_required_variables.csv"))

alpha_sets<-list(GAD7=paste0("ANX",1:7),PHQ9=paste0("DEP",1:9),PSS10=paste0("STR",1:10),
  MBI_EE=paste0("BO",c(1,2,3,6,8,13,14,16,20)),MBI_DP=paste0("BO",c(5,10,11,15,22)),
  MBI_low_PA=paste0("BO",MBI_REVERSE),FS14_total=paste0("FAT",1:14),FS14_physical=paste0("FAT",1:8),FS14_mental=paste0("FAT",9:14))
alphas<-rbindlist(lapply(c("Overall","ICU","Non-ICU"),function(g){
  z<-if(g=="Overall")eligible else if(g=="ICU")eligible[A_q9==8] else eligible[A_q9!=8]
  rbindlist(lapply(names(alpha_sets),function(s){
    cols<-alpha_sets[[s]]
    a<-psych::alpha(as.data.frame(z[,..cols]),check.keys=FALSE,warnings=FALSE)
    data.table(group=g,scale=s,n=nrow(z),n_items=length(alpha_sets[[s]]),alpha=a$total$raw_alpha)}))
}))
fwrite(alphas,file.path(out,"cronbach_alpha_overall_by_group.csv"))
fwrite(d,file.path(out,"scored_analysis_data.csv"))
append_log(log_file,"PASS scoring; eligible n=",nrow(eligible)," ICU=",sum(eligible$A_q9==8)," nonICU=",sum(eligible$A_q9!=8))
