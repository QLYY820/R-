#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table);library(openssl)})
args<-commandArgs(trailingOnly=TRUE)
if(length(args)<4L)stop("Usage: 10_finalize_audit.R <run_dir> <project_dir> <mode> <log>")
run<-normalizePath(args[1],winslash="/",mustWork=TRUE);project<-normalizePath(args[2],winslash="/",mustWork=TRUE);mode<-args[3];log_file<-args[4]
script_path<-sub("^--file=","",commandArgs(FALSE)[grep("^--file=",commandArgs(FALSE))]);source(file.path(dirname(normalizePath(script_path)),"common.R"))
append_log(log_file,"START content audit V2 and finalization")
notice<-if(mode=="simulation")"SIMULATION ONLY—NOT FOR MANUSCRIPT" else ""
rows<-list();add<-function(item,ok,evidence,required=TRUE){status<-if(isTRUE(ok))"PASS" else "FAIL";rows[[length(rows)+1L]]<<-data.table(item=item,status=status,required=required,evidence=evidence)}
na_add<-function(item){rows[[length(rows)+1L]]<<-data.table(item=item,status="NOT_APPLICABLE",required=FALSE,evidence="VARIABLES DO NOT EXIST IN STUDY DATA")}
prep<-file.path(run,"01_preparation");desc<-file.path(run,"02_descriptive_matching");net<-file.path(run,"03_networks");nct<-file.path(run,"04_NCT");down<-file.path(run,"05_downsampling");rnct<-file.path(run,"06_repeated_seed_NCT");boot<-file.path(run,"07_bootstrap");pred<-file.path(run,"08_predictability");fig<-file.path(run,"09_figures")
d<-fread(file.path(prep,"scored_analysis_data.csv"));e<-d[eligible_primary_network==TRUE];score<-fread(file.path(prep,"scoring_reconciliation.csv"));items<-fread(file.path(prep,"item_range_missingness_62.csv"))
add("Expected 62 modeled nodes",all(NODE_IDS%in%names(d))&&length(NODE_IDS)==62L,"62 node IDs verified")
add("No unexplained out-of-range item values",sum(items$n_out_of_range)==0L,paste("out-of-range=",sum(items$n_out_of_range)))
add("Recomputed scale scores equal stored totals",all(score$verified)&all(score$n_nonzero_difference==0)&all(score$correlation==1),"correlation=1; all differences=0")
add("No duplicate IDs in eligible sample",anyDuplicated(e$participant_hash)==0L,paste("eligible n=",nrow(e)))
flow<-fread(file.path(prep,"sample_flow.csv"));flow_eligible<-flow[stage=="Eligible primary network"]$n;flow_icu<-flow[stage=="Eligible ICU"]$n
add("Unified eligible sample used",length(flow_eligible)==1L&&nrow(e)==flow_eligible&&sum(e$A_q9==8L)==flow_icu,
    paste("eligible=",nrow(e)," ICU=",sum(e$A_q9==8L)," non-ICU=",sum(e$A_q9!=8L)))
ma<-fread(file.path(desc,"matching_models_summary.csv"));add("PSM-A and PSM-B and ordinary PSM-A completed",setequal(ma$model,c("PSM-A","PSM-B","Ordinary-inpatient PSM-A")),paste(ma$model,collapse="; "))
add("All matching models maximum absolute SMD <0.10",all(ma$maximum_absolute_SMD<.10),paste(round(ma$maximum_absolute_SMD,4),collapse=","))
ns<-fread(file.path(net,"all_network_summary.csv"));add("Primary networks are full 62-node models",all(ns[analysis=="PSM_A_full62"]$n_nodes==62L),"PSM-A full62 both groups")
el<-fread(file.path(net,"PSM_A_full62","ICU_edge_long_1891.csv"));add("Full62 edge long table contains 1,891 edges",nrow(el)==1891L,paste("rows=",nrow(el)))
sens<-fread(file.path(net,"network_sensitivity_comparison_matrix.csv"));add("Eleven network sensitivity specifications completed",uniqueN(sens$analysis)==11L,paste("specifications=",uniqueN(sens$analysis)))
nm<-fread(file.path(nct,"main_PSM_A","nct_run_metadata.csv"));add("Main NCT is unpaired",identical(nm$paired,FALSE),paste("paired=",nm$paired))
add("Main NCT uses 5,000 permutations",nm$permutations==5000L,paste("permutations=",nm$permutations))
ne<-fread(file.path(nct,"main_PSM_A","nct_edge_differences.csv"));nc<-fread(file.path(nct,"main_PSM_A","nct_node_centrality_differences.csv"))
add("Main NCT tests all 1,891 edges",nrow(ne)==1891L,paste("edge tests=",nrow(ne)))
add("NCT edge and node raw p and BH q columns exist",all(c("p_value","q_value_BH")%in%names(ne))&&all(c("p_value","q_value_BH")%in%names(nc)),"edge and node multiplicity columns verified")
sensmeta<-rbindlist(lapply(list.dirs(file.path(nct,"sensitivity"),recursive=FALSE,full.names=TRUE),function(x)fread(file.path(x,"nct_run_metadata.csv"))))
add("Five sensitivity NCTs use at least 1,000 permutations",nrow(sensmeta)==5L&&all(sensmeta$permutations>=1000L)&all(sensmeta$paired==FALSE),paste("runs=",nrow(sensmeta)))
ds<-fread(file.path(down,"repeated_downsampling_scenario_summary.csv"));add("Repeated downsampling 200 x 2 complete",nrow(ds)==2L&&all(ds$iterations==200L),paste(ds$iterations,collapse=" x "))
rr<-fread(file.path(rnct,"all_40_repeated_seed_NCT_results.csv"));add("Selected-seed NCT 20 x 2 complete",nrow(rr)==40L&&all(rr$permutations==1000L)&all(rr$paired==FALSE),paste("runs=",nrow(rr)))
cs<-fread(file.path(boot,"all_standard_centrality_CS_coefficients.csv"));add("Bootstrap actual repetitions >=1000",all(cs$bootstrap_samples>=1000L)&&all(file.exists(c(file.path(boot,"PSM_A","ICU","edge_accuracy_bootstrap_1000.rds"),file.path(boot,"PSM_A","Comparator","edge_accuracy_bootstrap_1000.rds")))),"edge PSM-A x2 and case PSM-A/PSM-B x4")
bcs<-fread(file.path(boot,"all_custom_bridge_CS_coefficients.csv"));add("Custom bridge stability exists",nrow(bcs)==12L&&all(c("bridge_strength","bridge_expected_influence","normalized_bridge_expected_influence")%in%unique(bcs$metric)),paste("rows=",nrow(bcs)))
pa<-fread(file.path(pred,"mgm_predictability_CV_audit.csv"));add("Predictability 5-fold CV complete",nrow(pa)==2L&&all(pa$nodes==62L)&all(pa$all_nodes_have_5_folds),"62 nodes x 2 groups x 5 folds")
figs<-list.files(fig,pattern="\\.(pdf|png|tiff)$",full.names=TRUE);add("Figures exported in required formats",all(c("pdf","png","tiff")%in%tools::file_ext(figs)),paste("figure files=",length(figs)))
logs<-list.files(run,pattern="\\.log$",recursive=TRUE,full.names=TRUE);seeds<-list.files(run,pattern="seed",recursive=TRUE,full.names=TRUE,ignore.case=TRUE);add("Logs and random seed records exist",length(logs)>=10L&&length(seeds)>=5L,paste("logs=",length(logs)," seed files=",length(seeds)))
na_add("Hospital-cluster robust standard errors");na_add("Multilevel hospital model");na_add("Region-stratified or region-matched analysis");na_add("Survey-weighted analysis")
audit<-rbindlist(rows);fwrite(audit,file.path(run,"statistical_coverage_audit_v2.csv"))
md<-c("# Statistical Coverage Audit V2",if(nzchar(notice))paste0("\n**",notice,"**") else NULL,"","| Item | Status | Required | Evidence |","|---|---|---:|---|",apply(audit,1,function(x)paste0("| ",paste(x,collapse=" | ")," |")))
writeLines(md,file.path(run,"statistical_coverage_audit_v2.md"),useBytes=TRUE)
fails<-audit[required==TRUE&status=="FAIL"]
runtime_file<-file.path(run,"pipeline_runtime.csv");rt<-if(file.exists(runtime_file))fread(runtime_file) else data.table(total_runtime_seconds=NA_real_)
report<-c(paste0("# ",toupper(mode)," FULL PIPELINE COMPLETION REPORT"),if(nzchar(notice))paste0("\n**",notice,"**") else NULL,"",
  paste0("- Pipeline exit status: ",if(nrow(fails))"NONZERO / AUDIT FAILED" else "0 / SUCCESS"),paste0("- Total runtime: ",round(rt$total_runtime_seconds[1],1)," seconds"),
  paste0("- Actual eligible sample: ",nrow(e)," (ICU ",sum(e$A_q9==8L),"; non-ICU ",sum(e$A_q9!=8L),")"),"- Main NCT: 5000 permutations, paired=FALSE","- Sensitivity and selected-seed NCT: 1000 permutations",
  "- Edge bootstrap: 1000 per PSM-A group","- Case-dropping bootstrap: 1000 per PSM-A and PSM-B group","- Repeated downsampling: 200 iterations x 2 pools",
  paste0("- Required analyses not run: ",nrow(fails)),"- NOT_APPLICABLE: hospital clustering, multilevel hospital models, region analyses, survey weighting",
  paste0("- Analysis completion status: ",if(nrow(fails))"NOT READY" else if(mode=="simulation")"READY FOR REAL-DATA EXECUTION" else "REAL-DATA PIPELINE COMPLETED"),"\n## Step Audit\n",
  paste0("- ",audit$item,": ",audit$status," (",audit$evidence,")"))
writeLines(report,file.path(run,"FULL_PIPELINE_COMPLETION_REPORT.md"),useBytes=TRUE)
how<-c("# Reproduce V2 Pipeline",if(nzchar(notice))paste0("\n**",notice,"**") else NULL,"","Run from a new empty output directory:","```bash",
  paste0("Rscript final_analysis_pipeline.R --input <data.csv> --dictionary <dictionary.csv> --config <analysis_config.yml> --output <output_dir> --mode ",mode),"```",
  "The pipeline is individual-level, unweighted, cross-sectional, resumable through step markers, and returns nonzero on any failed required step.")
writeLines(how,file.path(run,"HOW_TO_REPRODUCE_FULL_PIPELINE.md"),useBytes=TRUE)
writeLines(c(if(nzchar(notice))notice else NULL,"Figure 1. Full 62-node PSM-A networks and unpaired NCT summary.","Figure 2. Expected influence comparison.","Figure 3. Bridge expected influence using predefined symptom domains.","Supplementary figures report stability, multiplicity, sensitivity, and cross-validated predictability."),file.path(run,"figure_captions.txt"),useBytes=TRUE)
capture.output(sessionInfo(),file=file.path(run,"R_sessionInfo.txt"));capture.output(installed.packages()[,c("Package","Version")],file=file.path(run,"R_package_versions.txt"))
if(nrow(fails))stop("Content audit V2 contains required FAIL items: ",paste(fails$item,collapse="; "))

# Add simulation watermarks only in simulation mode. Real-data output remains clean.
if(mode=="simulation"){
  csvs<-list.files(run,pattern="\\.csv$",recursive=TRUE,full.names=TRUE)
  for(f in csvs){z<-fread(f);if(!"simulation_notice"%in%names(z))z[,simulation_notice:=notice];fwrite(z,f)}
  docs<-list.files(run,pattern="\\.(md|txt)$",recursive=TRUE,full.names=TRUE)
  for(f in docs){x<-readLines(f,warn=FALSE,encoding="UTF-8");if(!any(grepl(notice,x,fixed=TRUE)))writeLines(c(notice,"",x),f,useBytes=TRUE)}
}
files<-list.files(run,recursive=TRUE,full.names=TRUE);files<-files[file.info(files)$isdir==FALSE]
files<-files[basename(files)!="10_audit_finalize_console.log"]
hash_file<-function(f){con<-file(f,open="rb");on.exit(close(con));as.character(openssl::sha256(con))}
manifest<-data.table(path=gsub("\\\\","/",substring(files,nchar(run)+2L)),bytes=file.info(files)$size,
  sha256=vapply(files,hash_file,character(1)))
fwrite(manifest,file.path(run,"file_hash_manifest_sha256.csv"))
append_log(log_file,"PASS content audit V2; required FAIL=0; watermarks and SHA256 manifest complete")
