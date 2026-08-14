# Central configuration for the male LUTS and work-productivity analysis.
#
# IMPORTANT:
# - Do not enter sample sizes after exclusions, class counts, effect estimates,
#   confidence intervals, or P values in this file.
# - The only expected count below is the source-data integrity check supplied
#   by the research team: the real export contains 64,114 records.

CFG <- list(
  pipeline_version = "3.0.0-github",
  run_mode = "production",
  random_seed = 20260814L,
  expected_source_records = 64114L,
  enforce_expected_source_records = TRUE,

  max_iterations = 10000L,
  lca_random_starts = c(initial = 200L, retry = 500L, final = 1000L),
  lca_best_ll_min_repetitions = 2L,
  pseudoclass_draws_main = 1000L,
  pseudoclass_draws_sensitivity = 200L,
  local_dependence_bh_alpha = 0.05,
  local_dependence_broad_fraction = 0.25,
  local_dependence_bvr_material = 10,
  class_warn_fraction = 0.05,
  class_high_risk_fraction = 0.03,
  minimum_class_n_high_risk = 100L,

  # Data source. The pipeline first looks for an object with this name in the
  # RStudio Global Environment. If absent, it reads data_file.
  data_object_name = "data",
  data_file = "data/data.csv",
  dictionary_file = "metadata/variable_dictionary.csv",

  # Optional Word template. Leave blank to use a clean officer template.
  reference_docx = "",
  generate_manuscript = TRUE,

  # PRODUCTION GATES: keep FALSE until the preflight audit and original
  # questionnaire/export documentation have been checked on the bastion host.
  unique_id_mapping_confirmed = FALSE,
  sex_code_confirmed = FALSE,
  male_code = "1",
  male_module_ownership_confirmed = FALSE,
  sps6_confirmed_scheme = "dictionary_reverse_items_5_6",
  sps6_scoring_confirmed_for_production = FALSE,

  variables = list(
    id = "id",
    sex = "A_q2",
    source_luts_total = "I_niaolu_score_all_M",
    source_sps6_total = "D_shengchanlishousun_all",
    luts = c("I_q1", "I_q3", "I_q5", "I_q7", "I_q11", "I_q13", "I_q15",
             "I_q19", "I_q23", "I_q27", "I_q31", "I_q35", "I_q37"),
    sps6 = paste0("D_q21_", 1:6),
    covariates = c(age = "A_age", bmi = "A_BMI", education = "A_q5",
                   marital = "A_q6", department = "A_q9", title = "A_q11",
                   management = "A_q12", smoking = "A_q24", drinking = "A_q30",
                   chronic = "A_q19_1_1", shift = "C_q1", overtime = "C_q42",
                   water = "A_q38"),
    quality = c(work_start_year = "work_y", survey_year = "A_year_sub",
                birth_year = "A_year", duration1 = "timetaken.x",
                duration2 = "timetaken.y", duration3 = "timetaken.x.x",
                duration4 = "timetaken.y.y")
  ),

  luts_labels_zh = c(
    I_q1 = "尿频", I_q3 = "夜尿", I_q5 = "尿急", I_q7 = "急迫性尿失禁",
    I_q11 = "压力性尿失禁", I_q13 = "无明显诱因尿失禁", I_q15 = "排尿等待",
    I_q19 = "持续排尿需用力", I_q23 = "尿线减弱", I_q27 = "尿流中断",
    I_q31 = "排尿不尽感", I_q35 = "排尿后滴沥", I_q37 = "夜间遗尿"
  ),
  luts_dimensions = list(
    storage = c("I_q1", "I_q3", "I_q5", "I_q7", "I_q11", "I_q13", "I_q37"),
    voiding = c("I_q15", "I_q19", "I_q23", "I_q27"),
    post_micturition = c("I_q31", "I_q35")
  ),
  sps6_dimensions = list(
    work_limitations = paste0("D_q21_", 1:4),
    work_energy = paste0("D_q21_", 5:6)
  ),
  sps6_questions_zh = c(
    D_q21_1 = "因健康问题，我的工作压力更加难以调节了",
    D_q21_2 = "因健康问题，我没法完成工作中难度大的任务",
    D_q21_3 = "因健康问题，让我不能从工作中得到乐趣",
    D_q21_4 = "因健康问题，我觉得根本不可能开展某些工作任务",
    D_q21_5 = "尽管健康方面有问题，我仍然能集中精神完成工作",
    D_q21_6 = "尽管健康方面有问题，我仍觉得精力充沛可以完成所有工作"
  ),

  model_covariates = c("age", "bmi", "education", "married", "department",
                       "title_group", "management", "smoking", "drinking",
                       "chronic", "night_shift", "overtime_weeks"),
  no_hospital_models = TRUE
)
