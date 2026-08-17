DEFAULT_DATA_DIRECTORY <- intToUtf8(c(0x6211, 0x7684, 0x6570, 0x636E))

CONFIG <- list(
  pipeline_version = "4.0.0-bastion-mbi-dimensions",
  seed = 42L,
  expected_source_n = as.integer(
    Sys.getenv("MALE_SLEEP_EXPECTED_N", unset = "64114")
  ),
  male_code = 1,
  psqi_poor_cutoff = 7,
  canonical_xlsx = Sys.getenv(
    "MALE_SLEEP_XLSX",
    unset = file.path(path.expand("~"), DEFAULT_DATA_DIRECTORY, "data.xlsx")
  ),
  data_path = Sys.getenv(
    "MALE_SLEEP_DATA_PATH",
    unset = file.path(path.expand("~"), DEFAULT_DATA_DIRECTORY, "data_text.rds")
  ),
  output_base = Sys.getenv("MALE_SLEEP_OUTPUT_BASE", unset = "outputs"),
  variables = list(
    id = "id",
    sex = "A_q2",
    psqi_components = c(
      quality = "D_shuimianzhiliang_A",
      latency = "D_rushuishijian_B",
      duration = "D_shuimianshijian_C",
      efficiency = "D_shuimianxiaoliv_D",
      disturbance = "D_shuimianzhangai_E",
      hypnotics = "D_cuimianyaowu_F",
      daytime_dysfunction = "D_rijiangongnengzhangai_G"
    ),
    psqi_total = "D_PSQI_ALL",
    psqi_poor = "D_PSQI_yichang",
    burnout_items = paste0("F_q3_", 1:22),
    burnout_total = "F_zhiyejuandai_all",
    turnover_items = c(
      "C_q47_1", "C_q47_2", "C_q47_3",
      "C_q48_1", "C_q48_2", "C_q49_1"
    ),
    turnover_total = "C_lizhiyiyuan_all",
    covariates = c(
      age = "A_age",
      bmi = "A_BMI",
      education = "A_q5",
      marital = "A_q6",
      department = "A_q9",
      title = "A_q11",
      management = "A_q12",
      smoking = "A_q24",
      drinking = "A_q30",
      chronic = "A_q19_1_1",
      shift = "C_q1",
      overtime = "C_q42"
    )
  )
)
