# 手术室护士血液/体液接触分析：堡垒机运行说明

## 研究范围

- 人群：`A_q9 == 7` 的手术室护士。
- 主分析：普通临床护士 `A_q12 == 1`。
- 暴露：`E_q20`，`1=曾有职业性血液/体液接触`，`2=无接触`。
- 主要结局：MBI 情感衰竭连续得分；高情感衰竭为配套二分类结局。
- 设计：多中心横断面基线分析。只能解释为差异或关联，不能作因果解释。
- GitHub 仅保存代码和说明，不包含真实数据、量表文件或分析结果。

## 方法一：RStudio 中已经有名为 `data` 的数据框

确认 `data` 是 `data.frame` 或 `data.table`，然后运行：

```r
Sys.setenv(
  OR_DATA_OBJECT = "data",
  OR_DICTIONARY_FILE = "/secure/path/量表.xlsx",  # 推荐；没有时可删除本行
  OR_OUTPUT_DIR = "/secure/output/OR_blood_fluid_results",
  OR_EXPECTED_ROWS = "64114",
  OR_INSTALL_PACKAGES = "0"
)

script_url <- paste0(
  "https://raw.githubusercontent.com/QLYY820/R-/refs/heads/",
  "agent/or-blood-fluid-bastion-analysis/",
  "OR_blood_fluid_exposure/OR_blood_fluid_full_statistics.R"
)
source(url(script_url, encoding = "UTF-8"), encoding = "UTF-8")
```

脚本先复制 `data`，不会修改全局环境中的原对象。

## 方法二：直接读取堡垒机上的 data.xlsx

大型 XLSX 不能用 `readxl` 整表载入。脚本会调用 Python 3 标准库，以只读方式流式提取 134 个预先锁定的变量，再运行 R 分析：

```r
Sys.setenv(
  OR_DATA_FILE = "/secure/path/data.xlsx",
  OR_DICTIONARY_FILE = "/secure/path/量表.xlsx",
  OR_OUTPUT_DIR = "/secure/output/OR_blood_fluid_results",
  OR_EXPECTED_ROWS = "64114",
  OR_XLSX_SHEET = "1",
  OR_PYTHON = "/usr/bin/python3",  # Windows 可改为 python.exe 的完整路径
  OR_INSTALL_PACKAGES = "0"
)

script_url <- paste0(
  "https://raw.githubusercontent.com/QLYY820/R-/refs/heads/",
  "agent/or-blood-fluid-bastion-analysis/",
  "OR_blood_fluid_exposure/OR_blood_fluid_full_statistics.R"
)
source(url(script_url, encoding = "UTF-8"), encoding = "UTF-8")
```

若堡垒机没有 Python 3，请让数据管理员将真实数据加载为 R 对象 `data`，或导出为 CSV/RDS 后设置 `OR_DATA_FILE`。同规模 64,114 行 XLSX 的本机端到端测试约需 5 分钟，实际时间取决于堡垒机磁盘性能。

## 方法三：克隆代码后运行

```bash
git clone --branch agent/or-blood-fluid-bastion-analysis --single-branch https://github.com/QLYY820/R-.git
```

R 命令行示例：

```bash
Rscript R-/OR_blood_fluid_exposure/OR_blood_fluid_full_statistics.R \
  /secure/path/data.xlsx \
  /secure/path/量表.xlsx \
  /secure/output/OR_blood_fluid_results
```

## R 包

需要：`data.table`、`psych`、`sandwich`、`lmtest`、`survey`、`car`、`quantreg`、`MatchIt`、`ggplot2`、`readxl`。

如果堡垒机允许访问 CRAN，可在第一次运行前设置：

```r
Sys.setenv(OR_INSTALL_PACKAGES = "1")
```

否则请由管理员预装上述包。脚本不会静默安装软件包。

## 输出与核查

- 所有文件只写入 `OR_OUTPUT_DIR`；不会覆盖、排序保存或删除原始数据。
- 预计生成 31 个 CSV、2 个图形、1 个 Markdown 摘要和 `sessionInfo`。
- `22_cohort_arithmetic_check.csv` 核对源数据行数、研究人群和暴露分组人数。
- `29_extended_continuous_results.csv` 为核心调整连续结局。
- `30_extended_binary_results.csv` 为核心调整二分类结局。
- `41_overlap_weighted_continuous_extended.csv` 和 `42_overlap_weighted_binary_extended.csv` 为重叠加权结果。
- `45_primary_sequential_continuous_corrected.csv` 和 `46_primary_sequential_binary_corrected.csv` 显示加入排班及共暴露后的差异衰减。
- `complete_statistics_summary.md` 为运行后自动生成的简要结果。

真实数据运行后的文件才是最终结果，不得把开发环境中的数值直接写入论文。
