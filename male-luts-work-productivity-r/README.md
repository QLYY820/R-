# 男性下尿路症状潜在类别与工作生产力损失：R分析代码

本仓库包含TARGET护士队列“男性下尿路症状潜在类别与工作生产力损失”项目的可复现R代码。仓库不包含原始数据、参与者级输出、测试统计结果或LCA缓存。

## 正式数据完整性条件

- 堡垒机原始数据应为 **64,114条记录**。
- 64,114只用于核对是否读入了正确的原始导出文件。
- 男性模块样本量、回归样本量、类别数、P值和效应值全部由代码运行生成，未写死在脚本中。
- 当前没有医院编码，代码不拟合医院聚类、多水平、随机截距或医院固定效应模型。

## 在堡垒机RStudio中运行

### 1. 打开工程并安装依赖

双击 `male-luts-work-productivity.Rproj`，然后运行：

```r
source("install_packages.R", encoding = "UTF-8")
source("check_environment.R", encoding = "UTF-8")
```

如需完全恢复本次开发环境的包版本，可运行：

```r
renv::restore()
```

### 2. 载入真实数据

推荐把真实数据载入RStudio全局环境，并命名为 `data`：

```r
data <- readRDS("真实数据路径/data.rds")
# 或：data <- data.table::fread("真实数据路径/data.csv")
nrow(data)
# 应为 64114
```

也可以把CSV放入 `data/data.csv`。大型XLSX不建议直接用readxl读取；可先在堡垒机导入为R对象，或转换为CSV/RDS。

### 3. 只运行预审计

```r
source("01_preflight_in_RStudio.R", encoding = "UTF-8")
```

该步骤不会拟合LCA。重点检查：

- `outputs/sex_module_linkage_audit.xlsx`
- `outputs/SPS6_scoring_audit.xlsx`
- `00_audit/id_linkage_summary.csv`
- `00_audit/sex_module_crosstab.csv`
- `outputs/analysis_run_log.txt`

### 4. 确认生产开关

根据预审计、原始中文问卷和正式导出记录，修改 `config/config.R`：

```r
unique_id_mapping_confirmed = TRUE
sex_code_confirmed = TRUE
male_module_ownership_confirmed = TRUE
sps6_scoring_confirmed_for_production = TRUE
```

只有确实完成核验后才能设为TRUE。如果A_q2、男性模块、唯一ID或SPS-6计分不一致，production模式会停止。

### 5. 一键运行正式统计

```r
source("02_run_production_in_RStudio.R", encoding = "UTF-8")
```

命令行等价方式：

```powershell
Rscript run_all.R --mode production --data "D:/path/to/data.csv"
```

LCA会拟合1～8类，每类至少200个随机初始值；不稳定模型自动增加到500或1,000个初始值。真实数据运行可能需要几十分钟或更久。

## 统计流程

1. `R/01_audit_scales.R`：64,114条原始记录、唯一ID、重复ID、性别—模块路由、LUTS范围、作答质量和SPS-6双计分方案审计。
2. `R/02_lca.R`：13项0～4级多分类LCA，拟合1～8类；自动重试、类别选择、AvePP、熵、边界概率及完整局部依赖诊断；二分类LCA和ARI敏感性分析。
3. `R/03_association_continuous.R`：1,000次后验概率伪类别抽样、Rubin合并、HC3稳健标准误、调整边际均值、完整敏感性分析；连续LUTS总分、自然样条和维度模型比较。
4. `R/04_tables_figures.R`：生成主表、补充表及PDF/PNG/600 dpi TIFF图。
5. `R/05_documents_and_manifest.R`：自动生成Word工作稿、补充方法和中文审计报告；未提供模板时使用干净Word模板。
6. `R/06_validate.R`：检查脚本语法、必需成果、工作簿、固定结果数字和禁止的医院模型，并生成结果清单。

## 输入变量

所有变量名集中在 `config/config.R`。正式导出列名不同，只修改配置文件，不修改统计脚本。主要变量包括：

- 唯一ID：`id`
- 性别：`A_q2`，配置预期男性编码为1，必须在正式数据中核实
- LUTS：`I_q1`至`I_q37`中的13个男性ICIQ-MLUTS条目
- SPS-6：`D_q21_1`～`D_q21_6`
- 协变量：年龄、BMI、教育、婚姻、科室、职称、管理岗位、吸烟、饮酒、慢性病、夜班和加班
- 敏感性变量：每日液体摄入量

非识别性元数据见 `metadata/variable_dictionary.csv`。

## 输出与GitHub安全

所有数据、结果、文稿、缓存和日志目录均已写入 `.gitignore`。上传前必须阅读 `SECURITY.md` 并执行：

```powershell
git status --short
```

建议先建立私有GitHub仓库。具体步骤见 `UPLOAD_TO_GITHUB.md`。

## 随机种子和复现

固定随机种子为 `20260814`。运行日志、R版本和包版本保存在输出目录。`renv.lock`用于恢复开发环境包版本。
