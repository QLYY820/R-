# 男护士睡眠质量、职业倦怠三维度与离职意愿分析

本项目用于在堡垒机真实数据上复现以下横断面分析：

- 暴露：PSQI总分，主效应按每增加3分报告；
- 结局：情感衰竭、去人格化、个人成就感、离职意愿；
- 主模型：多变量线性回归和HC3稳健标准误；
- 多重性：4项共同主要检验采用Holm法校正；
- 非线性：限制性立方样条；
- 敏感性分析：PSQI二分类、无慢性病人群、不调整吸烟、多重插补，以及排除MBI 22项全同作答者。

代码不生成Word文稿，也不包含或上传原始数据。

## 固定数据源

堡垒机唯一原始输入：

```text
/home/sunxuexia/我的数据/data.xlsx
```

为避免早期大量缺失导致稀疏变量被误判为`logical`，`prepare_data.R`将Excel全部列强制按文本导入，并生成：

```text
/home/sunxuexia/我的数据/data_text.rds
```

后续所有统计均读取该RDS。预计源记录数固定为64,114；不一致时流程停止。

## 堡垒机Terminal运行

### 1. 下载独立分支

```bash
cd ~
git clone --branch agent/male-sleep-burnout-turnover-r --single-branch \
  https://github.com/QLYY820/R-.git male_sleep_burnout_turnover
cd ~/male_sleep_burnout_turnover
```

如果目录已经存在，进入该目录后执行：

```bash
git pull --ff-only
```

### 2. 安装统计包并检查环境

```bash
Rscript --vanilla install_packages.R
Rscript --vanilla check_environment.R
```

安装包仅包括统计、读表和制图依赖，不安装`officer`、`flextable`、`ragg`或任何Word生成包。

### 3. 后台运行

```bash
bash run_bastion_nohup.sh
```

RStudio网页或远程桌面关闭后，`nohup`任务通常仍会继续；堡垒机重启、服务器清理进程或管理员终止任务时则不会继续。

### 4. 查看进度

```bash
bash monitor_progress.sh
```

需要持续刷新时：

```bash
tail -f "$(cat logs/LATEST_LOG.txt)"
```

当日志出现以下内容时表示完成：

```text
PIPELINE_SUCCESS
```

## 输出

每次运行建立独立目录，不覆盖旧结果：

```text
outputs/run_YYYYMMDD_HHMMSS/
```

主要内容包括：

- `00_audit/`：ID、性别、计分、缺失和MBI作答质量审计；
- `01_descriptive/`：描述性统计；
- `02_regression/`：主回归和诊断；
- `03_spline/`：限制性立方样条；
- `04_sensitivity/`：多重插补及其他敏感性分析；
- `05_tables/`：投稿表格和`Main_Tables.xlsx`；
- `06_figures/`：PNG和PDF图；
- `07_report/`：统计分析报告；
- `08_logs/`：日志、结果清单和`sessionInfo()`。

运行结束后自动生成仅含汇总结果的传输压缩包：

```text
transfer/run_YYYYMMDD_HHMMSS_aggregate_results.zip
```

该压缩包排除原始数据、唯一ID、模型RDS对象及`_work`目录，可按既往流程临时上传至GitHub Release并在本地下载，下载确认后删除Release及标签。

## 关键计分规则

- 男性：`A_q2=1`；
- PSQI：7个0～3分成分重新求和，总分0～21；
- 情感衰竭：MBI条目1、2、3、6、8、13、14、16、20，总分0～54；
- 去人格化：条目5、10、11、15、22，总分0～30；
- 个人成就感：条目4、7、9、12、17、18、19、21，总分0～48，得分越高越有利；
- 离职意愿：6项，总分6～24；
- 不构建`EE + DP + (48 - PA)`合成总分；
- MBI 22项全同作答者不从主分析自动删除，仅在敏感性分析中排除。

## 复现性

- 固定随机种子：42；
- 自动保存R和包版本；
- 主分析、样条、多重插补、质量敏感性分析和表图均由`run_all.R`一键生成；
- 无医院编码，因此不拟合医院聚类、随机效应、多水平或医院固定效应模型。
