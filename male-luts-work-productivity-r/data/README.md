# Data directory

Do **not** upload source data to GitHub.

For file-based analysis, place the real export here as `data.csv`, or change
`CFG$data_file` in `config/config.R`. The production pipeline expects exactly
64,114 source records and stops if the count differs.

For an RStudio in-memory analysis, import the real data as an object named
`data` in the Global Environment. The pipeline then analyzes that object
without requiring a CSV copy.
