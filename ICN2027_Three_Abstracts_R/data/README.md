# Data directory

Do not upload real participant data to GitHub.

For the default setup, place the de-identified scored dataset here as
`scored_data.rds`. Alternatively, copy `config.example.R` to `config.R` and set
`DATA_FILE` to the protected absolute path on the bastion host.

Supported input formats: RDS and CSV. The object must be an R `data.frame` with
the original variable names listed in `required_variables.csv`.
