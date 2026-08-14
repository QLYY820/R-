# Data security

This repository must contain code and non-identifying metadata only.

Never commit or upload:

- source nurse-cohort data;
- participant-level RDS/RData/SAV/CSV/XLSX files;
- ID linkage exports;
- generated posterior-probability files or participant-level model objects;
- manuscripts or tables containing unapproved real-data results.

The `.gitignore` excludes common source-data and output formats. Before every
push, run `git status --short` and verify that only code, configuration, and
documentation files are staged.
