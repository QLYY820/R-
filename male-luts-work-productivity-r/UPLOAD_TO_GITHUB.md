# Uploading this code to GitHub

Use a **private repository** until the research team has approved public code
release and verified that no restricted metadata are present.

## GitHub CLI

```powershell
gh auth login
git init -b main
git add .gitignore *.R *.Rproj README.md SECURITY.md UPLOAD_TO_GITHUB.md config metadata data/README.md R
git status --short
git commit -m "Add reproducible male LUTS analysis pipeline"
gh repo create male-luts-work-productivity-r --private --source . --remote origin --push
```

Do not use `git add -A` until `git status --short` confirms that no source data
or analysis outputs are present.

## GitHub web interface

Alternatively, upload the prepared ZIP or repository files to a new private
repository. Do not upload the `data` contents or any generated output folders.
