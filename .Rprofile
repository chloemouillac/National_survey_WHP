source("renv/activate.R")

if (interactive() && Sys.getenv("RSTUDIO") == "") {
  init_path <- file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R")
  source(init_path)
  # Workaround .vsc.attach()
  .First.sys()
}