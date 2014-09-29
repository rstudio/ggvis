context("specs")

specs <- dir("../specs", pattern = "\\.[rR]$", full.names = TRUE)
for (spec in specs) {
  suppressMessages(
   sys.source(spec, envir = new.env(), chdir = TRUE)
  )
}
