context("specs")

specs <- dir("../specs", pattern = "\\.[rR]$", full.names = TRUE)
for (spec in specs) {
   sys.source(spec, envir = new.env(), chdir = TRUE)
}
