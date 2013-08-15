.onAttach <- function(...) {
  packageStartupMessage("The ggvis API is currently rapidly evolving. ",
    "We strongly recommend that you do not rely on this for production, but ",
    "feel free to explore. If you encounter a clear bug, please file a ",
    "minimal reproducible example at https://github.com/rstudio/ggvis/issues.",
    "For questions and other discussion, please use ", 
    "https://groups.google.com/group/ggvis.")
}
