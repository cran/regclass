  .onAttach <- function(...) {
      packageStartupMessage("Important regclass change from 1.3 to 1.4:\nAll functions that had a . in the name now have an _\nall.correlations -> all_correlations, cor.demo -> cor_demo, etc.\n")
  }
