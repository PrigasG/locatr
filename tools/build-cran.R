# Build locatr from a temporary source copy that excludes .git.
#
# Usage:
#   Rscript tools/build-cran.R
#   Rscript tools/build-cran.R --no-check
#
# This avoids Windows path-copy failures caused by very deep internal Git refs
# such as .git/refs/codex/turn-diffs/*.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script <- normalizePath(sub("^--file=", "", file_arg[[1]]),
                          winslash = "/", mustWork = TRUE)
  root <- normalizePath(file.path(dirname(script), ".."),
                        winslash = "/", mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

trailing <- commandArgs(trailingOnly = TRUE)
run_check <- !"--no-check" %in% trailing

pkg <- read.dcf(file.path(root, "DESCRIPTION"))[1, "Package"]
version <- read.dcf(file.path(root, "DESCRIPTION"))[1, "Version"]

tmp_root <- tempfile("locatr-cran-build-")
dir.create(tmp_root, recursive = TRUE)
src <- file.path(tmp_root, pkg)
dir.create(src, recursive = TRUE)

skip_top <- c(
  ".git", ".Rproj.user", "docs", "pkgdown", "analysis", "locatr.Rcheck",
  "CRAN-SUBMISSION"
)
top <- list.files(root, all.files = TRUE, no.. = TRUE, full.names = FALSE)
top <- top[!top %in% skip_top]
top <- top[!grepl("[.]tar[.]gz$", top)]

message("Copying source to: ", src)
for (item in top) {
  ok <- file.copy(file.path(root, item), src,
                  recursive = TRUE, copy.date = TRUE)
  if (!ok) {
    stop("Could not copy ", item, call. = FALSE)
  }
}

old <- setwd(tmp_root)
on.exit(setwd(old), add = TRUE)

r <- file.path(R.home("bin"), "R")
build_status <- system2(r, c("CMD", "build", shQuote(src)))
if (!identical(build_status, 0L)) {
  stop("R CMD build failed.", call. = FALSE)
}

tarball <- file.path(tmp_root, sprintf("%s_%s.tar.gz", pkg, version))
if (!file.exists(tarball)) {
  stop("Expected tarball was not created: ", tarball, call. = FALSE)
}

out_tarball <- file.path(root, basename(tarball))
invisible(file.copy(tarball, out_tarball, overwrite = TRUE))
message("Built tarball: ", out_tarball)

if (run_check) {
  check_status <- system2(r, c("CMD", "check", "--as-cran", "--no-manual",
                               shQuote(tarball)))
  if (!identical(check_status, 0L)) {
    stop("R CMD check failed. Check directory: ",
         file.path(tmp_root, paste0(pkg, ".Rcheck")), call. = FALSE)
  }
  message("R CMD check passed. Check directory: ",
          file.path(tmp_root, paste0(pkg, ".Rcheck")))
}
