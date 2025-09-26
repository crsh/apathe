test_that(
  "Knit bare skeletons"
  , {
    skip_on_cran()

    # Render skeleton
    bare_path <- file.path(tempdir(), "bare_skeleton.Rmd")

    rmarkdown::draft(
      bare_path
      , system.file(
        "rmarkdown", "templates", "uoc-psych-homework"
        , package = "apathe"
      )
      , create_dir = FALSE
      , edit = FALSE
    )

    try( rmarkdown::render(bare_path, quiet = TRUE) )
    expect_true(file.exists(gsub("\\.Rmd", ".pdf", bare_path)))

    rmarkdown::draft(
      bare_path
      , system.file(
        "rmarkdown", "templates", "uoc-psych-thesis"
        , package = "apathe"
      )
      , create_dir = FALSE
      , edit = FALSE
    )

    try( rmarkdown::render(bare_path, quiet = TRUE) )
    expect_true(file.exists(gsub("\\.Rmd", ".pdf", bare_path)))

  }
)
