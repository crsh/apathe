test_that(
  "Knit bare skeletons"
  , {
    skip_on_cran()

    # Render skeleton
    homework_path <- file.path(tempdir(), "homework_skeleton.Rmd")

    rmarkdown::draft(
      homework_path
      , system.file(
        "rmarkdown", "templates", "uoc-psych-homework"
        , package = "apathe"
      )
      , create_dir = FALSE
      , edit = FALSE
    )

    try( rmarkdown::render(homework_path, quiet = TRUE) )
    expect_true(file.exists(gsub("\\.Rmd", ".pdf", homework_path)))

    thesis_path <- file.path(tempdir(), "thesis_skeleton.Rmd")

    rmarkdown::draft(
      thesis_path
      , system.file(
        "rmarkdown", "templates", "uoc-psych-thesis"
        , package = "apathe"
      )
      , create_dir = FALSE
      , edit = FALSE
    )

    try( rmarkdown::render(thesis_path, quiet = TRUE) )
    expect_true(file.exists(gsub("\\.Rmd", ".pdf", thesis_path)))
  }
)
