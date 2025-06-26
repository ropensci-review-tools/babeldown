test_that("deepl_translate() works well", {
  with_mock_dir("example", {
    to_translate <- system.file("example.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less"
    )

    expect_true(file.exists(out_path))
    translated_lines <- readLines(out_path)
    expect_true(any(grepl("paquete", translated_lines)))
  })
})

test_that("deepl_translate() errors well", {
  expect_snapshot(
    deepl_translate("non-existing-file"),
    error = TRUE
  )

  file <- withr::local_tempfile()
  file.create(file)
  out_file <- withr::local_tempfile()
  file.create(out_file)
  with_mock_dir("get-glossary-ok", {
    expect_snapshot(
      deepl_translate(
        file,
        out_path = out_file,
        glossary_name = "non-existing-glossary",
        source_lang = "en",
        target_lang = "es"
      ),
      error = TRUE
    )
  })

  expect_snapshot(
    deepl_translate(
      file,
      out_path = out_file,
      formality = "non-existing-formality",
      source_lang = "en",
      target_lang = "es"
    ),
    error = TRUE
  )

  expect_snapshot(
    deepl_translate(
      file,
      out_path = "this/path/does/not/exist",
      source_lang = "en",
      target_lang = "es"
    ),
    error = TRUE
  )
})

test_that("deepl_translate() does not translate Markdown blocks (#16)", {
  with_mock_dir("example-markdown-blocks", {
    to_translate <- system.file("example-markdown-blocks.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less"
    )

    link_not_translated <- any(grepl("very nice", readLines(out_path)))
    expect_true(link_not_translated)
  })
})

test_that("deepl_translate() can translate YAML field (#16)", {
  with_mock_dir("example-yaml", {
    to_translate <- system.file("example-yaml.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = c("title", "description", "slug", "tags")
    )
  })

  lines <- readLines(out_path)
  expect_false(any(grepl("Wonderful", lines)))
  expect_false(any(grepl("Check", lines)))
  expect_false(any(grepl("universe", lines)))
  expect_equal(lines[6], "- paquete")
  expect_equal(lines[8], "draft: true")
  expect_equal(lines[9], "cool: false")
  expect_equal(lines[11], "- bla.html")
})
test_that("deepl_translate() can skip translation of YAML field (#16)", {
  with_mock_dir("example-yaml", {
    to_translate <- system.file("example-yaml.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )

    lines <- readLines(out_path)
    expect_true(any(grepl("Wonderful", lines)))
    expect_true(any(grepl("Check", lines)))
    expect_true(any(grepl("universe", lines)))
  })
})
test_that("deepl_translate() does not break TOML", {
  with_mock_dir("example-toml", {
    to_translate <- system.file("example-toml.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )

    lines <- readLines(out_path)
    expect_snapshot(lines[1:3])
  })
})

test_that("deepl_translate() protects+translate Hugo shortcodes", {

  to_translate <- system.file("example-shortcode.md", package = "babeldown")
  out_path <- withr::local_tempfile()
  with_mock_dir("example-shortcode", {
    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )
  })
  lines <- readLines(out_path)
  expect_true(any(grepl('\\{\\{< figure src="blop', lines)))
  expect_false(any(grepl("crying", lines)))
  expect_false(any(grepl("bad", lines)))
  expect_false(any(grepl("someone", lines)))

})

test_that("deepl_translate() protects+translate summary blocks", {
  with_mock_dir("example-summaryblock", {
    to_translate <- system.file("example-summary-blocks.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )

    lines <- readLines(out_path)
    expect_true(any(grepl('resumen', lines)))
  })
})


test_that("deepl_translate() doesn't remove backslashes", {
  with_mock_dir("example-encoding", {
    to_translate <- system.file("example-encoding.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )

    lines <- readLines(out_path)
    expect_no_match(lines, " U00B7")
  })
})

test_that("deepl_translate() translate fig-alt", {
  with_mock_dir("example-alt", {
    to_translate <- system.file("example-alt.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )

    lines <- readLines(out_path)
    expect_no_match(lines, "circle")
  })
})

test_that("deepl_translate() handles square brackets stuff well", {
  with_mock_dir("example-square", {
    lines <- deepl_translate_markdown_string(
      "Wickham says blah [-@wickham+2015#] and he says other things[^footnote1].
      Blah Blah [see @knuth1984, pp. 33-35; also @wickham2015, chap. 1]",
      source_lang = "en",
      target_lang = "es"
    )
    expect_match(lines, "\\[\\^footnote1\\]")
    expect_match(lines, "\\[-@wickham\\+2015\\#]")
    expect_match(lines, "también")
  })
  with_mock_dir("example-square2", {
    lines <- deepl_translate_markdown_string(
      "Wickham says blah [@wickham24].",
      source_lang = "en",
      target_lang = "es"
    )
    expect_match(lines, "\\[@wickham24]")
  })
})

test_that("deepl_translate() handles equations well", {
  to_translate <- system.file("example-equations.md", package = "babeldown")
  out_path <- withr::local_tempfile()
  with_mock_dir("example-equations", {
    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )
  })
  math_lines <- brio::read_lines(out_path)
  expect_snapshot(math_lines[4])
  expect_snapshot(sub(".*que ", "", math_lines[7]))
  expect_snapshot(math_lines[9])
})

test_that("deepl_translate() handles equations+footnote well", {
  to_translate <- system.file("example-equations-footnote.md", package = "babeldown")
  out_path <- withr::local_tempfile()
  with_mock_dir("example-equations-footnote", {
    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "FR",
      target_lang = "EN-US",
      yaml_fields = NULL
    )
  })
  foot_math_lines <- brio::read_lines(out_path)
  expect_snapshot(foot_math_lines)
})


test_that("deepl_translate() handles equations with curly well", {
  to_translate <- system.file("example-equations-curly.md", package = "babeldown")
  out_path <- withr::local_tempfile()
  with_mock_dir("example-equations-curly", {
    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "FR",
      target_lang = "EN-US",
      yaml_fields = NULL
    )
  })
  foot_curly_lines <- brio::read_lines(out_path)
  expect_snapshot(foot_curly_lines[5])
})


test_that("deepl_translate() protects fenced divs", {
  with_mock_dir("example-fences", {
    to_translate <- system.file("example-fences.md", package = "babeldown")
    out_path <- withr::local_tempfile()

    deepl_translate(
      path = to_translate,
      out_path = out_path,
      source_lang = "EN",
      target_lang = "ES",
      formality = "less",
      yaml_fields = NULL
    )

    lines <- readLines(out_path)
    expect_true(any(grepl('footer', lines)))
    expect_snapshot(brio::read_lines(out_path))
  })
})


