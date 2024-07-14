tab <- gt(exibble, rowname_col = "row", groupname_col = "group")

test_that("tab_header() works with `md()`/`html()`", {

  # Expect the rendered title and subtitle to be
  # exactly as provided
  expect_match_html(
    tab_header(tab, title = "Title", subtitle = "Subtitle"),
    c(">Title<", ">Subtitle<")
  )

  # Expect the rendered title and subtitle to be in
  # HTML (through Markdown formatting)
  expect_match_html(
    tab_header(tab, title = md("**Title**"), subtitle = md("**Subtitle**")),
    c("><strong>Title</strong><", "><strong>Subtitle</strong><")
  )

  # Expect the rendered title and subtitle to be in
  # HTML (through HTML formatting)
  html_title <- html("<strong>Title</strong>")
  html_subtitle <-  html("<strong>Subtitle</strong>")
  expect_match_html(
    tab_header(tab, title = html_title, subtitle = html_subtitle),
    c("><strong>Title</strong>", "<strong>Subtitle</strong><")
  )
})

test_that("tab_spanner() works with `md()`/`html()`", {

  # Expect the rendered spanner label to be
  # exactly as provided
  expect_match_html(
    tab_spanner(tab, label = "date/time", columns = c(date, time, datetime)),
    ">date/time<"
  )

  # Expect the rendered spanner label to be in
  # HTML (through Markdown formatting)
   expect_match_html(
     tab_spanner(tab, label = md("*date*/*time*"), columns = c(date, time, datetime)),
     "><em>date</em>/<em>time</em><"
  )

  # Expect the rendered spanner label to be in
  # HTML (through HTML formatting)
   expect_match_html(
     tab_spanner(tab, label = html("<em>date</em>/<em>time</em>"), columns = c(date, time, datetime)),
     "><em>date</em>/<em>time</em><"
   )
})
test_that("tab_source_note() works with `md()`/`html()`", {

  # Expect the rendered source note to be
  # exactly as provided
  expect_match_html(
    tab_source_note(tab, source_note = "Source Note"),
    "Source Note"
  )

  # Expect the rendered source note to be in
  # HTML (through Markdown formatting)
  expect_match_html(
    tab_source_note(tab, source_note = md("*Source Note*")),
    "<em>Source Note</em>"
  )

  # Expect the rendered source note to be in
  # HTML (through HTML formatting)
  expect_match_html(
    tab_source_note(tab, source_note = html("<em>Source Note</em>")),
    "<em>Source Note</em>"
  )
})
