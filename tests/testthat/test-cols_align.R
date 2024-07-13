# Create a data frame based on the internal `sp500.csv`
sp500 <-
  read.csv(
    system.file("extdata", "sp500.csv", package = "gt"),
    stringsAsFactors = FALSE
  )

# Function to skip tests if Suggested packages not available on system
check_suggests <- function() {
  skip_if_not_installed("rvest")
}

test_that("cols_align() works correctly", {

  # Check that specific suggested packages are available
  check_suggests()

  # Create a `tbl_html` object with `gt()`; the `mpg`,
  # `cyl`, and `drat` columns are aligned left
  tbl_html <-
    gt(mtcars_short) %>%
    cols_align(align = "left", columns = c(mpg, cyl, drat)) %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect that the columns with class `col_heading left`
  # are those columns that were aligned left
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_left']") %>%
    rvest::html_text() %>%
    expect_equal(c("mpg", "cyl", "drat"))

  # Expect that all other columns are center-aligned
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_right']") %>%
    rvest::html_text() %>%
    expect_equal(base::setdiff(colnames(mtcars_short), c("mpg", "cyl", "drat")))

  # Create a `tbl_html` object with `gt()`; columns `1` (`mpg`),
  # `2` (`cyl`), and `3` (`disp`) are aligned left
  tbl_html <-
    gt(mtcars_short) %>%
    cols_align(align = "left", columns = 1:3) %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect that the columns with class `gt_col_heading gt_left`
  # are those columns that were aligned left
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_left']") %>%
    rvest::html_text() %>%
    expect_equal(c("mpg", "cyl", "disp"))

  # Expect that all other columns are right-aligned
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_right']") %>%
    rvest::html_text() %>%
    expect_equal(base::setdiff(colnames(mtcars_short), c("mpg", "cyl", "disp")))

  # Expect that supplying an `align` value that is not `left`, `center`,
  # or `right` will result in an error
  expect_error(
    gt(mtcars_short) %>%
      cols_align(align = "righter", columns = c(mpg, cyl, drat)))

  # Expect that supplying a column name that doesn't exist in the
  # table columns will result in an error
  expect_error(
    gt(mtcars_short) %>%
      cols_align(align = "right", columns = car))

  # Expect that supplying any column index that doesn't exist in the
  # table will result in an error
  expect_error(
    gt(mtcars_short) %>%
      cols_align(align = "right", columns = c(1, 20)))

  # Create a `tbl_html` object with `gt()`; align all
  # columns to the left
  tbl_html <-
    gt(mtcars_short) %>%
    cols_align(align = "left") %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect that the columns with class `col_heading left`
  # includes all columns in `mtcars_short`
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_left']") %>%
    rvest::html_text() %>%
    expect_equal(colnames(mtcars_short))

  # Create a `tbl_html` object with `gt()`; align all
  # columns (using `columns = everything()`) to the left
  tbl_html <-
    gt(mtcars_short) %>%
    cols_align(align = "left", columns = everything()) %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect that the columns with class `col_heading left`
  # includes all columns in `mtcars_short`
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_left']") %>%
    rvest::html_text() %>%
    expect_equal(colnames(mtcars_short))

  # Create a `tbl_html` object with the `sp500` data
  # frame and `auto`-align all columns
  tbl_html <-
    gt(sp500) %>%
    cols_align(align = "auto") %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect that the `Date` column is left-formatted because
  # the column is of the `character` class
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_right']") %>%
    rvest::html_text() %>%
    expect_equal(c("Date", "Open", "High", "Low", "Close", "Volume"))
})

test_that("cols_align() sets the problem alignment in the stub.", {

  # Check that specific suggested packages are available
  check_suggests()

  # Create a new tibble for testing
  tbl <-
    data.frame(
      stub = c("*one*", "**two**", "three"),
      vals = 3:1,
      stringsAsFactors = FALSE

    )

  # Create a `tbl_html` object with `gt()`; don't align any columns
  # with `cols_align()`
  tbl_html <-
    gt(tbl, rowname_col = "stub") %>%
    tab_stubhead(label = "stub") %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect the stub is left aligned (because of the text content) and that the
  # other column is right aligned (because of the numerical content
  tbl_html %>%
    rvest::html_nodes("[class='gt_row gt_left gt_stub']") %>%
    rvest::html_text() %>%
    expect_equal(c("*one*", "**two**", "three"))
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_right']") %>%
    rvest::html_text() %>%
    expect_equal("vals")

  # Create a `tbl_html` object with `gt()`; center align all columns
  tbl_html <-
    gt(tbl, rowname_col = "stub") %>%
    tab_stubhead(label = "stub") %>%
    cols_align(align = "center") %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect both columns are center aligned
  tbl_html %>%
    rvest::html_nodes("[class='gt_row gt_center gt_stub']") %>%
    rvest::html_text() %>%
    expect_equal(c("*one*", "**two**", "three"))
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_center']") %>%
    rvest::html_text() %>%
    expect_equal("vals")

  # Create a `tbl_html` object with `gt()`; center align the stub column and
  # left align the `vals` column
  tbl_html <-
    gt(tbl, rowname_col = "stub") %>%
    tab_stubhead(label = "stub") %>%
    cols_align(align = "center", columns = stub) %>%
    cols_align(align = "left", columns = vals) %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect the correct alignment for each column
  tbl_html %>%
    rvest::html_nodes("[class='gt_row gt_center gt_stub']") %>%
    rvest::html_text() %>%
    expect_equal(c("*one*", "**two**", "three"))
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_left']") %>%
    rvest::html_text() %>%
    expect_equal(c("stub", "vals"))

  # Expect that the use of `stub()` produces the same result as providing the
  # stub column name
  tbl_html <-
    gt(tbl, rowname_col = "stub") %>%
    tab_stubhead(label = "stub") %>%
    cols_align(align = "center", columns = stub()) %>%
    cols_align(align = "left", columns = vals) %>%
    render_as_html() %>%
    xml2::read_html()

  # Expect the correct alignment for each column
  tbl_html %>%
    rvest::html_nodes("[class='gt_row gt_center gt_stub']") %>%
    rvest::html_text() %>%
    expect_equal(c("*one*", "**two**", "three"))
  tbl_html %>%
    rvest::html_nodes("[class='gt_col_heading gt_columns_bottom_border gt_left']") %>%
    rvest::html_text() %>%
    expect_equal(c("stub", "vals"))

  # Expect an error if using `stub()` when there is no stub
  expect_error(
    gt(tbl) %>%
    cols_align(align = "center", columns = stub())
  )
})

test_that("Decimal alignment works in the basic case", {

  small_tbl <-
    dplyr::tibble(
      char = LETTERS[1:9],
      num = c(1.2, -33.52, 9023.2, -283.527, NA, 0.401, -123.1, NA, 41)
    )

  # Use `cols_align_decimal()` on both columns
  gt_tbl_1 <-
    gt(small_tbl) %>%
    fmt_number(
      columns = num,
      decimals = 3,
      drop_trailing_zeros = TRUE
    ) %>%
    cols_align_decimal()

  # Use `cols_align_decimal()` on just the `num` column
  gt_tbl_2 <-
    gt(small_tbl) %>%
    fmt_number(
      columns = num,
      decimals = 3,
      drop_trailing_zeros = TRUE
    ) %>%
    cols_align_decimal(columns = num)

  # Expect that both tables result in the same HTML output
  expect_equal(
    render_as_html(gt_tbl_1),
    render_as_html(gt_tbl_2)
  )

  # Perform snapshot test
  gt_tbl_1 %>% render_as_html() %>% expect_snapshot()

  # Ensure that trailing decimal marks are aligned;
  # use `cols_align_decimal()` on both columns
  gt_tbl_3 <-
    gt(small_tbl) %>%
    fmt_number(
      columns = num,
      decimals = 3,
      drop_trailing_zeros = TRUE,
      drop_trailing_dec_mark = FALSE
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_3 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_percent()` on the `num` column, then align
  # decimal marks
  gt_tbl_4 <-
    gt(small_tbl) %>%
    fmt_percent(
      columns = num,
      decimals = 3,
      scale_values = FALSE,
      drop_trailing_zeros = TRUE
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_4 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_percent()` on the `num` column again, ensuring that
  # trailing decimal marks are not dropped; then align decimal marks
  gt_tbl_5 <-
    gt(small_tbl) %>%
    fmt_percent(
      columns = num,
      decimals = 3,
      scale_values = FALSE,
      drop_trailing_zeros = TRUE,
      drop_trailing_dec_mark = FALSE
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_5 %>% render_as_html() %>% expect_snapshot()


  # Use `fmt_partsper()` on the `num` column, then align
  # decimal marks
  gt_tbl_6 <-
    gt(small_tbl) %>%
    fmt_partsper(
      columns = num,
      decimals = 3,
      scale_values = FALSE,
      drop_trailing_zeros = TRUE
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_6 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_partsper()` on the `num` column again, ensuring that
  # trailing decimal marks are not dropped; then align decimal marks
  gt_tbl_7 <-
    gt(small_tbl) %>%
    fmt_partsper(
      columns = num,
      decimals = 3,
      scale_values = FALSE,
      drop_trailing_zeros = TRUE,
      drop_trailing_dec_mark = FALSE
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_7 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_partsper()` with `ppm` units; then align decimal marks
  gt_tbl_8 <-
    gt(small_tbl) %>%
    fmt_partsper(
      columns = num,
      decimals = 3,
      scale_values = FALSE,
      drop_trailing_zeros = TRUE,
      to_units = "ppm"
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_8 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_partsper()` with `ppm` units, ensuring that
  # trailing decimal marks are not dropped; then align decimal marks
  gt_tbl_9 <-
    gt(small_tbl) %>%
    fmt_partsper(
      columns = num,
      decimals = 3,
      scale_values = FALSE,
      drop_trailing_zeros = TRUE,
      drop_trailing_dec_mark = FALSE,
      to_units = "ppm"
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_9 %>% render_as_html() %>% expect_snapshot()

  # Use `cols_align_decimal()` on a table based on the exibble
  # dataset; expect that `cols_align_decimal()` has no material affect
  # on all of the columns
  gt_tbl_10 <-
    gt(exibble) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_10 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_percent()` and use a non-default `pattern` value;
  # then align decimal marks
  gt_tbl_11 <-
    gt(small_tbl) %>%
    fmt_percent(
      columns = num,
      decimals = 3,
      scale_values = FALSE,
      drop_trailing_zeros = TRUE,
      drop_trailing_dec_mark = FALSE,
      pattern = "abc{x}def"
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_11 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_number()` and use the `accounting` option; then align
  # decimal marks
  gt_tbl_12 <-
    gt(small_tbl) %>%
    fmt_number(
      columns = num,
      decimals = 3,
      drop_trailing_zeros = TRUE,
      drop_trailing_dec_mark = FALSE,
      accounting = TRUE
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_12 %>% render_as_html() %>% expect_snapshot()

  # Use `fmt_currency()` and use the `accounting` option with no decimals; then align
  # based on implied decimal
  gt_tbl_13 <-
    gt(small_tbl) %>%
    fmt_currency(
      columns = num,
      decimals = 0,
      accounting = TRUE
    ) %>%
    cols_align_decimal()

  # Perform snapshot test
  gt_tbl_13 %>% render_as_html() %>% expect_snapshot()
})
