# Create a table with four columns of values
tbl <-
  data.frame(
    col_1 = c(767.6, 403.3, 686.4, 662.6, 198.5, 132.1, 349.7, 63.7, 105.4, 924.2),
    col_2 = c(928.1, 461.5, 54.1, 148.8, 65.1, 118.1, 307.1, 504.3, 729.8, 424.6),
    col_3 = c(382, 15.1, 282.7, 984.6, 127.4, 91.2, 566.7, 152, 962.4, 740.8),
    col_4 = c(674.5, 242.8, 56.3, 928.1, 219.3, 874.3, 542.9, 724.5, 336.4, 104.2)
  )

# Function to skip tests if Suggested packages not available on system
check_suggests <- function() {
  skip_if_not_installed("rvest")
}

test_that("cols_label() works correctly", {

  # Check that specific suggested packages are available
  check_suggests()

  # Create a `tbl_html` object with `gt()` and label all
  # of the columns
  tbl_html <-
    gt(tbl) %>%
    cols_label(
      col_1 = "col_a",
      col_2 = "col_b",
      col_3 = "col_c",
      col_4 = "col_d"
    )

  # Expect that the values for the column labels are set
  # correctly in `col_labels`
  expect_equal(
    unlist(tbl_html$`_boxhead`$column_label),
    c("col_a", "col_b", "col_c", "col_d")
  )

  # Expect that the column labels are set
  tbl_html %>%
    render_as_html() %>%
    xml2::read_html() %>%
    selection_text("[class='gt_col_heading gt_columns_bottom_border gt_right']") %>%
    expect_equal(c("col_a", "col_b", "col_c", "col_d"))
})

test_that("cols_labels() doesn't change anything if empty.", {

  check_suggests()
  # Create a `tbl_html` object with `gt()` and label none
  # of the columns
  tbl_html <-
    gt(tbl) %>%
    cols_label()

  # Expect the original column names for `tbl` as values for
  # the column keys and for the column labels
  expect_equal(
    unlist(tbl_html$`_boxhead`$var),
    colnames(tbl)
  )
  expect_equal(
    unlist(tbl_html$`_boxhead`$column_label),
    colnames(tbl)
  )

  # Expect that the column labels are set as the column names
  tbl_html %>%
    render_as_html() %>%
    xml2::read_html() %>%
    selection_text("[class='gt_col_heading gt_columns_bottom_border gt_right']") %>%
    expect_equal(c("col_1", "col_2", "col_3", "col_4"))
})

test_that("cols_label() works with `.list`", {

  check_suggests()
  # Create a `tbl_html` object with `gt()` and label all
  # of the columns using a named list passed to `.list`
  tbl_html <-
    gt(tbl) %>%
    cols_label(
      .list = list(
        col_1 = "col_a",
        col_2 = "col_b",
        col_3 = "col_c",
        col_4 = "col_d"
      )
    )

  # Expect that the values for the column labels are set
  # correctly in `col_labels`
  expect_equal(
    unlist(tbl_html$`_boxhead`$column_label),
    c("col_a", "col_b", "col_c", "col_d")
  )

  # Expect that the column labels are set
  tbl_html %>%
    render_as_html() %>%
    xml2::read_html() %>%
    selection_text("[class='gt_col_heading gt_columns_bottom_border gt_right']") %>%
    expect_equal(c("col_a", "col_b", "col_c", "col_d"))
})

test_that("cols_label() errors with bad input", {

  # Expect an error if any names are missing
  expect_error(gt(tbl) %>% cols_label("col_a"))

  # Expect an error if any columns are not part of the original dataset
  expect_error(gt(tbl) %>% cols_label(col_a = "col_1"))

})

test_that("cols_label() deals well with partial matching", {
  # Expect no partial matching issues with column names and arguments
  expect_no_error(
    dplyr::tribble(
      ~a , ~d,
      1, 4,
      5, 8
    ) %>%
      gt() %>%
      cols_label(
        a = "label a",
        d = "label d"
      )
  )
  expect_no_error(
    dplyr::tribble(
      ~a , ~dat,
      1, 4,
      5, 8
    ) %>%
      gt() %>%
      cols_label(
        a = "label a",
        dat = "label dat"
      )
  )

  # Do expect an error in the unlikely case that a column
  # name is close enough to `.data`
  expect_error(
    dplyr::tribble(
      ~a , ~.dat,
      1, 4,
      5, 8
    ) %>%
      gt() %>%
      cols_label(
        a = "label a",
        .dat = "label dat"
      )
  )
})
