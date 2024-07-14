#------------------------------------------------------------------------------#
#
#                /$$
#               | $$
#     /$$$$$$  /$$$$$$
#    /$$__  $$|_  $$_/
#   | $$  \ $$  | $$
#   | $$  | $$  | $$ /$$
#   |  $$$$$$$  |  $$$$/
#    \____  $$   \___/
#    /$$  \ $$
#   |  $$$$$$/
#    \______/
#
#  This file is part of the 'rstudio/gt' project.
#
#  Copyright (c) 2018-2024 gt authors
#
#  For full copyright and license information, please look at
#  https://gt.rstudio.com/LICENSE.html
#
#------------------------------------------------------------------------------#


# cols_align() -----------------------------------------------------------------
#' Set the alignment of columns
#'
#' @description
#'
#' The individual alignments of columns (which includes the column labels and
#' all of their data cells) can be modified. We have the option to align text to
#' the `left`, the `center`, and the `right`. In a less explicit manner, we can
#' allow **gt** to automatically choose the alignment of each column based on
#' the data type (with the `auto` option).
#'
#' @param data *The gt table data object*
#'
#'   `obj:<gt_tbl>` // **required**
#'
#'   This is the **gt** table object that is commonly created through use of the
#'   [gt()] function.
#'
#' @param align *Alignment type*
#'
#'   `singl-kw:[auto|left|center|right]` // *default:* `"auto"`
#'
#'   This can be any of `"center"`, `"left"`, or `"right"` for center-, left-,
#'   or right-alignment. Alternatively, the `"auto"` option (the default), will
#'   automatically align values in columns according to the data type (see the
#'   Details section for specifics on which alignments are applied).
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // *default:* `everything()`
#'
#'   The columns for which the alignment should be applied. Can either be a
#'   series of column names provided in `c()`, a vector of column indices, or a
#'   select helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()], [num_range()], and [everything()]). By default this is set to
#'   [everything()] which means that the chosen alignment affects all columns.
#'
#' @return An object of class `gt_tbl`.
#'
#' @details
#'
#' When you create a **gt** table object using [gt()], automatic alignment of
#' column labels and their data cells is performed. By default, left-alignment
#' is applied to columns of class `character`, `Date`, or `POSIXct`;
#' center-alignment is for columns of class `logical`, `factor`, or `list`; and
#' right-alignment is used for the `numeric` and `integer` columns.
#'
#' @section Examples:
#'
#' Let's use [`countrypops`] to create a small **gt** table. We can change the
#' alignment of the `population` column with `cols_align()`. In this example,
#' the label and body cells of `population` will be aligned to the left.
#'
#' ```r
#' countrypops |>
#'   dplyr::select(-contains("code")) |>
#'   dplyr::filter(country_name == "San Marino") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt(rowname_col = "year", groupname_col = "country_name") |>
#'   cols_align(
#'     align = "left",
#'     columns = population
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_align_1.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-1
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
cols_align <- function(
    data,
    align = c("auto", "left", "center", "right"),
    columns = everything()
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # Get the `align` value, this stops the function if there is no match
  align <- rlang::arg_match(align)

  # Get the columns supplied in `columns` as a character vector
  column_names <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data,
      excl_stub = FALSE
    )

  if (align == "auto") {

    # Get the internal data table
    data_tbl <- dt_data_get(data = data)

    # Obtain a vector of column classes for each of the column names
    col_classes <- unlist(lapply(lapply(data_tbl[column_names], class), `[[`, 1))

    # Check whether all values in 'character' columns are
    # predominantly 'number-like' and modify `col_classes` accordingly
    col_classes <-
      determine_which_character_number(
        data_tbl = data_tbl,
        col_classes = col_classes
      )

    # Get a vector of `align` values based on the column classes
    align <-
      unname(
        sapply(
          col_classes, switch,
          "character-numeric" = "right",
          "character" = "left",
          "Date" = "right",
          "POSIXct" = "right",
          "logical" = "center",
          "factor" = "center",
          "list" = "center",
          "numeric" = "right",
          "integer" = "right",
          "center"
        )
      )

  } else {
    align <- rep(align, length(column_names))
  }

  for (i in seq(column_names)) {

    data <-
      dt_boxhead_edit(
        data = data,
        var = column_names[i],
        column_align = align[i]
      )
  }

  data
}

determine_which_character_number <- function(
    data_tbl = data_tbl,
    col_classes = col_classes
) {

  cols_character <- names(col_classes[col_classes == "character"])

  for (col in cols_character) {

    col_vals <- data_tbl[[col]]

    res <- grepl("^[0-9 -/:\\.]*$", col_vals[!is.na(col_vals)])

    if (length(res) > 0 && all(res)) {
      col_classes[names(col_classes) == col] <- "character-numeric"
    }
  }

  col_classes
}

# cols_align_decimal() ---------------------------------------------------------
#' Align all numeric values in a column along the decimal mark
#'
#' @description
#'
#' For numeric columns that contain values with decimal portions, it is
#' sometimes useful to have them lined up along the decimal mark for easier
#' readability. We can do this with `cols_align_decimal()` and provide any
#' number of columns (the function will skip over columns that don't require
#' this type of alignment).
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // *default:* `everything()`
#'
#'   The columns for which decimal alignment should be applied. Can either be a
#'   series of column names provided in `c()`, a vector of column indices, or a
#'   select helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()], [num_range()], and [everything()]). By default this is set to
#'   [everything()] which means that the decimal alignment affects all columns.
#'
#' @param dec_mark *Decimal mark*
#'
#'   `scalar<character>` // *default:* `"."`
#'
#'   The character used as a decimal mark in the numeric values to be aligned.
#'   If a locale value was used when formatting the numeric values then `locale`
#'   is better to use and it will override any value here in `dec_mark`.
#'
#' @param locale *Locale identifier*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional locale identifier that can be used to obtain the type of
#'   decimal mark used in the numeric values to be aligned (according to the
#'   locale's formatting rules). Examples include `"en"` for English (United
#'   States) and `"fr"` for French (France). We can call [info_locales()] for a
#'   useful reference for all of the locales that are supported. A locale ID can
#'   be also set in the initial [gt()] function call (where it would be used
#'   automatically by any function with a `locale` argument) but a
#'   `locale` value provided here will override that global locale.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Let's put together a two-column table to create a **gt** table. The first
#' column `char` just contains letters whereas the second column, `num`, has a
#' collection of numbers and `NA` values. We could format the numbers with
#' [fmt_number()] and elect to drop the trailing zeros past the decimal mark
#' with `drop_trailing_zeros = TRUE`. This can leave formatted numbers that are
#' hard to scan through because the decimal mark isn't fixed horizontally. We
#' could remedy this and align the numbers by the decimal mark with
#' `cols_align_decimal()`.
#'
#' ```r
#' dplyr::tibble(
#'   char = LETTERS[1:9],
#'   num = c(1.2, -33.52, 9023.2, -283.527, NA, 0.401, -123.1, NA, 41)
#' ) |>
#'   gt() |>
#'   fmt_number(
#'     columns = num,
#'     decimals = 3,
#'     drop_trailing_zeros = TRUE
#'   ) |>
#'   cols_align_decimal()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_align_decimal_1.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-2
#'
#' @section Function Introduced:
#' `v0.8.0` (November 16, 2022)
#'
#' @export
cols_align_decimal <- function(
    data,
    columns = everything(),
    dec_mark = ".",
    locale = NULL
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # Resolve the `locale` value here with the global locale value
  locale <- resolve_locale(data = data, locale = locale)

  # Obtain the decimal mark if a locale ID is provided
  dec_mark <- get_locale_dec_mark(locale, dec_mark)

  # Get the columns supplied in `columns` as a character vector
  resolved <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data,
      excl_stub = FALSE
    )

  # Only numeric columns should be transformed through
  # `cols_align_decimal()` so `column_names` should be filtered
  # to those types of columns
  table_data <- dt_data_get(data = data)
  table_data <- dplyr::select(table_data, dplyr::all_of(resolved))

  cols_are_numeric <-
    vapply(
      table_data,
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE,
      FUN = function(x) inherits(x, "numeric") || inherits(x, "integer")
    )

  # Subset columns to those that are numeric in the input table data
  columns <- colnames(table_data)[cols_are_numeric]

  # If the subsetting of columns finally results in no columns, return
  # the data unchanged
  if (length(columns) < 1) {
    return(data)
  }

  # Ensure that right alignment is set for all columns undergoing
  # the decimal alignment transformation
  data <- cols_align(data = data, columns = columns, align = "right")

  # Pass `data`, `columns`, `rows`, and the formatting
  # functions (as a function list) to `subst()`
  text_transform(
    data = data,
    locations = cells_body(
      columns = columns,
      rows = everything()
    ),
    fn = function(x) {
      align_to_char(x, align_at = dec_mark)
    }
  )
}

# Alignment helpers ------------------------------------------------
align_to_char <- function(x, align_at = ".") {

  na_x_vals <- grepl("^NA$", x)
  no_a_char <- !grepl(align_at, x, fixed = TRUE) & !grepl("[0-9]", x)
  has_t_dec <- grepl("[0-9]\\.$", x)

  x_no_align <- na_x_vals | no_a_char

  x_str <- as.character(x)

  split_x <- strsplit(x[!x_no_align], align_at, fixed = TRUE)

  x_lhs <-
    unlist(
      lapply(
        split_x,
        FUN = function(x) x[1]
      )
    )

  x_rhs <-
    unlist(
      lapply(
        split_x,
        FUN = function(x) paste0(x[-1], collapse = align_at)
      )
    )

  x_piece_lhs <-
    paste0(
      strrep("\U02007", max(nchar(x_lhs)) - nchar(x_lhs)),
      x_lhs
    )

  x_piece_rhs <-
    paste0(
      x_rhs,
      strrep("\U02007", max(nchar(x_rhs)) - nchar(x_rhs))
    )

  for (i in seq_along(x_piece_lhs)) {

    if (grepl("[^0-9]$", x_piece_lhs[i])) {

      extracted <- str_single_extract(x_piece_lhs[i], "[^0-9]+$")

      n_char_extracted <- nchar(extracted)

      x_piece_lhs[i] <- gsub(extracted, "", x_piece_lhs[i], fixed = TRUE)

      x_piece_rhs[i] <- paste0(extracted, x_piece_rhs[i])

      x_piece_rhs[i] <-
        gsub(
          paste0(paste(rep("\U02007", n_char_extracted), collapse = ""), "$"),
          "",
          x_piece_rhs[i]
        )
    }
  }

  x_align <- paste(x_piece_lhs, x_piece_rhs, sep = align_at)

  x_align_parens <- grepl("\\(.+?\\)", x_align)

  if (grepl(align_at, paste(x[!x_no_align], collapse = "|"), fixed = TRUE)) {

    x_align[!nchar(x_rhs) > 0 & !grepl(align_at, x[!x_no_align], fixed = TRUE)] <-
      sub(align_at, " ", x_align[!nchar(x_rhs) > 0], fixed = TRUE)

    x_align[x_align_parens] <- paste0(x_align[x_align_parens], "\U000A0")

  } else {

    x_align[!nchar(x_rhs) > 0 & !grepl(align_at, x[!x_no_align], fixed = TRUE)] <-
      sub(align_at, "", x_align[!nchar(x_rhs) > 0], fixed = TRUE)

    x_align[!x_align_parens] <- paste0(x_align[!x_align_parens], "\U000A0")
  }

  x_str[!x_no_align] <- x_align

  x_str
}
