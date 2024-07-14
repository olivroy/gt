#' Add one or more columns to a **gt** table
#'
#' @description
#'
#' We can add new columns to a table with `cols_add()` and it works quite a bit
#' like `dplyr::mutate()` does. The idea is that you supply name-value pairs
#' where the name is the new column name and the value part describes the data
#' that will go into the column. The latter can: (1) be a vector where the
#' length of the number of rows in the data table, (2) be a single value
#' (which will be repeated all the way down), or (3) involve other columns in
#' the table (as they represent vectors of the correct length). The new columns
#' are added to the end of the column series by default but can instead be added
#' internally by using either the `.before` or `.after` arguments. If entirely
#' empty (i.e., all `NA`) columns need to be added, you can use any of the `NA`
#' types (e.g., `NA`, `NA_character_`, `NA_real_`, etc.) for such columns.
#'
#' @inheritParams fmt_number
#'
#' @param ... *Cell data assignments*
#'
#'   `<multiple expressions>` // (or, use `.list`)
#'
#'   Expressions for the assignment of cell values to the new columns.
#'   Name-value pairs, in the form of `<column> = <value vector>` will work, so
#'   long as any `<column>` value does not already exist in the table. The
#'   `<value vector>` may be an expression that uses one or more column names in
#'   the table to generate a vector of values. Single values in `<value vector>`
#'   will be repeated down the new column. A vector where the length is exactly
#'   the number of rows in the table can also be used.
#'
#' @param .before,.after *Column used as anchor*
#'
#'   `<column-targeting expression>` // *default:* `NULL` (`optional`)
#'
#'   A single column-resolving expression or column index can be given to either
#'   `.before` or `.after`. The column specifies where the new columns should be
#'   positioned among the existing columns in the input data table. While select
#'   helper functions such as [starts_with()] and [ends_with()] can be used for
#'   column targeting, it's recommended that a single column name or index be
#'   used. This is to ensure that exactly one column is provided to either of
#'   these arguments (otherwise, the function will be stopped). If nothing is
#'   provided for either argument then any new column will be placed at the end
#'   of the column series.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Targeting the column for insertion with `.before` or `.after`:
#'
#' The targeting of a column for insertion is done through the `.before` or
#' `.after` arguments (only one of these options should be be used). While
#' **tidyselect**-style expressions or indices can used to target a column, it's
#' advised that a single column name be used. This is to avoid the possibility
#' of inadvertently resolving multiple columns (since the requirement is for a
#' single column).
#'
#' @section Examples:
#'
#' Let's take a subset of the [`exibble`] dataset and make a simple **gt** table
#' with it (using the `row` column for labels in the stub). We'll add a single
#' column to the right of all the existing columns and call it `country`. This
#' new column needs eight values and these will be supplied when using
#' `cols_add()`.
#'
#' ```r
#' exibble |>
#'   dplyr::select(num, char, datetime, currency, group) |>
#'   gt(rowname_col = "row") |>
#'   cols_add(
#'     country = c("TL", "PY", "GL", "PA", "MO", "EE", "CO", "AU")
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_add_1.png")`
#' }}
#'
#' We can add multiple columns with a single use of `cols_add()`. The columns
#' generated can be formatted and otherwise manipulated just as any column could
#' be in a **gt** table. The following example extends the first one by adding
#' more columns and immediately using them in various function calls like
#' [fmt_flag()] and [fmt_units()].
#'
#' ```r
#' exibble |>
#'   dplyr::select(num, char, datetime, currency, group) |>
#'   gt(rowname_col = "row") |>
#'   cols_add(
#'     country = c("TL", "PY", "GL", "PA", "MO", "EE", "CO", "AU"),
#'     empty = NA_character_,
#'     units = c(
#'       "k m s^-2", "N m^-2", "degC", "m^2 kg s^-2",
#'       "m^2 kg s^-3", "/s", "A s", "m^2 kg s^-3 A^-1"
#'     ),
#'     big_num = num ^ 3
#'   ) |>
#'   fmt_flag(columns = country) |>
#'   sub_missing(columns = empty, missing_text = "") |>
#'   fmt_units(columns = units) |>
#'   fmt_scientific(columns = big_num)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_add_2.png")`
#' }}
#'
#' In this table generated from a portion of the [`towny`] dataset, we add two
#' new columns (`land_area` and `density`) through a single use of `cols_add()`.
#' The new `land_area` column is a conversion of land area from square
#' kilometers to square miles and the `density` column is calculated by through
#' division of `population_2021` by that new `land_area` column. We hide the
#' now unneeded `land_area_km2` with [cols_hide()] and also perform some column
#' labeling and adjustments to column widths with [cols_label()] and
#' [cols_width()].
#'
#' ```r
#' towny |>
#'   dplyr::select(name, population_2021, land_area_km2) |>
#'   dplyr::filter(population_2021 > 100000) |>
#'   dplyr::slice_max(population_2021, n = 10) |>
#'   gt() |>
#'   cols_add(
#'     land_area = land_area_km2 / 2.58998811,
#'     density = population_2021 / land_area
#'   ) |>
#'   fmt_integer() |>
#'   cols_hide(columns = land_area_km2) |>
#'   cols_label(
#'     population_2021 = "Population",
#'     density = "Density, {{*persons* / sq mi}}",
#'     land_area ~ "Area, {{mi^2}}"
#'   ) |>
#'   cols_width(everything() ~ px(120))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_add_3.png")`
#' }}
#'
#' It's possible to start with an empty table (i.e., no columns and no rows) and
#' add one or more columns to that. You can, for example, use `dplyr::tibble()`
#' or `data.frame()` to create a completely empty table. The first `cols_add()`
#' call for an empty table can have columns of arbitrary length but subsequent
#' uses of `cols_add()` must adhere to the rule of new columns being the same
#' length as existing.
#'
#' ```r
#' dplyr::tibble() |>
#'   gt() |>
#'   cols_add(
#'     num = 1:5,
#'     chr = vec_fmt_spelled_num(1:5)
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_add_4.png")`
#' }}
#'
#' Tables can contain no rows, yet have columns. In the following example, we'll
#' create a zero-row table with three columns (`num`, `chr`, and `ext`) and
#' perform the same `cols_add()`-based addition of two columns of data. This is
#' another case where the function allows for arbitrary-length columns (since
#' always adding zero-length columns is impractical). Furthermore, here we can
#' reference columns that already exist (`num` and `chr`) and add values to
#' them.
#'
#' ```r
#' dplyr::tibble(
#'   num = numeric(0),
#'   chr = character(0),
#'   ext = character(0)
#' ) |>
#'   gt() |>
#'   cols_add(
#'     num = 1:5,
#'     chr = vec_fmt_spelled_num(1:5)
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_add_5.png")`
#' }}
#'
#' We should note that the `ext` column did not receive any values from
#' `cols_add()` but the table was expanded to having five rows nonetheless. So,
#' each cell of `ext` was by necessity filled with an `NA` value.
#'
#' @family column modification functions
#' @section Function ID:
#' 5-7
#'
#' @section Function Introduced:
#' `v0.10.0` (October 7, 2023)
#'
#' @export
cols_add <- function(
    .data,
    ...,
    .before = NULL,
    .after = NULL
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = .data)

  # Get the table's boxhead
  boxh_df <- dt_boxhead_get(data = .data)

  # Get the internal dataset and a vector of its column names
  data_tbl <- dt_data_get(data = .data)
  data_tbl_columns <- colnames(data_tbl)

  #
  # Special case where data table has no columns (and perhaps no rows); here,
  # we allow for one or more columns to be added with an arbitrary number of
  # rows, however, the number of rows should be consistent across the supplied
  # columns
  #

  if (nrow(data_tbl) < 1 && ncol(data_tbl) < 1) {

    # Generate boxhead rows that correspond to the new columns
    updated_boxh_df <-
      dt_boxhead_get(data = gt(dplyr::as_tibble(as.data.frame(list(...)))))

    # Modify the internal boxhead data frame
    .data <- dt_boxhead_set(data = .data, boxh = updated_boxh_df)

    # Manually add rows to the empty data table (if there are indeed some rows)
    if (nrow(dplyr::as_tibble(as.data.frame(list(...)))) > 0) {

      .data <-
        dt_data_add_rows(
          data = .data,
          row_data_list = list(...),
          before = NULL,
          after = NULL
        )
    }

    # Update the internal data table object
    .data <-
      dt_data_set(
        data = .data,
        data_tbl = dplyr::as_tibble(as.data.frame(list(...)))
      )

    return(.data)
  }

  #
  # Special case where data table has some columns (but no rows); here, we allow
  # for one or more columns to be added with an arbitrary number of rows,
  # however, the number of rows should be consistent across the supplied columns
  #

  if (nrow(data_tbl) < 1 && ncol(data_tbl) > 0) {

    # Generate boxhead rows that correspond to the new columns
    updated_boxh_df <-
      dt_boxhead_get(data = gt(dplyr::as_tibble(as.data.frame(list(...)))))

    updated_boxh_df <-
      vctrs::vec_rbind(
        dt_boxhead_get(data = .data),
        updated_boxh_df[
          !(updated_boxh_df$var %in% dt_boxhead_get(data = .data)[["var"]]),
        ]
      )

    # Modify the internal boxhead data frame
    .data <- dt_boxhead_set(data = .data, boxh = updated_boxh_df)

    # Determine whether the supplied set of values is zero length
    row_data_list_empty <-
      all(
        vapply(
          seq_along(list(...)),
          FUN.VALUE = logical(1),
          USE.NAMES = FALSE,
          FUN = function(x) {
            length(list(...)[[x]]) < 1
          }
        )
      )

    if (row_data_list_empty) {

      # Bind the zero-row tables together
      updated_data_tbl <-
        dplyr::bind_cols(
          dt_data_get(data = .data),
          dplyr::as_tibble(as.data.frame(list(...)))
        )

      # Update the internal data table object
      .data <-
        dt_data_set(
          data = .data,
          data_tbl = updated_data_tbl
        )

      return(.data)
    }

    # Manually add rows to the empty data table (if there are indeed some rows)
    if (nrow(dplyr::as_tibble(as.data.frame(list(...)))) > 0) {

      .data <-
        dt_data_add_rows(
          data = .data,
          row_data_list = list(...),
          before = NULL,
          after = NULL
        )
    }

    return(.data)
  }

  # Mutate the internal data table and get a vector of its column names
  data_tbl_mutated <- dplyr::mutate(data_tbl, ...)
  data_tbl_mutated_columns <- colnames(data_tbl_mutated)

  #
  # If the number of columns in the mutated table is not at least one
  # larger than the non-mutated table then return the data unchanged
  #

  column_count_diff <-
    length(data_tbl_mutated_columns) - length(data_tbl_columns)

  if (column_count_diff < 1) {
    return(.data)
  }

  # Determine which columns are new in the mutated table
  columns_new <- base::setdiff(data_tbl_mutated_columns, data_tbl_columns)

  # Generate a table that has only the new columns
  data_tbl_new_cols <-
    dplyr::select(data_tbl_mutated, dplyr::all_of(columns_new))

  # Generate boxhead rows that correspond to the new columns
  boxh_df_new_cols <- dt_boxhead_get(data = gt(data_tbl_new_cols))

  #
  # Resolve any `.before` or `.after` column and stop function upon
  # observing any inconsistencies
  #

  resolved_column_before <-
    resolve_cols_c(
      expr = {{ .before }},
      data = .data,
      null_means = "nothing"
    )

  if (length(resolved_column_before) < 1) {
    resolved_column_before <- NULL
  }

  if (
    !is.null(resolved_column_before) &&
    length(resolved_column_before) != 1
  ) {

    if (length(resolved_column_before) < 1) {
      cli::cli_abort("The expression used for `.before` did not resolve a column.")
    }

    if (length(resolved_column_before) > 1) {
      cli::cli_abort("The expression used for `.before` resolved multiple columns.")
    }
  }

  resolved_column_after <-
    resolve_cols_c(
      expr = {{ .after }},
      data = .data,
      null_means = "nothing"
    )

  if (length(resolved_column_after) < 1) {
    resolved_column_after <- NULL
  }

  if (
    !is.null(resolved_column_after) &&
    length(resolved_column_after) != 1
  ) {

    if (length(resolved_column_after) < 1) {
      cli::cli_abort("The expression used for `.after` did not resolve a column.")
    }

    if (length(resolved_column_after) > 1) {
      cli::cli_abort("The expression used for `.after` resolved multiple columns.")
    }
  }

  # Stop function if expressions are given to both `.before` and `.after`
  if (!is.null(resolved_column_before) && !is.null(resolved_column_after)) {
    cli::cli_abort("Expressions cannot be given to both `.before` and `.after`.")
  }

  #
  # Prepend, insert, or append the new data columns (`data_tbl_new_cols`)
  # to those existing in `data_tbl`
  #

  # Get the first and last column names from `data_tbl`
  first_colname <- colnames(data_tbl)[1]
  last_colname <- colnames(data_tbl)[ncol(data_tbl)]

  if (is.null(resolved_column_before) && is.null(resolved_column_after)) {

    updated_data_tbl <-
      dplyr::bind_cols(
        data_tbl,
        data_tbl_new_cols
      )

    updated_boxh_df <-
      vctrs::vec_rbind(
        boxh_df,
        boxh_df_new_cols
      )

  } else if (!is.null(resolved_column_before) && is.null(resolved_column_after)) {

    before_colnum <- which(colnames(data_tbl) == resolved_column_before)

    updated_data_tbl <-
      dplyr::bind_cols(
        dplyr::select(data_tbl, 1:(before_colnum - 1)),
        data_tbl_new_cols,
        dplyr::select(data_tbl, before_colnum:ncol(data_tbl))
      )

    before_colnum <- which(boxh_df[["var"]] == resolved_column_before)

    updated_boxh_df <-
      dplyr::bind_rows(
        boxh_df[(1:before_colnum) - 1, ],
        boxh_df_new_cols,
        boxh_df[before_colnum:nrow(boxh_df), ]
      )

  } else if (is.null(resolved_column_before) && !is.null(resolved_column_after)) {

    if (resolved_column_after == nrow(data_tbl)) {

      updated_data_tbl <-
        dplyr::bind_cols(
          data_tbl,
          data_tbl_new_cols
        )

      updated_boxh_df <-
        dplyr::bind_rows(
          boxh_df,
          boxh_df_new_cols
        )

    } else {

      after_colnum <- which(colnames(data_tbl) == resolved_column_after)

      updated_data_tbl <-
        dplyr::bind_cols(
          dplyr::select(data_tbl, 1:after_colnum),
          data_tbl_new_cols,
          dplyr::select(data_tbl, (after_colnum + 1):ncol(data_tbl))
        )

      after_colnum <- which(boxh_df[["var"]] == resolved_column_after)

      updated_boxh_df <-
        dplyr::bind_rows(
          boxh_df[1:after_colnum, ],
          boxh_df_new_cols,
          boxh_df[(after_colnum + 1):nrow(boxh_df), ]
        )
    }
  }

  # Modify the internal datasets
  .data <- dt_data_set(data = .data, data_tbl = updated_data_tbl)
  .data <- dt_boxhead_set(data = .data, boxh = updated_boxh_df)

  .data
}

#' Add a new column of nanoplots, taking input data from selected columns
#'
#' @description
#'
#' Nanoplots are tiny plots you can use in your **gt** table. They are simple by
#' design, mainly because there isn't a lot of space to work with. With that
#' simplicity, however, you do get a set of very succinct data visualizations
#' that adapt nicely to the amount of data you feed into them. With
#' `cols_nanoplot()` you take data from one or more columns as the basic inputs
#' for the nanoplots and generate a new column containing the plots. The
#' nanoplots are robust against missing values, and multiple strategies are
#' available for handling missingness.
#'
#' Nanoplots try to show individual data with reasonably good visibility.
#' Interactivity is included as a basic feature so one can hover over the data
#' points and vertical guides will display the value ascribed to each data
#' point. Because **gt** knows all about numeric formatting, values will be
#' compactly formatted so as to not take up valuable real estate. If you need to
#' create a nanoplot based on monetary values, that can be handled by providing
#' the currency code to the [nanoplot_options()] helper (then hook that up to
#' the `options` argument).  A guide on the left-hand side of the plot area will
#' appear on hover and display the minimal and maximal *y* values.
#'
#' There are three types of nanoplots available: `"line"`, `"bar"`, `"boxplot"`.
#' A line plot shows individual data points and has smooth connecting lines
#' between them to allow for easier scanning of values. You can opt for
#' straight-line connections between data points, or, no connections at all
#' (it's up to you). You can even eschew the data points and just have a simple
#' line. Regardless of how you mix and match difference plot layers, the plot
#' area focuses on the domain of the data points with the goal of showing you
#' the overall trend of the data. The data you feed into a line plot can consist
#' of a single vector of values (resulting in equally-spaced *y* values), or,
#' you can supply two vectors representative of *x* and *y*.
#'
#' A bar plot is built a little bit differently. The focus is on evenly-spaced
#' bars (requiring a single vector of values) that project from a zero line,
#' clearly showing the difference between positive and negative values. By
#' default, any type of nanoplot will have basic interactivity. One can hover
#' over the data points and vertical guides will display values ascribed to
#' each. A guide on the left-hand side of the plot area will display the minimal
#' and maximal *y* values on hover.
#'
#' Every box plot will take the collection of values for a row and construct the
#' plot horizontally. This is essentially a standard box-and-whisker diagram
#' where outliers are automatically displayed outside the left and right fences.
#'
#' While basic customization options are present in the `cols_nanoplot()`, many
#' more opportunities for customizing nanoplots on a more granular level are
#' possible with the [nanoplot_options()] helper function. That function should
#' be invoked at the `options` argument of `cols_nanoplot()`. Through that
#' helper, layers of the nanoplots can be selectively removed and the aesthetics
#' of the remaining plot components can be modified.
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns from which to get data for the dependent variable*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The columns which contain the numeric data to be plotted as nanoplots. Can
#'   either be a series of column names provided in `c()`, a vector of column
#'   indices, or a select helper function (e.g. [starts_with()], [ends_with()],
#'   [contains()], [matches()], [num_range()], and [everything()]). Data
#'   collected from the columns will be concatenated together in the order of
#'   resolution.
#'
#' @param rows *Rows that should contain nanoplots*
#'
#'   `<row-targeting expression>` // *default:* `everything()`
#'
#'   With `rows` we can specify which rows should contain nanoplots in the new
#'   column. The default [everything()] results in all rows in `columns` being
#'   formatted. Alternatively, we can supply a vector of row IDs within `c()`, a
#'   vector of row indices, or a select helper function (e.g. [starts_with()],
#'   [ends_with()], [contains()], [matches()], [num_range()], and [everything()]).
#'   We can also use expressions to filter down to the rows we need(e.g.,
#'   `[colname_1] > 100 & [colname_2] < 50`).
#'
#' @param plot_type *The type of nanoplot to display*
#'
#'   `singl-kw:[line|bar|boxplot]` // *default:* `"line"`
#'
#'   Nanoplots can either take the form of a line plot (using `"line"`), a bar
#'   plot (with `"bar"`), or a box plot (`"boxplot"`). A line plot, by default,
#'   contains layers for a data line, data points, and a data area. Each of
#'   these can be deactivated by using [nanoplot_options()]. With a bar plot,
#'   the always visible layer is that of the data bars. Furthermore, a line plot
#'   can optionally take in *x* values through the `columns_x_vals` argument
#'   whereas bar plots and box plots both ignore any data representing the
#'   independent variable.
#'
#' @param plot_height *The height of the nanoplots*
#'
#'   `scalar<character>` // *default:* `"2em"`
#'
#'   The height of the nanoplots. The default here is a sensible value of
#'   `"2em"`. By way of comparison, this is a far greater height than the
#'   default for icons through [fmt_icon()] (`"1em"`) and is the same height as
#'   images inserted via [fmt_image()] (also having a `"2em"` height default).
#'
#' @param missing_vals *Treatment of missing values*
#'
#'   `singl-kw:[gap|marker|zero|remove]` // *default:* `"gap"`
#'
#'   If missing values are encountered within the input data, there are three
#'   strategies available for their handling: (1) `"gap"` will show data gaps
#'   at the sites of missing data, where data lines will have discontinuities
#'   and bar plots will have missing bars; (2) `"marker"` will behave like
#'   `"gap"` but show prominent visual marks at the missing data locations; (3)
#'   `"zero"` will replace `NA` values with zero values; and (4) `"remove"` will
#'   remove any incoming `NA` values.
#'
#' @param autoscale *Automatically set x- and y-axis scale limits based on data*
#'
#'   `scalar<logical>` // *default:* `FALSE`
#'
#'   Using `autoscale = TRUE` will ensure that the bounds of all nanoplots
#'   produced are based on the limits of data combined from all input rows. This
#'   will result in a shared scale across all of the nanoplots (for *y*- and
#'   *x*-axis data), which is useful in those cases where the nanoplot data
#'   should be compared across rows.
#'
#' @param autohide *Automatically hide the `columns`/`columns_x_vals` column(s)*
#'
#'   `scalar<logical>` // *default:* `TRUE`
#'
#'   An option to automatically hide any columns specified in `columns` and also
#'   `columns_x_vals` (if used). Any columns with their state changed to
#'   'hidden' will behave the same as before, they just won't be displayed in
#'   the finalized table. Should you want to have these 'input' columns be
#'   viewable, set `autohide = FALSE`.
#'
#' @param columns_x_vals *Columns containing values for the optional x variable*
#'
#'   `<column-targeting expression>` // *default:* `NULL` (`optional`)
#'
#'   We can optionally obtain data for the independent variable (i.e., the
#'   *x*-axis data) if specifying columns in `columns_x_vals`. This is only for
#'   the `"line"` type of plot (set via the `plot_type` argument). We can supply
#'   either be a series of column names provided in `c()`, a vector of column
#'   indices, or a select helper function (e.g. [starts_with()], [ends_with()],
#'   [contains()], [matches()], [num_range()], and [everything()]). Data
#'   collected from the columns will be concatenated together in the order of
#'   resolution.
#'
#' @param reference_line *Add a reference line*
#'
#'   `scalar<numeric|integer|character>` // *default:* `NULL` (`optional`)
#'
#'   A reference line requires a single input to define the line. It could be a
#'   static numeric value, applied to all nanoplots generated. Or, the input can
#'   be one of the following for generating the line from the underlying data:
#'   (1) `"mean"`, (2) `"median"`, (3) `"min"`, (4) `"max"`, (5) `"q1"`, (6)
#'   `"q3"`, (7) `"first"`, or (8) `"last"`.
#'
#' @param reference_area *Add a reference area*
#'
#'   `vector<numeric|integer|character>|list` // *default:* `NULL` (`optional`)
#'
#'   A reference area requires two inputs to define bottom and top boundaries
#'   for a rectangular area. The types of values supplied are the same as those
#'   expected for `reference_line`, which is either a static numeric value or
#'   one of the following keywords for the generation of the value: (1)
#'   `"mean"`, (2) `"median"`, (3) `"min"`, (4) `"max"`, (5) `"q1"`, (6) `"q3"`,
#'   (7) `"first"`, or (8) `"last"`. Input can either be a vector or list with
#'   two elements.
#'
#' @param expand_x,expand_y *Expand plot scale in the x and y directions*
#'
#'   `vector<numeric|integer>` // *default:* `NULL` (`optional`)
#'
#'   Should you need to have plots expand in the *x* or *y* direction, provide
#'   one or more values to `expand_x` or `expand_y`. Any values provided that
#'   are outside of the range of data provided to the plot should result in a
#'   scale expansion.
#'
#' @param new_col_name *Column name for the new column containing the plots*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   A single column name in quotation marks. Values will be extracted from this
#'   column and provided to compatible arguments. If not provided the new column
#'   name will be `"nanoplots"`.
#'
#' @param new_col_label *Column label for the new column containing the plots*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   A single column label. If not supplied then the column label will inherit
#'   from `new_col_name` (if nothing provided to that argument, the label will
#'   be `"nanoplots"`).
#'
#' @param before,after *Column used as anchor*
#'
#'   `<column-targeting expression>` // *default:* `NULL` (`optional`)
#'
#'   A single column-resolving expression or column index can be given to either
#'   `before` or `after`. The column specifies where the new column containing
#'   the nanoplots should be positioned among the existing columns in the input
#'   data table. While select helper functions such as [starts_with()] and
#'   [ends_with()] can be used for column targeting, it's recommended that a
#'   single column name or index be used. This is to ensure that exactly one
#'   column is provided to either of these arguments (otherwise, the function
#'   will be stopped). If nothing is provided for either argument then the new
#'   column will be placed at the end of the column series.
#'
#' @param options *Set options for the nanoplots*
#'
#'   `obj:<nanoplot_options` // *default:* `NULL` (`optional`)
#'
#'   By using the [nanoplot_options()] helper function here, you can alter the
#'   layout and styling of the nanoplots in the new column.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Targeting cells with `columns` and `rows`:
#'
#' Targeting of values to insert into the nanoplots is done through `columns`
#' and additionally by `rows` (if nothing is provided for `rows` then entire
#' columns are selected). Aside from declaring column names in `c()` (with bare
#' column names or names in quotes) we can use also
#' **tidyselect**-style expressions. This can be as basic as supplying a select
#' helper like `starts_with()`, or, providing a more complex incantation like
#'
#' `where(~ is.numeric(.x) && max(.x, na.rm = TRUE) > 1E6)`
#'
#' which targets numeric columns that have a maximum value greater than
#' 1,000,000 (excluding any `NA`s from consideration).
#'
#' Once the columns are targeted, we may also target the `rows` within those
#' columns. This can be done in a variety of ways. If a stub is present, then we
#' potentially have row identifiers. Those can be used much like column names in
#' the `columns`-targeting scenario. We can use simpler **tidyselect**-style
#' expressions (the select helpers should work well here) and we can use quoted
#' row identifiers in `c()`. It's also possible to use row indices (e.g.,
#' `c(3, 5, 6)`) though these index values must correspond to the row numbers of
#' the input data (the indices won't necessarily match those of rearranged rows
#' if row groups are present). One more type of expression is possible, an
#' expression that takes column values (can involve any of the available columns
#' in the table) and returns a logical vector.
#'
#' @section How to supply data for nanoplots:
#'
#' The input data for nanoplots naturally needs to be numeric and there are two
#' major ways to formulate that data: (1) from single values across many
#' columns, and (2) using text-based value streams. It's pretty easy to
#' rationalize the first, and we may already have wide data in the input data
#' frame anyway (take a look at the [`illness`] and [`towny`] datasets for
#' examples of this). There's one data value per column so the key thing here is
#' to reference the columns in the correct order. With a select helper, good
#' column naming, and the columns being in the intended order, this is a snap.
#'
#' The second option is to use text-based value streams. Sometimes you simply
#' don't want or don't need multiple columns and so a single column with all of
#' the data might be more practical. To make this work, you'd need to have a set
#' of numerical values separated by some sort of delimiter (could be a comma, a
#' space, a semicolon, you get the idea). Here's an example with three numbers,
#' written three ways: `"3.6 -2.44 1.98"`, `"3.6, -2.44, 1.98"`, and
#' `"3.6;-2.44;1.98"`. You can include `NA` values, not a problem, and here's an
#' example of that: `"6.232 NA 3.7 0.93"`. Another form of value stream involves
#' using datetimes in the ISO 8601 form of `YYYY-MM-DD HH:MM:SS`. These will
#' be internally converted to numeric values (seconds elapsed since
#' `"1970-01-01 00:00:00"`). An example of a datetime-based value stream is:
#' `"2012-06-12 08:24:13, 2012-06-12 10:37:08, 2012-06-12 14:03:24"`.
#'
#' Value streams can be pretty big if you want them to be, and you don't have to
#' deal with containing individual values across multiple columns. For the case
#' where you need to provide two sets of values (*x* and *y*, for line plots
#' with `columns` and `columns_x_vals`), have two equivalently sized value
#' streams in two columns. Value streams can also be concatenated together by
#' referencing columns having their own separate value streams.
#'
#' @section Reference line and reference area:
#'
#' Neither a horizontal *reference line* nor a *reference area* is present in
#' the default view but these can be added by providing valid input values in
#' the `reference_line` and `reference_area` arguments. A reference line can
#' be either be a static numeric value (supply any number to `reference_line`),
#' or it can be a keyword that computes the reference line *y* value using the
#' data values for each nanoplot. The following keywords can be used:
#'
#' 1. `"mean"`: The mean of the data values
#' 2. `"median"`: Median of data values
#' 3. `"min"`: Minimum value in set of data values
#' 4. `"max"`: The maximum value
#' 5. `"q1"`: The first, or lower, quartile of the data values
#' 6. `"q3"`: The third quartile, otherwise known as the upper quartile
#' 7. `"first"`: The first data value
#' 8. `"last"`: The last data value
#'
#' The *reference area* accepts two inputs, and this can be two of the above
#' keywords, a keyword and a static numeric value, or two numeric values.
#'
#' @section Examples:
#'
#' Let's make some nanoplots with the [`illness`] dataset. The columns beginning
#' with 'day' all contain ordered measurement values, comprising seven
#' individual daily results. Using `cols_nanoplot()` we create a new column to
#' hold the nanoplots (with `new_col_name = "nanoplots"`), referencing the
#' columns containing the data (with `columns = starts_with("day")`). It's also
#' possible to define a column label here using the `new_col_label` argument.
#'
#' ```r
#' illness |>
#'   dplyr::slice_head(n = 10) |>
#'   gt(rowname_col = "test") |>
#'   tab_header("Partial summary of daily tests performed on YF patient") |>
#'   tab_stubhead(label = md("**Test**")) |>
#'   cols_hide(columns = starts_with("norm")) |>
#'   fmt_units(columns = units) |>
#'   cols_nanoplot(
#'     columns = starts_with("day"),
#'     new_col_name = "nanoplots",
#'     new_col_label = md("*Progression*")
#'   ) |>
#'   cols_align(align = "center", columns = nanoplots) |>
#'   cols_merge(columns = c(test, units), pattern = "{1} ({2})") |>
#'   tab_footnote(
#'     footnote = "Measurements from Day 3 through to Day 8.",
#'     locations = cells_column_labels(columns = nanoplots)
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_nanoplot_1.png")`
#' }}
#'
#' The previous table showed us some line-based nanoplots. We can also make very
#' small bar plots with `cols_nanoplot()`. Let's take the [`pizzaplace`] dataset
#' and make a small summary table showing daily pizza sales by type (there are
#' four types). This will be limited to the first ten days of pizza sales in
#' 2015, so, there will be ten rows in total. We can use `plot_type = "bar"` to
#' make bar plots from the daily sales counts in the `chicken`, `classic`,
#' `supreme`, and `veggie` columns. Because we know there will always be four
#' bars (one for each type of pizza) we can be a little creative and apply
#' colors to each of the bars through use of the `data_bar_fill_color` argument
#' in [nanoplot_options()].
#'
#' ```r
#' pizzaplace |>
#'   dplyr::select(type, date) |>
#'   dplyr::group_by(date, type) |>
#'   dplyr::summarize(sold = dplyr::n(), .groups = "drop") |>
#'   tidyr::pivot_wider(names_from = type, values_from = sold) |>
#'   dplyr::slice_head(n = 10) |>
#'   gt(rowname_col = "date") |>
#'   tab_header(
#'     title = md("First Ten Days of Pizza Sales in 2015")
#'   ) |>
#'   cols_nanoplot(
#'     columns = c(chicken, classic, supreme, veggie),
#'     plot_type = "bar",
#'     autohide = FALSE,
#'     new_col_name = "pizzas_sold",
#'     new_col_label = "Sales by Type",
#'     options = nanoplot_options(
#'       show_data_line = FALSE,
#'       show_data_area = FALSE,
#'       data_bar_stroke_color = "transparent",
#'       data_bar_fill_color = c("brown", "gold", "purple", "green")
#'     )
#'   ) |>
#'   cols_width(pizzas_sold ~ px(150)) |>
#'   cols_align(columns = -date, align = "center") |>
#'   fmt_date(columns = date, date_style = "yMMMEd") |>
#'   opt_all_caps()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_nanoplot_2.png")`
#' }}
#'
#' Now we'll make another table that contains two columns of nanoplots. Starting
#' from the [`towny`] dataset, we first reduce it down to a subset of columns
#' and rows. All of the columns related to either population or density will be
#' used as input data for the two nanoplots. Both nanoplots will use a reference
#' line that is generated from the median of the input data. And by naming the
#' new nanoplot-laden columns in a similar manner as the input data columns, we
#' can take advantage of select helpers (e.g., when using [tab_spanner()]). Many
#' of the input data columns are now redundant because of the plots, so we'll
#' elect to hide most of those with [cols_hide()].
#'
#' ```r
#' towny |>
#'   dplyr::select(name, starts_with("population"), starts_with("density")) |>
#'   dplyr::filter(population_2021 > 200000) |>
#'   dplyr::arrange(desc(population_2021)) |>
#'   gt() |>
#'   fmt_integer(columns = starts_with("population")) |>
#'   fmt_number(columns = starts_with("density"), decimals = 1) |>
#'   cols_nanoplot(
#'     columns = starts_with("population"),
#'     reference_line = "median",
#'     autohide = FALSE,
#'     new_col_name = "population_plot",
#'     new_col_label = md("*Change*")
#'   ) |>
#'   cols_nanoplot(
#'     columns = starts_with("density"),
#'     plot_type = "bar",
#'     autohide = FALSE,
#'     new_col_name = "density_plot",
#'     new_col_label = md("*Change*")
#'   ) |>
#'   cols_hide(columns = matches("2001|2006|2011|2016")) |>
#'   tab_spanner(
#'     label = "Population",
#'     columns = starts_with("population")
#'   ) |>
#'   tab_spanner(
#'     label = "Density ({{*persons* km^-2}})",
#'     columns = starts_with("density")
#'   ) |>
#'   cols_label_with(
#'     columns = -matches("plot"),
#'     fn = function(x) gsub("[^0-9]+", "", x)
#'   ) |>
#'   cols_align(align = "center", columns = matches("plot")) |>
#'   cols_width(
#'     name ~ px(140),
#'     everything() ~ px(100)
#'   ) |>
#'   opt_horizontal_padding(scale = 2)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_nanoplot_3.png")`
#' }}
#'
#' The [`sza`] dataset can, with just some use of **dplyr** and **tidyr**, give
#' us a wide table full of nanoplottable values. We'll transform the solar
#' zenith angles to solar altitude angles and create a column of nanoplots using
#' the newly calculated values. There are a few `NA` values during periods where
#' the sun hasn't risen (usually before 06:30 in the winter months) and those
#' values will be replaced with `0` using `missing_vals = "zero"`. We'll also
#' elect to create bar plots using the `plot_type = "bar"` option. The height of
#' the plots will be bumped up to `"2.5em"` from the default of `"2em"`.
#' Finally, we will use [nanoplot_options()] to modify the coloring of the data
#' bars.
#'
#' ```r
#' sza |>
#'   dplyr::filter(latitude == 20 & tst <= "1200") |>
#'   dplyr::select(-latitude) |>
#'   dplyr::filter(!is.na(sza)) |>
#'   dplyr::mutate(saa = 90 - sza) |>
#'   dplyr::select(-sza) |>
#'   tidyr::pivot_wider(
#'     names_from = tst,
#'     values_from = saa,
#'     names_sort = TRUE
#'   ) |>
#'   gt(rowname_col = "month") |>
#'   tab_header(
#'     title = "Solar Altitude Angles",
#'     subtitle = "Average values every half hour from 05:30 to 12:00"
#'   ) |>
#'   cols_nanoplot(
#'     columns = matches("0"),
#'     plot_type = "bar",
#'     missing_vals = "zero",
#'     new_col_name = "saa",
#'     plot_height = "2.5em",
#'     options = nanoplot_options(
#'       data_bar_stroke_color = "GoldenRod",
#'       data_bar_fill_color = "DarkOrange"
#'     )
#'   ) |>
#'   tab_options(
#'     table.width = px(400),
#'     column_labels.hidden = TRUE
#'   ) |>
#'   cols_align(
#'     align = "center",
#'     columns = everything()
#'   ) |>
#'   tab_source_note(
#'     source_note = "The solar altitude angle is the complement to
#'     the solar zenith angle. TMYK."
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_nanoplot_4.png")`
#' }}
#'
#' You can use number and time streams as data for nanoplots. Let's demonstrate
#' how we can make use of them with some creative transformation of the
#' [`pizzaplace`] dataset. A value stream is really a string with delimited
#' numeric values, like this: `"7.24,84.2,14"`. A value stream can also contain
#' dates and/or datetimes, and here's an example of that:
#' `"2020-06-02 13:05:13,2020-06-02 14:24:05,2020-06-02 18:51:37"`. Having data
#' in this form can often be more convenient since different nanoplots might
#' have varying amounts of data (and holding different amounts of data in a
#' fixed number of columns is cumbersome). There are `date` and `time` columns
#' in this dataset and we'll use that to get *x* values denoting high-resolution
#' time instants: the second of the day that a pizza was sold (this is true
#' pizza analytics). We also have the sell price for a pizza, and that'll serve
#' as the *y* values. The pizzas belong to four different groups (in the `type`
#' column) and we'll group by that and create value streams with
#' `paste(..., collapse = ",")` inside  the `dplyr::summarize()` call. With two value
#' streams in each row (having the same number of values) we can now make a
#' **gt** table with nanoplots.
#'
#' ```r
#' pizzaplace |>
#'   dplyr::filter(date == "2015-01-01") |>
#'   dplyr::mutate(date_time = paste(date, time)) |>
#'   dplyr::select(type, date_time, price) |>
#'   dplyr::group_by(type) |>
#'   dplyr::summarize(
#'     date_time = paste(date_time, collapse = ","),
#'     sold = paste(price, collapse = ",")
#'   ) |>
#'   gt(rowname_col = "type") |>
#'   tab_header(
#'     title = md("Pizzas sold on **January 1, 2015**"),
#'     subtitle = "Between the opening hours of 11:30 to 22:30"
#'   ) |>
#'   cols_nanoplot(
#'     columns = sold,
#'     columns_x_vals = date_time,
#'     expand_x = c("2015-01-01 11:30", "2015-01-01 22:30"),
#'     reference_line = "median",
#'     new_col_name = "pizzas_sold",
#'     new_col_label = "Pizzas Sold",
#'     options = nanoplot_options(
#'       show_data_line = FALSE,
#'       show_data_area = FALSE,
#'       currency = "USD"
#'     )
#'   ) |>
#'   cols_width(pizzas_sold ~ px(200)) |>
#'   cols_align(columns = pizzas_sold, align = "center") |>
#'   opt_all_caps()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_nanoplot_5.png")`
#' }}
#'
#' Notice that the columns containing the value streams are hid due to the
#' default argument `autohide = TRUE` because, while useful, they don't need to
#' be displayed to anybody viewing a table. Since we have a lot of data points
#' and a connecting line is not as valuable here, we also set
#' `show_data_line = FALSE` in [nanoplot_options()]. It's more interesting to
#' see the clusters of the differently priced pizzas over the entire day.
#' Specifying a `currency` in [nanoplot_options()] is a nice touch since the *y*
#' values are sale prices in U.S. Dollars (hovering over data points gives
#' correctly formatted values). Finally, having a reference line based on the
#' median gives pretty useful information. Seems like customers preferred
#' getting the `"chicken"`-type pizzas in large size!
#'
#' Using the [`gibraltar`] dataset, let's make a series of nanoplots across the
#' meteorological parameters of temperature, humidity, and wind speed. We'll
#' want to customize the appearance of the plots across three columns and we
#' can make this somewhat simpler by assigning a common set of options through
#' [nanoplot_options()]. In this table we want to make comparisons across
#' nanoplots in a particular column easier, so, we'll set `autoscale = TRUE` so
#' that there is a common y-axis scale for each of the parameters (based on the
#' extents of the data).
#'
#' ```r
#' nanoplot_options_list <-
#'   nanoplot_options(
#'     data_point_radius = px(4),
#'     data_point_stroke_width = px(2),
#'     data_point_stroke_color = "black",
#'     data_point_fill_color = "white",
#'     data_line_stroke_width = px(4),
#'     data_line_stroke_color = "gray",
#'     show_data_line = TRUE,
#'     show_data_points = TRUE,
#'     show_data_area = FALSE,
#'   )
#'
#' gibraltar |>
#'   dplyr::filter(date <= "2023-05-14") |>
#'   dplyr::mutate(time = as.numeric(hms::as_hms(paste0(time, ":00")))) |>
#'   dplyr::mutate(humidity = humidity * 100) |>
#'   dplyr::select(date, time, temp, humidity, wind_speed) |>
#'   dplyr::group_by(date) |>
#'   dplyr::summarize(
#'     time = paste(time, collapse = ","),
#'     temp = paste(temp, collapse = ","),
#'     humidity = paste(humidity, collapse = ","),
#'     wind_speed = paste(wind_speed, collapse = ","),
#'   ) |>
#'   dplyr::mutate(is_satsun = lubridate::wday(date) %in% c(1, 7)) |>
#'   gt(rowname_col = "date") |>
#'   tab_header(
#'     title = "Meteorological Summary of Gibraltar Station",
#'     subtitle = "Data taken from May 1-14, 2023."
#'   ) |>
#'   fmt_date(columns = stub(), date_style = "wd_m_day_year") |>
#'   cols_nanoplot(
#'     columns = temp,
#'     columns_x_vals = time,
#'     expand_x = c(0, 86400),
#'     autoscale = TRUE,
#'     new_col_name = "temperature_nano",
#'     new_col_label = "Temperature",
#'     options = nanoplot_options_list
#'   ) |>
#'   cols_nanoplot(
#'     columns = humidity,
#'     columns_x_vals = time,
#'     expand_x = c(0, 86400),
#'     autoscale = TRUE,
#'     new_col_name = "humidity_nano",
#'     new_col_label = "Humidity",
#'     options = nanoplot_options_list
#'   ) |>
#'   cols_nanoplot(
#'     columns = wind_speed,
#'     columns_x_vals = time,
#'     expand_x = c(0, 86400),
#'     autoscale = TRUE,
#'     new_col_name = "wind_speed_nano",
#'     new_col_label = "Wind Speed",
#'     options = nanoplot_options_list
#'   ) |>
#'   cols_units(
#'     temperature_nano = ":degree:C",
#'     humidity_nano = "% (RH)",
#'     wind_speed_nano = "m s^-1"
#'   ) |>
#'   cols_hide(columns = is_satsun) |>
#'   tab_style_body(
#'     style = cell_fill(color = "#E5FEFE"),
#'     values = TRUE,
#'     targets = "row",
#'     extents = c("body", "stub")
#'   ) |>
#'   tab_style(
#'     style = cell_text(align = "center"),
#'     locations = cells_column_labels()
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_nanoplot_6.png")`
#' }}
#'
#' Box plots can be generated, and we just need to use `plot_type = "boxplot"`
#' to make that type of nanoplot. Using a small portion of the [`pizzaplace`]
#' dataset, we will create a simple table that displays a box plot of pizza
#' sales for a selection of days. By converting the string-time 24-hour-clock
#' time values to the number of seconds elapsed in a day, we get continuous
#' values that can be incorporated into each box plot. And, by supplying a
#' function to the `y_val_fmt_fn` argument within [nanoplot_options()], we can
#' transform the integer seconds values back to clock times for display on
#' hover.
#'
#' ```r
#' pizzaplace |>
#'   dplyr::filter(date <= "2015-01-14") |>
#'   dplyr::mutate(time = as.numeric(hms::as_hms(time))) |>
#'   dplyr::summarize(time = paste(time, collapse = ","), .by = date) |>
#'   dplyr::mutate(is_weekend = lubridate::wday(date) %in% 6:7) |>
#'   gt() |>
#'   tab_header(title = "Pizza Sales in Early January 2015") |>
#'   fmt_date(columns = date, date_style = 2) |>
#'   cols_nanoplot(
#'     columns = time,
#'     plot_type = "boxplot",
#'     options = nanoplot_options(y_val_fmt_fn = function(x) hms::as_hms(x))
#'   ) |>
#'   cols_hide(columns = is_weekend) |>
#'   cols_width(everything() ~ px(250)) |>
#'   cols_align(align = "center", columns = nanoplots) |>
#'   cols_align(align = "left", columns = date) |>
#'   tab_style(
#'     style = cell_borders(
#'       sides = "left", color = "gray"),
#'     locations = cells_body(columns = nanoplots)
#'   ) |>
#'   tab_style_body(
#'     style = cell_fill(color = "#E5FEFE"),
#'     values = TRUE,
#'     targets = "row"
#'   ) |>
#'   tab_options(column_labels.hidden = TRUE)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_nanoplot_7.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-8
#'
#' @section Function Introduced:
#' `v0.10.0` (October 7, 2023)
#'
#' @export
cols_nanoplot <- function(
    data,
    columns,
    rows = everything(),
    plot_type = c("line", "bar", "boxplot"),
    plot_height = "2em",
    missing_vals = c("gap", "marker", "zero", "remove"),
    autoscale = FALSE,
    autohide = TRUE,
    columns_x_vals = NULL,
    reference_line = NULL,
    reference_area = NULL,
    expand_x = NULL,
    expand_y = NULL,
    new_col_name = NULL,
    new_col_label = NULL,
    before = NULL,
    after = NULL,
    options = NULL
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # Ensure that arguments are matched
  missing_vals <- rlang::arg_match(missing_vals)
  plot_type <- rlang::arg_match(plot_type)

  #
  # Resolution of columns and rows as character vectors
  #

  resolved_columns <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data,
      excl_stub = FALSE
    )

  resolved_columns_x <-
    resolve_cols_c(
      expr = {{ columns_x_vals }},
      data = data,
      excl_stub = FALSE,
      null_means = "nothing"
    )

  resolved_rows_idx <-
    resolve_rows_i(
      expr = {{ rows }},
      data = data
    )

  # Get the internal data table
  data_tbl <- dt_data_get(data = data)

  data_vals_plot_y <-
    generate_data_vals_list(
      data_tbl = data_tbl,
      resolved_columns = resolved_columns,
      resolved_rows_idx = resolved_rows_idx
    )

  if (length(resolved_columns_x) > 0) {

    data_vals_plot_x <-
      generate_data_vals_list(
        data_tbl = data_tbl,
        resolved_columns = resolved_columns_x,
        resolved_rows_idx = resolved_rows_idx
      )

  } else {
    data_vals_plot_x <- NULL
  }

  plot_height <- plot_height %||% "2em"
  # use nanoplots_options() by default for options_plots if options not set.
  options_plots <- options %||% nanoplot_options()

  # Get all `y` vals into a vector
  all_y_vals <- unlist(data_vals_plot_y)

  # Get all `y` vals from single-valued components of `data_vals_plot_y`
  # into a vector
  all_single_y_vals <- c()
  for (i in seq_along(data_vals_plot_y)) {
    if (length(data_vals_plot_y[[i]]) == 1 && !is.na(data_vals_plot_y[[i]])) {
      all_single_y_vals <- c(all_single_y_vals, data_vals_plot_y[[i]])
    }
  }

  # Automatically apply `expand_x` and `expand_y` values as necessary if
  # `autoscale` has been set to TRUE
  if (autoscale) {

    min_y_vals <- min(all_y_vals, na.rm = TRUE)
    max_y_vals <- max(all_y_vals, na.rm = TRUE)
    expand_y <- c(min_y_vals, max_y_vals)

    if (!is.null(data_vals_plot_x)) {

      all_x_vals <- unlist(data_vals_plot_x)
      min_x_vals <- min(all_x_vals, na.rm = TRUE)
      max_x_vals <- max(all_x_vals, na.rm = TRUE)
      expand_x <- c(min_x_vals, max_x_vals)
    }
  }

  # Initialize vector that will contain the nanoplots
  nanoplots <- c()

  for (i in seq_along(data_vals_plot_y)) {

    data_vals_plot_y_i <- data_vals_plot_y[i][[1]]

    if (!is.null(data_vals_plot_x)) {
      data_vals_plot_x_i <- data_vals_plot_x[i][[1]]
    } else {
      data_vals_plot_x_i <- NULL
    }

    data_plot_i <-
      generate_nanoplot(
        y_vals = data_vals_plot_y_i,
        y_ref_line = reference_line,
        y_ref_area = reference_area,
        x_vals = data_vals_plot_x_i,
        expand_x = expand_x,
        expand_y = expand_y,
        missing_vals = missing_vals,
        all_y_vals = all_y_vals,
        all_single_y_vals = all_single_y_vals,
        plot_type = plot_type,
        line_type = options_plots$data_line_type,
        currency = options_plots$currency,
        y_val_fmt_fn = options_plots$y_val_fmt_fn,
        y_axis_fmt_fn = options_plots$y_axis_fmt_fn,
        y_ref_line_fmt_fn = options_plots$y_ref_line_fmt_fn,
        data_point_radius = options_plots$data_point_radius,
        data_point_stroke_color = options_plots$data_point_stroke_color,
        data_point_stroke_width = options_plots$data_point_stroke_width,
        data_point_fill_color = options_plots$data_point_fill_color,
        data_line_stroke_color = options_plots$data_line_stroke_color,
        data_line_stroke_width = options_plots$data_line_stroke_width,
        data_area_fill_color = options_plots$data_area_fill_color,
        data_bar_stroke_color = options_plots$data_bar_stroke_color,
        data_bar_stroke_width = options_plots$data_bar_stroke_width,
        data_bar_fill_color = options_plots$data_bar_fill_color,
        data_bar_negative_stroke_color = options_plots$data_bar_negative_stroke_color,
        data_bar_negative_stroke_width = options_plots$data_bar_negative_stroke_width,
        data_bar_negative_fill_color = options_plots$data_bar_negative_fill_color,
        reference_line_color = options_plots$reference_line_color,
        reference_area_fill_color = options_plots$reference_area_fill_color,
        vertical_guide_stroke_color = options_plots$vertical_guide_stroke_color,
        vertical_guide_stroke_width = options_plots$vertical_guide_stroke_width,
        show_data_points = options_plots$show_data_points,
        show_data_line = options_plots$show_data_line,
        show_data_area = options_plots$show_data_area,
        show_ref_line = options_plots$show_reference_line,
        show_ref_area = options_plots$show_reference_area,
        show_vertical_guides = options_plots$show_vertical_guides,
        show_y_axis_guide = options_plots$show_y_axis_guide,
        interactive_data_values = options_plots$interactive_data_values,
        svg_height = plot_height
      )

    nanoplots <- c(nanoplots, data_plot_i)
  }

  data <-
    cols_add(
      .data = data,
      nanoplots,
      .before = before,
      .after = after
    )

  if (!is.null(new_col_name)) {

    # TODO: Ensure that the new column name is validated for use

    validated_new_col_name <- as.character(new_col_name)

    colnames(data$`_data`) <-
      sub(
        "nanoplots",
        validated_new_col_name,
        colnames(data$`_data`),
        fixed = TRUE
      )

    data$`_boxhead`[["var"]] <-
      sub(
        "nanoplots",
        validated_new_col_name,
        data$`_boxhead`[["var"]],
        fixed = TRUE
      )

    data <-
      fmt_passthrough(
        data = data,
        columns = validated_new_col_name,
        escape = FALSE
      )

  } else {

    validated_new_col_name <- "nanoplots"

    data <-
      fmt_passthrough(
        data = data,
        columns = "nanoplots",
        escape = FALSE
      )
  }

  # The label ascribed to the new column needs to be modified in two cases:
  # (1) If `new_column_name` provided and `new_col_label = NULL`, the label
  #     should be that provided in `new_column_name`
  # (2) If `new_col_label` is provided, change the label of that new column
  #     to the value stored in that argument

  if (!is.null(new_col_name) && is.null(new_col_label)) {

    data <-
      dt_boxhead_edit_column_label(
        data = data,
        var = validated_new_col_name,
        column_label = validated_new_col_name
      )
  }

  if (!is.null(new_col_label)) {

    data <-
      dt_boxhead_edit_column_label(
        data = data,
        var = validated_new_col_name,
        column_label = new_col_label
      )
  }

  data <-
    tab_style(
      data,
      style = paste0(
        "padding-top:0; ",
        "padding-bottom:0; ",
        "vertical-align: middle; ",
        "overflow-x: visible;"
      ),
      locations = cells_body(columns = validated_new_col_name)
    )

  if (isTRUE(autohide)) {

    data <-
      cols_hide(
        data = data,
        columns = resolved_columns
      )

    if (length(resolved_columns_x) > 0) {

      data <-
        cols_hide(
          data = data,
          columns = resolved_columns_x
        )
    }
  }

  data
}

generate_data_vals_list <- function(
    data_tbl,
    resolved_columns,
    resolved_rows_idx
) {

  data_vals_plot <- list()

  for (i in seq_len(nrow(data_tbl))) {

    if (!(i %in% resolved_rows_idx)) {

      data_vals_plot <- c(data_vals_plot, list(NA_character_))

    } else {

      data_vals_i <- dplyr::select(data_tbl, dplyr::all_of(resolved_columns))

      data_vals_i <- as.vector(data_vals_i[i, ])

      data_vals_j <- c()

      for (j in seq_along(data_vals_i)) {

        if (
          !is.na(data_vals_i[j][[1]]) &&
          is.character(data_vals_i[j][[1]])
        ) {

          #
          # Detect value stream type and convert accordingly
          #

          if (
            grepl("\\d{1,4}-\\d{2}-\\d{2}", data_vals_i[j][[1]])
          ) {

            data_vals_j <-
              c(data_vals_j, process_time_stream(data_vals_i[j][[1]]))

          } else {

            data_vals_j <-
              c(data_vals_j, process_number_stream(data_vals_i[j][[1]]))
          }


        } else {
          data_vals_j <- c(data_vals_j, unname(unlist(data_vals_i[j][[1]])))
        }
      }

      data_vals_i <- list(data_vals_j)

      data_vals_plot <- c(data_vals_plot, data_vals_i)
    }
  }

  data_vals_plot
}

#' Move one or more columns
#'
#' @description
#'
#' On those occasions where you need to move columns this way or that way, we
#' can make use of the `cols_move()` function. While it's true that the movement
#' of columns can be done upstream of **gt**, it is much easier and less error
#' prone to use the function provided here. The movement procedure here takes
#' one or more specified columns (in the `columns` argument) and places them to
#' the right of a different column (the `after` argument). The ordering of the
#' `columns` to be moved is preserved, as is the ordering of all other columns
#' in the table.
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The columns for which the moving operations should be applied. Can either
#'   be a series of column names provided in `c()`, a vector of column indices,
#'   or a select helper function (e.g. [starts_with()], [ends_with()],
#'   [contains()], [matches()], [num_range()], and [everything()]. The columns
#'   move as a group to a different position. The order of the remaining columns
#'   will be preserved.
#'
#' @param after *Column used as anchor*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The column used to anchor the insertion of the moved columns. All of the
#'   moved columns will be placed to the right of this column. While select
#'   helper functions such as [starts_with()] and [ends_with()] can be used for
#'   column targeting, it's recommended that a single column name be used. This
#'   is to ensure that exactly one column is provided here.
#'
#' @return An object of class `gt_tbl`.
#'
#' @details
#'
#' The columns supplied in `columns` must all exist in the table and none of
#' them can be in the `after` argument. The `after` column must also exist and
#' only one column should be provided here. If you need to place one or more
#' columns at the beginning of the column series, the [cols_move_to_start()]
#' function should be used. Similarly, if those columns to move should be placed
#' at the end of the column series then use [cols_move_to_end()].
#'
#' @section Examples:
#'
#' Use the [`countrypops`] dataset to create a **gt** table. We'll choose to
#' position the `population` column after the `country_name` column by using the
#' `cols_move()` function.
#'
#' ```r
#' countrypops |>
#'   dplyr::select(-contains("code")) |>
#'   dplyr::filter(country_name == "Japan") |>
#'   dplyr::slice_tail(n = 10) |>
#'   gt() |>
#'   cols_move(
#'     columns = population,
#'     after = country_name
#'   ) |>
#'   fmt_integer(columns = population)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_move_1.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-9
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
cols_move <- function(
    data,
    columns,
    after
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # if no `columns` are provided, return data unaltered
  if (rlang::quo_is_missing(rlang::enquo(columns))) {
    return(data)
  }

  # Get the columns supplied in `columns` as a character vector
  columns <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data
    )

  # Get the `after` columns as a character vector
  after <-
    resolve_cols_c(
      expr = {{ after }},
      data = data
    )

  vars <- dt_boxhead_get_vars(data = data)

  # Stop function if `after` contains multiple columns
  if (length(after) > 1) {
    cli::cli_abort("Only one column name should be supplied to `after`.")
  }

  # Stop function if `after` doesn't exist in `vars`
  if (!(after %in% vars)) {
    cli::cli_abort(
      "The column supplied to `after` doesn't exist in the input `data` table."
    )
  }

  # Stop function if no `columns` are provided
  if (length(columns) == 0) {
    cli::cli_abort("Columns must be provided.")
  }

  # Stop function if any of the `columns` don't exist in `vars`
  if (!all(columns %in% vars)) {
    cli::cli_abort(
      "All `columns` must exist and be visible in the input `data` table."
    )
  }

  # Get the remaining column names in the table
  moving_columns <- setdiff(columns, after)
  other_columns <- base::setdiff(vars, moving_columns)

  # Get the column index for where the set
  # of `columns` should be inserted after
  after_index <- which(other_columns == after)

  new_vars <- append(other_columns, moving_columns, after = after_index)

  dt_boxhead_set_var_order(
    data = data,
    vars = new_vars
  )
}

#' Move one or more columns to the start
#'
#' @description
#'
#' We can easily move set of columns to the beginning of the column series and
#' we only need to specify which `columns`. It's possible to do this upstream of
#' **gt**, however, it is easier with this function and it presents less
#' possibility for error. The ordering of the `columns` that are moved to the
#' start is preserved (same with the ordering of all other columns in the
#' table).
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The columns for which the moving operations should be applied. Can either
#'   be a series of column names provided in `c()`, a vector of column indices,
#'   or a select helper function (e.g. [starts_with()], [ends_with()],
#'   [contains()], [matches()], [num_range()], and [everything()]). The columns
#'   move as a group to the left-most side of the table. The order of the
#'   remaining columns will be preserved.
#'
#' @return An object of class `gt_tbl`.
#'
#' @details
#'
#' The columns supplied in `columns` must all exist in the table. If you need to
#' place one or columns at the end of the column series, [cols_move_to_end()]
#' should be used. More control is offered with [cols_move()], where columns
#' could be placed after a specific column.
#'
#' @section Examples:
#'
#' For this example, we'll use a portion of the [`countrypops`] dataset to
#' create a simple **gt** table. Let's move the `year` column, which is the
#' middle column, to the start of the column series with `cols_move_to_start()`.
#'
#' ```r
#' countrypops |>
#'   dplyr::select(-contains("code")) |>
#'   dplyr::filter(country_name == "Fiji") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt() |>
#'   cols_move_to_start(columns = year)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_move_to_start_1.png")`
#' }}
#'
#' We can also move multiple columns at a time. With the same
#' [`countrypops`]-based table, let's move both the `year` and `population`
#' columns to the start of the column series.
#'
#' ```r
#' countrypops |>
#'   dplyr::select(-contains("code")) |>
#'   dplyr::filter(country_name == "Fiji") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt() |>
#'   cols_move_to_start(columns = c(year, population))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_move_to_start_2.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-10
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
cols_move_to_start <- function(
    data,
    columns
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # if no `columns` are provided, return data unaltered
  if (rlang::quo_is_missing(rlang::enquo(columns))) {
    return(data)
  }

  vars <- dt_boxhead_get_vars(data = data)

  # Get the columns supplied in `columns` as a character vector
  columns <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data
    )

  # Stop function if no `columns` are provided
  if (length(columns) == 0) {
    cli::cli_abort("Columns must be provided.")
  }

  # Stop function if any of the `columns` don't exist in `vars`
  if (!all(columns %in% vars)) {
    cli::cli_abort(
      "All `columns` must exist and be visible in the input `data` table."
    )
  }

  # Get the remaining column names in the table
  other_columns <- base::setdiff(vars, columns)

  new_vars <- append(other_columns, columns, after = 0)

  dt_boxhead_set_var_order(
    data = data,
    vars = new_vars
  )
}

#' Move one or more columns to the end
#'
#' @description
#'
#' It's possible to move a set of columns to the end of the column series, we
#' only need to specify which `columns` are to be moved. While this can be done
#' upstream of **gt**, this function makes to process much easier and it's less
#' error prone. The ordering of the `columns` that are moved to the end is
#' preserved (same with the ordering of all other columns in the table).
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The columns for which the moving operations should be applied. Can either
#'   be a series of column names provided in `c()`, a vector of column indices,
#'   or a select helper function (e.g. [starts_with()], [ends_with()],
#'   [contains()], [matches()], [num_range()], and [everything()]. The columns
#'   move as a group to the right-most side of the table. The order of the
#'   remaining columns will be preserved.
#'
#' @return An object of class `gt_tbl`.
#'
#' @details
#'
#' The columns supplied in `columns` must all exist in the table. If you need to
#' place one or columns at the start of the column series, [cols_move_to_start()]
#' should be used. More control is offered with [cols_move()], where columns
#' could be placed after a specific column.
#'
#' @section Examples:
#'
#' For this example, we'll use a portion of the [`countrypops`] dataset to
#' create a simple **gt** table. Let's move the `year` column, which is the
#' middle column, to the end of the column series with `cols_move_to_end()`.
#'
#' ```r
#' countrypops |>
#'   dplyr::select(-contains("code")) |>
#'   dplyr::filter(country_name == "Benin") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt() |>
#'   cols_move_to_end(columns = year)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_move_to_end_1.png")`
#' }}
#'
#' We can also move multiple columns at a time. With the same
#' [`countrypops`]-based table, let's move both the `year` and `country_name`
#' columns to the end of the column series.
#'
#' ```r
#' countrypops |>
#'   dplyr::select(-contains("code")) |>
#'   dplyr::filter(country_name == "Benin") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt() |>
#'   cols_move_to_end(columns = c(year, country_name))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_move_to_end_2.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-11
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
cols_move_to_end <- function(
    data,
    columns
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # if no `columns` are provided, return data unaltered
  if (rlang::quo_is_missing(rlang::enquo(columns))) {
    return(data)
  }

  vars <- dt_boxhead_get_vars(data = data)

  # Get the columns supplied in `columns` as a character vector
  columns <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data
    )

  # Stop function if no `columns` are provided
  if (length(columns) == 0) {
    cli::cli_abort("Columns must be provided.")
  }

  # Stop function if any of the `columns` don't exist in `vars`
  if (!all(columns %in% vars)) {
    cli::cli_abort(
      "All `columns` must exist and be visible in the input `data` table."
    )
  }

  # Get the remaining column names in the table
  other_columns <- base::setdiff(vars, columns)

  new_vars <- append(other_columns, columns)

  dt_boxhead_set_var_order(
    data = data,
    vars = new_vars
  )
}

#' Hide one or more columns
#'
#' @description
#'
#' `cols_hide()` allows us to hide one or more columns from
#' appearing in the final output table. While it's possible and often desirable
#' to omit columns from the input table data before introduction to [gt()],
#' there can be cases where the data in certain columns is useful (as a column
#' reference during formatting of other columns) but the final display of those
#' columns is not necessary.
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The columns to hide in the output display table. Can either be a series of
#'   column names provided in `c()`, a vector of column indices, or a select
#'   helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()], [num_range()], and [everything()]).
#'
#' @return An object of class `gt_tbl`. `data` will be unaltered if `columns` is
#'   not supplied.
#'
#' @details
#'
#' The hiding of columns is internally a rendering directive, so, all columns
#' that are 'hidden' are still accessible and useful in any expression provided
#' to a `rows` argument. Furthermore, `cols_hide()` (as with many **gt**
#' functions) can be placed anywhere in a pipeline of **gt** function calls
#' (acting as a promise to hide columns when the timing is right). However,
#' there's perhaps greater readability when placing this call closer to the end
#' of such a pipeline. `cols_hide()` quietly changes the visible state of a
#' column (much like [cols_unhide()]) and doesn't yield warnings or messages
#' when changing the state of already-invisible columns.
#'
#' @section Examples:
#'
#' Let's use a small portion of the [`countrypops`] dataset to create a **gt**
#' table. We can hide the `country_code_2` and `country_code_3` columns with the
#' `cols_hide()` function.
#'
#' ```r
#' countrypops |>
#'   dplyr::filter(country_name == "Egypt") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt() |>
#'   cols_hide(columns = c(country_code_2, country_code_3))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_hide_1.png")`
#' }}
#'
#' Using another [`countrypops`]-based **gt** table, we can use the `population`
#' column to provide the conditional placement of footnotes. Then, we'll hide
#' that column along with the `country_code_3` column. Note that the order of
#' `cols_hide()` and [tab_footnote()] has no effect on the final display of the
#' table.
#'
#' ```r
#' countrypops |>
#'   dplyr::filter(country_name == "Pakistan") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt() |>
#'   cols_hide(columns = c(country_code_3, population)) |>
#'   tab_footnote(
#'     footnote = "Population above 220,000,000.",
#'     locations = cells_body(
#'       columns = year,
#'       rows = population > 220E6
#'     )
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_hide_2.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-12
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @seealso [cols_unhide()] to perform the inverse operation.
#'
#' @export
cols_hide <- function(
    data,
    columns
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # if no `columns` are provided, return data unaltered
  if (rlang::quo_is_missing(rlang::enquo(columns))) {
    return(data)
  }

  # Get the columns supplied in `columns` as a character vector
  columns <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data,
      excl_stub = FALSE
    )

  vars <- dt_boxhead_get_vars(data = data)

  # Stop function if any of the `columns` don't exist in `vars`
  if (!all(columns %in% vars)) {
    cli::cli_abort("All `columns` must exist in the input `data` table.")
  }

  # Set the `"hidden"` type for the `columns` in `_dt_boxhead`
  dt_boxhead_set_hidden(
    data = data,
    vars = columns
  )
}

#' Unhide one or more columns
#'
#' @description
#'
#' `cols_unhide()` allows us to take one or more hidden columns (usually done
#' via [cols_hide()]) and make them visible in the final output table. This may
#' be important in cases where the user obtains a `gt_tbl` object with hidden
#' columns and there is motivation to reveal one or more of those.
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // *default:* `everything()`
#'
#'   The columns to unhide in the output display table. Can either be a series
#'   of column names provided in `c()`, a vector of column indices, or a select
#'   helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()], [num_range()], and [everything()]).
#'
#' @return An object of class `gt_tbl`.
#'
#' @details
#'
#' The hiding and unhiding of columns is internally a rendering directive, so,
#' all columns that are 'hidden' are still accessible and useful in any
#' expression provided to a `rows` argument. The `cols_unhide()` function
#' quietly changes the visible state of a column (much like the [cols_hide()]
#' function) and doesn't yield warnings or messages when changing the state of
#' already-visible columns.
#'
#' @section Examples:
#'
#' Let's use a small portion of the [`countrypops`] dataset to create a **gt**
#' table. We'll hide the `country_code_2` and `country_code_3` columns with
#' [cols_hide()].
#'
#' ```r
#' tab_1 <-
#'   countrypops |>
#'   dplyr::filter(country_name == "Singapore") |>
#'   dplyr::slice_tail(n = 5) |>
#'   gt() |>
#'   cols_hide(columns = c(country_code_2, country_code_3))
#'
#' tab_1
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_unhide_1.png")`
#' }}
#'
#' If the `tab_1` object is provided without the code or source data to
#' regenerate it, and, the user wants to reveal otherwise hidden columns then
#' `cols_unhide()` becomes useful.
#'
#' ```r
#' tab_1 |> cols_unhide(columns = country_code_2)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_unhide_2.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-13
#'
#' @section Function Introduced:
#' `v0.3.0` (May 12, 2021)
#'
#' @seealso [cols_hide()] to perform the inverse operation.
#'
#' @export
cols_unhide <- function(
    data,
    columns
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # Get the columns supplied in `columns` as a character vector
  columns <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data
    )

  vars <- dt_boxhead_get_vars(data = data)

  # if no `columns` are provided, return data unaltered
  if (length(columns) == 0) {
    return(data)
  }

  # Stop function if any of the `columns` don't exist in `vars`
  if (!all(columns %in% vars)) {
    cli::cli_abort("All `columns` must exist in the input `data` table.")
  }

  # Set the `"visible"` type for the `columns` in `_dt_boxhead`
  dt_boxhead_set_not_hidden(
    data = data,
    vars = columns
  )
}

#' Merge data from two or more columns to a single column
#'
#' @description
#'
#' This function takes input from two or more columns and allows the contents to
#' be merged into a single column by using a pattern that specifies the
#' arrangement. We can specify which columns to merge together in the `columns`
#' argument. The string-combining pattern is to be provided in the `pattern`
#' argument. The first column in the `columns` series operates as the target
#' column (i.e., the column that will undergo mutation) whereas all following
#' `columns` will be untouched. There is the option to hide the non-target
#' columns (i.e., second and subsequent columns given in `columns`). The
#' formatting of values in different columns will be preserved upon merging.
#'
#' @inheritParams cols_align
#'
#' @param columns *Columns to target*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The columns for which the merging operations should be applied. The first
#'   column resolved will be the target column (i.e., undergo mutation) and the
#'   other columns will serve to provide input. Can either be a series of column
#'   names provided in `c()`, a vector of column indices, or a select helper
#'   function (e.g. [starts_with()], [ends_with()], [contains()], [matches()],
#'   [num_range()], and [everything()]). A vector is recommended because in that
#'   case we are absolutely certain about the order of columns, and, that order
#'   information is needed for this and other arguments.
#'
#' @param hide_columns *Subset of `columns` to hide*
#'
#'   `<column-targeting expression>|FALSE` // *default:* `columns[-1]`
#'
#'   Any column names provided here will have their state changed to `hidden`
#'   (via internal use of [cols_hide()]) if they aren't already hidden. This is
#'   convenient if the shared purpose of these specified columns is only to
#'   provide string input to the target column. To suppress any hiding of
#'   columns, `FALSE` can be used here.
#'
#' @param rows *Rows to target*
#'
#'   `<row-targeting expression>` // *default:* `everything()`
#'
#'   In conjunction with `columns`, we can specify which of their rows should
#'   participate in the merging process. The default [everything()] results in
#'   all rows in `columns` being formatted. Alternatively, we can supply a
#'   vector of row IDs within `c()`, a vector of row indices, or a select
#'   helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()], [num_range()], and [everything()]). We can also use
#'   expressions to filter down to the rows we need
#'   (e.g., `[colname_1] > 100 & [colname_2] < 50`).
#'
#' @param pattern *Formatting pattern*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   A formatting pattern that specifies the arrangement of the `columns` values
#'   and any string literals. The pattern uses numbers (within `{ }`) that
#'   correspond to the indices of columns provided in `columns`. If two columns
#'   are provided in `columns` and we would like to combine the cell data onto
#'   the first column, `"{1} {2}"` could be used. If a pattern isn't provided
#'   then a space-separated pattern that includes all `columns` will be
#'   generated automatically. Further details are provided in the *How the
#'   `pattern` works* section.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section How the `pattern` works:
#'
#' There are two types of templating for the `pattern` string:
#'
#' 1. `{ }` for arranging single column values in a row-wise fashion
#' 2. `<< >>` to surround spans of text that will be removed if any of the
#' contained `{ }` yields a missing value
#'
#' Integer values are placed in `{ }` and those values correspond to the columns
#' involved in the merge, in the order they are provided in the `columns`
#' argument. So the pattern `"{1} ({2}-{3})"` corresponds to the target column
#' value listed first in `columns` and the second and third columns cited
#' (formatted as a range in parentheses). With hypothetical values, this might
#' result as the merged string `"38.2 (3-8)"`.
#'
#' Because some values involved in merging may be missing, it is likely that
#' something like `"38.2 (3-NA)"` would be undesirable. For such cases, placing
#' sections of text in `<< >>` results in the entire span being eliminated if
#' there were to be an `NA` value (arising from `{ }` values). We could instead
#' opt for a pattern like `"{1}<< ({2}-{3})>>"`, which results in `"38.2"` if
#' either columns `{2}` or `{3}` have an `NA` value. We can even use a more
#' complex nesting pattern like `"{1}<< ({2}-<<{3}>>)>>"` to retain a lower
#' limit in parentheses (where `{3}` is `NA`) but remove the range altogether
#' if `{2}` is `NA`.
#'
#' One more thing to note here is that if [sub_missing()] is used on values in
#' a column, those specific values affected won't be considered truly missing by
#' `cols_merge()` (since it's been handled with substitute text). So, the
#' complex pattern `"{1}<< ({2}-<<{3}>>)>>"` might result in something like
#' `"38.2 (3-limit)"` if `sub_missing(..., missing_text = "limit")` were used
#' on the third column supplied in `columns`.
#'
#' @section Comparison with other column-merging functions:
#'
#' There are three other column-merging functions that offer specialized
#' behavior that is optimized for common table tasks: [cols_merge_range()],
#' [cols_merge_uncert()], and [cols_merge_n_pct()]. These functions operate
#' similarly, where the non-target columns can be optionally hidden from the
#' output table through the `autohide` option.
#'
#' @section Examples:
#'
#' Use a subset of the [`sp500`] dataset to create a **gt** table. Use the
#' `cols_merge()` function to merge the `open` & `close` columns together, and,
#' the `low` & `high` columns (putting an em dash between both). Relabel the
#' columns with [cols_label()].
#'
#' ```r
#' sp500 |>
#'   dplyr::slice(50:55) |>
#'   dplyr::select(-volume, -adj_close) |>
#'   gt() |>
#'   cols_merge(
#'     columns = c(open, close),
#'     pattern = "{1}&mdash;{2}"
#'   ) |>
#'   cols_merge(
#'     columns = c(low, high),
#'     pattern = "{1}&mdash;{2}"
#'   ) |>
#'   cols_label(
#'     open = "open/close",
#'     low = "low/high"
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_merge_1.png")`
#' }}
#'
#' Use a portion of [`gtcars`] to create a **gt** table. Use the `cols_merge()`
#' function to merge the `trq` & `trq_rpm` columns together, and, the `mpg_c` &
#' `mpg_h` columns. Given the presence of `NA` values, we can use patterns that
#' drop parts of the output text whenever missing values are encountered.
#'
#' ```r
#' gtcars |>
#'   dplyr::filter(year == 2017) |>
#'   dplyr::select(mfr, model, starts_with(c("trq", "mpg"))) |>
#'   gt() |>
#'   fmt_integer(columns = trq_rpm) |>
#'   cols_merge(
#'     columns = starts_with("trq"),
#'     pattern = "{1}<< ({2} rpm)>>"
#'   ) |>
#'   cols_merge(
#'     columns = starts_with("mpg"),
#'     pattern = "<<{1} city<</{2} hwy>>>>"
#'   ) |>
#'   cols_label(
#'     mfr = "Manufacturer",
#'     model = "Car Model",
#'     trq = "Torque",
#'     mpg_c = "MPG"
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_merge_2.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-14
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
cols_merge <- function(
    data,
    columns,
    hide_columns = columns[-1],
    rows = everything(),
    pattern = NULL
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  # Get the columns supplied in `columns` as a character vector
  columns <-
    resolve_cols_c(
      expr = {{ columns }},
      data = data,
      excl_stub = FALSE
    )

  pattern <- pattern %||% paste0("{", seq_along(columns), "}", collapse = " ")

  # Resolve the rows supplied in the `rows` argument
  resolved_rows_idx <-
    resolve_rows_i(
      expr = {{ rows }},
      data = data
    )

  # NOTE: It's important that `hide_columns` NOT be evaluated until after the
  # previous line has run. Otherwise, the default `hide_columns` value of
  # columns[-1] may not evaluate to a sensible result. It's also important
  # that `pattern` not be evaluated, for much the same reason as above.

  # Get the columns supplied in `hide_columns` as a character vector
  suppressWarnings(
    hide_columns <-
      resolve_cols_c(
        expr = {{ hide_columns }},
        data = data
      )
  )

  if (length(hide_columns) > 0) {

    hide_columns_from_supplied <- base::intersect(hide_columns, columns)

    if (length(base::setdiff(hide_columns, columns) > 0)) {
      cli::cli_warn(c(
        "Only a subset of columns supplied in `columns` will be hidden.",
        "*" = "Use an additional `cols_hide()` expression to hide any
        out-of-scope columns."
      ),
      .frequency = "regularly",
      .frequency_id = "cols_merge_hide_columns_scope"
      )
    }

    if (length(hide_columns_from_supplied) > 0) {

      data <-
        cols_hide(
          data = data,
          columns = dplyr::all_of(hide_columns_from_supplied)
        )
    }
  }

  # Create an entry and add it to the `_col_merge` attribute
  dt_col_merge_add(
    data = data,
    col_merge = dt_col_merge_entry(
      vars = columns,
      rows = resolved_rows_idx,
      type = "merge",
      pattern = pattern
    )
  )
}

#' Merge columns to a value-with-uncertainty column
#'
#' @description
#'
#' `cols_merge_uncert()` is a specialized variant of [cols_merge()]. It takes as
#' input a base value column (`col_val`) and either: (1) a single uncertainty
#' column, or (2) two columns representing lower and upper uncertainty bounds.
#' These columns will be essentially merged in a single column (that of
#' `col_val`). What results is a column with values and associated
#' uncertainties, and any columns specified in `col_uncert` are hidden from
#' appearing the output table.
#'
#' @inheritParams cols_align
#'
#' @param col_val *Column to target for base values*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The column that contains values for the start of the range. While select
#'   helper functions such as [starts_with()] and [ends_with()] can be used for
#'   column targeting, it's recommended that a single column name be used. This
#'   is to ensure that exactly one column is provided here.
#'
#' @param col_uncert *Column or columns to target for uncertainty values*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The most common case involves supplying a single column with uncertainties;
#'   these values will be combined with those in `col_val`. Less commonly, the
#'   lower and upper uncertainty bounds may be different. For that case, two
#'   columns representing the lower and upper uncertainty values away from
#'   `col_val`, respectively, should be provided. While select helper functions
#'   such as [starts_with()] and [ends_with()] can be used for column targeting,
#'   it's recommended that one or two column names be explicitly provided in a
#'   vector.
#'
#' @param rows *Rows to target*
#'
#'   `<row-targeting expression>` // *default:* `everything()`
#'
#'   In conjunction with `columns`, we can specify which of their rows should
#'   participate in the merging process. The default [everything()] results in
#'   all rows in `columns` being formatted. Alternatively, we can supply a
#'   vector of row IDs within `c()`, a vector of row indices, or a select
#'   helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()],  [num_range()], and [everything()]). We can also use
#'   expressions to filter down to the rows we need
#'   (e.g., `[colname_1] > 100 & [colname_2] < 50`).
#'
#' @param sep *Separator text for uncertainties*
#'
#'   `scalar<character>` // *default:* `" +/- "`
#'
#'   The separator text that contains the uncertainty mark for a single
#'   uncertainty value. The default value of `" +/- "` indicates that an
#'   appropriate plus/minus mark will be used depending on the output context.
#'   Should you want this special symbol to be taken literally, it can be
#'   supplied within the [I()] function.
#'
#' @param autohide *Automatic hiding of the `col_uncert` column(s)*
#'
#'   `scalar<logical>` // *default:* `TRUE`
#'
#'   An option to automatically hide any columns specified in `col_uncert`. Any
#'   columns with their state changed to 'hidden' will behave the same as
#'   before, they just won't be displayed in the finalized table.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Comparison with other column-merging functions:
#'
#' This function could be somewhat replicated using [cols_merge()] in the case
#' where a single column is supplied for `col_uncert`, however,
#' `cols_merge_uncert()` employs the following specialized semantics for `NA`
#' handling:
#'
#' 1. `NA`s in `col_val` result in missing values for the merged column (e.g.,
#' `NA` + `0.1` = `NA`)
#' 2. `NA`s in `col_uncert` (but not `col_val`) result in base values only for
#' the merged column (e.g., `12.0` + `NA` = `12.0`)
#' 3. `NA`s both `col_val` and `col_uncert` result in missing values for the
#' merged column (e.g., `NA` + `NA` = `NA`)
#'
#' Any resulting `NA` values in the `col_val` column following the merge
#' operation can be easily formatted using [sub_missing()].
#'
#' This function is part of a set of four column-merging functions. The other
#' three are the general [cols_merge()] function and the specialized
#' [cols_merge_range()] and [cols_merge_n_pct()] functions. These functions
#' operate similarly, where the non-target columns can be optionally hidden from
#' the output table through the `hide_columns` or `autohide` options.
#'
#' @section Examples:
#'
#' Let's use the [`exibble`] dataset to create a simple, two-column **gt** table
#' (keeping only the `num` and `currency` columns). We'll format the `num`
#' column with the [fmt_number()] function. Next we merge the `currency` and
#' `num` columns into the `currency` column; this will contain a base value and
#' an uncertainty and it's all done using the `cols_merge_uncert()` function.
#' After the merging process, the column label for the `currency` column is
#' updated with [cols_label()] to better describe the content.
#'
#' ```r
#' exibble |>
#'   dplyr::select(num, currency) |>
#'   dplyr::slice(1:7) |>
#'   gt() |>
#'   fmt_number(
#'     columns = num,
#'     decimals = 3,
#'     use_seps = FALSE
#'   ) |>
#'   cols_merge_uncert(
#'     col_val = currency,
#'     col_uncert = num
#'   ) |>
#'   cols_label(currency = "value + uncert.")
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_merge_uncert_1.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-15
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
cols_merge_uncert <- function(
    data,
    col_val,
    col_uncert,
    rows = everything(),
    sep = " +/- ",
    autohide = TRUE
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  resolved <-
    cols_merge_resolver(
      data = data,
      col_begin = {{ col_val }},
      col_end = {{ col_uncert }}
    )

  # Resolve the rows supplied in the `rows` argument
  resolved_rows_idx <-
    resolve_rows_i(
      expr = {{ rows }},
      data = data
    )

  # Create an entry and add it to the `_col_merge` attribute
  data <-
    dt_col_merge_add(
      data = data,
      col_merge = dt_col_merge_entry(
        vars = resolved$columns,
        rows = resolved_rows_idx,
        type = "merge_uncert",
        pattern = resolved$pattern,
        sep = sep
      )
    )

  if (isTRUE(autohide)) {

    col_uncert <-
      resolve_cols_c(
        expr = {{ col_uncert }},
        data = data
      )

    data <-
      cols_hide(
        data = data,
        columns = dplyr::all_of(col_uncert)
      )
  }

  data
}

#' Merge two columns to a value range column
#'
#' @description
#'
#' `cols_merge_range()` is a specialized variant of [cols_merge()]. It operates
#' by taking a two columns that constitute a range of values (`col_begin` and
#' `col_end`) and merges them into a single column. What results is a column
#' containing both values separated by an em dash. The column specified in
#' `col_end` is dropped from the output table.
#'
#' @inheritParams cols_align
#'
#' @param col_begin *Column to target for beginning of range*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The column that contains values for the start of the range. While select
#'   helper functions such as [starts_with()] and [ends_with()] can be used for
#'   column targeting, it's recommended that a single column name be used. This
#'   is to ensure that exactly one column is provided here.
#'
#' @param col_end *Column to target for end of range*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The column that contains values for the end of the range. While select
#'   helper functions such as [starts_with()] and [ends_with()] can be used for
#'   column targeting, it's recommended that a single column name be used. This
#'   is to ensure that exactly one column is provided here.
#'
#' @param rows *Rows to target*
#'
#'   `<row-targeting expression>` // *default:* `everything()`
#'
#'   In conjunction with `columns`, we can specify which of their rows should
#'   participate in the merging process. The default [everything()] results in
#'   all rows in `columns` being formatted. Alternatively, we can supply a
#'   vector of row IDs within `c()`, a vector of row indices, or a select
#'   helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()], [num_range()], and [everything()]). We can also use
#'   expressions to filter down to the rows we need
#'   (e.g., `[colname_1] > 100 & [colname_2] < 50`).
#'
#' @param autohide *Automatic hiding of the `col_end` column*
#'
#'   `scalar<logical>` // *default:* `TRUE`
#'
#'   An option to automatically hide the column specified as
#'   `col_end`. Any columns with their state changed to hidden will behave
#'   the same as before, they just won't be displayed in the finalized table.
#'
#' @param sep *Separator text for ranges*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   The separator text that indicates the values are ranged. If a `sep` value
#'   is not provided then the range separator specific to the `locale` provided
#'   will be used (if a locale isn't specified then an en dash will be used).
#'   You can specify the use of an en dash with `"--"`; a triple-hyphen sequence
#'   (`"---"`) will be transformed to an em dash. Should you want hyphens to be
#'   taken literally, the `sep` value can be supplied within the base [I()]
#'   function.
#'
#' @param locale *Locale identifier*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional locale identifier that can be used for applying a `sep` pattern
#'   specific to a locale's rules. Examples include `"en"` for English (United
#'   States) and `"fr"` for French (France). We can call [info_locales()] as a
#'   useful reference for all of the locales that are supported. A locale ID can
#'   be also set in the initial [gt()] function call (where it would be used
#'   automatically by any function with a `locale` argument) but a `locale`
#'   value provided here will override that global locale.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Comparison with other column-merging functions:
#'
#' This function could be somewhat replicated using [cols_merge()], however,
#' `cols_merge_range()` employs the following specialized operations for `NA`
#' handling:
#'
#' 1. `NA`s in `col_begin` (but not `col_end`) result in a display of only
#  the `col_end` values only for the merged column
#' 2. `NA`s in `col_end` (but not `col_begin`) result in a display of only
#' the `col_begin` values only for the merged column (this is the converse of
#' the previous)
#' 3. `NA`s both in `col_begin` and `col_end` result in missing values for
#' the merged column
#'
#' Any resulting `NA` values in the `col_begin` column following the merge
#' operation can be easily formatted using [sub_missing()]. Separate calls of
#' [sub_missing()] can be used for the `col_begin` and `col_end` columns for
#' finer control of the replacement values.
#'
#' This function is part of a set of four column-merging functions. The other
#' three are the general [cols_merge()] function and the specialized
#' [cols_merge_uncert()] and [cols_merge_n_pct()] functions. These functions
#' operate similarly, where the non-target columns can be optionally hidden from
#' the output table through the `hide_columns` or `autohide` options.
#'
#' @section Examples:
#'
#' Let's use a subset of the [`gtcars`] dataset to create a **gt** table,
#' keeping only the `model`, `mpg_c`, and `mpg_h` columns. Merge the `"mpg*"`
#' columns together as a single range column (which is labeled as MPG, in
#' italics) using the `cols_merge_range()` function. After the merging process,
#' the column label for the `mpg_c` column is updated with [cols_label()] to
#' better describe the content.
#'
#' ```r
#' gtcars |>
#'   dplyr::select(model, starts_with("mpg")) |>
#'   dplyr::slice(1:8) |>
#'   gt() |>
#'   cols_merge_range(
#'     col_begin = mpg_c,
#'     col_end = mpg_h
#'   ) |>
#'   cols_label(mpg_c = md("*MPG*"))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_merge_range_1.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-16
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
cols_merge_range <- function(
    data,
    col_begin,
    col_end,
    rows = everything(),
    autohide = TRUE,
    sep = NULL,
    locale = NULL
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  resolved <-
    cols_merge_resolver(
      data = data,
      col_begin = {{ col_begin }},
      col_end = {{ col_end }}
    )

  # Resolve the rows supplied in the `rows` argument
  resolved_rows_idx <-
    resolve_rows_i(
      expr = {{ rows }},
      data = data
    )

  # Stop function if `locale` does not have a valid value; normalize locale
  # and resolve one that might be set globally
  validate_locale(locale = locale)
  locale <- normalize_locale(locale = locale)
  locale <- resolve_locale(data = data, locale = locale)

  # Use locale-based marks if a `sep` value is not provided
  if (is.null(sep)) {

    # Get the range pattern for the locale (if not specified then 'en' is used)
    range_pattern <- get_locale_range_pattern(locale = locale)

    # Remove the placeholders from `range_pattern` since `cols_merge_range()`
    # only requires the internal separator text for `sep`
    sep <- gsub("\\{1\\}|\\{2\\}", "", range_pattern)
  }

  # Create an entry and add it to the `_col_merge` attribute
  data <-
    dt_col_merge_add(
      data = data,
      col_merge = dt_col_merge_entry(
        vars = resolved$columns,
        rows = resolved_rows_idx,
        type = "merge_range",
        pattern = resolved$pattern,
        sep = sep
      )
    )

  if (isTRUE(autohide)) {

    col_end <-
      resolve_cols_c(
        expr = {{ col_end }},
        data = data,
        excl_stub = FALSE
      )

    data <-
      cols_hide(
        data = data,
        columns = dplyr::all_of(col_end)
      )
  }

  data
}

cols_merge_resolver <- function(
    data,
    col_begin,
    col_end
) {

  # Get the columns supplied in `col_begin` as a character vector
  col_begin <-
    resolve_cols_c(
      expr = {{ col_begin }},
      data = data,
      excl_stub = FALSE
    )

  # Get the columns supplied in `col_end` as a character vector
  col_end <-
    resolve_cols_c(
      expr = {{ col_end }},
      data = data,
      excl_stub = FALSE
    )

  columns <- c(col_begin, col_end)

  list(
    columns = columns,
    pattern = "{1}{sep}{2}"
  )
}

#' Merge two columns to combine counts and percentages
#'
#' @description
#'
#' `cols_merge_n_pct()` is a specialized variant of [cols_merge()],
#' It operates by taking two columns that constitute both a count (`col_n`) and
#' a fraction of the total population (`col_pct`) and merges them into a single
#' column. What results is a column containing both counts and their associated
#' percentages (e.g., `12 (23.2%)`). The column specified in `col_pct` is
#' dropped from the output table.
#'
#' @inheritParams cols_align
#'
#' @param col_n *Column to target for counts*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The column that contains values for the count component. While select
#'   helper functions such as [starts_with()] and [ends_with()] can be used for
#'   column targeting, it's recommended that a single column name be used. This
#'   is to ensure that exactly one column is provided here.
#'
#' @param col_pct *Column to target for percentages*
#'
#'   `<column-targeting expression>` // **required**
#'
#'   The column that contains values for the percentage component. While select
#'   helper functions such as [starts_with()] and [ends_with()] can be used for
#'   column targeting, it's recommended that a single column name be used. This
#'   is to ensure that exactly one column is provided here. This column should
#'   be formatted such that percentages are displayed (e.g., with
#'   `fmt_percent()`).
#'
#' @param rows *Rows to target*
#'
#'   `<row-targeting expression>` // *default:* `everything()`
#'
#'   In conjunction with `columns`, we can specify which of their rows should
#'   participate in the merging process. The default [everything()] results in
#'   all rows in `columns` being formatted. Alternatively, we can supply a
#'   vector of row IDs within `c()`, a vector of row indices, or a select
#'   helper function (e.g. [starts_with()], [ends_with()], [contains()],
#'   [matches()], [num_range()], and [everything()]). We can also use
#'   expressions to filter down to the rows we need
#'   (e.g., `[colname_1] > 100 & [colname_2] < 50`).
#'
#' @param autohide *Automatic hiding of the `col_pct` column*
#'
#'   `scalar<logical>` // *default:* `TRUE`
#'
#'   An option to automatically hide the column specified as `col_pct`. Any
#'   columns with their state changed to hidden will behave the same as before,
#'   they just won't be displayed in the finalized table.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Comparison with other column-merging functions:
#'
#' This function could be somewhat replicated using [cols_merge()], however,
#' `cols_merge_n_pct()` employs the following specialized semantics for `NA`
#' and zero-value handling:
#'
#' 1. `NA`s in `col_n` result in missing values for the merged
#' column (e.g., `NA` + `10.2%` = `NA`)
#' 2. `NA`s in `col_pct` (but not `col_n`) result in
#' base values only for the merged column (e.g., `13` + `NA` = `13`)
#' 3. `NA`s both `col_n` and `col_pct` result in
#' missing values for the merged column (e.g., `NA` + `NA` = `NA`)
#' 4. If a zero (`0`) value is in `col_n` then the formatted output will be
#' `"0"` (i.e., no percentage will be shown)
#'
#' Any resulting `NA` values in the `col_n` column following the merge
#' operation can be easily formatted using [sub_missing()].
#' Separate calls of [sub_missing()] can be used for the `col_n` and
#' `col_pct` columns for finer control of the replacement values. It is the
#' responsibility of the user to ensure that values are correct in both the
#' `col_n` and `col_pct` columns (this function neither generates nor
#' recalculates values in either). Formatting of each column can be done
#' independently in separate [fmt_number()] and [fmt_percent()] calls.
#'
#' This function is part of a set of four column-merging functions. The other
#' three are the general [cols_merge()] function and the specialized
#' [cols_merge_uncert()] and [cols_merge_range()] functions. These functions
#' operate similarly, where the non-target columns can be optionally hidden from
#' the output table through the `hide_columns` or `autohide` options.
#'
#' @section Examples:
#'
#' Using a summarized version of the [`pizzaplace`] dataset, let's create a
#' **gt** table that displays the counts and percentages of the top 3 pizzas
#' sold by pizza category in 2015. The `cols_merge_n_pct()` function is used to
#' merge the `n` and `frac` columns (and the `frac` column is formatted using
#' [fmt_percent()]).
#'
#' ```r
#' pizzaplace |>
#'   dplyr::group_by(name, type, price) |>
#'   dplyr::summarize(
#'     n = dplyr::n(),
#'     frac = n/nrow(pizzaplace),
#'     .groups = "drop"
#'   ) |>
#'   dplyr::arrange(type, dplyr::desc(n)) |>
#'   dplyr::group_by(type) |>
#'   dplyr::slice_head(n = 3) |>
#'   gt(
#'     rowname_col = "name",
#'     groupname_col = "type"
#'   ) |>
#'   fmt_currency(price) |>
#'   fmt_percent(frac) |>
#'   cols_merge_n_pct(
#'     col_n = n,
#'     col_pct = frac
#'   ) |>
#'   cols_label(
#'     n = md("*N* (%)"),
#'     price = "Price"
#'   ) |>
#'   tab_style(
#'     style = cell_text(font = "monospace"),
#'     locations = cells_stub()
#'   ) |>
#'   tab_stubhead(md("Cat. and  \nPizza Code")) |>
#'   tab_header(title = "Top 3 Pizzas Sold by Category in 2015") |>
#'   tab_options(table.width = px(512))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_merge_n_pct_1.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-17
#'
#' @section Function Introduced:
#' `v0.3.0` (May 12, 2021)
#'
#' @export
cols_merge_n_pct <- function(
    data,
    col_n,
    col_pct,
    rows = everything(),
    autohide = TRUE
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  resolved <-
    cols_merge_resolver(
      data = data,
      col_begin = {{ col_n }},
      col_end = {{ col_pct }}
    )

  # Resolve the rows supplied in the `rows` argument
  resolved_rows_idx <-
    resolve_rows_i(
      expr = {{ rows }},
      data = data
    )

  # Create an entry and add it to the `_col_merge` attribute
  data <-
    dt_col_merge_add(
      data = data,
      col_merge = dt_col_merge_entry(
        vars = resolved$columns,
        rows = resolved_rows_idx,
        type = "merge_n_pct",
        pattern = resolved$pattern,
        sep = ""
      )
    )

  if (isTRUE(autohide)) {

    col_pct <-
      resolve_cols_c(
        expr = {{ col_pct }},
        data = data
      )

    data <-
      cols_hide(
        data = data,
        columns = dplyr::all_of(col_pct)
      )
  }

  data
}
