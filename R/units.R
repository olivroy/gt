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


# fmt_units() ------------------------------------------------------------------
#' Format measurement units
#'
#' @description
#'
#' `fmt_units()` lets you better format measurement units in the table body.
#' These must conform to **gt**'s specialized units notation (e.g.,
#' `"J Hz^-1 mol^-1"` can be used to generate units for the
#' *molar Planck constant*) for the best conversion. The notation here provides
#' several conveniences for defining units, so as long as the values to be
#' formatted conform to this syntax, you'll obtain nicely-formatted units no
#' matter what the table output format might be (i.e., HTML, LaTeX, RTF, etc.).
#' Details pertaining to the units notation can be found in the section entitled
#' *How to use **gt**'s units notation*.
#'
#' @inheritParams fmt_number
#'
#' @return An object of class `gt_tbl`.
#'
#' @section How to use **gt**'s units notation:
#'
#' The units notation involves a shorthand of writing units that feels familiar
#' and is fine-tuned for the task at hand. Each unit is treated as a separate
#' entity (parentheses and other symbols included) and the addition of subscript
#' text and exponents is flexible and relatively easy to formulate. This is all
#' best shown with examples:
#'
#' - `"m/s"` and `"m / s"` both render as `"m/s"`
#' - `"m s^-1"` will appear with the `"-1"` exponent intact
#' - `"m /s"` gives the same result, as `"/<unit>"` is equivalent to
#'   `"<unit>^-1"`
#' - `"E_h"` will render an `"E"` with the `"h"` subscript
#' - `"t_i^2.5"` provides a `t` with an `"i"` subscript and a `"2.5"` exponent
#' - `"m[_0^2]"` will use overstriking to set both scripts vertically
#' - `"g/L %C6H12O6%"` uses a chemical formula (enclosed in a pair of `"%"`
#'   characters) as a unit partial, and the formula will render correctly with
#'   subscripted numbers
#' - Common units that are difficult to write using ASCII text may be implicitly
#'   converted to the correct characters (e.g., the `"u"` in `"ug"`, `"um"`,
#'   `"uL"`, and `"umol"` will be converted to the Greek *mu* symbol; `"degC"`
#'   and `"degF"` will render a degree sign before the temperature unit)
#' - We can transform shorthand symbol/unit names enclosed in `":"` (e.g.,
#'   `":angstrom:"`, `":ohm:"`, etc.) into proper symbols
#' - Greek letters can added by enclosing the letter name in `":"`; you can
#'   use lowercase letters (e.g., `":beta:"`, `":sigma:"`, etc.) and uppercase
#'   letters too (e.g., `":Alpha:"`, `":Zeta:"`, etc.)
#' - The components of a unit (unit name, subscript, and exponent) can be
#'   fully or partially italicized/emboldened by surrounding text with `"*"` or
#'   `"**"`
#'
#' @section Examples:
#'
#' Let's use the [`illness`] dataset and create a new **gt** table. The `units`
#' column contains character values in **gt**'s specialized units notation
#' (e.g., `"x10^9 / L"`) so the `fmt_units()` function was used to better format
#' those units.
#'
#' ```r
#' illness |>
#'   gt() |>
#'   fmt_units(columns = units) |>
#'   sub_missing(columns = -starts_with("norm")) |>
#'   sub_missing(columns = c(starts_with("norm"), units), missing_text = "") |>
#'   sub_large_vals(rows = test == "MYO", threshold = 1200) |>
#'   fmt_number(
#'     decimals = 2,
#'     drop_trailing_zeros = TRUE
#'   ) |>
#'   tab_header(title = "Laboratory Findings for the YF Patient") |>
#'   tab_spanner(label = "Day", columns = starts_with("day")) |>
#'   cols_label_with(fn = ~ gsub("day_", "", .)) |>
#'   cols_merge_range(col_begin = norm_l, col_end = norm_u) |>
#'   cols_label(
#'     starts_with("norm") ~ "Normal Range",
#'     test ~ "Test",
#'     units ~ "Units"
#'   ) |>
#'   cols_width(
#'     starts_with("day") ~ px(80),
#'     everything() ~ px(120)
#'   ) |>
#'   tab_style(
#'     style = cell_text(align = "center"),
#'     locations = cells_column_labels(columns = starts_with("day"))
#'   ) |>
#'   tab_style(
#'     style = cell_fill(color = "aliceblue"),
#'     locations = cells_body(columns = c(test, units))
#'   ) |>
#'   opt_vertical_padding(scale = 0.4) |>
#'   opt_align_table_header(align = "left") |>
#'   tab_options(heading.padding = px(10))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_fmt_units_1.png")`
#' }}
#'
#' The [`constants`] dataset contains values for hundreds of fundamental
#' physical constants. We'll take a subset of values that have some molar basis
#' and generate a **gt** table from that. Like the [`illness`] dataset, this one
#' has a `units` column so, again, the `fmt_units()` function will be used to
#' format those units. Here, the preference for typesetting measurement units is
#' to have positive and negative exponents (e.g., not `"<unit_1> / <unit_2>"`
#' but rather `"<unit_1> <unit_2>^-1"`).
#'
#' ```r
#' constants |>
#'   dplyr::filter(grepl("molar", name)) |>
#'   gt() |>
#'   cols_hide(columns = c(uncert, starts_with("sf"))) |>
#'   fmt_units(columns = units) |>
#'   fmt_scientific(columns = value, decimals = 3) |>
#'   tab_header(title = "Physical Constants Having a Molar Basis") |>
#'   tab_options(column_labels.hidden = TRUE)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_fmt_units_2.png")`
#' }}
#'
#' @family data formatting functions
#' @section Function ID:
#' 3-19
#'
#' @section Function Introduced:
#' `v0.10.0` (October 7, 2023)
#'
#' @export
fmt_units <- function(
    data,
    columns = everything(),
    rows = everything()
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = data)

  valid_class <- c("character", "factor")
  check_columns_valid_if_strict(data, {{ columns }}, valid_class)

  # Pass `data`, `columns`, `rows`, and the formatting
  # functions as a function list to `fmt()`
  fmt(
    data = data,
    columns = {{ columns }},
    rows = {{ rows }},
    fns = list(
      html = function(x) {
        format_units_by_context(x, context = "html")
      },
      latex = function(x) {
        format_units_by_context(x, context = "latex")
      },
      rtf = function(x) {
        format_units_by_context(x, context = "rtf")
      },
      word = function(x) {
        format_units_by_context(x, context = "word")
      },
      default = function(x) {
        format_units_by_context(x, context = "plain")
      }
    )
  )
}

# cols_units() -----------------------------------------------------------------
#' Define units for one or more columns
#'
#' @description
#'
#' Column labels can sometimes contain measurement units, and these might range
#' from easy to define and typeset (e.g., `"m/s"`) to very difficult. Such
#' difficulty can arise from the need to include subscripts or superscripts,
#' non-ASCII symbols, etc. The `cols_units()` function tries to make this task
#' easier by letting you apply text pertaining to units to various columns. This
#' takes advantage of **gt**'s specialized units notation (e.g.,
#' `"J Hz^-1 mol^-1"` can be used to generate units for the
#' *molar Planck constant*). The notation here provides several conveniences for
#' defining units, letting you produce the correct formatting no matter what the
#' table output format might be (i.e., HTML, LaTeX, RTF, etc.). Details
#' pertaining to the units notation can be found in the section entitled
#' *How to use **gt**'s units notation* in [fmt_units()].
#'
#' @inheritParams fmt_number
#'
#' @param ... *Column units definitions*
#'
#'   `<multiple expressions>` // **required** (or, use `.list`)
#'
#'   Expressions for the assignment of column units for the table columns in
#'   `.data`. Two-sided formulas (e.g., `<LHS> ~ <RHS>`) can be used, where the
#'   left-hand side corresponds to selections of columns and the right-hand side
#'   evaluates to single-length values for the units to apply. Column names
#'   should be enclosed in `c()`. Select helpers like [starts_with()],
#'   [ends_with()], [contains()], [matches()], and [everything()] can be used in
#'   the LHS. Named arguments are also valid as input for simple mappings of
#'   column name to the **gt** units syntax; they should be of the form
#'   `<column name> = <units text>`. Subsequent expressions that operate on the
#'   columns assigned previously will result in overwriting column units
#'   defintion values.
#'
#' @param .list *Alternative to `...`*
#'
#'   `<list of multiple expressions>` // **required** (or, use `...`)
#'
#'   Allows for the use of a list as an input alternative to `...`.
#'
#' @param .units_pattern *Pattern to combine column labels and units*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   An optional pattern to be used for combining column labels with the defined
#'   units. The default pattern is `"{1}, {2}"`, where `"{1}"` refers to the
#'   column label text and `"{2}"` is the text related to the associated units.
#'   This default can be modified through the `column_labels.units_pattern`
#'   option found in [tab_options()]. Setting a value here will provide an
#'   override to the `column_labels.units_pattern` default (only for the
#'   resolved columns in the invocation of `cols_units()`).
#'
#' @return An object of class `gt_tbl`.
#'
#' @section How to use **gt**'s units notation:
#'
#' The units notation involves a shorthand of writing units that feels familiar
#' and is fine-tuned for the task at hand. Each unit is treated as a separate
#' entity (parentheses and other symbols included) and the addition of subscript
#' text and exponents is flexible and relatively easy to formulate. This is all
#' best shown with examples:
#'
#' - `"m/s"` and `"m / s"` both render as `"m/s"`
#' - `"m s^-1"` will appear with the `"-1"` exponent intact
#' - `"m /s"` gives the same result, as `"/<unit>"` is equivalent to
#'   `"<unit>^-1"`
#' - `"E_h"` will render an `"E"` with the `"h"` subscript
#' - `"t_i^2.5"` provides a `t` with an `"i"` subscript and a `"2.5"` exponent
#' - `"m[_0^2]"` will use overstriking to set both scripts vertically
#' - `"g/L %C6H12O6%"` uses a chemical formula (enclosed in a pair of `"%"`
#'   characters) as a unit partial, and the formula will render correctly with
#'   subscripted numbers
#' - Common units that are difficult to write using ASCII text may be implicitly
#'   converted to the correct characters (e.g., the `"u"` in `"ug"`, `"um"`,
#'   `"uL"`, and `"umol"` will be converted to the Greek *mu* symbol; `"degC"`
#'   and `"degF"` will render a degree sign before the temperature unit)
#' - We can transform shorthand symbol/unit names enclosed in `":"` (e.g.,
#'   `":angstrom:"`, `":ohm:"`, etc.) into proper symbols
#' - Greek letters can added by enclosing the letter name in `":"`; you can
#'   use lowercase letters (e.g., `":beta:"`, `":sigma:"`, etc.) and uppercase
#'   letters too (e.g., `":Alpha:"`, `":Zeta:"`, etc.)
#' - The components of a unit (unit name, subscript, and exponent) can be
#'   fully or partially italicized/emboldened by surrounding text with `"*"` or
#'   `"**"`
#'
#' @section Examples:
#'
#' Let's analyze some [`pizzaplace`] data with **dplyr** and then make a **gt**
#' table. Here we are separately defining new column labels with [cols_label()]
#' and then defining the units (to combine to those labels) through
#' `cols_units()`. The default pattern for combination is `"{1}, {2}"` which
#' is acceptable here.
#'
#' ```r
#' pizzaplace |>
#'   dplyr::mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
#'   dplyr::group_by(month) |>
#'   dplyr::summarize(
#'     n_sold = dplyr::n(),
#'     rev = sum(price)
#'   ) |>
#'   dplyr::mutate(chg = (rev - dplyr::lag(rev)) / dplyr::lag(rev)) |>
#'   dplyr::mutate(month = as.character(month)) |>
#'   gt(rowname_col = "month") |>
#'   fmt_integer(columns = n_sold) |>
#'   fmt_currency(columns = rev, use_subunits = FALSE) |>
#'   fmt_percent(columns = chg) |>
#'   sub_missing() |>
#'   cols_label(
#'     n_sold = "Number of Pizzas Sold",
#'     rev = "Revenue Generated",
#'     chg = "Monthly Changes in Revenue"
#'   ) |>
#'   cols_units(
#'     n_sold = "units month^-1",
#'     rev = "USD month^-1",
#'     chg = "% change *m*/*m*"
#'   ) |>
#'   cols_width(
#'     stub() ~ px(40),
#'     everything() ~ px(200)
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_units_1.png")`
#' }}
#'
#' The [`sza`] dataset has a wealth of information and here we'll generate
#' a smaller table that contains the average solar zenith angles at noon for
#' different months and at different northern latitudes. The column labels are
#' numbers representing the latitudes and it's convenient to apply units
#' of 'degrees north' to each of them with `cols_units()`. The extra thing we
#' wanted to do here was to ensure that the units are placed directly after
#' the column labels, and we do that with `.units_pattern = "{1}{2}"`. This
#' append the units (`"{2}"`) right to the column label (`"{1}"`).
#'
#' ```r
#' sza |>
#'   dplyr::filter(tst == "1200") |>
#'   dplyr::select(-tst) |>
#'   dplyr::arrange(desc(latitude)) |>
#'   tidyr::pivot_wider(
#'     names_from = latitude,
#'     values_from = sza
#'   ) |>
#'   gt(rowname_col = "month") |>
#'   cols_units(
#'     everything() ~ ":degree:N",
#'     .units_pattern = "{1}{2}"
#'   ) |>
#'   tab_spanner(
#'     label = "Solar Zenith Angle",
#'     columns = everything()
#'   ) |>
#'   text_transform(
#'     fn = toupper,
#'     locations = cells_stub()
#'   ) |>
#'   tab_style(
#'     style = cell_text(align = "right"),
#'     locations = cells_stub()
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_units_2.png")`
#' }}
#'
#' Taking a portion of the [`towny`] dataset, let's use spanners to describe
#' what's in the columns and use only measurement units for the column labels.
#' The columns labels that have to do with population and density information
#' will be replaced with units defined in `cols_units()`. We'll use a
#' `.units_pattern` value of `"{2}"`, which means that only the units will
#' be present (the `"{1}"`, representing the column label text, is omitted).
#' Spanners added through several invocations of [tab_spanner()] will declare
#' what the last four columns contain.
#'
#' ```r
#' towny |>
#'   dplyr::select(
#'     name, land_area_km2,
#'     ends_with("2016"), ends_with("2021")
#'   ) |>
#'   dplyr::slice_max(population_2021, n = 10) |>
#'   gt(rowname_col = "name") |>
#'   tab_stubhead(label = "City") |>
#'   fmt_integer() |>
#'   cols_label(
#'     land_area_km2 ~ "Area, {{km^2}}",
#'     starts_with("population") ~ "",
#'     starts_with("density") ~ ""
#'   ) |>
#'   cols_units(
#'     starts_with("population") ~ "*ppl*",
#'     starts_with("density") ~ "*ppl* km^-2",
#'     .units_pattern = "{2}"
#'   ) |>
#'   tab_spanner(
#'     label = "Population",
#'     columns = starts_with("population"),
#'     gather = FALSE
#'   ) |>
#'   tab_spanner(
#'     label = "Density",
#'     columns = starts_with("density"),
#'     gather = FALSE
#'   ) |>
#'   tab_spanner(
#'     label = "2016",
#'     columns = ends_with("2016"),
#'     gather = FALSE
#'   ) |>
#'   tab_spanner(
#'     label = "2021",
#'     columns = ends_with("2021"),
#'     gather = FALSE
#'   ) |>
#'   tab_style(
#'     style = cell_text(align = "center"),
#'     locations = cells_column_labels(
#'       c(starts_with("population"), starts_with("density"))
#'     )
#'   ) |>
#'   cols_width(everything() ~ px(120)) |>
#'   opt_horizontal_padding(scale = 3)
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_cols_units_3.png")`
#' }}
#'
#' @family column modification functions
#' @section Function ID:
#' 5-6
#'
#' @section Function Introduced:
#' `v0.10.0` (October 7, 2023)
#'
#' @export
cols_units <- function(
    .data,
    ...,
    .list = list2(...),
    .units_pattern = NULL
) {

  # Perform input object validation
  stop_if_not_gt_tbl(data = .data)

  # Collect a list of column units
  units_list <- .list

  column_vars <- dt_boxhead_get_vars(data = .data)

  # If nothing is provided, return `data` unchanged
  if (length(units_list) == 0) {
    return(.data)
  }

  for (i in seq_along(units_list)) {

    units_i <- units_list[i]

    # When input is provided as a list in `.list`, we obtain named vectors;
    # upgrade this to a list to match the input collected from `...`
    if (rlang::is_named(units_i) && rlang::is_scalar_vector(units_i)) {
      units_i <- as.list(units_i)
    }

    if (
      is.list(units_i) &&
      rlang::is_named(units_i) &&
      rlang::is_scalar_vector(units_i[[1]])
    ) {

      # Get column and value
      columns <- names(units_i)
      new_units <- units_i[[1]]

      if (!(columns %in% column_vars)) {
        cli::cli_abort(c(
          "Can't find column{?s} {.var {columns}} in the data.",
          "i" = "The LHS should include column names or a tidyselect statement."
        ))
      }

    } else if (
      is.list(units_i) &&
      rlang::is_formula(units_i[[1]])
    ) {

      units_i <- units_i[[1]]

      cols <- rlang::f_lhs(units_i)

      if (is.null(cols)) {
        cli::cli_abort(c(
          "A formula supplied to `cols_units()` must be two-sided.",
          "i" = "The LHS should include column names or a tidyselect statement."
        ))
      }

      # The default use of `resolve_cols_c()` won't work here if there
      # is a table stub column (because we need to be able to set the
      # stub column width and, by default, `resolve_cols_c()` excludes
      # the stub); to prevent this exclusion, we set `excl_stub` to FALSE
      columns <-
        resolve_cols_c(
          expr = !!cols,
          data = .data
        )

      new_units <- rlang::eval_tidy(rlang::f_rhs(units_i))
    }

    # basic check on units value.
    withCallingHandlers(
      check_string(new_units, call = NULL, arg = "unit"),
      error = function(e) {
        cli::cli_abort(
          "Incorrect unit for column{?s} {.var {columns}}.",
          parent = e
        )
      })

    for (j in seq_along(columns)) {

      # For each of the resolved columns, add the units text to the boxhead
      .data <-
        dt_boxhead_edit_column_units(
          data = .data,
          var = columns[j],
          column_units = new_units
        )

      if (!is.null(.units_pattern) && !is.na(.units_pattern)) {

        .data <-
          dt_boxhead_edit_column_pattern(
            data = .data,
            var = columns[j],
            column_pattern = .units_pattern
          )
      }
    }
  }

  .data
}

#' Get a conversion factor across two measurement units of a given class
#'
#' @description
#'
#' The `unit_conversion()` helper function gives us a conversion factor for
#' transforming a value from one form of measurement units to a target form.
#' For example if you have a length value that is expressed in miles you could
#' transform that value to one in kilometers through multiplication of the value
#' by the conversion factor (in this case `1.60934`).
#'
#' For `unit_conversion()` to understand the source and destination units, you
#' need to provide a keyword value for the `from` and `to` arguments. To aid as
#' a reference for this, call [info_unit_conversions()] to display an
#' information table that contains all of the keywords for every conversion
#' type.
#'
#' @param from *Units for the input value*
#'
#'   `scalar<character>` // **required**
#'
#'   The keyword representing the units for the value that requires unit
#'   conversion. In the case where the value has units of miles, the necessary
#'   input is `"length.mile"`.
#'
#' @param to *Desired units for the value*
#'
#'   `scalar<character>` // **required**
#'
#'   The keyword representing the target units for the value with units defined
#'   in `from`. In the case where input value has units of miles and we would
#'   rather want the value to be expressed as kilometers, the `to` value should
#'   be `"length.kilometer"`.
#'
#' @return A single numerical value.
#'
#' @section Examples:
#'
#' Let's use a portion of the [`towny`] dataset and create a table showing
#' population, density, and land area for 10 municipalities. The `land_area_km2`
#' values are in units of square kilometers, however, we'd rather the values
#' were in square miles. We can convert the numeric values while formatting the
#' values with [`fmt_number()`] by using `unit_conversion()` in the `scale_by`
#' argument since the return value of that is a conversion factor (which is
#' applied to each value by multiplication). The same is done for converting the
#' 'people per square kilometer' values in `density_2021` to 'people per square
#' mile', however, the units to convert are in the denominator so the inverse
#' of the conversion factor must be used.
#'
#' ```r
#' towny |>
#'   dplyr::arrange(desc(density_2021)) |>
#'   dplyr::slice_head(n = 10) |>
#'   dplyr::select(name, population_2021, density_2021, land_area_km2) |>
#'   gt(rowname_col = "name") |>
#'   fmt_integer(columns = population_2021) |>
#'   fmt_number(
#'     columns = land_area_km2,
#'     decimals = 1,
#'     scale_by = unit_conversion(
#'       from = "area.square-kilometer",
#'       to = "area.square-mile"
#'     )
#'   ) |>
#'   fmt_number(
#'     columns = density_2021,
#'     decimals = 1,
#'     scale_by = 1 / unit_conversion(
#'       from = "area.square-kilometer",
#'       to = "area.square-mile"
#'     )
#'   ) |>
#'   cols_label(
#'     land_area_km2 = "Land Area,<br>sq. mi",
#'     population_2021 = "Population",
#'     density_2021 = "Density,<br>ppl / sq. mi",
#'     .fn = md
#'   )
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_unit_conversion_1.png")`
#' }}
#'
#' With a small slice of the [`gibraltar`] dataset, let's display the
#' temperature values in terms of degrees Celsius (present in the data) *and* as
#' temperatures in degrees Fahrenheit (achievable via conversion). We can
#' duplicate the `temp` column through [cols_add()] (naming the new column as
#' `temp_f`) and when formatting through [fmt_integer()] we can call
#' `unit_conversion()` within the `scale_by` argument to perform this
#' transformation while formatting the values as integers.
#'
#' ```r
#' gibraltar |>
#'   dplyr::filter(
#'     date == "2023-05-15",
#'     time >= "06:00",
#'     time <= "12:00"
#'   ) |>
#'   dplyr::select(time, temp) |>
#'   gt() |>
#'   tab_header(
#'     title = "Air Temperature During Late Morning Hours at LXGB Stn.",
#'     subtitle = "May 15, 2023"
#'   ) |>
#'   cols_add(temp_f = temp) |>
#'   cols_move(columns = temp_f, after = temp) |>
#'   tab_spanner(
#'     label = "Temperature",
#'     columns = starts_with("temp")
#'   ) |>
#'   fmt_number(
#'     columns = temp,
#'     decimals = 1
#'   ) |>
#'   fmt_integer(
#'     columns = temp_f,
#'     scale_by = unit_conversion(
#'       from = "temperature.C",
#'       to = "temperature.F"
#'     )
#'   ) |>
#'   cols_label(
#'     time = "Time",
#'     temp = "{{degC}}",
#'     temp_f = "{{degF}}"
#'   ) |>
#'   cols_width(
#'     starts_with("temp") ~ px(80),
#'     time ~ px(100)
#'   ) |>
#'   opt_horizontal_padding(scale = 3) |>
#'   opt_vertical_padding(scale = 0.5) |>
#'   opt_align_table_header(align = "left") |>
#'   tab_options(heading.title.font.size = px(16))
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_unit_conversion_2.png")`
#' }}
#'
#' @family helper functions
#' @section Function ID:
#' 8-7
#'
#' @section Function Introduced:
#' `v0.11.0`
#'
#' @export
unit_conversion <- function(from, to) {

  force(from)
  force(to)

  if (from %in% temperature_keywords() && to %in% temperature_keywords()) {

    from <- normalize_temp_keyword(from)
    to <- normalize_temp_keyword(to)

    return(temperature_conversions(from = from, to = to))
  }

  if (!(from %in% conversion_factors[["from"]])) {
    cli::cli_abort("The unit supplied in {.arg from} is not known.")
  }
  if (!(to %in% conversion_factors[["to"]])) {
    cli::cli_abort("The unit supplied in {.arg to} is not known.")
  }

  if (from == to) {
    return(1.0)
  }

  row_conversion <-
    dplyr::filter(conversion_factors, from == {{ from }}, to == {{ to }})

  # In the case where units are valid and available in the internal dataset,
  # they may be across categories; such pairings do not allow for a conversion
  # to take place
  if (nrow(row_conversion) < 1) {
    cli::cli_abort("The conversion specified cannot be performed.")
  }

  row_conversion[["conv_factor"]]
}

# Units helpers ----------------------------------------------------------------

temperature_keywords <- function() {
  c(
    "temperature.celsius",
    "temp.celsius",
    "celsius",
    "temperature.C",
    "temp.C",
    "C",
    "temperature.fahrenheit",
    "temp.fahrenheit",
    "fahrenheit",
    "temperature.F",
    "temp.F",
    "F",
    "temperature.kelvin",
    "temp.kelvin",
    "kelvin",
    "temperature.K",
    "temp.K",
    "K",
    "temperature.rankine",
    "temp.rankine",
    "rankine",
    "temperature.R",
    "temp.R",
    "R"
  )
}

normalize_temp_keyword <- function(keyword) {

  switch(
    keyword,
    temperature.celsius =,
    temp.celsius =,
    celsius =,
    temperature.C =,
    temp.C =,
    C = "C",
    temperature.fahrenheit =,
    temp.fahrenheit =,
    fahrenheit =,
    temperature.F =,
    temp.F =,
    `F` = "F",
    temperature.kelvin =,
    temp.kelvin =,
    kelvin =,
    temperature.K =,
    temp.K =,
    K = "K",
    temperature.rankine =,
    temp.rankine =,
    rankine =,
    temperature.R =,
    temp.R =,
    R = "R"
  )
}

temperature_conversions <- function(from, to) {

  from_to <- paste0(from, to)

  switch(
    from_to,
    "CF" = function(x) (1.8 * x) + 32,
    "CK" = function(x) x + 273.15,
    "CR" = function(x) (1.8 * x) + 491.67,
    "FC" = function(x) (x - 32) * 5/9,
    "FK" = function(x) (x + 459.67) / 1.8,
    "FR" = function(x) x + 459.67,
    "KC" = function(x) x - 273.15,
    "KF" = function(x) ((x - 273.15) * 1.8) + 32,
    "KR" = function(x) x * 1.8,
    "RC" = function(x) (x - 32 - 459.67) / 1.8,
    "RF" = function(x) x - 459.67,
    "RK" = function(x) x / 1.8,
    "CC" = ,
    "FF" = ,
    "KK" = ,
    "RR" = 1
  )
}

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


# Create a `units_definition` object
define_units <- function(units_notation, is_chemical_formula = FALSE) {

  # Trim any incoming `{{`/`}}`
  input <- gsub("^\\{\\{\\s*|\\s*\\}\\}$", "", units_notation)

  #
  # Extract chemical notation text if present and process that
  # as redefined chem tokens
  #

  if (grepl("\\%.*\\%", input) || is_chemical_formula) {

    chem_text <- gsub(".*(\\%.*\\%).*", "\\1", input)
    chem_input <- gsub("^%|%$", "", chem_text)
    chem_input <- gsub("\\(\\^([^\\)])", "( ^\\1", chem_input)

    # Replace single bonds
    for (i in seq(1, 10)) {
      chem_input_int <- chem_input
      chem_input <- gsub("^(.*)([^ _\\{\\<]+)-([^ \\}\\>]+)(.*)$", "\\1\\2 {nsp} - {nsp} \\3\\4", chem_input)
      if (chem_input_int == chem_input) break
    }

    # Replace double bonds
    chem_input <- gsub("^([^\\<]+)=([^\\>]+)$", "\\1 {nsp}={nsp} \\2", chem_input)

    # Get a vector of chem tokens
    chem_tokens_vec <- unlist(strsplit(chem_input, split = "\\s+"))

    #
    # Process all chem tokens
    #

    # Resolve any subscripts and superscripts
    chem_tokens_vec <-
      vapply(
        chem_tokens_vec,
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE,
        FUN = function(x) {

          # Processing for simple chemical formulas (e.g., 'C7H7NO4')
          if (grepl("^[a-zA-Z0-9]+$", x) && grepl("^[A-Z]", x)) {

            # Internal subscripts
            for (i in seq(1, 10)) {
              x_str_int <- x
              x <- gsub("^(.*)([a-zA-Z])([0-9]+)([a-zA-Z])(.*)$", "\\1\\2_\\3 {nsp}\\4\\5", x)
              if (x_str_int == x) break
            }

            # Final subscript
            x <- gsub("^(.*)([a-zA-Z])([0-9]+)$", "\\1\\2_\\3", x)

            return(x)
          }

          if (grepl("[0-9]", x) && nchar(x) > 1) {

            # Preceding stoichiometric number
            x <- gsub("^([0-9]+)([a-zA-Z])(.*)$", "\\1:thinspace:\\2\\3", x)

            # Internal subscripts
            for (i in seq(1, 10)) {
              x_str_int <- x
              x <- gsub("^(.*)([a-zA-Z])([0-9]+)([a-zA-Z\\^])(.*)$", "\\1\\2_\\3 {nsp}\\4\\5", x)
              if (x_str_int == x) break
            }

            # Final subscript
            x <- gsub("^(.*)([a-zA-Z])([0-9]+)$", "\\1\\2_\\3", x)
          }

          if (grepl("\\)|\\(|\\]", x)) {

            # Subscript following ')' or ']'
            for (i in seq(1, 5)) {
              x_str_int <- x
              x <- gsub("^(.+)(\\)|\\])([0-9]+)(.*)$", "\\1\\2_\\3 {nsp}\\4", x)
              if (x_str_int == x) break
            }

            # Subscript preceding ')', '(', or ']'
            for (i in seq(1, 5)) {
              x_str_int <- x
              x <- gsub("^(.+)([0-9]+)(\\)|\\(|\\])(.*)$", "\\1_\\2 {nsp}\\3\\4", x)
              if (x_str_int == x) break
            }
          }

          # Terminating '+' or '-' denoting charge
          if (grepl("^.+([+-])$", x) && !grepl("\\^[n+-]+$", x)) {
            x <- gsub("^(.*)([a-zA-Z])([+-])$", "\\1\\2^\\3", x)
          }

          # Superscript following closing ']'
          if (grepl("]", x, fixed = TRUE) && grepl("[0-9+-]", x) && nchar(x) > 1) {
            x <- gsub("^(.*)(\\])([0-9+-]*)$", "\\1\\2^\\3", x)
          }

          # Removal of curly braces around charge value
          if (grepl("[a-zA-Z]\\^\\{.+?\\}$", x)) {
            x <- gsub("^(.*)([a-zA-Z])\\^\\{(.+?)\\}$", "\\1\\2^\\3", x)
          }

          # Conversion to subscripted, italicized 'x'
          if (grepl("_x$", x)) {
            x <- gsub("^(.*)([a-zA-Z])_x$", "\\1\\2_*x*", x)
          }

          # Standalone 'x' becomes italicized, convention for stoichiometry
          if (x == "x") {
            x <- "*x*"
          }

          # Up arrow for gas
          if (x %in% c("^", "(^)")) {
            x <- "{nsp}:uarr:"
          }

          # Down arrow for precipitate
          if (x %in% c("v", "(v)")) {
            x <- "{nsp}:darr:"
          }

          # Center dot for addition compounds (from standalone '*' or '.')
          if (x %in% c("*", ".")) {
            x <- "{nsp}:dot:{nsp}"
          }

          # The 'n' in a superscript is italicized by convention
          if (grepl("^(.*)\\^n\\+$", x)) {
            x <- sub("^(.*)\\^n\\+$", "\\1^*n*+", x)
          }

          # Isotope handling on LHS (w/ curly braces) -- '^{227}_{90}Th+'
          if (grepl("^\\^\\{([0-9+-]+)\\}_\\{([0-9+-]+)\\}.*$", x)) {
            x <- sub("^\\^\\{([0-9+-]+)\\}_\\{([0-9+-]+)\\}(.*)$", "{nsp}[_\\2^\\1] {nsp}\\3", x)
          }

          # Isotope handling on LHS (w/o curly braces) -- '^227_90Th+'
          if (grepl("^\\^([0-9+-]+)_([0-9+-]+).*$", x)) {
            x <- sub("^\\^([0-9+-]+)_([0-9+-]+)(.*)$", "{nsp}[_\\2^\\1] {nsp}\\3", x)
          }

          # Isotope handling on LHS (w/ curly braces) -- '^{227}Th+'
          if (grepl("^\\^\\{([0-9+-]+)\\}.*$", x)) {
            x <- sub("^\\^\\{([0-9+-]+)\\}(.*)$", "{nsp}[_:space:^\\1] {nsp}\\2", x)
          }

          # Isotope handling on LHS (w/0 curly braces) -- '^{227}Th+'
          if (grepl("^\\^([0-9+-]+).*$", x)) {
            x <- sub("^\\^([0-9+-]+)(.*)$", "{nsp}[_:space:^\\1] {nsp}\\2", x)
          }

          x
        }
      )

    # Resolve any reaction arrows
    chem_tokens_vec <-
      vapply(
        chem_tokens_vec,
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE,
        FUN = function(x) {
          if (grepl("<|>", x) && nchar(x) > 1) {
            x <- sub("<-->", ":lrseparr:", x, fixed = TRUE)
            x <- sub("<->", ":lrarr:", x, fixed = TRUE)
            x <- sub("->", ":rarr:", x, fixed = TRUE)
            x <- sub("<-", ":larr:", x, fixed = TRUE)
            x <- sub("<=>>", ":eqmrarr:", x, fixed = TRUE)
            x <- sub("<<=>", ":eqmlarr:", x, fixed = TRUE)
            x <- sub("<=>", ":eqmarr:", x, fixed = TRUE)
          }

          x
        }
      )

    # TODO: add text above and/or below reaction arrows through addition
    # of suitable <text> element(s)
    # <-->
    # <text x="39.5" y="-15%" style="font-size: 10; text-anchor: middle;">[TEXT_ABOVE]</text>

    # Insert chem tokens back into input
    chem_tokens_str <- paste(chem_tokens_vec, collapse = " ")

    input <- sub(chem_text, chem_tokens_str, input, fixed = TRUE)

    return(define_units(units_notation = input))
  }

  # Get a vector of raw tokens
  tokens_vec <- unlist(strsplit(input, split = " "))

  # Remove any empty tokens
  tokens_vec <- tokens_vec[tokens_vec != ""]

  # Replace any instances of `/<text>` with `<text>^-1`
  tokens_vec <-
    vapply(
      tokens_vec,
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE,
      FUN = function(x) {
        if (grepl("^/", x) && nchar(x) > 1) {
          x <- gsub("^/", "", x)
          x <- paste0(x, "^-1")
        }
        x
      }
    )

  units_list <- list()

  for (i in seq_along(tokens_vec)) {

    tokens_vec_i <- tokens_vec[i]

    unit_subscript <- NA_character_
    sub_super_overstrike <- FALSE
    chemical_formula <- FALSE
    exponent <- NULL

    if (
      is_chemical_formula ||
      (
        grepl("^%", tokens_vec_i) &&
        grepl("%$", tokens_vec_i) &&
        nchar(tokens_vec_i) > 2
      )
    ) {
      # Case where the unit is marked as a chemical formula

      chemical_formula <- TRUE

      # Extract the formula w/o the surrounding `%` signs
      unit <- gsub("^%|%$", "", tokens_vec_i)

    } else if (grepl(".+?\\[_.+?\\^.+?\\]", tokens_vec_i)) {
      # Case where both a subscript and exponent are present and
      # an overstrike arrangement is necessary

      sub_super_overstrike <- TRUE

      # Extract the unit w/o subscript from the string
      unit <- gsub("(.+?)\\[_.+?\\^.+?\\]", "\\1", tokens_vec_i)

      # Obtain only the subscript/exponent of the string
      sub_exponent <- gsub(".+?\\[(_.+?\\^.+?)\\]", "\\1", tokens_vec_i)

      # Extract the content after the underscore but terminate
      # before any `^`; this is the subscript
      unit_subscript <- gsub("^_(.+?)(\\^.+?)$", "\\1", sub_exponent)

      # Extract the content after the caret but terminate before
      # any `_`; this is the exponent
      exponent <- gsub("_.+?\\^(.+?)", "\\1", sub_exponent)

    } else if (grepl(".+?_.+?\\^.+?", tokens_vec_i)) {
      # Case where both a subscript and exponent are present and
      # the subscript is set before the exponent

      # Extract the unit w/o subscript from the string
      unit <- gsub("^(.+?)_.+?\\^.+?$", "\\1", tokens_vec_i)

      # Obtain only the subscript/exponent portion of the string
      sub_exponent <- gsub("^.+?(_.+?\\^.+?)$", "\\1", tokens_vec_i)

      # Extract the content after the underscore but terminate
      # before any `^`; this is the subscript
      unit_subscript <- gsub("^_(.+?)\\^.+?$", "\\1", sub_exponent)

      # Extract the content after the caret but terminate before
      # any `_`; this is the exponent
      exponent <- gsub("^_.+?\\^(.+?)$", "\\1", sub_exponent)

    } else if (grepl("^", tokens_vec_i, fixed = TRUE)) {
      # Case where only an exponent is present

      tokens_vec_i_split <- unlist(strsplit(tokens_vec_i, "^", fixed = TRUE))

      unit <- tokens_vec_i_split[1]
      exponent <- tokens_vec_i_split[2]

    } else if (grepl("_", tokens_vec_i, fixed = TRUE)) {
      # Case where only a subscript is present

      tokens_vec_i_split <- unlist(strsplit(tokens_vec_i, "_", fixed = TRUE))

      unit <- tokens_vec_i_split[1]
      unit_subscript <- tokens_vec_i_split[2]

    } else {
      unit <- tokens_vec_i
    }

    units_list[[length(units_list) + 1]] <-
      units_list_item(
        token = tokens_vec_i,
        unit = unit,
        unit_subscript = unit_subscript,
        exponent = exponent,
        chemical_formula = chemical_formula,
        sub_super_overstrike = sub_super_overstrike
      )
  }

  names(units_list) <- tokens_vec
  class(units_list) <- "units_definition"

  units_list
}

right_arrow_svg <- "<svg viewBox=\"0 0 86 11\" style=\"overflow:visible;width:2.5em;margin-bottom:0.15em;\"><g stroke=\"none\" stroke-width=\"1\" fill=\"none\" fill-rule=\"evenodd\"><g transform=\"translate(43.004359, 5.903074) rotate(-180.000000) translate(-43.004359, -5.903074) translate(3.611719, 1.837074)\" fill=\"#010101\"><rect x=\"13.6378344\" y=\"3.381\" width=\"65.147447\" height=\"1.37\"></rect><path d=\"M16.772,4.066 L6.26228535e-14,0 C1.2,1.009 1.896,2.496 1.896,4.066 C1.896,5.634 1.201,7.125 6.26228535e-14,8.132 L16.772,4.066 Z\" fill-rule=\"nonzero\" transform=\"translate(8.386000, 4.066000) scale(-1, 1) translate(-8.386000, -4.066000)\"></path></g></g></svg>"
left_arrow_svg  <- "<svg viewBox=\"0 0 86 11\" style=\"overflow:visible;width:2.5em;margin-bottom:0.15em;\"><g stroke=\"none\" stroke-width=\"1\" fill=\"none\" fill-rule=\"evenodd\"><g transform=\"translate(3.000000, 1.903074)\" fill=\"#010101\"><rect x=\"13.2427961\" y=\"3.381\" width=\"65.6275819\" height=\"1.37\"></rect><path d=\"M16.772,4.066 L0,0 C1.2,1.009 1.896,2.496 1.896,4.066 C1.896,5.634 1.201,7.125 0,8.132 L16.772,4.066 Z\" fill-rule=\"nonzero\" transform=\"translate(8.386000, 4.066000) scale(-1, 1) translate(-8.386000, -4.066000)\"></path></g></g></svg>"
lr_arrow_svg <- "<svg viewBox=\"0 0 86 11\" style=\"overflow:visible;width:2.5em;margin-bottom:0.15em;\"><g stroke=\"none\" stroke-width=\"1\" fill=\"none\" fill-rule=\"evenodd\"><rect fill=\"#010101\" x=\"16.2427961\" y=\"5.28407392\" width=\"53\" height=\"1.37\"></rect><path d=\"M82.397,5.96907392 L65.625,1.90307392 C66.825,2.91207392 67.521,4.39907392 67.521,5.96907392 C67.521,7.53707392 66.826,9.02807392 65.625,10.0350739 L82.397,5.96907392 Z\" id=\"right_arrow\" fill=\"#010101\" fill-rule=\"nonzero\"></path><path d=\"M19.772,5.96907392 L3,1.90307392 C4.2,2.91207392 4.896,4.39907392 4.896,5.96907392 C4.896,7.53707392 4.201,9.02807392 3,10.0350739 L19.772,5.96907392 Z\" fill=\"#010101\" fill-rule=\"nonzero\" transform=\"translate(11.386000, 5.969074) scale(-1, 1) translate(-11.386000, -5.969074)\"></path></g></svg>"
lr_sep_arrow_svg <- "<svg viewBox=\"0 0 86 20\" style=\"overflow:visible;width:2.5em;margin-bottom:0.02em;\"><g stroke=\"none\" stroke-width=\"1\" fill=\"none\" fill-rule=\"evenodd\"><g transform=\"translate(3.000000, 2.487191)\" fill=\"#010101\"><rect id=\"line\" x=\"13.2427961\" y=\"10.381\" width=\"63\" height=\"1.37\"></rect><path d=\"M16.772,11.066 L0,7 C1.2,8.009 1.896,9.496 1.896,11.066 C1.896,12.634 1.201,14.125 0,15.132 L16.772,11.066 Z\" fill-rule=\"nonzero\" transform=\"translate(8.386000, 11.066000) scale(-1, 1) translate(-8.386000, -11.066000)\"></path><rect x=\"3.24279614\" y=\"3.381\" width=\"63\" height=\"1.37\"></rect><path d=\"M79.397,4.066 L62.625,0 C63.825,1.009 64.521,2.496 64.521,4.066 C64.521,5.634 63.826,7.125 62.625,8.132 L79.397,4.066 Z\" fill-rule=\"nonzero\"></path></g></g></svg>"
eqm_arrows_svg <- "<svg viewBox=\"0 0 86 15\" style=\"overflow:visible;width:2.5em;margin-bottom:0.1em;\"><g stroke=\"none\" stroke-width=\"1\" fill=\"none\" fill-rule=\"evenodd\"><g transform=\"translate(6.242796, 1.870000)\" fill=\"#010101\"><rect x=\"0.501391193\" y=\"3.63\" width=\"62.4986088\" height=\"1.37\"></rect><path d=\"M72.8288191,5 L56.0567904,0 C57.2567904,1.24077718 57.9527904,3.06935563 57.9527904,5 L72.8288191,5 Z\" fill-rule=\"nonzero\"></path><rect x=\"10\" y=\"6.64830768\" width=\"62.5\" height=\"1.37\"></rect><path d=\"M16.7720287,11.6483077 L0,6.64830768 C1.2,7.88908486 1.896,9.71766331 1.896,11.6483077 L16.7720287,11.6483077 Z\" fill-rule=\"nonzero\" transform=\"translate(8.386014, 9.148308) scale(-1, -1) translate(-8.386014, -9.148308)\"></path></g></g></svg>"
eqm_arrows_right_svg <- "<svg viewBox=\"0 0 86 15\" style=\"overflow:visible;width:2.5em;margin-bottom:0.1em;\"><g stroke=\"none\" stroke-width=\"1\" fill=\"none\" fill-rule=\"evenodd\"><g transform=\"translate(6.744187, 1.454117)\" fill=\"#010101\"><rect x=\"0\" y=\"3.63\" width=\"62.4986088\" height=\"1.37\"></rect><path d=\"M72.3274279,5 L55.5553992,-1.13686838e-13 C56.7553992,1.24077718 57.4513992,3.06935563 57.4513992,5 L72.3274279,5 Z\" fill-rule=\"nonzero\"></path><rect x=\"16.4986088\" y=\"6.64830768\" width=\"44.5\" height=\"1.37\"></rect><path d=\"M23.2706375,11.6483077 L6.49860881,6.64830768 C7.69860881,7.88908486 8.39460881,9.71766331 8.39460881,11.6483077 L23.2706375,11.6483077 Z\" fill-rule=\"nonzero\" transform=\"translate(14.884623, 9.148308) scale(-1, -1) translate(-14.884623, -9.148308)\"></path></g></g></svg>"
eqm_arrows_left_svg <- "<svg viewBox=\"0 0 86 15\" style=\"overflow:visible;width:2.5em;margin-bottom:0.1em;\"><g stroke=\"none\" stroke-width=\"1\" fill=\"none\" fill-rule=\"evenodd\"><g transform=\"translate(6.242796, 1.972234)\" fill=\"#010101\"><rect x=\"11.5026504\" y=\"3.63\" width=\"44.4973496\" height=\"1.37\"></rect><path d=\"M65.8288191,5 L49.0567904,-1.13686838e-13 C50.2567904,1.24077718 50.9527904,3.06935563 50.9527904,5 L65.8288191,5 Z\" fill-rule=\"nonzero\"></path><rect x=\"10\" y=\"6.64830768\" width=\"62.5\" height=\"1.37\"></rect><path d=\"M16.7720287,11.6483077 L0,6.64830768 C1.2,7.88908486 1.896,9.71766331 1.896,11.6483077 L16.7720287,11.6483077 Z\" fill-rule=\"nonzero\" transform=\"translate(8.386014, 9.148308) scale(-1, -1) translate(-8.386014, -9.148308)\"></path></g></g></svg>"

units_list_item <- function(
    token,
    unit,
    unit_subscript = NULL,
    exponent = NULL,
    sub_super_overstrike = FALSE,
    chemical_formula = FALSE
) {

  list_item <-
    list(
      token = token,
      unit = unit,
      unit_subscript = NA_character_,
      exponent = NA_character_,
      sub_super_overstrike = FALSE,
      chemical_formula = FALSE
    )

  if (!is.null(exponent)) {
    list_item[["exponent"]] <- exponent
  }

  if (!is.null(unit_subscript)) {
    list_item[["unit_subscript"]] <- unit_subscript
  }

  list_item[["sub_super_overstrike"]] <- sub_super_overstrike
  list_item[["chemical_formula"]] <- chemical_formula

  list_item
}


# Render a `units_definition` object to string
render_units <- function(units_object, context = "html") {

  for (i in seq_along(units_object)) {

    units_str_i <- ""

    units_object_i <- units_object[[i]]
    unit <- units_object_i[["unit"]]
    unit_subscript <- units_object_i[["unit_subscript"]]
    exponent <- units_object_i[["exponent"]]
    sub_super_overstrike <- units_object_i[["sub_super_overstrike"]]
    chemical_formula <- units_object_i[["chemical_formula"]]

    if (context == "latex") {
      unit <- escape_latex(unit)
      unit_subscript <- escape_latex(unit_subscript)
      exponent <- escape_latex(exponent)
    }

    if (
      context %in% c("html", "latex") &&
      grepl("x10", unit) &&
      !chemical_formula
    ) {
      unit <- gsub("x", "&times;", unit, fixed = TRUE)
    }

    unit <- units_symbol_replacements(text = unit, context = context)

    # Process Markdown for different components
    if (!is.na(unit) && nchar(unit) > 2 && grepl("*", unit)) {

      if (context == "html") {
        unit <- commonmark::markdown_html(text = unit)
        unit <- gsub("^<p>|</p>\n$", "", unit)
      } else if (context == "latex") {
        unit <- commonmark::markdown_latex(text = unit)
        unit <- gsub("\n$", "", unit)
      } else if (context == "rtf") {
        unit <- markdown_to_rtf(text = unit)
      }
    }

    if (!is.na(unit_subscript) && nchar(unit_subscript) > 2 && grepl("*", unit_subscript)) {

      unit_subscript <- units_symbol_replacements(text = unit_subscript, context = context)

      if (context == "html") {
        unit_subscript <- commonmark::markdown_html(text = unit_subscript)
        unit_subscript <- gsub("^<p>|</p>\n$", "", unit_subscript)
      } else if (context == "latex") {
        unit_subscript <- commonmark::markdown_latex(text = unit_subscript)
        unit_subscript <- gsub("\n$", "", unit_subscript)
      } else if (context == "rtf") {
        unit_subscript <- markdown_to_rtf(text = unit_subscript)
      }
    }

    if (!is.na(exponent) && nchar(exponent) > 2 && grepl("*", exponent)) {

      exponent <- units_symbol_replacements(text = exponent, context = context)

      if (context == "html") {
        exponent <- commonmark::markdown_html(text = exponent)
        exponent <- gsub("^<p>|</p>\n$", "", exponent)
      } else if (context == "latex") {
        exponent <- commonmark::markdown_latex(text = exponent)
        exponent <- gsub("\n$", "", exponent)
      } else if (context == "rtf") {
        exponent <- markdown_to_rtf(text = exponent)
      }
    }

    units_str_i <- paste0(units_str_i, unit)

    # Overstriking of subscripts and superscripts is only possible
    # for the `"html"` context; deactivate this for any other context
    if (sub_super_overstrike && context != "html") {
      sub_super_overstrike <- FALSE
    }

    if (
      sub_super_overstrike &&
      !is.na(unit_subscript) &&
      !is.na(exponent)
    ) {

      if (context == "html") {
        exponent <- gsub("-", "&minus;", exponent)
      } else if (context == "latex") {
        exponent <- gsub("-", "--", exponent)
      }

      text_align <- if (units_str_i == "{nsp}") "right" else "left"

      units_str_i <-
        paste0(
          units_str_i,
          units_html_sub_super(
            content_sub = unit_subscript,
            content_sup = exponent,
            text_align = text_align
          )
        )

    } else {

      if (!is.na(unit_subscript)) {

        unit_subscript <-
          units_to_subscript(content = unit_subscript, context = context)

        units_str_i <- paste0(units_str_i, unit_subscript)
      }

      if (!is.na(exponent)) {

        if (context == "html") {
          exponent <- gsub("-", "&minus;", exponent)
        } else if (context == "latex") {
          exponent <- gsub("-", "--", exponent)
        }

        exponent <- units_to_superscript(content = exponent, context = context)

        units_str_i <- paste0(units_str_i, exponent)
      }
    }

    units_object[[i]][["built"]] <- units_str_i
  }

  units_str <- ""

  for (i in seq_along(units_object)) {

    unit_add <- units_object[[i]][["built"]]

    if (grepl("\\($|\\[$", units_str) || grepl("^\\)|^\\]", unit_add)) {
      spacer <- ""
    } else {
      spacer <- " "
    }

    # Treat special case where two simple units on both sides of a solidus
    # should have no extra spacing (e.g., 'm / s' -> 'm/s')
    if (length(units_object) == 3 && units_object[[2]][["unit"]] == "/") {
      spacer <- ""
    }

    units_str <- paste0(units_str, spacer, unit_add)
  }

  units_str <- gsub("^\\s+|\\s+$", "", units_str)
  units_str <- gsub("\\s*\\{nsp\\}\\s*", "", units_str)
  units_str <- gsub(":rarr:", right_arrow_svg, units_str)
  units_str <- gsub(":larr:", left_arrow_svg, units_str)
  units_str <- gsub(":lrarr:", lr_arrow_svg, units_str)
  units_str <- gsub(":lrseparr:", lr_sep_arrow_svg, units_str)
  units_str <- gsub(":eqmarr:", eqm_arrows_svg, units_str)
  units_str <- gsub(":eqmrarr:", eqm_arrows_right_svg, units_str)
  units_str <- gsub(":eqmlarr:", eqm_arrows_left_svg, units_str)

  units_str
}

units_to_superscript <- function(content, context = "html") {

  if (context == "html") {

    out <-
      paste0(
        "<span style=\"white-space:nowrap;\">",
        "<sup style=\"line-height:0;\">", content, "</sup>",
        "</span>"
      )
  }

  if (context == "latex") {
    out <- paste0("\\textsuperscript{", content, "}")
  }

  if (context == "rtf") {
    out <- paste0("\\super ", content, " \\nosupersub")
  }

  if (context == "word") {

    out <-
      as.character(
        xml_r(
          xml_rPr(
            xml_baseline_adj(
              v_align = "superscript"
            )
          ),
          xml_t(content)
        )
      )
  }

  out
}

units_to_subscript <- function(content, context = "html") {

  if (context == "html") {

    out <-
      paste0(
        "<span style=\"white-space:nowrap;\">",
        "<sub style=\"line-height:0;\">", content, "</sub>",
        "</span>"
      )
  }

  if (context == "latex") {
    out <- paste0("\\textsubscript{", content, "}")
  }

  if (context == "rtf") {
    out <- paste0("\\sub ", content, " \\nosupersub")
  }

  if (context == "word") {

    out <-
      as.character(
        xml_r(
          xml_rPr(
            xml_baseline_adj(
              v_align = "subscript"
            )
          ),
          xml_t(content)
        )
      )
  }

  out
}

units_html_sub_super <- function(
    content_sub,
    content_sup,
    text_align = "left"
) {

  paste0(
    "<span style=\"",
    "display:inline-block;",
    "line-height:1em;",
    "text-align:", text_align, ";",
    "font-size:60%;",
    "vertical-align:-0.25em;",
    "margin-left:0.1em;",
    "\">",
    content_sup,
    "<br>",
    content_sub,
    "</span>"
  )
}

units_symbol_replacements <- function(
    text,
    context = "html"
) {

  if (context == "html") {
    text <- replace_units_symbol(text, "^-", "^-", "&minus;")
  }

  if (context == "latex") {
    text <- replace_units_symbol(text, "^-", "^-", "--")
  }

  if (context %in% c("html", "rtf", "latex")) {

    text <- replace_units_symbol(text, "^um$", "um", "&micro;m")
    text <- replace_units_symbol(text, "^uL$", "uL", "&micro;L")
    text <- replace_units_symbol(text, "^umol", "^umol", "&micro;mol")
    text <- replace_units_symbol(text, "^ug$", "ug", "&micro;g")
    text <- replace_units_symbol(text, ":micro:", ":micro:", "&micro;")
    text <- replace_units_symbol(text, ":mu:", ":mu:", "&micro;")
    text <- replace_units_symbol(text, "^ohm$", "ohm", "&#8486;")
    text <- replace_units_symbol(text, ":ohm:", ":ohm:", "&#8486;")
    text <- replace_units_symbol(text, ":angstrom:", ":angstrom:", "&#8491;")
    text <- replace_units_symbol(text, ":times:", ":times:", "&times;")
    text <- replace_units_symbol(text, ":plusminus:", ":plusminus:", "&plusmn;")
    text <- replace_units_symbol(text, ":permil:", ":permil:", "&permil;")
    text <- replace_units_symbol(text, ":permille:", ":permille:", "&permil;")
    text <- replace_units_symbol(text, ":degree:", ":degree:", "&deg;")
    text <- replace_units_symbol(text, ":degrees:", ":degrees:", "&deg;")
    text <- replace_units_symbol(text, "degC", "degC", "&deg;C")
    text <- replace_units_symbol(text, "degF", "degF", "&deg;F")
    text <- replace_units_symbol(text, ":space:", ":space:", "&nbsp;")
    text <- replace_units_symbol(text, ":thinspace:", ":thinspace:", "&#8201;")
    text <- replace_units_symbol(text, ":dot:", ":dot:", "&sdot;")
    text <- replace_units_symbol(text, ":darr:", ":darr:", "&darr;")
    text <- replace_units_symbol(text, ":uarr:", ":uarr:", "&uarr;")
    text <- replace_units_symbol(text, ":Alpha:", ":Alpha:", "&Alpha;")
    text <- replace_units_symbol(text, ":alpha:", ":alpha:", "&alpha;")
    text <- replace_units_symbol(text, ":Beta:", ":Beta:", "&Beta;")
    text <- replace_units_symbol(text, ":beta:", ":beta:", "&beta;")
    text <- replace_units_symbol(text, ":Gamma:", ":Gamma:", "&Gamma;")
    text <- replace_units_symbol(text, ":gamma:", ":gamma:", "&gamma;")
    text <- replace_units_symbol(text, ":Delta:", ":Delta:", "&Delta;")
    text <- replace_units_symbol(text, ":delta:", ":delta:", "&delta;")
    text <- replace_units_symbol(text, ":Epsilon:", ":Epsilon:", "&Epsilon;")
    text <- replace_units_symbol(text, ":epsilon:", ":epsilon:", "&epsilon;")
    text <- replace_units_symbol(text, ":Zeta:", ":Zeta:", "&Zeta;")
    text <- replace_units_symbol(text, ":zeta:", ":zeta:", "&zeta;")
    text <- replace_units_symbol(text, ":Eta:", ":Eta:", "&Eta;")
    text <- replace_units_symbol(text, ":eta:", ":eta:", "&eta;")
    text <- replace_units_symbol(text, ":Theta:", ":Theta:", "&Theta;")
    text <- replace_units_symbol(text, ":theta:", ":theta:", "&theta;")
    text <- replace_units_symbol(text, ":Iota:", ":Iota:", "&Iota;")
    text <- replace_units_symbol(text, ":iota:", ":iota:", "&iota;")
    text <- replace_units_symbol(text, ":Kappa:", ":Kappa:", "&Kappa;")
    text <- replace_units_symbol(text, ":kappa:", ":kappa:", "&kappa;")
    text <- replace_units_symbol(text, ":Lambda:", ":Lambda:", "&Lambda;")
    text <- replace_units_symbol(text, ":lambda:", ":lambda:", "&lambda;")
    text <- replace_units_symbol(text, ":Mu:", ":Mu:", "&Mu;")
    text <- replace_units_symbol(text, ":mu:", ":mu:", "&mu;")
    text <- replace_units_symbol(text, ":Nu:", ":Nu:", "&Nu;")
    text <- replace_units_symbol(text, ":nu:", ":nu:", "&nu;")
    text <- replace_units_symbol(text, ":Xi:", ":Xi:", "&Xi;")
    text <- replace_units_symbol(text, ":xi:", ":xi:", "&xi;")
    text <- replace_units_symbol(text, ":Omicron:", ":Omicron:", "&Omicron;")
    text <- replace_units_symbol(text, ":omicron:", ":omicron:", "&omicron;")
    text <- replace_units_symbol(text, ":Pi:", ":Pi:", "&Pi;")
    text <- replace_units_symbol(text, ":pi:", ":pi:", "&pi;")
    text <- replace_units_symbol(text, ":Rho:", ":Rho:", "&Rho;")
    text <- replace_units_symbol(text, ":rho:", ":rho:", "&rho;")
    text <- replace_units_symbol(text, ":Sigma:", ":Sigma:", "&Sigma;")
    text <- replace_units_symbol(text, ":sigma:", ":sigma:", "&sigma;")
    text <- replace_units_symbol(text, ":sigmaf:", ":sigmaf:", "&sigmaf;")
    text <- replace_units_symbol(text, ":Tau:", ":Tau:", "&Tau;")
    text <- replace_units_symbol(text, ":tau:", ":tau:", "&tau;")
    text <- replace_units_symbol(text, ":Upsilon:", ":Upsilon:", "&Upsilon;")
    text <- replace_units_symbol(text, ":upsilon:", ":upsilon:", "&upsilon;")
    text <- replace_units_symbol(text, ":Phi:", ":Phi:", "&Phi;")
    text <- replace_units_symbol(text, ":phi:", ":phi:", "&phi;")
    text <- replace_units_symbol(text, ":Chi:", ":Chi:", "&Chi;")
    text <- replace_units_symbol(text, ":chi:", ":chi:", "&chi;")
    text <- replace_units_symbol(text, ":Psi:", ":Psi:", "&Psi;")
    text <- replace_units_symbol(text, ":psi:", ":psi:", "&psi;")
    text <- replace_units_symbol(text, ":Omega:", ":Omega:", "&Omega;")
    text <- replace_units_symbol(text, ":omega:", ":omega:", "&omega;")
  }

  if (context == "word") {

    text <- replace_units_symbol(text, "^um$", "um", paste0("\U003BC", "m"))
    text <- replace_units_symbol(text, "^uL$", "uL", paste0("\U003BC", "L"))
    text <- replace_units_symbol(text, "^umol", "^umol", paste0("\U003BC", "mol"))
    text <- replace_units_symbol(text, "^ug$", "ug", paste0("\U003BC", "g"))
    text <- replace_units_symbol(text, ":micro:", ":micro:", "\U003BC")
    text <- replace_units_symbol(text, ":mu:", ":mu:", "\U003BC")
    text <- replace_units_symbol(text, "^ohm$", "ohm", "\U02126")
    text <- replace_units_symbol(text, ":ohm:", ":ohm:", "\U02126")
    text <- replace_units_symbol(text, ":times:", ":times:", "\U000D7")
    text <- replace_units_symbol(text, ":plusminus:", ":plusminus:", "\U000B1")
    text <- replace_units_symbol(text, ":permil:", ":permil:", "\U00089")
    text <- replace_units_symbol(text, ":permille:", ":permille:", "\U00089")
    text <- replace_units_symbol(text, ":degree:", ":degree:", "\U000B0")
    text <- replace_units_symbol(text, ":degrees:", ":degrees:", "\U000B0")
    text <- replace_units_symbol(text, "degC", "degC", paste0("\U000B0", "C"))
    text <- replace_units_symbol(text, "degF", "degF", paste0("\U000B0", "F"))
    text <- replace_units_symbol(text, ":space:", ":space:", " ")
    text <- replace_units_symbol(text, ":thinspace:", ":thinspace:", "\U2009")
    text <- replace_units_symbol(text, ":Alpha:", ":Alpha:", "\U0391")
    text <- replace_units_symbol(text, ":alpha:", ":alpha:", "\U03B1")
    text <- replace_units_symbol(text, ":Beta:", ":Beta:", "\U0392")
    text <- replace_units_symbol(text, ":beta:", ":beta:", "\U03B2")
    text <- replace_units_symbol(text, ":Gamma:", ":Gamma:", "\U0393")
    text <- replace_units_symbol(text, ":gamma:", ":gamma:", "\U03B3")
    text <- replace_units_symbol(text, ":Delta:", ":Delta:", "\U0394")
    text <- replace_units_symbol(text, ":delta:", ":delta:", "\U03B4")
    text <- replace_units_symbol(text, ":Epsilon:", ":Epsilon:", "\U0395")
    text <- replace_units_symbol(text, ":epsilon:", ":epsilon:", "\U03B5")
    text <- replace_units_symbol(text, ":Zeta:", ":Zeta:", "\U0396")
    text <- replace_units_symbol(text, ":zeta:", ":zeta:", "\U03B6")
    text <- replace_units_symbol(text, ":Eta:", ":Eta:", "\U0397")
    text <- replace_units_symbol(text, ":eta:", ":eta:", "\U03B7")
    text <- replace_units_symbol(text, ":Theta:", ":Theta:", "\U0398")
    text <- replace_units_symbol(text, ":theta:", ":theta:", "\U03B8")
    text <- replace_units_symbol(text, ":Iota:", ":Iota:", "\U0399")
    text <- replace_units_symbol(text, ":iota:", ":iota:", "\U03B9")
    text <- replace_units_symbol(text, ":Kappa:", ":Kappa:", "\U039A")
    text <- replace_units_symbol(text, ":kappa:", ":kappa:", "\U03BA")
    text <- replace_units_symbol(text, ":Lambda:", ":Lambda:", "\U039B")
    text <- replace_units_symbol(text, ":lambda:", ":lambda:", "\U03BB")
    text <- replace_units_symbol(text, ":Mu:", ":Mu:", "\U039C")
    text <- replace_units_symbol(text, ":mu:", ":mu:", "\U03BC")
    text <- replace_units_symbol(text, ":Nu:", ":Nu:", "\U039D")
    text <- replace_units_symbol(text, ":nu:", ":nu:", "\U03BD")
    text <- replace_units_symbol(text, ":Xi:", ":Xi:", "\U039E")
    text <- replace_units_symbol(text, ":xi:", ":xi:", "\U03BE")
    text <- replace_units_symbol(text, ":Omicron:", ":Omicron:", "\U039F")
    text <- replace_units_symbol(text, ":omicron:", ":omicron:", "\U03BF")
    text <- replace_units_symbol(text, ":Pi:", ":Pi:", "\U03A0")
    text <- replace_units_symbol(text, ":pi:", ":pi:", "\U03C0")
    text <- replace_units_symbol(text, ":Rho:", ":Rho:", "\U03A1")
    text <- replace_units_symbol(text, ":rho:", ":rho:", "\U03C1")
    text <- replace_units_symbol(text, ":Sigma:", ":Sigma:", "\U03A3")
    text <- replace_units_symbol(text, ":sigma:", ":sigma:", "\U03C3")
    text <- replace_units_symbol(text, ":sigmaf:", ":sigmaf:", "\U03C2")
    text <- replace_units_symbol(text, ":Tau:", ":Tau:", "\U03A4")
    text <- replace_units_symbol(text, ":tau:", ":tau:", "\U03C4")
    text <- replace_units_symbol(text, ":Upsilon:", ":Upsilon:", "\U03A5")
    text <- replace_units_symbol(text, ":upsilon:", ":upsilon:", "\U03C5")
    text <- replace_units_symbol(text, ":Phi:", ":Phi:", "\U03A6")
    text <- replace_units_symbol(text, ":phi:", ":phi:", "\U03C6")
    text <- replace_units_symbol(text, ":Chi:", ":Chi:", "\U03A7")
    text <- replace_units_symbol(text, ":chi:", ":chi:", "\U03C7")
    text <- replace_units_symbol(text, ":Psi:", ":Psi:", "\U03A8")
    text <- replace_units_symbol(text, ":psi:", ":psi:", "\U03C8")
    text <- replace_units_symbol(text, ":Omega:", ":Omega:", "\U03A9")
    text <- replace_units_symbol(text, ":omega:", ":omega:", "\U03C9")
  }

  text
}

replace_units_symbol <- function(text, detect, pattern, replace) {
  if (grepl(detect, text)) text <- gsub(pattern, replace, text)
  text
}
