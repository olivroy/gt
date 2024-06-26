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


#' View a table with info on date styles
#'
#' @description
#'
#' The [fmt_date()] function lets us format date-based values in a convenient
#' manner using preset styles. The table generated by the `info_date_style()`
#' function provides a quick reference to all styles, with associated format
#' names and example outputs using a fixed date (`2000-02-29`).
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on the different date-formatting styles (which are used
#' by supplying a number code to the [fmt_date()] function).
#'
#' ```r
#' info_date_style()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_date_style_1.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-1
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
info_date_style <- function() {

  date_formats() %>%
    dplyr::mutate(date = "2000-02-29") %>%
    dplyr::mutate(flexible = dplyr::case_when(
      flexible ~ "FLEXIBLE",
      .default = ""
    )) %>%
    gt(rowname_col = "format_number") %>%
    cols_hide(columns = format_code) %>%
    fmt_date(columns = date, rows = 1, date_style = 1) %>%
    fmt_date(columns = date, rows = 2, date_style = 2) %>%
    fmt_date(columns = date, rows = 3, date_style = 3) %>%
    fmt_date(columns = date, rows = 4, date_style = 4) %>%
    fmt_date(columns = date, rows = 5, date_style = 5) %>%
    fmt_date(columns = date, rows = 6, date_style = 6) %>%
    fmt_date(columns = date, rows = 7, date_style = 7) %>%
    fmt_date(columns = date, rows = 8, date_style = 8) %>%
    fmt_date(columns = date, rows = 9, date_style = 9) %>%
    fmt_date(columns = date, rows = 10, date_style = 10) %>%
    fmt_date(columns = date, rows = 11, date_style = 11) %>%
    fmt_date(columns = date, rows = 12, date_style = 12) %>%
    fmt_date(columns = date, rows = 13, date_style = 13) %>%
    fmt_date(columns = date, rows = 14, date_style = 14) %>%
    fmt_date(columns = date, rows = 15, date_style = 15) %>%
    fmt_date(columns = date, rows = 16, date_style = 16) %>%
    fmt_date(columns = date, rows = 17, date_style = 17) %>%
    fmt_date(columns = date, rows = 18, date_style = 18) %>%
    fmt_date(columns = date, rows = 19, date_style = 19) %>%
    fmt_date(columns = date, rows = 20, date_style = 20) %>%
    fmt_date(columns = date, rows = 21, date_style = 21) %>%
    fmt_date(columns = date, rows = 22, date_style = 22) %>%
    fmt_date(columns = date, rows = 23, date_style = 23) %>%
    fmt_date(columns = date, rows = 24, date_style = 24) %>%
    fmt_date(columns = date, rows = 25, date_style = 25) %>%
    fmt_date(columns = date, rows = 26, date_style = 26) %>%
    fmt_date(columns = date, rows = 27, date_style = 27) %>%
    fmt_date(columns = date, rows = 28, date_style = 28) %>%
    fmt_date(columns = date, rows = 29, date_style = 29) %>%
    fmt_date(columns = date, rows = 30, date_style = 30) %>%
    fmt_date(columns = date, rows = 31, date_style = 31) %>%
    fmt_date(columns = date, rows = 32, date_style = 32) %>%
    fmt_date(columns = date, rows = 33, date_style = 33) %>%
    fmt_date(columns = date, rows = 34, date_style = 34) %>%
    fmt_date(columns = date, rows = 35, date_style = 35) %>%
    fmt_date(columns = date, rows = 36, date_style = 36) %>%
    fmt_date(columns = date, rows = 37, date_style = 37) %>%
    fmt_date(columns = date, rows = 38, date_style = 38) %>%
    fmt_date(columns = date, rows = 39, date_style = 39) %>%
    fmt_date(columns = date, rows = 40, date_style = 40) %>%
    fmt_date(columns = date, rows = 41, date_style = 41) %>%
    tab_header(
      title = "Date Formatting Options",
      subtitle = md("Usable in the `fmt_date()` and `fmt_datetime()` functions")
    ) %>%
    text_transform(
      locations = cells_body(
        columns = flexible,
        rows = flexible == "FLEXIBLE"
      ),
      fn = function(x) {
        paste0("<span style='",
          "color: darkslategray; font-weight: 700; font-size: smaller; border: solid 1px; background: aliceblue; border-color: mediumpurple; border-width: 2px; border-radius: 4px; padding: 0px 8px 0px 8px;",
          "'>", x, "</span>"
        )
      }
    ) %>%
    opt_align_table_header(align = "left") %>%
    cols_label(
      format_name = "Format Name",
      flexible = "",
      date = "Formatted Date"
    ) %>%
    tab_style(
      style = cell_text(font = "monospace"),
      locations = cells_body(columns = date)
    )
}

#' View a table with info on time styles
#'
#' @description
#'
#' The [fmt_time()] function lets us format time-based values in a convenient
#' manner using preset styles. The table generated by the `info_time_style()`
#' function provides a quick reference to all styles, with associated format
#' names and example outputs using a fixed time (`14:35`).
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on the different time-formatting styles (which are used
#' by supplying a number code to the [fmt_time()] function).
#'
#' ```r
#' info_time_style()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_time_style_1.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-2
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
info_time_style <- function() {

  time_formats() %>%
    dplyr::mutate(time = "14:35") %>%
    dplyr::mutate(flexible = dplyr::case_when(
      flexible ~ "FLEXIBLE",
      TRUE ~ ""
    )) %>%
    gt(rowname_col = "format_number") %>%
    cols_hide(columns = format_code) %>%
    fmt_time(columns = time, rows = 1, time_style = 1) %>%
    fmt_time(columns = time, rows = 2, time_style = 2) %>%
    fmt_time(columns = time, rows = 3, time_style = 3) %>%
    fmt_time(columns = time, rows = 4, time_style = 4) %>%
    fmt_time(columns = time, rows = 5, time_style = 5) %>%
    fmt_time(columns = time, rows = 6, time_style = 6) %>%
    fmt_time(columns = time, rows = 7, time_style = 7) %>%
    fmt_time(columns = time, rows = 8, time_style = 8) %>%
    fmt_time(columns = time, rows = 9, time_style = 9) %>%
    fmt_time(columns = time, rows = 10, time_style = 10) %>%
    fmt_time(columns = time, rows = 11, time_style = 11) %>%
    fmt_time(columns = time, rows = 12, time_style = 12) %>%
    fmt_time(columns = time, rows = 13, time_style = 13) %>%
    fmt_time(columns = time, rows = 14, time_style = 14) %>%
    fmt_time(columns = time, rows = 15, time_style = 15) %>%
    fmt_time(columns = time, rows = 16, time_style = 16) %>%
    fmt_time(columns = time, rows = 17, time_style = 17) %>%
    fmt_time(columns = time, rows = 18, time_style = 18) %>%
    fmt_time(columns = time, rows = 19, time_style = 19) %>%
    fmt_time(columns = time, rows = 20, time_style = 20) %>%
    fmt_time(columns = time, rows = 21, time_style = 21) %>%
    fmt_time(columns = time, rows = 22, time_style = 22) %>%
    fmt_time(columns = time, rows = 23, time_style = 23) %>%
    fmt_time(columns = time, rows = 24, time_style = 24) %>%
    fmt_time(columns = time, rows = 25, time_style = 25) %>%
    tab_header(
      title = "Time Formatting Options",
      subtitle = md("Usable in the `fmt_time()` and `fmt_datetime()` functions")
    ) %>%
    text_transform(
      locations = cells_body(
        columns = flexible,
        rows = flexible == "FLEXIBLE"
      ),
      fn = function(x) {
        paste0(
          "<span style='",
          "color: darkslategray; font-weight: 700; font-size: smaller; border: solid 1px; background: aliceblue; border-color: mediumpurple; border-width: 2px; border-radius: 4px; padding: 0px 8px 0px 8px;",
          "'>", x, "</span>"
        )
      }
    ) %>%
    opt_align_table_header(align = "left") %>%
    cols_label(
      format_name = "Format Name",
      flexible = "",
      time = "Formatted Time",
      time_type = "12/24h"
    ) %>%
    tab_style(
      style = cell_text(size = "smaller"),
      locations = cells_body(columns = time_type)
    ) %>%
    tab_style(
      style = cell_text(font = "monospace"),
      locations = cells_body(columns = time)
    ) %>%
    sub_missing(columns = time_type) %>%
    cols_align(align = "center", columns = time_type)
}

#' View a table with info on supported currencies
#'
#' @description
#'
#' The [fmt_currency()] function lets us format numeric values as currencies.
#' The table generated by the [info_currencies()] function provides a quick
#' reference to all the available currencies. The currency identifiers are
#' provided (name, 3-letter currency code, and 3-digit currency code) along with
#' the each currency's exponent value (number of digits of the currency
#' subunits). A formatted example is provided (based on the value of `49.95`) to
#' demonstrate the default formatting of each currency.
#'
#' @details
#' There are 172 currencies, which can lead to a verbose display table. To make
#' this presentation more focused on retrieval, we can provide an initial letter
#' corresponding to the 3-letter currency code to `begins_with`. This will
#' filter currencies in the info table to just the set beginning with the
#' supplied letter.
#'
#' @param type *Type of currency*
#'
#'   `singl-kw:[code|symbol]` // *default:* `"code"`
#'
#'   The type of currency information provided. Can either be `code` where
#'   currency information corresponding to 3-letter currency codes is provided,
#'   or `symbol` where currency info for common currency names (e.g., dollar,
#'   pound, yen, etc.) is returned.
#'
#' @param begins_with *Show currencies beginning with a specific letter*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Providing a single letter will filter currencies to only those that begin
#'   with that letter in their currency code. The default (`NULL`) will produce
#'   a table with all currencies displayed. This option only constrains the
#'   information table where `type == "code"`.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on all of the currencies where the three-letter code
#' begins with an `"h"`.
#'
#' ```r
#' info_currencies(begins_with = "h")
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_currencies_1.png")`
#' }}
#'
#' Get a table of info on all of the common currency name/symbols that can be
#' used with [fmt_currency()].
#'
#' ```r
#' info_currencies(type = "symbol")
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_currencies_2.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-3
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
info_currencies <- function(
    type = c("code", "symbol"),
    begins_with = NULL
) {

  if (type[1] == "code") {

    if (!is.null(begins_with)) {

      starting <-
        substr(begins_with, 1, 1) %>%
        toupper()

      curr <-
        currencies %>%
        dplyr::filter(grepl(paste0("^", starting, ".*"), curr_code))

    } else {
      curr <- currencies
    }
    tab_1 <- curr
    tab_1$symbol <- NULL
    tab_1$value <- 49.95
    tab_1 <- dplyr::relocate(tab_1, "curr_name")
    tab_1 <- gt(tab_1)

    for (i in seq_len(nrow(curr))) {

      tab_1 <-
        fmt_currency(
          tab_1,
          columns = "value",
          rows = i,
          currency = curr[[i, "curr_code"]]
        )
    }

    tab_1 <-
      tab_spanner(
        tab_1,
        label = "Identifiers",
        columns = c("curr_name", "curr_code", "curr_number")
      )
    tab_1 <-
      cols_label(
        tab_1,
        curr_name = html("Currency\nName"),
        curr_code = html("Currency\nCode"),
        curr_number = html("Currency\nNumber"),
        exponent = "Exp",
        value = html("Formatted\nCurrency"),
      )
    tab_1 <-
      tab_header(
        tab_1,
        title = md("Currencies Supported in **gt**"),
        subtitle = md("Currency codes are used in the `fmt_currency()` function")
      )
    tab_1 <-
      tab_style(
        tab_1,
        style = cell_text(align = "left"),
        locations = list(
          cells_title(groups = "title"),
          cells_title(groups = "subtitle")
        )
      )

    return(tab_1)
  }

  if (type[1] == "symbol") {

    curr <- currency_symbols

    # Prepare gt for example.
    tab_1 <- currency_symbols
    tab_1$symbol <- NULL
    tab_1$value <- 49.95
    tab_1 <- gt(tab_1)

    for (i in seq_len(nrow(curr))) {

      tab_1 <-
        fmt_currency(
          tab_1,
          columns = "value",
          rows = i,
          currency = curr[[i, "curr_symbol"]]
        )
    }

    tab_1 <-
      cols_label(
        tab_1,
        curr_symbol = html("Currency\nSymbol"),
        value = html("Formatted\nCurrency"),
      )
    tab_1 <-
      tab_header(
        tab_1,
        title = md("Currencies Supported in **gt**"),
        subtitle = md("Currency symbols are used in the `fmt_currency()` function")
      )
    tab_1 <-
      tab_style(
        tab_1,
        style = cell_text(align = "left"),
        locations = list(
          cells_title(groups = "title"),
          cells_title(groups = "subtitle")
        )
      )

    return(tab_1)
  }
}

#' View a table with info on supported locales
#'
#' @description
#'
#' Many of the `fmt_*()` functions have a `locale` argument that makes
#' locale-based formatting easier. The table generated by the `info_locales()`
#' function provides a quick reference to all the available locales. The locale
#' identifiers are provided (base locale ID, common display name) along with the
#' each locale's group and decimal separator marks. A formatted numeric example
#' is provided (based on the value of `11027`) to demonstrate the default
#' formatting of each locale.
#'
#' @details
#' There are 712 locales, which means that a very long display table is provided
#' by default. To trim down the output table size, we can provide an initial
#' letter corresponding to the base locale ID to `begins_with`. This will filter
#' locales in the info table to just the set that begins with the supplied
#' letter.
#'
#' @param begins_with *Show locales beginning with a specific letter*
#'
#'   `scalar<character>` // *default:* `NULL` (`optional`)
#'
#'   Providing a single letter will filter locales to only those that begin
#'   with that letter in their locale ID. The default (`NULL`) will produce
#'   a table with all locales displayed
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on all of the locales where the base locale ID begins
#' with a `"v"`.
#'
#' ```r
#' info_locales(begins_with = "v")
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_locales_1.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-4
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
info_locales <- function(begins_with = NULL) {

  locale <- lang_desc <- script_desc <- territory_desc <- NULL
  variant_desc <- group <- decimal <- NULL

  if (!is.null(begins_with)) {

    starting <- tolower(substr(begins_with, 1, 1))
    loc <- dplyr::filter(locales, grepl(paste0("^", starting, ".*"), locale))

  } else {
    loc <- locales
  }

  tab_1 <-
    dplyr::select(
      loc, locale, lang_desc, script_desc,
      territory_desc, variant_desc, group, decimal
    )
  tab_1 <-
    dplyr::mutate(
      tab_1,
      display_name = paste0(
        lang_desc,
          paste0(
            " (",
            territory_desc, ", ",
            script_desc, ", ",
            variant_desc,
            ")"
          )
      )
    )
  tab_1 <- dplyr::select(tab_1, "locale", "display_name", "group", "decimal")
  tab_1$display_name <- gsub(" (NA, NA, NA)", "", tab_1$display_name, fixed = TRUE)
  tab_1$display_name <- gsub(", NA, NA", "", tab_1$display_name, fixed = TRUE)
  tab_1$display_name <- gsub("NA, ", "", tab_1$display_name, fixed = TRUE)
  tab_1$display_name <- gsub(", NA)", ")", tab_1$display_name, fixed = TRUE)
  tab_1$value <- 11027
  tab_1 <- gt(tab_1)

  for (i in seq_len(nrow(loc))) {

    tab_1 <-
      fmt_number(
        tab_1,
        columns = "value",
        rows = i,
        locale = loc$locale[i]
      )
  }

  tab_1 %>%
    tab_spanner(
      label = "Separators",
      columns = c("group", "decimal")
    ) %>%
    cols_merge(
      columns = c("locale", "display_name"),
      pattern = "<code>{1}</code><br><span style=font-size:11px>{2}</span>"
    ) %>%
    cols_label(
      locale = "Locale",
      group = "Group",
      decimal = "Decimal",
      value = html("Formatted<br>Value")
    ) %>%
    cols_align(
      align = "center",
      columns = c("group", "decimal")
    ) %>%
    tab_header(
      title = md("Locales Supported in **gt**"),
      subtitle = md("Locale codes are used in several `fmt_*()` functions")
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = list(
        cells_title(groups = "title"),
        cells_title(groups = "subtitle")
      )
    ) %>%
    tab_style(
      style = cell_text(size = px(32)),
      locations = cells_body(columns = c(group, decimal))
    ) %>%
    tab_options(data_row.padding = "5px")
}

#' View a table with info on color palettes
#'
#' @description
#'
#' While the [data_color()] function allows us to flexibly color data cells in
#' our **gt** table, the harder part of this process is discovering and
#' choosing color palettes that are suitable for the table output. We can make
#' this process much easier in two ways: (1) by using the **paletteer**
#' package, which makes a wide range of palettes from various R packages readily
#' available, and (2) calling the `info_paletteer()` function to give us an
#' information table that serves as a quick reference for all of the discrete
#' color palettes available in **paletteer**.
#'
#' @details
#'
#' The palettes displayed are organized by package and by palette name. These
#' values are required when obtaining a palette (as a vector of hexadecimal
#' colors), from the the `paletteer::paletteer_d()` function. Once we are
#' familiar with the names of the color palette packages (e.g.,
#' **RColorBrewer**, **ggthemes**, **wesanderson**), we can narrow down
#' the content of this information table by supplying a vector of such package
#' names to `color_pkgs`.
#'
#' Colors from the following color packages (all supported by **paletteer**)
#' are shown by default with `info_paletteer()`:
#' \itemize{
#' \item **awtools**, 5 palettes
#' \item **dichromat**, 17 palettes
#' \item **dutchmasters**, 6 palettes
#' \item **ggpomological**, 2 palettes
#' \item **ggsci**, 42 palettes
#' \item **ggthemes**, 31 palettes
#' \item **ghibli**, 27 palettes
#' \item **grDevices**, 1 palette
#' \item **jcolors**, 13 palettes
#' \item **LaCroixColoR**, 21 palettes
#' \item **NineteenEightyR**, 12 palettes
#' \item **nord**, 16 palettes
#' \item **ochRe**, 16 palettes
#' \item **palettetown**, 389 palettes
#' \item **pals**, 8 palettes
#' \item **Polychrome**, 7 palettes
#' \item **quickpalette**, 17 palettes
#' \item **rcartocolor**, 34 palettes
#' \item **RColorBrewer**, 35 palettes
#' \item **Redmonder**, 41 palettes
#' \item **wesanderson**, 19 palettes
#' \item **yarrr**, 21 palettes
#' }
#'
#' @param color_pkgs *Filter to specific color packages*
#'
#'   `vector<character>` // *default:* `NULL` (`optional`)
#'
#'   A vector of color packages that determines which sets of palettes should be
#'   displayed in the information table. If this is `NULL` (the default) then
#'   all of the discrete palettes from all of the color packages represented in
#'   **paletteer** will be displayed.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on just the `"ggthemes"` color palette (easily accessible
#' from the **paletteer** package).
#'
#' ```r
#' info_paletteer(color_pkgs = "ggthemes")
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_paletteer_1.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-5
#'
#' @section Function Introduced:
#' `v0.2.0.5` (March 31, 2020)
#'
#' @export
info_paletteer <- function(color_pkgs = NULL) {

  if (is.null(color_pkgs)) {
    return(readRDS(system_file("gt_tables/info_paletteer.rds")))
  }

  palettes_strips_df <-
    dplyr::filter(palettes_strips, package %in% color_pkgs)

  palettes_strips <-
    dplyr::pull(palettes_strips_df, colors)

  palettes_strips_df %>%
    dplyr::select(package, palette, length) %>%
    dplyr::mutate(`Color Count and Palette` = NA) %>%
    gt(groupname_col = "package", rowname_col = "palette") %>%
    text_transform(
      locations = cells_body("Color Count and Palette"),
      fn = function(x) {
        palettes_strips
      }
    ) %>%
    cols_label(length = "") %>%
    tab_stubhead(label = "Package and Palette Name") %>%
    tab_header(
      title = md("Palettes Made Easily Available with **paletteer**"),
      subtitle = md("Palettes like these are useful with the `data_color()` function")
    ) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = list(
        cells_title(groups = "title"),
        cells_title(groups = "subtitle")
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#E3E3E3"),
        cell_text(font = "Courier", size = "smaller", weight = "bold")
      ),
      locations = cells_stub(rows = TRUE)
    ) %>%
    tab_style(
      style = cell_text(font = "Courier"),
      locations = cells_body(columns = "length")
    ) %>%
    tab_options(
      row_group.background.color = "#FFFFF0",
      column_labels.background.color = "#666660",
      row_group.font.weight = "600",
      row_group.font.size = "smaller"
    ) %>%
    tab_source_note(
      source_note = md(
        paste0(
          "The **paletteer** package is maintained by Emil Hvitfeldt. More ",
          "information can be found on [the **paletteer** site]",
          "(https://emilhvitfeldt.github.io/paletteer/) and on the ",
          "[**CRAN** info page]",
          "(https://cran.r-project.org/web/packages/paletteer/index.html)."
        )
      )
    )
}

#' View a table on recommended Google Fonts
#'
#' @description
#'
#' The [google_font()] helper function can be used wherever a font name should
#' be specified. There are two instances where this helper can be used: the
#' `name` argument in [opt_table_font()] (for setting a table font) and in that
#' of [cell_text()] (used with [tab_style()]). Because there is an overwhelming
#' number of fonts available in the *Google Fonts* catalog, the
#' `info_google_fonts()` provides a table with a set of helpful font
#' recommendations. These fonts look great in the different parts of a **gt**
#' table. Why? For the most part they are suitable for body text, having large
#' counters, large x-height, reasonably low contrast, and open apertures. These
#' font features all make for high legibility at smaller sizes.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on some of the recommended *Google Fonts* for tables.
#'
#' ```r
#' info_google_fonts()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_google_fonts_1.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-6
#'
#' @section Function Introduced:
#' `v0.2.2` (August 5, 2020)
#'
#' @export
info_google_fonts <- function() {
  readRDS(system_file("gt_tables/info_google_fonts.rds"))
}

#' View a table with all available flags for `fmt_flag()`
#'
#' @description
#'
#' The [fmt_flag()] function can be used to render flag icons within
#' body cells that have 2-letter country codes. There are a lot of countries,
#' so, the `info_flags()` function can be helpful in showing all of the valid
#' and supported country codes along with their flag icons.
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on all the available flag icons.
#'
#' ```r
#' info_flags()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_flags_1.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-7
#'
#' @section Function Introduced:
#' `v0.10.0` (October 7, 2023)
#'
#' @export
info_flags <- function() {
  readRDS(system_file("gt_tables/info_flags.rds"))
}

#' View a table with all available Font Awesome icons for `fmt_icon()`
#'
#' @description
#'
#' The [fmt_icon()] function can be used to render *Font Awesome* icons within
#' body cells that reference the icon names. Further to this, the text
#' transformation functions (e.g., [text_case_match()]) allow for the insertion
#' of these icons as replacement text (so long as you use the `fa()` function
#' from the **fontawesome** package). Because there is a very large number of
#' icons available to use in *Font Awesome*, `info_icons()` can be used to
#' provide us with a table that lists all the icons along with their short and
#' full names (either can be used with [fmt_icon()]).
#'
#' @return An object of class `gt_tbl`.
#'
#' @section Examples:
#'
#' Get a table of info on all the available *Font Awesome* icons.
#'
#' ```r
#' info_icons()
#' ```
#'
#' \if{html}{\out{
#' `r man_get_image_tag(file = "man_info_icons_1.png")`
#' }}
#'
#' @family information functions
#' @section Function ID:
#' 11-8
#'
#' @section Function Introduced:
#' `v0.10.0` (October 7, 2023)
#'
#' @export
info_icons <- function() {
  readRDS(system_file("gt_tables/info_icons.rds"))
}
