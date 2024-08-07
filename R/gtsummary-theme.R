my_theme <- function() {
  list(
    "pkgwide-str:theme_name" = "my_theme()",
    "pkgwide-fn:pvalue_fun" = label_style_pvalue(digits = 2),
    "pkgwide-fn:prependpvalue_fun" = label_style_pvalue(digits = 2, prepend_p = TRUE),
    "pkgwide-str:ci.sep" = " to ",
    "style_number-arg:decimal.mark" = ".",
    "style_number-arg:big.mark" = ",",
    "add_difference-fn:addnl-fn-to-run" = function(x) {
      # merging coef and CI columns, if error, returning x unaltered
      tryCatch(
        {
          new_header_text <-
            paste0(
              x$table_styling$header |> dplyr::filter(.data$column == "estimate") |> dplyr::pull("label"),
              " **(**",
              x$table_styling$header |> dplyr::filter(.data$column == "conf.low") |> dplyr::pull("label"),
              "**)**"
            )

          # adding CI footnote to any existing abbreviation footnote, e.g. for OR, HR, etc.
          estimate_footnote <-
            x$table_styling$footnote_abbrev |>
            dplyr::filter(.data$column %in% "estimate") |>
            dplyr::filter(dplyr::row_number() == dplyr::n(), !is.na(.data$footnote)) |>
            dplyr::pull("footnote") |>
            c("CI = Confidence Interval") |>
            paste(collapse = ", ")
          x %>%
            # merge estimate and CI into one cell
            modify_column_merge(
              rows = !!expr(.data$variable %in% !!x$table_body$variable &
                              !is.na(.data$estimate)),
              pattern = "{estimate} ({conf.low} to {conf.high})"
            ) |>
            # update column header
            modify_header(estimate = new_header_text) |>
            # add CI abbreviation footnote
            modify_footnote(estimate = estimate_footnote, abbreviation = TRUE)
        },
        error = function(e) x
      )
    },
    "tbl_regression-fn:addnl-fn-to-run" = function(x) {
      # merging coef and CI columns, if error, returning x unaltered
      tryCatch(
        {
          new_header_text <-
            paste0(
              x$table_styling$header|> dplyr::filter(.data$column == "estimate") |> dplyr::pull("label"),
              " **(", style_number(x$inputs$conf.level, scale = 100), "% CI)**"
            )

          # adding CI footnote to any existing abbreviation footnote, e.g. for OR, HR, etc.
          estimate_footnote <-
            x$table_styling$footnote_abbrev |>
            dplyr::filter(.data$column %in% "estimate") |>
            dplyr::filter(dplyr::row_number() == dplyr::n(), !is.na(.data$footnote)) |>
            dplyr::pull("footnote") |>
            c("CI = Confidence Interval") |>
            paste(collapse = ", ")
          x %>%
            # merge estimate and CI into one cell
            modify_column_merge(
              rows = !!expr(.data$variable %in% !!x$table_body$variable &
                              !is.na(.data$estimate) &
                              !.data$reference_row %in% TRUE),
              pattern = "{estimate} ({conf.low}-{conf.high})"
            ) |>
            # hide p-value column
            modify_column_hide(any_of("p.value")) %>%
            # update column header
            modify_header(estimate = new_header_text) |>
            # add CI abbreviation footnote
            modify_footnote(estimate = estimate_footnote, abbreviation = TRUE)
        },
        error = function(e) x
      )
    },
    # compact gt tables
    "as_gt-lst:addl_cmds" = list(
      tab_spanner = rlang::expr(
        gt::tab_options(
          table.font.size = gt::px(12),
          data_row.padding = gt::px(1),
          summary_row.padding = gt::px(1),
          grand_summary_row.padding = gt::px(1),
          footnotes.padding = gt::px(1),
          source_notes.padding = gt::px(1),
          row_group.padding = gt::px(1),
          table.width = pct(100)
        )
      )
    )
  )
}
