utils::globalVariables(c(
  'Variable', 'Level', 'At Risk',
  'Events', 'HR 95%CI', 'p'
))

# Create a count summary row
.row_counts <- function(
  label, data, by,
  percent.sign, less.than.one, remove.na,
  digits
) {

  table.row <- tibble::tribble(
    ~'Variable', ~'Key', ~'Value',
    label, 'Overall', as.character(nrow(data))
  )

  if (!is.null(by)) {
    for (level.by in levels(data[[by]])) {
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = level.by,
        Value = utile.tools::paste_freq(
          count = nrow(dplyr::filter(data, !! rlang::sym(by) == level.by)),
          total = ifelse(
            remove.na,
            nrow(dplyr::filter(.data = data, !is.na(!! rlang::sym(by)))),
            nrow(data)
          ),
          percent.sign = percent.sign,
          remove.na = remove.na,
          digits = digits
        )
      )
    }
  }

  # Fold row data into a table and return
  tidyr::spread(data = table.row, key = 'Key', value = 'Value')
}


# Create a numeric summary row
.row_numeric <- function(
  label, col, data,
  by, parametric, percent.sign,
  less.than.one, label.stats, remove.na,
  digits, p.digits
) {

  if (label.stats) label <- if (!parametric) paste0(label, ', median[IQR]') else paste0(label, ', mean\u00B1SD')

  table.row <- tibble::tribble(
    ~'Variable', ~'Key', ~'Value',
    label, 'Overall',
    if (!parametric)
      utile.tools::paste_median(
        col = data[[col]],
        less.than.one = less.than.one,
        digits = digits
      )
    else
      utile.tools::paste_mean(
        col = data[[col]],
        less.than.one = less.than.one,
        digits = digits
      )
  )

  # Strata summary statistics
  if (!is.null(by)) {
    for (level.by in levels(data[[by]])) {
      data.by <- dplyr::filter(data, !! rlang::sym(by) == level.by)
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = level.by,
        Value =
          if (!parametric)
            utile.tools::paste_median(
              col = data.by[[col]],
              less.than.one = less.than.one,
              digits = digits
            )
          else
            utile.tools::paste_mean(
              data.by[[col]],
              less.than.one = less.than.one,
              digits = digits
            )
      )
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = paste0('Missing: ', level.by),
        Value = utile.tools::paste_freq(
          count = nrow(dplyr::filter(.data = data.by, is.na(!! rlang::sym(col)))),
          total = nrow(data.by),
          percent.sign = percent.sign,
          remove.na = remove.na,
          digits = digits
        )
      )
    }
    table.row <- tibble::add_row(
      .data = table.row,
      Variable = label,
      Key = 'Probability',
      Value = utile.tools::test_numeric(
        col = col, by = by, parametric = parametric,
        data = data, digits = digits, p.digits = p.digits
      )
    )
    if (label.stats) {
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = 'Test',
        Value = if (!parametric) 'Wilcox {NP}' else 'Student\'s {P}'
      )
    }
  } else {
    table.row <- tibble::add_row(
      .data = table.row,
      Variable = label,
      Key = 'Missing',
      Value = utile.tools::paste_freq(
        count = nrow(dplyr::filter(data, is.na(!! rlang::sym(col)))),
        total = nrow(data),
        percent.sign = percent.sign,
        remove.na = remove.na,
        digits = digits
      )
    )
  }

  # Fold row data into a table and return
  tidyr::spread(data = table.row, key = 'Key', value = 'Value')
}


# Create a factor summary row
.row_factor <- function(
  label, col,
  data, by,
  parametric, percent.sign,
  label.stats, remove.na,
  digits, p.digits
) {

  if (label.stats) label <- paste0(label, ', n(%)')

  # Create variable header row
  table.row <- tibble::tibble(Variable = character(), Key = character(), Value = character())

  if (!is.null(by)) {
    for (level.by in levels(data[[by]])) {
      table.row <- tibble::add_row(.data = table.row, Variable = label, Key = level.by, Value = NA)
      data.by <- dplyr::filter(data, !! rlang::sym(by) == level.by)
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = paste0('Missing: ', level.by),
        Value = utile.tools::paste_freq(
          count = nrow(dplyr::filter(.data = data.by, is.na(!! rlang::sym(col)))),
          total = nrow(data.by),
          percent.sign = percent.sign,
          remove.na = remove.na,
          digits = digits
        )
      )
    }
    table.row <- tibble::add_row(
      .data = table.row,
      Variable = label,
      Key = 'Probability',
      Value = utile.tools::test_factor(
        col = col, by = by, parametric = parametric,
        data = data, digits = digits, p.digits = p.digits
      )
    )
    if (label.stats) {
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = 'Test',
        Value = if (!parametric) 'Chisq {NP}' else 'Fisher\'s {P}'
      )
    }
  } else {
    table.row <- tibble::add_row(
      .data = table.row,
      Variable = label,
      Key = 'Missing',
      Value = utile.tools::paste_freq(
        count = nrow(dplyr::filter(data, is.na(!! rlang::sym(col)))),
        total = nrow(data),
        percent.sign = percent.sign,
        remove.na = remove.na,
        digits = digits
      )
    )
  }

  table <- tidyr::spread(data = table.row, key = 'Key', value = 'Value')

  # Summarize: Each level
  for (level in sort(levels(data[[col]]))) {
    level.name <- paste('  ', level, sep = '')

    # Level data subset
    subset <- dplyr::filter(data, !! rlang::sym(col) == level)

    # Create row data stub
    table.row <- tibble::tribble(
      ~'Variable', ~'Key', ~'Value',
      level.name, 'Overall', utile.tools::paste_freq(
        count = nrow(subset),
        total = ifelse(
          remove.na,
          nrow(dplyr::filter(.data = data, !is.na(!! rlang::sym(col)))),
          nrow(data)
        ),
        percent.sign = percent.sign,
        remove.na = remove.na,
        digits = digits
      )
    )

    # Summarize strata data if necessary
    if (!is.null(by)) {
      for (level.by in sort(levels(data[[by]]))) {
        table.row <- tibble::add_row(
          .data = table.row,
          Variable = level.name,
          Key = level.by,
          Value = utile.tools::paste_freq(
            count = nrow(dplyr::filter(subset, !! rlang::sym(by) == level.by)),
            total = ifelse(
              remove.na,
              nrow(dplyr::filter(data, !! rlang::sym(by) == level.by & !is.na(!! rlang::sym(col)))),
              nrow(dplyr::filter(data, !! rlang::sym(by) == level.by))
            ),
            percent.sign = percent.sign,
            remove.na = remove.na,
            digits = digits
          )
        )
      }
    }

    # Fold row level data into main table
    table <- dplyr::bind_rows(table, tidyr::spread(data = table.row, key = 'Key', value = 'Value'))
  }

  # Return table
  table
}


# Create a logical summary row
.row_logical <- function(
  label, col,
  data, by,
  parametric, inverse,
  percent.sign, label.stats,
  remove.na, digits,
  p.digits
) {

  if (label.stats) label <- if (!inverse) paste0(label, ', yes, n(%)') else paste0(label, ', no, n(%)')

  # True/yes Subset
  subset <-
    if (!inverse) dplyr::filter(data, !! rlang::sym(col))
  else dplyr::filter(data, !(!! rlang::sym(col)))

  # Create row data stub
  table.row <- tibble::tribble(
    ~'Variable', ~'Key', ~'Value',
    label, 'Overall',
    utile.tools::paste_freq(
      count = nrow(subset),
      total = ifelse(
        remove.na,
        nrow(dplyr::filter(.data = data, !is.na(!! rlang::sym(col)))),
        nrow(data)
      ),
      percent.sign = percent.sign,
      remove.na =remove.na,
      digits = digits
    )
  )

  # Summarize strata data if necessary
  if (!is.null(by)) {
    for (level.by in levels(data[[by]])) {
      data.by <- dplyr::filter(data, !! rlang::sym(by) == level.by)
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = level.by,
        Value = utile.tools::paste_freq(
          count = nrow(dplyr::filter(subset, !! rlang::sym(by) == level.by)),
          total = ifelse(
            remove.na,
            nrow(dplyr::filter(.data = data.by, !is.na(!! rlang::sym(col)))),
            nrow(data.by)
          ),
          percent.sign = percent.sign,
          remove.na = remove.na,
          digits = digits
        )
      )
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = paste0('Missing: ', level.by),
        Value = utile.tools::paste_freq(
          count = nrow(dplyr::filter(.data = data.by, is.na(!! rlang::sym(col)))),
          total = nrow(data.by),
          percent.sign = percent.sign,
          remove.na = remove.na,
          digits = digits
        )
      )
    }
    table.row <- tibble::add_row(
      .data = table.row,
      Variable = label,
      Key = 'Probability',
      Value = utile.tools::test_factor(
        col = col, by = by, parametric = parametric,
        data = data, digits = digits, p.digits = p.digits
      )
    )
    if (label.stats) {
      table.row <- tibble::add_row(
        .data = table.row,
        Variable = label,
        Key = 'Test',
        Value = if (!parametric) 'Chisq {NP}' else 'Fisher\'s {P}'
      )
    }
  } else {
    table.row <- tibble::add_row(
      .data = table.row,
      Variable = label,
      Key = 'Missing',
      Value = utile.tools::paste_freq(
        count = nrow(dplyr::filter(data, is.na(!! rlang::sym(col)))),
        total = nrow(data),
        percent.sign = percent.sign,
        remove.na = remove.na,
        digits = digits
      )
    )
  }

  # Fold row data into main table and return
  tidyr::spread(data = table.row, key = 'Key', value = 'Value')
}

# Create coxph summary row
.row_coxph <- function (label, col, fit, digits, p.digits, percent.sign) {

  # Hard stop
  if (is.null(fit) | class(fit) != 'coxph') stop('Missing valid coxph object. [check: fit]')

  # Tabulate and return
  dplyr::transmute(
    .data = dplyr::filter(
      utile.tools::tabulate_model(
        fit = fit, digits = digits, p.digits = p.digits,
        percent.sign = percent.sign
      ),
      Variable == col
    ),
    Variable = purrr::map2_chr(
      Variable, Level,
      function(x, y)
        if (!is.na(y)) {
          if (dplyr::n() == 1) paste(if (!is.null(label)) label else x, y, sep = ', ')
          else paste0('   ', y)
        } else if (!is.null(label)) label
        else x
    ),
    `At Risk` = `At Risk`,
    Events = Events,
    `HR 95%CI` = `HR 95%CI`,
    p = p
  )
}


# Merge rows
.merge_rows <- function(.table, table, by, data) {

  # Merge tables
  if (!is.null(.table)) table <- dplyr::bind_rows(.table, table)

  # Detect main columns
  cols <- intersect(c('Variable', 'Overall'), names(table))

  # Detect level columns
  if (!is.null(by)) {
    levels.by <- sort(levels(data[[by]]))
    cols <- c(
      cols,
      intersect(levels.by, names(table)),
      intersect(paste0('Missing: ', levels.by), names(table))
    )
  }

  # Detect miscellaneous columns
  cols <- c(cols, intersect(c('Missing', 'Probability', 'Test'), names(table)))
  table <- dplyr::select_at(
    .tbl = table,
    .vars = dplyr::vars(
      cols,
      dplyr::everything() # Preserve unmatched columns
    )
  )

  # Return table
  table
}


# Make NA's human readable
.replace_na <- function(table) replace(table, is.na(table), '')
