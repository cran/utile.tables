#' @title Build summary tables
#' @description Takes a data or model object and summarizes it into a ready to
#' export, human-readable summary table.
#' @param .object An object of a supported class. See S3 methods below.
#' @param ... Arguments passed to the appropriate S3 method.
#' @return An object of class tbl_df (tibble) summarizing the provided object.
#' @seealso \code{\link{build_table.data.frame}},
#' \code{\link{build_table.coxph}},
#' \code{\link{build_table.lm}}
#' @export
build_table <- function (.object, ...) { UseMethod('build_table') }


#' @export
build_table.default <- function (.object, ...) {
  stop('Object of class \'', class(.object), '\' no supported.')
}


#' @title Build summary tables from data.frame objects
#' @description Takes a data.frame object and summarizes the columns into a
#' ready to export, human-readable summary table. Capable of stratifying data
#' and performing appropriate hypothesis testing.
#' @param .object A data.frame.
#' @param ... One or more unquoted expressions separated by commas representing
#' columns in the data.frame. May be specified using
#' \code{\link[tidyselect:select_helpers]{tidyselect helpers}}. If left empty,
#' all columns are summarized.
#' @param .by An unquoted expression. The data column to stratify the
#' summary by.
#' @param .inverse A logical. For logical data, report the frequency
#' of FALSE values instead of the TRUE.
#' @param .label.stat A logical. Append the type of summary statistic
#' to the column label.
#' @param .stat A character. Name of the summary statistic to use for numeric data.
#' Supported options include the mean ('mean') and median ('median').
#' @param .stat.pct.sign A logical. Paste a percent symbol after all
#' reported frequencies.
#' @param .col.overall A logical. Append a column with the statistic for all data.
#' If \code{.by} is not specified, this parameter is ignored.
#' @param .col.missing A logical. Append a column listing the
#' frequencies of missing data for each row.
#' @param .test.continuous A character. A character. Name of statistical test to compare groups.
#' Supported options include ANOVA linear model ('anova'), Kruskal-Wallis ('kruskal'),
#' and Wilcoxon rank sum ('wilcoxon') tests.
#' @param .test.nominal A character. Name of statistical test to compare groups.
#' Supported options include Pearson's Chi-squared Test ('chisq') and Fisher's
#' Exact Test ('fisher').
#' @param .test.simulate.p A logical. Whether to use Monte Carlo simulation of
#' the p-value when testing nominal data.
#' @param .col.test A logical. Append a column containing the test
#' each p-value was derived from.
#' @param .digits An integer. The number of digits to round numbers to.
#' @param .p.digits An integer. The number of p-value digits to report.
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' object.
#' @seealso \code{\link{build_table}}
#' @examples
#' # Sample data
#' df <- data.frame(
#'   strata = factor(sample(letters[2:3], 1000, replace = TRUE)),
#'   numeric = sample(1:100, 1000, replace = TRUE),
#'   numeric2 = sample(1:100, 1000, replace = TRUE),
#'   factor = factor(sample(1:5, 1000, replace = TRUE)),
#'   logical = sample(c(TRUE,FALSE), 1000, replace = TRUE)
#' )
#'
#' # Summarize all columns
#' build_table(df, .by = strata)
#'
#' # Summarize & rename selected columns
#' build_table(df, numeric2, factor, .by = strata)
#' @export
build_table.data.frame <- function (
  .object,
  ...,
  .by,
  .inverse = FALSE,
  .label.stat = TRUE,
  .stat = c('mean', 'median'),
  .stat.pct.sign = FALSE,
  .col.overall = TRUE,
  .col.missing = FALSE,
  .test.continuous = c('anova', 'kruskal', 'wilcoxon'),
  .test.nominal = c('chisq', 'fisher'),
  .test.simulate.p = FALSE,
  .col.test = FALSE,
  .digits = 1,
  .p.digits = 4
) {

  # Match arguments
  .stat <- match.arg(.stat)
  .test.continuous <- match.arg(.test.continuous)
  .test.nominal <- match.arg(.test.nominal)

  # Column selection
  cols <- if (rlang::dots_n(...) > 0) {
    tidyselect::eval_select(rlang::expr(c(...)), data = .object)
  } else {
    rlang::set_names(1:length(names(.object)), names(.object))
  }

  # By variable selection and validation
  by <- if (!missing(.by) & rlang::dots_n(.by) == 1) {
    tidyselect::eval_select(rlang::enexpr(.by), data = .object)
  }

  if (length(by) > 0) {
    if (is.logical(.object[[by]]) | is.factor(.object[[by]])) {

      # Cast logicals to factors
      if (is.logical(.object[[by]])) .object[[by]] <- as.factor(.object[[by]])

      # Explicit NA's
      .object[[by]] <- .explicit_na(.object[[by]])

      # Prevent summarization
      cols <- cols[!(cols %in% by)]

    } else by <- integer()
  }

  # Pre-specify row configuration
  build_row_ <- function (x, ...) {
    build_row(
      x = x,
      y = if (length(by) == 1) .object[[by]],
      ...,
      label.stat = .label.stat,
      inverse = .inverse,
      stat = .stat,
      stat.pct.sign = .stat.pct.sign,
      col.overall = .col.overall,
      col.missing = .col.missing,
      test.simulate.p = .test.simulate.p,
      col.test = .col.test,
      digits = .digits,
      p.digits = .p.digits
    )
  }

  # Create table
  table <- dplyr::bind_rows(
    build_row_(x = .object),
    purrr::list_rbind(
      purrr::imap(
        cols, ~ {
          build_row_(
            x = .object[[.x]],
            label = .y,
            test = if (inherits(.object[[.x]], c('factor', 'logical'))) .test.nominal
              else .test.continuous
          )
        }
      )
    )
  )

  # Replace NA's & return table
  .replace_na(table)

}


#' @title Build summary tables from coxph model objects
#' @description Takes a Cox PH model object and summarizes it into a ready to
#' export, human-readable summary table.
#' @param .object An object of class \code{\link[survival]{coxph}}.
#' @param ... One or more unquoted expressions separated by commas representing
#' columns in the data.frame. May be specified using
#' \code{\link[tidyselect:select_helpers]{tidyselect helpers}}. If left empty,
#' all terms are summarized.
#' @param .test A character. The name of the
#' \code{\link[stats:add1]{stats::drop1}} test to use with the model. Supported
#' tests include Wald's Test ('Wald') and Likelihood Ratio Test ('LRT').
#' @param .col.test A logical. Append a columns for the test and accompanying
#' statistic used to derive the p-value.
#' @param .level A double. The confidence level required.
#' @param .stat.pct.sign A logical. Paste a percent symbol after all reported
#' frequencies.
#' @param .digits An integer. The number of digits to round numbers to.
#' @param .p.digits An integer. The number of p-value digits to report. Note
#' that the p-value still rounded to the number of digits specified in
#' \code{.digits}.
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' object.
#' @seealso \code{\link{build_table}}
#' @examples
#' library(survival)
#' library(dplyr)
#'
#' data_lung <- lung |>
#'   mutate_at(vars(inst, status, sex), as.factor) |>
#'   mutate(status = case_when(status == 1 ~ 0, status == 2 ~ 1))
#'
#' fit <- coxph(Surv(time, status) ~ sex + meal.cal, data = data_lung)
#'
#' fit |> build_table(Sex = sex, Calories = meal.cal, .test = 'LRT')
#' @export
build_table.coxph <- function (
  .object,
  ...,
  .test = c('LRT', 'Wald'),
  .col.test = FALSE,
  .level = 0.95,
  .stat.pct.sign = TRUE,
  .digits = 1,
  .p.digits = 4
) {

  # Column selection
  terms <- if (rlang::dots_n(...) > 0) {
    tidyselect::eval_select(expr = rlang::expr(c(...)), data = .object$assign)
  } else {
    rlang::set_names(
      x = 1:length(names(.object$assign)),
      nm = names(.object$assign)
    )
  }

  # Filter assignments and map level names
  assignments <- purrr::imap(
    .object$assign[terms], ~ {
      if (.y %in% names(.object$xlevels)) {
        rlang::set_names(x = c(NA_real_, .x), nm = .object$xlevels[[.y]])
      } else .x
    }
  )
  names(assignments) <- names(terms)

  # Check test argument
  prefer.tests <- '.test' %in% names(match.call())
  .test <- match.arg(.test)

  # Tabulate & format estimates
  estimates <- as.data.frame(
    cbind(
      summary(.object)$coefficients,
      stats::confint(.object, level = .level)
    )
  )
  estimates[,6:7] <- exp(estimates[,6:7])
  estimates[,-5] <- round(estimates[,-5], digits = .digits)
  estimates[,5] <- format.pval(
    pv = estimates[,5],
    digits = .digits,
    eps = 0.0001,
    nsmall = .p.digits,
    scientific = F,
    na.form = ''
  )
  estimates[] <- lapply(estimates, as.character)


  # Tabulate & format special tests
  tests <- as.data.frame(
    stats::drop1(
      if (any(is.na(eval(.object$call$data)[all.vars(stats::formula(.object))]))) {
        .refit_model(x = .object, na.rm = TRUE)
      } else .object,
      test = 'Chisq'
    )
  )[terms + 1, 3:4]
  tests[,-2] <- round(tests[,-2], digits = .digits)
  tests[,2] <- format.pval(
    pv = tests[,2],
    digits = .digits,
    eps = 0.0001,
    nsmall = .p.digits,
    scientific = F,
    na.form = ''
  )
  tests[] <- lapply(tests, as.character)


  # Generate table
  table <- purrr::imap(
    assignments,
    function (w, x) {

      single_level <- (has_levels <- !is.null(names(w))) & length(w) == 2
      if (single_level) w <- w[-1] # Ignore reference level of a 2-level


      # Create column object
      cols <- list()

      # Variable name
      cols$Variable <- if (single_level) paste0(x, ', ', names(w)) else x

      # Number of observations
      cols$n <- as.character(.object$n)

      # Number of events
      cols$Event <- as.character(.object$nevent)

      # Effect estimate & CI
      cols$`HR [CI]` = if (!has_levels | single_level) {
        paste(
          estimates[w, 2],
          if (all(!is.na(estimates[w, 6:7]))) {
            paste0('[', estimates[w, 6], '-', estimates[w, 7], ']')
          } else '[NA]'
        )
      } else ''

      # p-value
      cols$p <- if ((prefer.tests & .test == 'LRT') | (has_levels & !single_level)) {
        tests[match(x, names(assignments)), 2]
      } else estimates[w, 5]

      # Report test
      if (.col.test) {

        cols$Test <-
          if ((prefer.tests & .test == 'LRT') | (has_levels & !single_level)) {
            'LRT'
          } else 'Wald'

        cols$Statistic <-
          if ((prefer.tests & .test == 'LRT') | (has_levels & !single_level)) {
            tests[match(x, names(assignments)), 1]
          } else estimates[w, 4]

      }


      # Generate factor level rows
      if (has_levels & !single_level) {

        # Level names (incl. ref)
        cols$Variable <- c(cols$Variable, paste('  ', names(w)))

        # Number of observations
        cols$n <- c(cols$n, rep('', length(w)))

        # Number of events
        cols$Event <- c(cols$Event, rep('', length(w)))

        # Effect estimate and CI
        cols$`HR [CI]` <- c(
          cols$`HR [CI]`,
          'Reference',
          paste(
            estimates[w[-1], 2],
            ifelse(
              !is.na(estimates[w[-1], 6]) & !is.na(estimates[w[-1], 7]),
              paste0('[', estimates[w[-1], 6], '-', estimates[w[-1], 7], ']'),
              '[NA]'
            )
          )
        )

        # Level p-value
        cols$p <- c(cols$p, '', estimates[w[-1], 5])

        # Report test
        if (.col.test) {

          cols$Test <- c(cols$Test, '', rep('Wald', length(w[-1])))
          cols$Statistic <- c(cols$Statistic, '', estimates[w[-1], 4])

        }

      }

      # Return as df (better performance)
      dplyr::as_tibble(cols)

    }
  )

  # Concatonate rows
  table <- purrr::list_rbind(table)

  # Replace NA's
  .replace_na(table)

}


#' @title Build summary tables from lm model objects
#' @description Takes a linear regression model object and summarizes it into a
#' ready to export, human-readable summary table.
#' @param .object An object of class \code{\link[stats]{lm}}.
#' @param ... One or more unquoted expressions separated by commas representing
#' columns in the data.frame. May be specified using
#' \code{\link[tidyselect:select_helpers]{tidyselect helpers}}. If left empty,
#' all terms are summarized.
#' @param .test A character. The name of the
#' \code{\link[stats:add1]{stats::drop1}} test to use with the model. Supported
#' options include the F-Test ('F') and Chi-squared Test ('Chisq').
#' @param .col.test A logical. Append a columns for the test and accompanying
#' statistic used to derive the p-value.
#' @param .level A double. The confidence level required.
#' @param .stat.pct.sign A logical. Paste a percent symbol after all reported
#' frequencies.
#' @param .digits An integer. The number of digits to round numbers to.
#' @param .p.digits An integer. The number of p-value digits to report. Note
#' that the p-value still rounded to the number of digits specified in
#' \code{.digits}.
#' @return An object of class \code{tbl_df} (tibble) summarizing the provided
#' object.
#' @seealso \code{\link{build_table}}
#' @examples
#' library(dplyr)
#'
#' data_mtcars <- datasets::mtcars |>
#'   mutate_at(vars('vs', 'am'), as.logical) |>
#'   mutate_at(vars('gear', 'carb', 'cyl'), as.factor)
#'
#' fit <- lm(mpg ~ vs + drat + cyl, data = data_mtcars)
#'
#' fit |> build_table()
#' @export
build_table.lm <- function (
  .object,
  ...,
  .test = c('F', 'Chisq'),
  .col.test = FALSE,
  .level = 0.95,
  .stat.pct.sign = TRUE,
  .digits = 1,
  .p.digits = 4
) {

  # Assignments
  assignments <- .create_assigns(
    x = c('(Intercept)', attr(terms(.object), 'term.labels')),
    y = .object$assign
  )

  # Column selection
  terms <- if (rlang::dots_n(...) > 0) {
    tidyselect::eval_select(expr = rlang::expr(c(...)), data = assignments)
  } else {
    rlang::set_names(
      x = 1:length(names(assignments)),
      nm = names(assignments)
    )
  }

  # Filter assignments and map level names
  assignments <- purrr::imap(
    assignments[terms], ~ {
      if (.y %in% names(.object$xlevels)) {
        rlang::set_names(x = c(NA_real_, .x), nm = .object$xlevels[[.y]])
      } else .x
    }
  )
  names(assignments) <- names(terms)

  # Check test argument
  prefer.tests <- '.test' %in% names(match.call())
  .test <- match.arg(.test)

  # Tabulate & format estimates
  estimates <- as.data.frame(
    cbind(
      summary(.object)$coefficients,
      stats::confint(.object, level = .level)
    )
  )
  estimates[,-4] <- round(estimates[,-4], digits = .digits)
  estimates[,4] <- format.pval(
    pv = estimates[,4],
    digits = .digits,
    eps = 0.0001,
    nsmall = .p.digits,
    scientific = F,
    na.form = ''
  )
  estimates[] <- lapply(estimates, as.character)

  # Tabulate & format special tests
  tests <- as.data.frame(
    stats::drop1(
      .object,
      test = .test
    )
  )[terms + 1, if (.test == 'Chisq') 5 else 5:6]
  if (.test == 'F') tests[,-2] <- round(tests[,-2], digits = .digits)
  else tests <- cbind(stat = '', tests)
  tests[,2] <- format.pval(
    pv = tests[,2],
    digits = .digits,
    eps = 0.0001,
    nsmall = .p.digits,
    scientific = F,
    na.form = ''
  )
  tests[] <- lapply(tests, as.character)

  # Generate table
  table <- purrr::imap(
    assignments,

    # Map assignments
    function (w, x) {

      single_level <- (has_levels <- !is.null(names(w))) & length(w) == 2
      if (single_level) w <- w[-1] # Ignore reference level of a 2-level

      cols <- list()

      # Variable name
      cols$Variable <- if (single_level) paste0(x, ', ', names(w)) else x

      # Effect estimate & CI
      cols$`HR [CI]` <- if (!has_levels | single_level) {
        paste(
          estimates[w, 1],
          if (all(!is.na(estimates[w, 5:6]))) {
            paste0('[', estimates[w, 5], '-', estimates[w, 6], ']')
          } else '[NA]'
        )
      } else ''

      # p-value
      cols$p <- if ((prefer.tests & x != '(Intercept)') | (has_levels & !single_level)) {
        tests[match(x, names(assignments)), 2]
      } else estimates[w, 4]

      # Report test
      if (.col.test) {

        cols$Test <-
          if ((prefer.tests & x != '(Intercept)') | (has_levels & !single_level)) {
            if(.test == 'F') 'F-stat' else .test
          } else 't-value'

        cols$Statistic <-
          if ((prefer.tests & x != '(Intercept)') | (has_levels & !single_level)) {
            tests[match(x, names(assignments)), 1]
          } else estimates[w, 3]

      }

      # Generate factor level rows
      if (has_levels & !single_level) {

        # Level names
        cols$Variable <- c(
          cols$Variable,
          paste('  ', names(w))
        )

        # Effect estimate and CI
        cols$`HR [CI]` = c(
          cols$`HR [CI]`,
          'Reference',
          paste(
            estimates[w[-1], 1],
            ifelse(
              !is.na(estimates[w[-1], 5]) & !is.na(estimates[w[-1], 6]),
              paste0('[', estimates[w[-1], 5], '-', estimates[w[-1], 6], ']'),
              '[NA]'
            )
          )
        )

        # Level p-value
        cols$p <- c(cols$p, '', estimates[w[-1], 4])

        # Report test
        if (.col.test) {
          cols$Test <- c(cols$Test, '',  rep('t-value', length(w[-1])))
          cols$Statistic <- c(cols$Statistic, '', estimates[w[-1], 3])
        }

      }

      # Return data
      dplyr::as_tibble(cols)

    }
  )

  # Concatenate rows
  table <- purrr::list_rbind(table)

  # Replace NA's & return
  .replace_na(table)

}

