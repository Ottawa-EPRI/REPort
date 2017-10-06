library(huxtable)
library(magrittr)
library(xtable)

# Original huxreg taken at commit 2e6a66fcfd4fddb1f931971d4687728b528175a7 of
# the huxreg repository.
huxreg2 <- function (
  ...,
  heading_rows    = 1,
  error_style     = c('stderr', 'ci', 'statistic', 'pvalue'),
  error_pos       = c('below', 'same', 'right'),
  number_format   = '%.3f',
  pad_decimal     = '.',
  ci_level        = 0.95,
  stars           = c('***' = 0.001, '**' = 0.01, '*' = 0.05),
  bold_signif     = NULL,
  borders         = TRUE,
  note            = '%stars%.',
  statistics      = c('N' = 'nobs', 'R2' = 'r.squared', 'logLik', 'AIC'),
  coefs           = NULL,
  omit_coefs      = NULL
) {
  if (! requireNamespace('broom', quietly = TRUE)) stop('huxreg requires the "broom" package. To install, type:\n',
                                                        'install.packages("broom")')
  models <- list(...)
  if (inherits(models[[1]], 'list')) models <- models[[1]]
  mod_names <- names_or(models, bracket(seq_along(models)))
  if (missing(error_style)) error_style <- 'stderr'
  error_style <- sapply(error_style, match.arg, choices = eval(formals(huxreg)$error_style))
  error_pos <- match.arg(error_pos)

  tidy_with_ci <- function (obj) {
    if (has_builtin_ci(obj)) return(broom::tidy(obj, conf.int = TRUE, conf.level = ci_level))
    tidied <- broom::tidy(obj) # should return 'estimate' and 'std.error'
    cbind(tidied, make_ci(tidied[, c('estimate', 'std.error')], ci_level))
  }
  tidied <- lapply(models, if ('ci' %in% error_style) tidy_with_ci else broom::tidy)

  my_coefs <- unique(unlist(lapply(tidied, function (x) x$term)))
  if (! missing(omit_coefs)) my_coefs <- setdiff(my_coefs, omit_coefs)
  if (! missing(coefs)) {
    if (! all(coefs %in% my_coefs)) stop('Unrecognized coefficient names: ',
                                         paste(setdiff(coefs, my_coefs), collapse = ', '))
    my_coefs <- coefs
  }
  coef_names <- names_or(my_coefs, my_coefs)

  tidied <- lapply(tidied, merge, x = data.frame(term = my_coefs, stringsAsFactors = FALSE), all.x = TRUE, by = 'term',
                   sort = FALSE)
  tidied <- lapply(tidied, function (x) {
    x$term[! is.na(match(x$term, my_coefs))] <- coef_names[match(x$term, my_coefs)]
    x <- x[match(unique(coef_names), x$term), ]
  })
  coef_names <- unique(coef_names)
  tidied <- lapply(tidied, function (x) {
    numcols <- sapply(x, is.numeric)
    x[, numcols] <- sapply(x[, numcols], format_number, number_format)
    x
  })
  if (! is.null(stars)) tidied <- lapply(tidied, function (x) {
    stars_arg <- c(0, sort(stars), ' ' = 1)
    x$estimate[ !is.na(x$estimate) ] <- with (x[! is.na(x$estimate), ], paste(estimate, symnum(as.numeric(p.value),
                                                                                               cutpoints = stars_arg, symbols = names(stars_arg)[-1], na = ' ')))
    x
  })

  tidied <- lapply(tidied, function (x) {
    x$error_cell <- make_error_cells(x, error_style)
    x
  })

  # now we cbind the models
  coef_col <- switch(error_pos,
                     same  = function (est, se) ifelse(is.na(est), NA, paste(est, se)),
                     below = interleave,
                     right = cbind
  )
  cols <- lapply(tidied, function (mod) coef_col(mod$estimate, mod$error_cell))
  cols <- Reduce(cbind, cols)
  cols <- hux(cols)
  number_format(cols) <- number_format
  if (! is.null(bold_signif)) {
    bold_cols <- lapply(tidied, function (mod) mod$p.value <= bold_signif)
    bold_cols <- switch(error_pos,
                        same  = bold_cols,
                        below = lapply(bold_cols, rep, each = 2),
                        right = lapply(bold_cols, function (x) cbind(x, x))
    )
    bold_cols <- Reduce(cbind, bold_cols)
    bold(cols) <- bold_cols
  }

  all_sumstats <- lapply(models, function(m) {
    bg <- try(broom::glance(m), silent = TRUE)
    bg <- if (class(bg) == 'try-error') {
      warning('No `glance` method for model of class ', class(m)[1])
      NULL
    } else t(bg)
    nobs <- nobs(m, use.fallback = TRUE)
    x <- as.data.frame(rbind(nobs = nobs, bg))
    x$stat  <- rownames(x)
    x$class <- c(class(nobs), sapply(bg, class))
    x
  })

  stat_names <- unique(unlist(lapply(all_sumstats, function (x) x$stat)))
  if (! is.null(statistics)) {
    if (! all(statistics %in% stat_names)) stop('Unrecognized statistics: ',
                                                paste(setdiff(statistics, stat_names), collapse = ', '),
                                                '\nTry setting "statistics" explicitly in the call to huxreg()')
    stat_names <- statistics
  }
  sumstats <- lapply(all_sumstats, merge, x = data.frame(stat = stat_names), by = 'stat', all.x = TRUE, sort = FALSE)
  sumstats <- lapply(sumstats, function (x) x[match(stat_names, x$stat), ])
  ss_classes <- lapply(sumstats, function (x) x$class)
  sumstats <- lapply(sumstats, function (x) x$V1)
  sumstats <- Reduce(cbind, sumstats)
  sumstats <- apply(
    sumstats,
    c(1, 2),
    function(x) {
      if (x %% 1 != 0 & is.numeric(number_format)) {
        format_number(x, number_format)
      } else {
        x
      }
    }
  )
  ss_classes <- Reduce(cbind, ss_classes)

  sumstats <- hux(sumstats)
  number_format(sumstats) <- number_format
  number_format(sumstats)[ss_classes == 'integer'] <- 0

  if (error_pos == 'right') {
    sumstats2 <- as_hux(matrix('', nrow(sumstats), ncol(sumstats) * 2))
    for (i in seq_len(ncol(sumstats))) {
      sumstats2[, i * 2 - 1] <- sumstats[, i]
    }
    sumstats <- sumstats2
  }
  cols <- cbind(if (error_pos == 'below') interleave(coef_names, '') else coef_names, cols,
                copy_cell_props = FALSE)
  sumstats <- cbind(names_or(stat_names, stat_names), sumstats, copy_cell_props = FALSE)

  if (error_pos == 'right') mod_names <- interleave(mod_names, '')
  mod_names <- c('', mod_names)
  result <- rbind(mod_names, cols, sumstats, copy_cell_props = FALSE)
  if (isTRUE(borders)) result <- set_bottom_border(result, c(1, 1 + nrow(cols), nrow(result)), everywhere, 1)
  colnames(result) <- mod_names # may fail
  if (error_pos == 'right') result <- set_colspan(result, 1, evens, 2)
  align(result)[1, ]    <- 'center'
  align(result)[-1, -1] <- 'right'
  pad_decimal(result)[-1, -1] <- pad_decimal

  if (! is.null(note)) {
    stars_note <- paste0(names(stars), ' p < ', stars, collapse = '; ')
    note <- gsub('%stars%', stars_note, note)
    result <- rbind(result, c(note, rep('', ncol(result) - 1)))
    result <- set_colspan(result, final(), 1, ncol(result))
    if (ncol(result) > 1) {
      result <- set_colspan(result, final(), 2:ncol(result), 0)
    }
    result <- set_wrap(result, final(), 1, TRUE)
    result <- set_align(result, final(), 1, 'left')
    result <- set_bottom_border(result, final(), everywhere, 0)
  }

  xlevels <- Map(function(x) x[['xlevels']], models)
  attributes(result)[['xlevels']] <-
    if (length(xlevels) > 1) {
      Reduce(modifyList, xlevels)
    } else {
      xlevels[[1]]
    }
  term_labels <- Map(
    function(x) attributes(x[['terms']])[['term.labels']], models
  )
  attributes(result)[['term_labels']] <- unique(unlist(term_labels))

  attributes(result)[['heading_rows']] <- heading_rows
  tabular_environment(result) <- 'longtable'

  result
}
environment(huxreg2) <- environment(huxreg)

huxtable_headrow = function(hux_table, heading) {
  ncols = ncol(hux_table)
  heading_row = c(
    sprintf('<<HEADING>>%s<</HEADING>>', heading), rep(NA, ncols - 1)
  ) %>%
    as.list() %>%
    as.data.frame()

   heading_huxtable = huxtable(heading_row)
   italic(heading_huxtable)[1, 1] = TRUE
   colspan(heading_huxtable)[1, 1] = ncols
   colspan(heading_huxtable)[1, 2:ncols] = 0
   top_padding(heading_huxtable) = 30
   heading_huxtable
}

base_vars = function(result, no_binary_heading = TRUE) {
  xlevels = attributes(result)$xlevels
  xlevels_names = names(xlevels)
  base_levels = Map(function(x) x[[1]], xlevels)
  term_labels = attributes(result)$term_labels

  xlevels_names_rev_sort = xlevels_names[
    nchar(xlevels_names) %>% order(xlevels_names, decreasing = TRUE)
  ]

  interactions = grep('^.*\\S:.*$', term_labels)
  if (length(interactions > 0)) {
    interaction_list = term_labels[interactions] %>% strsplit(':')

    for (interaction in interaction_list) {
      interaction_xlevels = xlevels[interaction]
      interaction_rx = interaction
      for (i in seq_along(interaction_xlevels)) {
        if (is.null(interaction_xlevels[[i]])) {
          interaction_rx[[i]] = sprintf('(%s)',
                                        Hmisc::escapeRegex(interaction_rx[[i]]))
        } else {
          interaction_rx[[i]] = sprintf('%s(.*)',
                                        Hmisc::escapeRegex(interaction_rx[[i]]))
        }
      }
      rx_seek = paste0(interaction_rx, collapse = ':')
      rx_seek = paste0('^', rx_seek, '$')

      min_location = min(grep(rx_seek, result[[1]]))

      interact_base_add_omit = Map(
        function(lst, name) sprintf('%s (%s)', name, lst),
        base_levels[interaction],
        names(base_levels[interaction])
      )
      for (i in seq_along(interact_base_add_omit)) {
        if (length(interact_base_add_omit[[i]]) == 0) {
          interact_base_add_omit[[i]] = interaction[[i]]
        }
      }
      interact_heading = paste0(interact_base_add_omit, collapse = ' * ')

      # FIXME: no_binary_heading logic correct? Shoud padding be an option?
      if (all(Map(length, interaction_xlevels) <= 2) & no_binary_heading) {
        top_padding(result[min_location, ]) = 30
      } else {
      #if (any(Map(length, interaction_xlevels) > 2) & no_binary_heading) {
        result = rbind(
          result[1:min_location - 1,],
          huxtable_headrow(result, interact_heading),
          result[min_location:nrow(result),]
        )
      }

      num_interactors = length(interaction)
      result[[1]] = gsub(
        rx_seek,
        paste0('\\', 1:num_interactors, collapse = ' * '),
        result[[1]]
      )
    }
  }

  for (var in xlevels_names_rev_sort) {
    rx_seek = paste0('^', Hmisc::escapeRegex(var), '(.*)$')
    min_location = min(grep(rx_seek, result[[1]]))
    result[[1]] = gsub(rx_seek, '\\1', result[[1]])

    if (length(xlevels[[var]]) <= 2 & no_binary_heading) {
      top_padding(result[min_location, ]) = 30
    }
    # FIXME: no_binary_heading logic correct? Padding as an option?
    if (length(xlevels[[var]]) > 2 & no_binary_heading) {
      result = rbind(
        result[1:min_location - 1,],
        huxtable_headrow(
          result, sprintf('%s (%s)', names(base_levels[var]), base_levels[var])
        ),
        result[min_location:nrow(result),]
      )
    }
  }

  result[[1]] = gsub('<<HEADING>>', '', result[[1]], fixed = TRUE)
  result[[1]] = gsub('<</HEADING>>', '', result[[1]], fixed = TRUE)
  result
}

var_labels = function(result, var_list) {
  var_list_ordered = var_list[
    names(var_list) %>% nchar() %>% order(names(var_list), decreasing = TRUE)
  ]

  for (el in key_value(var_list_ordered)) {
    result[[1]] = gsub(el$key, el$value, result[[1]], fixed = TRUE)
  }
  result
}

key_value = function(lst) {
  kv = Map(function(x, y) list(key = x, value = y), names(lst), lst)
  names(kv) = NULL
  kv
}

output_latex = function(result) {
  attrs = attributes(result)
  ncols = ncol(result)
  # FIXME: This assumes the longtable dcolumn environment. We may want some
  #        options here, although not necessarily for regression output.
  header = ''
  for (i in 1:attrs$heading_rows) {
    current_header = paste0(
        unlist(Map(
          function(x) {
            if (x != '') sprintf('\\multicolumn{1}{c}{%s}', x) else x
          },
          sanitize(result[i, ])
        )),
        collapse = ' & '
      )
    current_header = paste(current_header, '\\\\ \\midrule')
    header = c(header, current_header)
  }

  preamble = c(
    sprintf('\\begin{longtable}{l%s}',
            rep('D{.}{.}{6}', ncols - 1) %>% paste0(collapse = '')),
    sprintf('\\caption{%s}',  sanitize(attrs$caption)),
    sprintf('\\label{%s} \\\\',  sanitize(attrs$caption)),
    header,
    '\\endfirsthead',
    sprintf('\\caption*{%s} \\\\',
            sanitize(paste(attrs$caption, '(Continued)'))),
    header,
    '\\endhead',
    '\\bottomrule',
    sprintf('\\multicolumn{%s}{r@{}}{continued \\ldots}\\\\', ncol(result)),
    '\\endfoot',
    '\\endlastfoot'
  )

  output = preamble

  for (i in (attrs$heading_rows + 1):nrow(result)) {
    row = result[i, ]
    row = sanitize(row)

    row = unlist(Map(
      function(x) {
        if (!is.na(x) & substr(x, 1, 1) == '(') {
          paste0('\\hbox{}', x)
        } else {
          x
        }
      },
      row,
      USE.NAMES = FALSE
    ))

    row = unlist(Map(
      function(value, attribute) {
        if (attribute) sprintf('\\emph{%s}', value) else value
      },
      row,
      attrs$italic[i, ]
    ))

    add_multicolumn = unlist(Map(
      function(value, attribute) {
        if (attribute > 1) {
          sprintf('\\multicolumn{%s}{l}{%s}', attribute, value)
        } else {
          value
        }
      },
      row,
      attrs$colspan[i, ]
    ))

    row = add_multicolumn[attrs$colspan[i, ] > 0]
    row = gsub(' (\\*+)$', '^{\\1}', row)

    if (all(top_padding(result)[i, ] == 30)) {
      row[1] = paste0('\\noalign{\\vskip 4mm}', row[1])
    }

    row = paste(row, collapse = ' & ')
    row = paste(row, '\\\\')
    row = gsub('NA', '', row, fixed = TRUE)

    add_bottom_border = all(attrs$bottom_border[i, ] == 1)
    if (add_bottom_border) {
      row = paste0(row, ' \\midrule')
    }

    output = append(output, row)
  }

  output = append(output, '\\end{longtable}')
  unlist(output)
}

add_latex_preamble = function(output) {
  c(
    '\\documentclass[10pt]{article}',
    '\\usepackage{booktabs}',
    '\\usepackage{numprint}',
    '\\usepackage{dcolumn}',
    '\\usepackage{longtable}',
    '\\usepackage[margin=1in]{geometry}',
    '\\usepackage[justification=centering]{caption}',
    '\\usepackage[hang,flushmargin]{footmisc}',
    '{\\renewcommand{\\arraystretch}{1.1}',
    '',
    '\\begin{document}',
    '\\begin{center}',
    '',
    output
  )
}

add_latex_end = function(output) {
  c(
    output,
    '',
    '\\end{center}',
    '\\end{document}'
  )
}

add_latex_pagebreak = function(output) {
  c(
    output,
    '',
    '\\pagebreak',
    ''
  )
}
