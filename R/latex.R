#' add_reg_labels
#'
#' Output a huxtable table to LaTeX.
#'
#' @param ht A huxtable.
#'
#' @export
#'
output_latex <- function(ht) {
  attrs <- attributes(ht)
  ncols <- ncol(ht)
  # FIXME: This assumes the longtable dcolumn environment. We may want some
  #        options here, although not necessarily for regression output.
  header <- ''
  for (i in 1:attrs$heading_rows) {
    current_header <- paste0(
        unlist(Map(
          function(x) {
            if (x != '') sprintf('\\multicolumn{1}{c}{%s}', x) else x
          },
          xtable::sanitize(ht[i, ])
        )),
        collapse <- ' & '
      )
    current_header <- paste(current_header, '\\\\ \\midrule')
    header <- c(header, current_header)
  }

  preamble <- c(
    sprintf(
      '\\begin{longtable}{l%s}',
      paste0(rep('D{.}{.}{6}', ncols - 1), collapse = '')
    ),
    sprintf('\\caption{%s}',  xtable::sanitize(attrs$caption)),
    sprintf('\\label{%s} \\\\',  xtable::sanitize(attrs$caption)),
    header,
    '\\endfirsthead',
    sprintf('\\caption*{%s} \\\\',
            xtable::sanitize(paste(attrs$caption, '(Continued)'))),
    header,
    '\\endhead',
    '\\bottomrule',
    sprintf('\\multicolumn{%s}{r@{}}{continued \\ldots}\\\\', ncol(ht)),
    '\\endfoot',
    '\\endlastfoot'
  )

  output <- preamble

  for (i in (attrs$heading_rows + 1):nrow(ht)) {
    row <- ht[i, ]
    row <- xtable::sanitize(row)

    row <- unlist(Map(
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

    row <- unlist(Map(
      function(value, attribute) {
        if (attribute) sprintf('\\emph{%s}', value) else value
      },
      row,
      attrs$italic[i, ]
    ))

    add_multicolumn <- unlist(Map(
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

    row <- add_multicolumn[attrs$colspan[i, ] > 0]
    row <- gsub(' (\\*+)$', '^{\\1}', row)

    if (all(attributes(ht[i, ])$top_padding == 30)) {
      row[1] <- paste0('\\noalign{\\vskip 4mm}', row[1])
    }

    row <- paste(row, collapse = ' & ')
    row <- paste(row, '\\\\')
    row <- gsub('NA', '', row, fixed = TRUE)

    add_bottom_border <- all(attrs$bottom_border[i, ] == 1)
    if (add_bottom_border) {
      row <- paste0(row, ' \\midrule')
    }

    output <- append(output, row)
  }

  output <- append(output, '\\end{longtable}')
  unlist(output)
}

#' add_latex_preamble
#'
#' Add a LaTeX preamble.
#'
#' @param output The vector of LaTeX lines to which to add the preamble to. It
#'        is added to the beginning of the vector.
#'
#' @export
#'
add_latex_preamble <- function(output) {
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

#' add_latex_end
#'
#' "Close" the LaTeX document.
#'
#' @param output The vector of LaTeX lines to which to add the closing lines to.
#'        It is added to the beginning of the vector.
#'
#' @export
#'
add_latex_end <- function(output) {
  c(
    output,
    '',
    '\\end{center}',
    '\\end{document}'
  )
}

add_latex_pagebreak <- function(output) {
  c(
    output,
    '',
    '\\pagebreak',
    ''
  )
}
