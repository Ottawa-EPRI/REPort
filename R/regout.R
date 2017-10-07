convert_names_from_null <- function(x) {
  if(is.null(names(x))) {
    names(x) <- rep("", length(x))
  }
  x
}

huxtable_headrow <- function(ht, heading) {
  ncols <- ncol(ht)
  heading_row <- as.data.frame(as.list(c(
    sprintf('<<HEADING>>%s<</HEADING>>', heading), rep(NA, ncols - 1)
  )))

  heading_huxtable <- huxtable::huxtable(heading_row)
  huxtable::set_italic(heading_huxtable[1, 1], TRUE)
  # Note that while there is a function within huxtable to set the colspan,
  # it will _not_ work as we intend. We will therefore override manually.
  attributes(heading_huxtable)$colspan[1, 1] <- ncols
  attributes(heading_huxtable)$colspan[1, 2:ncols] <- 0
  huxtable::set_top_padding(heading_huxtable, 30)
  heading_huxtable
}

#' regout
#'
#' A wrapper around huxreg which allows specification of the number of initial
#' heading rows. It also adds model term labels and xlevels as attributes.
#'
#' @param ... arguments to be passed to huxreg.
#'
#' @param heading_rows The number of rows which are headings.
#'
#' @export
#'
regout <- function(..., heading_rows = 1) {
  args <- convert_names_from_null(list(...))
  unnamed_args <- names(args) == ''

  model_args <- args[unnamed_args]
  options <- args[!unnamed_args]

  hux_table <- do.call(huxtable::huxreg, c(model_args, options))

  hux_table <- huxtable::set_tabular_environment(hux_table, 'longtable')

  model_xlevels <- Map(function(x) x[['xlevels']], model_args)
  if (length(model_xlevels == 1)) {
    attributes(hux_table)$xlevels <- model_xlevels[[1]]
  } else if (length(model_xlevels) > 1) {
    attributes(hux_table)$xlevels <- Reduce(modifyList, model_xlevels)
  }

  term_labels <- Map(
    function(x) attributes(x$terms)$term.labels,
    model_args
  )
  attributes(hux_table)$term_labels <- unique(unlist(term_labels))

  attributes(hux_table)$heading_rows <- heading_rows

  hux_table
}

#' add_base_vars
#'
#' Add base (omitted) variables as a heading row to the regression.
#'
#' @param ht A huxtable.
#'
#' @param no_binary_heading Do not add heading row for factors with 2 values.
#'
#' @export
#'
add_base_vars <- function(ht, no_binary_heading = TRUE) {
  xlevels <- attributes(ht)$xlevels
  xlevels_names <- names(xlevels)
  base_levels <- Map(function(x) x[[1]], xlevels)
  term_labels <- attributes(ht)$term_labels

  xlevels_names_rev_sort <- xlevels_names[
    order(nchar(xlevels_names), xlevels_names, decreasing = TRUE)
  ]

  interactions <- grep('^.*\\S:.*$', term_labels)
  if (length(interactions > 0)) {
    interaction_list <- strsplit(term_labels[interactions], ':')

    for (interaction in interaction_list) {
      interaction_xlevels <- xlevels[interaction]
      interaction_rx <- interaction
      for (i in seq_along(interaction_xlevels)) {
        if (is.null(interaction_xlevels[[i]])) {
          interaction_rx[[i]] <- sprintf(
            '(%s)', Hmisc::escapeRegex(interaction_rx[[i]])
          )
        } else {
          interaction_rx[[i]] <- sprintf(
            '%s(.*)', Hmisc::escapeRegex(interaction_rx[[i]])
          )
        }
      }
      rx_seek <- paste0(interaction_rx, collapse = ':')
      rx_seek <- paste0('^', rx_seek, '$')

      min_location <- min(grep(rx_seek, ht[[1]]))

      interact_base_add_omit <- Map(
        function(lst, name) sprintf('%s (%s)', name, lst),
        base_levels[interaction],
        names(base_levels[interaction])
      )
      for (i in seq_along(interact_base_add_omit)) {
        if (length(interact_base_add_omit[[i]]) == 0) {
          interact_base_add_omit[[i]] <- interaction[[i]]
        }
      }
      interact_heading <- paste0(interact_base_add_omit, collapse = ' * ')

      # FIXME: no_binary_heading logic correct? Shoud padding be an option?
      if (all(Map(length, interaction_xlevels) <= 2) & no_binary_heading) {
        top_padding(ht[min_location, ]) <- 30
      } else {
        #if (any(Map(length, interaction_xlevels) > 2) & no_binary_heading) {
        ht <- rbind(
          ht[1:min_location - 1,],
          huxtable_headrow(ht, interact_heading),
          ht[min_location:nrow(ht),]
        )
      }

      num_interactors <- length(interaction)
      ht[[1]] <- gsub(
        rx_seek,
        paste0('\\', 1:num_interactors, collapse = ' * '),
        ht[[1]]
      )
    }
  }

  for (var in xlevels_names_rev_sort) {
    rx_seek <- paste0('^', Hmisc::escapeRegex(var), '(.*)$')
    min_location <- min(grep(rx_seek, ht[[1]]))
    ht[[1]] <- gsub(rx_seek, '\\1', ht[[1]])

    if (length(xlevels[[var]]) <= 2 & no_binary_heading) {
      top_padding(ht[min_location, ]) <- 30
    }
    # FIXME: no_binary_heading logic correct? Padding as an option?
    if (length(xlevels[[var]]) > 2 & no_binary_heading) {
      ht <- rbind(
        ht[1:min_location - 1,],
        huxtable_headrow(
          ht, sprintf('%s (%s)', names(base_levels[var]), base_levels[var])
        ),
        ht[min_location:nrow(ht),]
      )
    }
  }

  ht[[1]] <- gsub('<<HEADING>>', '', ht[[1]], fixed = TRUE)
  ht[[1]] <- gsub('<</HEADING>>', '', ht[[1]], fixed = TRUE)
  ht
}

#' add_reg_labels
#'
#' Add regression labels.
#'
#' @param ht A huxtable.
#'
#' @param var_list A named list, where names are the original raw column names
#'        and the values are the intended labels.
#'
#' @export
#'
add_reg_labels <- function(ht, var_list) {
  var_list_ordered <- var_list[
    order(nchar(names(var_list)), names(var_list), decreasing = TRUE)
  ]

  for (i in seq_along(var_list_ordered)) {
    key <- names(var_list_ordered)[i]
    value <- var_list_ordered[[i]]
    ht[[1]] <- gsub(key, value, ht[[1]], fixed = TRUE)
  }
  ht
}
