convert_names_from_null <- function(x) {
  if(is.null(names(x))) {
    names(x) <- rep("", length(x))
  }
  x
}

regout <- function(..., heading_rows = 1) {
  args <- convert_names_from_null(list(...))
  unnamed_args <- names(args) == ''

  model_args <- args[unnamed_args]
  options <- args[!unnamed_args]

  hux_table <- do.call(huxtable::huxreg, c(model_args, options))

  huxtable::tabular_environment(hux_table) <- 'longtable'

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

huxtable_headrow <- function(hux_table, heading) {
  ncols <- ncol(hux_table)
  heading_row <- c(
    sprintf('<<HEADING>>%s<</HEADING>>', heading), rep(NA, ncols - 1)
  ) %>%
    as.list() %>%
    as.data.frame()

   heading_huxtable <- huxtable(heading_row)
   italic(heading_huxtable)[1, 1] <- TRUE
   colspan(heading_huxtable)[1, 1] <- ncols
   colspan(heading_huxtable)[1, 2:ncols] <- 0
   top_padding(heading_huxtable) <- 30
   heading_huxtable
}

base_vars <- function(result, no_binary_heading = TRUE) {
  xlevels <- attributes(result)$xlevels
  xlevels_names <- names(xlevels)
  base_levels <- Map(function(x) x[[1]], xlevels)
  term_labels <- attributes(result)$term_labels

  xlevels_names_rev_sort <- xlevels_names[
    nchar(xlevels_names) %>% order(xlevels_names, decreasing = TRUE)
  ]

  interactions <- grep('^.*\\S:.*$', term_labels)
  if (length(interactions > 0)) {
    interaction_list <- term_labels[interactions] %>% strsplit(':')

    for (interaction in interaction_list) {
      interaction_xlevels <- xlevels[interaction]
      interaction_rx <- interaction
      for (i in seq_along(interaction_xlevels)) {
        if (is.null(interaction_xlevels[[i]])) {
          interaction_rx[[i]] <- sprintf('(%s)',
                                        Hmisc::escapeRegex(interaction_rx[[i]]))
        } else {
          interaction_rx[[i]] <- sprintf('%s(.*)',
                                        Hmisc::escapeRegex(interaction_rx[[i]]))
        }
      }
      rx_seek <- paste0(interaction_rx, collapse = ':')
      rx_seek <- paste0('^', rx_seek, '$')

      min_location <- min(grep(rx_seek, result[[1]]))

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
        top_padding(result[min_location, ]) <- 30
      } else {
      #if (any(Map(length, interaction_xlevels) > 2) & no_binary_heading) {
        result <- rbind(
          result[1:min_location - 1,],
          huxtable_headrow(result, interact_heading),
          result[min_location:nrow(result),]
        )
      }

      num_interactors <- length(interaction)
      result[[1]] <- gsub(
        rx_seek,
        paste0('\\', 1:num_interactors, collapse = ' * '),
        result[[1]]
      )
    }
  }

  for (var in xlevels_names_rev_sort) {
    rx_seek <- paste0('^', Hmisc::escapeRegex(var), '(.*)$')
    min_location <- min(grep(rx_seek, result[[1]]))
    result[[1]] <- gsub(rx_seek, '\\1', result[[1]])

    if (length(xlevels[[var]]) <= 2 & no_binary_heading) {
      top_padding(result[min_location, ]) <- 30
    }
    # FIXME: no_binary_heading logic correct? Padding as an option?
    if (length(xlevels[[var]]) > 2 & no_binary_heading) {
      result <- rbind(
        result[1:min_location - 1,],
        huxtable_headrow(
          result, sprintf('%s (%s)', names(base_levels[var]), base_levels[var])
        ),
        result[min_location:nrow(result),]
      )
    }
  }

  result[[1]] <- gsub('<<HEADING>>', '', result[[1]], fixed = TRUE)
  result[[1]] <- gsub('<</HEADING>>', '', result[[1]], fixed = TRUE)
  result
}

add_reg_labels <- function(result, var_list) {
  var_list_ordered <- var_list[
    names(var_list) %>% nchar() %>% order(names(var_list), decreasing = TRUE)
  ]

  for (el in key_value(var_list_ordered)) {
    result[[1]] <- gsub(el$key, el$value, result[[1]], fixed = TRUE)
  }
  result
}
