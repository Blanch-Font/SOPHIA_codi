# ==============================================================================
#
# DESCRIPTIVE OVERVIEW OF NUMERIC AND FACTOR VARIABLES
#
# Author: Carl Delfin (CRDF)
# Version: 2
# Date: 2022-10-07
#
# Instructions:
#
# (1) You must have the openxlsx package in order to save output as Excel sheets:
#     install.packages("openxlsx", dependencies = TRUE)
#
# (2) You must have the data loaded in a data frame named 'df' before running
#     the script, and the data must be processed according to the variable
#     format table provided.
#
# (3) You must set a path where the output will be saved, edit the 'path'
#     variable below so that it matches your system.
#
# (4) If you want to have output by groups, you can add as many grouping
#     variables as you want in the 'group_vars' vector below. You will get
#     one output for each grouping variable.
#
# (5) OPTIONAL: If you want to filter out cases where the smallest group or
#     count is less than 5, set 'filter_5' to TRUE below.
#
# (6) When the above is done, just run the script, e.g. by pressing the
#     'Source' button in RStudio.
#
# ==============================================================================

# change as necessary:
path <- ""

# change as necessary:
group_vars <- c("sex", "cat.bmi", "cat.age", "cat.hba1c", "cat.fglucose", "hypoglyc")

# change as necessary
filter_5 <- TRUE

# ------------------------------------------------------------------------------
cat("Running initial checks...\n")
# ------------------------------------------------------------------------------

# check if a data frame named 'df' is available
if (exists("df") == FALSE)
  stop("You must have a data frame named 'df' available in the Global Environment!")

# check if an object named 'path' is available
if (exists("path") == FALSE)
  stop("You must define where to save results in a 'path' object!")

# only proceed if openxlsx package is installed
if (nzchar(system.file(package = "openxlsx")) == FALSE)
  stop("You must have the 'openxlsx' package installed!")

# notify user about filtering
if (filter_5 == TRUE)
  cat("Groups and counts with n < 5 are filtered out before saving data!\n")

# ------------------------------------------------------------------------------
cat("Creating necessary functions...\n")
# ------------------------------------------------------------------------------

# get descriptives from numeric variables:

get_numeric_desc <- function(variable) {
  tmp <- df[, variable]
  out <- data.frame(
    variable = variable,
    n_total = length(tmp),
    n_missing = sum(is.na(tmp)),
    n_complete = sum(!is.na(tmp)),
    perc_complete = round((sum(!is.na(tmp)) / length(tmp)) * 100, 2),
    min = round(min(tmp, na.rm = TRUE), 2),
    max = round(max(tmp, na.rm = TRUE), 2),
    mean = round(mean(tmp, na.rm = TRUE), 2),
    sd = round(sd(tmp, na.rm = TRUE), 2),
    se = round(sd(tmp, na.rm = TRUE) / sqrt(sum(!is.na(tmp))), 2),
    median = round(median(tmp, na.rm = TRUE), 2),
    q25 = round(quantile(tmp, probs = 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(tmp, probs = 0.75, na.rm = TRUE), 2))

  rownames(out) <- NULL
  return(out)
}

# descriptives from numeric variables, by group:

get_numeric_desc_grp <- function(variable, group) {

  res <- NULL
  out <- NULL

  for (i in levels(df[, group])) {
    tmp <- subset(df, eval(parse(text = group)) == i)
    tmp <- tmp[, variable]

    res <- data.frame(
      variable = variable,
      group_var = group,
      group_level = i,
      n_total = length(tmp),
      n_missing = sum(is.na(tmp)),
      n_complete = sum(!is.na(tmp)),
      perc_complete = round((sum(!is.na(tmp)) / length(tmp)) * 100, 2),
      min = round(min(tmp, na.rm = TRUE), 2),
      max = round(max(tmp, na.rm = TRUE), 2),
      mean = round(mean(tmp, na.rm = TRUE), 2),
      sd = round(sd(tmp, na.rm = TRUE), 2),
      se = round(sd(tmp, na.rm = TRUE) / sqrt(sum(!is.na(tmp))), 2),
      median = round(median(tmp, na.rm = TRUE), 2),
      q25 = round(quantile(tmp, probs = 0.25, na.rm = TRUE), 2),
      q75 = round(quantile(tmp, probs = 0.75, na.rm = TRUE), 2))

    rownames(res) <- NULL
    out <- rbind(res, out)
  }

  return(out)
}

# descriptives from factor variables:

get_factor_desc <- function(variable) {

  res <- NULL
  out <- NULL

  for (i in levels(df[, variable])) {
    tmp <- subset(df, eval(parse(text = variable)) == i)
    tmp <- tmp[, variable]

    res <- data.frame(
      variable = variable,
      n_complete = sum(!is.na(df[, variable])),
      variable_level = i,
      count = length(tmp))

    rownames(res) <- NULL
    out <- rbind(res, out)
  }

  out$perc_of_n_complete <- round((out$count / sum(!is.na(df[, variable]))) * 100, 2)
  return(out)
}

# descriptives from factor variables, by group:

get_factor_desc_grp <- function(variable, group) {

  res <- NULL
  out <- NULL

  for (i in levels(df[, variable])) {
    tmp1 <- subset(df, eval(parse(text = variable)) == i)
    tmp1 <- tmp1[, c(variable, group)]

    for (j in levels(df[, group])) {
      tmp2 <- subset(tmp1, eval(parse(text = group)) == j)
      res <- data.frame(
        variable = variable,
        variable_level = i,
        group = group,
        group_level = j,
        n_complete_within_variable_level = nrow(tmp1),
        n_complete_within_group_level = length(na.omit(df[, group])),
        n_complete_in_both_variable_and_group = nrow(na.omit(df[, c(variable, group)])),
        count_within_variable_level_and_group_level = nrow(tmp2),
        perc_of_n_complete_in_both_variable_and_group = round((nrow(tmp2) / nrow(na.omit(df[, c(variable, group)]))) * 100, 2))

      rownames(res) <- NULL
      out <- rbind(out, res)
    }
  }

  return(out)
}

# ------------------------------------------------------------------------------
cat("Creating descriptive summaries for numeric variables...\n")
# ------------------------------------------------------------------------------

# all numeric variables in 'df'
numeric_desc <- do.call(rbind.data.frame,
                        lapply(colnames(df[sapply(df, is.numeric)]),
                               get_numeric_desc))

openxlsx::write.xlsx(numeric_desc,
                     paste0(path, "numeric_desc.xlsx"))

# all numeric variables in 'df', by category, if specified by user
for (i in group_vars) {
  numeric_desc_grp <- do.call(rbind.data.frame,
                              mapply(get_numeric_desc_grp,
                                     colnames(df[sapply(df, is.numeric)]),
                                     group = i,
                                     SIMPLIFY = FALSE))

  rownames(numeric_desc_grp) <- NULL

  # filter?
  if (filter_5 == TRUE) {
    numeric_desc_grp <- numeric_desc_grp[numeric_desc_grp$n_complete >= 5, ]
  }

  openxlsx::write.xlsx(numeric_desc_grp,
                       paste0(path, "numeric_desc_by_", i, ".xlsx"))
}

# ------------------------------------------------------------------------------
cat("Creating descriptive summaries for factor variables...\n")
# ------------------------------------------------------------------------------

# all factor variables in 'df'
factor_desc <- do.call(rbind.data.frame,
                       lapply(colnames(df[sapply(df, is.factor)]),
                              get_factor_desc))

# filter?
if (filter_5 == TRUE) {
  factor_desc <- factor_desc[factor_desc$count >= 5, ]
}

openxlsx::write.xlsx(factor_desc,
                     paste0(path, "factor_desc.xlsx"))

# all factor variables in 'df', by category, if specified by user
for (i in group_vars) {
  factor_desc_grp <- do.call(rbind.data.frame,
                              mapply(get_factor_desc_grp,
                                     colnames(df[sapply(df, is.factor)]),
                                     group = i,
                                     SIMPLIFY = FALSE))

  rownames(factor_desc_grp) <- NULL

  # filter?
  if (filter_5 == TRUE) {
    factor_desc_grp <- factor_desc_grp[factor_desc_grp$count >= 5, ]
  }

  openxlsx::write.xlsx(factor_desc_grp,
                       paste0(path, "factor_desc_by_", i, ".xlsx"))
}

# ------------------------------------------------------------------------------
cat("Finished!\n")
# ------------------------------------------------------------------------------
