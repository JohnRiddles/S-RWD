#' Calculates identity disclosure
#'
#' Calculates measures of identity disclosure from the keys using the synthetic and original data. The function accepts both regular data frames and duckplyr data frames
#'
#' @param data_s A data frame. Synthetic data
#' @param data_o A data frame. Original data
#' @param keys A character vector. Specifying the characteristics that can be directly identified. The variables will all be treated as categorized variables.
#'
#' @return A named list with a component for each identity disclosure measure. Including UiS, UiO, RepU, UiOiS.
#' \describe{
#' \item{RepU} {Developed by Raab et al. (2024) to evaluate the likelihood that an individual record from the original dataset can be uniquely identified within the synthetic data.
#' This is using k-anonymy approach.}
#' }
#'
#' @export

get_identity_disclosure <- function(data_s, data_o, keys) {
  n_data_o <- data_o |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::pull(n) |>
    as.numeric()

  # UiS (% Unique in Synthetic): % of the orig records would have unique combinations of these keys in Synthetic.
  # num: On data_s, count by keys, then filter on uniques
  # denom: n(data_o)
  UiS_num <- data_s |>
    dplyr::summarize(Frequency = dplyr::n(), .by = all_of(keys)) |>
    dplyr::filter(Frequency == 1L)
  UiS <- UiS_num |>
    dplyr::summarize(UiS = dplyr::n()) |>
    dplyr::pull(UiS) |>
    as.numeric()
  UiS <- UiS / n_data_o * 100.0

  # UiO (% Unique in Original): % of the orig records would have unique combinations of these keys in Original.
  # num: On data_o, count by keys, then filter on uniques
  # denom: n(data_o)
  UiO_num <- data_o |>
    dplyr::summarize(Frequency = dplyr::n(), .by = all_of(keys)) |>
    dplyr::filter(Frequency == 1L)
  UiO <- UiO_num |>
    dplyr::summarize(UiO = dplyr::n()) |>
    dplyr::pull(UiO) |>
    as.numeric()
  UiO <- UiO / n_data_o * 100.0

  # RepU (Replicated uniques): % of the orig records would be unique in the orig and also unique in the synthetic.
  # num: On data_o, count by keys, then filter on uniques.
  #      Then look up keys for each of those records in the data_s uniques (i.e. unique records on keys in data_s),
  #      count the ones that could find the match.
  # denom: n(data_o)
  RepU_num <- UiO_num |>
    dplyr::semi_join(UiS_num |> dplyr::select(all_of(keys)), by = keys) |>
    dplyr::mutate(flag = 1L)
  RepU <- RepU_num |>
    dplyr::summarize(RepU = dplyr::n()) |>
    dplyr::pull(RepU) |>
    as.numeric()
  RepU <- RepU / n_data_o * 100.0

  # UiOiS: % Unique in Original in Synthetic
  # num: On data_o, filter on records that have attributes keys matched to data_s. Then count the ones that are also uniques.
  # denom: n(data_o)
  UiOiS_num <- data_o |>
    dplyr::semi_join(data_s |> dplyr::select(all_of(keys)), by = keys) |>
    dplyr::mutate(flag = 1L) |>
    dplyr::summarize(Frequency = dplyr::n(), .by = all_of(keys)) |>
    filter(Frequency == 1)
  UiOiS <- UiOiS_num |>
    dplyr::summarize(UiOiS = dplyr::n()) |>
    dplyr::pull(UiOiS) |>
    as.numeric()
  UiOiS <- UiOiS / n_data_o * 100.0

  # Return all results as a list
  list(
    UiS = UiS,
    UiO = UiO,
    RepU = RepU,
    UiOiS = UiOiS
    #  UiS_num = UiS_num,
    #  UiO_num = UiO_num,
    #  RepU_num = RepU_num,
    #  UiOiS_num = UiOiS_num
  )
}

#' Calculates attribute disclosure
#'
#' Calculates measures of attribute disclosure from the keys and targets using the synthetic and original data. The function accepts both regular data frames and duckplyr data frames
#'
#' @param data_s A data frame. Synthetic data
#' @param data_o A data frame. Original data
#' @param keys A character vector. Specifying the characteristics that can be directly identified. The variables will all be treated as categorized variables.
#' @param target A character. Specifying the target that is sensitive. The variable will be treated as categorized variable.
#'
#' @return A named list with a component for each attribute disclosure measure. Including Dorig, Dsyn, DiSDiO, DiSCO, TCAP
#'
#' @export

get_attribute_disclosure <- function(data_s, data_o, keys, target) {
  n_data_o <- data_o |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::pull(n) |>
    as.numeric()

  n_data_s <- data_s |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::pull(n) |>
    as.numeric()

  # Add the flag to original data where the records on keys match the synthetic
  ods_wt_flag <- data_o |>
    dplyr::left_join(
      data_s |>
        dplyr::select(all_of(keys)) |>
        dplyr::distinct() |>
        dplyr::mutate(.match = 1L),
      by = keys
    ) |>
    dplyr::mutate(flag = dplyr::coalesce(.match, 0L)) |>
    dplyr::select(-.match)

  # Add the flag to synthetic data where the records on keys match the original
  syn_wt_flag <- data_s |>
    dplyr::left_join(
      data_o |>
        dplyr::select(all_of(keys)) |>
        dplyr::distinct() |>
        dplyr::mutate(.match = 1L),
      by = keys
    ) |>
    dplyr::mutate(flag = dplyr::coalesce(.match, 0L)) |>
    dplyr::select(-.match)

  # Dorig (% Disclosive in Original): % of the orig records having same level of Target on each keys combinations
  # num: On data_o, count records having the same level of Target by keys (i.e. disclosive records on keys and target)
  # denom: n(data_o)
  dis_tar <- data_o |>
    dplyr::summarize(
      n_level_target = dplyr::n_distinct(!!sym(target)),
      .by = all_of(keys)
    ) |>
    dplyr::filter(n_level_target == 1L)
  Dorig_num <- data_o |>
    dplyr::semi_join(dis_tar %>% dplyr::select(all_of(keys)), by = keys)
  Dorig <- Dorig_num |>
    dplyr::summarize(Dorig = dplyr::n()) |>
    dplyr::pull(Dorig) |>
    as.numeric()
  Dorig <- Dorig / n_data_o * 100.0

  # Dsyn (% Disclosive in Synthetic): % of the synthetic records having same level of target on each keys combinations
  # num: On data_s, count records having the same level of Target by keys (i.e. disclosive records on keys and target)
  # denom: n(data_s)
  dis_tar <- data_s |>
    dplyr::summarize(
      n_level_target = dplyr::n_distinct(!!sym(target)),
      .by = all_of(keys)
    ) |>
    dplyr::filter(n_level_target == 1L)
  Dsyn_num <- data_s |>
    dplyr::semi_join(dis_tar |> dplyr::select(all_of(keys)), by = keys)
  Dsyn <- Dsyn_num |>
    dplyr::summarize(Dsyn = dplyr::n()) |>
    dplyr::pull(Dsyn) |>
    as.numeric()
  Dsyn <- Dsyn / n_data_s * 100.0

  # DiSDiO (Disclosive in Synthetic Disclosive in Original)
  # num: on data_o, filter on disclosive records on keys and targets.
  #      count the ones that could match by keys and target to the data_s records that are disclosive on keys and target
  # denom: n(data_o)
  dis_tar <- ods_wt_flag |>
    filter(flag == 1) |>
    dplyr::summarize(
      n_level_target = dplyr::n_distinct(!!sym(target)),
      .by = all_of(keys)
    ) |>
    dplyr::filter(n_level_target == 1L)
  DiSDiO_num <- ods_wt_flag |>
    dplyr::semi_join(dis_tar |> dplyr::select(all_of(keys)), by = keys)
  DiSDiO_num <- DiSDiO_num |>
    dplyr::semi_join(
      Dsyn_num |> dplyr::select(all_of(c(keys, target))),
      by = c(keys, target)
    ) |>
    dplyr::mutate(DiSDiO_flag = 1L)
  DiSDiO <- DiSDiO_num |>
    dplyr::summarize(DiSDiO = dplyr::n()) |>
    dplyr::pull(DiSDiO) |>
    as.numeric()
  DiSDiO <- DiSDiO / n_data_o * 100.0

  # DiSCO (Disclosive in the Synthetic and Correct): the proportion of the orig records that are disclosive in the orig and also in the synthetic with a correct attribution to the target.
  # num: on data_o, count the ones that could match by keys and target to the data_s records that are disclosive on keys and target
  # denom: n(data_o)
  DiSCO_num <- ods_wt_flag |>
    dplyr::semi_join(
      Dsyn_num |> dplyr::select(all_of(c(keys, target))),
      by = c(keys, target)
    ) |>
    dplyr::mutate(DiSCO_flag = 1L)
  DiSCO_num <- DiSCO_num |>
    dplyr::summarize(DiSCO = dplyr::n()) |>
    dplyr::pull(DiSCO) |>
    as.numeric()
  DiSCO <- DiSCO_num / n_data_o * 100.0

  # TCAP: Targeted Correct Attribution Probability
  # Numerator: On data_o, look up Keys and Target for each original record in the data_s records that are disclosive on Target by keys
  #            count the matched ones.
  # Denominator: numbers of records in ODS where could found a match on SYN by Keys.
  TCAP_dnum <- ods_wt_flag |>
    filter(flag == 1) |>
    dplyr::summarize(TCAP_dnum = dplyr::n()) |>
    dplyr::pull(TCAP_dnum) |>
    as.numeric()
  TCAP <- DiSCO_num / TCAP_dnum * 100

  # TCAP_1: Targeted Correct Attribution Probability - Taget 1
  # This is more informative when it comes to the health condition target, with binary outcome 0/1, where value=1 is the sensitive outcome.
  # Numerator: On data_o, look up Keys and Target for each original record in the data_s records that are disclosive on Target = 1 by keys
  #            count the matched ones.
  # Denominator: numbers of records in data_o where could found a match on data_s by Keys.
  dis_tar_1 <- data_s |>
    dplyr::filter(!!sym(target) == 1) |>
    dplyr::summarize(
      n = dplyr::n_distinct(!!sym(target)),
      .by = all_of(keys)
    ) |>
    dplyr::select(-n)
  dis_tar_alt <- dis_tar |>
    dplyr::semi_join(dis_tar_1 |> dplyr::select(all_of(keys)), by = keys)
  Dsyn_num_alt <- data_s |>
    dplyr::semi_join(dis_tar_alt |> dplyr::select(all_of(keys)), by = keys)
  DiSCO_num_alt <- ods_wt_flag |>
    dplyr::semi_join(
      Dsyn_num_alt |> dplyr::select(all_of(c(keys, target))),
      by = c(keys, target)
    ) |>
    dplyr::mutate(DiSCO_flag = 1L)
  DiSCO_num_alt <- DiSCO_num_alt |>
    dplyr::summarize(DiSCO = dplyr::n()) |>
    dplyr::pull(DiSCO) |>
    as.numeric()
  TCAP_1 <- DiSCO_num_alt / TCAP_dnum * 100

  # TCAP_0: Targeted Correct Attribution Probability - Taget 0
  # Numerator: On data_o, look up Keys and Target for each original record in the data_s records that are disclosive on Target = 0 by keys
  #            count the matched ones.
  # Denominator: numbers of records in data_o where could found a match on data_s by Keys.
  dis_tar_2 <- data_s |>
    dplyr::filter(!!sym(target) == 0) |>
    dplyr::summarize(
      n = dplyr::n_distinct(!!sym(target)),
      .by = all_of(keys)
    ) |>
    dplyr::select(-n)
  dis_tar_alt2 <- dis_tar |>
    dplyr::semi_join(dis_tar_2 |> dplyr::select(all_of(keys)), by = keys)
  Dsyn_num_alt2 <- data_s |>
    dplyr::semi_join(dis_tar_alt2 |> dplyr::select(all_of(keys)), by = keys)
  DiSCO_num_alt2 <- ods_wt_flag |>
    dplyr::semi_join(
      Dsyn_num_alt2 |> dplyr::select(all_of(c(keys, target))),
      by = c(keys, target)
    ) |>
    dplyr::mutate(DiSCO_flag = 1L)
  DiSCO_num_alt2 <- DiSCO_num_alt2 |>
    dplyr::summarize(DiSCO = dplyr::n()) |>
    dplyr::pull(DiSCO) |>
    as.numeric()
  TCAP_0 <- DiSCO_num_alt2 / TCAP_dnum * 100

  # Return results
  list(
    #  ods_wt_flag = ods_wt_flag,
    #  syn_wt_flag = syn_wt_flag,
    #  Dorig_num   = Dorig_num,
    Dorig = Dorig,
    Dsyn = Dsyn,
    DiSDiO = DiSDiO,
    DiSCO = DiSCO,
    TCAP = TCAP,
    TCAP_1 = TCAP_1,
    TCAP_0 = TCAP_0
  )
}

#### Exact Match ####

get_exact_match <- function(data_s, data_o, vars) {
  exact_match_p <- data_s %>%
    dplyr::left_join(
      data_o |>
        dplyr::select(all_of(vars)) |>
        dplyr::distinct() |>
        dplyr::mutate(.match = 1L),
      by = vars
    ) |>
    dplyr::mutate(flag = dplyr::coalesce(.match, 0L)) |>
    dplyr::select(-.match) |>
    dplyr::summarize(.by = flag, Frequency = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::mutate(
      Percent = 100.0 * Frequency / sum(Frequency),
      `Cumulative Frequency` = cumsum(Frequency),
      `Cumulative Percent` = cumsum(Percent)
    ) |>
    filter(flag == 1)

  return(exact_match_p$Percent)
}

#### Outliers ####

# threshold: #% of the cases
get_outlier_c <- function(data_s, data_o, keys, target, threshold = 5) {
  n_data_s <- data_s |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::pull(n) |>
    as.numeric()

  # Add the flag to synthetic data where the records on keys match the original
  outliers_data <- data_s %>%
    dplyr::semi_join(data_o |> dplyr::select(all_of(keys)), by = keys) |>
    dplyr::mutate(flag = 1L) |>
    dplyr::summarize(.by = all_of(target), n = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::mutate(
      percentage = n / sum(n) * 100.0,
      is_outlier = percentage < threshold,
      n_outlier = n * is_outlier
    )
  p_outliers <- sum(outliers_data$n_outlier) / n_data_s * 100.0

  list(
    #  outliers_data = outliers_data,
    p_outliers = p_outliers
  )
}

#### Generalized Targeted Correct Attribution Probability (GTCAP) ####

#' Univariate correct attribution probability
#'
#' @description
#' `get_univ_att_prob` computes the univariate correct attribution probability from given parameters, as described in https://arxiv.org/ftp/arxiv/papers/2310/2310.06571.pdf.
#'
#' @details
#' When the input dataframe contains only categorical variables, the value returned is the proportion of rows in the dataframe having the same combination of values
#' as the input row. When the input dataframe also contains numerical variables, this value is weighted according to the proximity between these variables in the
#' input row vs. each other row of the dataframe. The weights are defined by radii passed as parameters.
#'
#' @param row The target row of the dataframe.
#' @param data A dataframe.
#' @param rad A vector containing radii, one for each numerical variable of the dataframe. The order of radii must be the order of numerical variables in the dataframe.
#'
#' @return The computed probability.
get_univ_att_prob <- function(row, data, rad) {
  cat_vars <- sapply(row, function(col) !is.numeric(col))
  cat_vars <- names(row)[cat_vars]
  num_vars <- setdiff(names(row), cat_vars)
  correctness <- numeric(0)
  for (i in 1:nrow(data)) {
    row_2 <- data[i, , drop = FALSE]
    if (any(!mapply(identical, row[cat_vars], row_2[cat_vars]))) {
      correctness[i] <- 0
    } else {
      if (length(num_vars) == 0) {
        correctness[i] <- 1
      } else {
        num_row_1 <- row[num_vars]
        num_row_2 <- row_2[num_vars]

        na_1 <- is.na(num_row_1)
        na_2 <- is.na(num_row_2)
        num_row_1[na_1 & !na_2] <- num_row_2[na_1 & !na_2]
        num_row_2[!na_1 & na_2] <- num_row_1[!na_1 & na_2]
        num_row_1[na_1 & na_2] <- 0
        num_row_2[na_1 & na_2] <- 0

        result <- sum(
          pmax(0, -abs(num_row_1 - num_row_2) / rad + 1) / length(num_row_1)
        )
        correctness[i] <- result
      }
    }
  }
  return(mean(correctness))
}


#' Proximity coefficient between rows of a dataframe
#'
#' @description
#' `get_prox_coef` returns a proximity coefficient between two rows of a given dataframe, according to specified radii.
#'
#' @details
#' When the input dataframe contains only categorical variables, this returns either 0 or 1 depending on the value of these variables in the rows.
#' When the input dataframe also contains numerical variables, this value is weighted according to the proximity between these variables in the
#' input rows. The weights are defined by radii passed as parameters.
#'
#' @param row_1 A row of a dataframe.
#' @param row_2 Another row of the same dataframe.
#' @param rad A vector containing radii, one for each numerical variable of the dataframe. The order of radii must be the order of numerical variables in the dataframe.
#'
#' @return The computed probability.
get_prox_coef <- function(row_1, row_2, rad) {
  cat_vars <- sapply(row_1, function(col) !is.numeric(col))
  cat_vars <- names(row_1)[cat_vars]
  if (any(!mapply(identical, row_1[cat_vars], row_2[cat_vars]))) {
    return(0)
  }
  num_vars <- setdiff(names(row_1), cat_vars)
  if (length(num_vars) == 0) {
    return(1)
  }
  num_row_1 <- row_1[num_vars]
  num_row_2 <- row_2[num_vars]

  na_1 <- is.na(num_row_1)
  na_2 <- is.na(num_row_2)
  num_row_1[na_1 & !na_2] <- num_row_2[na_1 & !na_2]
  num_row_2[!na_1 & na_2] <- num_row_1[!na_1 & na_2]
  num_row_1[na_1 & na_2] <- 0
  num_row_2[na_1 & na_2] <- 0

  result <- sum(
    pmax(0, -abs(num_row_1 - num_row_2) / rad + 1) / length(num_row_1)
  )
  return(result)
}


#' Generalized Correct Attribution Probability (GCAP)
#'
#' @description
#' `get_GCAP` returns the GCAP for a given row of a dataframe, as described in https://arxiv.org/ftp/arxiv/papers/2310/2310.06571.pdf.
#'
#' @param row A row of a dataframe.
#' @param data The dataframe.
#' @param keys A vector containing names of variables of the dataframe to take as keys.
#' @param rad_keys A vector containing radii, one for each numerical key. The order of radii must correspond to the order of variables in the dataframe.
#' @param targets A vector containing names of variables of the dataframe to take as targets.
#' @param rad_targets A vector containing radii, one for each numerical target. The order of radii must correspond to the order of variables in the dataframe.
#'
#' @return The computed probability.
get_GCAP <- function(row, data, keys, rad_keys, targets, rad_targets) {
  cat_vars <- sapply(data, function(col) !is.numeric(col))
  cat_vars <- names(data)[cat_vars]
  cat_keys <- subset(keys, keys %in% cat_vars)

  n_corr <- sum(
    sapply(cat_keys, function(key) {
      return(data[[key]] == row[[key]])
    }),
    na.rm = TRUE
  )

  if (n_corr == 0) {
    return(0)
  }

  key_row <- row[keys]
  key_data <- data[, keys]
  coefs_keys <- sapply(1:nrow(key_data), function(i) {
    get_prox_coef(key_data[i, , drop = FALSE], key_row, unlist(rad_keys))
  })
  if (sum(coefs_keys) == 0) {
    return(0)
  }
  target_row <- row[targets]
  target_data <- data[, targets, drop = FALSE]

  coefs_targets <- sapply(1:nrow(target_data), function(i) {
    get_prox_coef(
      target_data[i, , drop = FALSE],
      target_row,
      unlist(rad_targets)
    )
  })

  return(sum(coefs_keys * coefs_targets) / sum(coefs_keys))
}


#' Statistically unique rows of a dataframe
#'
#' @description
#' `get_unique_rows` returns the unique rows of dataframe, according to specified keys and radii.
#'
#' @param data The dataframe.
#' @param keys A vector containing names of variables of the dataframe to take as keys.
#' @param rad_keys A vector containing radii, one for each numerical key. The order of radii must correspond to the order of variables in the dataframe.
#'
#' @return A dataframe composed of unique rows only.
get_unique_rows <- function(data, keys, rad_keys) {
  cat_vars <- sapply(data, function(col) !is.numeric(col))
  cat_vars <- names(data)[cat_vars]
  cat_keys <- subset(keys, keys %in% cat_vars)
  num_vars <- setdiff(names(data), cat_vars)
  num_keys <- subset(keys, keys %in% num_vars)

  unique_rows_cat <- data[
    !duplicated(data[, cat_keys]) &
      !duplicated(data[, cat_keys], fromLast = TRUE),
  ]
  dup_rows_cat <- data[
    !(duplicated(data[, cat_keys]) |
      duplicated(data[, cat_keys], fromLast = TRUE)),
  ]

  unique_rows_all <- data[FALSE, ]

  if (length(num_keys) > 0) {
    for (i in 1:nrow(dup_rows_cat)) {
      current_row <- dplyr::slice(dup_rows_cat, i)
      is_unique <- TRUE
      for (j in 1:nrow(dup_rows_cat)) {
        if (i == j) {
          next
        }
        comp_row <- dplyr::slice(dup_rows_cat, j)
        for (id_key in 1:length(num_keys)) {
          key <- num_keys[id_key]
          radius <- rad_keys[id_key]
          if (
            !is.na(current_row[[key]]) &&
              !is.na(comp_row[[key]]) &&
              abs(current_row[[key]] - comp_row[[key]]) < radius
          ) {
            is_unique <- FALSE
            break
          }
        }
        if (!is_unique) {
          break
        }
      }

      if (is_unique) {
        unique_rows_all <- dplyr::bind_rows(unique_rows_all, current_row)
      }
    }
  }
  unique_rows_all <- dplyr::bind_rows(unique_rows_all, unique_rows_cat)
  return(unique_rows_all)
}


#' Generalized Targeted Correct Attribution Probability (GTCAP)
#'
#' @description
#' `get_GTCAP` computes the GTCAP for a synthetic version of a dataframe, as described in https://arxiv.org/ftp/arxiv/papers/2310/2310.06571.pdf.
#'
#' @param orig The original dataframe.
#' @param synth The synthetic dataframe.
#' @param keys A vector containing names of variables of the dataframe to take as keys.
#' @param rad_keys A vector containing radii, one for each numerical key. The order of radii must correspond to the order of variables in the dataframe.
#' @param targets A vector containing names of variables of the dataframe to take as targets.
#' @param rad_targets A vector containing radii, one for each numerical target. The order of radii must correspond to the order of variables in the dataframe.
#' @param n_cores The number of logical processes to use for the computation. If set to NULL, the function will
#' attempt to determine the number of physical cores on the device and use this number, minus one.
#'
#' @return A list of values.
#'  \itemize{
#'    \item mean - The standardized mean GCAP for target synthetic rows.
#'    \item ind - A vector containing standardized GCAP values, one for each target synthetic row.
#'  }
get_GTCAP <- function(
  orig,
  synth,
  keys,
  rad_keys,
  targets,
  rad_targets,
  n_cores = NULL
) {
  if (is.null(n_cores)) {
    n_cores <- parallel::detectCores() - 1
  }

  unique_rows <- get_unique_rows(
    orig,
    c(keys, targets),
    c(rad_keys, rad_targets)
  ) #on considère les uniques à la fois selon les clés et les cibles

  process_rows <- function(id_list) {
    ind_baseline <- numeric(nrow(unique_rows))
    ind_CAP_orig <- numeric(nrow(unique_rows))
    ind_CAP_synth <- numeric(nrow(unique_rows))
    ind_GCAP <- numeric(nrow(unique_rows))

    for (i in id_list) {
      row <- unique_rows[i, , drop = FALSE]

      ind_baseline[i] <- get_univ_att_prob(
        row[, targets, drop = FALSE],
        orig[, targets, drop = FALSE],
        rad_targets
      )
      ind_CAP_synth[i] <- get_GCAP(
        row,
        synth,
        keys,
        unlist(rad_keys),
        targets,
        unlist(rad_targets)
      )
      ind_CAP_orig[i] <- get_GCAP(
        row,
        orig,
        keys,
        unlist(rad_keys),
        targets,
        unlist(rad_targets)
      )
      ind_GCAP[i] <- NaN

      if (ind_CAP_orig[i] - ind_baseline[i] != 0) {
        if (ind_baseline[i] > ind_CAP_synth[i]) {
          ind_GCAP[i] <- 0
        } else if (ind_baseline[i] > ind_CAP_orig[i]) {
          ind_GCAP[i] <- 0
        } else if (ind_CAP_synth[i] > ind_CAP_orig[i]) {
          ind_GCAP[i] <- 1
        } else {
          ind_GCAP[i] <- (ind_CAP_synth[i] - ind_baseline[i]) /
            (ind_CAP_orig[i] - ind_baseline[i])
        }
      }
    }
    return(list(
      baseline = ind_baseline,
      CAP_orig = ind_CAP_orig,
      CAP_synth = ind_CAP_synth,
      GCAP = ind_GCAP
    ))
  }

  cluster <- parallel::makeCluster(n_cores)
  parallel::clusterExport(
    cluster,
    list(
      "orig",
      "synth",
      "keys",
      "rad_keys",
      "targets",
      "rad_targets",
      "unique_rows"
    ),
    envir = environment()
  )
  split_id <- split(
    1:nrow(unique_rows),
    cut(1:nrow(unique_rows), n_cores, labels = FALSE)
  )
  lists <- parallel::parLapply(cluster, split_id, process_rows)
  parallel::stopCluster(cluster)

  ind_baseline <- unlist(lapply(lists, function(lst) lst$baseline))
  ind_CAP_orig <- unlist(lapply(lists, function(lst) lst$CAP_orig))
  ind_CAP_synth <- unlist(lapply(lists, function(lst) lst$CAP_synth))
  ind_GCAP <- unlist(lapply(lists, function(lst) lst$GCAP))

  mean_baseline <- mean(ind_baseline, na.rm = TRUE)
  mean_orig <- mean(ind_CAP_orig, na.rm = TRUE)
  mean_synth <- mean(ind_CAP_synth, na.rm = TRUE)

  res <- list(
    mean = (mean_synth - mean_baseline) / (mean_orig - mean_baseline),
    ind = ind_GCAP
  )
  return(res)
}
