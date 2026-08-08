# Utility_functions.R
#
# Robyn Ferg
# 08/22/223
#
# This script includes code for utility functions to determine how similar
# two data sets (original and synthetic/perturbed) are to each other.
# Originally created for 2015-2019 longitudinal SDR, but code written to be 
# generalizable to any dataset.
# Descriptions of each utility measure can be found here:
# "\\westat.com\dfs\SDR_NCSES\7. Data Delivery and Dissemination\StatConfid\Utility\Utility Measures.docx"


# The original and treated data should be contained within
# a single dataset. Call this 'data'.
#
# Data should be set up in the following ways:
# - For variables that had some sort of treatment, have the original column
#   included with the suffix '_O'
# - To account for implicates, can specify a specific suffix (e.g., '_1')
# - NA values are all treated as NA, no other value
# - Have an associated survey object
#
#
# Utility measure are divided into three groups:
# - QC Checks
# - Weighted Frequency Checks
# - Measure of Variance Checks
# - Measure of Association Checks


# Packages
library(tidyverse)
library(ggplot2)
library(survey)
library(stringr)
library(psych)
library(wCorr)
library(openxlsx)
library(SDCNway)
library(caret)


################################################################################
######################## QC Checks #############################################
################################################################################

# Percent of records changed
pct_records_changed_var = function(data, var, new_suff, og_suff){
  # percent of records changed for an individual variable 'var'
  num_changed = sum(data[[paste0(var, new_suff)]] != data[[paste0(var, og_suff)]], na.rm=TRUE) + # numeric non-matches
    sum(!is.na(data[[paste0(var, new_suff)]]) & is.na(data[[paste0(var, og_suff)]])) + # non-missing new & missing old
    sum(is.na(data[[paste0(var, new_suff)]]) & !is.na(data[[paste0(var, og_suff)]])) # missing new & non-missing old
  pct_changed = num_changed/nrow(data)
  return(pct_changed)
}

pct_records_changed = function(data, changed_vars, og_suff, new_suff, col_suff){
  # Percent of records with changed values for each variable
  pct_changes = sapply(changed_vars, 
                       function(x) pct_records_changed_var(data, x, new_suff, og_suff))
  out = data.frame('variable' = changed_vars)
  out[[paste0('pct_changes', col_suff)]] = 100*pct_changes
  rownames(out) = 1:nrow(out)
  return(out)
}

pct_records_changed_imp = function(data, changed_vars, og_suff, 
                                   imps=paste0('_',1:5), sum_stats=TRUE,
                                   print=FALSE, out_df=NA, out_dir=NA){
  # Calculates percent of records changed for each variable across all implicates
  # and calculates summary statistics (if sum_stats=TRUE)
  ## get pct of observations that changed for each variable & implicate
  changes = lapply(imps, 
                   function(x) pct_records_changed(data, changed_vars, 
                                                   new_suff=x, og_suff=og_suff, col_suff=x)) %>%
    reduce(left_join, by='variable')
  ## add summary statistics
  if(sum_stats){
   changes$mean_pct_changes = rowMeans(changes[,-which(names(changes)=='variable')])
   changes$sd_pct_changes = apply(changes[,-which(names(changes)=='variable')],
                                  1, sd)
  }
  ## print
  if(print) print(changes)
  ## save
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(changes, paste0(out_df, '.xlsx'))
  }
  return(changes)
}



# Change flag crosstabs
change_flg_single_var = function(data, var, new_suff, og_suff){
  # Create crosstab for whether or not a single variable was changed and tier
  data$change_flag = ifelse(!is.na(data[[paste0(var, new_suff)]]) & is.na(data[[paste0(var, og_suff)]]) |
                              is.na(data[[paste0(var, new_suff)]]) & !is.na(data[[paste0(var, og_suff)]]) |
                              data[[paste0(var, new_suff)]] != data[[paste0(var, og_suff)]],
                            1, 0)
  out_crosstabs = data %>% count(tier_for_crosstabs, change_flag)
  names(out_crosstabs)[1] = 'tier'
  return(out_crosstabs)
}

change_flg_crosstabs = function(data, changed_vars, tier_var, og_suff, new_suffs,
                                print=TRUE, out_df=NA, out_dir=NA){
  # Create crosstabs of whether or not a variable was changed and 
  # tier (low/high level of swapping, etc.)
  data$tier_for_crosstabs = data[[tier_var]]
  
  # iterate through each implicate
  all_out = list()
  for(i in new_suffs){
    out = lapply(changed_vars, function(x) change_flg_single_var(data, x, new_suff, og_suff))
    names(out) = changed_vars
    # print
    if(print){ print(new_suff); print(out) }
    # save output
    if(!is.na(out_df)){
      if(!is.na(out_dir)) setwd(out_dir)
      write.xlsx(out, paste0(out_df, '.xlsx'))
    }
    all_out[[length(all_out)+1]] = out
  }
  names(all_out) = new_suffs
  return(out)
}



# Skip patterns
new_patterns = function(d_old, d_new, vars, new_suff){
  # creates crosstabs of 'variables' from 'd_old' and 'd_new' and returns
  # combinations that are in d_new but not in d_old.
  
  vars_new = case_when(paste0(vars, new_suff) %in% names(d_new) ~ paste0(vars, new_suff),
                       TRUE ~ vars)
  
  # crosstabs in old data
  old_ctabs = d_old %>% count(!!!syms(vars))
  
  # crosstabs in new data
  new_ctabs = d_new %>% count(!!!syms(vars_new))
  names(new_ctabs) = names(old_ctabs)
  
  # merge crosstabs
  all_ctabs = merge(old_ctabs, new_ctabs, by=vars, all=TRUE)
  
  # restrict to crosstabs only in new data
  new_combos = all_ctabs[is.na(all_ctabs$n.x),]
  
  # output
  if(nrow(new_combos)>0) return(new_combos)
  else return(NA)
}

missing_skip_patterns = function(d_old, d_new, vars_list, 
                                 new_suffs=paste0('_', 1:5),
                                 print=FALSE, out_dir, out_df){
  # return combinations that are in d_new and not in d_old for several
  # combinations of variables in a list
  
  out = list()
  new_pattern_names = c()
  for(i in new_suffs){
    for(vars in vars_list){
      new_patterns_i = new_patterns(d_old, d_new, vars, new_suff=i)
      if(class(new_patterns_i)=='data.frame'){
        out[[length(out)+1]] = new_patterns_i
        new_pattern_names = c(new_pattern_names,
                              paste0(i, "--", paste(vars, collapse=" ")))
      }
    }    
  }
  new_pattern_names = case_when(nchar(new_pattern_names)>15 ~ substr(new_pattern_names, 1, 15),
                                TRUE ~ new_pattern_names)
  names(out) = new_pattern_names
  
  # print results
  if(print) print(out)
  # save output
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(out, paste0(out_df, '.xlsx'))
  }
  return(out)
}

# Old version of finding new skip patterns:
# diff_patterns = function(c1, c2){
#   # find patterns in crosstabs c1 but not c2: c1=new; c2=original
#   c1 = c1[,1:(ncol(c1)-1)]
#   c2 = c2[,1:(ncol(c2)-1)]
#   names(c2) = names(c1)
#   names(c1)[names(c1)=='.unweighted_freq'] = '.unweighted_freq1'
#   names(c2)[names(c2)=='.unweighted_freq'] = '.unweighted_freq2'
#   c1$in_new = 1
#   c2$in_og = 1
#   all_c = merge(c1, c2, all.x=TRUE)
#   new_combos = all_c[is.na(all_c$in_og),]
#   new_combos = new_combos[,1:(ncol(new_combos)-3)]
#   return(new_combos)
# }
#
# missing_skip_patterns = function(data_new, data_og, miss_vars, cat_vars,
#                                  new_suff, og_suff='', k=4, print=TRUE,
#                                  out_df=NA, out_dir=NA){
#   # Create k-way crosstabs of missingness for original and new values--
#   # output combination in new but not original
#   # miss_vars = variables to convert to missing/non-missing
#   # cat_vars = categorial variables
#   
#   # create indicator variables for missingness of changed variables
#   if(!is.na(miss_vars)){
#     for(var in miss_vars){
#       data_og[[paste0(var, '_O_MISS')]] = ifelse(is.na(data_og[[paste0(var, og_suff)]]), 1, 0)
#       data_new[[paste0(var, '_MISS')]] = ifelse(is.na(data_new[[paste0(var, new_suff)]]), 1, 0)
#     }
#   }
#   
#   # varpool 
#   varpool_og = c()
#   varpool_new = c()
#   if(sum(is.na(miss_vars))==0){
#     varpool_og = c(varpool_og, paste0(miss_vars, '_O_MISS'))
#     varpool_new = c(varpool_new, paste0(miss_vars, '_MISS'))
#   }
#   if(sum(is.na(cat_vars))==0){
#     varpool_og = c(varpool_og, paste0(cat_vars, og_suff))
#     varpool_new = c(varpool_new, paste0(cat_vars, new_suff))
#   }
#   
#   # create k-way crosstabs using SDCNway
#   k = min(length(varpool_og), k)
#   orig_crosstabs = sdc_extabs(data_og, 
#                               varpool = varpool_og,
#                               mindim = k, maxdim = k)
#   new_crosstabs = sdc_extabs(data_new,
#                              varpool = varpool_new,
#                              mindim = k, maxdim = k)
#   
#   # find combinations in new crosstabs but not in old crosstabs
#   out = lapply(1:length(orig_crosstabs$tabulation),
#                   function(x) diff_patterns(orig_crosstabs$tabulation[[x]],
#                                             new_crosstabs$tabulation[[x]]))
#   # keep only data frames with new combinations
#   out = out[[which(sapply(1:length(out), function(x) nrow(out[[x]]))>0)]]
#   
#   # print results
#   if(print) print(out)
#   # save output
#   if(!is.na(out_df)){
#     if(!is.na(out_dir)) setwd(out_dir)
#     write.xlsx(out, paste0(out_df, '.xlsx'))
#   }
#   
#   return(out)
#         
# }



################################################################################
######################## Weighted Frequency Checks #############################
################################################################################

# Distance between variables
## Continuous
summ_stats_var = function(data, var, new_suff, og_suff){
  # output summary statistics for distance between original and updated 
  # var in data
  var_orig = data[[paste0(var, og_suff)]]
  var_new = data[[paste0(var, new_suff)]]
  dists = abs(var_orig - var_new)
  return(c(summary(dists)))
}
wgt_summ_stats_var = function(svy, var, new_suff, og_suff){
  # output weighted summary statistics for distance between original and
  # updated var in data
  svy$variables[['var_diff']] = svy$variables[[paste0(var, og_suff)]] -
    svy$variables[[paste0(var, new_suff)]]
  quantiles = svyquantile(~abs(var_diff), svy, c(.25, .5, .75), na.rm=TRUE)
  mean = svymean(~abs(var_diff), svy, na.rm=TRUE)
  wgt_summary = c(quantiles[[1]][1,1], quantiles[[1]][2,1], mean[1], quantiles[[1]][3,1])
  names(wgt_summary) = c('wgt_Q1', 'wgt_median', 'wgt_mean', 'wgt_Q3')
  return(wgt_summary)
}

dist_btwn_cont_vars = function(data, changed_vars_cont, new_svy, 
                               new_suffs=paste0('_', 1:5), og_suff='',
                               print=TRUE, out_df=NA, out_dir=NA){
  # summary statistics for distance between original and updated version of 
  # continuous variables
  # Note that updated weights are used
  
  # Calculate distances for each implicate
  out_imps = data.frame()
  for(new_suff in new_suffs){
    # unweighted 
    dist_summ = sapply(changed_vars_cont, function(x) summ_stats_var(data, x, new_suff, og_suff)) %>%
      bind_rows() %>% data.frame()
    # weighted
    wgt_dist_summ = sapply(changed_vars_cont, function(x) wgt_summ_stats_var(new_svy, x, new_suff, og_suff)) %>% t
    
    # merge weighted and unweighted
    all_dist_summ = data.frame('Implicate'=as.numeric(substr(new_suff, 2, 2)),
                               'Variable'=changed_vars_cont, 
                               cbind(dist_summ, wgt_dist_summ))
    out = rbind(out, all_dist_summ)
  }
  
  # Calculate average distance across implicates
  out_summary = sapply(unique(out$Variable),
                       function(x) colMeans(out[out$Variable==x,-c(1:2)])) %>% 
    t %>% data.frame()
  out_summary = cbind('Variable' = unique(out$Variable),
                      out_summary)
  
  out = list('all_implicates'=out, 'summary'=out_summary)
  
  # print results
  if(print) print(out)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(out, paste0(out_df, '.xlsx'))
  }
  
  return(out)
}


## Categorical
HD = function(P, Q){
  # Calculate Hellinger's distance between two categorical distributions P and Q
  P = P/sum(P)
  Q = Q/sum(Q)
  return( (1/sqrt(2)) * sqrt(sum((sqrt(P) - sqrt(Q))^2)) )
}

wgt_hellinger_dist = function(svy_new, svy_og, var, new_suff, og_suff){
  # Calculate Hellinger distance between original and updated var

  # Weighted sums for each level
  ## Original
  wgt_sum_orig = svytable(as.formula(paste0('~', var, og_suff)), svy_og)
  ## Updated
  wgt_sum_new = svytable(as.formula(paste0('~', var, new_suff)), svy_new)
  
  # Merge
  both_sums = merge(data.frame(wgt_sum_orig), data.frame(wgt_sum_new),
                    by.x=paste0(var, og_suff), by.y=paste0(var, new_suff),
                    all=TRUE)
  both_sums[is.na(both_sums)] = 0
  
  # Hellinger's distance
  hellinger_dist = HD(both_sums$Freq.x, both_sums$Freq.y)

  return(hellinger_dist)
}

dist_btwn_cat_vars = function(svy_new, svy_og, 
                              new_suffs=paste0('_', 1:5), og_suff='', 
                              changed_vars_cat, print=TRUE,
                              out_df=NA, out_dir=NA){
  # Hellinger distance between original and updated categorical variables
  
  # distance for all implicates
  out = data.frame('Variable'=changed_vars_cat)
  for(new_suff in new_suffs){
    hellinger_dists = sapply(changed_vars_cat, function(x) wgt_hellinger_dist(svy_new, svy_og, x, new_suff, og_suff))
    temp = data.frame('Variable'=changed_vars_cat)
    temp[[paste0('HellingerDistance', new_suff)]] = hellinger_dists
    out = merge(out, temp, all=TRUE)
  }
  
  # Summary of distances across implicates
  out$Mean_HellingerDist = rowMeans(out[,paste0('HellingerDistance', new_suffs)])
  out$SD_HellingerDist = apply(out[,paste0('HellingerDistance', new_suffs)],
                               1, sd)
  
  # print results
  if(print) print(out)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(out, paste0(out_df, '.xlsx'))
  }
}





# Confidence Interval Overlap
IO = function(l1, u1, l2, u2){
  # interval overlap between 2 confidence intervals: (l1, u1) and (l2, u2)
  numerator = min(u1, u2) - max(l1, l2)
  io = 0.5 * (numerator/(u1-l1) + numerator/(u2-l2))
  return(io)
}

simple_ci_overlap_fcn = function(var, stat, svy_new, svy_og, new_suff, og_suff='', cat=NA){
  # calculates confidence interval overlap for simple statistic of var
  # var = variable name
  # stat = 'mean', 'prop', or 'sum'
  # svy_new = survey object of new data
  # svy_og = survey object of original data
  # new_suff = implicate suffix of treated variable
  # og_suff = suffix of original variable
  # cat = which category is of interest if calculating proportion
  
  # Get confidence intervals
  if(stat=='mean'){ # if calculating mean
    new_est = svymean(as.formula(paste0('~', var, new_suff)), svy_new, na.rm=TRUE)
    old_est = svymean(as.formula(paste0('~', var, og_suff)), svy_og, na.rm=TRUE)
  }
  if(stat=='prop'){ # if calculating proportion
    new_est = svyciprop(as.formula(paste0('~', var, new_suff, '==', cat)), svy_new, na.rm=TRUE)
    old_est = svyciprop(as.formula(paste0('~', var, og_suff, '==', cat)), svy_og, na.rm=TRUE)
  }
  if(stat=='sum'){ # if calculating sum
    new_est = svytotal(as.formula(paste0('~', var, new_suff)), svy_new, na.rm=TRUE)
    old_est = svytotal(as.formula(paste0('~', var, og_suff)), svy_og, na.rm=TRUE)
  }
  
  # get confidence intervals
  new_confint = confint(new_est)
  old_confint = confint(old_est)
  
  # test for whether new estimate is within the original CI
  new_est_in_old_ci = ifelse(new_est[1] >= old_confint[1] &
                               new_est[1] <= old_confint[2],
                             1, 0)
  
  # CI overlap
  ci_io = IO(new_confint[1], new_confint[2], old_confint[1], old_confint[2])

  return(list('new_est' = new_est[1],
              'new_lwr'=new_confint[1],
              'new_upr'=new_confint[2],
              'old_est' = old_est[1],
              'old_lwr'=old_confint[1],
              'old_upr'=old_confint[2],
              'io' = ci_io,
              'new_est_in_old_ci' = new_est_in_old_ci))
}

simple_ci_overlap = function(confint_vars, confint_stats, prop_cats=NA, 
                             svy_new, svy_og, new_suff, og_suff,
                             print=TRUE, out_df=NA, out_dir=NA){
  # calculates confidence interval overlap of simple statistics
  # and whether estimate for updated data is within the original CI
  # confint_vars = vector of variables
  # confint_stats = vector of 'mean', 'prop', or 'sum'
  # prop_cat = level of variable to calculate proportion
  # svy_new = survey design object of treated data
  # svy_og = survey design object of original data
  # new_suff = implicate suffix of treated variable
  # og_suff = suffix of original variable
  # print = whether to print results
  # out_df = name to call saved Excel file (NA if not saving)
  # out_dir = directory to save output data frame
  
  # create data frame will all inputs
  out = data.frame('var' = confint_vars,
                   'stat' = confint_stats,
                   'prop_cat' = prop_cats)
  # calculate confidence intervals and interval overlap
  cis_overlaps = apply(out, 1, function(x) simple_ci_overlap_fcn(x[1], x[2], svy_new, svy_og, new_suff, og_suff, x[3])) %>% 
    do.call(cbind, .) %>% # convert from list of lists to data frame
    data.frame() %>%
    t()
  out = cbind(out, cis_overlaps)
  
  # print results
  if(print) print(out)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(out, paste0(out_df, '.xlsx'))
  }
  
  return(out)
}


subset_ci_overlap = function(confint_vars, confint_stats, prop_cats=NA, 
                             svy_new, svy_og, new_suff, og_suff,
                             subset_vars,
                             print=TRUE, out_df=NA, out_dir=NA){
  # calculates confidence interval overlap of simple statistics
  # and whether estimate for updated data is within the original CI
  # within a specified subsets
  # confint_vars = vector of variables
  # confint_stats = vector of 'mean', 'prop', or 'sum'
  # prop_cat = level of variable to calculate proportion
  # svy_new = survey design object of treated data
  # svy_og = survey design object of original data
  # new_suff = implicate suffix of treated variable
  # og_suff = suffix of original variable
  # print = whether to print results
  # out_df = name to call saved Excel file (NA if not saving)
  # out_dir = directory to save output data frame
  
  # output data frame 
  out = data.frame()
  
  for(subset_var in subset_vars){
    
    # subset survey object to specified subgroup
    svy_new = subset(svy_new, get(paste0(subset_var, new_suff))==1)
    svy_og = subset(svy_og, get(paste0(subset_var, og_suff))==1)
  
    # create data frame with all inputs
    subset_out = data.frame('subset'=subset_var, 
                     'var' = confint_vars,
                     'stat' = confint_stats,
                     'prop_cat' = prop_cats)
    # calculate confidence intervals and interval overlap
    cis_overlaps = apply(subset_out, 1, function(x) simple_ci_overlap_fcn(x[2], x[3], svy_new, svy_og, new_suff, og_suff, x[3])) %>% 
      do.call(cbind, .) %>% # convert from list of lists to data frame
      data.frame() %>%
      t()
    subset_out = cbind(subset_out, cis_overlaps)
    out = rbind(out, subset_out)
  
  }
  
  # print results
  if(print) print(out)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(out, paste0(out_df, '.xlsx'))
  }
  
  return(out)
}




# High Utility Crosstabs
high_util_crosstabs = function(data_new, data_og=NA, new_suff, og_suff='', 
                               k=NA, cat_vars, weight_var,
                               print=TRUE, plot=TRUE,
                               plot_name=NA, out_df=NA, out_dir=NA){
  # create weighted crosstabs of high-utility variables and compare
  # original counts vs. new counts
  
  if(is.na(k)) k = length(cat_vars)
  
  # original data
  og_crosstabs = sdc_extabs(data=data_og,
                            weight=weight_var,
                            varpool=paste0(cat_vars, og_suff),
                            mindim=2,
                            maxdim=3)
  og_all_cross = bind_rows(og_crosstabs$tabulation)
  if(og_suff!=''){
    names(og_all_cross) = ifelse(grepl(suff_og, names(og_all_cross)),
                                 substr(names(og_all_cross), 1, nchar(names(og_all_cross))-nchar(og_suff)),
                                 names(og_all_cross))
  }
  # new data
  new_crosstabs = sdc_extabs(data=data_new,
                             weight=weight_var,
                             varpool=paste0(cat_vars, new_suff),
                             mindim=1,
                             maxdim=k)
  new_all_cross = bind_rows(new_crosstabs$tabulation)
  names(new_all_cross) = ifelse(grepl(new_suff, names(new_all_cross)),
                                substr(names(new_all_cross), 1, nchar(names(new_all_cross))-nchar(new_suff)),
                                names(new_all_cross))
  # merge
  all_crosstabs = merge(og_all_cross, new_all_cross,
                        all=TRUE, by=cat_vars)
  
  # rename
  names(all_crosstabs)[names(all_crosstabs)=='.weighted_freq.x'] = 'orig_wgt_count'
  names(all_crosstabs)[names(all_crosstabs)=='.unweighted_freq.x'] = 'orig_unwgt_count'
  names(all_crosstabs)[names(all_crosstabs)=='.weighted_freq.y'] = 'new_wgt_count'
  names(all_crosstabs)[names(all_crosstabs)=='.unweighted_freq.y'] = 'new_unwgt_count'
  all_crosstabs$.violation.x = NULL
  all_crosstabs$.violation.y = NULL
  
  # plot
  if(plot){
    scatterplot = ggplot(all_crosstabs, aes(orig_wgt_count, new_wgt_count)) +
      geom_point() +
      geom_abline(intercept=0, slope=1, col='grey') +
      geom_point() + 
      theme_bw() +
      xlab('Old Weighted Counts') + ylab('New Weighted Counts') +
      ggtitle('Effect of Data Treatment on High Utility Crosstab Counts')
    print(scatterplot)
    if(!is.na(plot_name)){
      if(!is.na(out_dir)) setwd(out_dir)
      ggsave(paste0(plot_name, '.pdf'), scatterplot, width=8, height=6)
    }
  }
  
  # print results
  if(print) print(all_crosstabs)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(all_crosstabs, paste0(out_df, '.xlsx'))
  }
  
  return(all_crosstabs)
  
}



# High-Utility Crosstabs, but forcing variables into the crosstabs
high_util_crosstabs_force = function(data_new, data_og=NA, new_suff, og_suff='', 
                                     k=NA, cat_vars, weight_var,
                                     force_vars,
                                     print=TRUE, plot=TRUE,
                                     plot_name=NA, out_df=NA, out_dir=NA){
  # create weighted crosstabs of high-utility variables and compare
  # original counts vs. new counts
  
  if(is.na(k)) k = length(cat_vars)
  
  # original data
  og_crosstabs = sdc_extabs(data=data_og,
                            weight=weight_var,
                            varpool=paste0(c(cat_vars, force_vars), og_suff),
                            forcelist = paste0(force_vars, og_suff),
                            forcenum = 1,
                            mindim=2,
                            maxdim=3,
                            include_mu_argus=FALSE)
  og_all_cross = bind_rows(og_crosstabs$tabulation)
  if(og_suff!=''){
    names(og_all_cross) = ifelse(grepl(suff_og, names(og_all_cross)),
                                 substr(names(og_all_cross), 1, nchar(names(og_all_cross))-nchar(og_suff)),
                                 names(og_all_cross))
  }
  # new data
  for(var in c(cat_vars, force_vars)){
    if(!paste0(var, new_suff) %in% names(data_new)){
      print(paste('Adding', paste0(var, new_suff), 'to data'))
      data_new[[paste0(var, new_suff)]] = data_new[[var]]
    }
  }
  new_crosstabs = sdc_extabs(data=data_new,
                             weight=weight_var,
                             varpool=paste0(c(cat_vars, force_vars), new_suff),
                             forcelist = paste0(force_vars, new_suff),
                             forcenum = 1,
                             mindim=2,
                             maxdim=3,
                             include_mu_argus=FALSE)
  new_all_cross = bind_rows(new_crosstabs$tabulation)
  names(new_all_cross) = ifelse(grepl(new_suff, names(new_all_cross)),
                                substr(names(new_all_cross), 1, nchar(names(new_all_cross))-nchar(new_suff)),
                                names(new_all_cross))
  # merge
  all_crosstabs = merge(og_all_cross, new_all_cross,
                        all=TRUE, by=c(cat_vars, force_vars))
  
  # rename
  names(all_crosstabs)[names(all_crosstabs)=='.weighted_freq.x'] = 'orig_wgt_count'
  names(all_crosstabs)[names(all_crosstabs)=='.unweighted_freq.x'] = 'orig_unwgt_count'
  names(all_crosstabs)[names(all_crosstabs)=='.weighted_freq.y'] = 'new_wgt_count'
  names(all_crosstabs)[names(all_crosstabs)=='.unweighted_freq.y'] = 'new_unwgt_count'
  all_crosstabs$.violation.x = NULL
  all_crosstabs$.violation.y = NULL
  
  # plot
  if(plot){
    scatterplot = ggplot(all_crosstabs, aes(orig_wgt_count, new_wgt_count)) +
      geom_point() +
      geom_abline(intercept=0, slope=1, col='grey') +
      geom_point() + 
      theme_bw() +
      xlab('Old Weighted Counts') + ylab('New Weighted Counts') +
      ggtitle('Effect of Data Treatment on High Utility Crosstab Counts')
    print(scatterplot)
    if(!is.na(plot_name)){
      if(!is.na(out_dir)) setwd(out_dir)
      ggsave(paste0(plot_name, '.pdf'), scatterplot, width=8, height=6)
    }
  }
  
  # print results
  if(print) print(all_crosstabs)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(all_crosstabs, paste0(out_df, '.xlsx'))
  }
  
  return(all_crosstabs)
  
}




# k-Marginal Score
total_variation_norm = function(x, y){
  # total variation norm between two distributions x and y
  
  # normalize
  x_norm = x/sum(x)
  y_norm = y/sum(y)
  
  # calculate norm
  tvn = .5 * sum(abs(x_norm-y_norm))
  
  return(tvn)
}
kway_crosstabs = function(data, vars, suff, weight_var){
  # calculate k-way distributions (k = # vars)
  nvars = length(vars)
  crosstabs = sdc_extabs(data=data, 
                         weight=weight_var, 
                         varpool=paste0(vars, suff), 
                         mindim=nvars, 
                         maxdim=nvars)
  return(crosstabs$tabulation$V1)
}

k_marginal_score = function(data_new, data_og, cat_vars, weight_var, 
                            new_suff, og_suff='', k=3, n=25, 
                            print=TRUE, out_df=NA, out_dir=NA){
  # Calculate k-marginal score:
  # Out of the list of cat_vars, randomly select k and calculate crossstabs
  # for original and updated variables. Calculate total variation norm between
  # the two crosstab distributions. Repeat min(n, len(cat_vars)-choose-k) times
  # and output distribution of total variation norms.
  
  # number of crosstabs to create
  d = length(cat_vars)
  possible_combos = choose(d, k)
  n_combos = min(n, possible_combos)
  
  # k-way marginal distributions
  if(n_combos == possible_combos){ # iterate through all possible combinations
    # original data
    og_crosstabs = sdc_extabs(data=data_og,
                              weight=weight_var,
                              varpool=paste0(cat_vars, og_suff),
                              mindim=k,
                              maxdim=k)
    og_all_cross = og_crosstabs$tabulation
    # new data
    new_crosstabs = sdc_extabs(data=data_new,
                               weight=weight_var,
                               varpool=paste0(cat_vars, new_suff),
                               mindim=k,
                               maxdim=k)
    new_all_cross = new_crosstabs$tabulation
  }
  else{# sample n possible combinations
    # all possible combinations
    all_combos = combn(cat_vars, k)
    rand_combos = all_combos[,sample(1:ncol(all_combos), n, replace=FALSE)]
    og_all_cross = lapply(1:n, function(x) kway_crosstabs(data_og, rand_combos[,x], og_suff, weight_var=weight_var))
    new_all_cross = lapply(1:n, function(x) kway_crosstabs(data_new, rand_combos[,x], new_suff, weight_var=weight_var))
  }
  
  # calculate total variation norms for each k-way crosstabs
  tvns = sapply(1:n, function(x) total_variation_norm(og_all_cross[[x]]$.weighted_freq,
                                                      new_all_cross[[x]]$.weighted_freq))
  
  # summary statistics of total variation norms
  summ_stats = summary(tvns)
  
  # print results
  if(print) print(tvns); print(summ_stats)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(summ_stats, paste0(out_df, '.xlsx'))
  }
  
  return(list('tvns'=tvns, 'summary_stats'=summ_stats))
  
}



## Higher Order Conjunction
HOC = function(og_data, new_data, new_suffs=paste0('_', 1:5), og_suff='',
               hoc_vars, ki, niter=100, print=TRUE,
               out_df=NA, out_dir=NA){
  # Calculated higher order conjunction--a randomized procedure--on data
  # og_data = original data frame
  # new_data = new data frame
  # new_suff = suffix for new data frame implicate
  # hoc_vars = vector of variables in which similarity is calculated form
  # ki = range for each variable with order corresponding to hoc_vars
  # niter = number of iterations
  
  # vector of resulting similarities
  sim_out = data.frame()
  
  # sample niter individuals
  set.seed(1234)
  samp = sample(1:nrow(og_data), niter, replace = FALSE)
  
  # iterate through sampled individuals
  for(ind in samp){
    # iterate through all implicates
    for(new_suff in new_suffs){
      hoc_og_data = og_data
      hoc_new_data = new_data
      
      # narrow down subset to individuals who had the exact same categorical responses
      # as sampled individual
      cat_vars = hoc_vars[ki==0]
      for(var in cat_vars){
        # value of sampled respondent
        samp_val = og_data[[paste0(var, og_suff)]][ind] 
        # remove non-similar rows
        if(nrow(hoc_og_data)>0) hoc_og_data = hoc_og_data[hoc_og_data[[paste0(var, og_suff)]]==samp_val,]
        if(nrow(hoc_new_data)>0) hoc_new_data = hoc_new_data[hoc_new_data[[paste0(var, new_suff)]]==samp_val,]
      }
      
      # narrow down subset to individuals whose continuous variables are with +/- ki
      cont_vars = hoc_vars[ki!=0]
      for(var in cont_vars){
        # value of sampled respondent
        samp_val = og_data[[paste0(var, og_suff)]][ind]
        # index of variable for ki
        ki_var = ki[which(hoc_vars==var)]
        # remove non-similar rows
        if(nrow(hoc_og_data)>0){
          hoc_og_data = hoc_og_data[hoc_og_data[[paste0(var, og_suff)]] <= samp_val+ki_var &
                                      hoc_og_data[[paste0(var, og_suff)]] >= samp_val-ki_var,]
        }
        if(nrow(hoc_new_data)>0){
          hoc_new_data = hoc_new_data[hoc_new_data[[paste0(var, og_suff)]] <= samp_val+ki_var &
                                      hoc_new_data[[paste0(var, og_suff)]] >= samp_val-ki_var,]
        }
      }
      
      # similarity for og and new data --> pct of rows that are similar
      similarity_og = nrow(hoc_og_data)/nrow(og_data)
      similarity_new = nrow(hoc_new_data)/nrow(new_data)
      
      sim_out = rbind(sim_out, 
                      data.frame('sim_og'=similarity_og,
                                 'sim_new'=similarity_new))
    }
    
    # overall similarity score as the mean absolute difference
    sim_out$sim_score = abs(sim_out$sim_og - sim_out$sim_new)
    out = list('all_similarities'=sim_out, 'similarity_summary'=summary(sim_out$sim_score))
  }
  
  # print results
  if(print) print(out$similarity_summary)
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(out, paste0(out_df, '.xlsx'))
  }
  
}




################################################################################
######################## Measure of Association Checks #########################
################################################################################

# Pairwise Associations
change_in_corrs = function(data_new, data_og, new_suff, og_suff='', pair_assoc_vars, method='Pearson', 
                           weight_var, plot=TRUE, plot_name=NA, out_dir=NA, print=TRUE,
                           out_df=NA){
  # Change in pairwise and global correlations between new and original data
  # method = 'Pearson' or 'Spearman'
  
  # create matrix of old and new correlations
  old_pa_corrs = matrix(NA, nrow=length(pair_assoc_vars), ncol=length(pair_assoc_vars))
  new_pa_corrs = old_pa_corrs
  for(i in 1:(length(pair_assoc_vars)-1)){
    vari = pair_assoc_vars[i]
    for(j in (i+1):length(pair_assoc_vars)){
      varj = pair_assoc_vars[j]
      pairwise_comp_sub_old = data_og[!is.na(data_og[[paste0(vari, og_suff)]]) &
                                     !is.na(data_og[[paste0(varj, og_suff)]]),]
      pairwise_comp_sub_new = data_new[!is.na(data_new[[paste0(vari, new_suff)]]) &
                                     !is.na(data_new[[paste0(varj, new_suff)]]),]
      old_entry = weightedCorr(x = pairwise_comp_sub_old[[paste0(vari, og_suff)]], 
                               y = pairwise_comp_sub_old[[paste0(varj, og_suff)]],
                               method = method,
                               weights = pairwise_comp_sub_old[[weight_var]])
      new_entry = weightedCorr(x = pairwise_comp_sub_new[[paste0(vari, new_suff)]], 
                               y = pairwise_comp_sub_new[[paste0(varj, new_suff)]],
                               method = method,
                               weights = pairwise_comp_sub_new[[weight_var]])
      old_pa_corrs[i,j] = old_entry
      old_pa_corrs[j,i] = old_entry
      new_pa_corrs[i,j] = new_entry
      new_pa_corrs[j,i] = new_entry
    }
  }
  
  # relative and absolute difference in correlations
  abs_diff_pa_corrs = abs(old_pa_corrs - new_pa_corrs)
  rel_diff_pa_corrs = abs_diff_pa_corrs/old_pa_corrs
  
  # make plot of original and new correlations
  old_pa_corrs_list = old_pa_corrs[upper.tri(old_pa_corrs)] %>% c()
  new_pa_corrs_list = new_pa_corrs[upper.tri(new_pa_corrs)] %>% c()
  
  if(plot){
    pa_plot = ggplot(data.frame('old_corrs'=old_pa_corrs_list, 'new_corrs'=new_pa_corrs_list),
           aes(x=old_corrs, y=new_corrs)) +
      geom_abline(intercept=0, slope=1, col='grey') +
      geom_point() + 
      theme_bw() +
      xlab('Old Correlation') + ylab('New Correlation') +
      ggtitle(paste('Effect of Data Treatment on Pairwise', str_to_title(method), 'Correlations'))
    print(pa_plot)
    if(!is.na(plot_name)){
      if(!is.na(out_dir)) setwd(out_dir)
      ggsave(paste0(plot_name, '.pdf'), pa_plot, width=8, height=6)
    }
  }
  
  # Global utility of pairwise correlations
  npairs_orig = pairwiseCount(data_og[,paste0(pair_assoc_vars, og_suff)])
  SE_pair_orig = (1-old_pa_corrs)^2/sqrt(npairs_orig)
  SE_pair_orig_list = SE_pair_orig[upper.tri(SE_pair_orig)] %>% c()
  
  R_ased = 1/length(SE_pair_orig_list) * sum(abs(old_pa_corrs_list-new_pa_corrs_list)/SE_pair_orig_list)
  
  # Combine all output into list
  rownames(old_pa_corrs) = pair_assoc_vars
  colnames(old_pa_corrs) = pair_assoc_vars
  rownames(new_pa_corrs) = pair_assoc_vars
  colnames(new_pa_corrs) = pair_assoc_vars
  rownames(abs_diff_pa_corrs) = pair_assoc_vars
  colnames(abs_diff_pa_corrs) = pair_assoc_vars
  rownames(rel_diff_pa_corrs) = pair_assoc_vars
  colnames(rel_diff_pa_corrs) = pair_assoc_vars
  all_out = list('orig_pairwise_assoc' = old_pa_corrs,
                 'new_pairwise_assoc' = new_pa_corrs,
                 'abs_diff_pairwise_assoc' = abs_diff_pa_corrs,
                 'rel_diff_pairwise_assoc' = rel_diff_pa_corrs,
                 'global_pairwise_utility' = R_ased)
  
  # print results
  if(print){
    print(all_out)
  }
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(all_out, paste0(out_df, '.xlsx'), rowNames=TRUE)
  }
  
  # Output
  return(list('old_assoc' = old_pa_corrs,
              'new_assoc' = new_pa_corrs,
              'abs_diff_pa_corrs' = abs_diff_pa_corrs,
              'rel_diff_pa_corrs' = rel_diff_pa_corrs,
              'R_ased' = R_ased))
}



# U-Statistic
u_stat = function(new_data, og_data, new_suffs=paste0('_', 1:5), og_suff='', 
                  changed_vars, print=TRUE, out_df=NA, out_dir=NA){
  # Calculate U-statistic: stack original and updated data and run a logistic
  # regression model to predict data set based on variables that were changed.
  # Null distribution of U statistic from Snokes et al. (2018)
  
  out = data.frame()
  
  for(new_suff in new_suffs){
    # subset to changed variables
    orig_data = og_data[,paste0(changed_vars, og_suff)]
    new_data = new_data[,paste0(changed_vars, new_suff)]
    
    # rename to same variables
    names(orig_data) = changed_vars
    names(new_data) = changed_vars
    
    # add variables for dataset
    orig_data$new_data = 0
    new_data$new_data = 1
    
    # stack
    data_for_U = rbind(orig_data, new_data)
    
    # logistic regression
    logreg = glm(new_data~., data=data_for_U, family=binomial())
    
    # U statistic
    N = nrow(data_for_U)
    U = 1/N * sum((logreg$fitted.values - 0.5)^2)
    
    # Null U-statistic distributed as a multiple of Chi-square distribution
    # with k-1 degrees of freedom
    # Expected value of U-statistic
    k = length(logreg$coefficients)
    EV = (k-1)/(8*N)
    # SE of U-statistic
    SE = sqrt(2*(k-1)/(8*N))
    # p-value
    pval = 1-pchisq(U, df=EV)
    
    # output
    all_out = data.frame('U' = U,
                   'EV_U0' = EV,
                   'SE_U0' = SE,
                   'pval' = pval)
  }
  
  
  # print results
  if(print){
    print(all_out)
  }
  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(all_out, paste0(out_df, '.xlsx'))
  }
  
  return(all_out)
}




# Regression Coefficients
reg_coeff = function(svy, outcome, preds, suff){
  # get weighted regression coefficients for predicting outcome given preds

  # OLS or logistic regression
  num_outcomes = length(unique(svy$variables[[outcome]]))
  type = ifelse(num_outcomes==2, 'logistic', 'ols')
  
  # fit regression model
  if(type=='ols'){
      reg = svyglm(as.formula(paste0(outcome, suff, '~', paste(paste0(preds, suff), collapse='+'))),
               svy)
  }
  else{
    reg = svyglm(as.formula(paste0(outcome, suff, '~', paste(paste0(preds, suff), collapse='+'))),
                 svy, family=quasibinomial)
  }
  # attach confidence interval
  out = cbind(summary(reg)$coefficients, confint(reg))
  out = data.frame('Outcome'=rep(outcome, nrow(out)), 
                   'Predictor'=names(reg$coefficients),
                   data.frame(out))
  return(out)
}

beta_comparison = function(svy_new, svy_og, outcomes, all_preds, new_suff, og_suff='', print=TRUE,
                           plot=TRUE, plot_name=NA, out_df=NA, out_dir=NA){
  # Calculates regression coefficients for multiple regression models, compares
  # beta values and p-values before and after
  # Inputs:
  # svy_new = new survey design object
  # svy_og = survey design object of original data
  # outcomes = vector of outcomes
  # all_preds = list of vectors of predictors for each outcome
  # new_suff = suffix of updated variables
  # og_suff = suffix of original variables
  # print = TRUE/FALSE, whether or not to print all output
  # plot = TRUE/FALSE, whether to create a scatterplot of p-values
  # plot_name = name of file to save plot as, NA if not saving
  # out_df = name of Excel file to save all output, NA if not saving
  # out_dir = output directory to save plot, out_df, NA if default directory
  
  # coefficients
  n_regs = length(outcomes)
  ## original
  og_coeffs = lapply(1:n_regs, function(x) reg_coeff(svy_og, outcomes[x], all_preds[[x]], og_suff))
  ## updated
  new_coeffs = lapply(1:n_regs, function(x) reg_coeff(svy_new, outcomes[x], all_preds[[x]], new_suff))
  
  # Merge
  all_coeffs = cbind(do.call(rbind, og_coeffs), do.call(rbind, new_coeffs)[,-c(1:2)])
  # remove intercept rows and convert to data frame
  all_coeffs = all_coeffs[-which(rownames(all_coeffs)=='(Intercept)'),] %>% data.frame()
  
  # interval overlap
  all_coeffs$IO = IO(all_coeffs$X2.5.., all_coeffs$X97.5.., all_coeffs$X2.5...1, all_coeffs$X97.5...1)
  
  # significance
  all_coeffs$og_sig = ifelse(all_coeffs$Pr...t..<.05, 1, 0)
  all_coeffs$new_sig = ifelse(all_coeffs$Pr...t...1<.05, 1, 0)
  
  all_out = list('Coefficients'=all_coeffs)
  if(print) print(all_coeffs)
  
  # 2x2 significance table of beta coefficients
  if(length(unique(c(all_coeffs$og_sig, all_coeffs$new_sig)))==2){
    xtab = table(all_coeffs$og_sig, all_coeffs$new_sig)
    conf_mat = xtab
    #conf_mat = confusionMatrix(xtab)
    if(print) print(conf_mat)
    
    all_out[['Confusion Matrix']] = conf_mat
  }
  
  # plot of before/after p-values
  if(plot){
    pval_plot = ggplot(all_coeffs, aes(all_coeffs$Pr...t.., all_coeffs$Pr...t...1)) +
      geom_abline(intercept=0, slope=1, col='grey') + 
      geom_point() +
      theme_bw() +
      xlab('Old P-value') + ylab('New P-value') +
      ggtitle(paste('Effect of Data Treatment on P-values'))
    print(pval_plot)
    if(!is.na(plot_name)){
      if(!is.na(out_dir)) setwd(out_dir)
      ggsave(paste0(plot_name, '.pdf'), pval_plot, width=8, height=6)
    }
  }

  # save results
  if(!is.na(out_df)){
    if(!is.na(out_dir)) setwd(out_dir)
    write.xlsx(all_out, paste0(out_df, '.xlsx'))
  }

  return(all_out)
}






################################################################################
######################## Measure of Variance Checks ############################
################################################################################


# Between-Implicate Variance
btwn_implicate_stats = function(n_imp, out_dir,
                                pct_rec_ch=NA, dist_btwn_cont=NA, dist_btwn_cat=NA,
                                high_util_cross=NA, ci_overlap=NA, k_marg=NA,
                                pair_assoc=NA, reg_coeffs=NA, u_stat=NA){
  # Calculates mean and SD across implicates for various of the above utility measures
  
  # n_imp = number of implicates
  # out_dir = directory where all output is stored
  # Unique portion of spreadsheet name for...
  # pct_rec_ch = percent of records changed
  # dist_btwn_cont = distance between continuous variables
  # dist_btwn_cat = distance between categorical variables
  
  setwd(out_dir)
  all_files = list.files(out_dir)
  all_files = all_files[grepl('.xlsx', all_files)]
  all_files = all_files[!grepl('_implicates', all_files)]
  
  # Percent of records changed
  if(!is.na(pct_rec_ch)){
    ## Get list of spreadsheet with pct of records changed output
    pct_rec_ch_files = all_files[grepl(pct_rec_ch, all_files)] 
    ## Check for correct number of implicates
    if(length(pct_rec_ch_files) != n_imp){
      print('Incorrect number of implicates for percent of records changed.')
    }
    ## read in and combine into 1 data frame
    all_pct_rec_ch = lapply(pct_rec_ch_files, read.xlsx) %>%
      reduce(left_join, by='variable')
    names(all_pct_rec_ch)[2:(n_imp+1)] = paste0('pct_changes', 1:n_imp)
    ## mean and SD of pct records changed
    all_pct_rec_ch$mean = apply(all_pct_rec_ch[,-1], 1, mean)
    all_pct_rec_ch$SD = apply(all_pct_rec_ch[,-1], 1, sd)
    ## save resulting file
    write.xlsx(all_pct_rec_ch, paste0(pct_rec_ch, '_implicates.xlsx'))
  }
  
  # Distance between continuous variables
  if(!is.na(dist_btwn_cont)){
    ## Get list of spreadsheet names with dist btwn continuous vars output
    dist_btwn_cont_files = all_files[grepl(dist_btwn_cont, all_files)]
    ## Check for correct number of implicates
    if(length(dist_btwn_cont_files) != n_imp){
      print('Incorrect number of implicates for distance between continuous variables.')
    }
    ## read in and combine into 1 data frame
    all_dist_btwn_cont = lapply(dist_btwn_cont_files, read.xlsx)
    ### remove names of variables 
    all_dist_btwn_cont1 = lapply(all_dist_btwn_cont, function(x) x[,-1])
    ### convert to array (to more easily do calculations)
    all_dist_btwn_cont_array = array(unlist(all_dist_btwn_cont1), dim=c(dim(all_dist_btwn_cont1[[1]]), length(all_dist_btwn_cont1)))
    ### calculate means
    all_dist_btwn_cont_mean = apply(all_dist_btwn_cont_array, c(1,2), mean, na.rm=TRUE)
    all_dist_btwn_cont_mean = cbind(all_dist_btwn_cont[[1]][,1], data.frame(all_dist_btwn_cont_mean))
    names(all_dist_btwn_cont_mean) = names(all_dist_btwn_cont[[1]])
    ### calculate sds
    all_dist_btwn_cont_sd = apply(all_dist_btwn_cont_array, c(1,2), sd, na.rm=TRUE)
    all_dist_btwn_cont_sd = cbind(all_dist_btwn_cont[[1]][,1], data.frame(all_dist_btwn_cont_sd))
    names(all_dist_btwn_cont_sd) = names(all_dist_btwn_cont[[1]])
    ## output
    all_dist_btwn_cont[[length(all_dist_btwn_cont)+1]] = all_dist_btwn_cont_mean
    all_dist_btwn_cont[[length(all_dist_btwn_cont)+1]] = all_dist_btwn_cont_sd
    names(all_dist_btwn_cont) = c(1:(length(all_dist_btwn_cont)-2), 'mean', 'sd')
    ## save
    write.xlsx(all_dist_btwn_cont, paste0('dist_btwn_cont_implicates.xlsx'))
  }
  
  # Distance between categorical variables
  if(!is.na(dist_btwn_cat)){
    ## Get list of spreadsheet with pct of records changed output
    dist_btwn_cat_files = all_files[grepl(dist_btwn_cat, all_files)] 
    ## Check for correct number of implicates
    if(length(dist_btwn_cat_files) != n_imp){
      print('Incorrect number of implicates for distance between categorical variables.')
    }
    ## read in and combine into 1 data frame
    all_dist_btwn_cat = lapply(dist_btwn_cat_files, read.xlsx) %>%
      reduce(left_join, by='Variable')
    names(all_dist_btwn_cat)[2:(n_imp+1)] = paste0('Hellinger_dist_', 1:n_imp)
    ## mean and SD of pct records changed
    all_dist_btwn_cat$mean = apply(all_dist_btwn_cat[,-1], 1, mean)
    all_dist_btwn_cat$SD = apply(all_dist_btwn_cat[,-1], 1, sd)
    ## save resulting file
    write.xlsx(all_dist_btwn_cat, paste0(dist_btwn_cat, '_implicates.xlsx'))
  }
  
  # High Utility Crosstabs
  if(!is.na(high_util_cross)){
    ## Get list of spreadsheets with high utility crosstabs
    high_util_cross_files = all_files[grepl(high_util_cross, all_files)]
    high_util_cross_files = high_util_cross_files[grepl('.xlsx', high_util_cross_files)]
    ## Check for correct number of implicates
    if(length(high_util_cross_files) != n_imp){
      print('Incorrect number of implicates for high utility crosstabs.')
    }
    ## read in and combine
    all_cross = lapply(high_util_cross_files, read.xlsx)
    ## count number of variables in each crosstab
    merge_vars = names(all_cross[[1]])[1:(ncol(all_cross[[1]])-4)]
    num_crosstab_vars = function(ctabs, num_vars){
      ctabs$num_vars = apply(ctabs, 1, function(x) sum(!is.na(x[1:num_vars])))
      return(ctabs)
    }
    all_cross = lapply(all_cross, function(x) num_crosstab_vars(x, length(merge_vars)))
    ## difference between weighted counts in crosstabs
    crosstab_diff = function(ctabs){
      ctabs$abs_rel_diff = abs(ctabs$orig_wgt_count-ctabs$new_wgt_count)/ctabs$orig_wgt_count
      return(ctabs)
    }
    all_cross = lapply(all_cross, crosstab_diff)
    ## summary of abs. rel. diff. by number of variabls
    diff_by_var = function(ctabs){
      out = ctabs %>% 
        group_by(num_vars) %>%
        summarize(mean = mean(abs_rel_diff, na.rm=TRUE),
                  sd = sd(abs_rel_diff, na.rm=TRUE))
      return(out)
    }
    all_cross_summary = lapply(all_cross, diff_by_var)
    ## merge
    all_cross_summary = all_cross_summary %>% reduce(left_join, by='num_vars')
    names(all_cross_summary) = c('num_vars', paste0(c('mean', 'sd'), rep(1:5, each=2)))
    ## overall sd in mean relative distance for implicates
    all_cross_summary$mean_of_means_across_implicates = apply(all_cross_summary[,grepl('mean', names(all_cross_summary))],
                                                            1, mean)
    all_cross_summary$sd_of_means_across_implicates = apply(all_cross_summary[,grepl('mean', names(all_cross_summary))],
                                                            1, sd)
    ## save output
    write.xlsx(all_cross_summary, paste0(high_util_cross, '_implicates.xlsx'))
  }
  
  # Confidence Interval Overlap and Point Estimates w/in Interval
  if(!is.na(ci_overlap)){
    ## Get list of spreadsheets with confidence intervals
    ci_overlap_files = all_files[grepl(ci_overlap, all_files)]
    ## Check for correct number of implicates
    if(length(ci_overlap_files) != n_imp){
      print('Incorrect number of implicates for CI overlap.')
    }
    ## read in files
    all_ci_overlap = lapply(ci_overlap_files, read.xlsx)
    ## keep only variables for var, io, and new_est_in_old_ci
    if('subset' %in% names(all_ci_overlap[[1]])) merge_vars=c('var', 'subset')
    else merge_vars = c('var')
    all_ci_overlap = lapply(all_ci_overlap, function(x) x[,c(merge_vars, 'io', 'new_est_in_old_ci')])
    ## merge
    all_ci_overlap = all_ci_overlap %>% reduce(left_join, by=merge_vars)
    names(all_ci_overlap) = c(merge_vars, 
                              paste0(c('io', 'new_est_in_old_ci'), rep(1:5, each=2)))
    ## add summary statistics
    all_ci_overlap$mean_io = apply(all_ci_overlap[,grepl('io', names(all_ci_overlap))],
                                   1, function(x) mean(as.numeric(x)))
    all_ci_overlap$sd_io = apply(all_ci_overlap[,grepl('io', names(all_ci_overlap))],
                                 1, function(x) sd(as.numeric(x)))
    all_ci_overlap$mean_in_ci = apply(all_ci_overlap[,grepl('new_est', names(all_ci_overlap))],
                                      1, function(x) mean(as.numeric(x)))
    all_ci_overlap$sd_in_ci = apply(all_ci_overlap[,grepl('new_est', names(all_ci_overlap))],
                                    1, function(x) sd(as.numeric(x)))
    ## save output
    write.xlsx(all_ci_overlap, paste0(ci_overlap, '_implicates.xlsx'))
  }
  
  # k-Marginal score
  if(!is.na(k_marg)){
    ## Get list of spreadsheets with k-marginal scores
    k_marg_files = all_files[grepl(k_marg, all_files)]
    ## check for correct number of implicates
    if(length(k_marg_files) != n_imp){
      print('Incorrect number of implicates for k-marginal scores.')
    }
    ## read in files
    k_margs = lapply(k_marg_files, read.xlsx)
    k_margs = lapply(k_margs, setNames, c('stat', 'k_marginal'))
    all_k_margs = k_margs %>% reduce(left_join, by='stat')
    names(all_k_margs) = c('Stat', paste0('imp', 1:(ncol(all_k_margs)-1)))
    ## save output
    write.xlsx(all_k_margs, paste0(k_marg, '_implicates.xlsx'))
  }
  
  # Pairwise Associations
  if(!is.na(pair_assoc)){
    ## Get list of spreadsheets with pairwise associations
    pair_assoc_files = all_files[grepl(pair_assoc, all_files)]
    pair_assoc_files = pair_assoc_files[grepl('.xlsx', pair_assoc_files)]
    ## check for correct number of implicates
    if(length(pair_assoc_files) != n_imp){
      print('Incorrect number of implicates for pairwise associations.')
    }
    ## read in files
    all_pair_assoc = lapply(pair_assoc_files, function(x) read.xlsx(x, 'rel_diff_pairwise_assoc'))
    ## get rel diff in correlations in upper tri
    all_pair_assoc = sapply(all_pair_assoc, function(x) x[,-1][upper.tri(x[,-1])]) %>% data.frame
    ## summary statistics of relative differences in correlations
    all_pair_assoc$mean = apply(all_pair_assoc, 1, mean)
    all_pair_assoc$sd = apply(all_pair_assoc[,-which(names(all_pair_assoc)=='mean')], 1, sd)
    pair_assoc_stats = data.frame('means_of_rel_diff_in_corrs'=apply(all_pair_assoc, 2, mean),
                                  'sd_of_rel_diff_in_corrs'=apply(all_pair_assoc, 2, sd))
    ## save results
    write.xlsx(list(all_pair_assoc, pair_assoc_stats), paste0(pair_assoc, '_implicates.xlsx'))
  }
  
  # Regression Coefficients
  if(!is.na(reg_coeffs)){
    ## Get list of spreadsheets with regression coefficients
    reg_coeff_files = all_files[grepl(reg_coeffs, all_files)]
    ## check for correct number of implicates
    if(length(reg_coeff_files) != n_imp){
      print('Incorrect number of implicates for regression coefficients.')
    }
    ## read in files
    all_reg_coeffs = lapply(reg_coeff_files, function(x) read.xlsx(x, 'Coefficients'))
    ## get accuracy for each implicate
    get_acc = function(tab){
      acc = sum(tab$og_sig==tab$new_sig)/nrow(tab)
      return(acc)
    }
    reg_coeff_sig_acc = sapply(all_reg_coeffs, get_acc)
    imp_reg_acc = data.frame('implicate'=1:length(reg_coeff_sig_acc),
                             'accuracy'=reg_coeff_sig_acc)
    imp_reg_acc = rbind(imp_reg_acc, data.frame('implicate'=c('mean', 'sd'), 
                                                'accuracy'=c(mean(reg_coeff_sig_acc),
                                                             sd(reg_coeff_sig_acc))))
    ## save results
    write.xlsx(imp_reg_acc, paste0(reg_coeffs, '_implicates.xlsx'))
  }
  
  # U-Statistic
  if(!is.na(u_stat)){
    ## Get list of spreadsheets with u-statistics
    u_stat_files = all_files[grepl(u_stat, all_files)]
    ## check for correct number of implicates
    if(length(u_stat_files) != n_imp){
      print('Incorrect number of implicates for U-Statistics.')
    }
    ## read in files
    u_stats = lapply(u_stat_files, read.xlsx) %>% bind_rows()
    ## calculate mean, sd
    u_stats_means = apply(u_stats, 2, mean)
    u_stats_sd = apply(u_stats, 2, sd)
    u_stats = rbind(u_stats, u_stats_means, u_stats_sd)
    u_stats = cbind(data.frame('x'=c(1:length(u_stat_files), 'mean', 'sd')), u_stats)
    ## save results
    write.xlsx(u_stats, paste0(u_stat, '_implicates.xlsx'))
  }
  
  
}





################################################################################
######################## Treated Data Set Rankings #############################
################################################################################


# Reads in summarized utility measures and create ranking of each treatment method
treatment_rankings = function(output_folders,
                              dist_btwn_cont=NA){
  
  rankings = data.frame()
  
  # distance between continuous variables
  
  
  # distance between categorical variables
  # high-utility crosstabs
  # confidence interval overlap
  # k-marginal score
  # higher-order conjunction
  # pairwise associations
  # significance of regression coefficients
  # u-statistic
  
  
}
