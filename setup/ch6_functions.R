#  title: "ch6_functions.R"
#  date: 04/03/2021
#  author: Grant Fleming
#  purpose: functions for ch6_communities_script.Rmd and ch6_compas_script.Rmd 

# helper function for make_protected to add "_protected" suffix to columns in a dataframe
protected = function(x) {
  paste0(x, "_protected")
}

# use protected to rename columns in a dataframe with "_protected" suffix
make_protected = function(df, string) {
  if (is.data.frame(df) == TRUE) {
    df %>% 
      rename_with(protected, matches(string))
  } else {
    stop("This function only takes dataframes (or tibbles) as inputs")
  }
}

# pull protected features from a dataframe
get_protected_feats = function(df) {
  
  df %>% 
    dplyr::select(ends_with("protected")) %>% 
    colnames() %>% 
    str_remove("_protected")
  
}

# preprocess data and prepare it for modeling
gen_preprocessed_data = function(data_split, target = NA, model_names = NA, split_ratio = NA,
                                 corr_threshold = 1, impute = FALSE, standardize = FALSE,
                                 seed = NA) {
  
  if (!is.na(seed)) {
    set.seed(seed)
  }
  
  if ("boosted_trees" %in% model_names) {
    
    if (!is.na(split_ratio)) {
      
      data_split = data_split$data %>% 
        mutate(across(all_of(target), as.numeric)) %>% 
        initial_split(split_ratio)
      
    } else {
      
      abort("must provide a numeric value between 0 and 1 for split_ratio if using boosted_trees")
      
    }
    
  }
  
  
  # get column types
  col_types = data_split %>%
    training() %>% 
    dplyr::summarize(across(everything(), class)) %>% 
    pivot_longer(cols = everything(),
                 names_to = "variables",
                 values_to = "type") 
  
  if (any(col_types$type == "integer")) {
    
    col_types = data_split %>%
      training() %>% 
      mutate(across(where(is.integer), as.numeric)) %>% 
      dplyr::summarize(across(everything(), class)) %>% 
      pivot_longer(cols = everything(),
                   names_to = "variables",
                   values_to = "type") 
    
  }
  
  
  if ("boosted_trees" %in% model_names) {
    
    stop("boosted_trees or other xgboost-like models are not yet supported")
    
  } 
  
  df_protected = data_split %>% 
    training()
  
  # get a formula for creating our recipe
  form = paste(paste(target, "~"), str_c(colnames(df_protected %>% dplyr::select(-all_of(target))), 
                                         collapse = "+"))
  
  if (standardize == TRUE) {
    
    if (impute == TRUE) {
      
      if (any(col_types$type == "numeric") & any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_corr(all_numeric(), threshold = corr_threshold, -all_outcomes()) %>% 
          step_medianimpute(all_numeric(), -all_outcomes()) %>% 
          step_modeimpute(all_nominal(), -all_outcomes()) %>% 
          step_range(all_numeric(), -all_outcomes()) 
        
      } else if (!any(col_types$type == "numeric") & any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_modeimpute(all_nominal(), -all_outcomes()) 
        
      } else if (any(col_types$type == "numeric") & !any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_corr(all_numeric(), threshold = corr_threshold, -all_outcomes()) %>% 
          step_medianimpute(all_numeric(), -all_outcomes()) %>% 
          step_range(all_numeric(), -all_outcomes())
      }
      
    } else {
      
      if (!any(col_types$type == "numeric") & any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes())
        
      } else {
        
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_corr(all_numeric(), threshold = corr_threshold, -all_outcomes()) %>% 
          step_range(all_numeric(), -all_outcomes())
        
      }
      
    }
    
  } else {
    
    if (impute == TRUE) {
      
      if (any(col_types$type == "numeric") & any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_corr(all_numeric(), threshold = corr_threshold, -all_outcomes()) %>% 
          step_medianimpute(all_numeric(), -all_outcomes()) %>% 
          step_modeimpute(all_nominal(), -all_outcomes())
        
      } else if (!any(col_types$type == "numeric") & any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_modeimpute(all_nominal(), -all_outcomes()) 
        
      } else if (any(col_types$type == "numeric") & !any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_corr(all_numeric(), threshold = corr_threshold, -all_outcomes()) %>% 
          step_medianimpute(all_numeric(), -all_outcomes()) 
      }
      
    } else {
      
      if (!any(col_types$type == "numeric") & any(col_types$type == "factor")) {
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes())
        
      } else {
        
        rec = recipe(df_protected, form) %>%
          step_nzv(all_predictors(), -all_outcomes()) %>%
          step_corr(all_numeric(), threshold = corr_threshold, -all_outcomes()) 
        
      }
      
    }
    
  }
  
  
  # nzv, corr, normalize, medianimpute, modeimpute, center, scale, dummy, discretize, dummy,
  # unknown
  
  # formalize our pre-processing steps
  rec_prep = rec %>% 
    prep()
  
  # preprocess our train data by "baking" it with our prepped recipe
  train_data = bake(rec_prep, training(data_split) %>% mutate(across(where(is.integer), as.numeric)))
  
  # preprocess our test data by "baking" it with our prepped recipe
  test_data = bake(rec_prep, testing(data_split) %>% mutate(across(where(is.integer), as.numeric)))
  
  # get original data for output
  input_data = df_protected %>% 
    mutate(across(where(is.integer), as.numeric))
  
  # get features
  data_feats = train_data %>% 
    colnames() 
  
  # get a formula for creating our recipe
  final_form = paste(paste(target, "~"), str_c(colnames(train_data %>% dplyr::select(-all_of(target))), 
                                               collapse = "+"))
  
  # return a list containing our two pre-processed datasets
  list("input_data" = input_data,
       "train_raw" = training(data_split),
       "test_raw" = testing(data_split),
       "train" = train_data, 
       "test" = test_data,
       "features" = data_feats,
       "recipe" = rec,
       "recipe_prepped" = rec_prep,
       "formula" = final_form)
  
}

# function creates one plot per protected feature of the top n = number features that are correlated with
# the particular level of the protected feature (protected group) being plotted. larger gaps between the
# red and black points (average correlation between that feature and all other protected groups) indicate 
# that the feature on the X axis is more correlated with that protected group than with the others. 
check_correlations = function(df, drop_cols, protected_features, number = 5) {
  
  features = df %>% 
    select(-all_of(drop_cols))
  
  correlations = features %>% 
    correlate() %>% 
    stretch() %>% 
    rename(feature1 = x, feature2 = y, corr = r) %>% 
    filter(feature1 %in% protected_features) 
  
  correlations %>% 
    group_by(feature2) %>% 
    mutate(avg_corr = mean(abs(corr))) %>% 
    ungroup() %>% 
    drop_na() %>% 
    group_by(feature1) %>% 
    arrange(feature1, corr) %>% 
    group_split() %>% 
    map(., ~slice_max(., corr, n = number)) %>% 
    map(., ~ggplot(.) +
          geom_point(aes(x = reorder(feature2, corr), y = corr, color = "red", size = 0.4)) +
          geom_point(aes(x = reorder(feature2, corr), y = avg_corr, size = 0.4)) +
          ylim(0, 1) +
          labs(x = "Features", y = "Absolute correlation", title = paste("Top Features by Absolute Correlation with",
                                                                         .x[[1]])) +
          theme(legend.position = "none"))
  
}
