#  title: "ch7_functions.R"
#  date: 04/03/2021
#  author: Grant Fleming
#  purpose: functions for ch7_compas_script.Rmd and ch7_communities_script.Rmd 

# tidy-friendly dropping of cols that contain too many of certain vals (or NAs eventually)
col_dropper = function(df, val_interest = NA, threshold = 0.10, missing = FALSE) {
  
  drop_cols = purrr::map(df, function(x) sum(x == val_interest)/length(x)) %>% 
    enframe() %>% 
    unnest(value) %>% 
    filter(value >= threshold) %>% 
    pull(name)
  
  df %>% 
    dplyr::select(!any_of(drop_cols))
  
}

# internal function for make_protected to add "_protected" suffix to columns in a dataframe
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

# internal function for check_vals
internal_check_vals = function(x, val = NA) {
  
  if(is.na(val)) {
    
    sum(is.na(x))
    
  } else {
    
    sum(x == val)
    
  }
  
}

# function that returns a dataframe of the number of times a specific val (usually NA) appears per column
check_vals = function(x, val = NA) {
  
  purrr::map_df(x, internal_check_vals, val) %>%  # 13 NA in assaults, let's drop
    pivot_longer(cols = everything(),
                 names_to = "variables",
                 values_to = "num") %>% 
    filter(num > 0)
  
}


# function to assign names in a named list to a columns in a dataframe
assign_names = function(df, names) {
  
  df %>% 
    mutate(model = {{names}}) %>% 
    relocate(model, everything())
  
}

# function to clean compas data
clean_compas = function(df) {
  
  df %>% 
    filter(days_b_screening_arrest <= 30) %>%
    filter(days_b_screening_arrest >= -30) %>%
    filter(is_recid != -1) %>%
    filter(c_charge_degree != "O") %>%
    filter(compas_screening_date <= as.Date("2014-04-01")) %>% 
    dplyr::select(age_cat, c_charge_degree, race, sex, priors_count, 
                  juv_fel_count, juv_misd_count, juv_other_count, two_year_recid) %>% 
    mutate(race = ifelse(race %in% c("Native American", "Asian", "Other"), "Other", race)) %>% 
    # mutate(race = factor(race, levels = c("Caucasian", "African-American", "Hispanic"))) %>%
    mutate(juv_charge = ifelse(juv_fel_count > 0 | juv_misd_count > 0 | 
                                 juv_other_count > 0, "yes", "no")) %>% 
    dplyr::select(-c(juv_fel_count, juv_misd_count, juv_other_count)) %>%
    mutate(two_year_recid = factor(two_year_recid, levels = c("1", "0"))) %>%
    # mutate(two_year_recid = factor(two_year_recid, levels = c("0", "1"))) %>%
    clean_names() 
  
}

# function to clean communities and crime data
clean_crimes = function(df) {
  
  initial_df = df_raw %>% 
    relocate(assaults) %>% 
    dplyr::select(-c(131:ncol(.))) %>% 
    dplyr::select(-c(2:6)) %>% 
    mutate(assaults = as.numeric(assaults)) %>% 
    rename_with(tolower) %>% 
    drop_na() %>%  # drop na again here because the first time was just an example
    col_dropper(val_interest = "?", threshold = 0.01) %>% 
    mutate(unhoused = numinshelters + numstreet) %>% 
    dplyr::select(-c("numinshelters", "numstreet")) %>% 
    dplyr::select(-c("agepct12t21", "agepct16t24")) %>% 
    dplyr::select(-"lemaspctofficdrugun") %>% 
    dplyr::select(-matches("pctimmig|pctrec")) %>%   # drop irrelevant immigrant cols
    dplyr::select(-matches("num")) %>% 
    dplyr::select(-matches("percap"))
  
  clean_df = initial_df %>% 
    mutate(racenonwhite = racepctblack + racepctasian + racepcthisp) %>% 
    mutate(majority_race = case_when(
      (racepctwhite > racenonwhite) ~ "Majority White",
      (racenonwhite > racepctwhite) ~ "Majority Non-White"
    )) %>% 
    mutate(pcthsonly = 100 - pctnothsgrad - pctbsormore) %>% 
    mutate(majority_education = case_when(
      pctnothsgrad > pctbsormore & pctnothsgrad > pcthsonly ~ "Less than Highschool", 
      pcthsonly > pctbsormore & pcthsonly > pctnothsgrad ~ "Highschool", 
      pctbsormore > pctnothsgrad & pctbsormore > pcthsonly ~ "Post-Highschool"
    )) %>% 
    select(-c(pctnothsgrad, pcthsonly, pctbsormore, racepctwhite, racepctblack, racepctasian, racepcthisp,
              racenonwhite))
  
  return(clean_df)
  
}

# preprocess data and prepare it for modeling; returns a named list with many elements
gen_preprocessed_data = function(data_split, target = NA, model_names = NA, split_ratio = NA,
                                     corr_threshold = 1, impute = FALSE, standardize = FALSE,
                                     seed = NA) {
  
  if (!any(class(compas_split) == "rsplit")) {
    
    stop("the first argument of this functions must be a rsplit object generated
          from a function within the rsample package")
  }
  
  if (!is.na(seed)) {
    
    set.seed(seed)
  
  }
  
  if ("boosted_trees" %in% model_names) {
    
    if (!is.na(split_ratio)) {
      
      data_split = data_split$data %>% 
        mutate(across(all_of(target), as.numeric)) %>% 
        initial_split(split_ratio)
      
    } else {
      
      stop("must provide a numeric value between 0 and 1 for split_ratio if using boosted_trees")
      
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
  
  # access training split of data
  df_protected = data_split %>% 
    training()
  
  # get a formula for creating our recipe
  form = paste(paste(target, "~"), str_c(colnames(df_protected %>% dplyr::select(-all_of(target))), 
                                         collapse = "+"))
  
  # if else conditions for transformations of the data
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

# remove "_protected" from column names
make_unprotected = function(df, cols) {
  df %>% 
    str_remove_all("_protected")
  
}

# write output to datasheet and folder
write_datasheet = function(result_list, rmd_name = NA, csv_name = NA, rmd = TRUE, csv = FALSE,
                           title_var = "Report", data_name = NA, rows_represent_text = NA,
                           features_generation_text = NA) {
  
  title_var = title_var
  
  if (rmd == TRUE) {
    rmarkdown::render(here(rmd_name),
                      output_file = title_var,
                      output_dir = here())
  }
  
  if (csv == TRUE) {
    write_csv(result_list[["training"]], path = here(paste0(title_var, "_train.csv")))
    write_csv(result_list[["testing"]], path = here(paste0(title_var, "_train.csv")))
  }
}

# function to generate a dataframe of possible models for the user to select.
# all require the parsnip package from tidymodels. should be ran at the start
# of each new session if modeling needs to be done later
gen_model_frame = function() {
  
  # NOTE: tuning does not currently work
  tune_mods = list(linear_reg(),
                   logistic_reg(
                     mixture = tune(),
                     penalty = tune()
                   ),
                   svm_rbf(
                     cost = tune(),
                     margin = tune()
                   ),
                   nearest_neighbor(
                     neighbors = tune()
                   ),
                   decision_tree(
                     tree_depth = tune(),
                     min_n = tune()
                   ),
                   rand_forest(
                     trees = 1000,
                     mtry = tune(),
                     min_n = tune()
                   ),
                   boost_tree(
                     mtry = tune(),
                     min_n = tune(),
                     tree_depth = tune(),
                     learn_rate = tune(),
                     loss_reduction = tune()
                   ),
                   mars())
  
  params = list(list(),
                list(penalty(), mixture()),
                list(),
                list(),
                list(tree_depth(), min_n()),
                list(mtry(), min_n()),
                list(mtry(), min_n(), tree_depth(), learn_rate(), loss_reduction()),
                list())
  
  tibble(model_name = list("linear_regression", "logistic_regression", "svm_rbf", "nearest_neighbor",
                           "decision_tree", "random_forest", "boosted_trees", "mars"),
         model_function = list(linear_reg(), logistic_reg(), svm_rbf(), nearest_neighbor(),
                               decision_tree(), rand_forest(trees = 1000), boost_tree(), mars()),
         model_engine = list("lm", "glm", "kernlab", "kknn", "rpart", "randomForest", "xgboost", "earth"),
         tune_mods = tune_mods) %>%
    unnest(c(model_name, model_engine)) 
  
}
model_frame = gen_model_frame() # assign gen_model_frame output to an object in the local session

# internal function for gen_mods
modder = function(model_function, model_engine, model_mode) {
  
  model_function %>%
    set_engine(model_engine) %>%
    set_mode(model_mode)
  
}

# generate parsnip model specifications according to names vector input by user
gen_mods = function(model_frame, names, mode = NA, tune = FALSE, recipe) {
  
  if (!all(names %in% model_frame$model_name)) {
    
    stop("one or more invalid names entered; check gen_model_frame() for valid model_name")
    
  }
  
  if (is.na(mode)) {
    stop("Need to declare whether model is of type 'regression' or 'classification'")
  }  
  
  if (tune == TRUE) {
    
    interim = model_frame %>% 
      slice(order(factor(model_name, levels = names))) %>% 
      filter(model_name %in% names) %>% 
      mutate(mode = mode) %>%
      dplyr::select(-model_name) %>% 
      mutate(data = list(data)) %>% 
      select(tune_mods, model_engine, mode) %>% 
      rename(model_function = 1)
    
    mods = purrr::pmap(interim, modder)
    
    wflows = purrr::map(mods, gen_workflow, recipe)
    
    return(wflows)
    
  } else {
    
    interim = model_frame %>%
      slice(order(factor(model_name, levels = names))) %>% 
      filter(model_name %in% names) %>% 
      mutate(mode = mode) %>%
      dplyr::select(-model_name, -tune_mods)
    
    mods = purrr::pmap(interim, modder)
    
    wflows = purrr::map(mods, gen_workflow, recipe)
    
  }
  
  return(wflows)
  
}

# function to generate a workflows::workflow object from a parsnip model
# and recipes::recipe
gen_workflow = function(model, recipe) {
  
  workflows::workflow() %>%
    add_model(model) %>%
    add_recipe(recipe)

}

# generate a rsample object with n = num_resamples bootstrap samples
gen_boots = function(df, num_resamples) {
  
  boots = df %>%
    bootstraps(times = num_resamples)
  
}

# generate resamples - NOTE that the tune option does not currently
# work for this function 
gen_resample = function(wflow, name, resample, metrics, tune = FALSE, 
                        model_frame, df, target,
                        size = 30, grid_points = 30) {
  
  # pull out workflow from map frame
  if (any(class(wflow) == "data.frame") | any(class(wflow) == "list")) {
    
    wflow = wflow$wflow[[1]]
    
  }
  
  if (tune == FALSE) {
    
    if (is.na(metrics)) {
      
      stop("need to provide one or more metrics generated by yardstick::metric_set()")
      
    } else if (!any(class(metrics) == "metric_set")) {
      
      # option triggered if only one metric entered into metrics argument
      
      fit_resamples(
        wflow,
        resamples = resample,
        control = control_resamples(extract = function(x) extract_model(x)),
        metrics = metric_set(metrics)) %>%
        mutate(model = name) %>%
        dplyr::select(model, everything())
      
    } else {
      
      # option triggered if multiple metrics generated via metric_set are entered into metrics argument 
      
      fit_resamples(
        wflow,
        resamples = resample,
        control = control_resamples(extract = function(x) extract_model(x)),
        metrics = metrics) %>%
        mutate(model = name) %>%
        dplyr::select(model, everything())
      
    }
    
    
  } else {
    
    if (is.na(metrics)) {
      
      stop("need to provide one or more metrics generated by yardstick::metric_set()")
      
    } 
    
    if (name == "logistic_regression") {
      
      if (!any(class(metrics) == "metric_set")) {
        
        # option triggered if only one metric entered into metrics argument
        
        fit_resamples(
          wflow,
          resamples = resample,
          control = control_resamples(extract = function(x) extract_model(x)),
          metrics = metric_set(metrics)) %>%
          mutate(model = name) %>%
          dplyr::select(model, everything())
        
      } else {
        
        fit_resamples(
          wflow,
          resamples = resample,
          control = control_resamples(extract = function(x) extract_model(x)),
          metrics = metrics) %>%
          mutate(model = name) %>%
          dplyr::select(model, everything())
        
      }
      
    } else {
      
      df_small = df %>% 
        dplyr::select(!matches(c("protected", target)))
      
      grid = wflow %>% 
        parameters() %>% 
        finalize(x = df_small) %>% 
        pull(object) %>% 
        grid_latin_hypercube(size = 30)
      
    }
    
    if (!any(class(metrics) == "metric_set")) {
      
      # option triggered if only one metric entered into metrics argument
      
      tune_grid(
        wflow,
        grid = grid,
        resamples = resample,
        metrics = metric_set(metrics)) #%>%
      # mutate(model = name) %>%
      # dplyr::select(model, everything())
      
    } else {
      
      tune_grid(
        wflow,
        grid = grid,
        resamples = resample,
        metrics = metrics) #%>%
      mutate(model = name) %>%
        dplyr::select(model, everything())
      
    }
    
    # tune_bayes(
    #   model,
    #   preprocessor = recipe,
    #   resamples = resample,
    #   iter = 10,
    #   control = control_bayes(no_improve = 20, verbose = TRUE),
    #   metrics = metric_set(metric)) %>%
    #   mutate(model = name) %>%
    #   dplyr::select(model, everything())
    
  }
  
}

# create a plot comparing the performance of an interpretable and black box model
plot_coi_comparison = function(resamples, interp_model, bbox_model, metric_comparison = NA) {
  
  if (is.na(metric_comparison)) {
    
    clean_resamples = resamples %>% 
      purrr::map(collect_metrics) %>% 
      bind_rows() %>% 
      relocate(model) %>% 
      dplyr::select(model, .metric, mean) %>% 
      arrange(.metric, -mean) %>% 
      filter(model %in% c(interp_model, bbox_model))
    
  } else {
    
    clean_resamples = resamples %>% 
      purrr::map(collect_metrics) %>% 
      bind_rows() %>% 
      filter(.metric %in% metric_comparison) %>% 
      relocate(model) %>% 
      dplyr::select(model, .metric, mean) %>% 
      arrange(.metric, -mean) %>% 
      filter(model %in% c(interp_model, bbox_model))
    
    interp_frame = clean_resamples %>% 
      filter(model == interp_model)
    
    clean_resamples %>% 
      filter(model == bbox_model) %>% 
      mutate(coi = round((mean - interp_frame$mean)/(interp_frame$mean)*100, 2)) %>% 
      ggplot(aes(.metric, coi)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(x = "Metric", y = "Cost of Interpretability (percent change)", 
           title = paste("COI for", bbox_model, "relative to", interp_model)) 
    
  }
  
}

# create new model specifications from one or more rsample::rset object.
# NOTE: that the tune option for this function currently only works when set
# to FALSE
gen_model_struct = function(resample, model_workflow, tune = FALSE, interest_metric = NA) {
  
  if (tune == FALSE) {
    
    if (any(class(model_workflow) == "data.frame") | any(class(model_workflow) == "list")) {
      
      model_workflow = model_workflow$wflow[[1]] 
      
    }
    
    model_workflow
    
  } else {
    
    # pull out workflow from map frame
    
    if (any(class(model_workflow) == "data.frame") | any(class(model_workflow) == "list")) {
      
      model_workflow = model_workflow$wflow[[1]] 
      
    }
    
    if (is.na(tune)) {
      
      stop("enter a string corresponding to one of the metrics used for tuning (e.g 'precision')")
      
    }
    
    best_mod = resample %>% 
      select_best(tune, metric = interest_metric)
    
    finalize_workflow(model_workflow, best_mod)
    
  }
  
}

# create a dataframe that stores resampling specifications
gen_map_frame = function(wflow, boots, metrics = NA, names) {
  
  
  map_frame = tibble(wflow = list(wflow),
                     resample = list(boots),
                     metrics = list(metrics),
                     name = list(names))
  
}

# create baseline featureless and optimal performance "models" off of
# the user-supplied data and resample results
gen_bounding = function(processed_data, target, boots, metrics, 
                        optimal_mean = NA, optimal_sd = NA) {
  
  bounding_models = list()
  
  holder = bake_resamples(processed_data, boots)
  
  bounding_models[["f_less"]] = purrr::map_df(holder, gen_featureless, target, metrics)
  
  if (!is.na(optimal_mean) | !is.na(optimal_sd)) {
    
    if (is.numeric(optimal_mean) & is.numeric(optimal_sd)) {
      
      bounding_models[["optimal"]] = gen_optimal(optimal_mean, optimal_sd, num = nrow(bounding_models[["f_less"]])) %>% 
        mutate(metric = bounding_models$f_less$metric)
      
    } else {
      
      stop("ensure that optimal_mean and optimal_sd are each numeric values")
      
    }
    
  }
  
  return(bounding_models)
  
}

# internal function for gen_bounding_models
bake_resamples = function(processed_data, boots) {
  
  resample_frame = tibble(recipe = list(processed_data[["recipe_prepped"]]),
                          resample = list(boots)) %>% 
    unnest(resample)
  
  resample_list = purrr::map2(resample_frame$recipe, resample_frame$splits, bake)
  
  resample_list
  
}

# internal function for plot_baseline_models
metric_selector = function(resamples) {
  
  resamples %>%
    unnest(.metrics) %>%
    dplyr::select(model, .metric, .estimate) %>%
    janitor::clean_names()
  
}

# plots boxplots of performance (by display_metric) for bounding models
# and resample results
plot_baseline_models = function(bounding_models, resamples, num_resamples = NA,
                             full_plot = FALSE, display_metric = NA) {
  
  if (is.na(display_metric)) {
    
    scores = purrr::map(resamples, metric_selector) %>%
      bind_rows() %>%
      arrange(model)
    
    if (length(unique(scores$metric)) > 1) {
      
      stop("Multiple resampling metrics detected. Please indicate the desired metric to plot by 
            entering it as a string in the 'display_metric' argument of the function")
      
    } 
    
  } else {
    
    scores = purrr::map(resamples, metric_selector) %>%
      bind_rows() %>%
      arrange(model) %>%
      filter(metric == display_metric)
    
  }
  
  if (is.na(num_resamples)) {
    
    stop("provide number of resamples metrics calculated for the models")
    
  }
  
  if (full_plot == FALSE) {
    
    if (display_metric %in% c("rmse", "mae", "mse", "rse", "mape", "smape")) {
      
      scores %>%
        ggplot(aes(x = reorder(model, -estimate), y = estimate, color = model)) +
        geom_boxplot() +
        coord_flip() +
        labs(x = "Model", y = paste(display_metric, "over", as.character(num_resamples), "resamples"),
             title = "Two-Measure Performance Baseline") + 
        theme(legend.position = "none")
      
    } else {
      
      scores %>%
        ggplot(aes(x = reorder(model, estimate), y = estimate, color = model)) +
        geom_boxplot() +
        coord_flip() +
        labs(x = "Model", y = paste(display_metric, "over", as.character(num_resamples), "resamples"),
             title = "Two-Measure Performance Baseline") + 
        theme(legend.position = "none")
      
    }
    
  } else {
    
    if (display_metric %in% c("rmse", "mae", "mse", "rse", "mape", "smape")) {
      
      if (is.null(bounding_models[["optimal"]])) {
        
        scores_plotting = bind_rows(bounding_models[["f_less"]], scores) %>%
          arrange(model) %>%
          filter(metric == display_metric)
        
        scores_plotting %>%
          ggplot(aes(x = reorder(model, -estimate), y = estimate, color = model)) +
          geom_boxplot() +
          coord_flip() +
          labs(x = "Model", y = paste(display_metric, "over", as.character(num_resamples), "resamples"),
               title = "Three-Measure Performance Baseline") + 
          theme(legend.position = "none")
        
      } else {
        
        scores_plotting = bind_rows(bounding_models[["f_less"]], scores, bounding_models[["optimal"]]) %>%
          arrange(model) %>%
          filter(metric == display_metric)
        
        scores_plotting %>%
          ggplot(aes(x = reorder(model, -estimate), y = estimate, color = model)) +
          geom_boxplot() +
          coord_flip() +
          labs(x = "Model", y = paste(display_metric, "over", as.character(num_resamples), "resamples"),
               title = "Four-Measure Performance Baseline") + 
          theme(legend.position = "none")
        
      } 
      
    } else {
      
      if (is.null(bounding_models[["optimal"]])) {
        
        scores_plotting = bind_rows(bounding_models[["f_less"]], scores) %>%
          arrange(model) %>%
          filter(metric == display_metric)
        
        scores_plotting %>%
          ggplot(aes(x = reorder(model, estimate), y = estimate, color = model)) +
          geom_boxplot() +
          coord_flip() +
          labs(x = "Model", y = paste(display_metric, "over", as.character(num_resamples), "resamples"),
               title = "Three-Measure Performance Baseline") + 
          theme(legend.position = "none")
        
      } else {
        
        scores_plotting = bind_rows(bounding_models[["f_less"]], scores, bounding_models[["optimal"]]) %>%
          arrange(model) %>%
          filter(metric == display_metric)
        
        scores_plotting %>%
          ggplot(aes(x = reorder(model, estimate), y = estimate, color = model)) +
          geom_boxplot() +
          coord_flip() +
          labs(x = "Model", y = paste(display_metric, "over", as.character(num_resamples), "resamples"),
               title = "Four-Measure Performance Baseline") + 
          theme(legend.position = "none")
        
      } 
      
    }
    
  }
  
}

# function to estimate featureless performance
gen_featureless = function(holder, target, metrics) {
  
  target_type = holder %>% 
    dplyr::select(all_of(target)) %>% 
    pull() %>% 
    class()
  
  if (target_type == "factor") {
    
    target_vec = holder %>% 
      dplyr::select(target) %>% 
      pull(target)
    
    common_val = target_vec %>% 
      enframe() %>% 
      group_by(value) %>% 
      dplyr::count() %>% 
      arrange(-n) %>%
      ungroup() %>% 
      dplyr::slice(1) %>% 
      pull(value) %>% 
      as.character()
    
    f_less_vec = holder %>% 
      dplyr::select(target) %>% 
      mutate(f_less = factor(common_val, levels = levels(target_vec))) %>% 
      pull(f_less)
    
    holder %>% 
      dplyr::select(target) %>% 
      mutate(f_less = factor(common_val, levels = levels(target_vec))) %>% 
      metrics(truth = target_vec, estimate = f_less_vec) %>% 
      dplyr::rename(metric = .metric, estimate = .estimate) %>%
      mutate(model = "f_less") %>% 
      dplyr::select(model, metric, estimate)
    
  } else {
    
    target_vec = holder %>% 
      dplyr::select(target) %>% 
      pull(target)
    
    common_value = target_vec %>% 
      enframe() %>% 
      summarize(avg = mean(value)) %>% 
      pull()
    
    f_less_vec = holder %>% 
      dplyr::select(target) %>% 
      mutate(f_less = common_value) %>% 
      pull(f_less)
    
    holder %>% 
      dplyr::select(target) %>% 
      mutate(f_less = common_value) %>% 
      metrics(truth = target_vec, estimate = f_less_vec) %>% 
      dplyr::rename(metric = .metric, estimate = .estimate) %>%
      mutate(model = "f_less") %>% 
      dplyr::select(model, metric, estimate) %>% 
      mutate(estimate = ifelse(estimate == Inf, NA, estimate))
    
  }
  
}

# function to estimate optimal model performance as a function
# of user-input parameters (hopefully, ones informed by expert
# subject matter knowledge)
gen_optimal = function(optimal_mean, optimal_sd, num) {
  
  distribution = rnorm(n = num, mean = optimal_mean, sd = optimal_sd)
  
  tibble("model" = "optimal",
         "metric" = "accuracy",
         "estimate" = distribution)
  
}

# generate predictions for each model object provided and bind to
# input dataframes
gen_pred_data = function(fit_mods, mode, data_split, event_level) {
  
  if (mode == "classification") {
    
    if (class(fit_mods) == "list") {
      
      new_recs = purrr::map(fit_mods, pull_workflow_prepped_recipe)
      
      new_fits = purrr::map(fit_mods, pull_workflow_fit)
      
      new_test_data = purrr::map(new_recs, bake, testing(data_split))
      
      new_preds = purrr::map2(new_fits, new_test_data, predict, type = "prob")
      
      target = new_recs[[1]]$var_info %>% 
        filter(role == "outcome") %>% 
        pull(variable)
      
      bound_data = purrr::map2(new_test_data, new_preds, 
                        add_preds,
                        mode = mode, 
                        event_level = event_level,
                        target = target)
      
    } else if (class(fit_mods) == "workflow") {
      
      new_recs = pull_workflow_prepped_recipe(fit_mods)
  
      new_fits = pull_workflow_fit(fit_mods)
      
      new_test_data = bake(new_recs, testing(data_split))
      
      new_preds = new_fits %>% 
        predict(new_test_data, type = "prob")
      
      target = new_recs$var_info %>% 
        filter(role == "outcome") %>% 
        pull(variable)
      
      bound_data = add_preds(new_test_data, 
                             new_preds, 
                             mode = mode, 
                             event_level = event_level,
                             target = target)
      
    }
    
  } else if (mode == "regression") {
    
    new_recs = purrr::map(fit_mods, pull_workflow_prepped_recipe)
    
    new_fits = purrr::map(fit_mods, pull_workflow_fit)
    
    new_test_data = purrr::map(new_recs, bake, testing(data_split))
    
    new_preds = purrr::map2(new_fits, new_test_data, predict, type = "numeric")
    
    bound_data = purrr::map2(new_test_data, new_preds, add_preds,
                      mode = mode)
    
    if (class(fit_mods) == "list") {
      
      new_recs = purrr::map(fit_mods, pull_workflow_prepped_recipe)
      
      new_fits = purrr::map(fit_mods, pull_workflow_fit)
      
      new_test_data = purrr::map(new_recs, bake, testing(data_split))
      
      new_preds = purrr::map2(new_fits, new_test_data, predict, type = "numeric")
      
      bound_data = purrr::map2(new_test_data, new_preds, add_preds,
                        mode = mode)
      
    } else if (class(fit_mods) == "workflow") {
      
      new_recs = pull_workflow_prepped_recipe(fit_mods)
      
      new_fits = pull_workflow_fit(fit_mods)
      
      new_test_data = bake(new_recs, testing(data_split))
      
      new_preds = new_fits %>% 
        predict(new_test_data, type = "numeric")
      
      bound_data = add_preds(new_test_data, 
                             new_preds, 
                             mode = mode)
      
    }
    
  } else {
    
    stop("mode must be either 'classification' or 'regression'")
    
  }
  
  return(bound_data)
  
}

# calculate overall performance metrics from a dataframe of data + predictions
gen_overall_metrics = function(df, target, estimate, val_interest,  
                               metrics, mode) {
  
  df_target = df %>%
    pull(target)

  df_estimate = df %>%
    pull(estimate)

  if (mode == "classification") {

    if (is.na(target) | is.na(estimate)) {

      stop("need to provide values for target and estimate if analyzing a classification model_model")

    } else {

      prob_id = str_c("pred", val_interest, sep = "_")

      tibble(data = list(df),
             truth = list(df_target),
             estimate = list(df_estimate),
             prob = list(prob_id)) %>%
        purrr::pmap(metrics) %>%
        magrittr::extract2(1) %>%
        clean_names()

    }

  } else if (mode == "regression") {

    tibble(data = list(df),
           truth = list(df_target),
           estimate = list(df_estimate)) %>%
      purrr::pmap(metrics) %>%
      magrittr::extract2(1) %>%
      clean_names()

  } else {

    stop("mode argument only accepts values 'regression' or 'classification'")

  }
  
}

# calculate per-group performance metrics from a dataframe of data + predictions
gen_protected_metrics = function(df, model, target, event_name, estimate, mode,
                                 protected, privileged, metrics) {
  
  if (is.na(protected)) {
    
    vars = df %>% 
      dplyr::select(matches("_protected")) %>%
      colnames() 
    
    protected = vars
    
  } else {
    
    vars = df %>% 
      dplyr::select(matches(protected)) %>%
      colnames()
    
  }
  
  if (is.null(metrics)) {
    
    stop("metrics argument requires metric functions from the yardstick package as inputs")
    
  } 
  
  if (mode == "classification") {
    
    group_metrics_raw = vars %>% 
      set_names(vars) %>% 
      purrr::map_df( ~ {
        
        df %>%
          group_by(across(all_of(.x))) %>%
          metrics(!!sym(target), !!sym(event_name), estimate = pred_class)
        
      },
      .id = "protected_feature") %>% 
      clean_names() %>% 
      dplyr::select(-estimator) %>% 
      unite("group", matches("_protected"), na.rm = TRUE) %>% 
      arrange(protected_feature, group) %>% 
      mutate(id = NA)
    
    holder = list()
    for (i in 1:length(protected)) {
      holder[[i]] = group_metrics_raw %>% 
        mutate(id = ifelse(str_detect(protected_feature, protected[[i]]), 
                           protected[[i]], id)) %>% 
        mutate(privileged = ifelse(str_detect(group, privileged[[i]]), TRUE, FALSE)) 
    }
    
    group_metrics = holder %>% 
      bind_rows() %>% 
      drop_na() %>% 
      mutate(model = model) %>% 
      relocate(model, id, protected_feature, group, privileged, metric, estimate) 
    
  } else if (mode == "regression") {
    
    group_metrics_raw = vars %>% 
      set_names(vars) %>% 
      purrr::map_dfr( ~ {
        
        df %>% 
          group_by(across(all_of(.x))) %>% 
          metrics({{target}}, {{estimate}})
        
      },
      .id = "protected_feature") %>% 
      clean_names() %>% 
      dplyr::select(-estimator) %>% 
      unite("group", matches("_protected"), na.rm = TRUE) %>% 
      arrange(protected_feature, group) %>% 
      mutate(id = NA)
    
    
    holder = list()
    for (i in 1:length(protected)) {
      holder[[i]] = group_metrics_raw %>% 
        mutate(id = ifelse(str_detect(protected_feature, protected[[i]]), 
                           protected[[i]], id)) %>% 
        mutate(privileged = ifelse(str_detect(group, privileged[[i]]), TRUE, FALSE))
    }
    
    group_metrics = holder %>% 
      bind_rows() %>% 
      drop_na() %>% 
      mutate(model = model) %>% 
      relocate(model, id, protected_feature, group, privileged, metric, estimate) 
    
  } else {
    
    stop("mode argument only accepts values 'regression' or 'classification'")
    
  }
  
}

# plot per-group metrics created by gen_protected_metrics
plot_protected_metrics = function(protected_metrics, protected = NA, 
                                  display_metric = NA) {
  
  metrics_filtered = protected_metrics %>% 
    filter(str_detect(protected_feature, {{protected}})) %>% 
    clean_names() %>% 
    filter(metric == {{display_metric}}) %>% 
    dplyr::select(model, protected_feature, group, metric, estimate)
  
  models = metrics_filtered %>% 
    distinct(model) %>% 
    pull()
  
  if (display_metric %in% c("rmse", "mae", "mse", "rse", "mape", "smape")) {
    
    if (length(models) > 1) {
      
      metrics_filtered %>% 
        ggplot(aes(x = reorder(group, -estimate), y = estimate, fill = model)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        geom_text(aes(label = round(estimate, 3)), 
                  hjust = -0.2, position = position_dodge(width = 0.9)) +
        ylim(0, 1.05) +
        labs(x = "Protected Groups",, y = display_metric, 
             title = paste("Performance across groups for", protected)) 
      
      
    } else {
      
      metrics_filtered %>% 
        ggplot(aes(x = reorder(group, -estimate), y = estimate)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        geom_text(aes(label = round(estimate, 3)),
                  hjust = -0.2, position = position_dodge(width = 0.9)) +
        ylim(0, 1.05) +
        labs(x = "Protected Groups", y = display_metric, 
             title = paste("Performance across groups for", protected)) 
      
    }
    
  } else {
    
    if (length(models) > 1) {
      
      metrics_filtered %>% 
        ggplot(aes(x = reorder(group, estimate), y = estimate, fill = model)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        geom_text(aes(label = round(estimate, 3)),
                  hjust = -0.2, position = position_dodge(width = 0.9)) +
        ylim(0, 1.05) +
        labs(x = "Protected Groups", y = display_metric, 
             title = paste("Performance across groups for", protected))
      
      
    } else {
      
      metrics_filtered %>% 
        ggplot(aes(x = reorder(group, estimate), y = estimate)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        geom_text(aes(label = round(estimate, 3)),
                  hjust = -0.2, position = position_dodge(width = 0.9)) +
        ylim(0, 1.05) +
        labs(x = "Protected Groups", y = display_metric, 
             title = paste("Performance across groups for", protected)) 
      
    }
    
  }
  
}

# create a plot of error metrics for a particular feature - false_positive = TRUE
# for FPR and false_positive = FALSE for FNR
plot_error_direction = function(df, mode, display_feature, false_positive = TRUE, model, target) {
  
  if (class(df) == "list") {
    
    df_raw = list()
    for (i in 1:length(model)) {
      
      df_raw[[i]] = df[[i]] %>% 
        mutate(model = model[[i]]) %>% 
        relocate(model)
      
    }
    
    df = bind_rows(df_raw)
    
    raw_frame = df %>% 
      pivot_longer(cols = matches("_protected"),
                   names_to = "protected_feature",
                   values_to = "group") %>% 
      relocate(protected_feature, group, everything())
    
    targ_frame = raw_frame %>%
      dplyr::select(all_of(target))
    
    if (mode == "classification") {
    
      score_frame_raw = raw_frame %>% 
        convert_preds(target)
      
      score_frame = score_frame_raw %>% 
        filter(str_detect(protected_feature, display_feature)) %>% 
        mutate(fp_ratio = ((FP)/(FP + TN))*100) %>% 
        mutate(fn_ratio = ((FN)/(FN + TP))*100) %>% 
        dplyr::select(-c(FN, FP, TN, TP)) 
      
      if (false_positive == TRUE) {
        
        score_frame %>%
          filter(str_detect(protected_feature, display_feature)) %>%
          ggplot(aes(reorder(group, fp_ratio), fp_ratio, fill = model)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          geom_text(aes(y = fp_ratio + 0.7*sign(fp_ratio), label = round(fp_ratio, 2), group = model), 
                    hjust = 0,
                    position = position_dodge(width = 0.9)) +
          labs(x = "Protected Groups", y = "False Positive Rate",
               title = paste("False Positive Rate across groups for", display_feature)) 
        
      } else {
        
        score_frame %>%
          filter(str_detect(protected_feature, display_feature)) %>%
          ggplot(aes(reorder(group, fn_ratio), fn_ratio, fill = model)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          geom_text(
            aes(y = fn_ratio + 0.7*sign(fn_ratio), label = round(fn_ratio, 2), 
                group = model), 
            position = position_dodge(width = 0.9),
            hjust = 0) +
          labs(x = "Protected Groups", y = "False Negative Rate",
               title = paste("False Negative Rate across groups for", display_feature)) 
        
      }
      
    } else if (mode == "regression") {
      
      print("Not yet ready")
      
    } else {
      
      stop("mode requires specifying either 'classification' or 'regression' depending upon the type of model ran")
      
    }
    
    
  } else {
    
    df = df %>% 
      mutate(model = model) %>% 
      relocate(model)
    
    raw_frame = df %>% 
      pivot_longer(cols = matches("_protected"),
                   names_to = "protected_feature",
                   values_to = "group") %>% 
      relocate(protected_feature, group, everything())
    
    targ_frame = raw_frame %>%
      dplyr::select(all_of(target))
    
    if (mode == "classification") {
      
      score_frame_raw = raw_frame %>% 
        convert_preds(target)
      
      score_frame = score_frame_raw %>% 
        filter(str_detect(protected_feature, display_feature)) %>% 
        mutate(fp_ratio = ((FP)/(FP + TN))*100) %>% 
        mutate(fn_ratio = ((FN)/(FN + TP))*100) %>% 
        dplyr::select(-c(FN, FP, TN, TP)) 
      
      if (false_positive == TRUE) {
        
        score_frame %>%
          filter(str_detect(protected_feature, display_feature)) %>%
          ggplot(aes(reorder(group, fp_ratio), fp_ratio, fill = model)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          geom_text(
            aes(y = fp_ratio + 0.7*sign(fp_ratio), label = round(fp_ratio, 2), 
                group = model), 
            position = position_dodge(width = 0.9),
            inherit.aes = TRUE) +
          labs(x = "Protected Groups", y = "False Positive Rate",
               title = paste("False Positive Rate across groups for", display_feature))
        
      } else {
        
        score_frame %>%
          filter(str_detect(protected_feature, display_feature)) %>%
          ggplot(aes(reorder(group, fn_ratio), fn_ratio, fill = model)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip() +
          geom_text(
            aes(y = fn_ratio + 0.7*sign(fn_ratio), label = round(fn_ratio, 2), group = model), 
            position = position_dodge(width = 0.9),
            inherit.aes = TRUE) +
          labs(x = "Protected Groups", y = "False Negative Rate",
               title = paste("False Negative Rate across groups for", display_feature))
        
      }
      
    } else if (mode == "regression") {
      
      print("Not yet ready")
      
    } else {
      
      stop("mode requires specifying either 'classification' or 'regression' depending upon the type of model ran")
      
    }
    
  }
  
}

# dummify factor variables 
dummifier = function(data, target, one_hot = TRUE) {
  
  form = paste(paste(target, "~"), str_c(colnames(data %>% dplyr::select(-all_of(target))),
                                         collapse = "+"))
  
  recipe(data, form) %>% 
    step_dummy(all_nominal(), one_hot = one_hot, -target) %>% 
    prep() %>% 
    juice()
  
}

# add predictions from parsnip::predict to an input dataframe
add_preds = function(data, preds, model_mode, event_level, target, threshold = NA) {
  
  if (model_mode == "classification") {
    
    if (is.na(threshold)) {
      
      event_col = preds %>% 
        dplyr::select(paste(".pred", event_level, sep = "_")) %>% 
        colnames()
      
      other_col = preds %>% 
        dplyr::select(!any_of(event_col)) %>% 
        colnames()
      
      non_event_level = str_remove(other_col, ".pred_")
      
      data %>% 
        bind_cols(preds) %>% 
        mutate(pred_class = factor(ifelse(!!sym(event_col) > !!sym(other_col), !!(event_level), !!(non_event_level)),
                                   levels = c(!!(event_level), !!(non_event_level)))) %>%
        clean_names() %>% 
        mutate(pred_class = fct_reorder(pred_class, as.numeric(!!sym(target)))) # maybe works?
      
    } else {
      
      event_col = preds %>% 
        dplyr::select(paste(".pred", event_level, sep = "_")) %>% 
        colnames()
      
      other_col = preds %>% 
        dplyr::select(!any_of(event_col)) %>% 
        colnames()
      
      non_event_level = str_remove(other_col, ".pred_")
      
      data %>% 
        bind_cols(preds) %>% 
        mutate(pred_class = factor(ifelse(!!sym(event_col) > threshold, !!(event_level), !!(non_event_level)))) %>% 
        clean_names()
      
    }
    
  } else if (model_mode == "regression") {
    
    data %>% 
      bind_cols(preds) %>% 
      clean_names()
    
  } else {
    
    stop("model_mode argument only accepts values 'regression' or 'classification'")
    
  }
  
}

# generate iml Predictor objects from fit parsnip models
get_iml_preds = function(fits, data, target) {
  
  lapply(fits, function(x) Predictor$new(x, data = as_tibble(data) %>% dplyr::select(-target), 
                                         y = as_tibble(data) %>% pull(target),
                                         class = ".pred_1",
                                         type = "prob"))
  
}

# plot permutation feature importance from iml predictor objects
plot_permutation_importance = function(preds, model_names, mode = NA) {
  
  if (mode == "classification") {
    
    auc_error = function(actual, predicted) 1 - yardstick::roc_auc_vec(actual, predicted)
      
      imps = lapply(preds, function(x) FeatureImp$new(x, loss = auc_error))
      
      model_names = names(imps)
      
      smol_imps = purrr::map2(imps, model_names, function(x, y) x$results %>% top_n(20) %>% as_tibble() %>% 
                                mutate(model_name = y)) %>%
        bind_rows() %>% 
        filter(feature != 'race_protected')
      
      ggplot(smol_imps, aes(importance, reorder(feature, importance), 
                            group = model_name, color = model_name)) + 
        geom_point(aes(shape = model_name), size = 4, position = position_dodge(1))  +
        geom_errorbarh(aes(xmin = importance.05, xmax = importance.95), size = 1,
                       position = position_dodge(1)) +
        labs(x = paste("Importance measured by AUC Error"), y = "Feature", 
             title = paste("Permutation Feature Importance"))
    
  } else if (mode == "regression") {
    
    if (is.na(metric)) {
      
      imps = lapply(preds, function(x) FeatureImp$new(x, loss = "rmse")) 
      smol_imps = lapply(imps, function(x) x$results %>% top_n(20))
      global_plots = lapply(smol_imps, function(x) ggplot(x, aes(importance, reorder(feature, importance))) + 
                              geom_point(size = 3) +
                              geom_errorbarh(aes(xmin = importance.05, xmax = importance.95), size = 0.3))
      imp_plots = list()
      for (i in 1:length(preds)) {
        imp_plots[[i]] = global_plots[[i]] + 
          labs(x = paste("Importance measured by rmse"), y = "Feature", 
               title = paste("Permutation Feature Importance", model_names[[i]])) 
      }
      
      return(imp_plots)
      
    } else if (metric %in% c("rmse", "mae", "mse", "rse", "mape", "smape")) {
      
      imps = lapply(preds, function(x) FeatureImp$new(x, loss = metric)) 
      smol_imps = lapply(imps, function(x) x$results %>% top_n(20))
      global_plots = lapply(smol_imps, function(x) ggplot(x, aes(importance, reorder(feature, importance))) + 
                              geom_point(aes(size = 7, shape = model_name), position = position_dodge(1))  +
                              geom_errorbarh(aes(xmin = importance.05, xmax = importance.95), size = 0.3))
      imp_plots = list()
      
      for (i in 1:length(preds)) {
        imp_plots[[i]] = global_plots[[i]] + 
          labs(x = paste("Importance measured by", metric), y = "Feature", 
               title = paste("Permutation Feature importance for", model_names[[i]])) 
        
      }
      
      return(imp_plots)
      
    } else {
      
      stop("supported metric arguments for regression are 'rmse', 'mae', 'mase'")
      
    }
    
  } else {
    
    stop("specify whether the underlying model is type 'classification' or 'regression' in the mode argument")
    
  }
  
}

# plot PDP plots from iml Predictor objects
plot_global_interp = function(preds, model_names, display_feature = NULL) {
  
  pdp_ice_data = lapply(preds, function(x) FeatureEffects$new(x, method = "pdp+ice", features = display_feature))
  
  pdp_ice_data_cleaned = list()
  for (i in 1:length(pdp_ice_data)) { 
    pdp_ice_data_cleaned[[i]] = pdp_ice_data[[i]]$results %>% 
      data.frame() %>% 
      as_tibble() %>% 
      dplyr::select(1:4) %>% 
      dplyr::rename(feature_val = 1,
                    y_hat = 2,
                    type = 3,
                    id = 4) %>% 
      arrange(feature_val) %>% 
      mutate(model = model_names[[i]]) 
    
  }
 
  pdp_ice_data_cleaned %>% 
    bind_rows() %>% 
    filter(type == "pdp") %>% 
    ggplot(aes(feature_val, y_hat)) + 
      geom_line(aes(feature_val, y_hat, group = model, color = model), size = 2) +
      ylim(0, 1) +
      labs(x = "Feature Value", y = "Probability", 
           title = paste("PDP Plot for models on Feature", display_feature)) 
  
  
} 

# get observations from specific protected groups within an iml 
# Predictor object
get_observations = function(iml_pred, protected, num_obs) {
  
  iml_pred$data$X %>% 
    as_tibble() %>% 
    group_by({{protected}}) %>% 
    group_split() %>% 
    purrr::map(sample_n, num_obs)
  
}

# internal function for fairness_framer
convert_preds = function(raw_frame, target) {
  
  raw_frame %>% 
    group_by(model, protected_feature, group) %>% 
    conf_mat({{target}}, pred_class) %>% 
    mutate(conf_mat = purrr::map(conf_mat, tidy)) %>%
    unnest(conf_mat) %>% 
    ungroup() %>% 
    mutate(name = case_when(
      name == "cell_1_1" ~ "TP",
      name == "cell_1_2" ~ "FP",
      name == "cell_2_1" ~ "FN",
      name == "cell_2_2" ~ "TN"
    )) %>% 
    pivot_wider(names_from = name,
                values_from = value)
  
}

# internal function for gen_fairness_metrics
fairness_framer = function(df, privileged_class, target, targ_frame, mode) {
  
  if (mode == "classification") {
    
    score_frame_raw = df %>% 
      convert_preds(target)
    
    frame_privileged = score_frame_raw %>% 
      filter(str_detect(group, privileged_class)) %>% 
      mutate(p_demographic_parity = (TP+FP)) %>% 
      mutate(p_equal_opportunity = (TP)/(TP+FP)) %>% 
      mutate(p_spec = (TN)/(TN+FP)) %>% 
      mutate(p_error_rate = (FP+FN)/(TP+FP+TN+FN)) %>% 
      mutate(p_accuracy = ((TP+TN)/(TP+FP+TN+FN))) %>% 
      mutate(p_FDR = (FP)/(TP+FP)) %>% 
      mutate(p_FOR = (FN)/(TN+FN)) %>% 
      mutate(p_tpr = ((TP)/(TP+FP))) %>% 
      mutate(p_fpr = ((FP)/(FP+TN))) %>% 
      mutate(p_fnr = ((FN)/(FN+TP))) %>%
      mutate(p_tnr = ((TN)/(TN+FP))) %>%
      dplyr::select(-c(FN, FP, TN, TP, group))
    
    frame_final = score_frame_raw %>% 
      left_join(frame_privileged, by = c(
        "model" = "model",
        "protected_feature" = "protected_feature"
      )) %>% 
      mutate(raw_demographic_parity = (TP+FP)) %>% 
      mutate(raw_equal_opportunity = (TP)/(TP+FP)) %>% 
      mutate(raw_spec = (TN)/(TN+FP)) %>% 
      mutate(raw_error_rate = (FP+FN)/(TP+FP+TN+FN)) %>% 
      mutate(raw_accuracy = (TP+TN)/(TP+FP+TN+FN)) %>% 
      mutate(raw_FDR = (FP)/(TP+FP)) %>% 
      mutate(raw_FOR = (FN)/(TN+FN)) %>% 
      mutate(raw_tpr = ((TP)/(TP+FN))) %>% 
      mutate(raw_fpr = ((FP)/(FP+TN))) %>% 
      mutate(raw_fnr = ((FN)/(FN+TP))) %>%
      mutate(raw_tnr = ((TN)/(TN+FP))) %>%
      mutate(demographic_parity = (raw_demographic_parity - p_demographic_parity)/(p_demographic_parity)*100) %>% 
      mutate(equal_opportunity = (raw_equal_opportunity - p_equal_opportunity)/(p_equal_opportunity )*100) %>% 
      mutate(error_rate = (raw_error_rate - p_error_rate)/(p_error_rate)*100) %>% 
      mutate(accuracy = (raw_accuracy - p_accuracy)/(p_accuracy)*100) %>% 
      mutate(tpr = (raw_tpr - p_tpr)/(p_tpr)*100) %>% 
      mutate(fpr = (raw_fpr - p_fpr)/(p_fpr)*100) %>%
      mutate(tnr = (raw_tnr - p_tnr)/(p_tnr)*100) %>% 
      mutate(fnr = (raw_fnr - p_fnr)/(p_fnr)*100) %>% 
      mutate(FOR = (raw_FOR - p_FOR)/(p_FOR)*100) %>% 
      mutate(FDR = (raw_FDR - p_FDR)/(p_FDR)*100) %>% 
      mutate(spec = (raw_spec - p_spec)/(p_spec)*100) %>% 
      mutate(equalized_odds = abs(raw_tpr - p_tpr) + abs(raw_tpr - p_tpr)) %>% 
      mutate(privileged = ifelse(group == privileged_class, TRUE, FALSE)) %>% 
      dplyr::select(-c(p_demographic_parity, p_error_rate, p_accuracy, p_tpr, p_fpr, p_tnr, p_fnr, p_FDR, p_FOR, p_spec,
                       p_equal_opportunity, raw_demographic_parity, raw_error_rate, raw_accuracy, raw_tpr, raw_tnr, raw_spec,
                       raw_equal_opportunity, raw_fpr, raw_fnr, raw_FOR, raw_FDR)) 
    
  } else {
    
    internal_metrics = metric_set(rmse, mae, mase)
    
    score_frame_raw = df %>% 
      group_by(model, protected_feature, group) %>% 
      internal_metrics("assaults", estimate = "pred") %>% 
      rename_with(rm_period) %>% 
      select(-estimator)
    
    
    frame_privileged = score_frame_raw %>% 
      filter(str_detect(group, privileged_class)) %>% 
      dplyr::select(-group) %>% 
      rename_with(rm_period) %>% 
      rename(p_estimate = estimate)
    
    frame_final = score_frame_raw %>% 
      mutate(privileged = ifelse(str_detect(group, privileged_class), TRUE, FALSE)) %>%
      left_join(frame_privileged, by = c(
        "model" = "model",
        "protected_feature" = "protected_feature",
        "metric" = "metric"
      )) %>% 
      mutate(estimate = estimate/p_estimate) %>% 
      select(-p_estimate)
    
  }
  
}

# calculate fairness metrics within each model across protected groups
gen_fairness_metrics = function(df, model, target, mode, protected, privileged = NA) {
  
  if (is.na(privileged)) {
    
    stop("must specify a privileged group to calculate fairness metrics, e.g privileged = 'Caucasian'")
    
  }
  
  if (class(df) == "list") {
    
    df_raw = list()
    for (i in 1:length(model)) {
      
      df_raw[[i]] = df[[i]] %>% 
        mutate(model = model[[i]]) %>% 
        relocate(model)
      
    }
    
    df = bind_rows(df_raw)
    
    if (mode == "classification") {
      
      # get a list of dataframes for each protected feature
      holder = list()
      for (i in 1:length(protected)) {
        
        holder[[i]] = df %>% 
          dplyr::select(matches(c(protected[[i]], "pred_|model", target)))
        
      }
      
      # make the protected feature a value in column (group) rather than its own column
      raw_frames = purrr::map(holder, function(x) 
        x %>% 
          pivot_longer(cols = matches("_protected"),
                       names_to = "protected_feature", 
                       values_to = "group") %>% 
          relocate(protected_feature, group, everything()))
      
      # get target variable values
      targ_frame = raw_frames[[1]] %>%
        dplyr::select(all_of(target))
      
      # calculate confusion matrix results and fairness metrics, pivot so one metric per row
      purrr::map2(raw_frames, privileged, fairness_framer, target, targ_frame, mode) %>% 
        bind_rows() %>% 
        dplyr::select(-c(FN, FP, TN, TP)) %>% 
        dplyr::relocate(model, privileged, everything()) %>% 
        pivot_longer(cols = c(equalized_odds, equal_opportunity, spec, demographic_parity, error_rate,
                              accuracy, tpr, fpr, tnr, fnr, FDR, FOR),
                     names_to = "metric",
                     values_to = "estimate")
      
      
    } else if (mode == "regression") {
      
      holder = list()
      for (i in 1:length(protected)) {
        
        holder[[i]] = df %>% 
          dplyr::select(matches(c("_protected|pred|model", target))) %>% 
          dplyr::select(matches(c(protected[[i]], "pred|model", target)))
        
      }
      
      
      raw_frames = purrr::map(holder, function(x) 
        x %>% 
          pivot_longer(cols = matches("_protected"),
                       names_to = "protected_feature", 
                       values_to = "group") %>% 
          relocate(protected_feature, group, everything()))
      
      targ_frame = raw_frames[[1]] %>%
        dplyr::select(all_of(target))
      
      purrr::map2(raw_frames, privileged, fairness_framer, target, targ_frame, mode, val_interest) %>%
        bind_rows() %>%
        dplyr::relocate(model, privileged, everything()) 
      
    } else {
      
      stop("mode requires entering a value of 'classification' or 'regression'")
      
    }
    
  } else {
    
    df = df %>% 
      mutate(model = model) %>% 
      relocate(model)
    
    if (mode == "classification") {
      
      # get a list of dataframes for each protected feature
      
      holder = list()
      for (i in 1:length(protected)) {
        
        holder[[i]] = df %>% 
          dplyr::select(matches(c(protected[[i]], "pred_|model", target)))
        
      }
      
      # make the protected feature a value in column (group) rather than its own column
      raw_frames = purrr::map(holder, function(x) 
        x %>% 
          pivot_longer(cols = matches("_protected"),
                       names_to = "protected_feature", 
                       values_to = "group") %>% 
          relocate(protected_feature, group, everything()))
      
      # get target variable values
      targ_frame = raw_frames[[1]] %>%
        dplyr::select(all_of(target))
      
      # calculate confusion matrix results and fairness metrics, pivot so one metric per row
      purrr::map2(raw_frames, privileged, fairness_framer, target, targ_frame, mode) %>% 
        bind_rows() %>% 
        dplyr::select(-c(FN, FP, TN, TP)) %>% 
        dplyr::relocate(model, privileged, everything()) %>% 
        pivot_longer(cols = c(equalized_odds, equal_opportunity, spec, demographic_parity, error_rate,
                              accuracy, tpr, fpr, tnr, fnr, FDR, FOR),
                     names_to = "metric",
                     values_to = "estimate")
      
      
    } else if (mode == "regression") {
      
      holder = list()
      for (i in 1:length(protected)) {
        
        holder[[i]] = df %>% 
          dplyr::select(matches(c("_protected|pred|model", target))) %>% 
          dplyr::select(matches(c(protected[[i]], "pred|model", target)))
        
      }
      
      
      raw_frames = purrr::map(holder, function(x) 
        x %>% 
          pivot_longer(cols = matches("_protected"),
                       names_to = "protected_feature", 
                       values_to = "group") %>% 
          relocate(protected_feature, group, everything()))
      
      targ_frame = raw_frames[[1]] %>%
        dplyr::select(all_of(target))
      
      purrr::map2(raw_frames, privileged, fairness_framer, target, targ_frame, mode, val_interest) %>%
        bind_rows() %>%
        dplyr::relocate(model, privileged, everything()) 
      
    } else {
      
      stop("mode requires entering a value of 'classification' or 'regression'")
      
    }
    
  }
  
}

# plota a dataframe of fairness metrics generated by gen_fairness_metrics
plot_fairness_metrics = function(group_metrics, display_feature, metric) {
  
  group_levels = group_metrics %>% 
    filter(str_detect(protected_feature, display_feature)) %>% 
    summarize(num = n_distinct(group)) %>% 
    pull(num)
  
  privileged = group_metrics %>% 
    filter(str_detect(protected_feature, display_feature)) %>% 
    filter(privileged == TRUE) %>% 
    distinct(group) %>% 
    mutate(group = as.character(group)) %>% 
    pull()
  
  if (group_levels == 2) {
    
    metrics_filtered = group_metrics %>% 
      filter(str_detect(protected_feature, display_feature)) %>% 
      filter(privileged == FALSE) %>% 
      filter(metric == {{metric}}) %>% 
      dplyr::select(model, privileged, protected_feature, group, metric, estimate)
    
    models = metrics_filtered %>% 
      distinct(model) 
    
    if (nrow(models) > 1) {
      
      metrics_filtered %>% 
        ggplot(aes(x = reorder(group, estimate), y = estimate, fill = model)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        geom_text(aes(y = estimate + 0.015*sign(estimate), label = paste0(round(estimate, 2), "%")), 
                  position = position_dodge(width = 0.9),
                  hjust = ifelse(metrics_filtered$estimate < 0, 1, -0.01)) +
        labs(x = "Protected Groups",  y = paste("Difference in", metric, "relative to privileged group", privileged),
             title = paste("Model fairness across", display_feature, "groups by", metric)) 
      
    } else {
      
      metrics_filtered %>%
        ggplot(aes(x = reorder(group, estimate), y = estimate)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        ylim(-3, 3) +
        geom_text(aes(y = estimate + 0.015*sign(estimate), label = paste0(round(estimate, 2), "%")), 
                  position = position_dodge(width = 0.9),
                  hjust = ifelse(metrics_filtered$estimate < 0, 1, -0.01)) +
        labs(x = "Protected Groups", y = paste("Difference in", metric, "relative to privileged group", privileged),
             title = paste("Model fairness across", display_feature, "groups by", metric)) 
      
    }
    
    
  } else if (group_levels > 2) {
    
    metrics_filtered = group_metrics %>% 
      filter(str_detect(protected_feature, display_feature)) %>% 
      filter(privileged == FALSE) %>% 
      filter(metric == {{metric}}) %>% 
      dplyr::select(model, privileged, group, metric, estimate)
    
    models = metrics_filtered %>% 
      distinct(model) 
      
      metrics_filtered %>% 
        ggplot(aes(x = reorder(group, -estimate), y = estimate, fill = model)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        geom_text(aes(y = estimate + 0.015*sign(estimate), label = paste0(round(estimate, 2), "%")), 
                  position = position_dodge(width = 0.9),
                  hjust = ifelse(metrics_filtered$estimate < 0, 1, -0.01)) +
        labs(x = "Protected Groups", y = paste("Difference in", metric, "relative to privileged group", privileged), 
             title = paste("Model fairness across", display_feature, "groups by", metric))
    
  } else {
    
    stop("ensure that the examined group contains at least 2 unique sub-groups")
    
  } 
  
}

# internal function to plot_model_thresholds
per_group_thresholding = function(df, target, prob_event, threshold_sequence, protected, metrics) {
  
  result_list = list()
  for (j in 1:length(threshold_sequence)) {
    
    target_vec = df %>% 
      dplyr::select(target) %>% 
      pull()
    
    estimate_vec = df %>% 
      mutate(pred_class = factor(ifelse(df %>% dplyr::select(prob_event) > threshold_sequence[[j]], 1, 0),
                                 levels = c("1", "0"))) %>% 
      pull(pred_class)
    
    result_list[[j]] = df %>% 
      metrics(truth = target_vec, estimate = estimate_vec) %>%
      mutate(thresh = threshold_sequence[[j]]) %>% 
      mutate(protected_group = protected)
    
  }
  
  result_list
  
}

# plot of performance metrics by protected group for multiple possible thresholds
plot_model_thresholds = function(pred_data, target, prob_event, protected, metrics, 
                                 threshold_sequence = as.list(seq(0, 1, 0.02))) {
  
  result_list = list()
  
  protected_labels = pred_data %>% 
    dplyr::distinct(across(all_of(protected))) %>% 
    dplyr::arrange(across(all_of(protected))) %>% 
    pull(protected) 
  
  df_list = pred_data %>% 
    dplyr::select(all_of(c(protected, target, prob_event))) %>% 
    dplyr::arrange(across(all_of(protected))) %>% 
    dplyr::group_by(across(all_of(protected))) %>% 
    group_split()
  
  slug_tibble = tibble(df = df_list,
                       target = list(target),
                       prob_event = list(prob_event),
                       threshold_sequence = list(threshold_sequence),
                       protected = protected_labels,
                       metrics = list(metrics))
  
  result_list = purrr::pmap(slug_tibble, per_group_thresholding) %>% 
    bind_rows()
  
  result_list %>%
    bind_rows() %>%
    ggplot(aes(thresh, .estimate)) +
    geom_line(aes(color = .metric), size = 2) +
    facet_wrap(~protected_group) +
    labs(x = "Probability threshold for event", y = "Metric estimate", 
         title = "Plot of Performance Metric Tradeoffs by Threshold",
         color = "Metric") 
  
}

# calculate optimal classification thresholds by Youden's J
gen_optimal_thresholds = function(df, protected, target, prob_event) {
  
  protected_labels = df %>% 
    dplyr::distinct(across(all_of(protected))) %>% 
    dplyr::arrange(across(all_of(protected))) %>% 
    pull(protected)
  
  df_list = df %>% 
    dplyr::select(all_of(c(protected, target, prob_event))) %>% 
    dplyr::arrange(across(all_of(protected))) %>% 
    dplyr::group_by(across(all_of(protected))) %>% 
    group_split()
  
  threshold_frame = purrr::map(df_list, function(x)
    x %>%
      roc_curve(.data[[target]], .data[[prob_event]]) %>%
      mutate(j_stat = sensitivity + specificity - 1) %>%
      arrange(-j_stat) %>%
      top_n(1) %>% 
      dplyr::select(j_stat) 
  ) %>% 
    bind_rows() %>% 
    mutate(group = protected_labels) %>% 
    relocate(group)
  
  return(threshold_frame)
  
}

# internal function to gen_mitigated_preds
per_group_thresholds = function(df_group, threshold_list, prob_event) {
  
  df_group %>% 
    mutate(pred_class = factor(ifelse(df_group %>% dplyr::select(all_of(prob_event)) > threshold_list, 1, 0), 
                               levels = c("1", "0")))
  
}

# create new predictions based upon user-supplied classification thresholds
gen_mitigated_preds = function(df, threshold_frame = NA, threshold_list = NA, protected, prob_event) {
  
  if (is.na(threshold_frame) & is.na(threshold_list)) {
    
    stop("need to provide a tibble of thresholds from gen_optimal_thresholds or a list of numerics corresponding
         to desired thresholds for each group")
    
  } else if (!is.na(threshold_frame) & is.na(threshold_list)) {
    
    threshold_list = threshold_frame %>% 
      pull(j_stat)
    
    df %>% 
      group_by(across(all_of(protected))) %>% 
      group_split() %>% 
      purrr::map2(threshold_list, per_group_thresholds, prob_event) %>% 
      bind_rows()
    
  } else if (is.na(threshold_frame) & !is.na(threshold_list)) {
    
    df %>% 
      dplyr::arrange(across(all_of(protected))) %>% 
      group_by(across(all_of(protected))) %>% 
      group_split() %>% 
      purrr::map2(threshold_list, per_group_thresholds, prob_event) %>% 
      bind_rows()
    
  } else {
    
    stop("provide a valid tibble for threshold_frame or list for threshold_list")
    
  }
  
}

# create and fit a parsnip specification for each protected group 
gen_group_model = function(model, df, protected_group) {
  
  protected_levels = df %>% 
    dplyr::select(all_of(protected_group)) %>% 
    distinct(across(all_of(protected_group))) %>% 
    dplyr::arrange(across(all_of(protected_group))) %>% 
    pull() %>%
    str_split(pattern = " ")
  
  df %>% 
    filter(across(all_of(protected_group), ~ . == protected_levels[[2]]))
  
  purrr::map(protected_levels, function(x)
    model %>% 
      fit(df %>% filter(across(all_of(protected_group), ~ . == x)))
  )
  
}