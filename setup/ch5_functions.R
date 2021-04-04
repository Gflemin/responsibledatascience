#  title: "ch5_functions.R"
#  date: 04/03/2021
#  author: Grant Fleming
#  purpose: functions for ch5_script.Rmd 

# function to generate a dataframe of possible models for the user to select.
# all require the parsnip package from tidymodels. should be ran at the start
# of each new session if modeling needs to be done later
gen_model_frame = function() {
  
  tibble(name = list("linear_regression", "logistic_regression", "svm_rbf", "nearest_neighbor",
                     "decision_tree", "random_forest", "boosted_trees", "mars"),
         model_function = list(linear_reg(), logistic_reg(), svm_rbf(), nearest_neighbor(),
                               decision_tree(), rand_forest(), boost_tree(), mars()),
         model_engine = list("lm", "glm", "kernlab", "kknn", "rpart", "ranger", "xgboost", "earth")) %>%
    unnest(c(name, model_engine))
  
}
model_frame = gen_model_frame()

# internal function for gen_mods
modder = function(model_function, model_engine, mode) {
  
  model_function %>%
    set_engine(model_engine) %>%
    set_mode(mode)
  
}

# generate parsnip model specifications according to names vector input by user
gen_mods = function(model_frame, models, mode = "classification") {
  
  if (is.na(mode)) {
    stop("Need to declare whether model is of type 'regression' or 'classification'")
  }  
  
  interim = model_frame %>%
    filter(name %in% models) %>%
    mutate(mode = mode) %>%
    select(-name)
  
  pmap(interim, modder)
  
}

# function to preprocess data (feature engineering)
preprocessor = function(df_raw, seed = 10) {
  
  # remove stochastic variability
  set.seed(10)
  
  # use rsample::initial_split() to split data
  data_split = df_raw %>%
    initial_split(prop = 0.7)
  
  # pull out our train and test data from our rsplit object, data_split
  training_data = training(data_split)
  testing_data = testing(data_split)
  
  # generate our important features
  imp_feats = Boruta(training(data_split), training(data_split)$Class)
  
  # get a list of our important features
  imp_confirmed = enframe(imp_feats$finalDecision) %>%
    filter(value == "Confirmed") %>%
    mutate(name = str_remove(name, "train_data\\.")) %>%
    pull(name)
  
  # subset the data in our training split to only important features
  df_imps = training_data %>%
    dplyr::select(all_of(imp_confirmed))
  
  # wrap our data in a recipe call and specify our preprocessing steps
  credit_rec = recipe(Class ~., data = df_imps) %>%
    step_zv(all_predictors()) %>%
    step_center(Duration, Amount) %>%
    step_scale(Duration, Amount, factor = 2)
  
  # formalize our preprocessing steps
  credit_rec_prep = credit_rec %>%
    prep()
  
  # preprocess our train data by "baking" it with our prepped recipe
  train_data = bake(credit_rec_prep, training(data_split))
  
  # preprocess our test data by "baking" it with our prepped recipe
  test_data = bake(credit_rec_prep, testing(data_split))
  
  # return a list containing our two pre-processed datasets
  list("train" = train_data,
       "test" = test_data,
       "features" = imp_confirmed,
       "recipe" = credit_rec)
  
}

# function to generate a workflows workflow object from a parsnip model
# and recipes recipe
gen_workflow = function(model, recipe) {
  
  workflows::workflow() %>%
    add_model(model) %>%
    add_recipe(recipe)
  
}

# generate resamples
gen_resample = function(model, recipe, resample, metric, name) {
  
  fit_resamples(
    model,
    preprocessor = recipe,
    resamples = resample,
    control = control_resamples(extract = function(x) extract_model(x)),
    metrics = metric_set(metric)
  ) %>%
    mutate(model = name) %>%
    dplyr::select(model, everything())
  
}

# create a dataframe that stores resampling specifications
gen_map_frame = function(df, boots, mods, metric, model_names) {
  
  map_frame = tibble(model = mods,
                     recipe = rep(list(df[["recipe"]]), length(mods)),
                     resample = rep(list(boots), length(mods)),
                     metric = rep(list(metric), length(mods)),
                     name = model_names)
  
}

# internal function to benchmarker
gen_means = function(resamples, bounding_models) {
  
  resample_means = map(resamples, resample_cleaner) %>%
    bind_rows()
  
  bounding_means = map(bounding_models, bounding_cleaner) %>%
    bind_rows()
  
  bind_rows(resample_means, bounding_means) %>%
    mutate(estimate = round(estimate, 3)) %>%
    arrange(estimate)
  
}

# internal function to gen_means
resample_cleaner = function(resample) {
  
  resample %>%
    collect_metrics() %>%
    janitor::clean_names() %>%
    dplyr::select(-std_err, -n, -estimator) %>%
    rename(estimate = mean)
  
}

# internal function to bounding_cleaner
bounding_cleaner = function(bounding) {
  
  bounding %>%
    mutate(estimate = mean(estimate)) %>%
    slice(1)
  
}

# create baseline featureless and optimal performance "models" off of
# the user-supplied data and resample results
gen_bounding = function(df, target, f_less, boots, optimal, optimal_mean, optimal_sd) {
  
  bounding_models = list()
  holder = map(pull(boots[, 1]), analysis) %>% 
    map(., as_tibble)
  
  if (!is.na(f_less) | !(is.na(optimal))) {
    if (!is.na(f_less)) {
      bounding_models[["f_less"]] = map_df(holder, gen_featureless, df, target)
    }
    if (!is.na(optimal)) {
      bounding_models[["optimal"]] = gen_optimal(optimal_mean, optimal_sd)
    }
  }
  
  return(bounding_models)
  
}

# internal function for gen_t_tests
resample_extractor = function(resample) {
  
  resample %>%
    dplyr::select(-.notes, -id, -splits) %>%
    unnest(.metrics) %>%
    clean_names() %>%
    pull(estimate)
  
}

# internal function for gen_t_tests
resample_name_extractor = function(resample) {
  
  temp_name = resample %>%
    distinct(model) %>%
    pull()
  
}

# internal function for gen_t_tests
two_test_interp = function(v1, v2) {
  
  t.test(v1, v2) %>%
    tidy() %>%
    dplyr::select(p.value) %>%
    rename(pvalue_interp = p.value) %>%
    mutate(pvalue_interp = ifelse(pvalue_interp == 1, NA, pvalue_interp)) %>%  # need to case_when this
    mutate(pvalue_interp = round(pvalue_interp, 3))
  
}

# internal function for gen_t_tests
two_test_bbox = function(v1, v2) {
  
  t.test(v1, v2) %>%
    tidy() %>%
    dplyr::select(p.value) %>%
    rename(pvalue_bbox = p.value) %>%
    mutate(pvalue_bbox = ifelse(pvalue_bbox == 1, NA, pvalue_bbox)) %>%  # need to case_when this
    mutate(pvalue_bbox = round(pvalue_bbox, 3))
  
}

# function to test for significant differences (via t-tests)
# between mean resample performance metrics of models and 
# their associated bounding models 
gen_t_tests = function(means, bounding_models, resamples) {
  
  bounding_set = map(bounding_models, pull, estimate)
  
  resample_names = map(resamples, resample_name_extractor)
  names(resamples) = resample_names
  resample_set = map(resamples, resample_extractor)
  full_set = append(bounding_set, resample_set)
  
  interp_frame = map(full_set, two_test_interp, resample_set[[1]]) %>%
    bind_rows() %>%
    mutate(model = names(full_set))
  
  bbox_frame = map(full_set, two_test_bbox, resample_set[[2]]) %>%
    bind_rows() %>%
    mutate(model = names(full_set))
  
  df = means %>%
    left_join(interp_frame, by = "model") %>%
    left_join(bbox_frame, by = "model") %>% 
    select(-config)
  
  return(df)
  
}

# internal function for plot_baseline_models
metric_selector = function(resample) {
  
  resample %>%
    unnest(.metrics) %>%
    dplyr::select(model, .metric, .estimate) %>%
    clean_names()
  
}

# plots boxplots of performance (by display_metric) for bounding models
# and resample results
gen_baseline_plot = function(bounding_models, resamples, full_plot = FALSE) {
  
  scores = map(resamples, metric_selector) %>%
    bind_rows() %>%
    arrange(model) %>%
    mutate(estimate = round(estimate, 3))
  
  if (full_plot == FALSE) {
    
    scores %>%
      ggplot(aes(x = model, y = estimate, color = model)) +
      geom_boxplot() +
      labs(x = "Model", y = "Mean Performance (Accuracy)", title = "Two-Measure Performance Baseline")  +
      theme(legend.position = "none")
    
  } else {
    
    scores = bind_rows(bounding_models[["f_less"]], scores, bounding_models[["optimal"]]) %>%
      group_by(model) %>%
      mutate(estimate = round(mean(estimate), 3)) %>%
      distinct()
    
    scores %>%
      ggplot(aes(x = reorder(model, estimate), y = estimate, fill = model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = estimate), vjust = -0.3) +
      ylim(0, 1.05) +
      labs(x = "Model", y = "Mean Performance (Accuracy)", title = "Four-Measure Performance Baseline")  +
      theme(legend.position = "none")
    
  }
  
}


# function to generate bootstrap resamples from our original data
gen_boots = function(df_raw, target, features, num) {
  
  boots = df_raw %>%
    dplyr::select(all_of(target_name), all_of(features)) %>%
    bootstraps(times = num)
  
}

# function to estimate featureless performance
gen_featureless = function(holder, df, target) {
  
  holder %>% 
    dplyr::select(target) %>%
    group_by(.[, 1]) %>%
    summarize(estimate = n()/nrow(df[["train"]]), .groups = "drop_last") %>%
    mutate(estimate = round(estimate, 3)) %>%
    filter(Class == "Bad") %>%
    mutate(model = "f_less",
           metric = "accuracy") %>%
    dplyr::select(model, metric, estimate) %>%
    ungroup()
  
}

# function to generate optimal bayes error
gen_optimal = function(mean = 0.92, sd = 0.02) {
  
  distribution = rnorm(n = 30, mean = mean, sd = sd)
  
  tibble("model" = "optimal",
         "metric" = "accuracy",
         "estimate" = distribution)
  
}

# function that takes an input dataframe and benchmarks the
# predictive performance of various models on the data over
# multiple bootstrap resamples
benchmarker = function(df_raw, num = 30, seed = 10, metric = accuracy, 
                       model_names = NA, target = NA,
                       f_less = TRUE, optimal = TRUE, optimal_mean = 0.9, optimal_sd = 0.02) {
  
  set.seed(seed)
  
  df = preprocessor(df_raw)
  
  mods = gen_mods(model_frame, model_names)
  
  boots = gen_boots(df_raw, target, df[["features"]], num)
  
  map_frame = gen_map_frame(df, boots, mods, metric, model_names)
  
  resamples = pmap(map_frame, gen_resample)
  
  bounding_models = gen_bounding(df, target, f_less, boots, optimal, optimal_mean, optimal_sd)
  
  means = gen_means(resamples, bounding_models)
  
  metrics = gen_t_tests(means, bounding_models, resamples)
  
  two_plot = gen_baseline_plot(bounding_models, resamples, full_plot = FALSE)
  
  four_plot = gen_baseline_plot(bounding_models, resamples, full_plot = TRUE)
  
  # bind together and return
  list("metrics" = metrics,
       "resamples" = resamples,
       "two_plot" = two_plot,
       "four_plot" = four_plot)
  
}


