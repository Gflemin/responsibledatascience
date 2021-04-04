#  title: "ch2_functions.R"
#  date: 04/03/2021
#  author: Grant Fleming
#  purpose: functions for ch2_script.Rmd 

# function to do light preprocessing and feature selection by importance
preprocess_data = function(df_raw, split_prop, target, short = TRUE, seed = 10, id = NA) {
  
  # remove stochastic variability
  set.seed(seed)
  
  # use rsample::initial_split() to split data
  data_split = df_raw %>%
    initial_split(prop = split_prop)
  
  # pull out our train and test data from our rsplit object, data_split
  training_data = training(data_split)
  testing_data = testing(data_split)
 
  # generate our important features 
  imp_feats = Boruta(training(data_split), training(data_split) %>% select(all_of(target)) %>% pull())
  
  # get a list of our important features
  imp_confirmed = enframe(imp_feats$finalDecision) %>%     
    filter(value == "Confirmed" | value == "Tentative") %>%
    mutate(name = str_remove(name, "train_data\\.")) %>%
    pull(name)     
  
  ### now, filter raw data by correct vars and re-split
  
  data_split = df_raw %>% 
    select(all_of(imp_confirmed)) %>% 
    initial_split(prop = split_prop)
  
  # pull out our train and test data from our rsplit object, data_split
  training_data = training(data_split)
  testing_data = testing(data_split)
  
  if (short == TRUE) {
    
    # get col types
    types = sapply(training_data, class) %>% 
      enframe()
    
    # create recipe and make changes to data
    if (!is.na(any(types$value) == "factor")) {
      rec = recipe(as.formula(str_c(target, str_c(colnames(training_data), collapse = "+"), sep = "~")),
                   data = training_data) %>%
        step_zv(all_predictors()) %>%
        step_string2factor(all_nominal(), -all_outcomes()) %>% # will cause errors if no character vars in input data
        prep()
    } else {
      rec = recipe(as.formula(str_c(target, str_c(colnames(training_data), collapse = "+"), sep = "~")),
                   data = training_data) %>%
        step_zv(all_predictors()) %>%
        prep()
    }
    
    # get changed training data and test data
    df = juice(rec) 
    test_df = bake(rec, assessment(data_split))
    
    # return a list of train and test data
    list("train" = df,
         "test" = test_df)
    
  } else {
    
    ### further down-select variables using step-wise selection
    
    # build a linear model
    mod = lm(as.formula(str_c(target, str_c(colnames(training_data), collapse = "+"), sep = "~")),
             data = training_data)
    
    # do stepwise selection (using AIC) in both directions, pull out terms
    mod_stepwise = stats::step(mod, direction = "both") %>% 
      tidy() %>% 
      filter(!str_detect(tolower(term), "intercept")) %>% 
      pull(term) %>% 
      c(target)
    
    # get new training data
    new_training_data = training_data %>% 
      select(all_of(mod_stepwise))
    
    # get col types
    types = sapply(new_training_data, class) %>% 
      enframe()
    
    # create recipe and make changes to data
    if (!is.na(any(types$value) == "factor")) {
      rec = recipe(as.formula(str_c(target, str_c(colnames(new_training_data), collapse = "+"), sep = "~")),
                   data = new_training_data) %>%
        step_zv(all_predictors()) %>%
        step_normalize(all_numeric(), -all_outcomes()) %>% 
        step_string2factor(all_nominal(), -all_outcomes()) %>% # will cause errors if no character vars in input data
        prep()
    } else {
      rec = recipe(as.formula(str_c(target, str_c(colnames(new_training_data), collapse = "+"), sep = "~")),
                   data = new_training_data) %>%
        step_zv(all_predictors()) %>%
        step_normalize(all_numeric(), -all_outcomes()) %>% 
        prep()
    }
    
    # get changed training data and test data
    df = juice(rec)
    test_df = bake(rec, assessment(data_split))
    
    # return a list of train and test data
    list("train" = df,
         "test" = test_df)
    
  }
  
}


# boof = lm(G3 ~., data = data[[1]])
# hey = stats::step(boof, direction = "both") %>% 
#   tidy() %>% 
#   filter(!str_detect(tolower(term), "intercept")) %>% 
#   pull(term)
# 
# target = "G3"
# 
# data[[1]] %>% 
#   select(all_of(c(target, hey)))
# 
# 
