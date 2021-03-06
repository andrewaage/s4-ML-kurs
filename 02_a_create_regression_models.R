
library(parsnip)
library(yardstick)

# Linear model ------------------------------------------------------------

# 1. Create recipe
# Note: This should be done for each model seperately as different models require
# different techniques for optimal feature engineering.
lm_recipe <- recipe(ad_tot_price ~. , data = finn_train_raw) %>% 
  step_mutate(ad_home_type  = fct_lump(ad_home_type, 4),
              ad_owner_type = fct_lump(ad_owner_type, 3),
              bedrooms_missing = is.na(ad_bedrooms)) %>%
  step_medianimpute(ad_bedrooms) %>%
  step_rm(ad_id) %>% 
  prep()

finn_train <- bake(lm_recipe, finn_train_raw)
finn_test  <- bake(lm_recipe, finn_test_raw)

linear_mod <- linear_reg() %>%
  set_engine("lm") %>%
  fit(
    ad_tot_price ~ 
      ad_owner_type
    + ad_home_type
    + ad_bedrooms
    + ad_sqm
    + ad_expense
    + avg_income
    + ad_built
    + bedrooms_missing,
    data = finn_train
  )

# View summary
summary(linear_mod$fit)

prediction <- predict(linear_mod, finn_test) %>%
  bind_cols(finn_test_raw) %>%
  rename(estimate     = .pred,
         truth        = ad_tot_price) %>%
  mutate(
    dev = truth - estimate,
    abs_dev = abs(truth - estimate),
    abs_dev_perc = abs_dev / truth
  )

# Evaluate model
multi_metric <- metric_set(mape, rmse, mae, rsq)

prediction %>%
  multi_metric(truth = truth, estimate = estimate)

# Check out the pdp-plot
# Note how unrealistic this is - it will just keep increasing...
linear_mod$fit %>%
  pdp::partial(pred.var = "avg_income", train = finn_train) %>%
  autoplot() +
  theme_light()

# Linear model exercises ---------------------------------------------------------------

# 1
# Add "kommune_name" to the model. What happens? Why can this be dangerous
# when calling the "predict"-function?

# 2
# Use step_other on the "kommune_name"-variable. Set a reasonable threshold-value.

# 3
# Change the "built" variable to "years_since_built". 
# Why does this not help your model at all?

# 4
# The model removes 2423 observations. Find out why and fix the problem!


# Random forest -----------------------------------------------------------

# We go the "opposite" way here: instead of specyfying which variables
# we want to include, we specify which variables we don't want to include.
# Note: The ranger-algorithm does not allow missing values by default
rf_recipe <- recipe(ad_tot_price ~. , data = finn_train_raw) %>% 
  step_mutate(ad_home_type  = fct_lump(ad_home_type, 4),
              ad_owner_type = fct_lump(ad_owner_type, 3),
              bedrooms_missing = is.na(ad_bedrooms)) %>%
  step_medianimpute(all_numeric()) %>%
  step_modeimpute(all_nominal()) %>% 
  step_rm(ad_id, 
          ad_price,
          ad_tot_price_per_sqm,
          ad_debt, 
          kommune_no, 
          kommune_name, 
          fylke_name, 
          zip_no, 
          zip_name) %>% 
  prep()

finn_train <- bake(rf_recipe, finn_train_raw)
finn_test  <- bake(rf_recipe, finn_test_raw)

# Note: in engine call we can send in ranger-specific arguments,
# e.g. to specify that we want importance. See ?ranger for more options
ranger_mod <- rand_forest(mode = "regression", trees = 200) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  fit(ad_tot_price ~ ., data = finn_train)

# Here we use the "dot"-formula: i.e., we specify that price should be explained
# by all remaining variables in the dataset (remember, we removed a lot earlier)

ranger_mod$fit

prediction <- predict(ranger_mod, finn_test) %>% 
  bind_cols(finn_test_raw) %>% 
  rename(estimate     = .pred, 
         truth        = ad_tot_price) %>% 
  mutate(abs_dev      = abs(truth - estimate),
         abs_dev_perc = abs_dev/truth)

# Evaluate
prediction %>%
  multi_metric(truth = truth, estimate = estimate)

# Get variable importance:
ranger::importance(x = ranger_mod$fit) %>%
  enframe() %>% 
  ggplot(aes(x = fct_reorder(name, value), y = value)) +
  geom_col(fill = "seagreen4", color = "black") +
  coord_flip() +
  labs(x = NULL, title = "Variable importance plot") +
  theme_light()

# We can also get partial dependency plots using "pdp"
# Note: this is a bit slow...
ranger_mod$fit %>%
  pdp::partial(pred.var = "avg_income", train = finn_train) %>%
  autoplot() +
  theme_light()

# We see that random forest is finding highly non-linear relationships!
# Also, the figure makes a lot more sense - higher income completely
# stops being relevant at one point, instead of increasing "forever"

# Random forest exercises -------------------------------------------------

# 1
# Try setting mtry = 3. What happens with the MAPE? 

# 2
# Try setting number_of_trees = 10. Why does your model get worse?
# What happens to the training error, and what happens to the test error?

# 3
# Try adding "ad_tot_price_per_sqm" to the model. What happens?
# Why is this cheating?

# 4
# Recreate the model, but use ranger directely instead of via parsnip
# see ?ranger


# xgboost -----------------------------------------------------------------

xg_recipe <- recipe(ad_tot_price ~. , data = finn_train_raw) %>% 
  step_mutate(ad_home_type  = fct_lump(ad_home_type, 4),
              ad_owner_type = fct_lump(ad_owner_type, 3),
              bedrooms_missing = is.na(ad_bedrooms)) %>%
  step_rm(ad_id, 
          ad_price,
          ad_tot_price_per_sqm,
          ad_debt, 
          kommune_no, 
          kommune_name, 
          fylke_name, 
          zip_no, 
          zip_name) %>%
  prep()

finn_train <- bake(xg_recipe, finn_train_raw)
finn_test  <- bake(xg_recipe, finn_test_raw)

# Note: Parameters should be optimized using cross-validation!
xg_mod <- boost_tree(mode = "regression",
                     trees = 300,
                     min_n = 2,
                     tree_depth = 6,
                     learn_rate = 0.15,
                     loss_reduction = 0.9) %>% 
  set_engine("xgboost", tree_method = "exact") %>% 
  fit(ad_tot_price ~ ., data = finn_train)


prediction <- predict(xg_mod, finn_test) %>% 
  bind_cols(finn_test_raw) %>% 
  rename(estimate     = .pred, 
         truth        = ad_tot_price) %>% 
  mutate(abs_dev      = abs(truth - estimate),
         abs_dev_perc = abs_dev/truth)

prediction %>%
  multi_metric(truth = truth, estimate = estimate)

# Get variable importance:
xgboost::xgb.importance(model = xg_mod$fit) %>% 
  xgboost::xgb.ggplot.importance()

# Check out a particular tree:
xgboost::xgb.plot.tree(model = xg_mod$fit, trees = 50)

# Check distribution of predicted vs truth
prediction %>% 
  select(estimate, truth) %>% 
  rownames_to_column(var = "id") %>% 
  pivot_longer(-id, names_to = "type", values_to = "value") %>% 
  ggplot(aes(x = value, fill = type)) +
  geom_density(alpha = 0.3)

prediction %>% 
  select(estimate, truth, fylke_name) %>% 
  rownames_to_column(var = "id") %>% 
  pivot_longer(-c(id, fylke_name), names_to = "type", values_to = "value") %>% 
  ggplot(aes(x = value, fill = type)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~fylke_name)


# xgboost exercises -------------------------------------------------------

# 1
# Change tree_depth to 1. What happens to the variable importance list? Why?

# 2
# Find the mean value of your predictions. Verify that it is close to the mean
# value of the target-variable (ad_tot_price).

# 3
# Set trees = 10 and re-train your model. Find the mean of your prediction again.
# What happened?

# 4 (harder)
# Recreate the model, but use xgboost directely instead of parsnip.
# Example: https://www.andrewaage.com/post/a-simple-solution-to-a-kaggle-competition-using-xgboost-and-recipes/

