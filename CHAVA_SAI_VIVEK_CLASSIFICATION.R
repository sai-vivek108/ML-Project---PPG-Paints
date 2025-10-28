## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load_tidyverse-------------------------------------------------------------------------------------
library(tidyverse, tidymodels)
library(caret)


## ----read_final_data------------------------------------------------------------------------------------
df <- readr::read_csv("paint_project_train_data.csv", col_names = TRUE, show_col_types = F)
df %>% glimpse()


## ----make_reg_data--------------------------------------------------------------------------------------
dfii <- df %>% 
  mutate(y = ifelse(outcome == 1, 'event', 'non_event')) %>% 
  mutate(y = factor(y, levels = c("event", "non_event"))) %>% 
  select(R, G, B, 
         Lightness, Saturation, Hue,y,
          outcome)

dfii %>% glimpse()


## ----standardization------------------------------------------------------------------------------------
set.seed(12321)

train_id <- createDataPartition(dfii$outcome, p = .8, list = FALSE)

df_train <- dfii[train_id,]

df_test <- dfii[-train_id,] 




## -------------------------------------------------------------------------------------------------------
df_train  %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(c(R, G, B, Hue)) %>% ggplot(mapping= aes(x = value, fill=as.factor(outcome))) +geom_histogram(bins = 20) + facet_wrap(~name, scales = 'free') +
  theme_minimal()


## -------------------------------------------------------------------------------------------------------
df_test  %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(c(R, G, B, Hue)) %>% ggplot(mapping= aes(x = value, fill=as.factor(outcome))) +geom_histogram(bins = 20) + facet_wrap(~name, scales = 'free') +
  theme_minimal()


## -------------------------------------------------------------------------------------------------------
cols_num <- c("R", "G", "B", "Hue")
var_mean <- as.vector(apply(df_train[,cols_num], 2, mean))
var_sd <- as.vector(apply(df_train[,cols_num], 2, sd))
df_train_stan  <- df_train[,cols_num] %>% scale(center = var_mean, scale = var_sd) %>%
  as.data.frame() %>% tibble::as_tibble() %>%
  bind_cols(df_train[,c('Lightness', 'Saturation', 'y', 'outcome')])

df_train_stan %>% glimpse()


## -------------------------------------------------------------------------------------------------------
df_test_stan <- df_test[,cols_num] %>% scale(center = var_mean, scale = var_sd) %>%
  as.data.frame() %>% tibble::as.tibble() %>%
  bind_cols(df_test[,c('Lightness', 'Saturation', 'y', 'outcome')])

df_test_stan %>% glimpse()


## -------------------------------------------------------------------------------------------------------
#Intercept only model no INPUTS
glm_intercept <- glm(outcome~1, data = df_train_stan)

#Categorical variables only linear additive
glm_category <- glm(outcome~Saturation + Lightness, family = 'binomial', data = df_train_stan)

#Continuous variables only linear additive
glm_continuous <- glm(outcome~R+G+B+Hue, family = 'binomial' , data = df_train_stan)

#All categorical and continuous variables linear additive
glm_all_linear <- glm(outcome~., family = 'binomial' , data = df_train_stan %>% select(-y))

#Interaction of the categorical inputs with all continuous inputs main effects
glm_cat_interaction <- glm(outcome~ Saturation*(R+G+B+Hue)+ Lightness *(R+G+B+Hue), family = 'binomial', 
                           data = df_train_stan)

#Add categorical inputs to all main effect and all pairwise interactions of continuous
glm_cat_lin <- glm(outcome~Saturation+Lightness+(R+G+B+Hue)^2, family = 'binomial', data= df_train_stan)

# Interaction of the categorical inputs with all main effect and all pairwise interactions of continuous inputs
glm_cat_pairwise <- glm(outcome~Saturation*(R+G+B+Hue)^2 +Lightness*(R+G+B+Hue)^2, family = 'binomial',
                        data= df_train_stan)

# Interaction of Splines of Hue with 5 DoF with the linear and quadratic continuous variables and categorical
glm_basis_1 <- glm(outcome~(splines:: ns(Hue,5)) *(R+G+B+I(R^2)+I(G^2)+I(B^2)+Saturation+Lightness) , 
                   family = 'binomial', data = df_train_stan )

# interaction of splines of Hue with 5 DoF, linear additives of R,G,B, Lightness and Saturation
glm_basis_2 <- glm(outcome~(splines:: ns(Hue,5)) *(R+G+B+Lightness) * Saturation, family = 'binomial',
                 data = df_train_stan )

# Interaction between the Categorical and linear additives of Continuous variables
glm_basis_3 <- glm(outcome~(R+G+B+Hue+I(Hue^2))*Saturation*Lightness, family = 'binomial',
                   data = df_train_stan )



## ----calculate_acc--------------------------------------------------------------------------------------
calculate_accuracy <- function(fitted_model, data) {
  
  predictions <- predict(fitted_model, newdata = data, type = "response")

  predicted_classes <- ifelse(predictions > 0.5, 1, 0)

  accuracy <- mean(predicted_classes == df_test$outcome)
  
  return(accuracy)
}


## ----summary_models-------------------------------------------------------------------------------------
summary_model <- tibble()
lm_list <- c('glm_intercept','glm_category','glm_continuous','glm_all_linear','glm_cat_interaction',
             'glm_cat_lin','glm_cat_pairwise','glm_basis_1','glm_basis_2','glm_basis_3')
 for (i in lm_list){
   Accuracy <- calculate_accuracy(get(i), df_test_stan %>% select(-y))
   result <-broom::glance(get(i)) %>% select(AIC, BIC)
   summary_model <- bind_rows(summary_model,tibble(Model=i, result, Accuracy)) 
   
 }
  
summary_model 


## -------------------------------------------------------------------------------------------------------
 summary_model %>% 
  pivot_longer(c(AIC, BIC, Accuracy)) %>% 
  ggplot(mapping = aes(x = Model, y = value)) +
  geom_point(size = 4) +
  facet_wrap(~name, scales = 'free_y') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))



## -------------------------------------------------------------------------------------------------------
summary_model %>% arrange(AIC)


## -------------------------------------------------------------------------------------------------------
coefplot::coefplot(glm_cat_lin)


## -------------------------------------------------------------------------------------------------------
coefplot::coefplot(glm_category)


## -------------------------------------------------------------------------------------------------------
coefplot::coefplot(glm_all_linear)


## ----model_mat------------------------------------------------------------------------------------------
# model matrix of best model
Xmat_cat_lin <- model.matrix(formula(glm_cat_lin), data = df_test_stan)
# Model matrix of the third best model
Xmat_all_linear <- model.matrix(formula(glm_all_linear), data = df_test_stan)

info_cat_lin <- list(yobs = df$outcome,
                     design_matrix = Xmat_cat_lin,
                     mu_beta = 0,
                     tau_beta = 4.5)
info_all_linear <- list(yobs = df$outcome,
                     design_matrix = Xmat_all_linear,
                     mu_beta = 0,
                     tau_beta = 4.5)




## ----logistic_log_post----------------------------------------------------------------------------------
logistic_logpost <- function(unknowns, my_info)
{
  # extract the design matrix and assign to X
  X <- my_info$design_matrix
  
  # calculate the linear predictor
  eta <- X %*% as.matrix(unknowns)
  
  # calculate the event probability
  mu <- boot::inv.logit(eta)
  
  # evaluate the log-likelihood
  log_lik <- sum(dbinom( x = my_info$y, size =1 , prob = mu, log = TRUE))
  
  # evaluate the log-prior
  log_prior <- sum(dnorm(x = unknowns,mean = my_info$mu_beta, sd = my_info$tau_beta, log = TRUE))
  
  # sum together
  
  log_lik + log_prior
  
}


## ----laplace_func---------------------------------------------------------------------------------------
my_laplace <- function(start_guess, logpost_func, ...)
{
  # code adapted from the `LearnBayes`` function `laplace()`
  fit <- optim(start_guess,
               logpost_func,
               gr = NULL,
               ...,
               method = "BFGS",
               hessian = TRUE,
               control = list(fnscale = -1, maxit = 5001))
  
  mode <- fit$par
  post_var_matrix <- -solve(fit$hessian)
  p <- length(mode)
  int <- p/2 * log(2 * pi) + 0.5 * log(det(post_var_matrix)) + logpost_func(mode, ...)
  # package all of the results into a list
  list(mode = mode,
       var_matrix = post_var_matrix,
       log_evidence = int,
       converge = ifelse(fit$convergence == 0,
                         "YES", 
                         "NO"),
       iter_counts = as.numeric(fit$counts[1]))
}


## ----laplace_model--------------------------------------------------------------------------------------

laplace_cat_lin <- my_laplace(rep(0,ncol(info_cat_lin$design_matrix)), logistic_logpost, info_cat_lin)

laplace_all_linear <- my_laplace(rep(0,ncol(info_all_linear$design_matrix)),
                                 logistic_logpost, info_all_linear)


## ----post_mean_sd---------------------------------------------------------------------------------------

post_mode_cat_lin <- laplace_cat_lin$mode

post_sd_cat_lin <- sqrt(diag(laplace_cat_lin$var_matrix))

post_mode_all_lin <- laplace_all_linear$mode

post_sd_all_lin <- sqrt(diag(laplace_all_linear$var_matrix))



## -------------------------------------------------------------------------------------------------------
exp(laplace_all_linear$log_evidence - laplace_cat_lin$log_evidence)


## ----posterior_coeff_viz_func---------------------------------------------------------------------------
viz_post_coefs <- function(post_means, post_sds, xnames)
{
  tibble::tibble(
    mu = post_means,
    sd = post_sds,
    x = xnames
  ) %>% 
    mutate(x = factor(x, levels = xnames)) %>% 
    ggplot(mapping = aes(x = x)) +
    geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +
    geom_point(mapping = aes(y = mu)) +
    geom_linerange(mapping = aes(ymin = mu - 2 * sd,
                                 ymax = mu + 2 * sd,
                                 group = x)) +
    labs(x = 'feature', y = 'coefficient value') +
    coord_flip() +
    theme_minimal()
}


## ----post_coeff_viz-------------------------------------------------------------------------------------

viz_post_coefs(laplace_all_linear$mode[1:ncol(Xmat_all_linear)],
               sqrt(diag(laplace_all_linear$var_matrix))[1:ncol(Xmat_all_linear)],
               colnames(Xmat_all_linear))



## ----test_grid------------------------------------------------------------------------------------------
set.seed(1234)
viz_grid_glm <- expand.grid(
  G = seq(round(min(df_train_stan$G),4),round(max(df_train_stan$G),4),length.out=101),
  B = seq(round(min(df_train_stan$B),4),round(max(df_train_stan$B),4),length.out=6),
                        R = median(df_train_stan$R),
                        Hue = median(df_train_stan$Hue),
                        Saturation = unique(df_train_stan$Saturation),
                        Lightness = unique(df_train_stan$Lightness),
                        KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()
viz_grid_glm %>% glimpse()



## ----generate_post_samples------------------------------------------------------------------------------
generate_glm_post_samples <- function(mvn_result, num_samples)
{
  # specify the number of unknown beta parameters
  length_beta <- length(mvn_result$mode)
  
  # generate the random samples
  beta_samples <-  MASS::mvrnorm(n = num_samples,
                mu = mvn_result$mode,
                Sigma = mvn_result$var_matrix) 
  # change the data type and name
  beta_samples %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    purrr::set_names(sprintf("beta_%02d", (1:length_beta) - 1))
}


## -------------------------------------------------------------------------------------------------------
post_logistic_pred_samples <- function(Xnew, Bmat)
{
  # calculate the linear predictor at all prediction points and posterior samples
  eta_mat <-  Xnew%*%t(Bmat)
  
  # calculate the event probability
  mu_mat <-  boot::inv.logit(eta_mat)
  
  # book keeping
  list(eta_mat = eta_mat, mu_mat = mu_mat)
}


## -------------------------------------------------------------------------------------------------------
summarize_logistic_pred_from_laplace <- function(mvn_result, Xtest, num_samples)
{
  # generate posterior samples of the beta parameters
  betas <- generate_glm_post_samples(mvn_result, num_samples)
  
  # data type conversion
  betas <- as.matrix(betas)
  
  # make posterior predictions on the test set
  pred_test <-  post_logistic_pred_samples(Xtest, betas)
  
  # calculate summary statistics on the posterior predicted probability
  # summarize over the posterior samples
  
  # posterior mean, should you summarize along rows (rowMeans) or 
  # summarize down columns (colMeans) ???
  mu_avg <- rowMeans(pred_test$mu_mat)
  
  # posterior quantiles
  mu_q05 <- apply(pred_test$mu_mat, 1, stats::quantile, probs = 0.05)
  mu_q95 <- apply(pred_test$mu_mat, 1, stats::quantile, probs = 0.95)
  
  # book keeping
  tibble::tibble(
    mu_avg = mu_avg,
    mu_q05 = mu_q05,
    mu_q95 = mu_q95
  ) %>% 
    tibble::rowid_to_column("pred_id")
}


## ----model_matrix---------------------------------------------------------------------------------------
set.seed(3214)

bayes_glm_all_linear <- model.matrix(~ R + G + B + Hue + Lightness + Saturation, data = viz_grid_glm)

bayes_glm_cat_lin <- model.matrix(~ Saturation + Lightness + (R + G + B + Hue)^2, data = viz_grid_glm)



## ----viz_post_pred--------------------------------------------------------------------------------------
set.seed(3214)

post_pred_all_lin <- summarize_logistic_pred_from_laplace(laplace_all_linear, bayes_glm_all_linear,3000)

post_pred_cat_lin <- summarize_logistic_pred_from_laplace(laplace_cat_lin, bayes_glm_cat_lin,3000)



## ----viz_bayes_logpost----------------------------------------------------------------------------------
viz_bayes_logpost_preds <- function(post_pred_summary, input_df)
{
  post_pred_summary %>% 
    left_join(input_df %>% tibble::rowid_to_column('pred_id'),
              by = 'pred_id') %>% 
    ggplot(mapping = aes(x = G)) +
    geom_ribbon(mapping = aes(ymin = mu_q05, ymax = mu_q95), fill='yellow',
                alpha = 0.25) +
    geom_line(mapping = aes(y = mu_avg),
              size = 0.5) +
    facet_wrap( ~ B, labeller = 'label_both') +
    coord_cartesian(ylim = c(-.5,1))+
    labs(y = "event probability") +
    theme_minimal()
}


## -------------------------------------------------------------------------------------------------------
viz_bayes_logpost_preds(post_pred_all_lin, viz_grid_glm)


## -------------------------------------------------------------------------------------------------------
viz_bayes_logpost_preds(post_pred_cat_lin, viz_grid_glm)


## ----train_control_spec---------------------------------------------------------------------------------
my_ctrl_1 <- trainControl(method = 'repeatedcv', number = 10, repeats = 3,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         savePredictions = TRUE)

my_metric <- 'ROC'


## ----model_1--------------------------------------------------------------------------------------------
set.seed(1256)

mod_1_c <- train(y ~ R + G + B + Hue + Lightness + Saturation, method='glmnet', trControl = my_ctrl_1, 
               preProcess=c('center', 'scale'), family='binomial', metric=my_metric,
               data = df_train)
mod_1_c %>% readr::write_rds("cls_mod_1.rds")
mod_1_c

## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_1_c))


## ----model_2--------------------------------------------------------------------------------------------
set.seed(1245)
mod_2_c <- train(y ~ Saturation + Lightness + (R + G + B + Hue)^2, method='glm', trControl = my_ctrl_1,
               preProcess= c('center', 'scale'), metric = my_metric,
               data= df_train)
mod_2_c %>% readr::write_rds("cls_mod_2.rds")
mod_2_c


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_2_c))


## ----model_3--------------------------------------------------------------------------------------------
set.seed(12343)
mod_3_c <- train(y~ Saturation + Lightness, method= 'glm',
               trControl = my_ctrl_1, metric = my_metric,
               preProcess= c('center', 'scale'), data = df_train)
mod_3_c %>% readr::write_rds("cls_mod_3.rds")
mod_3_c


## ----enet_default---------------------------------------------------------------------------------------
set.seed(1234)
# training a default elastic net
enet_default <- train(y ~ Saturation + Lightness + (R + G + B + Hue)^2, method = 'glmnet', 
                      metric = my_metric, trControl = my_ctrl_1,
                      preProcess= c('center', 'scale'), family='binomial', 
                      data = df_train)



## ----tuning_grid----------------------------------------------------------------------------------------
lambda_grid <- exp(seq(log(min(enet_default$results$lambda)),
                          log(max(enet_default$results$lambda)), length.out = 25))
enet_grid <- expand.grid(alpha = seq(0.1,1, by=0.1),
                         lambda = lambda_grid)


## ----mod_4----------------------------------------------------------------------------------------------
set.seed(1234)

mod_4_c <- train(y ~ Saturation + Lightness + (R + G + B + Hue)^2, method = 'glmnet', metric = my_metric,
               trControl = my_ctrl_1, family = 'binomial',
               preProcess= c('center', 'scale'), tuneGrid = enet_grid, data = df_train)
mod_4_c %>% readr::write_rds("cls_mod_4.rds")
plot(mod_4_c, xTrans=log)


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_4_c))



## ----enet_default_2-------------------------------------------------------------------------------------
set.seed(12345)
# training a default elastic net
enet_1_default <- train(y ~ Saturation + Lightness + (R + G + B + Hue)^2, method = 'glmnet',
                        metric = my_metric, family='binomial',
                         trControl = my_ctrl_1, preProcess= c('center', 'scale'), data = df_train)

lambda_grid <- exp(seq(log(min(enet_1_default$results$lambda)),
                          log(max(enet_1_default$results$lambda)), length.out = 25))
enet_grid <- expand.grid(alpha = seq(0.1,1, by=0.1),
                         lambda = lambda_grid)


## ----mod_5----------------------------------------------------------------------------------------------
set.seed(12335)
mod_5_c <- train(y ~ Saturation + Lightness + (R + G + B + Hue)^2, method = 'glmnet', 
               metric = my_metric, trControl = my_ctrl_1, family = 'binomial', 
               preProcess= c('center', 'scale'), tuneGrid = enet_grid, data = df_train)
mod_5_c %>% readr::write_rds("cls_mod_5.rds")
plot(mod_5_c, xTrans = log)


## -------------------------------------------------------------------------------------------------------
mod_5_c$bestTune


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_5_c))


## ----mod_6----------------------------------------------------------------------------------------------
set.seed(1234543)
nnet_grid <- expand.grid(size=c(5,9,13,17,21), decay=exp(seq(-6,0,length.out=11)))
mod_6_c <- train(y~R+G+B+Hue+Lightness+Saturation, method='nnet', 
                 metric = my_metric, trControl = my_ctrl_1,
               trace=F, tuneGrid = nnet_grid, preProcess=c('center', 'scale'), family='binomial',
               data = df_train)
mod_6_c %>% readr::write_rds("cls_mod_6.rds")
plot(mod_6_c, xTrans = log)


## -------------------------------------------------------------------------------------------------------
mod_6_c$bestTune


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_6_c))


## ----mod_7----------------------------------------------------------------------------------------------
set.seed(123342)
rf_grid <- expand.grid(mtry = c(1:7),
                          KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE)

mod_7_c <- caret::train(y ~ R+G+B+Hue+Lightness+Saturation, method = 'rf',
                      data = df_train , family = 'binomial',
                      importance = TRUE,
                      metric = my_metric,
                      trControl = my_ctrl_1,
                      tuneGrid = rf_grid)
mod_7_c %>% readr::write_rds("cls_mod_7.rds")
mod_7_c


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_7_c))


## -------------------------------------------------------------------------------------------------------
mod_7_c$bestTune


## -------------------------------------------------------------------------------------------------------
set.seed(12233)
xgb_default <- train( y ~ R+G+B+Hue+Lightness+Saturation,
                      data = df_train, family='binomial', 
                      method = 'xgbTree',
                      metric = my_metric,
                      trControl = my_ctrl_1,
                      verbosity = 0,
                      nthread = 1)


## -------------------------------------------------------------------------------------------------------
set.seed(12234)
xgb_grid <- expand.grid(nrounds = c(25, 50, 100, 200, 400, 800),
                             max_depth = c(3, 6, 9),
                             eta =  c(0.0625*xgb_default$bestTune$eta,
                                      0.125*xgb_default$bestTune$eta,
                                      1.0*xgb_default$bestTune$eta),
                             gamma = xgb_default$bestTune$gamma,
                             colsample_bytree = xgb_default$bestTune$colsample_bytree,
                             min_child_weight = xgb_default$bestTune$min_child_weight,
                             subsample = xgb_default$bestTune$subsample)

mod_8_c <- train( y ~ R+G+B+Hue+Lightness+Saturation, 
                   data = df_train, family = 'binomial',
                   method = 'xgbTree', 
                   metric = my_metric, 
                   tuneGrid = xgb_grid,
                   trControl = my_ctrl_1, 
                   verbosity = 0,
                   nthread = 2)
mod_8_c %>% readr::write_rds("cls_mod_8.rds")


## -------------------------------------------------------------------------------------------------------
plot(mod_8_c)


## -------------------------------------------------------------------------------------------------------
mod_8_c$bestTune


## ----mod_9----------------------------------------------------------------------------------------------
set.seed(3333)

knn_grid <- expand.grid(k = 1:16)


mod_9_c <- train(y ~ R+G+B+Hue+Lightness+Saturation,
                    data = df_train,
                    method = "knn", 
                    preProcess = c("center", "scale"),
                    metric = my_metric,
                    trControl = my_ctrl_1,
                tuneGrid = knn_grid)
mod_9_c %>% readr::write_rds("cls_mod_9.rds")
mod_9_c


## -------------------------------------------------------------------------------------------------------
plot(mod_9_c)


## ----mod_10---------------------------------------------------------------------------------------------
set.seed(123445)
mars_grid <- expand.grid( degree = 1:4, 
                          nprune = seq(2, 100, length.out = 10) %>% floor())

mod_10_c <- train(y~ R+G+B+Hue+Lightness+Saturation, method = 'earth', tuneGrid = mars_grid,
                preProcess=c("center", "scale"), metric = my_metric,  glm=list(family='binomial'),
                trControl = my_ctrl_1, data = df_train)
mod_10_c %>% readr::write_rds("cls_mod_10.rds")



## -------------------------------------------------------------------------------------------------------
plot(mod_10_c)


## -------------------------------------------------------------------------------------------------------
mod_10_c$bestTune

## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_10_c))


## -------------------------------------------------------------------------------------------------------
model_compare <- resamples(list( linear_add = mod_1_c, lm_cat_pairwise = mod_2_c,
                                 lm_cat_interaction = mod_3_c, enet_cat_pairwise = mod_4_c,
                                 enet_cet_interaction = mod_5_c, NNet = mod_6_c,
                                RF = mod_7_c, XGB = mod_8_c,
                                KNN= mod_9_c, MARS = mod_10_c))
dotplot(model_compare)


## ----results_compile------------------------------------------------------------------------------------
compile_all_model_preds <- function(models)
{
  purrr::map2_dfr(models, names(models),
                  function(model, model_name){
                    model$pred %>% tibble::as_tibble() %>% 
                      select(obs, event, Resample) %>% 
                      mutate(model_name = model_name)
                  })
}


## ----compile_preds--------------------------------------------------------------------------------------
model_list <- list(
  linear_add = mod_1_c,
  lm_cat_pairwise = mod_2_c,
  lm_cat_interaction = mod_3_c,
  enet_cat_pairwise = mod_4_c,
  enet_cet_interaction = mod_5_c,
  NNet = mod_6_c,
  RF = mod_7_c,
  XGB = mod_8_c,
  KNN = mod_9_c,
  MARS = mod_10_c
)

compiled_preds <- compile_all_model_preds(model_list)
compiled_preds %>% glimpse()


## ----plot_roc_models------------------------------------------------------------------------------------
library(yardstick)
compiled_preds %>% 
  group_by(model_name) %>% 
  roc_curve(obs, event) %>% 
  autoplot()


## -------------------------------------------------------------------------------------------------------
df_test %>% ggplot(mapping = aes(x=outcome)) + geom_bar()


## ----model_eval-----------------------------------------------------------------------------------------
evaluate_models <- function(df_test, model_list) {
  results <- data.frame(Model = character(), 
                        Accuracy = numeric(),
                        Sensitivity = numeric(),
                        Specificity = numeric(),
                        F1_Score = numeric(),
                        stringsAsFactors = FALSE)

  for (model_name in names(model_list)) {
    model <- model_list[[model_name]]
    
    # Make predictions on test data
    predictions <- predict(model, newdata = df_test %>% select(-outcome))

    # Confusion Matrix
    confusion_matrix <- confusionMatrix(predictions, df_test$y)

    # Extract metrics from confusion matrix
    accuracy <- confusion_matrix$overall["Accuracy"]
    sensitivity <- confusion_matrix$byClass["Sensitivity"]
    specificity <- confusion_matrix$byClass["Specificity"]
    f1_score <- confusion_matrix$byClass["F1"]

    # Append results to the dataframe
    results <- rbind(results, data.frame(Model = model_name,
                                         Accuracy = accuracy,
                                         Sensitivity = sensitivity,
                                         Specificity = specificity,
                                         F1_Score = f1_score))
  }

  return(results)
}

# Example usage:
# Assuming df_test is your test dataframe with a "target_variable" column
df_results <- evaluate_models(df_test, list( linear_add = mod_1_c, lm_cat_pairwise = mod_2_c,
                                             lm_cat_interaction = mod_3_c, enet_cat_pairwise = mod_4_c,
                                             enet_cet_interaction = mod_5_c, NNet = mod_6_c,
                                             RF = mod_7_c, XGB = mod_8_c, KNN= mod_9_c,
                                             MARS = mod_10_c))




## -------------------------------------------------------------------------------------------------------
df_results %>%
  tibble::rowid_to_column() %>%
  pivot_longer(c('Accuracy', 'Sensitivity', 'Specificity', 'F1_Score')) %>%
  ggplot(aes(x = Model, y = value, color = name, group = name)) +
  geom_point() +
  geom_line(size = 1.2) +
  labs(title = "Model Evaluation Metrics",
       x = "Model",
       y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

