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
  mutate(y = boot::logit( (response - 0) / (100 - 0) ) ) %>% 
  select(R, G, B, 
         Lightness, Saturation, Hue,
         y, outcome)

dfii %>% glimpse()


## ----train_test_split-----------------------------------------------------------------------------------
# Train - test split
set.seed(12321)

train_id <- sample(1:nrow(dfii), size = floor(0.8 * nrow(dfii)))

df_train <- dfii %>% slice(train_id)

df_test <- dfii %>% slice(-train_id)

df_train_reg <- df_train %>% select(-outcome)
df_test_reg <- df_test %>% select(-outcome)



## ----standardaize---------------------------------------------------------------------------------------
cols_num <- c("R", "G", "B", "Hue")
var_mean <- as.vector(apply(df_train[,cols_num], 2, mean))
var_sd <- as.vector(apply(df_train[,cols_num], 2, sd))
df_train_stan  <- df_train[,cols_num] %>% scale(center = var_mean, scale = var_sd) %>%
  as.data.frame() %>% tibble::as_tibble() %>% bind_cols(df_train[,c('Lightness', 'Saturation', 'y')])

df_train_stan %>% glimpse()


## -------------------------------------------------------------------------------------------------------
df_test_stan <- df_test[,cols_num] %>% scale(center = var_mean, scale = var_sd) %>%
  as.data.frame() %>% tibble::as_tibble() %>% bind_cols(df_test[,c('Lightness', 'Saturation', 'y')])
df_test_stan %>% glimpse()


## -------------------------------------------------------------------------------------------------------
#Intercept only model no INPUTS
lm_intercept <- lm(y~1, data = df_train_stan)

#Categorical variables only linear additive
lm_category <- lm(y~Saturation + Lightness, data = df_train_stan)

#Continuous variables only linear additive
lm_continuous <- lm(y~R+G+B+Hue , data = df_train_stan)

#All categorical and continuous variables linear additive
lm_all_linear <- lm(y~. , data = df_train_stan )

#Interaction of the categorical inputs with all continuous inputs main effects
lm_cat_interaction <- lm(y~ Saturation*(R+G+B+Hue)+ Lightness *(R+G+B+Hue), data = df_train_stan)

#Add categorical inputs to all main effect and all pairwise interactions of continuous
lm_cat_lin <- lm(y~Saturation+Lightness+(R+G+B+Hue)^2, data= df_train_stan)

# Interaction of the categorical inputs with all main effect and all pairwise interactions of continuous inputs
lm_cat_pairwise <- lm(y~Saturation*(R+G+B+Hue)^2 +Lightness*(R+G+B+Hue)^2, data= df_train_stan)

# Interaction of Splines of Hue with 5 DoF with the linear and quadratic continuous variables and categorical
lm_basis_1 <- lm(y~(splines:: ns(Hue,5)) *(R+G+B+I(R^2)+I(G^2)+I(B^2)+Saturation+Lightness),
                 data = df_train_stan )

# interaction of splines of Hue with 5 DoF, linear additives of R,G,B, Lightness and Saturation
lm_basis_2 <- lm(y~(splines:: ns(Hue,5)) *(R+G+B+Lightness) * Saturation,
                 data = df_train_stan )

# Interaction between the Categorical and linear additives of Continuous variables
lm_basis_3 <- lm(y~(R+G+B+Hue)*Saturation*Lightness, data = df_train_stan )



## -------------------------------------------------------------------------------------------------------
summary_model <- tibble()
lm_list <- c('lm_intercept','lm_category','lm_continuous','lm_all_linear','lm_cat_interaction',
             'lm_cat_lin','lm_cat_pairwise','lm_basis_1','lm_basis_2','lm_basis_3')
 for (i in lm_list){
   result <-broom::glance(get(i))
   rmse <- modelr::rmse(get(i), df_train_stan)
   summary_model <- bind_rows(summary_model,tibble(Model=i,rmse=rmse, result)) 
   
 }
  
summary_model 


## -------------------------------------------------------------------------------------------------------
summary_model %>% gather(Measure, Value,AIC, BIC) %>% 
  ggplot(mapping = aes(x=Model, y=Value, color = Measure, group=Measure)) +
  geom_line()+  labs(title = "Model Comparison", x = "Model", y = "Value") +
  geom_point(aes(Model))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


## -------------------------------------------------------------------------------------------------------
summary_model %>% 
  select(Model, df, r.squared, AIC, BIC) %>% 
  pivot_longer(!c("Model", "df")) %>% 
  ggplot(mapping = aes(x = Model, y = value)) +
  geom_point(size = 5) +
  facet_wrap(~name, scales = "free_y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))



## -------------------------------------------------------------------------------------------------------
summary_model %>% ggplot(mapping = aes(Model, rmse, group=1))+
  geom_line()+geom_point() + theme_minimal()+  
  geom_point(data = summary_model[which.min(summary_model$rmse), , drop = FALSE],color = "red", size = 3) +
  theme(axis.text.x = element_text(angle = 45))


## ----top_3_models_1-------------------------------------------------------------------------------------
summary(lm_cat_lin)
lm_cat_lin %>% coefplot::coefplot()


## ----top_3_models_2-------------------------------------------------------------------------------------
summary(lm_cat_interaction)
lm_cat_interaction %>% coefplot::coefplot()


## ----top_3_models_3-------------------------------------------------------------------------------------
summary(lm_cat_pairwise)
lm_cat_pairwise %>% coefplot::coefplot()


## ----blm_1----------------------------------------------------------------------------------------------
blm_1 <- model.matrix(formula(lm_cat_lin), data = df_train_stan)
info_blm_1 <- list(yobs = df_train$y,
  design_matrix = blm_1,
  mu_beta = 0 ,
  tau_beta = 4,
  sigma_rate =1 )



## ----blm_2----------------------------------------------------------------------------------------------
blm_2 <- model.matrix(formula(lm_cat_interaction), data = df_train_stan)
info_blm_2 <- list(yobs = df_train$y,
  design_matrix = blm_2,
  mu_beta = 0 ,
  tau_beta = 4,
  sigma_rate =1 )


## -------------------------------------------------------------------------------------------------------
dim(blm_1)
dim(blm_2)


## -------------------------------------------------------------------------------------------------------
lm_logpost <- function(unknowns, my_info)
{
  # specify the number of unknown beta parameters
  length_beta <- ncol(my_info$design_matrix)
  
  # extract the beta parameters from the `unknowns` vector
  beta_v <- unknowns[1:length_beta]
  
  # extract the unbounded noise parameter, varphi
  lik_varphi <- unknowns[length_beta + 1]
  
  # back-transform from varphi to sigma
  lik_sigma <- exp(lik_varphi)
  
  # extract design matrix
  X <-  my_info$design_matrix
  #print(X)
  
  # calculate the linear predictor
  mu <- as.vector(X %*% as.matrix(beta_v))
  #print(mu)
  
  # evaluate the log-likelihood
  log_lik <- sum(dnorm(x = my_info$yobs,
                       mean = mu,
                       sd = lik_sigma,
                       log = TRUE))
  
  
  # evaluate the log-prior
  log_prior_beta <-  sum(dnorm(x = beta_v,
                              mean = my_info$mu_beta,
                              sd = my_info$tau_beta,
                              log = TRUE))
  
   log_prior_sigma <- dexp(x = lik_sigma,
                          rate = my_info$sigma_rate,
                          log = TRUE)
  
  # add the mean trend prior and noise prior together
  log_prior <- log_prior_beta + log_prior_sigma
  
  # account for the transformation
  log_derive_adjust <- lik_varphi
  
  # sum together
  log_lik + log_prior + log_derive_adjust
  
}


## -------------------------------------------------------------------------------------------------------
my_laplace <- function(start_guess, logpost_func, ...)
{
  # code adapted from the `LearnBayes`` function `laplace()`
  fit <- optim(start_guess,
               logpost_func,
               gr = NULL,
               ...,
               method = "BFGS",
               hessian = TRUE,
               control = list(fnscale = -1, maxit = 1001))
  
  mode <- fit$par
  post_var_matrix <- -solve(fit$hessian)
  p <- length(mode) # number of unknown parameters
  int <- p/2 * log(2*pi) + 1/2 * log(det(post_var_matrix)) + logpost_func(mode, ...)
  # package all of the results into a list
  list(mode = mode,
       var_matrix = post_var_matrix,
       log_evidence = int,
       converge = ifelse(fit$convergence == 0,
                         "YES", 
                         "NO"),
       iter_counts = as.numeric(fit$counts[1]))
}



## -------------------------------------------------------------------------------------------------------
laplace_lm_blm1 <- my_laplace(rep (0, ncol(blm_1)+1), lm_logpost, info_blm_1)
laplace_lm_blm2 <- my_laplace(rep (0, ncol(blm_2)+1), lm_logpost, info_blm_2)


## -------------------------------------------------------------------------------------------------------
exp(laplace_lm_blm1$log_evidence)/sum(exp(laplace_lm_blm2$log_evidence))


## -------------------------------------------------------------------------------------------------------
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
    theme_bw()
}


## -------------------------------------------------------------------------------------------------------
viz_post_coefs(laplace_lm_blm1$mode[1:ncol(blm_1)], sqrt(diag(laplace_lm_blm1$var_matrix))[1:ncol(blm_1)], colnames(blm_1))


## -------------------------------------------------------------------------------------------------------
viz_post_coefs <- function(post_means, post_sds, mle_sigma, xnames) {
  tibble::tibble(
    mu = post_means,
    sd = post_sds,
    term =  xnames
  ) %>% 
    left_join(mle_sigma, by = 'term')%>% 
    pivot_longer(c("mu", "mle")) %>% 
    filter(term != '(Intercept)')  %>%  
    ggplot(mapping = aes(y=value, x = term)) +
    geom_hline(yintercept = 0, color = 'grey', linetype = 'dashed') +
    geom_point(mapping = aes(color = name, shape=name), shape = 16, position = position_dodge(width = 0.2)) +  # Change shape for post_means points
    stat_summary(geom = 'linerange',
                 fun.max = 'max', fun.min = 'min',
                 mapping = aes(group = term),
                 color = 'black', linewidth = 1) +
    # geom_linerange(mapping = aes(ymin = mu - 2 * sd,
    #                              ymax = mu + 2 * sd,
    #                              group = term)) +
    labs(x = 'feature', y = 'coefficient value') +
    coord_flip() +
    theme_minimal()+
    scale_color_manual(name = "Legend", values = c("red", "black"), labels = c("MLE", "mu"))
}


## -------------------------------------------------------------------------------------------------------
mle_sigma_values <- broom::tidy(lm_cat_lin) %>% select(term, mle = estimate)
 

viz_post_coefs(laplace_lm_blm1$mode[1:ncol(blm_1)], sqrt(diag(laplace_lm_blm1$var_matrix))[1:ncol(blm_1)],
               mle_sigma_values, colnames(blm_1))


## ----test_grid------------------------------------------------------------------------------------------

viz_grid_lm <- expand.grid(
  G = seq(round(min(df_train_stan$G),3),round(max(df_train_stan$G),3),length.out=101),
  B = seq(round(min(df_train_stan$B),3),round(max(df_train_stan$B),3),length.out=6),
                        R = median(df_train_stan$R),
                        Hue = median(df_train_stan$Hue),
                        Saturation = unique(df_train_stan$Saturation),
                        Lightness = unique(df_train_stan$Lightness),
                        KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()
viz_grid_lm %>% glimpse()


## ----prediction_eval_func-------------------------------------------------------------------------------
tidy_predict <- function(mod, xnew)
{
  pred_df <- predict(mod, xnew, interval = "confidence") %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    dplyr::select(pred = fit, ci_lwr = lwr, ci_upr = upr) %>% 
    bind_cols(predict(mod, xnew, interval = 'prediction') %>% 
                as.data.frame() %>% tibble::as_tibble() %>% 
                dplyr::select(pred_lwr = lwr, pred_upr = upr))
  
  xnew %>% bind_cols(pred_df)
}



## ----predict_viz_grid-----------------------------------------------------------------------------------
lm_best_mod <- tidy_predict(lm_cat_lin, viz_grid_lm)


## ----viz_test_grid--------------------------------------------------------------------------------------
lm_best_mod %>% 
  ggplot( mapping = aes(x = G)) + 
  geom_ribbon( mapping = aes(ymin = pred_lwr , ymax = pred_upr), fill = 'red') +
  geom_ribbon( mapping = aes( ymin = ci_lwr, ymax = ci_upr), fill = 'blue') +
  geom_line( mapping = aes(y = pred))  + facet_wrap(~B) + coord_cartesian((ylim = c(-1,1))) +
  theme_minimal()


## ----lm_post_samples------------------------------------------------------------------------------------
generate_lm_post_samples <- function(mvn_result, length_beta, num_samples)
{
  MASS::mvrnorm(n = num_samples,
                mu = mvn_result$mode,
                Sigma = mvn_result$var_matrix) %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    purrr::set_names(c(sprintf("beta_%02d", 0:(length_beta-1)), "varphi")) %>% 
    mutate(sigma = exp(varphi))
}


## ----post_pred_mean-------------------------------------------------------------------------------------
post_lm_pred_samples <- function(Xnew, Bmat, sigma_vector)
{
  # number of new prediction locations
  M <- nrow(Xnew)
  # number of posterior samples
  S <- nrow(Bmat)
  
  # matrix of linear predictors
  Umat <- Xnew %*% t(Bmat)

  
  # assmeble matrix of sigma samples, set the number of rows
  Rmat <- matrix(rep(sigma_vector, M), nrow(Xnew) , byrow = TRUE)
  
  # generate standard normal and assemble into matrix
  # set the number of rows
  Zmat <- matrix(rnorm(M*S), nrow(Xnew) , byrow = TRUE)

  
  # calculate the random observation predictions
  Ymat <- Umat + Rmat * Zmat
 
  
  # package together
  list(Umat = Umat, Ymat = Ymat)
}


## ----summarize_wrapper_func-----------------------------------------------------------------------------
make_post_lm_pred <- function(Xnew, post)
{
  Bmat <- post %>% dplyr::select(starts_with("beta")) %>% as.matrix()
  
  sigma_vector <- post %>% pull(sigma)
  
  post_lm_pred_samples(Xnew, Bmat, sigma_vector)
}


## ----summarize_post_pred--------------------------------------------------------------------------------
summarize_lm_pred_from_laplace <- function(mvn_result, Xtest, num_samples)
{
  # generate posterior samples of the beta parameters
  post <- generate_lm_post_samples(mvn_result, ncol(Xtest), num_samples)
  #print('post')
  #print(dim(post))
  
  # make posterior predictions on the test set
  pred_test <- make_post_lm_pred(Xtest, post)
  
  # calculate summary statistics on the predicted mean and response
  # summarize over the posterior samples
  
  # posterior mean, should you summarize along rows (rowMeans) or 
  # summarize down columns (colMeans) ???
  mu_avg <- rowMeans(pred_test$Umat)
  y_avg <- rowMeans(pred_test$Ymat)
  
  # posterior quantiles for the middle 95% uncertainty intervals
  mu_lwr <- apply(pred_test$Umat, 1, stats::quantile, probs = 0.025)
  mu_upr <- apply(pred_test$Umat, 1, stats::quantile, probs = 0.975)
  y_lwr <- apply(pred_test$Ymat, 1, stats::quantile, probs = 0.025)
  y_upr <- apply(pred_test$Ymat, 1, stats::quantile, probs = 0.975)
  
  # book keeping
  tibble::tibble(
    mu_avg = mu_avg,
    mu_lwr = mu_lwr,
    mu_upr = mu_upr,
    y_avg = y_avg,
    y_lwr = y_lwr,
    y_upr = y_upr
  ) %>% 
    tibble::rowid_to_column("pred_id")
}


## ----bayes_model_best-----------------------------------------------------------------------------------
#creating test design matrix
bayes_best_mod <- model.matrix(~Saturation + Lightness + (R + G + B + Hue)^2, data = viz_grid_lm)
# summarizing posterior predictions
post_pred_summary <- summarize_lm_pred_from_laplace(laplace_lm_blm1, bayes_best_mod, 5000)



## ----post_pred_viz--------------------------------------------------------------------------------------
post_pred_summary %>% 
  left_join(viz_grid_lm %>% tibble::rowid_to_column("pred_id"),
            by = 'pred_id') %>% ggplot(mapping = aes(G)) +
  geom_ribbon(aes(ymin=y_lwr, ymax = y_upr), fill= 'blue') +
  geom_ribbon(aes(ymin= mu_lwr, ymax = mu_upr), color='yellow', fill='black')+
  geom_line(aes(y=mu_avg))+
  coord_cartesian(ylim = c(-7,7)) + facet_wrap(~B) + theme_minimal()


## ----train_control_spec---------------------------------------------------------------------------------
my_ctrl_1 <- trainControl(method = 'repeatedcv', number = 10, repeats = 5, savePredictions = T)

my_metric <- 'RMSE'


## ----model_1--------------------------------------------------------------------------------------------
set.seed(1256)

mod_1 <- train(formula(lm_all_linear), method='lm', trControl = my_ctrl_1, 
               preProcess=c('center', 'scale'), metric= my_metric,
               data = df_train_reg)
mod_1 %>% readr::write_rds("reg_mod_1.rds")
plot(varImp(mod_1))


## ----model_2--------------------------------------------------------------------------------------------
set.seed(1245)
mod_2 <- train(formula(lm_cat_lin), method='lm', trControl = my_ctrl_1,
               preProcess= c('center', 'scale'), data= df_train_reg)
mod_2 %>% readr::write_rds("reg_mod_2.rds")
plot(varImp(mod_2))


## ----model_3--------------------------------------------------------------------------------------------
set.seed(12343)
mod_3 <- train(formula(lm_cat_interaction), method= 'lm', trControl = my_ctrl_1,
               preProcess= c('center', 'scale'), data = df_train_reg)
mod_3 %>% readr::write_rds("reg_mod_3.rds")
plot(varImp(mod_3))


## ----enet_default---------------------------------------------------------------------------------------
set.seed(1234)
# training a default elastic net
enet_default <- train(formula(lm_cat_lin), method = 'glmnet', metric = my_metric, trControl = my_ctrl_1,
               preProcess= c('center', 'scale'), data = df_train_reg)



## ----tuning_grid----------------------------------------------------------------------------------------
lambda_grid <- exp(seq(log(min(enet_default$results$lambda)),
                          log(max(enet_default$results$lambda)), length.out = 25))
enet_grid <- expand.grid(alpha = seq(0.1,1, by=0.1),
                         lambda = lambda_grid)


## ----mod_4----------------------------------------------------------------------------------------------
set.seed(1234)

mod_4 <- train(formula(lm_cat_lin), method = 'glmnet', metric = my_metric, trControl = my_ctrl_1,
               preProcess= c('center', 'scale'), tuneGrid = enet_grid, data = df_train_reg)
mod_4 %>% readr::write_rds("reg_mod_4.rds")
plot(mod_4, xTrans=log)


## -------------------------------------------------------------------------------------------------------
mod_4$bestTune


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_4))


## ----enet_default_2-------------------------------------------------------------------------------------
set.seed(12345)
# training a default elastic net
enet_1_default <- train(formula(lm_cat_interaction), method = 'glmnet', metric = my_metric, 
                         trControl = my_ctrl_1, preProcess= c('center', 'scale'), data = df_train_reg)

lambda_grid <- exp(seq(log(min(enet_1_default$results$lambda)),
                          log(max(enet_1_default$results$lambda)), length.out = 25))
enet_grid <- expand.grid(alpha = seq(0.1,1, by=0.1),
                         lambda = lambda_grid)


## ----mod_5----------------------------------------------------------------------------------------------
set.seed(12335)
mod_5 <- train(formula(lm_cat_interaction), method = 'glmnet', metric = my_metric, trControl = my_ctrl_1,
               preProcess= c('center', 'scale'), tuneGrid = enet_grid, data = df_train_reg)
mod_5 %>% readr::write_rds("reg_mod_5.rds")
plot(mod_5, xTrans = log)


## -------------------------------------------------------------------------------------------------------
mod_5$bestTune


## -------------------------------------------------------------------------------------------------------
# coef(mod_5$finalModel, s=mod_5$bestTune$lambda)
plot(varImp(mod_5))


## ----mod_6a---------------------------------------------------------------------------------------------
set.seed(1234543)
nnet_grid <- expand.grid(size=c(5,9,13,17,21), decay=exp(seq(-6,0,length.out=11)))
mod_6 <- train(y~., method='nnet', metric = my_metric, trControl = my_ctrl_1,
               trace=F, tuneGrid = nnet_grid, preProcess=c('center', 'scale'), data = df_train_reg)
mod_6 %>% readr::write_rds("reg_mod_6.rds")
plot(mod_6, xTrans = log)


## -------------------------------------------------------------------------------------------------------
mod_6$bestTune


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_6))


## ----mod_7----------------------------------------------------------------------------------------------
set.seed(123342)
rf_grid <- expand.grid(mtry = c(1:7),
                          KEEP.OUT.ATTRS = FALSE,
                        stringsAsFactors = FALSE)

mod_7 <- caret::train(y ~ ., method = 'rf',
                      data = df_train_reg,
                      importance = TRUE,
                      metric = my_metric,
                      trControl = my_ctrl_1,
                      tuneGrid = rf_grid)
mod_7 %>% readr::write_rds("reg_mod_7.rds")
mod_7


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_7))


## -------------------------------------------------------------------------------------------------------
set.seed(12233)
xgb_default <- train( y ~ .,
                      data = df_train_reg,
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

mod_8 <- train( y ~ ., 
                   data = df_train_reg, 
                   method = 'xgbTree', 
                   metric = my_metric, 
                   tuneGrid = xgb_grid,
                   trControl = my_ctrl_1, 
                   verbosity = 0,
                   nthread = 2)
mod_8 %>% readr::write_rds("reg_mod_8.rds")



## -------------------------------------------------------------------------------------------------------
plot(mod_8)

## -------------------------------------------------------------------------------------------------------
mod_8$bestTune


## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_8))


## ----mod_9----------------------------------------------------------------------------------------------
set.seed(3333)

knn_grid <- expand.grid(k = 1:16)


mod_9<- train(y ~ .,
                    data = df_train_reg,
                    method = "knn", 
                    preProcess = c("center", "scale"),
                    metric = my_metric,
                    trControl = my_ctrl_1,
                tuneGrid = knn_grid)

mod_9 %>% readr::write_rds("reg_mod_9.rds")


## -------------------------------------------------------------------------------------------------------
plot(mod_9)


## -------------------------------------------------------------------------------------------------------
mod_9$bestTune


## ----mod_10---------------------------------------------------------------------------------------------
set.seed(123445)
mars_grid <- expand.grid( degree = 1:4, 
                          nprune = seq(2, 100, length.out = 10) %>% floor())

mod_10 <- train(y~ ., method = 'earth', tuneGrid = mars_grid,
                preProcess=c("center", "scale"), metric = my_metric, 
                trControl = my_ctrl_1, data = df_train_reg)
mod_10 %>% readr::write_rds("reg_mod_10.rds")
mod_10


## -------------------------------------------------------------------------------------------------------
plot(mod_10)


## -------------------------------------------------------------------------------------------------------
mod_10$bestTune

## -------------------------------------------------------------------------------------------------------
plot(varImp(mod_10))


## -------------------------------------------------------------------------------------------------------
model_compare <- resamples(list( linear_add = mod_1, lm_cat_pairwise = mod_2,
                                 lm_cat_interaction = mod_3, enet_cat_pairwise = mod_4,
                                 enet_cet_interaction = mod_5, NNet = mod_6,
                                RF = mod_7, XGB = mod_8,
                                KNN= mod_9, MARS = mod_10))
dotplot(model_compare)


## ----model_eval-----------------------------------------------------------------------------------------
evaluate_models <- function(models, df_test) {
  results <- data.frame(Model = character(), MSE = numeric(), RMSE = numeric(), MAE = numeric(), stringsAsFactors = FALSE)

  for (i in names(models)) {
    model <- models[[i]]
    model_name <- i
     
    # Predict on df_test
    
    predictions <- predict(model, newdata = df_test %>% select(-y ))
    mse_tr <- model$results$RMSE
    mae_tr <- model$results$MAE
    rsq_tr <- model$results$Rsquared
    # Calculate metrics
    mse <- mean((df_test$y - predictions)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(df_test$y - predictions))

    # Store results in the data frame
    result <- data.frame(Model = model_name, MSE = mse, RMSE = rmse, MAE = mae, mse_tr = mse_tr,
                         mae_tr = mae_tr, rsq_tr <- rsq_tr)
    results <- rbind(results, result)
  }

  return(results)
}

model_list <-list(linear_add = mod_1, lm_cat_pairwise = mod_2,
                                 lm_cat_interaction = mod_3, enet_cat_pairwise = mod_4,
                                 enet_cet_interaction = mod_5, NNet = mod_6,
                                RF = mod_7, XGB = mod_8,
                                KNN= mod_9, MARS = mod_10)

df_results <- evaluate_models(model_list, df_test_reg)




## -------------------------------------------------------------------------------------------------------
 df_results


## -------------------------------------------------------------------------------------------------------
df_results %>%
  tibble::rowid_to_column() %>%
  pivot_longer(c('MSE', 'MAE', 'mse_tr', 'mae_tr'), values_to = 'Value') %>%
  ggplot(aes(x = Model, y = Value, color = name, group = name)) +
  geom_point() +
  geom_line(size = 1.2) +
  labs(title = "Model Evaluation Metrics",
       x = "Model",
       y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


## -------------------------------------------------------------------------------------------------------
df_results %>%
  tibble::rowid_to_column() %>%
  pivot_longer(c('MSE', 'MAE', 'RMSE'), values_to = 'Value') %>%
  ggplot(aes(x = Model, y = Value, color = name, group = name)) +
  geom_point() +
  geom_line(size = 1.2) +
  labs(title = "Model Evaluation Metrics",
       x = "Model",
       y = "Metric Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))


## ----best_model-----------------------------------------------------------------------------------------
for (metric_col in c("MSE", "RMSE", "MAE")) {
  min_index <- which.min(df_results[[metric_col]])
  min_model <- df_results$Model[min_index]
  min_score <- df_results[[metric_col]][min_index]
  
  cat(sprintf("For %s, the model '%s' has the minimum score: %f\n", metric_col, min_model, min_score))
}

