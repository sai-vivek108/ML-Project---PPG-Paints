## ----setup, include=FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----load_tidyverse-------------------------------------------------------------------------------------
library(tidyverse, tidymodels)



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


## -------------------------------------------------------------------------------------------------------
visdat::vis_miss(dfii)


## -------------------------------------------------------------------------------------------------------
visdat::vis_dat(dfii)


## -------------------------------------------------------------------------------------------------------
cat("Minimum R:", min(dfii$R), " | Minimum G:", min(dfii$G), " | Minimum B:", min(dfii$B), "\n")
cat("Maximum R:", max(dfii$R), " | Maximum G:", max(dfii$G), " | Maximum B:", max(dfii$B))



## -------------------------------------------------------------------------------------------------------
df %>% ggplot(mapping = aes(x=response)) +geom_histogram(bins=30)


## -------------------------------------------------------------------------------------------------------
df %>% ggplot(aes(x=outcome)) + geom_bar()


## -------------------------------------------------------------------------------------------------------
df %>% ggplot(aes(Lightness)) + geom_bar() +xlab('')


## -------------------------------------------------------------------------------------------------------
df %>% ggplot(aes(Saturation)) + geom_bar() +xlab('')


## ----make_longformat_1----------------------------------------------------------------------------------
lf <- dfii %>% 
  tibble::rowid_to_column() %>% 
  pivot_longer(c(R, G, B, Hue, y))
lf %>% glimpse()


## ----eda_1----------------------------------------------------------------------------------------------
lf %>% ggplot(mapping= aes(x = value, fill=as.factor(outcome))) +geom_histogram(bins = 20) + facet_wrap(~name, scales = 'free') +
  theme_minimal()


## -------------------------------------------------------------------------------------------------------
dfii %>% tibble::rowid_to_column() %>% pivot_longer(c(R, G, B, Hue)) %>% 
  ggplot(mapping = aes(x=value,y=y, color=outcome, group=outcome)) +
  geom_smooth(formula = 'y~x', method ='lm')+ 
  geom_point() + facet_wrap(~name, scales = 'free')


## -------------------------------------------------------------------------------------------------------
#df %>% tibble::rowid_to_column() %>% pivot_longer(c(R, G, B))
lf %>% ggplot(mapping = aes(x=value,y=outcome)) +
  geom_smooth(formula = 'y~x', method ='glm')+ 
  geom_point() + facet_wrap(~name, scales = 'free')


## -------------------------------------------------------------------------------------------------------
lf %>% ggplot(mapping = aes(x = Saturation, y = value)) +
  geom_boxplot(mapping = aes(fill = as.factor(outcome), color = as.factor(outcome)), alpha = 0.35) +
  facet_grid(name ~ ., scales = "free_y") +
  theme_bw()


## -------------------------------------------------------------------------------------------------------
lf %>% ggplot(mapping = aes(x = Lightness, y = value)) +
  geom_boxplot(mapping = aes(fill = as.factor(outcome), color = as.factor(outcome)), alpha = 0.35) +
  facet_grid(name ~ ., scales = "free_y") +
  theme_bw()


## -------------------------------------------------------------------------------------------------------
lf %>% ggplot(mapping = aes(x=value,y=outcome)) +
  geom_smooth(formula = 'y~x*I(x^2)', method ='glm')+ 
  geom_point() + facet_wrap(~name, scales = 'free')



## -------------------------------------------------------------------------------------------------------
num_col <- dfii %>% select(-outcome, -Lightness, -Saturation)
options(dlookr_offline=F)
dlookr::plot_normality(num_col)


## -------------------------------------------------------------------------------------------------------
dfii %>%
  tibble::rowid_to_column() %>%
  pivot_longer(cols = c( Hue, y)) %>%
  ggplot(mapping = aes(x = Lightness, y = value, color=Saturation )) +
  geom_smooth(formula = 'y~x', method = 'lm') +
  geom_point()+
  facet_wrap(~name, scales = 'free')+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))



## -------------------------------------------------------------------------------------------------------
dfii %>% tibble::rowid_to_column() %>%
  ggplot(mapping = aes(x=G, y=B, color=R, shape=Lightness)) + geom_point() +
  scale_color_viridis_c() + 
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21, 22)) +
  facet_wrap(~Saturation, scales = 'free') + theme_minimal()


## -------------------------------------------------------------------------------------------------------

dfii %>% 
  select(R, G, B, Hue, y) %>%
  cor() %>%
  corrplot::corrplot.mixed(
    # method = 'color',
    order = 'AOE',
    tl.col = 'black',
    tl.pos = 'lt' 
  )



## -------------------------------------------------------------------------------------------------------
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))

  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]

  if (missing(cex.cor)) cex.cor <- 0.3/strwidth(txt)

  text(0.5, 0.5, paste0(prefix, txt), cex = cex.cor, col = "blue", font = 2)
}


## -------------------------------------------------------------------------------------------------------

suppressWarnings({
  dfii %>% 
    select(-outcome, -Saturation, -Lightness) %>%
    pairs(
      # col = as.factor(dfii$Lightness),
      upper.panel = panel.cor,
      main = "Pairs Plot with Lightness Coloring"
    )
  # legend("right", legend = levels(as.factor(dfii$Lightness)), fill = palette())
})




## -------------------------------------------------------------------------------------------------------

suppressWarnings({
  dfii %>% 
    select(-outcome, -Saturation, -Lightness) %>%
    pairs(
      col = as.factor(dfii$Lightness),
      upper.panel = panel.cor,
      main = "Pairs Plot with Lightness Coloring"
    )
  legend("right", legend = levels(as.factor(dfii$Lightness)), fill = palette())
})




## -------------------------------------------------------------------------------------------------------
suppressWarnings({
  dfii %>% select(-outcome, -Saturation, -Lightness) %>%
    pairs( col = as.factor(dfii$Saturation),
      upper.panel = panel.cor,
      main = "Pairs Plot with Saturation Coloring")
  
  # Add color legend
  legend("right", legend = levels(as.factor(dfii$Saturation)), fill = palette())
})



## -------------------------------------------------------------------------------------------------------
df_features <- dfii %>% select(-y, -outcome, -Lightness, -Saturation)
df_pca <- prcomp(df_features, center = T, scale.=T)
df_pca %>% glimpse()


## -------------------------------------------------------------------------------------------------------
df_pca$x %>% dim()
df_pca$x %>% colnames()


## -------------------------------------------------------------------------------------------------------
df_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  bind_cols(dfii %>% select(Lightness)) %>% 
  pivot_longer(!c("rowid", "Lightness")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  ggplot(mapping = aes(x = as.factor(pc_id), y = value)) +
  geom_boxplot(mapping = aes(fill = Lightness, color = Lightness,
                             group = interaction(pc_id, Lightness)),
               alpha = 0.2) +
  stat_summary(fun.data = "mean_se",
               mapping = aes(group = interaction(pc_id, Lightness),
                             color = Lightness),
               fun.args = list(mult = 2),
               position = position_dodge(0.75)) +
  ggthemes::scale_color_calc() +
  ggthemes::scale_fill_calc() +
  theme_bw()


## -------------------------------------------------------------------------------------------------------
df_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  bind_cols(dfii %>% select(Saturation)) %>% 
  pivot_longer(!c("rowid", "Saturation")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  ggplot(mapping = aes(x = as.factor(pc_id), y = value)) +
  geom_boxplot(mapping = aes(fill = Saturation, color = Saturation,
                             group = interaction(pc_id, Saturation)),
               alpha = 0.2) +
  stat_summary(fun.data = "mean_se",
               mapping = aes(group = interaction(pc_id, Saturation),
                             color = Saturation),
               fun.args = list(mult = 2),
               position = position_dodge(0.75)) +
  ggthemes::scale_color_calc() +
  ggthemes::scale_fill_calc() +
  theme_bw()


## -------------------------------------------------------------------------------------------------------
df_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  bind_cols(dfii %>% select(outcome)) %>% 
  pivot_longer(!c("rowid", "outcome")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  ggplot(mapping = aes(x = as.factor(pc_id), y = value)) +
  geom_boxplot(mapping = aes(fill = as.factor(outcome), color = as.factor(outcome),
                             group = interaction(pc_id, outcome)),
               alpha = 0.2) +
  stat_summary(fun.data = "mean_se",
               mapping = aes(group = interaction(pc_id, outcome),
                             color = as.factor(outcome)),
               fun.args = list(mult = 2),
               position = position_dodge(0.75)) +
  ggthemes::scale_color_calc() +
  ggthemes::scale_fill_calc() +
  theme_bw()


## -------------------------------------------------------------------------------------------------------
factoextra::fviz_screeplot(df_pca, ncp=16, addlabels=T)


## -------------------------------------------------------------------------------------------------------
factoextra::fviz_pca_var(df_pca, col.var='contrib',labs='',
                         gradient.cols = c("darkorange", "grey", 'navyblue'),
                         repel = TRUE)


## -------------------------------------------------------------------------------------------------------
factoextra::fviz_contrib(df_pca, choice='var', axes=1)


## ----bonus_data-----------------------------------------------------------------------------------------
df_bonus <- readr::read_csv('paint_project_bonus_data.csv', show_col_types=F, col_names = T) %>%
  select(-challenge_outcome)
df_total <- df_bonus %>% bind_rows(df)
df_total %>% glimpse()


## -------------------------------------------------------------------------------------------------------
visdat::vis_dat(df_total)


## -------------------------------------------------------------------------------------------------------
df_total %>% ggplot(mapping = aes(x=Lightness)) + geom_bar() + theme_minimal()


## -------------------------------------------------------------------------------------------------------
df_total %>% ggplot(aes(x=Saturation)) + geom_bar()


## -------------------------------------------------------------------------------------------------------
lf_total <- df_total %>% tibble::rowid_to_column() %>% pivot_longer(c(R,G,B,Hue, response)) 
lf_total %>% 
  ggplot(aes(x=value)) + geom_histogram( bins=30)+ facet_wrap(~name, scales = 'free')


## -------------------------------------------------------------------------------------------------------
lf_total %>% ggplot(mapping= aes(x = value, fill=as.factor(outcome))) +geom_histogram(bins = 20) + facet_wrap(~name, scales = 'free') +
  theme_minimal()


## -------------------------------------------------------------------------------------------------------
df_total %>% tibble::rowid_to_column() %>% pivot_longer(c(R, G, B, Hue)) %>% 
  ggplot(mapping = aes(x=value,y=response, color=outcome, group=outcome)) +
  geom_smooth(formula = 'y~x', method ='lm')+ 
  geom_point() + facet_wrap(~name, scales = 'free')


## -------------------------------------------------------------------------------------------------------
df_total %>%
  tibble::rowid_to_column() %>%
  pivot_longer( c( Hue, response)) %>%
  ggplot(mapping = aes(x = Lightness, y = value, color=Saturation )) +
  geom_smooth(formula = 'y~x', method = 'lm') +
  geom_point()+
  facet_wrap(~name, scales = 'free')+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -------------------------------------------------------------------------------------------------------
df_total %>% tibble::rowid_to_column() %>%
  ggplot(mapping = aes(x=G, y=B, color=R, shape=Lightness)) + geom_point() +
  scale_color_viridis_c() + 
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21, 22)) +
  facet_wrap(~Saturation, scales = 'free') + theme_minimal()



## -------------------------------------------------------------------------------------------------------
df_tot_features <- df_total %>% select(-response, -outcome, -Lightness, -Saturation)
df_tot_pca <- prcomp(df_features, center = T, scale.=T)
df_tot_pca %>% glimpse()


## -------------------------------------------------------------------------------------------------------



## -------------------------------------------------------------------------------------------------------
# apply(df_tot_pca$rotation^2, 2, sum)


