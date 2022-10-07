# tidymodel_practice

library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results


#성게데이터 (세가지 섭식방법에 따른 성게 크기 예측)
df <- 
tibble::tribble(
     ~TREAT,  ~IV, ~SUTW,
  "Initial",  3.5,  0.01,
  "Initial",    5,  0.02,
  "Initial",    8, 0.061,
  "Initial",   10, 0.051,
  "Initial",   13, 0.041,
  "Initial",   13, 0.061,
  "Initial",   15, 0.041,
  "Initial",   15, 0.071,
  "Initial",   16, 0.092,
  "Initial",   17, 0.051,
  "Initial",   19, 0.051,
  "Initial",   20, 0.082,
  "Initial",   21, 0.102,
  "Initial",   21, 0.092,
  "Initial",   24, 0.051,
  "Initial",   24, 0.061,
  "Initial",   24, 0.082,
  "Initial",   28, 0.071,
  "Initial",   29, 0.071,
  "Initial",   35, 0.082,
  "Initial",   36, 0.061,
  "Initial",   39, 0.082,
  "Initial",   39, 0.112,
  "Initial",   44, 0.102,
      "Low",    5, 0.041,
      "Low",    8, 0.031,
      "Low",  8.5, 0.041,
      "Low", 11.5, 0.082,
      "Low",   10, 0.071,
      "Low",   14, 0.051,
      "Low",   15, 0.061,
      "Low", 15.5, 0.082,
      "Low",   18, 0.061,
      "Low",   18, 0.061,
      "Low",   18, 0.071,
      "Low",   22, 0.041,
      "Low", 21.5, 0.061,
      "Low", 20.5, 0.061,
      "Low",   25, 0.082,
      "Low",   26, 0.061,
      "Low",   38, 0.071,
      "Low",   41, 0.082,
      "Low",   41, 0.061,
      "Low",   43, 0.061,
      "Low",   45, 0.071,
     "High",  7.5, 0.051,
     "High",  9.5, 0.051,
     "High",    9, 0.082,
     "High",    9, 0.092,
     "High",   12, 0.092,
     "High",   13, 0.061,
     "High", 14.5, 0.051,
     "High",   15, 0.102,
     "High",   13, 0.112,
     "High", 11.5, 0.071,
     "High",   14, 0.071,
     "High",   14, 0.051,
     "High", 15.5, 0.082,
     "High",   15, 0.092,
     "High", 17.5, 0.102,
     "High",   19, 0.122,
     "High",   19, 0.102,
     "High", 19.5, 0.163,
     "High",   20, 0.051,
     "High", 23.5, 0.112,
     "High",   30, 0.153,
     "High",   26, 0.082,
     "High",   29, 0.122,
     "High",   39, 0.102,
      "Low", 47.5, 0.041,
      "Low", 46.5, 0.061,
      "Low", 11.5, 0.031
  )


urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  df %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>%  #컬럼이름 바꾸기
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High"))) #변수 순서 

urchins

#먹이주는 방식에 따라 초기/나중 봉합사 길이 비교
ggplot(urchins,
       aes(x = initial_volume, 
           y = width, 
           group = food_regime, 
           col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)
#> `geom_smooth()` using formula 'y ~ x'


#모델링
width ~ initial_volume * food_regime # 봉합사 길이 = 초기 볼륨 * 먹이 주는 방식

#선형회귀모델
linear_reg()
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm

#모델 유형 지정 --> 모델 엔진 

linear_reg() %>% 
  set_engine("keras")

lm_mod <- linear_reg()

#훈련
lm_fit <- 
  lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)
lm_fit


#훈련결과 자세히 보기
tidy(lm_fit)

tidy(lm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))

#예측하기 (초기 20일 때, 각각 먹이 주는 방식에 따라 얼마나 봉합사 길이가 나올까?)
new_points <- expand.grid(initial_volume = 20, 
                          food_regime = c("Initial", "Low", "High"))
new_points


mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

#예측의 상한치와 하한치 보기
conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred


#예측값 시각화

plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)


# and plot:
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "urchin size")


#다른 모델로 예측해보면...
#베이지안분석

# set the prior distribution (데이터의 사전 분포를 코시 종모양분포라고 가정)
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make the parsnip model (모델 만들기)
bayes_mod <-   
  linear_reg() %>% 
  set_engine("stan", 
             prior_intercept = prior_dist, 
             prior = prior_dist) 

# train the model (훈련)
bayes_fit <- 
  bayes_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

#베이지안 모델로 훈련한 값
print(bayes_fit, digits = 5)


#모델 간단히 보기
tidy(bayes_fit, conf.int = TRUE)

#20과 상한치, 하한치 만들기
bayes_plot_data <- 
  new_points %>% 
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "urchin size") + 
  ggtitle("Bayesian model with t(1) prior distribution")


#먹이 주는 방식에 따라 초기 볼륨 중간값 
urchins %>% 
  group_by(food_regime) %>% 
  summarize(med_vol = median(initial_volume))

# 베이지안 모델
bayes_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

#x : 초기 볼륨, y= 나중 봉합사 길이, fitting : 선형
ggplot(urchins,
       aes(initial_volume, width)) +      # returns a ggplot object 
  geom_jitter() +                         # same
  geom_smooth(method = lm, se = FALSE) +  # same                    
  labs(x = "Volume", y = "Width")         # etc


###########################################################################################
library(tidyverse)
library(tidymodels)

data(ames)
ames <- ames %>% janitor::clean_names()
head(ames)

str(ames)

#데이터 분할

set.seed(2021)
ames_split <- initial_split(ames, prop = 0.8, strata = sale_price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

#전처리 (3단계)
 # recipe(요리 방식을 정의하는 단계), prep(요리 재료를 준비하는 단계), bake(요리를 하는 단계)로 구성된다.

# recipe는 사전에 처리할 함수를 정의하는 단계(데이터 전처리 함수 만들기)
# prep은 training set으로 부터 recipe에서 정의한 데이터 전처리 과정을 계산하는 단계이다
# bake는 recipe, prep을 이용해서 계산된 전처리 방식을 output으로 출력하는 단계이다.

# 전처리 함수
# Normalization

## step_center(var) - 평균을 빼서 중심 이동
## step_normalize(var) - 평균 빼고, 분산으로 나눠서 표준화
# 
 
# Filters
# 
## step_corr(threshold = 0.9) - 상관계수 절대값이 큰 변수 제거
## step_rm(var) - 변수 제거
## step_zv() - 분산이 0인 변수 제거
## step_nzv() - 분산이 거의 0인 변수 제거

# Transformations
# 
## step_log(var, base = exp(1) ) - 로그 변환
## step_logit(var) - 로짓 변환
## step_poly(var, degree = 2) - 변수에 polynomial term 추가(glm에서 poly() 와 동일, 즉 orthogonal polynomial 이용)
## step_BoxCox() - Boxcox 변환
## step_YeoJohnson - YeoJohnson 변환
 
# Discretization
# 
## step_discretize(var, num_breaks = 4) - 연속형 변수 이산형으로 변환
## step_cut() - 연속형 변수를 지정한 값을 기준으로 이산형으로 변환
## include_outside_range - 지정한 범위를 넘어선 값을 양끝 break에 포함시킬지 여부. default = FALSE이며 결측치 처리됨
   # breaks - 절단 기준이 되는 값
 
# Dummy variables and encodings
# 
## step_date() - date 변수에서 year, month, day of week 변수를 새롭게 생성
### feature = c(‘dow’, ‘month’, ‘year’) - 요일, 달, 연도 변수 추가
### abbr = T - Sunday or Sun
### label = Sunday or number
# 
## step_holiday() - date 변수에서 공휴일에 관한 이진변수 새롭게 생성
### holidays = c(‘LaborDay’, ‘NewYearDay’, ‘ChristmasDay’)
### holidays = timeDate::listHolidays(‘US’)
 
## step_dummy() - character or factor 변수를 더미변수로 변환
### one_hot = TRUE - C +1개의 더미변수 생성(one_hot = F: C-1개 더미변수 생성
## step_other() - 범주형 변수의 level이 여러개일 때, 하위 범주를 기타로 묶음
### threshold = 0.05 - 하위 5% 범주는 기타로 묶임
### other : 기타로 지정할 level 이름 지정

##step_interact() - 상호작용 항 추가
                                 

#회귀분석 : sale_price 예측
ames_rec <- 
  recipe(sale_price ~ ., data = ames_train) %>% #모델식
  step_string2factor(all_nominal()) %>%  #문자는 범주형으로 
  step_other(all_nominal(), threshold = 0.01) %>% # 범주형 변수중 하휘 1%는 기타로 묶음
  step_nzv(all_nominal()) #step_nzv() - 분산이 거의 0인 변수 제거 (수치가 거의 한종류)


ames_rec_prepped <- prep(ames_rec)
ames_train_prepped <- bake(ames_rec_prepped, new_data = NULL) #train 데이터 전처리
ames_test_prepped <- bake(ames_rec_prepped, ames_test) #test 데이터 전처리


## --> 전처리 완료, 

#workflow : 전저리된 데이터를 이용하여 모델 피팅

#랏소 모델 만들기
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% # mixture = 1 : LASSO, 0 : ridge 
  set_engine("glmnet")

# 핏팅하기
lasso_wf <- workflow() %>% 
  add_formula(sale_price ~.) %>% 
  add_model(lasso_spec)

lasso_wf

#모델 하이퍼 파라미터 최적화
lambda_grid <- grid_regular(penalty(), levels = 50)

# 교차검증을 위한 데이터 생성
vb_folds <- vfold_cv(ames_train_prepped, v = 5)
vb_folds


lasso_res <- tune_grid(  #하이퍼 파라미터 튜닝
  lasso_wf,           #워크 플로우
  resamples = vb_folds,#교차검증셋 
  grid = lambda_grid,  #튜닝 방식
  control = control_grid(save_pred = TRUE),  #최종 튜닝후 pred 저장 옵션
)


#최적의 하이퍼 파라미터 보기
show_best(lasso_res, 'rmse')

#하이퍼 파라미터에 따른 rmse 가 어떻게 되는지 보자자

lasso_res %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean, color = .metric)) + # .metric : rmse 
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.5) + 
  geom_line(size = 1.5, show.legend = F) + 
  facet_wrap(~.metric, scales = 'free', nrow = 2)


#best 파라미터 넣고 workflow 업데이터

best_param <- select_best(lasso_res, 'rmse')
final_lasso <- finalize_workflow(lasso_wf, best_param)
final_lasso

#최종모델로 train 훈련하기
lasso_fit <- fit(final_lasso, data = ames_train_prepped)

#훈련된 모델로 test 넣어 예측하기
pred_lasso <- 
  predict(lasso_fit, ames_test_prepped) %>% 
  mutate(modelo = "LASSO")

pred_lasso %>% head()

length(ames_test_prepped$sale_price)
nrow(pred_lasso[,1] )
df <- data.frame(ames_test_prepped$sale_price, pred_lasso[,1] )
plot(df)

library(ModelMetrics)
library(caret)
head(df)
caret::R2(df[,1], df[,2])
rsq(df, ames_test_prepped.sale_price, .pred, na_rm = TRUE)
metrics(df, ames_test_prepped.sale_price, .pred) #회귀와 분류 모두 예측 (회귀는 rmse, rsq, mae, 분류는 accuracy, kap)

#(kap : 관찰된 일치 비율에서 우연에 의한 일치 비율을 뺀 값과 평가자들의 평가가 완벽하게 일치할 비율인 '1'에서 우연에 의한 일치 비율을 뺀 값의 비로 정의된다.)

#auc 를 알려면

# two_class_example %>% 
#   roc_auc(truth, Class1)

#roc curve 그리려면
library(yardstick)

data("hpc_cv")
hpc_cv <- as_tibble(hpc_cv)
hpc_cv
str(hpc_cv)
precision(as.factor(hpc_cv[,1]), as.factor(hpc_cv[,2]))
precision(hpc_cv$obs   , hpc_cv$pred     , estimator = "micro")

library(ggplot2)

hpc_cv %>%
  group_by(Resample) %>%
  roc_curve(obs, VF:L) %>%
  autoplot()
