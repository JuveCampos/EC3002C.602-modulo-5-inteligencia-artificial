# =====================================================
# Comparación de OLS, Ridge y Lasso
# Predicción de ingreso mensual (datos simulados)
# =====================================================

# 1. Paquetes -------------------------------------------------------------
library(tidyverse)
library(glmnet)
library(broom)  # para extraer coeficientes ordenados de OLS

set.seed(123)

# 2. Simulación de datos --------------------------------------------------
n <- 500  # número de personas

educacion <- rnorm(n, mean = 12, sd = 3)              # años de educación formal
experiencia <- educacion + rnorm(n, 5, 2)             # años de experiencia (correlacionada)
edad <- experiencia + rnorm(n, 25, 5)                 # edad (también correlacionada)
horas_semana <- rnorm(n, mean = 45, sd = 10)          # horas trabajadas por semana
genero <- rbinom(n, 1, 0.45)                          # 1 = mujer, 0 = hombre
sector_publico <- rbinom(n, 1, 0.3)                   # sector público
region_norte <- rbinom(n, 1, 0.4)
region_sur <- rbinom(n, 1, 0.3)
ingles <- rbinom(n, 1, 0.5)                           # habla inglés

# Variable respuesta (ingreso mensual)
ingreso <- 3500 + 
  800 * educacion + 
  400 * experiencia + 
  100 * horas_semana - 
  2500 * genero + 
  1500 * ingles + 
  rnorm(n, 0, 10000)  # ruido aleatorio

datos <- tibble(
  ingreso, educacion, experiencia, edad,
  horas_semana, genero, sector_publico,
  region_norte, region_sur, ingles
)

# 3. Entrenamiento / prueba ----------------------------------------------
set.seed(123)
n_train <- round(0.7 * n)
id_train <- sample(1:n, size = n_train)
id_test  <- setdiff(1:n, id_train)

train <- datos[id_train, ]
test  <- datos[id_test, ]

X_train <- model.matrix(ingreso ~ ., data = train)[, -1]
y_train <- train$ingreso

X_test  <- model.matrix(ingreso ~ ., data = test)[, -1]
y_test  <- test$ingreso

# 4. OLS (regresión lineal clásica) --------------------------------------
ols_fit <- lm(ingreso ~ ., data = train)
ols_pred <- predict(ols_fit, newdata = test)

# 5. Ridge ---------------------------------------------------------------
ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0, standardize = TRUE)
ridge_lambda_opt <- ridge_cv$lambda.min
ridge_fit <- glmnet(X_train, y_train, alpha = 0, lambda = ridge_lambda_opt)
ridge_pred <- predict(ridge_fit, newx = X_test)

# 6. Lasso ---------------------------------------------------------------
lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, standardize = TRUE)
lasso_lambda_opt <- lasso_cv$lambda.min
lasso_fit <- glmnet(X_train, y_train, alpha = 1, lambda = lasso_lambda_opt)
lasso_pred <- predict(lasso_fit, newx = X_test)

# 7. Función RMSE --------------------------------------------------------
rmse <- function(real, pred) sqrt(mean((real - pred)^2))

rmse_ols <- rmse(y_test, ols_pred)
rmse_ridge <- rmse(y_test, ridge_pred)
rmse_lasso <- rmse(y_test, lasso_pred)

# 8. Comparación de desempeño --------------------------------------------
desempeno <- tibble(
  Modelo = c("OLS", "Ridge", "Lasso"),
  RMSE = c(rmse_ols, rmse_ridge, rmse_lasso)
)
desempeno

# 9. Coeficientes comparados ---------------------------------------------
# ridge_coef <- coef(ridge_fit) %>% as.matrix() %>% as_tibble(rownames = "variable") %>% rename(ridge = s1)
# lasso_coef <- coef(lasso_fit) %>% as.matrix() %>% as_tibble(rownames = "variable") %>% rename(lasso = s1)

ridge_coef <- coef(ridge_fit) %>%
  as.matrix() %>%
  as_tibble(rownames = "variable") %>%
  rename(ridge = 2)

lasso_coef <- coef(lasso_fit) %>%
  as.matrix() %>%
  as_tibble(rownames = "variable") %>%
  rename(lasso = 2)


ols_coef <- tidy(ols_fit) %>% select(term, estimate) %>% rename(variable = term, ols = estimate)

# Unimos todos los coeficientes
coef_comparados <- full_join(ols_coef, ridge_coef, by = "variable") %>%
  full_join(lasso_coef, by = "variable") %>%
  mutate(across(c(ols, ridge, lasso), ~round(., 2)))

coef_comparados

# 10. Visualización (opcional) -------------------------------------------
coef_comparados %>%
  filter(variable != "(Intercept)") %>%
  pivot_longer(cols = c(ols, ridge, lasso), names_to = "modelo", values_to = "coef") %>%
  ggplot(aes(x = variable, y = coef, fill = modelo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparación de coeficientes: OLS vs Ridge vs Lasso",
    y = "Coeficiente estimado", x = "Variable explicativa"
  ) +
  theme_minimal()

