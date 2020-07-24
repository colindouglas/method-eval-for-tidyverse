library(tidyverse)

x_variable <- c(0.006, 0.014, 0.025, 0.05, 0.15, 0.3, 0.5)
y_variable <- c(0.4, 0.733333333333333, 0.875, 1, 1, 1, 1)

df <- data.frame(x = x_variable,
                 y = y_variable)


probit_model <- glm(formula = df$y ~ df$x, family = quasibinomial(link = "probit"))

intercept <- coef(probit_model)[[1]]
slope <- coef(probit_model)[[2]]

prediction <- tibble(x = c(0.001, x_variable)) %>%
  mutate(y = predict(probit_model, newdata = ., type = "response"))


# The geom_smooth() layer is correct, according to my reference
# The stat_function _should_ be identical but it isn't
plot <- df %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_line(data = prediction) +
  scale_x_log10(limits = c(0.001, 0.5))


print(plot + geom_smooth(formula = y ~ x, color = "black", fullrange = TRUE,
                     method = "glm", method.args = list(family = quasibinomial(link = "probit"))))
