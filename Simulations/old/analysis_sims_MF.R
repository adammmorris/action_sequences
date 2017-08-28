require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

df <- read.csv('sims.csv') %>% tbl_df

df.crits <- df %>% 
  mutate(last.common = factor(lag(S2) == (lag(Action1) + 1), c(T, F), c("Common", "Rare")),
         stay1 = Action1 == lag(Action1),
         last.re = lag(Re),
         last.reinf = factor(ifelse(lag(Re) == 0, 0, ifelse(lag(Re) > 0, 1, -1)), c(1, 0, -1), c("+", "0", "-")),
         last.reinf.abs = abs(lag(Re)),
         re.fac = factor(lag(Re)),
         chose.best1 = factor(lag(Action1) == lag(bestA1), c(T, F), c('Best1', 'Other1')),
         chose.best2 = factor(lag(Action2) == lag(bestA2), c(T, F), c('Best2', 'Other2'))) %>%
  filter(round != 1)

# Stay1 ~ Common X Reinf
df.bysubj <- df.crits %>% group_by(last.reinf, chose.best1, chose.best2, last.common, subject) %>%
  filter(last.reinf != '0') %>%
  summarize(stay1 = mean(stay1), re.abs = mean(last.reinf.abs), Q = mean(Q), PE = mean(PE))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1), re.abs = mean(re.abs), Q = mean(Q), PE = mean(PE))

ggplot(df.agg, aes(x = last.reinf, y = stay1.mean, group = last.common, fill = last.common)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "")) +
  labs(x = "", y = "Prob of repeating action 1") +
  facet_wrap(~ chose.best1 + chose.best2)

# Q vals
ggplot(df.agg, aes(x = last.reinf, y = Q, group = last.common, fill = last.common)) +
  geom_bar(stat = "identity", position = dodge) +
  guides(fill = guide_legend(title = "")) +
  labs(x = "", y = "Prob of repeating action 1") +
  facet_wrap(~ chose.best1 + chose.best2)

# PEs
ggplot(df.agg, aes(x = last.reinf, y = PE, group = last.common, fill = last.common)) +
  geom_bar(stat = "identity", position = dodge) +
  guides(fill = guide_legend(title = "")) +
  labs(x = "", y = "Prob of repeating action 1") +
  facet_wrap(~ chose.best1 + chose.best2)

model <- glmer(stay1 ~ last.re * last.common + (1 + last.re * last.common | subject), family = binomial, data = df.crits)
model2 <- lm(stay1 ~ last.reinf * last.common * last.reinf.abs, data = df.bysubj)
