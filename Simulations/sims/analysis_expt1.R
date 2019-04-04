# Setup -------------------------------------------------------------------
require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(RColorBrewer)

# only works in Rstudio -- otherwise you have to set the path manually!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)


# Import data -------------------------------------------------------------

# You can change this to whichever simulation you want to analyze.
df <- read.csv('expt1/sims_MB_MB.csv') %>% tbl_df %>% arrange(subject)

df.crits <- df %>%
  mutate(
    last.common = factor(lag(S2) == (lag(Action1) + 1), c(T, F), c('Common', 'Rare')),
    sameS2 = factor(lag(S2) == S2, c(T, F), c("Same S2", "Different S2")),
    stay1 = Action1 == lag(Action1),
    stay2 = Action2 == lag(Action2),
    stay1.fac = factor(Action1 == lag(Action1), c(T, F), c("Same action1", "Different action1")),
    stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
    last.reinf = lag(Re),
    subject_id = as.numeric(subject),
    last.reinf.fac = factor(last.reinf > 0, c(T,F), c("+", "-"))
  ) %>%
  filter(round != min(round, na.rm = T))

# Test 1: Stay1 ~ Common X Reinf ------------------------------------------


# Plot
df.bysubj <- df.crits %>% group_by(last.reinf.fac, last.common, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay1.mean, group = last.common, fill = last.common)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "") +
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(color = 'white', fill = NA))

# Models
model1 <- glmer(stay1 ~ last.reinf * last.common + (1 + last.reinf * last.common | subject),
                family = binomial, data = df.crits, contrasts = list(last.common = contr.sum))
summary(model1)



# Test 2: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Common ----------

# Plot
df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Common') %>%
  group_by(stay1.fac, last.reinf.fac, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 4),
        panel.background = element_rect(color = 'white', fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  coord_cartesian(ylim=c(0,1))

# Model
model2 <- glmer(stay2 ~ last.reinf * stay1.fac + (1 + last.reinf * stay1.fac | subject),
                family = binomial, data = df.crits, contrasts = list(stay1.fac = contr.sum))
summary(model2)


