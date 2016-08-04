require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

#----- Setup -----#
real <- T

if (real) {
  df <- read.csv('Behavioral/old/1b/data.csv') %>% tbl_df
} else {
  df <- read.csv('Simulations/data/1b/sims_MBAS.csv') %>% tbl_df
}

## Exclusion
if (real) {
  #df.demo <- read.csv('Behavioral/dez_2step/v1/data_demo.csv')
  #exclude.subj <- df.demo$subject[(df.demo$reading_time / 60000) < 1]
  #df <- df %>% filter(!(df$subject %in% exclude.subj))
}

# Do any special things you need to do for this data set
if (real) {
  df$rt1 <- log(df$rt1)
  df$rt1 <- df$rt1 - mean(df$rt1)
  df$rt2[df$rt2 == 0] <- .Machine$double.xmin
  df$rt2 <- log(df$rt2)
  df$rt2 <- df$rt2 - mean(df$rt2)
}
df$Action1[df$Action1 %in% c(3, 4)] <- df$Action1[df$Action1 %in% c(3, 4)] - 2
df$Action2[df$Action2 %in% c(3, 4)] <- df$Action2[df$Action2 %in% c(3, 4)] - 2
df$Action2[df$Action2 %in% c(5, 6)] <- df$Action2[df$Action2 %in% c(5, 6)] - 4
#df$S2 <- df$S2 - 1

if (real) {
  df.crits <- df %>% 
    mutate(
      last.common = factor(lag(S2) != 5, c(T, F), c('Common', 'Rare')),
      sameS2 = factor(lag(S2) == S2, c(T, F), c("Same S2", "Different S2")),
      stay1 = Action1 == lag(Action1),
      stay2 = Action2 == lag(Action2),
      stay1.fac = factor(Action1 == lag(Action1), c(T, F), c("Same action1", "Different action1")),
      stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
      rt1.fac = factor(rt1 < median(rt1), c(T, F), c("RT1 fast", "RT1 slow")),
      last.reinf = lag(Re),
      last.reinf.fac = factor(ifelse(lag(Re) == 0, 0, ifelse(lag(Re) > 0, 1, -1)), c(1, 0, -1), c("+", "0", "-")),
      last.reinf.abs = abs(lag(Re))
      ) %>%
   filter(round > 76)
} else {
  df.crits <- df %>% 
    mutate(
      last.common = factor(lag(S2) != 4, c(T, F), c("Common", "Rare")),
      sameS2 = factor(lag(S2) == S2, c(T, F), c("Same S2", "Different S2")),
      stay1 = Action1 == lag(Action1),
      stay2 = Action2 == lag(Action2),
      stay1.fac = factor(Action1 == lag(Action1), c(T, F), c("Same action1", "Different action1")),
      stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
      last.reinf = lag(Re),
      last.reinf.fac = factor(ifelse(lag(Re) == 0, 0, ifelse(lag(Re) > 0, 1, -1)), c(1, 0, -1), c("+", "0", "-")),
      last.reinf.abs = abs(lag(Re))
      ) %>%
    filter(round != min(round, na.rm = T))
}

#----- Tests -----#

## Test 1: Stay1 ~ Reinf | Last.Common == 'Rare'
# Plot
df.bysubj <- df.crits %>% 
  filter(last.reinf.fac != '0' & last.common == 'Rare') %>%
  group_by(last.reinf.fac, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay1.mean, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "")) +
  labs(x = "", y = "Prob of repeating action 1")

# Models
#model.daw.mm <- glmer(stay1 ~ last.reinf.fac * last.common + (1 + last.reinf.fac * last.common | subject), family = binomial,
#                   data = df.crits)
model.daw <- lm(stay1 ~ last.reinf.fac, data = df.bysubj,
                 contrasts = list(last.reinf.fac = contr.sum, last.common = contr.sum))
summary(model.daw)


## Test 2: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Common
# Plot
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Common') %>%
  group_by(stay1.fac, last.reinf.fac, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = stay1.fac, y = stay2.mean, group = last.reinf.fac, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Last reinforcement")) +
  labs(x = "", y = "Prob of repeating action 2")

# Model
model.test2 <- lm(stay2 ~ stay1.fac * last.reinf.fac, data = df.bysubj, 
                  contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum))
summary(model.test2)


## Test 3: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Rare
# Plot
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Rare') %>%
  group_by(stay1.fac, last.reinf.fac, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = stay1.fac, y = stay2.mean, group = last.reinf.fac, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Last reinforcement")) +
  labs(x = "", y = "Prob of repeating action 2")

# Model
model.test3 <- lm(stay2 ~ stay1.fac * last.reinf.fac, data = df.bysubj, 
                  contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum))
summary(model.test3)


## Tests 2 & 3 combined
# Plot
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2') %>%
  group_by(stay1.fac, last.reinf.fac, last.common, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = stay1.fac, y = stay2.mean, group = last.reinf.fac, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Last reinforcement")) +
  labs(x = "", y = "Prob of repeating action 2") +
  facet_wrap(~ last.common)

# Model
model.test23 <- lm(stay2 ~ stay1.fac * last.reinf.fac * last.common, data = df.bysubj, 
                  contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum, last.common = contr.sum))
summary(model.test23)


## RT2
# After common
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Common') %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, subject) %>%
  summarize(rt2 = mean(rt2))

df.agg <- df.bysubj %>%
  summarize(rt2.mean = mean(rt2), rt2.se = se(rt2))

ggplot(df.agg, aes(x = stay1.fac, y = rt2.mean, group = stay2.fac, fill = stay2.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Stage 2 choice")) +
  labs(x = "", y = "RT of action 2") +
  facet_wrap(~ last.reinf.fac)

# After rare
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Rare') %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, subject) %>%
  summarize(rt2 = mean(rt2))

df.agg <- df.bysubj %>%
  summarize(rt2.mean = mean(rt2), rt2.se = se(rt2))

ggplot(df.agg, aes(x = stay1.fac, y = rt2.mean, group = stay2.fac, fill = stay2.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Stage 2 choice")) +
  labs(x = "", y = "RT of action 2") +
  facet_wrap(~ last.reinf.fac)


## RT1
# Original daw plot, split by RT1
df.bysubj <- df.crits %>% group_by(last.reinf.fac, last.common, rt1.fac, subject) %>%
  filter(last.reinf.fac != '0') %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay1.mean, group = last.common, fill = last.common)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "")) +
  labs(x = "", y = "Prob of repeating action 1") +
  facet_wrap(~ rt1.fac)

# Combined Stay2 stuff, split by RT1.fac
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2') %>%
  group_by(stay1.fac, last.reinf.fac, last.common, rt1.fac, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = stay1.fac, y = stay2.mean, group = last.reinf.fac, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Last reinforcement")) +
  labs(x = "", y = "Prob of repeating action 2") +
  facet_wrap(~ rt1.fac + last.common)

# RT2 after common, split by RT1
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Common') %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, rt1.fac, subject) %>%
  summarize(rt2 = mean(rt2))

df.agg <- df.bysubj %>%
  summarize(rt2.mean = mean(rt2), rt2.se = se(rt2))

ggplot(df.agg, aes(x = stay1.fac, y = rt2.mean, group = stay2.fac, fill = stay2.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Stage 2 choice")) +
  labs(x = "", y = "RT of action 2") +
  facet_wrap(~ rt1.fac + last.reinf.fac)

# RT2 after rare, split by RT1
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Rare') %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, rt1.fac, subject) %>%
  summarize(rt2 = mean(rt2))

df.agg <- df.bysubj %>%
  summarize(rt2.mean = mean(rt2), rt2.se = se(rt2))

ggplot(df.agg, aes(x = stay1.fac, y = rt2.mean, group = stay2.fac, fill = stay2.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Stage 2 choice")) +
  labs(x = "", y = "RT of action 2") +
  facet_wrap(~ rt1.fac + last.reinf.fac)