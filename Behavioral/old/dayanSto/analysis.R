require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)

df <- read.csv('sims_db_MB_MF.csv') %>% tbl_df
df$rt2 <- as.numeric(as.character(df$rt2))
df$rt2[is.na(df$rt2)] <- mean(df$rt2, rm.na = T)
df <- df %>% filter(rt1 > 500 & rt1 < 10000 & rt2 > 200 & rt2 < 5000)
df$rt1 <- log(df$rt1)
df$rt2 <- log(df$rt2)

df.crits <- df %>%
  mutate(stay1 = Action == lag(Action),
         stay2 = Action2 == lag(Action2),
         stay1.fac = factor(Action == lag(Action), c(T, F), c("Same action1", "Different action1")),
         stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
         reinforcement = factor(lag(Re) > 0, c(T, F), c("Rewarded", "Punished")),
         reinf_abs = abs(lag(Re)),
         rt1 = rt1 - mean(rt1),
         rt2 = rt2 - mean(rt2)) %>%
  filter(lag(Action2) == 1)

se <- function(x) {sd(x) / sqrt(length(x))};

# Fig 5
df.bysubj <- df.crits %>% group_by(reinforcement, stay1.fac, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

dodge <- position_dodge(width=0.9)
ggplot(df.agg, aes(x = reinforcement, y = stay2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Stage 1 choice")) +
  labs(x = "", y = "Prob of choosing middle again")

model <- glmer(stay2 ~ stay1.fac * reinforcement * reinf_abs + (stay1.fac * reinforcement * reinf_abs | subject), family = binomial,
               data = df.crits)
model2 <- glmer(stay2 ~ stay1.fac * reinforcement + reinf_abs + (stay1.fac * reinforcement + reinf_abs | subject), family = binomial,
               data = df.crits)
# Fig 7
df.bysubj.rt <- df.crits %>% group_by(reinforcement, stay1.fac, stay2.fac, subject) %>%
  summarize(rt1 = mean(rt1), rt2 = mean(rt2))
df.agg.rt <- df.bysubj.rt %>%
  summarize(rt1.mean = mean(rt1), rt1.se = se(rt1),
            rt2.mean = mean(rt2), rt2.se = se(rt2))

ggplot(df.agg.rt, aes(x = reinforcement, y = rt2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Stage 1 choice")) +
  labs(x = "", y = "RT") +
  facet_wrap(~ stay2.fac)