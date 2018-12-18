# Setup -------------------------------------------------------------------



require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(RColorBrewer)
require(BayesFactor)

setwd('/Users/adam/Me/Psychology/Projects/Dezfouli/git')

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)


# Import data -------------------------------------------------------------

real <- T
rewardsAreProbs <- F
if (real) {
  df.demo <- read.csv('Behavioral/1b_fix/v3/real/demo.csv')
  subjlist = df.demo$subject
  df <- read.csv('Behavioral/1b_fix/v3/real/data.csv') %>% tbl_df %>% filter(subject %in% subjlist) %>% arrange(subject) %>% mutate(round = trial_index)
} else {
  df <- read.csv('Simulations/sims/1b_fix/sims_MFMB_MFMB.csv') %>% tbl_df %>% arrange(subject)
}

## Exclusion
if (real) {
  exclude.subj <- as.character(df.demo$subject[(df.demo$reading_time / 60000) < 1])
  exclude.subj <- c(exclude.subj, setdiff(as.character(df.demo$subject[df.demo$belief2 != 0 | df.demo$belief3 != 0]), exclude.subj))
  df <- df %>% filter(!(as.character(subject) %in% exclude.subj))
}

df.crits <- df %>%
  mutate(
    last.common = factor(lag(S2) == (lag(Action1) + 1), c(T, F), c('Common', 'Rare')),
    sameS2 = factor(lag(S2) == S2, c(T, F), c("Same S2", "Different S2")),
    stay1 = Action1 == lag(Action1),
    stay2 = Action2 == lag(Action2),
    stay1.fac = factor(Action1 == lag(Action1), c(T, F), c("Same action1", "Different action1")),
    stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
    last.reinf = lag(Re),
    subject_id = as.numeric(subject)
  ) %>%
  filter(round != min(round, na.rm = T))

if (real | !rewardsAreProbs) {
  df.crits = df.crits %>% mutate(last.reinf.fac = factor(last.reinf >= 0, c(T,F), c("+", "-")))
} else {
  df.crits = df.crits %>% mutate(last.reinf.fac = factor(last.reinf > 0, c(T,F), c("+", "-")))
}

if (real) {
  df.crits = df.crits %>%
    filter(practice != 1 & timeouts == 0)
}



# Test 1: Stay1 ~ Reinf | Last.Common == 'Rare' ---------------------------


# Plot
df.bysubj <- df.crits %>% 
  filter(last.common == 'Rare') %>%
  group_by(last.reinf.fac, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay1.mean, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "")+coord_cartesian(ylim=c(0,1))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(color = 'white', fill = NA))

# Models
model.daw.mm <- glmer(stay1 ~ last.reinf.fac + (1 + last.reinf.fac | subject), family = binomial,
                   data = df.crits %>% filter(last.common == 'Rare'), contrasts = list(last.reinf.fac = contr.sum))
summary(model.daw.mm)



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
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(colour = "white", fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  coord_cartesian(ylim=c(0,1))
  
# Model
model.test2 <- glmer(stay2 ~ stay1.fac * last.reinf.fac + (1 + stay1.fac * last.reinf.fac | subject), data = df.crits, 
                  family = binomial, contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum))
summary(model.test2)


## Test 3: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Rare
# Plot
df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare') %>%
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
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(colour = "white", fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  coord_cartesian(ylim=c(0,1))
  
# Model
model.test3.mm <- glmer(stay2 ~ stay1.fac * last.reinf.fac + (1 + stay1.fac * last.reinf.fac | subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare'),
                        contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum))
summary(model.test3.mm)
anovaBF(stay2 ~ stay1.fac * last.reinf.fac, data = df.bysubj, whichModels = "top")

# Tests 2 & 3 combined ----------------------------------------------------


df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2') %>%
  group_by(stay1.fac, last.reinf.fac, last.common, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = F) +
  #labs(x = "", y = "") +
  theme(#axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        #axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(colour = "white", fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  facet_wrap(~ last.common) +
  ylim(0,1)

# figure this out...
model.test4.mm2 <- glmer(stay2 ~ stay1.fac * last.reinf.fac * last.common + (stay1|subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2'),
                        contrasts = list(stay1.fac = contr.sum, last.reinf.fac = contr.sum, last.common = contr.sum))
model.test4.mm <- glmer(stay2 ~ stay1.fac * last.reinf.fac * last.common + (1+stay1.fac * last.reinf.fac * last.common||subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2'), contrasts = list(stay1.fac = contr.sum, last.reinf.fac = contr.sum, last.common = contr.sum))

summary(model.test4.mm2)


# RT2 ---------------------------------------------------------------------



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

# differences

df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2' & !is.na(last.reinf.fac)) %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, last.common, subject) %>%
  summarize(rt2 = mean(rt2))

# differences version
df.bysubj2 = df.bysubj %>% filter(stay2.fac == 'Same action2')
for (i in 1:nrow(df.bysubj2)) {
  other = df.bysubj$subject == df.bysubj2$subject[i] & df.bysubj$last.reinf.fac == df.bysubj2$last.reinf.fac[i] & df.bysubj$stay1.fac == df.bysubj2$stay1.fac[i] & df.bysubj$stay2.fac == 'Different action2'
  if (any(other)) {
    df.bysubj2$rt2.diff[i] = df.bysubj$rt2[other] - df.bysubj2$rt2[i] 
  }
}

df.agg <- df.bysubj2 %>%
  summarize(rt2.mean = mean(rt2.diff), rt2.se = se(rt2.diff))

ggplot(df.agg, aes(x = last.reinf.fac, y = rt2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "") + ylim(-150,250)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(fill = 'white'))+
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  facet_wrap(~ last.common)

# model.rt = lmer(rt2 ~ stay1.fac * stay2.fac * last.reinf.fac + (1 + stay1.fac * stay2.fac * last.reinf.fac | subject), data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Common'))
# summary(model.rt)

model.rt.rare = lm(rt2.diff ~ stay1.fac * last.pe2.fac, data = df.bysubj2 %>% filter(last.common == 'Rare'))
model.rt.rare.null = lm(rt2.diff ~ stay1.fac + last.reinf.fac, data = df.bysubj2 %>% filter(last.common == 'Rare'))
summary(model.rt.rare)
exp((BIC(model.rt.rare) - BIC(model.rt.rare.null)) / 2)

model.rt.common = lm(rt2.diff ~ stay1.fac * last.pe2.fac, data = df.bysubj2 %>% filter(last.common == 'Common'))
summary(model.rt.common)

model.rt = lm(rt2.diff ~ stay1.fac * last.reinf.fac * last.common, data = df.bysubj2)
summary(model.rt)


# Save for modeling -------------------------------------------------------


if (real) {
  write.table(df.crits %>% select(Action1, S2, Action2, Re, subject_id), 'Behavioral/1b_fix/v3/real/data_fitting.csv',
              row.names = F, col.names = F, sep = ",")
}
save.image('Behavioral/1b_fix/v3/real/analysis.RData')
