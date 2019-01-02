# Setup -------------------------------------------------------------------



require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(RColorBrewer)

setwd('/Users/adam/Me/Psychology/Projects/Dezfouli/git')

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)


# Import data -------------------------------------------------------------

real <- T
rewardsAreProbs <- F
if (real) {
  df.demo <- read.csv('Behavioral/2step/v1/data_demo.csv')
  subjlist = df.demo$subject
  df <- read.csv('Behavioral/2step/v1/data.csv') %>% tbl_df %>% filter(subject %in% subjlist) %>% arrange(subject)
} else {
  df <- read.csv('Simulations/sims/2step/real1.csv') %>% tbl_df %>% arrange(subject)
}

## Exclusion
if (real) {
  exclude.subj <- as.character(df.demo$subject[(df.demo$reading_time / 60000) < 1])
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
model.daw.mm <- glmer(stay1 ~ last.reinf.fac * last.common + (1 + last.reinf.fac * last.common | subject),
                      family = binomial, data = df.crits, contrasts = list(last.reinf.fac = contr.sum, last.common = contr.sum))
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
        panel.border = element_rect(colour = "black", fill = NA, size = 4),
        panel.background = element_rect(color = 'white', fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  coord_cartesian(ylim=c(0,1))

# Model
model.test2.mm <- glmer(stay2 ~ last.reinf.fac * stay1.fac + (1 + last.reinf.fac * stay1.fac | subject),
                      family = binomial, data = df.crits, contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum))
summary(model.test2.mm)



# Test 3: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Rare ------------


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
  #labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 4),
        panel.background = element_rect(color = 'white', fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  ylim(0,.75)

# Model
model.test3.mm <- glmer(stay2 ~ last.reinf.fac * stay1.fac + (1 + last.reinf.fac * stay1.fac | subject),
                        family = binomial, data = df.crits)
summary(model.test3.mm)


# Tests 2 & 3 combined ----------------------------------------------------



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
model.test34 <- lm(stay2 ~ stay1.fac * last.reinf.fac * last.common, data = df.bysubj, 
                  contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum, last.common = contr.sum))
summary(model.test34)


# RT2 | common ------------------------------------------------------------

df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Common') %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, subject) %>%
  summarize(rt2 = mean(rt2))

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
  guides(fill = F) + ylim(-100,500) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(fill = 'white'))+
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown"))
  
model.rt = lmer(rt2 ~ stay1.fac * stay2.fac * last.reinf.fac + (1 + stay1.fac * stay2.fac * last.reinf.fac | subject), 
                data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Common'),
                contrasts = list(stay1.fac = contr.sum, stay2.fac = contr.sum, last.reinf.fac = contr.sum))
summary(model.rt)

 

# Check continuity --------------------------------------------------------

# Plot
df.bysubj <- df.crits %>% 
  filter(last.common == 'Common') %>%
  group_by(last.reinf, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.reinf, y = stay1.mean)) +
  geom_point(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = F) +
  geom_smooth(method='lm') +
  labs(x = "", y = "")+coord_cartesian(ylim=c(0,1))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    panel.background = element_rect(color = 'white', fill = NA))

# Models
model.daw.mm <- glmer(stay1 ~ last.reinf.fac + (1 + last.reinf.fac | subject), family = binomial,
                      data = df.crits %>% filter(last.common == 'Rare'), contrasts = list(last.reinf.fac = contr.sum))
summary(model.daw.mm)



# Make modeling CSV -------------------------------------------------------



if (real) {
  write.table(df.crits %>% select(Action1, S2, Action2, Re, subject_id), 'Behavioral/dez_2step/v1/data_fitting.csv',
              row.names = F, col.names = F, sep = ",")
}

# Save --------------------------------------------------------------------

save.image('Simulations/sims/2step_probs/sims_MB_MB.Rdata')
