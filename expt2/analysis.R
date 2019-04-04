# Setup -------------------------------------------------------------------



require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(RColorBrewer)
require(BayesFactor)

# only works in Rstudio -- otherwise you have to set the path manually!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)


# Import data -------------------------------------------------------------


df.demo <- read.csv('demo.csv')
subjlist = df.demo$subject
df <- read.csv('data.csv') %>% tbl_df %>% filter(subject %in% subjlist) %>% arrange(subject) %>% mutate(round = trial_index)

## Exclusion
exclude.subj <- as.character(df.demo$subject[(df.demo$reading_time / 60000) < 1])
exclude.subj <- c(exclude.subj, setdiff(as.character(df.demo$subject[df.demo$belief2 != 0 | df.demo$belief3 != 0]), exclude.subj))
exclude.subj <- c(exclude.subj, 'A1MUEKEQQVROE7') # (s)he glitched
df <- df %>% filter(!(as.character(subject) %in% exclude.subj))

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
    last.reinf.fac = factor(last.reinf >= 0, c(T,F), c("+", "-")),
    stay1.fac.num = as.numeric(stay1.fac),
    stay2.fac.num = as.numeric(stay2.fac),
    last.reinf.fac.num = as.numeric(last.reinf.fac),
    last.common.num = as.numeric(last.common)
  ) %>%
  filter(round != min(round, na.rm = T), practice != 1, timeouts == 0)



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
model1 <- glmer(stay1 ~ last.reinf + (1 + last.reinf | subject), family = binomial,
                   data = df.crits %>% filter(last.common == 'Rare'))
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
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(colour = "white", fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  coord_cartesian(ylim=c(0,1))
  
# Model
model2 <- glmer(stay2 ~ stay1.fac * last.reinf + (1 + stay1.fac * last.reinf | subject), data = df.crits, 
                  family = binomial, contrasts = list(stay1.fac = contr.sum))
summary(model2)



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
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(colour = "white", fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  coord_cartesian(ylim=c(0,1))
  
# Model
model3 <- glmer(stay2 ~ stay1.fac * last.reinf + (1 + stay1.fac * last.reinf | subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare'),
                        contrasts = list(stay1.fac = contr.sum))
summary(model3)
# hard to compute a bayes factor for the mixed-effects model, so we'll do it for the simpler model (where all of each subject's choices are averaged together)
model3.bf = generalTestBF(stay2 ~ stay1.fac * last.reinf,
        data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare') %>% group_by(stay1.fac, last.reinf, subject) %>% summarize(stay2 = mean(stay2)),
        whichModels = "top")
summary(model3.bf)

# Test 4: interaction between 2 & 3 ----------------------------------------------------

df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2') %>%
  group_by(stay1.fac, last.reinf.fac, last.common, subject) %>%
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
  facet_wrap(~ last.common) +
  ylim(0,1)

# have to convert factors to numerical versions for this one to get around lmer bug w/ uncorrelated factor random effects
# (the random effects have to be uncorrelated to get this one to converge)
model4 <- glmer(stay2 ~ stay1.fac.num * last.reinf * last.common.num + (stay1.fac.num*last.reinf*last.common.num||subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2'))
summary(model4)


 # Test 5, 6, and 7: RT  ---------------------------------------------------------------------

# I'm plotting these effects by showing the RT difference b/w choosing the same A2 and different A2
# But the statistics are done over the actual RT, where same vs. different A2 is another factor in the interaction
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
  labs(x = "", y = "") + ylim(-150,300)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(fill = 'white'))+
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  facet_wrap(~ last.common)

# Test after common transition
# for all of these ones w/ three-way interactions or higher, we need uncorrelated random effects to get convergence
model5 = lmer(rt2 ~ stay1.fac.num * stay2.fac.num * last.reinf + (stay1.fac.num * stay2.fac.num * last.reinf || subject),
                     data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Common'))
summary(model5)

# Test after rare transition
model6 = lmer(rt2 ~ stay1.fac.num * stay2.fac.num * last.reinf + (stay1.fac.num * stay2.fac.num * last.reinf || subject),
              data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare'))
summary(model6)

# Test interaction
model7 = lmer(rt2 ~ stay1.fac.num * stay2.fac.num * last.reinf * last.common.num + (stay1.fac.num * stay2.fac.num * last.reinf * last.common.num || subject),
              data = df.crits %>% filter(sameS2 == 'Different S2'))
summary(model7)



# Check whether people are really treating rewards as graded --------------------------------------------------------

# Plot
df.bysubj <- df.crits %>% 
  filter(last.common == 'Common') %>%
  group_by(last.reinf, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.reinf, y = stay1.mean, fill = last.reinf)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "")+coord_cartesian(ylim=c(0,1))+
  theme(#axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(color = 'white', fill = NA))

# Models
model.cont <- glmer(stay1 ~ last.reinf + (1 + last.reinf | subject), family = binomial,
                    data = df.crits %>% filter(last.common == 'Common'))
model.cont.null <- glmer(stay1 ~ last.reinf + (1 + last.reinf.fac | subject), family = binomial,
                         data = df.crits %>% filter(last.common == 'Common'), contrasts = list(last.reinf.fac = contr.sum))
AIC(model.cont) - AIC(model.cont.null)


# Save -------------------------------------------------------

write.table(df.crits %>% select(Action1, S2, Action2, Re, subject_id), 'data_fitting.csv',
              row.names = F, col.names = F, sep = ",")
save.image('analysis.RData')
