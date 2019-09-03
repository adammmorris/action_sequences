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
path = 'expt2/elig/sims_MFMB_MB'
df <- read.csv(paste0(path, '.csv')) %>% tbl_df %>% arrange(subject)

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
    last.reinf.fac = factor(last.reinf > 0, c(T,F), c("+", "-")),
    stay1.fac.num = as.numeric(stay1.fac),
    stay2.fac.num = as.numeric(stay2.fac),
    last.reinf.fac.num = as.numeric(last.reinf.fac),
    last.common.num = as.numeric(last.common)
  ) %>%
  filter(round != min(round, na.rm = T))



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
  labs(x = "", y = "")+coord_cartesian(ylim=c(.5, .8))+
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
  coord_cartesian(ylim=c(.2,.8))

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
  coord_cartesian(ylim=c(.2,.8))

# Model
model3 <- glmer(stay2 ~ stay1.fac * last.reinf + (1 + stay1.fac * last.reinf | subject), family = binomial,
                data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare'),
                contrasts = list(stay1.fac = contr.sum))
summary(model3)
model3.null <- glmer(stay2 ~ stay1.fac+ last.reinf + (1 + stay1.fac * last.reinf | subject), family = binomial,
                data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare'),
                contrasts = list(stay1.fac = contr.sum))
exp((BIC(model3) - BIC(model3.null)) / 2)

# Test 4: interaction between 2 & 3 ----------------------------------------------------

df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2') %>%
  group_by(stay1.fac, last.reinf.fac, last.common, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  #guides(fill = F) +
  #labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(colour = "white", fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  facet_wrap(~ last.common)

# have to convert factors to numerical versions for this one to get around lmer bug w/ uncorrelated factor random effects
# (the random effects have to be uncorrelated to get this one to converge)
model4 <- glmer(stay2 ~ stay1.fac.num * last.reinf * last.common.num + (stay1.fac.num*last.reinf*last.common.num||subject), family = binomial,
                data = df.crits %>% filter(sameS2 == 'Different S2'))
summary(model4)

# save analysis -----------------------------------------------------------

save.image(paste0(path, '.rdata'))
