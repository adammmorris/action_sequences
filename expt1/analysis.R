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

df.demo <- read.csv('demo.csv')
subjlist = df.demo$subject
df <- read.csv('data.csv') %>% tbl_df %>% filter(subject %in% subjlist) %>% arrange(subject)

## Exclusion
exclude.subj <- as.character(df.demo$subject[(df.demo$reading_time / 60000) < 1])
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
    last.reinf.fac = factor(last.reinf >= 0, c(T,F), c("+", "-"))
  ) %>%
  filter(round != min(round, na.rm = T), practice != 1, timeouts == 0)

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



# Test 3: RT ------------------------------------------------------------

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
  
model3 = lmer(rt2 ~ stay1.fac * stay2.fac * last.reinf + (1 + stay1.fac * stay2.fac * last.reinf | subject), 
                data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Common'),
                contrasts = list(stay1.fac = contr.sum, stay2.fac = contr.sum))
summary(model3)

# Show RT distribution
ggplot(df.crits, aes(x = rt2, group = stay1.fac, fill = stay1.fac)) +
  geom_histogram(alpha = .6, position = 'identity') +
  xlab('') + ylab('') +
  scale_y_continuous(labels = NULL, breaks = NULL) +
  scale_x_continuous(breaks = c(0,1000,2000), limits = c(0,2200)) +
  facet_wrap(~ last.common + last.reinf.fac) +
  theme(strip.text.x = element_blank(), legend.position = 'none') +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown"))


# Check whether people are treating rewards as graded --------------------------------------------------------

# Plot
df.bysubj <- df.crits %>% 
  filter(last.common == 'Common') %>%
  group_by(last.reinf, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.reinf, y = stay1.mean)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = F) +
  #geom_smooth(method='lm') +
  labs(x = "", y = "")+coord_cartesian(ylim=c(0,1))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 2),
    panel.background = element_rect(color = 'white', fill = NA))

# Models
model.cont <- glmer(stay1 ~ last.reinf + (1 + last.reinf | subject), family = binomial,
                         data = df.crits %>% filter(last.common == 'Common'))
model.cont.null <- glmer(stay1 ~ last.reinf.fac + (1 + last.reinf.fac | subject), family = binomial,
                      data = df.crits %>% filter(last.common == 'Common'), contrasts = list(last.reinf.fac = contr.sum))
AIC(model.cont) - AIC(model.cont.null)


# Make CSV for model fitting -------------------------------------------------------

write.table(df.crits %>% select(Action1, S2, Action2, Re, subject_id), 'expt1/data_fitting.csv',
            row.names = F, col.names = F, sep = ",")

# Save --------------------------------------------------------------------

save.image('analysis.RData')
