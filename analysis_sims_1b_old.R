require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

#----- Setup -----#
real <- T
stem = 'Behavioral/1b_fix/v3/real/'

if (real) {
  df <- read.csv(paste0(stem, 'data.csv')) %>% tbl_df
  df.demo <- read.csv(paste0(stem, 'demo.csv'))
} else {
  df <- read.csv('Simulations/data/1b_fix/sims_MFMB_MF.csv') %>% tbl_df
}
df <- df %>% arrange(subject)
df$subject <- trimws(as.character(df$subject))
df$subject <- ifelse(regexpr('\n', df$subject) > -1, substr(df$subject, 1, regexpr('\n', df$subject) - 1), df$subject)
df.demo$subject <- trimws(as.character(df.demo$subject))
df.demo$subject <- ifelse(regexpr('\n', df.demo$subject) > -1, substr(df.demo$subject, 1, regexpr('\n', df.demo$subject) - 1), df.demo$subject)

nstates = length(unique(df$S2)) + 1
nactions = 2
curvals = matrix(nrow = nstates, ncol = nactions)
for (i in 1:nrow(df)) {
  df$prev1[i] = curvals[1,df$Action1[i]]
  df$prev2[i] = curvals[df$S2[i],df$Action2[i]]
  
  curvals[1,df$Action1[i]] = df$Re[i]
  curvals[df$S2[i],df$Action2[i]] = df$Re[i]
  
  if (df$S2[i] == 4) {
    otherAction = ifelse(df$Action2[i] == 1, 2, 1)
    curvals[df$S2[i],otherAction] = df$Re[i]
  }
  
  if (i < nrow(df) && df$subject[i] != df$subject[i+1]) {
    curvals = matrix(nrow = nstates, ncol = nactions)
  }
}

# Make proper df
if (real) {
  df.crits <- df %>% 
    mutate(
      last.common = factor(lag(S2) != 4, c(T, F), c('Common', 'Rare')),
      sameS2 = factor(lag(S2) == S2, c(T, F), c("Same S2", "Different S2")),
      stay1 = Action1 == lag(Action1),
      stay2 = Action2 == lag(Action2),
      stay1.fac = factor(Action1 == lag(Action1), c(T, F), c("Same action1", "Different action1")),
      stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
      last.reinf = lag(Re),
      last.reinf.fac = factor(ifelse(lag(Re) == 0, 0, ifelse(lag(Re) > 0, 1, -1)), c(1, 0, -1), c("+", "0", "-")),
      last.reinf.fac = factor(last.reinf >= 0, c(T,F), c("+", "-")), last.reinf.abs = abs(lag(Re)), last.reinf.abs = abs(lag(Re)),
      last.pe1 = lag(Re) - lag(prev1),
      last.pe2 = lag(Re) - lag(prev2),
      last.pe1.fac = factor(last.pe1 >= 0, c(T,F), c("+", "-")),
      last.pe2.fac = factor(last.pe1 >= 0, c(T,F), c("+", "-")),
      subject_id = as.numeric(as.factor(subject))
    ) %>%
    filter(practice != 1 & timeouts == 0)
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
      #last.reinf.fac = factor(ifelse(lag(Re) == 0, 0, ifelse(lag(Re) > 0, 1, -1)), c(1, 0, -1), c("+", "0", "-")),
      last.reinf.fac = factor(last.reinf >= 0, c(T,F), c("+", "-")),      last.reinf.abs = abs(lag(Re)),      last.reinf.abs = abs(lag(Re))
    ) %>%
    filter(round != min(round, na.rm = T))
}

# Exclusion
if (real) {
  exclude.subj = c()
  
  # people who glitched
  #exclude.subj <- c('AY1UTM5SDLH52', 'A4BKCA4S49HU', 'ANV8C2DATCAPB', 'ASTR3EPUOKEXV', 'A87D3K9YVKTQ7', 'A87D3K9YVKTQ7', 'A1PUHCEBSOWETV', 'A2TNZ0EUJZ5NK2')
  
  # people who didn't read the instructions
  exclude.subj <- c(exclude.subj, df.demo$subject[(df.demo$reading_time / 60000) < 1])
 
  # people who stayed the whole time
  exclude.subj <- c(exclude.subj, (df.crits %>%
                      group_by(subject) %>%
                      summarize(stay1 = mean(stay1), stay2 = mean(stay2),
                                numPos_rare = sum(last.common == 'Rare' & last.reinf.fac == '+'),
                                numNeg_rare = sum(last.common == 'Rare' & last.reinf.fac == '-')) %>%
                      filter(stay1 == 1 | stay2 == 1))$subject)

  # people who failed critical checks (or never got to submit demo)
  exclude.subj <- c(exclude.subj, setdiff(df$subject, df.demo$subject))
  
  df.demo = df.demo %>% mutate(numFailedChecks = (check1 != 0)+(check2 != 1)+ (check3 != 2))
  #exclude.subj <- c(exclude.subj, df.demo$subject[df.demo$check1 != 0 | df.demo$check2 != 1 | df.demo$check3 != 2 |
  #                                                  df.demo$belief2 != 0 | df.demo$belief3 != 0])
  exclude.subj = c(exclude.subj, df.demo$subject[df.demo$belief2 != 0 | df.demo$belief3 != 0])
  
  df.crits <- df.crits %>% filter(!(df.crits$subject %in% exclude.subj))
}

# Do any special things you need to do for this data set
# if (real) {
#   df.crits$rt1[df.crits$rt1 == 0] <- .Machine$double.xmin
#   df.crits$rt1 <- log(df.crits$rt1)
#   df.crits$rt1 <- df.crits$rt1 - mean(df.crits$rt1)
#   df.crits$rt2[df.crits$rt2 == 0] <- .Machine$double.xmin
#   df.crits$rt2 <- log(df.crits$rt2)
#   df.crits$rt2 <- df.crits$rt2 - mean(df.crits$rt2)
#   
#   #df$Action1[df$Action1 %in% c(3, 4)] <- df$Action1[df$Action1 %in% c(3, 4)] - 2
#   #df$Action2[df$Action2 %in% c(3, 4)] <- df$Action2[df$Action2 %in% c(3, 4)] - 2
#   #df$Action2[df$Action2 %in% c(5, 6)] <- df$Action2[df$Action2 %in% c(5, 6)] - 4
# }

#----- Tests -----#

## Test 1: Stay1 ~ Reinf | Last.Common == 'Rare'
# Plot
df.bysubj <- df.crits %>% 
  filter(last.common == 'Rare') %>%
  group_by(last.pe1.fac, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.pe1.fac, y = stay1.mean, fill = last.pe1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "")+ylim(0,1)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(color = 'white', fill = NA))

# Models
model.daw.mm <- glmer(stay1 ~ last.reinf.fac + (1 + last.reinf.fac | subject), family = binomial,
                   data = df.crits %>% filter(last.common == 'Rare'))
# model.daw <- lm(stay1 ~ last.reinf.fac, data = df.bysubj,
#                  contrasts = list(last.reinf.fac = contr.sum, last.common = contr.sum))
summary(model.daw.mm)


## Test 2: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Common
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
  #labs(x = "", y = "") +
  theme(#axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        #axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(colour = "white", fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  ylim(0,1)
  
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
  ylim(0,1)
  
# Model
model.test3.mm <- glmer(stay2 ~ stay1 * last.reinf + (1 + stay1 * last.reinf | subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare'),
                        control = glmerControl(optimizer = "bobyqa"))
model.test3.null <- glmer(stay2 ~ stay1 + last.reinf + (1 + stay1 * last.reinf | subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Rare'),
                        control = glmerControl(optimizer = "bobyqa"))
summary(model.test3.mm)
anova(model.test3.mm, model.test3.null)

exp((BIC(model.test3.mm) - BIC(model.test3.null)) / 2)

## Tests 2 & 3 combined
df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2') %>%
  group_by(stay1.fac, last.pe2.fac, last.common, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = last.pe2.fac, y = stay2.mean, group = stay1.fac, fill = stay1.fac)) +
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
model.test4.mm <- glmer(stay2 ~ stay1 * last.reinf * last.common + (1 | subject) + (stay1|subject) + (last.reinf|subject) +
                          (last.common|subject) +(stay1:last.common|subject)+(stay1:last.reinf|subject)+
                          (last.reinf:last.common|subject)+(stay1:last.reinf:last.common|subject), family = binomial,
                        data = df.crits %>% filter(sameS2 == 'Different S2'))
model.test4.null <- glmer(stay2 ~ stay1 + last.reinf + last.common + stay1:last.common + stay1:last.reinf + last.reinf:last.common +
                            (1 + stay1 * last.reinf * last.common | subject), family = binomial,
                          data = df.crits %>% filter(sameS2 == 'Different S2'),
                          control = glmerControl(optimizer = "bobyqa"))
summary(model.test4.mm)
anova(model.test4.mm, model.test4.null)

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

# differences

df.bysubj <- df.crits %>% filter(sameS2 == 'Different S2' & !is.na(last.pe2.fac)) %>%
  group_by(stay1.fac, last.pe2.fac, stay2.fac, last.common, subject) %>%
  summarize(rt2 = mean(rt2))

# differences version
df.bysubj2 = df.bysubj %>% filter(stay2.fac == 'Same action2')
for (i in 1:nrow(df.bysubj2)) {
  other = df.bysubj$subject == df.bysubj2$subject[i] & df.bysubj$last.pe2.fac == df.bysubj2$last.pe2.fac[i] & df.bysubj$stay1.fac == df.bysubj2$stay1.fac[i] & df.bysubj$stay2.fac == 'Different action2'
  if (any(other)) {
    df.bysubj2$rt2.diff[i] = df.bysubj$rt2[other] - df.bysubj2$rt2[i] 
  }
}

df.agg <- df.bysubj2 %>%
  summarize(rt2.mean = mean(rt2.diff), rt2.se = se(rt2.diff))

ggplot(df.agg, aes(x = last.pe2.fac, y = rt2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "") + ylim(-150,200)+
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

## Modeling

if (real) {
  write.table(df.crits %>% select(Action1, S2, Action2, Re, subject_id), paste0(stem, 'data_fitting.csv'),
              row.names = F, col.names = F, sep = ",")
}

## Bonuses
write.table(df.demo %>% select(WorkerID = subject, Bonus = bonus),
            paste0(stem, 'Bonus.csv'), row.names = FALSE, col.names = FALSE, sep = ",")