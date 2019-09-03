require(tidyverse)

df = read.csv('params.csv', header = F)

ggplot(df, aes(x = V1)) +
  geom_histogram() +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL)

ggplot(df, aes(x = V2)) +
  geom_histogram() +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL)

ggplot(df, aes(x = V3)) +
  geom_histogram() +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL)

ggplot(df, aes(x = V4)) +
  geom_histogram() +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL)

ggplot(df, aes(x = V5)) +
  geom_histogram() +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL)

ggplot(df, aes(x = V6)) +
  geom_histogram() +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL)+
  scale_x_continuous(breaks = NULL) +
  theme(panel.border = element_rect(size = 3, fill = NA), axis.line = element_blank())

ggplot(df, aes(x = V7)) +
  geom_histogram() +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL)
