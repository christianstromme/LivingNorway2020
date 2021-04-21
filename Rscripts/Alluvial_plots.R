#install.packages("ggalluvial")
library("ggalluvial")

#1.1
eng.clmm.graph <- clmm(Values ~ University + Gender + (1|NO), data = ana.data.1.1)
summary(eng.clmm.graph)

head(as.data.frame(UCBAdmissions), n = 12)
is_alluvia_form(as.data.frame(UCBAdmissions), axes = 1:3, silent = TRUE)
ggplot(as.data.frame(UCBAdmissions),
       aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")

scale1 = tibble(numeric_scale = c(1, 2, 3, 4, 5, 6),
                 text_scale = c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week", "I don't know")) %>% 
  mutate(Values = as.factor(numeric_scale),
         text_scale = factor(text_scale, levels = c("Never", "Rarely", "Several times a year", "Several times a month", "Several times a week", "I don't know")))

dat <- ana.data.1.1 %>% 
  group_by(Gender, University, Values) %>% 
  summarise(Freq = n()) %>% 
  left_join(scale1, by = "Values")
ggplot(as.data.frame(dat),
       aes(y = Freq, axis1 = Gender, axis2 = University)) +
  geom_alluvium(aes(fill = text_scale), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "University"), expand = c(.05, .05)) +
  scale_fill_viridis_d(end = 0.9) +
  labs(fill = "")



ggplot(as.data.frame(dat),
       aes(y = Freq,
           axis1 = Gender, axis2 = University)) +
  geom_alluvium(aes(fill = text_scale),
                width = 0, knot.pos = 0, reverse = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2, labels = c("Gender", "University")) +
  scale_fill_viridis_d(end = 0.9) +
  coord_flip() +
  labs(fill = "")

