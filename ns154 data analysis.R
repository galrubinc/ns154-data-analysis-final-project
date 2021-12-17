#importing library for power calculation
library(pwr)

#difference of means test for experiment 2- 500 MLs
control_2 <- c(3.12,
             4.5,
             2.89,
             3.17,
             4.755,
             3.06,
             3.23,
             4.81,
             3.575,
             3.355,
             5.11,
             3.825,
             3.685,
             5.245,
             4.1)

treatment_2 <- c(2.015,
               1.355,
               1.86,
               2.61,
               1.43,
               2.15,
               2.785,
               1.48,
               2.21,
               3.18,
               1.52,
               2.385,
               3.455,
               2.15,
               2.815
)


cohens_d <- (mean(control_2) - mean(treatment_2))/sqrt((sd(treatment_2)**2 + sd(control_2)**2)/2) * ((length(control_2)-3)/(length(control_2) - 2.25)) * (sqrt((length(control_2) - 2)/length(control_2)))

pwr.t.test(d= cohens_d, sig.level=0.05, power= 0.80, type="two.sample", alternative="two.sided")

t.test(control_2, treatment_2, alternative = "two.sided")

#difference of means test for experiment 1 (175 MLs)
control_1 <- c(15.5,
               12.5,
               22.5,
               17,
               14.5,
               26,
               19,
               16.5,
               28,
               20.5,
               17.5,
               30,
               24.5,
               18.5,
               32,
               28.5,
               20.5,
               33,
               35,
               22,
               36)

treatment_1 <- c(19.5,
                 15.5,
                 8.5,
                 21.5,
                 17.5,
                 9.5,
                 23,
                 18.5,
                 11,
                 24.5,
                 20,
                 12,
                 27,
                 21.5,
                 13,
                 29.5,
                 23,
                 14.5,
                 33,
                 24,
                 16
)


cohens_d_1 <- (mean(control_1) - mean(treatment_1))/sqrt((sd(treatment_1)**2 + sd(control_1)**2)/2) * ((length(control_1)-3)/(length(control_1) - 2.25)) * (sqrt((length(control_1) - 2)/length(control_1)))

pwr.t.test(d= cohens_d_1, sig.level=0.05, power= 0.80, type="two.sample", alternative="two.sided")

t.test(control_1, treatment_1, alternative = "two.sided")


#difference of means test for experiment 2- 500 MLs- with only minimum and maximum timepoints
control_2s <- c(3.12,
               4.5,
               2.89,
               3.685,
               5.245,
               4.1)

treatment_2s <- c(2.015,
                 1.355,
                 1.86,
                 3.455,
                 2.15,
                 2.815)


cohens_ds <- (mean(control_2s) - mean(treatment_2s))/sqrt((sd(treatment_2s)**2 + sd(control_2s)**2)/2) * ((length(control_2s)-3)/(length(control_2s) - 2.25)) * (sqrt((length(control_2s) - 2)/length(control_2s)))

pwr.t.test(d= cohens_ds, sig.level=0.05, power= 0.80, type="two.sample", alternative="two.sided")

t.test(control_2s, treatment_2s, alternative = "two.sided")

cohens_ds

#difference of means test for experiment 1-175 MLs- with only minimum and maximum timepoints
control_1s <- c(15.5,
                12.5,
                22.5,
                35,
                22,
                36)

treatment_1s <- c(19.5,
                  15.5,
                  8.5,
                  33,
                  24,
                  16)


cohens_d1s <- (mean(control_1s) - mean(treatment_1s))/sqrt((sd(treatment_1s)**2 + sd(control_1s)**2)/2) * ((length(control_1s)-3)/(length(control_1s) - 2.25)) * (sqrt((length(control_1s) - 2)/length(control_1s)))

pwr.t.test(d= cohens_d1s, sig.level=0.05, power= 0.80, type="two.sample", alternative="two.sided")

t.test(control_1s, treatment_1s, alternative = "two.sided")
cohens_d1s
