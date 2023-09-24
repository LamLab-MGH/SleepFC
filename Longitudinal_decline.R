#Import Excel file (File > Import Dataset) 
#must be called "dataset"

dataset <- read_excel("Dropbox (Partners HealthCare)/Research Project - Courtney/230327/WORKING FILE2023-01-18 - ADRC NP Demographic info - complete_KP (Courtney Berezuk's conflicted copy 2023-06-20).xlsx")

#Load Packages
library(lme4)
library(lmerTest)
library(ggplot2)

library(readxl)

#Create dataframe of AD participants with longitudinal MoCA scores and FC data
ADMoca <- dataset[complete.cases(dataset$MoCA_selectsubs) == 1,]
ADMoca_REM <- dataset[complete.cases(dataset$MoCA_selectsubs_REMdata) == 1,]
ADMoca_Awake <- dataset[complete.cases(dataset$MoCA_selectsubs_Awakedata) == 1,]



#Model 1: GE-N2 Gamma 

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2gamma + GE_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2gamma_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 2: GE-N2 Delta   

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2delta + GE_N2delta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2delta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2delta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Delta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 3: GE-N2 Alpha   

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2alpha + GE_N2alpha:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2alpha_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2alpha_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Alpha GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()


#Model 4: GE-REM Alpha  

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_REMalpha + GE_REMalpha:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca_REM, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_REMalpha_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_REMalpha_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM-Alpha GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()


#Model 5: GE-REM Beta

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_REMbeta + GE_REMbeta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca_REM, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_REMbeta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_REMbeta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM-Beta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 6: GE-REM Delta

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_REMdelta + GE_REMdelta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca_REM, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_REMdelta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_REMdelta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM-Delta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()




#Model 7: GE_wPLI_AwakeDelta

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_wPLI_AwakeDelta + GE_wPLI_AwakeDelta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca_Awake, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_wPLI_AwakeDelta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_wPLI_AwakeDelta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "GE_wPLI_AwakeDelta") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()


#Model 8: GE_wPLI_AwakeTheta

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_wPLI_AwakeTheta + GE_wPLI_AwakeTheta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca_Awake, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_wPLI_AwakeTheta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_wPLI_AwakeTheta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "GE_wPLI_AwakeTheta") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 9: GE_wPLI_N2alpha

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_wPLI_N2alpha + GE_wPLI_N2alpha:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_wPLI_N2alpha_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_wPLI_N2alpha_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "GE_wPLI_N2alpha") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 10: GE_wPLI_REMtheta

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_wPLI_REMtheta + GE_wPLI_REMtheta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca_REM, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_wPLI_REMtheta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_wPLI_REMtheta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "GE_wPLI_REMtheta") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()






























### EXCLUDING ADEp (n=2) ###
#Model 1: GE-N2 Gamma 

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2gamma + GE_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca_NoEp)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))


#with error bars
ggplot(ADMoca_NoEp, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2gamma_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 2: GE-N2 Delta   

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2delta + GE_N2delta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca_NoEp)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))


#with error bars
ggplot(ADMoca_NoEp, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2delta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2delta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Delta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 3: GE-N2 Alpha   

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2alpha + GE_N2alpha:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca_NoEp)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))


#with error bars
ggplot(ADMoca_NoEp, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2alpha_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2alpha_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Alpha GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()


#Model 4: GE-REM Alpha  

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_REMalpha + GE_REMalpha:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca_NoEp)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))


#with error bars
ggplot(ADMoca_NoEp, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_REMalpha_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_REMalpha_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM-Alpha GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()


#Model 5: GE-REM Beta   (MISSING)

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_REMbeta + GE_REMbeta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca_NoEp)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca_NoEp, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_REMbeta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_REMbeta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM-Beta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 6: GE-REM Delta

model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_REMdelta + GE_REMdelta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca_NoEp)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

#with error bars
ggplot(ADMoca_NoEp, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_REMdelta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0) +
  geom_point(alpha = 0)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_REMdelta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM-Delta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()





############################## OLD ANALYSES FOR AAIC POSTER ###############################################

### Run mixed-effects models ###

#Model 1: GE-N2Gamma   
model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2gamma + GE_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE, aes(group = GE_N2gamma_Tertile)) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#with error bars
ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2gamma_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 2: AEC Frontal Average - N2Gamma   
model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + AEC_Front_avg_N2gamma + AEC_Front_avg_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_avg_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE, aes(group = AEC_Front_avg_N2gamma_Tertile)) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Average") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#with error bars
ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_avg_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = TRUE, aes(group = AEC_Front_avg_N2gamma_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Average") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 3: AEC Frontal Temporal Average - N2Gamma
model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + AEC_Front_Temp_avg_N2gamma + AEC_Front_Temp_avg_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_Temp_avg_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE, aes(group = AEC_Front_Temp_avg_N2gamma_Tertile)) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Temporal Average") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#with error bars
ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_Temp_avg_N2gamma_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = TRUE, aes(group = AEC_Front_Temp_avg_N2gamma_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Temporal Average") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 4: GE-N2Delta   
model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2delta + GE_N2delta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2delta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE, aes(group = GE_N2delta_Tertile)) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Delta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#with error bars
ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2delta_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2delta_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Delta GE") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#Model 5: AEC_Post_Temp_avg_REMalpha   
model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + AEC_Post_Temp_avg_REMalpha + AEC_Post_Temp_avg_REMalpha:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
summary(model.MoCA)
qqnorm(residuals(model.MoCA))
qqline(residuals(model.MoCA))
hist(residuals(model.MoCA))

ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Post_Temp_avg_REMalpha_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE, aes(group = AEC_Post_Temp_avg_REMalpha_Tertile)) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM Alpha AEC Posterior Temporal Average") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()

#with error bars
ADMocaREM <- dataset[complete.cases(dataset$MoCA_selectsubs_REMdata) == 1,]
ggplot(ADMocaREM, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Post_Temp_avg_REMalpha_Tertile), group = Sbj_ID)) + 
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm", se = TRUE, aes(group = AEC_Post_Temp_avg_REMalpha_Tertile), alpha = 0.15) +
  labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM Alpha AEC Posterior Temporal Average") +
  ggtitle("MoCA") +
  scale_color_manual(values = c("black", "purple", "red"), 
                     labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
  theme_classic()















############################## OLD ANALYSES FOR AAIC POSTER ###############################################

### Run mixed-effects models ###
    
#Model 1: GE-N2Gamma   
   model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2gamma + GE_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
   summary(model.MoCA)
        qqnorm(residuals(model.MoCA))
        qqline(residuals(model.MoCA))
        hist(residuals(model.MoCA))
    
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2gamma_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = FALSE, aes(group = GE_N2gamma_Tertile)) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma GE") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
    #with error bars
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2gamma_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2gamma_Tertile), alpha = 0.15) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma GE") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
#Model 2: AEC Frontal Average - N2Gamma   
    model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + AEC_Front_avg_N2gamma + AEC_Front_avg_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
    summary(model.MoCA)
    qqnorm(residuals(model.MoCA))
    qqline(residuals(model.MoCA))
    hist(residuals(model.MoCA))
    
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_avg_N2gamma_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = FALSE, aes(group = AEC_Front_avg_N2gamma_Tertile)) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Average") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
    #with error bars
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_avg_N2gamma_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = TRUE, aes(group = AEC_Front_avg_N2gamma_Tertile), alpha = 0.15) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Average") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()

#Model 3: AEC Frontal Temporal Average - N2Gamma
    model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + AEC_Front_Temp_avg_N2gamma + AEC_Front_Temp_avg_N2gamma:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
    summary(model.MoCA)
    qqnorm(residuals(model.MoCA))
    qqline(residuals(model.MoCA))
    hist(residuals(model.MoCA))
    
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_Temp_avg_N2gamma_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = FALSE, aes(group = AEC_Front_Temp_avg_N2gamma_Tertile)) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Temporal Average") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
    #with error bars
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Front_Temp_avg_N2gamma_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = TRUE, aes(group = AEC_Front_Temp_avg_N2gamma_Tertile), alpha = 0.15) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Gamma AEC Frontal Temporal Average") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
#Model 4: GE-N2Delta   
    model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + GE_N2delta + GE_N2delta:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
    summary(model.MoCA)
    qqnorm(residuals(model.MoCA))
    qqline(residuals(model.MoCA))
    hist(residuals(model.MoCA))
    
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2delta_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = FALSE, aes(group = GE_N2delta_Tertile)) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Delta GE") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
    #with error bars
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(GE_N2delta_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = TRUE, aes(group = GE_N2delta_Tertile), alpha = 0.15) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "N2-Delta GE") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
#Model 5: AEC_Post_Temp_avg_REMalpha   
    model.MoCA <- lmer(MoCA ~ Age_at_EEG + Sex_01 + Education_years + Disease_Duration_EEG + Time_EEGtoNPVisit_y + AEC_Post_Temp_avg_REMalpha + AEC_Post_Temp_avg_REMalpha:Time_EEGtoNPVisit_y + (1|Sbj_ID), data = ADMoca)
    summary(model.MoCA)
    qqnorm(residuals(model.MoCA))
    qqline(residuals(model.MoCA))
    hist(residuals(model.MoCA))
    
    ggplot(ADMoca, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Post_Temp_avg_REMalpha_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = FALSE, aes(group = AEC_Post_Temp_avg_REMalpha_Tertile)) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM Alpha AEC Posterior Temporal Average") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
    #with error bars
    ADMocaREM <- dataset[complete.cases(dataset$MoCA_selectsubs_REMdata) == 1,]
    ggplot(ADMocaREM, aes(x = Time_EEGtoNPVisit_y, y = MoCA, color = factor(AEC_Post_Temp_avg_REMalpha_Tertile), group = Sbj_ID)) + 
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.5)+
      geom_smooth(method = "lm", se = TRUE, aes(group = AEC_Post_Temp_avg_REMalpha_Tertile), alpha = 0.15) +
      labs(x = "Time (Years from EEG Visit)", y = "MoCA (/30)", color = "REM Alpha AEC Posterior Temporal Average") +
      ggtitle("MoCA") +
      scale_color_manual(values = c("black", "purple", "red"), 
                         labels = c("1st Tertile", "2nd Tertile", "3rd Tertile")) +
      theme_classic()
    
    
    
    
       
