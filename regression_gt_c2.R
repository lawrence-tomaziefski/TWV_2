source("161227_data_setup.R")

require(dplyr)
require(lmtest)
require(car)
require(MASS)
require(ggplot2)

phase_iii = combined_data_wide %>%
  filter(joint_phase == "III")


write.csv(phase_iii, file = "phase_iii.csv")
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
color_frame = filter(trac_colors, palette == 1)
freqData = as.data.frame(table(phase_iii$force_protection, phase_iii$off_road))
names(freqData) <- c("protection", "off_road", "freq")
freqData$protection <- as.numeric(as.character(freqData$protection))
freqData$off_road <- as.numeric(as.character(freqData$off_road))

g = ggplot(filter(freqData, freq > 0), aes(x = off_road, y = protection))
g = g  + scale_size(range = c(2, 25), guide = "none" )
g = g + geom_point(colour="black", aes(size = freq+.5, show_guide = FALSE))
g = g + geom_point(aes(colour=freq, size = freq))
g = g + scale_colour_gradient(low = color_frame$color_code[8] , high= color_frame$color_code[5])  
g = g + theme_bw()
g = g + stat_smooth(method = rlm)
g

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

#Nested Linear Model
fit0 = lm(force_protection ~1, data = phase_iii)
fit1 = update(fit0, .~. + I(close-mean(close)))
fit2 = update(fit1, .~. + I(off_road-mean(off_road)))           
fit3 = update(fit2, .~. + role)
fit4 = update(fit3, .~. + grouped_unit_type)
fit5 = update(fit4, .~. + vignette)
            

fit_off = lm(force_protection ~ I(on_road^2), data = phase_iii)
anova(fit0, fit1,fit2,fit3,fit4,fit5)

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#Using AIC Method

base_model = lm(force_protection ~ 1, data = phase_iii)
complete_model = update(base_model, force_protection ~ I(support-mean(support)) + I(close-mean(close))+ I(on_road-mean(on_road)) + I(off_road-mean(off_road)) + grouped_unit_type + role + vignette)

step_model = step(base_model,
                  scope = list(lower = base_model,
                               upper = complete_model),
                  direction = "both")
summary(step_model)
###lm(formula = force_protection ~ grouped_unit_type + I(off_road - mean(off_road)) + vignette + role, data = phase_iii)


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(step_model)

#check for heteroskadisity
bptest(step_model); ncvTest(step_model)

#check for multicollinearity
vif(step_model)

#check for interaction

anova(lm(formula = force_protection ~ grouped_unit_type + vignette + off_road + 
           grouped_unit_type:vignette + grouped_unit_type:off_road + vignette:off_road, data = phase_iii))

#no interaction exist in the model

ad.test(phase_iii$force_protection,pnorm)$p.value

#identify outliers with cook's distance

cutoff = 4/((nrow(phase_iii)-length(step_model$coefficients)-2))

outlierTest(step_model)

par(opar)

opar

rr.huber_step_model = rlm(force_protection ~ grouped_unit_type + I(off_road - mean(off_road)) + vignette + role, data = phase_iii)
summary(rr.huber_step_model)

rr.bisquare_step_model = rlm(force_protection ~ relevel(factor(grouped_unit_type), "Maneuver") + I(off_road - mean(off_road)) + vignette + relevel(factor(role), "Gun Truck") -1, data = phase_iii, psi = psi.bisquare)
summary(rr.bisquare_step_model)

par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(rr.bisquare_step_model)

fitted_values = data.frame(fitted_values = rr.bisquare_step_model$fitted.values, residuals = rr.bisquare_step_model$resid)

rr.bisquare_step_model$residuals
cutoff = 4/((nrow(phase_iii)-length(rr.bisquare_step_model$coefficients)-2))

predict(rr.bisquare_step_model, newdata =data.frame( 
                                          grouped_unit_type = c("Maneuver"),
                                          off_road = c(100),
                                          vignette = c("Vignette 1"),
                                          role = c("Gun Truck")),interval = "confidence")

outliers = boxplot(phase_iii$force_protection ~ phase_iii$off_road)
outliers


base_model_rlm = rlm(force_protection ~ 1, data = phase_iii)
complete_model_rlm = update(base_model_rlm, force_protection ~ I(support-mean(support)) + I(close-mean(close))+ I(on_road-mean(on_road)) + I(off_road-mean(off_road)) + grouped_unit_type + role + vignette)

step_model_rlm = step(base_model_rlm,
                  scope = list(lower = base_model_rlm,
                               upper = complete_model_rlm),
                  direction = "both")
summary(step_model_rlm)