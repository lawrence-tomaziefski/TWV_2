require(dplyr)
require(lmtest)
require(car)
require(MASS)
require(ggplot2)
require(xlsx)
require(GGally)
require(Hmisc)



twv_raw = read.csv(file = "phase_iii.csv", header = TRUE, stringsAsFactors = FALSE)

normed = function(x){
        (x-mean(x))/sd(x)
   }

normed = as.data.frame(lapply(twv_raw[,12:33], normed))

twv = dplyr::select(twv_raw,1:11)
twv_normed = cbind(twv, normed)

trac_colors = read.xlsx(file = "color_frame.xlsx", sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)
color_frame = filter(trac_colors, palette == 1)
freqData = as.data.frame(table(twv$force_protection, twv$off_road))
names(freqData) <- c("protection", "off_road", "freq")
freqData$protection <- as.numeric(as.character(freqData$protection))
freqData$off_road <- as.numeric(as.character(freqData$off_road))

g = ggplot(filter(freqData, freq > 0), aes(x = off_road, y = protection))
g = g  + scale_size(range = c(2, 25), guide = "none" )
g = g + geom_point(colour="black", aes(size = freq+.5, show_guide = FALSE))
g = g + geom_point(aes(colour=freq, size = freq))
g = g + scale_colour_gradient(low = color_frame$color_code[5] , high= color_frame$color_code[10])  
g = g + theme_bw()
g = g + stat_smooth(method = loess)
g

cor(twv$force_protection,twv$off_road)

base_model_lm = lm(force_protection ~ 1, data = twv)
complete_model_lm = update(base_model_lm, .~.)


#complete_model_lm = update(base_model_lm, force_protection ~ I(support-mean(support)) + I(close-mean(close))+ I(on_road-mean(on_road)) + I(off_road-mean(off_road)) + grouped_unit_type + role + vignette)

step_model_lm = step(base_model_lm,
                      scope = list(lower = base_model_lm,
                                   upper = complete_model_lm),
                      direction = "both")
summary(step_model_lm)

car_plots = ggpairs(twv[,27:33],
                    lower = list(continuous = "smooth"),
                    diag = list(continuous = "blank"),
                    title = "Vehicle Attribute Correlations")



qq = qplot(on_road, force_protection, color = grouped_unit_type, data = twv_normed)
qq + geom_smooth(method = "lm", formula = y~x, se = FALSE)


##########
cut_off_road = cut2(twv_raw$off_road, g = 5)
table(cut_off_road)

mean(twv_raw$on_road)

mean(c(51,80,80,74))


on_road_avgs = twv_raw %>%
        group_by(vm_unit_type, role) %>%
        dplyr::summarize(on_road_avg = mean(on_road))


qplot(on_road, force_protection, data=twv_raw,
            geom=c("boxplot","jitter"))

twv_ibct = twv_raw %>%
        filter(echelon == "ABCT")

qq = qplot(on_road, force_protection, color = grouped_unit_type, data = twv_ibct)
qq + geom_smooth(method = "lm", formula = y~x, se = FALSE)


base_model_ibct = lm(force_protection ~ 1, data = twv_ibct)
complete_model_ibct = update(base_model_ibct, force_protection ~ I(support-mean(support)) + I(close-mean(close))+ I(on_road-mean(on_road)) + I(off_road-mean(off_road)) + grouped_unit_type + role + vignette)

step_model_ibct = step(base_model_ibct,
                     scope = list(lower = base_model_ibct,
                                  upper = complete_model_ibct),
                     direction = "both")
summary(step_model_ibct)



cut_on_road = cut2(twv_raw$on_road, g = 5)
table(cut_on_road)

outliers = boxplot(twv_raw$force_protection ~ twv_raw$on_road)
outliers

outlier_data = data.frame(force_protection = outliers$out, on_road = outliers$group)

outlier_data = mutate(outlier_data, on_road = outliers$names[on_road])


twv_outlier = filter(twv_raw, force_protection %in% outlier_data$force_protection 
                     & on_road %in% outlier_data$on_road)


twv_no_outlier = twv_raw %>%
        filter(!index %in% twv_outlier$index)

base_model_ibct = lm(force_protection ~ 1, data = twv_no_outlier)
complete_model_ibct = update(base_model_ibct, force_protection ~ I(support-mean(support)) + I(close-mean(close))+ I(on_road-mean(on_road)) + I(off_road-mean(off_road)) + grouped_unit_type + role + vignette)

step_model_ibct = step(base_model_ibct,
                       scope = list(lower = base_model_ibct,
                                    upper = complete_model_ibct),
                       direction = "both")
summary(step_model_ibct)

outliers2 = boxplot(twv_no_outlier$force_protection ~ twv_no_outlier$on_road)
outliers2