


library(psych)
library(dplyr)

pth="/home/elias/erg/STATISTICS/Roula/"

file="wender_items.csv"

# Reading csv file
data_0     = read.csv( paste0(pth, file), header = TRUE, sep = ",", quote = "" )

dt_wa<- (data_0[7:65])
dt_wb<- (data_0[66:124])
dt_wc<- (data_0[125:183])
 


dt_wa_adhd <- select(dt_wa, WA_3,WA_4,WA_5,WA_6,WA_7,WA_9,WA_10,WA_11,WA_12,WA_15,WA_16,WA_17,WA_20,WA_21,WA_23,WA_24,WA_25,WA_26,WA_27,WA_28,WA_39,WA_40,WA_50,WA_55,WA_59)
dt_wb_adhd <- select(dt_wb, WB_3,WB_4,WB_5,WB_6,WB_7,WB_9,WB_10,WB_11,WB_12,WB_15,WB_16,WB_17,WB_20,WB_21,WB_23,WB_24,WB_25,WB_26,WB_27,WB_28,WB_39,WB_40,WB_50,WB_55,WB_59)
dt_wc_adhd <- select(dt_wc, WC_3,WC_4,WC_5,WC_6,WC_7,WC_9,WC_10,WC_11,WC_12,WC_15,WC_16,WC_17,WC_20,WC_21,WC_23,WC_24,WC_25,WC_26,WC_27,WC_28,WC_39,WC_40,WC_50,WC_55,WC_59)

dt_wa_radhd <- select(dt_wa, WA_1,WA_2,WA_8,WA_13,WA_14,WA_18,WA_19,WA_22,WA_29,WA_30,WA_31,WA_33,WA_34,WA_35,WA_36,WA_37,WA_38,WA_41)
dt_wb_radhd <- select(dt_wb, WB_1,WB_2,WB_8,WB_13,WB_14,WB_18,WB_19,WB_22,WB_29,WB_30,WB_31,WB_33,WB_34,WB_35,WB_36,WB_37,WB_38,WB_41)
dt_wc_radhd <- select(dt_wc, WC_1,WC_2,WC_8,WC_13,WC_14,WC_18,WC_19,WC_22,WC_29,WC_30,WC_31,WC_33,WC_34,WC_35,WC_36,WC_37,WC_38,WC_41)

dt_wa_school <- select(dt_wa, WA_42,WA_43,WA_44,WA_45,WA_46,WA_47,WA_48)
dt_wb_school <- select(dt_wb, WB_42,WB_43,WB_44,WB_45,WB_46,WB_47,WB_48)
dt_wc_school <- select(dt_wc, WC_42,WC_43,WC_44,WC_45,WC_46,WC_47,WC_48)

dt_wa_health <- select(dt_wa, WA_49,WA_50,WA_51,WA_52,WA_53,WA_54,WA_55,WA_56,WA_57,WA_58,WA_59)
dt_wb_health <- select(dt_wb, WB_49,WB_50,WB_51,WB_52,WB_53,WB_54,WB_55,WB_56,WB_57,WB_58,WB_59)
dt_wc_health <- select(dt_wc, WC_49,WC_50,WC_51,WC_52,WC_53,WC_54,WC_55,WC_56,WC_57,WC_58,WC_59)

dt_wa_all <- select(dt_wa, WA_1,  WA_2,  WA_3,  WA_4,  WA_5,  WA_6,  WA_7,  WA_8,  WA_9,  WA_10,  WA_11,  WA_12,  WA_13,  WA_14,  WA_15,  WA_16,  WA_17,  WA_18,  WA_19,  WA_20,  WA_21,  WA_22, WA_23,  WA_24,  WA_25,  WA_26,  WA_27,  WA_28,  WA_29,  WA_30,  WA_31,  WA_33,  WA_34,  WA_35,  WA_36,  WA_37,  WA_38, 
WA_39,  WA_40,  WA_41,  WA_42,  WA_43,  WA_44,  WA_45,  WA_46,  WA_47,  WA_48,  WA_49,  WA_50,  WA_51,  WA_52, WA_53,  WA_54,  WA_55,  WA_56,  WA_57,  WA_58,  WA_59)

dt_wb_all <- select(dt_wb, WB_1,  WB_2,  WB_3,  WB_4,  WB_5,  WB_6,  WB_7,  WB_8,  WB_9,  WB_10,  WB_11,  WB_12,  WB_13,  WB_14,  WB_15,  WB_16,  WB_17,  WB_18,  WB_19,  WB_20,  WB_21,  WB_22, WB_23,  WB_24,  WB_25,  WB_26,  WB_27,  WB_28,  WB_29,  WB_30,  WB_31,  WB_33,  WB_34,  WB_35,  WB_36,  WB_37,  WB_38, 
                    WB_39,  WB_40,  WB_41,  WB_42,  WB_43,  WB_44,  WB_45,  WB_46,  WB_47,  WB_48,  WB_49,  WB_50,  WB_51,  WB_52, WB_53,  WB_54,  WB_55,  WB_56,  WB_57,  WB_58,  WB_59)

dt_wc_all <- select(dt_wc, WC_1,  WC_2,  WC_3,  WC_4,  WC_5,  WC_6,  WC_7,  WC_8,  WC_9,  WC_10,  WC_11,  WC_12,  WC_13,  WC_14,  WC_15,  WC_16,  WC_17,  WC_18,  WC_19,  WC_20,  WC_21,  WC_22, WC_23,  WC_24,  WC_25,  WC_26,  WC_27,  WC_28,  WC_29,  WC_30,  WC_31,  WC_33,  WC_34,  WC_35,  WC_36,  WC_37,  WC_38, 
                    WC_39,  WC_40,  WC_41,  WC_42,  WC_43,  WC_44,  WC_45,  WC_46,  WC_47,  WC_48,  WC_49,  WC_50,  WC_51,  WC_52, WC_53,  WC_54,  WC_55,  WC_56,  WC_57,  WC_58,  WC_59)


do.call(rbind, list(
alpha(dt_wa_adhd)[[1]],
alpha(dt_wb_adhd)[[1]],
alpha(dt_wc_adhd)[[1]],
alpha(dt_wa_radhd)[[1]],
alpha(dt_wb_radhd)[[1]],
alpha(dt_wc_radhd)[[1]],

alpha(dt_wa_health)[[1]],
alpha(dt_wb_health)[[1]],
alpha(dt_wc_health)[[1]],

alpha(dt_wa_school)[[1]],
alpha(dt_wb_school)[[1]],
alpha(dt_wc_school)[[1]],

alpha(dt_wa_all)[[1]],
alpha(dt_wb_all)[[1]],
alpha(dt_wc_all)[[1]]
))

