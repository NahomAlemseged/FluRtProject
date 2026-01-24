library(dplyr)
library(tidyr)
library(ggplot2)


setwd("c:/Users/nahomw/Desktop/from_mac/nahomworku/Desktop/uthealth/gra_project/proj_2")
df_all <- read.csv("VIW_FNT.csv")
View(df_all)
#################################################
# extract south hemisphere countries
##################################################

df_ <- df_all[df_all$HEMISPHERE == "SH",]
# View(df)

###########################################################################

sum(is.na(df_))

###########################################################################
rm_col <- function(data, col) {
  red_data <- data[, !names(data) %in% col]  # use col directly
  return(red_data)
}

# Example usage
df <- rm_col(df_, col = "B_P")  # Pass the column name as a string


###################################################################

df_ <- df_[, !names(df) %in% c("HEMISPHERE", "LAB_RESULT_COMMENT")]
###########################################################################
df <- df_[, names(df_) %in% c("FLUSEASON", "ITZ", "COUNTRY_CODE", "COUNTRY_AREA_TERRITORY", "ISO_YEAR", "ISO_WEEK", "SPEC_PROCESSED_NB", "AH1N12009",             
          "AH1", "AH3", "AH5", "AH7N9", "INF_A", "BVIC_2DEL", "BVIC_3DEL", "BVIC_NODEL", "BVIC_DELUNK", "BYAM", "BNOTDETERMINED", "INF_B", "INF_ALL", "INF_NEGATIVE", "ILI_ACTIVITY", 
          "ADENO", "BOCA")]
df$AH1N12009_p  <- df$AH1N12009/df$SPEC_PROCESSED_NB
df$AH1_p  <- df$AH1/df$SPEC_PROCESSED_NB
df$AH3_p  <- df$AH3/df$SPEC_PROCESSED_NB
df$AH5_p  <- df$AH5/df$SPEC_PROCESSED_NB
df$AH7N9_p  <- df$AH7N9/df$SPEC_PROCESSED_NB
df$A_p  <- df$INF_A/df$SPEC_PROCESSED_NB
df$B_p  <- df$INF_B/df$SPEC_PROCESSED_NB
df$BVIC_2DEL_p <-  df$BVIC_2DEL/df$SPEC_PROCESSED_NB
df$BVIC_3DEL_p <-  df$BVIC_2DEL/df$SPEC_PROCESSED_NB
# "BVIC_3DEL"
############################################################
# PLOT LINES FOR A AND B
############################################################

func_plot <- function(df, variant){
  df %>%
    filter(COUNTRY_CODE == "AUS") %>%
    select(ISO_YEAR, ISO_WEEK, all_of(variant_cols)) %>%
    pivot_longer(cols = all_of(variant_cols),
                 names_to = "Variant",
                 values_to = "Count") %>%
    filter(ISO_YEAR >= 2020 & ISO_YEAR <= 2024) %>%
    arrange(ISO_YEAR, ISO_WEEK) %>%
    ggplot(aes(x = ISO_WEEK + (ISO_YEAR * 100),
               y = Count)) +
    
    # POINTS
    geom_point(alpha = 0.7, size = 1.8, color = "steelblue") +
    
    # LOESS with CI
    geom_smooth(method = "loess",
                se = TRUE,
                span = 0.1,
                size = 1.2,
                color = "darkred") +
    
    # FACETS: one panel per variant
    facet_wrap(~ Variant, scales = "free_y") +
    
    labs(title = "Trends of Influenza A Variants in Australia (2005–2010)",
         x = "ISO Year + Week",
         y = "Count") +
    theme_minimal(base_size = 12)
}

# variant_cols <- c("AH1N12009_p", "AH1_p", "AH3_p", "AH5_p", "AH7N9_p", "A_p","BVIC_2DEL", "BVIC_3DEL", 'B_p')

variant_cols <- c("AH1N12009_p", "AH1_p", "AH3_p", "AH5_p", "AH7N9_p", "A_p","BVIC_2DEL_p", "BVIC_3DEL_p", 'B_p')



func_plot(df,variant)

###################################################################################################


 ### OVerall trend 

trend_plot <- function(df, variant_cols){
  
  df %>%
    filter(COUNTRY_CODE %in% c("AUS", "ARG", "BRA")) %>% 
    select(COUNTRY_CODE, ISO_YEAR, ISO_WEEK, all_of(variant_cols)) %>%
    filter(ISO_YEAR >= 2005 & ISO_YEAR <= 2024) %>%
    pivot_longer(cols = all_of(variant_cols),
                 names_to = "Variant",
                 values_to = "Count") %>%
    arrange(COUNTRY_CODE, ISO_YEAR, ISO_WEEK) %>%
    
    ggplot(aes(x = ISO_WEEK + ISO_YEAR * 100,
               y = Count)) +
    
    # geom_point(alpha = 0.3, size = 1.5, color = "steelblue") +
    geom_smooth(method = "loess",
                se = TRUE,
                span = 0.1,
                size = 1,
                color = "darkred") +
    
    facet_grid(rows = vars(COUNTRY_CODE),
               cols = vars(Variant),
               scales = "free_y") +
    
    labs(title = "Influenza Variant Trends by Country and Variant",
         x = "Year-Week",
         y = "Count") +
    
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(size = 10))
}




variant_cols <- c("AH1N12009_p", "AH1_p", "AH3_p", "AH5_p", "AH7N9_p","BVIC_2DEL", "BVIC_3DEL")
# variant_cols <- c("AH1N12009_p", "AH1_p", "AH3_p", "AH5_p", "AH7N9_p", "A_p","BVIC_2DEL", "BVIC_3DEL", 'B_p')


variant_cols <- c("AH1N12009_p", "AH1_p", "AH3_p", "AH5_p", "AH7N9_p")

variant_cols <- c("A_p", 'B_p')



trend_plot(df,variant_cols)

#################################################################################################################

variant_barplot <- function(df, variant_cols) {
  
  df %>%
    filter(COUNTRY_CODE %in% c("AUS", "ARG", "BRA")) %>% 
    select(COUNTRY_CODE, ISO_YEAR, ISO_WEEK, all_of(variant_cols)) %>%
    pivot_longer(cols = all_of(variant_cols),
                 names_to = "Variant",
                 values_to = "Count") %>%
    
    # Year-week as a numeric for continuous timeline
    mutate(YearWeek = ISO_YEAR + ISO_WEEK/53) %>%
    
    ggplot(aes(x = YearWeek,
               y = Count,
               fill = Variant)) +
    
    geom_col(position = "stack") +
    
    facet_wrap(~ COUNTRY_CODE, scales = "free_y") +
    
    labs(title = "Stacked Bar Chart of Influenza Variants",
         x = "Year",
         y = "Weekly Count",
         fill = "Variant") +
    
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(size = 11),
          legend.position = "right")
}

variant_barplot(df, variant_cols)










