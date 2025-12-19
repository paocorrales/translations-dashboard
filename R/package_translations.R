# Loading packages
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(plotly)

# Sourcing the main dataset script
source("R/global_translations.R")

# Updating final dataset for summary
final_df_updated <- final_df |> group_by(package) |>    
                    summarise(across(c(translated_count,untranslated_count, 
                                       fuzzy_count), sum)) |> 
                    mutate(language = "All", .after = package)

final_df_updated <- final_df_updated |>
  mutate(
    pc_trans_count = translated_count / rowSums(final_df_updated[,3:5]),
    pc_untrans_count = untranslated_count / rowSums(final_df_updated[,3:5]),
    pc_fuzzy_count = fuzzy_count / rowSums(final_df_updated[,3:5]) 
  )

                           
final_df <- bind_rows(final_df, final_df_updated)

package_df <- final_df |> 
            mutate(Translated = round(pc_trans_count*100, 2),
                     Untranslated = round(pc_untrans_count*100, 2)*-1,
                     Fuzzy = round(pc_fuzzy_count*100, 2)*-1)

package_df <- package_df |> pivot_longer(cols = 9:11, values_to = "percentage",
                                         names_to = "status")


# Filtering values for overall 
# * Need to add a filter here
package_df <- package_df |> filter(language == "All")


# Diverging bar plot to show the difference between translation status
ggplot(package_df, aes(x = percentage, y = package, fill = status)) +
  geom_bar(stat = "identity") +
  labs(y = "R packages", x = "Percentage") +
  scale_fill_discrete(name = "Translation status", 
                      breaks = c('Fuzzy', 'Untranslated', 
                                 'Translated')) +
  theme(legend.position = "top")



