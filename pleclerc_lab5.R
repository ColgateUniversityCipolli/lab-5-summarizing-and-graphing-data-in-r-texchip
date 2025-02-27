
library(tidyverse)

not.allentown <- read_csv("data/essentia.data.csv")
allentown <- read_csv("data/essentia.data.allentown.csv")

range.check <- function(feature) {
allen.feature <- allentown[[feature]]
not.allentown |>
  group_by(artist) |>
  summarize(min = min(get(feature), na.rm=T),
            LF = quantile(get(feature),probs = .25, na.rm=T)-1.5*IQR(get(feature),na.rm=T),
            UF = quantile(get(feature),probs = .75, na.rm=T)+1.5*IQR(get(feature),na.rm=T),
            max = max(get(feature),na.rm=T)) |>
  mutate(out.of.range = allen.feature < min | allen.feature > max,
         unusual = allen.feature < LF | allen.feature > UF,
         description = case_when(
           out.of.range ~ "Out of Range",
           unusual ~ "Outlying",
           .default = "Within Range")
         )
}

numeric.cols <- allentown |>
  select_if(is.numeric) |>
  names()

descriptors <- tibble(artist = unique(not.allentown$artist))
  for(i in 1:length(numeric.cols)){
    desc.name <- numeric.cols[i]
    output <- range.check(desc.name)
    descriptors <- descriptors |>
      left_join(output |> select(artist, description), by = "artist") |>
      rename(!!desc.name := description)
  }

summ.table <- descriptors |>
  pivot_longer(cols = -artist, names_to = "feature", values_to = "description") |>
  group_by(artist, description) |>
  summarize(count = n(), .groups = "drop") |>
  pivot_wider(names_from = description, values_from = count, values_fill = 0)
print(summ.table)

summ.table.long <- summ.table |>
  pivot_longer(cols = -artist, names_to = "range_status", values_to = "count")

library(xtable)
print(xtable(summ.table,                                  # Table to print
             caption = "Count of descriptor status for \"Allentown\" between artists.",   # Caption for the table
             label = "tab:reference"),             # Label to reference (e.g., \ref{tab:reference})
      table.placement = "H")                       # Places it [H]ere in the document

ggplot(summ.table.long, aes(x = artist, y = count, fill = range_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept=0) +
  labs(title = "Count of Descriptor Status for \"Allentown\" between Artists",
       x = "Artist",
       y = "Count",
       fill = "Descriptor Status") +
  theme_bw()
