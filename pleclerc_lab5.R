
library(tidyverse)

not.allentown <- read_csv("data/essentia.data.csv")
allentown <- read_csv("data/essentia.data.allentown.csv")
feature <- allentown$overall_loudness

feature <- "overall_loudness"
allen.feature <- allentown[[feature]]
not.allentown |>
  group_by(artist) |>
  summarize(min = min(get(feature)),
            LF = quantile(get(feature),probs = .25)-1.5*IQR(get(feature)),
            UF = quantile(get(feature),probs = .75)+1.5*IQR(get(feature)),
            max = max(get(feature))) |>
  mutate(out.of.range = allen.feature < min | allen.feature > max,
         unusual = allen.feature < LF | allen.feature > UF,
         description = case_when(
           out.of.range ~ "Out of Range",
           unusual ~ "Outlying",
           .default = "Within Range")
         )



