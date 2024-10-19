library("flextable")

# Set up some formatting for flextable to be applied to most tables
flextable_format <- function(data) {
  data %>%
    flextable() |>
    bold(part = "header") %>%
    bg(bg = "#0000FF", part = "header") %>%
    color(color = "white", part = "header") %>%
    align(align = "left", part = "header") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top", part = "body") %>%
    colformat_num(big.mark = ",") %>%
    fontsize(size = 12, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    border(border = fp_border_default(color = "#000000", width = 0.5), part = "all") |>
    autofit()
}

Faction_Roster_Count_Table <- Faction_Roster_Count %>% 
  flextable_format()

Faction_Roster_List_Tier0_Table <- Faction_Roster_List_Tier0 %>% 
  flextable_format()

Faction_Roster_List_Tier1_Table <- Faction_Roster_List_Tier1 %>% 
  flextable_format()

rmarkdown::render(
  "activity report.Rmd")
