# calculate predicted flow metrics for cleaned watersheds


# Libraries ---------------------------------------------------------------

library(tidyverse)
# install from a specific branch
#library(remotes)
remotes::install_github("ceff-tech/ffc_api_client/ffcAPIClient#74")
# devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)
library(mapview)
mapviewOptions(fgb = FALSE)


# Load Data ---------------------------------------------------------------
#
# load("data_output/little_shasta_catchment_flowlines.rda")
#
# # mapview
# (m1 <- mapview(evans, color="steelblue") +
#     mapview(lsh_springs, col.regions="skyblue") +
#     mapview(flowlines, zcol="comid"))
#     #mapview(flowlines, zcol="streamorde"))
#     #mapview(h10, color="darkblue", lwd=2, alpha.regions=0))
#
# # Select the Lower COMIDs -------------------------------------------------
#
# library(mapedit)
#
# # select COMIDs
# comids <- selectFeatures(flowlines, map = m1@map, mode = "draw")
#
# # filter
# df_coms <- comids %>% select(-edit_id) %>%
#   mutate(comid_f=as.factor(comid))
#
# # mapview
# mapview(df_coms, zcol="comid_f", lwd=4) + mapview(flowlines, color="cyan4", lwd=0.5)
#
# save(df_coms, file="data_output/05_selected_comids_AOI.rda")

# Now pull predictions ----------------------------------------------------

load("data_output/05_selected_comids_AOI.rda")

library(ffcAPIClient)
library(purrr)

ffctoken <- set_token(Sys.getenv("EFLOWS_WEBSITE_TOKEN"))
ffcAPIClient::clean_account(ffctoken)
tst <- get_predicted_flow_metrics("8211251", online = F)
# make a list of comids
coms <- df_coms$comid
com_predictions <- map(coms, ~get_predicted_flow_metrics(.x, online = TRUE, wyt = "any"))

# combine
lshasta_ffc_preds <- com_predictions %>% bind_rows() %>% arrange(metric)

# refactor the comids
lshasta_ffc_preds <- lshasta_ffc_preds %>%
  mutate(comid_f = factor(comid,
                           levels = c("3917198","3917200",
                                      "3917948","3917950",
                                      "3917244","3917946"))) %>%
  arrange(metric, comid_f)
#summary(lshasta_ffc_preds$comid_f)

# mapview
mapview(flowlines[,c(1:20)], color="cyan4", lwd=0.5, legend=FALSE) +
  mapview(df_coms[,c(1:20,141:142)], zcol="comid_f", lwd=4, layer.name="Selected COMIDs") +
  mapview(evans, color="steelblue", legend=FALSE) +
  mapview(lsh_springs, col.regions="skyblue") +
  mapview(h10, color="darkblue", lwd=2, alpha.regions=0, legend=FALSE)


# Some Plots --------------------------------------------------------------

library(ggthemes)
library(ggdark)

# add component
lshasta_ffc_preds <- lshasta_ffc_preds %>%
  mutate(flow_component = case_when(
    grepl("DS_", metric) ~ "Dry-season baseflow",
    grepl("SP_", metric) ~ "Spring recession flow",
    grepl("Peak_", metric) ~ "Peak flow",
    grepl("Wet_", metric) ~ "Wet-season baseflow",
    grepl("FA_", metric) ~ "Fall pulse flow"
  ),
  flow_component = factor(flow_component, levels = c("Fall pulse flow", "Wet-season baseflow", "Peak flow", "Spring recession flow", "Dry-season baseflow", "General")),
  metric = as.factor(metric),
  metric = fct_reorder2(metric, flow_component, metric))

# peak metrics
ggplot() + geom_point(data=lshasta_ffc_preds %>% filter(flow_component=="Peak flow"), aes(x=metric, y=p50, color=comid_f), size=4) + coord_flip() +
 scale_color_viridis_d() +
  scale_y_log10() +
  labs(y="Log scale (p50)", subtitle="Peak Flows") +
  ggdark::dark_theme_light()

# spring metrics
ggplot() + geom_point(data=lshasta_ffc_preds %>% filter(flow_component=="Spring recession flow", metric!="SP_ROC"), aes(x=metric, y=p50, fill=comid_f),
                      size=4, pch=21, color="gray20", alpha=0.8) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(subtitle="Spring Recession Flows") +
  ggdark::dark_theme_light()

# recession rate is identical for all
ggplot() + geom_point(data=lshasta_ffc_preds %>% filter(flow_component=="Fall pulse flow"), aes(x=metric, y=p50, fill=comid_f),
                      size=4, pch=21, color="gray20", alpha=0.8) +
  coord_flip() +
  scale_y_log10() +
  labs(y="Log scale (p50)", subtitle="Fall Pulse Flows") +
  scale_fill_viridis_d() +
  ggdark::dark_theme_light()


# Save Out ----------------------------------------------------------------

# write CSV
write_csv(lshasta_ffc_preds, file = "data_output/lshasta_ffc_predictions.csv")

