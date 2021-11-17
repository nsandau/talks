library(tidymodels)
library(tidyverse)
library(here)

source("theme.r")

### LOAD DATA

cols <- c("abn", "acl", "men")

n_cases <- read_csv("data/train-abnormal.csv", col_names = F) %>% nrow()


truth <- list.files("data", pattern = "valid-.") %>%
    map2_dfc(
        .x = .,
        .y = cols,
        .f = ~ read.csv(here("2021", "dos_mrknee", "data", .x), header = F, col.names = c("id", .y))
    ) %>%
    select(all_of(cols)) %>%
    mutate(across(.fns = as.factor))



probas <- read.csv("data/all_valids_preds.csv",
    header = F,
    col.names = cols
)

preds <- probas %>%
    mutate(
        across(.fns = ~ if_else(.x > 0.53, 1, 0)),
        across(.fns = as.factor)
    )

radiologists_perf <-
    tribble(
        ~path, ~spec, ~sens,
        "abn", 0.844, 0.905,
        "acl", 0.933, 0.906,
        "men", 0.882, 0.820
    ) %>% mutate(name = "Radiologists")

stanford_perf <-
    tribble(
        ~path, ~spec, ~sens,
        "abn", 0.714, 0.879,
        "acl", 0.968, 0.759,
        "men", 0.741, 0.710
    ) %>% mutate(name = "Bien et al.")


# dfs <- cols %>% map(
#  ~tibble(truth[.x], probas[.x], preds[.x], .name_repair = ~set_names(c("truth", "probas", "preds")))) %>%   set_names(cols)



########### SENS AND SPECIFICITY

# function to calculate sens and spec

calc_sns_spc <- function(col, type) {
    tp <- sum(truth[[col]] == 1 & preds[[col]] == 1)
    tn <- sum(truth[[col]] == 0 & preds[[col]] == 0)
    fp <- sum(truth[[col]] == 0 & preds[[col]] == 1)
    fn <- sum(truth[[col]] == 1 & preds[[col]] == 0)
    if (type == "sens") {
        out <- Hmisc::binconf(tp, tp + fn, method = "wilson")
    }
    if (type == "spec") {
        out <- Hmisc::binconf(tn, tn + fp, method = "wilson")
    }
    return(out)
}



sens <- cols %>%
    map(~ calc_sns_spc(.x, "sens") %>%
        as.data.frame() %>%
        tibble()) %>%
    bind_rows() %>%
    mutate(path = cols)

spec <- cols %>%
    map(~ calc_sns_spc(.x, "spec") %>%
        as.data.frame() %>%
        tibble()) %>%
    bind_rows() %>%
    mutate(path = cols)


#### AUC

aucs <- cols %>%
    map(~ pROC::roc(response = truth[[.x]], predictor = as.numeric(probas[[.x]]))) %>%
    map(~ c(pROC::ci.auc(.x, method = "delong"))) %>%
    set_names(cols)


#### ROC CURVE

roc_vals <- cols %>%
    map(~ roc_curve(
        data = tibble(
            estimate = probas[[.x]],
            truth = truth[[.x]]
        ),
        truth, estimate,
        event_level = "second"
    ))

model_perf <- tibble(path = sens$path, sens = sens$PointEst, spec = spec$PointEst) %>% mutate(name = "Model")


performances <- model_perf %>%
    bind_rows(stanford_perf) %>%
    bind_rows(radiologists_perf)


roc_plts <- roc_vals %>%
    map(~ autoplot(.x)) %>%
    map2(
        .x = ., .y = cols,
        ~ .x + geom_point(
            data = performances %>% filter(path == .y),
            aes(x = 1 - spec, y = sens, color = name, shape = name),
            size = 3
        ) + annotate("text", x = 0.5, y = 1.03, label = str_c(
            "AUC: ", round(aucs[[.y]][1], 2)
        ), size = 8)
    ) %>%
    map(~ .x + labs(
        x = "False Positive Rate (1- specificity)",
        y = "True Positive Rate (Sensitivity)"
    ) +
        theme(plot.margin = margin(b = 0.8, t = 0.8, r = 0.8, l = 0.8), text = element_text(color = "black", size = 18), legend.title = element_blank(), panel.grid.minor = element_blank()) +
        scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1)) +
        scale_x_continuous(breaks = seq(0, 1, by = 0.1), labels = seq(0, 1, by = 0.1))) %>%
    set_names(cols)


cols %>% map(~ roc_plts[[.x]] %>% ggsave(filename = str_c("roc_", .x, ".svg"), plot = ., path = "figures", device = "svg", width = 25, height = 20, units = "cm"))