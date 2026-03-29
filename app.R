
# app.R
# Pokémon evolution dashboard / presentation-style Shiny app

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(readr)
library(janitor)
library(ggplot2)
library(cluster)
library(ggrepel)
library(scales)

# =========================
# 1. File paths
# =========================
pokemon_path <- "/Users/kalle/Desktop/Pokemon/pokemon.xlsx"
evolution_path <- "/Users/kalle/Desktop/Pokemon/evolution_long.csv"

# =========================
# 2. Helper functions
# =========================
clean_pokemon_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace_all("'", "") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("-", "") %>%
    str_replace_all("\\s+", "")
}

build_data <- function(pokemon_path, evolution_path) {
  pokemon_raw <- read_excel(pokemon_path) %>% clean_names()
  evolution_raw <- read_csv(evolution_path, show_col_types = FALSE) %>% clean_names()

  pokemon_wide <- pokemon_raw %>%
    group_by(number, name, total, hp, attack, defense, special_attack, special_defense, speed) %>%
    mutate(type_slot = row_number()) %>%
    pivot_wider(
      names_from = type_slot,
      values_from = type,
      names_prefix = "type_"
    ) %>%
    ungroup() %>%
    distinct() %>%
    mutate(name_clean = clean_pokemon_name(name))

  evolution_clean <- evolution_raw %>%
    transmute(
      evolving_from = clean_pokemon_name(evolving_from),
      evolving_to   = clean_pokemon_name(evolving_to)
    )

  pokemon_wide <- pokemon_wide %>%
    left_join(
      evolution_clean %>%
        rename(name_clean = evolving_from, evolves_to = evolving_to),
      by = "name_clean"
    ) %>%
    left_join(
      evolution_clean %>%
        rename(name_clean = evolving_to, evolves_from = evolving_from),
      by = "name_clean"
    ) %>%
    filter(!str_detect(name, regex("mega", ignore_case = TRUE))) %>%
    mutate(
      evolution_stage = case_when(
        is.na(evolves_from) & !is.na(evolves_to) ~ 1L,
        !is.na(evolves_from) & !is.na(evolves_to) ~ 2L,
        !is.na(evolves_from) & is.na(evolves_to) ~ 3L,
        is.na(evolves_from) & is.na(evolves_to) ~ 0L
      ),
      offense = attack + special_attack,
      defense_total = defense + special_defense
    )

  evolution_gain <- pokemon_wide %>%
    select(name, name_clean, evolves_to, type_1, evolution_stage, total, offense, defense_total, hp, speed) %>%
    rename(
      from_stage = evolution_stage,
      total_base = total,
      offense_base = offense,
      defense_base = defense_total,
      hp_base = hp,
      speed_base = speed
    ) %>%
    left_join(
      pokemon_wide %>%
        select(name_clean, evolution_stage, total, offense, defense_total, hp, speed) %>%
        rename(
          evolves_to = name_clean,
          to_stage = evolution_stage,
          total_next = total,
          offense_next = offense,
          defense_next = defense_total,
          hp_next = hp,
          speed_next = speed
        ),
      by = "evolves_to"
    ) %>%
    mutate(
      stage_transition = paste0(from_stage, " \u2192 ", to_stage),
      total_gain = total_next - total_base,
      offense_gain = offense_next - offense_base,
      defense_gain = defense_next - defense_base,
      hp_gain = hp_next - hp_base,
      speed_gain = speed_next - speed_base
    ) %>%
    filter(!is.na(total_gain))

  list(
    pokemon_wide = pokemon_wide,
    evolution_gain = evolution_gain
  )
}

compute_profiles <- function(evolution_gain, exclude_types = c("FLYING"), max_k = 8) {
  gain_profile <- evolution_gain %>%
    filter(!(type_1 %in% exclude_types)) %>%
    group_by(type_1, stage_transition) %>%
    summarise(mean_gain = mean(total_gain, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = stage_transition, values_from = mean_gain) %>%
    drop_na()

  clustering_data <- gain_profile %>% select(`1 → 2`, `2 → 3`)
  clustering_scaled <- scale(clustering_data)

  ks <- 2:min(max_k, nrow(gain_profile) - 1)
  silhouette_scores <- sapply(ks, function(k) {
    km <- kmeans(clustering_scaled, centers = k, nstart = 50)
    ss <- silhouette(km$cluster, dist(clustering_scaled))
    mean(ss[, 3])
  })

  silhouette_df <- data.frame(k = ks, avg_silhouette = silhouette_scores)
  best_k <- silhouette_df$k[which.max(silhouette_df$avg_silhouette)]

  set.seed(123)
  km <- kmeans(clustering_scaled, centers = best_k, nstart = 50)
  gain_profile$cluster <- factor(km$cluster)

  cluster_summary <- gain_profile %>%
    group_by(cluster) %>%
    summarise(
      mean_early = mean(`1 → 2`),
      mean_late = mean(`2 → 3`),
      diff = mean_early - mean_late,
      .groups = "drop"
    )

  # Adaptive labels
  if (best_k == 2) {
    early_cluster <- cluster_summary %>% slice_max(diff, n = 1) %>% pull(cluster)
    late_cluster <- cluster_summary %>% slice_min(diff, n = 1) %>% pull(cluster)
    cluster_labels <- tibble(
      cluster = factor(c(early_cluster, late_cluster)),
      growth_profile = c("Early bloomer", "Late bloomer")
    )
  } else if (best_k == 3) {
    early_cluster <- cluster_summary %>% slice_max(diff, n = 1) %>% pull(cluster)
    late_cluster <- cluster_summary %>% slice_min(diff, n = 1) %>% pull(cluster)
    steady_cluster <- cluster_summary %>%
      filter(!(cluster %in% c(early_cluster, late_cluster))) %>%
      pull(cluster)

    cluster_labels <- tibble(
      cluster = factor(c(early_cluster, late_cluster, steady_cluster)),
      growth_profile = c("Early bloomer", "Late bloomer", "Steady grower")
    )
  } else {
    # Data-driven naming with the most interpretable 4-cluster fallback
    cluster_summary2 <- cluster_summary %>% arrange(desc(diff))
    cluster_labels <- cluster_summary2 %>%
      mutate(
        growth_profile = case_when(
          row_number() == 1 ~ "Early bloomer",
          row_number() == n() ~ "Late bloomer",
          abs(diff) == min(abs(diff)) ~ "Steady grower",
          TRUE ~ "Late surge"
        )
      ) %>%
      select(cluster, growth_profile) %>%
      distinct(cluster, .keep_all = TRUE)
  }

  gain_profile <- gain_profile %>%
    left_join(cluster_labels, by = "cluster")

  list(
    gain_profile = gain_profile,
    silhouette_df = silhouette_df,
    best_k = best_k,
    cluster_summary = cluster_summary
  )
}

# =========================
# 3. UI
# =========================
ui <- page_navbar(
  title = "Pokémon Evolution Strategy",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  nav_panel(
    "Executive Summary",
    layout_column_wrap(
      width = 1/4,
      value_box(
        title = "Pokémon in dataset",
        value = textOutput("n_pokemon", inline = TRUE),
        theme = "primary"
      ),
      value_box(
        title = "Evolution events",
        value = textOutput("n_transitions", inline = TRUE),
        theme = "success"
      ),
      value_box(
        title = "Best k",
        value = textOutput("best_k", inline = TRUE),
        theme = "warning"
      ),
      value_box(
        title = "Top early bloomer",
        value = textOutput("top_early", inline = TRUE),
        theme = "danger"
      )
    ),
    layout_columns(
      card(
        full_screen = TRUE,
        card_header("Key message"),
        p("Pokémon types do not gain strength uniformly across evolution. Some types peak early, others gain most of their power in the final jump, and some show a steadier progression."),
        p("This app is designed like a conference or business presentation: it gives an executive overview first, then moves into supporting evidence and a clear typology.")
      ),
      card(
        full_screen = TRUE,
        card_header("Average total-stat gain by type and transition"),
        plotOutput("bar_gain_plot", height = 420)
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Type growth profiles"),
      plotOutput("cluster_plot", height = 520)
    )
  ),

  nav_panel(
    "Growth Profiles",
    sidebar_layout(
      sidebar = sidebar(
        checkboxGroupInput(
          "exclude_types",
          "Exclude types from clustering",
          choices = NULL,
          selected = "FLYING"
        ),
        sliderInput("max_k", "Maximum k to evaluate", min = 3, max = 8, value = 8, step = 1),
        selectInput(
          "label_style",
          "Label display",
          choices = c("All types", "Only extreme types"),
          selected = "All types"
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Silhouette method"),
        plotOutput("silhouette_plot", height = 320),
        p("The silhouette score evaluates how well each type fits its assigned cluster compared with alternative clusters. Higher values indicate more coherent and better-separated clusters.")
      ),
      card(
        full_screen = TRUE,
        card_header("Clustered growth-profile map"),
        plotOutput("cluster_plot2", height = 520)
      )
    )
  ),

  nav_panel(
    "Type Explorer",
    sidebar_layout(
      sidebar = sidebar(
        selectInput("selected_type", "Highlight one type", choices = NULL),
        selectInput(
          "metric",
          "Metric",
          choices = c(
            "Total gain" = "total_gain",
            "Offense gain" = "offense_gain",
            "Defense gain" = "defense_gain",
            "HP gain" = "hp_gain",
            "Speed gain" = "speed_gain"
          ),
          selected = "total_gain"
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Distribution by type"),
        plotOutput("distribution_plot", height = 520)
      ),
      card(
        full_screen = TRUE,
        card_header("Type profile summary"),
        tableOutput("type_summary_table")
      )
    )
  ),

  nav_panel(
    "Method & Interpretation",
    card(
      full_screen = TRUE,
      card_header("How to read this app"),
      tags$ul(
        tags$li(tags$b("Stage 1 → 2 vs 2 → 3:"), " Each point compares the average gain a type receives from the first evolution step and the second evolution step."),
        tags$li(tags$b("Diagonal line:"), " Types below the line gain more early; types above the line gain more late."),
        tags$li(tags$b("Silhouette method:"), " We compare several clustering solutions and choose the one that best balances within-cluster similarity and between-cluster separation."),
        tags$li(tags$b("Growth profiles:"), " Early bloomers gain much of their power in the first jump, late bloomers gain most in the final jump, and steady growers are more balanced.")
      ),
      p("In a conference or business setting, the main takeaway is not the exact cluster number. The real value is the strategic interpretation: different types reward different investment patterns over time.")
    )
  )
)

# =========================
# 4. Server
# =========================
server <- function(input, output, session) {

  base_data <- reactive({
    build_data(pokemon_path, evolution_path)
  })

  observe({
    d <- base_data()
    all_types <- sort(unique(d$evolution_gain$type_1))
    updateCheckboxGroupInput(session, "exclude_types", choices = all_types, selected = "FLYING")
    updateSelectInput(session, "selected_type", choices = all_types, selected = "POISON")
  })

  prof <- reactive({
    compute_profiles(
      base_data()$evolution_gain,
      exclude_types = input$exclude_types,
      max_k = input$max_k
    )
  })

  output$n_pokemon <- renderText({
    comma(nrow(base_data()$pokemon_wide))
  })

  output$n_transitions <- renderText({
    comma(nrow(base_data()$evolution_gain))
  })

  output$best_k <- renderText({
    prof()$best_k
  })

  output$top_early <- renderText({
    gp <- prof()$gain_profile %>% arrange(desc(`1 → 2` - `2 → 3`))
    gp$type_1[1]
  })

  output$silhouette_plot <- renderPlot({
    ggplot(prof()$silhouette_df, aes(k, avg_silhouette)) +
      geom_line(linewidth = 1.1, color = "#2C3E50") +
      geom_point(size = 3.2, color = "#E67E22") +
      scale_x_continuous(breaks = prof()$silhouette_df$k) +
      labs(
        title = "Choosing the number of clusters",
        x = "Number of clusters (k)",
        y = "Average silhouette width"
      ) +
      theme_minimal(base_size = 14)
  })

  cluster_plot_fun <- function(all_labels = TRUE) {
    gp <- prof()$gain_profile
    label_data <- gp
    if (!all_labels) {
      label_data <- gp %>%
        mutate(extreme = abs(`1 → 2` - `2 → 3`)) %>%
        arrange(desc(extreme)) %>%
        slice_head(n = 8)
    }

    ggplot(gp, aes(x = `1 → 2`, y = `2 → 3`, color = growth_profile)) +
      geom_abline(
        slope = 1, intercept = 0,
        linetype = "dashed", linewidth = 0.8, color = "grey50"
      ) +
      annotate("text", x = max(gp$`1 → 2`) * 0.93, y = min(gp$`2 → 3`) + 5,
               label = "Earlier growth", color = "grey45", size = 4.2) +
      annotate("text", x = min(gp$`1 → 2`) + 8, y = max(gp$`2 → 3`) - 4,
               label = "Later growth", color = "grey45", size = 4.2) +
      geom_point(size = 4.5, alpha = 0.92) +
      geom_text_repel(
        data = label_data,
        aes(label = type_1),
        size = 4.5,
        box.padding = 0.35,
        point.padding = 0.25,
        segment.color = "grey70",
        segment.alpha = 0.8,
        show.legend = FALSE
      ) +
      scale_color_brewer(palette = "Set2") +
      labs(
        title = "When do Pokémon types gain strength?",
        subtitle = paste("Silhouette-chosen k =", prof()$best_k),
        x = "Average gain from stage 1 → 2",
        y = "Average gain from stage 2 → 3",
        color = "Growth profile"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 13, color = "grey25"),
        legend.position = "right",
        panel.grid.minor = element_blank()
      )
  }

  output$cluster_plot <- renderPlot({
    cluster_plot_fun(all_labels = TRUE)
  })

  output$cluster_plot2 <- renderPlot({
    cluster_plot_fun(all_labels = identical(input$label_style, "All types"))
  })

  output$bar_gain_plot <- renderPlot({
    d <- base_data()$evolution_gain %>%
      filter(!(type_1 %in% input$exclude_types)) %>%
      group_by(type_1, stage_transition) %>%
      summarise(mean_total_gain = mean(total_gain, na.rm = TRUE), .groups = "drop")

    ggplot(
      d,
      aes(x = reorder(type_1, mean_total_gain, FUN = max), y = mean_total_gain, fill = stage_transition)
    ) +
      geom_col(position = position_dodge(width = 0.72), width = 0.66) +
      coord_flip() +
      scale_fill_manual(values = c("1 → 2" = "#4C78A8", "2 → 3" = "#F58518")) +
      labs(
        title = "Average total-stat gain by type and evolution step",
        x = NULL, y = "Average total-stat gain", fill = "Transition"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.major.y = element_blank()
      )
  })

  output$distribution_plot <- renderPlot({
    metric_lab <- c(
      total_gain = "Total gain",
      offense_gain = "Offense gain",
      defense_gain = "Defense gain",
      hp_gain = "HP gain",
      speed_gain = "Speed gain"
    )[input$metric]

    d <- base_data()$evolution_gain %>%
      filter(!(type_1 %in% input$exclude_types)) %>%
      mutate(
        highlight = if_else(type_1 == input$selected_type, "Selected type", "Other types")
      )

    ggplot(d, aes(x = reorder(type_1, .data[[input$metric]], FUN = mean), y = .data[[input$metric]])) +
      geom_boxplot(outlier.shape = NA, width = 0.55, fill = "grey88", color = "grey35") +
      geom_jitter(aes(color = highlight), width = 0.12, alpha = 0.5, size = 1.8) +
      stat_summary(fun = mean, geom = "point", size = 3.2, color = "#C0392B") +
      coord_flip() +
      scale_color_manual(values = c("Other types" = "grey55", "Selected type" = "#2A9D8F")) +
      labs(
        title = paste(metric_lab, "by type"),
        subtitle = "Dots show individual evolution events; red points show means",
        x = NULL,
        y = metric_lab,
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "top",
        panel.grid.major.y = element_blank()
      )
  })

  output$type_summary_table <- renderTable({
    base_data()$evolution_gain %>%
      filter(type_1 == input$selected_type) %>%
      group_by(type_1, stage_transition) %>%
      summarise(
        n = n(),
        mean_total_gain = round(mean(total_gain, na.rm = TRUE), 1),
        mean_offense_gain = round(mean(offense_gain, na.rm = TRUE), 1),
        mean_defense_gain = round(mean(defense_gain, na.rm = TRUE), 1),
        mean_hp_gain = round(mean(hp_gain, na.rm = TRUE), 1),
        mean_speed_gain = round(mean(speed_gain, na.rm = TRUE), 1),
        .groups = "drop"
      )
  })
}

shinyApp(ui, server)
