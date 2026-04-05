# app.R — NCAA Bracket Lab: Power Model Critique & AdjEM Improvement
# STA345 Final Project

library(shiny)
library(bslib)
library(tidyverse)

source("R/utils.R")
source("R/models.R")
source("R/generators.R")
source("R/scoring.R")
source("R/bracket_viz.R")
source("R/diagnostics.R")

# Load and join data
games <- read_csv("data/combined.csv", show_col_types = FALSE) %>%
  mutate(round = case_when(
    round_of == 64 ~ 1, round_of == 32 ~ 2, round_of == 16 ~ 3,
    round_of ==  8 ~ 4, round_of ==  4 ~ 5, round_of ==  2 ~ 6
  ))

metrics <- read_csv("data/metrics.csv", show_col_types = FALSE) %>%
  mutate(AdjEM = AdjOE - AdjDE)

metrics_slim <- metrics %>%
  select(team = `Mapped ESPN Team Name`, Season, AdjEM)

games_rated <- games %>%
  left_join(metrics_slim, by = c("year" = "Season", "winning_team_name" = "team")) %>%
  rename(win_AdjEM = AdjEM) %>%
  left_join(metrics_slim, by = c("year" = "Season", "losing_team_name" = "team")) %>%
  rename(lose_AdjEM = AdjEM)

available_years <- sort(unique(games$year))
# AdjEM data available from 2002
adjem_years <- available_years[available_years >= 2013 & available_years != 2020]

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = "March Madness Bracket Lab",
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   primary = "#2c3e50", "navbar-bg" = "#2c3e50"),
  fillable = TRUE,

  # --- Tab 1: Critique -------------------------------------------------------
  nav_panel(
    title = "Power Model Critique",
    icon = icon("magnifying-glass"),
    layout_sidebar(
      sidebar = sidebar(
        title = "About", width = 300,
        tags$p("The Power Model (Ludden et al. 2020) predicts outcomes using:"),
        tags$p(tags$b("P(s1 beats s2) = s2^a / (s1^a + s2^a)")),
        tags$p("For Round 1, a is derived from observed win rates.
                For Rounds 2-6, a single a per round is used."),
        hr(),
        tags$p(tags$b("The Problem:"), "Solving for a from the observed
                win rate and plugging it back in gives the exact same win rate.
                The model is circular.")
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Circularity: p_bar == p_hat for Every Round 1 Matchup"),
          card_body(tableOutput("circularity_table"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Round 1 Alpha Values"),
          card_body(plotOutput("r1_alpha_plot", height = "380px"))
        ),
        card(
          card_header("Rounds 2-6: One Alpha Hides Real Variation"),
          card_body(plotOutput("pooling_plot", height = "380px"))
        )
      )
    )
  ),

  # --- Tab 2: Bracket Generator -----------------------------------------------
  nav_panel(
    title = "Generate Bracket",
    icon = icon("basketball"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Settings", width = 280,
        selectInput("gen_model", "Model",
                    choices = c("Power Model" = "power",
                                "AdjEM Model (Ours)" = "adjem")),
        selectInput("gen_type", "Generator",
                    choices = c("R64 (Full Sim)" = "R64",
                                "E8 (Elite Eight)" = "E8",
                                "F4A (Final Four)" = "F4A",
                                "Pick Favorite" = "PF")),
        selectInput("gen_year", "Tournament Year",
                    choices = rev(adjem_years), selected = 2024),
        actionButton("gen_go", "Generate Bracket",
                     class = "btn-primary btn-lg w-100"),
        hr(),
        checkboxInput("show_comparison", "Show Power vs AdjEM side-by-side",
                      value = FALSE),
        hr(),
        uiOutput("bracket_info")
      ),
      card(
        card_body(
          conditionalPanel(
            condition = "!input.show_comparison",
            plotOutput("bracket_plot", height = "700px")
          ),
          conditionalPanel(
            condition = "input.show_comparison",
            layout_columns(
              col_widths = c(6, 6),
              div(tags$h5(textOutput("title_power"), style = "text-align:center;"),
                  plotOutput("bracket_power", height = "600px")),
              div(tags$h5(textOutput("title_adjem"), style = "text-align:center;"),
                  plotOutput("bracket_adjem", height = "600px"))
            )
          )
        )
      )
    )
  ),

  # --- Tab 3: Results ---------------------------------------------------------
  nav_panel(
    title = "Results",
    icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Experiment", width = 280,
        selectInput("res_years", "Years", choices = adjem_years,
                    selected = c(2019, 2021, 2022, 2023, 2024), multiple = TRUE),
        sliderInput("res_n", "Brackets per pool", 100, 2000, 500, 100),
        sliderInput("res_r", "Replications", 1, 10, 5),
        actionButton("res_go", "Run Comparison",
                     class = "btn-primary btn-lg w-100"),
        hr(),
        uiOutput("res_status")
      ),
      layout_columns(
        col_widths = 12,
        card(card_header("Power vs AdjEM (R64 Generator)"),
             card_body(tableOutput("res_table")))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Max Score by Year"),
             card_body(plotOutput("res_plot", height = "350px"))),
        card(card_header("Summary"),
             card_body(tableOutput("res_summary")))
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --- Tab 1: Critique (computed once on load) --------------------------------

  all_train <- setdiff(available_years, 2020)

  output$circularity_table <- renderTable({
    build_circularity_table(games_rated, all_train)
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  output$r1_alpha_plot <- renderPlot({
    plot_r1_alphas(games_rated, all_train)
  })

  output$pooling_plot <- renderPlot({
    plot_round_pooling(games_rated, all_train)
  })

  # --- Tab 2: Bracket Generator -----------------------------------------------

  gen_result <- eventReactive(input$gen_go, {
    year <- as.integer(input$gen_year)
    train_yrs <- setdiff(2002:(year - 1), 2020)
    train_yrs <- train_yrs[train_yrs %in% available_years]
    sp <- compute_sampling_params(games_rated, train_yrs)

    # Power bracket
    power_mat <- build_power_prob_matrix(build_power_model(games_rated, train_yrs))
    b_power <- switch(input$gen_type,
      "R64" = gen_R64(power_mat), "E8" = gen_E8(power_mat, sp$q_e8),
      "F4A" = gen_F4A(power_mat, sp$q_f4), "PF" = gen_PF())

    # AdjEM bracket
    region_mats <- build_adjem_region_matrices(games_rated, metrics, train_yrs, year)
    b_adjem <- if (input$gen_type == "PF") gen_PF() else
      simulate_bracket_regionaware(region_mats)

    actual <- tryCatch(precompute_actual(games_rated, year), error = function(e) NULL)
    s_power <- if (!is.null(actual)) score_bracket_fast(b_power, actual) else NA
    s_adjem <- if (!is.null(actual)) score_bracket_fast(b_adjem, actual) else NA

    selected <- if (input$gen_model == "power") b_power else b_adjem
    selected_score <- if (input$gen_model == "power") s_power else s_adjem

    list(selected = selected, selected_score = selected_score,
         b_power = b_power, b_adjem = b_adjem,
         s_power = s_power, s_adjem = s_adjem,
         year = year, model = input$gen_model, gen_type = input$gen_type)
  })

  output$bracket_plot <- renderPlot({
    req(gen_result())
    g <- gen_result()
    label <- if (g$model == "power") "Power Model" else "AdjEM Model"
    draw_bracket(g$selected, title = paste0(label, " — ", g$gen_type, " — ", g$year))
  })

  output$bracket_power <- renderPlot({
    req(gen_result())
    draw_bracket(gen_result()$b_power, title = "Power Model")
  })

  output$bracket_adjem <- renderPlot({
    req(gen_result())
    draw_bracket(gen_result()$b_adjem, title = "AdjEM Model")
  })

  output$title_power <- renderText({
    req(gen_result())
    g <- gen_result()
    paste0("Power Model", if (!is.na(g$s_power)) paste0(" (", g$s_power, " pts)") else "")
  })

  output$title_adjem <- renderText({
    req(gen_result())
    g <- gen_result()
    paste0("AdjEM Model", if (!is.na(g$s_adjem)) paste0(" (", g$s_adjem, " pts)") else "")
  })

  output$bracket_info <- renderUI({
    req(gen_result())
    g <- gen_result()
    b <- g$selected
    tagList(
      tags$p(tags$b("Champion:"), paste("Seed", b$champion)),
      tags$p(tags$b("Final Four:"), paste(b$f4_seeds, collapse = ", ")),
      if (!is.na(g$selected_score))
        tags$p(tags$b("ESPN Score:"),
               tags$span(g$selected_score, style = "font-size:18px; color:#2c3e50;")),
      if (input$show_comparison) tagList(
        hr(),
        tags$p(tags$b("Power Score:"), g$s_power),
        tags$p(tags$b("AdjEM Score:"), g$s_adjem)
      )
    )
  })

  # --- Tab 3: Results ---------------------------------------------------------

  res_data <- eventReactive(input$res_go, {
    withProgress(message = "Running simulations...", {
      years <- as.integer(input$res_years)
      N <- input$res_n; R <- input$res_r

      results <- map_dfr(seq_along(years), function(idx) {
        yr <- years[idx]
        incProgress(1/length(years), detail = paste("Year", yr))
        train_yrs <- setdiff(2002:(yr-1), 2020)
        train_yrs <- train_yrs[train_yrs %in% available_years]

        power_mat <- build_power_prob_matrix(build_power_model(games_rated, train_yrs))
        region_mats <- build_adjem_region_matrices(games_rated, metrics, train_yrs, yr)
        actual <- precompute_actual(games_rated, yr)

        run_one <- function(model_name, gen_fn) {
          rmax <- numeric(R)
          for (r in 1:R) {
            best <- 0L
            for (i in 1:N) {
              b <- gen_fn()
              s <- score_bracket_fast(b, actual)
              if (s > best) best <- s
            }
            rmax[r] <- best
          }
          tibble(model = model_name, year = yr,
                 max_score = mean(rmax), max_score_sd = sd(rmax))
        }

        bind_rows(
          run_one("Power", function() gen_R64(power_mat)),
          run_one("AdjEM", function() simulate_bracket_regionaware(region_mats))
        )
      })
      results
    })
  })

  output$res_status <- renderUI({
    req(res_data())
    tags$p(tags$b("Done."), nrow(res_data()), "experiments")
  })

  output$res_table <- renderTable({
    req(res_data())
    res_data() %>%
      mutate(max_score = round(max_score, 0), max_score_sd = round(max_score_sd, 0)) %>%
      pivot_wider(names_from = model, values_from = c(max_score, max_score_sd)) %>%
      mutate(Improvement = max_score_AdjEM - max_score_Power) %>%
      select(Year = year, `Power Max` = max_score_Power, `AdjEM Max` = max_score_AdjEM,
             Improvement)
  }, striped = TRUE, hover = TRUE, width = "100%", digits = 0)

  output$res_plot <- renderPlot({
    req(res_data())
    res_data() %>%
      mutate(model = factor(model, levels = c("Power", "AdjEM"))) %>%
      ggplot(aes(x = factor(year), y = max_score, fill = model)) +
      geom_col(position = "dodge", width = 0.6) +
      geom_errorbar(aes(ymin = max_score - max_score_sd,
                        ymax = max_score + max_score_sd),
                    position = position_dodge(0.6), width = 0.2) +
      scale_fill_manual(values = c("Power" = "#e74c3c", "AdjEM" = "#2ecc71")) +
      labs(x = "Year", y = "Avg Max ESPN Score", fill = "Model") +
      theme_minimal()
  })

  output$res_summary <- renderTable({
    req(res_data())
    res_data() %>%
      group_by(Model = model) %>%
      summarise(`Avg Max Score` = round(mean(max_score), 0),
                `Best Year` = round(max(max_score), 0),
                .groups = "drop") %>%
      arrange(desc(`Avg Max Score`))
  }, striped = TRUE, hover = TRUE, width = "100%")
}

shinyApp(ui = ui, server = server)
