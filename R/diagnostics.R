# diagnostics.R — Diagnostic functions for Power Model critique (Tab 1)
# Assumes utils.R and models.R are already sourced by app.R

# =============================================================================
# CIRCULARITY DEMONSTRATION
# Show that the Power Model's predicted probabilities for Round 1
# are identical to the observed frequencies — the model is circular.
# =============================================================================

build_circularity_table <- function(data, train_years) {
  train <- data %>%
    filter(year %in% train_years) %>%
    mutate(
      s1 = pmin(winning_team_seed, losing_team_seed),
      s2 = pmax(winning_team_seed, losing_team_seed),
      s1_won = (winning_team_seed == s1)
    )

  # Compute observed frequencies (p_bar) for each R1 matchup
  r1 <- train %>%
    filter(round == 1) %>%
    group_by(s1, s2) %>%
    summarise(
      games = n(),
      wins = sum(s1_won),
      p_bar = mean(s1_won),
      .groups = "drop"
    )

  # Compute alpha and then p_hat from the Power Model
  r1 <- r1 %>%
    rowwise() %>%
    mutate(
      alpha = compute_alpha(p_bar, s1, s2),
      p_hat = power_model_prob(alpha, s1, s2),
      difference = abs(p_bar - p_hat)
    ) %>%
    ungroup()

  r1 %>%
    transmute(
      Matchup = paste0("(", s1, ") vs (", s2, ")"),
      Games = games,
      `Observed Win Rate (p_bar)` = round(p_bar, 4),
      `Alpha` = round(alpha, 3),
      `Model Prediction (p_hat)` = round(p_hat, 4),
      `|Difference|` = format(difference, scientific = TRUE, digits = 2)
    )
}

# =============================================================================
# ROUND 1 ALPHA BAR CHART
# Show that alpha is just a monotonic transformation of frequency.
# =============================================================================

plot_r1_alphas <- function(data, train_years) {
  model <- build_power_model(data, train_years)

  df <- model$r1_alphas %>%
    mutate(
      matchup = paste0(s1, " vs ", s2),
      matchup = factor(matchup, levels = matchup[order(s1, s2)])
    )

  ggplot(df, aes(x = matchup, y = alpha, fill = p_bar)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_gradient2(low = "firebrick", mid = "gray90", high = "steelblue",
                         midpoint = 0.5, limits = c(0, 1),
                         name = "Win Rate") +
    labs(
      title = "Round 1 Alpha Values by Matchup",
      subtitle = "Alpha is just a 1-to-1 transformation of the observed win rate",
      x = "Seed Matchup", y = "Alpha"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
}

# =============================================================================
# ROUND 2-6 POOLING PROBLEM
# Show that using one alpha per round ignores large within-round variation.
# =============================================================================

plot_round_pooling <- function(data, train_years) {
  train <- data %>%
    filter(year %in% train_years) %>%
    mutate(
      s1 = pmin(winning_team_seed, losing_team_seed),
      s2 = pmax(winning_team_seed, losing_team_seed),
      s1_won = (winning_team_seed == s1)
    )

  # Compute matchup-specific win rates within each round (R2-R6)
  matchup_rates <- train %>%
    filter(round >= 2, s1 != s2) %>%
    group_by(round, s1, s2) %>%
    summarise(p_bar = mean(s1_won), n = n(), .groups = "drop") %>%
    filter(n >= 3) %>%
    mutate(
      matchup = paste0(s1, "v", s2),
      round_label = c("R32", "S16", "E8", "F4", "NCG")[round - 1]
    )

  # Compute the single pooled alpha per round (what Power Model uses)
  round_alphas <- matchup_rates %>%
    rowwise() %>%
    mutate(alpha = compute_alpha(p_bar, s1, s2)) %>%
    ungroup() %>%
    group_by(round, round_label) %>%
    summarise(
      pooled_alpha = weighted.mean(alpha, w = n),
      .groups = "drop"
    )

  # For each round, convert the pooled alpha back to a "predicted" p for
  # the median seed matchup, to show as a reference line
  round_avg_p <- round_alphas %>%
    mutate(ref_p = power_model_prob(pooled_alpha, 3, 8))

  ggplot(matchup_rates, aes(x = matchup, y = p_bar)) +
    geom_point(aes(size = n), color = "steelblue", alpha = 0.7) +
    geom_text(aes(label = n), size = 2.2, vjust = -1, color = "gray40") +
    facet_wrap(~round_label, scales = "free_x", nrow = 1) +
    labs(
      title = "Within-Round Win Rate Variation (Rounds 2-6)",
      subtitle = "The Power Model collapses all these into a single alpha per round",
      x = "Seed Matchup", y = "Observed Win Rate (better seed)",
      size = "Games"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
      strip.text = element_text(face = "bold")
    ) +
    ylim(0, 1)
}
