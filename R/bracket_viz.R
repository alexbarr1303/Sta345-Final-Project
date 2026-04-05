# bracket_viz.R — NCAA tournament bracket visualization using ggplot2

library(ggplot2)

# =============================================================================
# BRACKET DRAWING
# =============================================================================

# Draw a full 64-team NCAA tournament bracket
# bracket: output from any generator (list with regions, f4_seeds, f4_winners, champion)
# team_names: optional named vector mapping "seed_region" to team names
# title: plot title
draw_bracket <- function(bracket, title = "NCAA Tournament Bracket", show_probs = FALSE, prob_mat = NULL) {

  # Layout constants
  # X: rounds go from outside in. Left side: rounds 1-4, Right side: rounds 1-4
  # Y: teams stacked vertically
  # Left regions: 1 (top-left), 2 (bottom-left)
  # Right regions: 3 (top-right), 4 (bottom-right)

  segments_df <- data.frame(x = numeric(), xend = numeric(),
                            y = numeric(), yend = numeric(),
                            stringsAsFactors = FALSE)
  labels_df <- data.frame(x = numeric(), y = numeric(), label = character(),
                          hjust = numeric(), size = numeric(),
                          is_upset = logical(),
                          stringsAsFactors = FALSE)

  # Helper: add a horizontal segment
  add_seg <- function(x1, y1, x2, y2) {
    segments_df[nrow(segments_df) + 1, ] <<- c(x1, x2, y1, y2)
  }

  # Helper: add a label
  add_label <- function(x, y, lab, hj = 0, sz = 2.8, upset = FALSE) {
    labels_df[nrow(labels_df) + 1, ] <<- list(x, y, lab, hj, sz, upset)
  }

  # --- Layout parameters ---
  # Each region has 16 slots in Round 1, 8 in R2, 4 in R3, 2 in R4
  # Total vertical space: 32 slots per side (2 regions stacked)
  # Spacing: Round 1 teams are 1 unit apart within a region

  slot_h <- 1  # vertical spacing between R1 teams

  # Region 1 (top-left): y positions 17-32
  # Region 2 (bottom-left): y positions 1-16
  # Region 3 (top-right): y positions 17-32
  # Region 4 (bottom-right): y positions 1-16

  # X positions for each round (left side goes right, right side goes left)
  # Left: R1=0, R2=2, R3=4, R4=6
  # Center: F4=8, NCG=10
  # Right: R1=20, R2=18, R3=16, R4=14

  left_x  <- c(0, 2.5, 5, 7.5)
  right_x <- c(20, 17.5, 15, 12.5)
  center_x <- 10

  # Draw one region
  draw_region <- function(region_data, region_idx) {
    is_left <- region_idx <= 2
    is_top  <- region_idx %in% c(1, 3)

    x_pos <- if (is_left) left_x else right_x
    y_offset <- if (is_top) 17 else 1
    hj <- if (is_left) 0 else 1

    # Round 1: 8 games = 16 teams
    r1_seeds <- region_data$r1
    for (i in 1:8) {
      matchup_seeds <- c(REGION_SEEDS[i, 1], REGION_SEEDS[i, 2])
      winner <- r1_seeds[i]

      y1 <- y_offset + (i - 1) * 2
      y2 <- y1 + 1

      # Draw the two team lines
      add_seg(x_pos[1], y1, x_pos[1] + 1.5, y1)
      add_seg(x_pos[1], y2, x_pos[1] + 1.5, y2)
      # Vertical connector
      add_seg(x_pos[1] + 1.5, y1, x_pos[1] + 1.5, y2)

      # Labels
      lab1 <- paste0("(", matchup_seeds[1], ")")
      lab2 <- paste0("(", matchup_seeds[2], ")")
      upset1 <- (winner == matchup_seeds[2])  # upset if higher seed won

      lx <- if (is_left) x_pos[1] + 0.1 else x_pos[1] - 0.1
      add_label(lx, y1 + 0.15, lab1, hj, 2.3, FALSE)
      add_label(lx, y2 + 0.15, lab2, hj, 2.3, FALSE)
    }

    # Round 2: 4 games
    r2_seeds <- region_data$r2
    for (i in 1:4) {
      y_center <- y_offset + (i - 1) * 4 + 1.5
      y1 <- y_center - 1
      y2 <- y_center + 1

      add_seg(x_pos[2], y1, x_pos[2] + 1.5, y1)
      add_seg(x_pos[2], y2, x_pos[2] + 1.5, y2)
      add_seg(x_pos[2] + 1.5, y1, x_pos[2] + 1.5, y2)

      # Winner label from R1
      pair <- c(region_data$r1[2*i - 1], region_data$r1[2*i])
      for (j in 1:2) {
        yy <- if (j == 1) y1 else y2
        is_up <- (pair[j] > min(pair))
        lx2 <- if (is_left) x_pos[2] + 0.1 else x_pos[2] - 0.1
        add_label(lx2, yy + 0.15, paste0("(", pair[j], ")"), hj, 2.3, is_up)
      }
    }

    # Round 3: 2 games (Sweet 16)
    r3_seeds <- region_data$r3
    for (i in 1:2) {
      y_center <- y_offset + (i - 1) * 8 + 3.5
      y1 <- y_center - 2
      y2 <- y_center + 2

      add_seg(x_pos[3], y1, x_pos[3] + 1.5, y1)
      add_seg(x_pos[3], y2, x_pos[3] + 1.5, y2)
      add_seg(x_pos[3] + 1.5, y1, x_pos[3] + 1.5, y2)

      pair <- region_data$r2[c(2*i - 1, 2*i)]
      for (j in 1:2) {
        yy <- if (j == 1) y1 else y2
        is_up <- (pair[j] > min(pair))
        lx3 <- if (is_left) x_pos[3] + 0.1 else x_pos[3] - 0.1
        add_label(lx3, yy + 0.15, paste0("(", pair[j], ")"), hj, 2.5, is_up)
      }
    }

    # Round 4: 1 game (Elite Eight → region winner)
    r4_seed <- region_data$r4
    y_center <- y_offset + 7.5
    y1 <- y_center - 4
    y2 <- y_center + 4

    add_seg(x_pos[4], y1, x_pos[4] + 1.5, y1)
    add_seg(x_pos[4], y2, x_pos[4] + 1.5, y2)
    add_seg(x_pos[4] + 1.5, y1, x_pos[4] + 1.5, y2)

    pair <- region_data$r3
    for (j in 1:2) {
      yy <- if (j == 1) y1 else y2
      is_up <- (pair[j] > min(pair))
      lx4 <- if (is_left) x_pos[4] + 0.1 else x_pos[4] - 0.1
      add_label(lx4, yy + 0.15, paste0("(", pair[j], ")"), hj, 2.8, is_up)
    }
  }

  # Draw all 4 regions
  for (i in 1:4) {
    draw_region(bracket$regions[[i]], i)
  }

  # Final Four labels
  f4 <- bracket$f4_seeds
  f4w <- bracket$f4_winners
  champ <- bracket$champion

  # F4 game 1 (regions 1 vs 2) — left side
  add_label(9, 20, paste0("F4: (", f4[1], ") vs (", f4[2], ")"), 0.5, 3.2, FALSE)
  add_label(9, 19, paste0("Winner: (", f4w[1], ")"), 0.5, 3.0,
            f4w[1] > min(f4[1], f4[2]))

  # F4 game 2 (regions 3 vs 4) — right side
  add_label(11, 20, paste0("F4: (", f4[3], ") vs (", f4[4], ")"), 0.5, 3.2, FALSE)
  add_label(11, 19, paste0("Winner: (", f4w[2], ")"), 0.5, 3.0,
            f4w[2] > min(f4[3], f4[4]))

  # Championship
  add_label(center_x, 16.5, paste0("Championship: (", f4w[1], ") vs (", f4w[2], ")"),
            0.5, 3.5, FALSE)
  champ_upset <- champ > min(f4w)
  add_label(center_x, 15.5, paste0("CHAMPION: Seed ", champ),
            0.5, 4.5, champ_upset)

  # --- Build the plot ---
  # Fix column types
  segments_df <- segments_df %>%
    mutate(across(everything(), as.numeric))

  labels_df <- labels_df %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      hjust = as.numeric(hjust),
      size = as.numeric(size),
      is_upset = as.logical(is_upset),
      label = as.character(label)
    )

  p <- ggplot() +
    geom_segment(data = segments_df,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 color = "gray40", linewidth = 0.3) +
    geom_text(data = labels_df %>% filter(!is_upset),
              aes(x = x, y = y, label = label, hjust = hjust),
              size = labels_df %>% filter(!is_upset) %>% pull(size),
              color = "gray20") +
    geom_text(data = labels_df %>% filter(is_upset),
              aes(x = x, y = y, label = label, hjust = hjust),
              size = labels_df %>% filter(is_upset) %>% pull(size),
              color = "red3", fontface = "bold") +
    # Region labels
    annotate("text", x = 1, y = 33.5, label = "Region 1", fontface = "bold", size = 4) +
    annotate("text", x = 1, y = 0, label = "Region 2", fontface = "bold", size = 4) +
    annotate("text", x = 19, y = 33.5, label = "Region 3", fontface = "bold", size = 4) +
    annotate("text", x = 19, y = 0, label = "Region 4", fontface = "bold", size = 4) +
    # Title
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    coord_cartesian(xlim = c(-1, 21), ylim = c(-1, 35))

  p
}

# =============================================================================
# SCORE DISTRIBUTION PLOT
# =============================================================================

plot_score_distributions <- function(results_df, year_filter = NULL) {
  df <- results_df
  if (!is.null(year_filter)) {
    df <- df %>% filter(year %in% year_filter)
  }

  scores_long <- df %>%
    select(generator, year, scores) %>%
    unnest(scores)

  ggplot(scores_long, aes(x = scores, fill = generator)) +
    geom_density(alpha = 0.35) +
    labs(title = "Bracket Score Distributions by Generator",
         x = "ESPN Score", y = "Density", fill = "Generator") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    facet_wrap(~year, scales = "free_y")
}

# =============================================================================
# MAX SCORE COMPARISON PLOT
# =============================================================================

plot_max_scores <- function(results_df) {
  ggplot(results_df, aes(x = factor(year), y = max_score_mean, fill = generator)) +
    geom_col(position = "dodge") +
    geom_errorbar(
      aes(ymin = max_score_mean - max_score_sd,
          ymax = max_score_mean + max_score_sd),
      position = position_dodge(0.9), width = 0.25
    ) +
    labs(title = "Average Max Score by Generator and Year",
         x = "Year", y = "Avg Max Score (ESPN)", fill = "Generator") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
}

# =============================================================================
# PERFORMANCE HEATMAP
# =============================================================================

plot_heatmap <- function(results_df) {
  results_df %>%
    select(generator, year, max_score_mean) %>%
    ggplot(aes(x = factor(year), y = generator, fill = max_score_mean)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(max_score_mean)), size = 3.5) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = "Max Score Heatmap: Generator x Year",
         x = "Year", y = "Generator", fill = "Avg Max Score") +
    theme_minimal()
}
