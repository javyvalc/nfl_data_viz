library(shiny)
library(nflverse)
library(bslib)
library(tidyverse)

app_data_totals <- nflreadr::load_player_stats() |>
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) |>
  filter(season_type == "REG") |>
  group_by(player_id, player_name, player_display_name, position, team_color, team_color2) |>
  summarise(
    average_fantasy_points_ppr = mean(fantasy_points_ppr, na.rm = TRUE),
    total_fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE)
  )
app_data_teams <- nflreadr::load_team_stats()
ui <- fluidPage(
  titlePanel("NFL Data Explorer"),
  theme = bs_theme(preset = "litera"),
  sidebarLayout(
    sidebarPanel(
      accordion(
        accordion_panel(
          "Running Backs controls:",
          sliderInput("obs_rb",
                      "Top X Running Backs:",
                      min = 1,
                      max = 20,
                      value = 5)
        ),
        accordion_panel(
          "Quarterbacks controls:",
          sliderInput("obs_qb",
                      "Top X QBs:",
                      min = 1,
                      max = 20,
                      value =5)
        ),
        accordion_panel(
          "Wide Receivers controls:",
          sliderInput("obs_wr",
                      "Top X WRs:",
                      min = 1,
                      max = 20,
                      value =5)
        ),
        accordion_panel(
          "Tight Ends controls:",
          sliderInput("obs_te",
                      "Top X TEs:",
                      min = 1,
                      max = 20,
                      value =5)
        ),
        id = "sidebar_accordion",
        open = "Running Backs controls:"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "All Positions",
          plotOutput("top_player_by_position")
        ),
        tabPanel(
          "QB",
          plotOutput("top_n_qb_chart"),
          tableOutput("top_n_qb")
        ),
        tabPanel(
          "RB",
          plotOutput("rb_plot"),
          tableOutput("rb_table")
        ),
        tabPanel(
          "WR",
          plotOutput("wr_plot"),
          tableOutput("wr_table")
        ),
        tabPanel(
          "TE",
          plotOutput("te_plot"),
          tableOutput("te_table")
        ),
        tabPanel(
          "K",
          plotOutput("k_plot"),
          tableOutput("k_table")
        ),
        tabPanel(
          "FGs",
          plotOutput("gwfg")
        )
      )
    )
  )
)
server <- function(input, output) {
  output$top_player_by_position <- renderPlot({
    app_data_totals |>
      filter(position %in% c("QB", "RB", "WR", "TE")) |>
      group_by(position) |>
      slice_max(total_fantasy_points_ppr) |>
      ungroup() |>
      mutate(position_player_name = paste(position, player_display_name, sep = ": ")) |>
      ggplot(aes(
        x = fct_reorder(player_id, desc(total_fantasy_points_ppr)),
        y = total_fantasy_points_ppr,
        fill = team_color,
        color = team_color2
      )) +
      geom_col() +
      scale_fill_identity() +
      scale_colour_identity() +
      geom_text(
        aes(label = position_player_name), 
        vjust = 1.5,
        color = "white",
        size = 4
      ) +
      theme(axis.text.x.bottom = element_nfl_headshot(size = 2)) +
      labs(title = "Top Player for Fantasy Points Total By Position")
  })
  output$top_n_qb <- renderTable({
    app_data_totals |>
      filter(position == "QB") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_qb)
  })
  output$top_n_qb_chart <- renderPlot({
    app_data_totals |>
      filter(position == "QB") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_qb) |>
      ggplot(aes(x = fct_reorder(player_display_name, desc(total_fantasy_points_ppr)), y = total_fantasy_points_ppr, fill = team_color, color = team_color2)) +
      geom_col() +
      scale_fill_identity() +
      scale_color_identity() +
      geom_text(
        aes(label = total_fantasy_points_ppr, color = "white"),
        position = position_stack(0.5)
      )
  })
  output$wr_plot <- renderPlot({
    app_data_totals |>
      filter(position == "WR") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_wr) |>
      ggplot(aes(x = fct_reorder(player_display_name, desc(total_fantasy_points_ppr)), y = total_fantasy_points_ppr, fill = team_color, color = team_color2)) +
      geom_col() +
      scale_fill_identity() +
      scale_color_identity() +
      geom_text(
        aes(label = total_fantasy_points_ppr, color = "white"),
        position = position_stack(0.5)
      )
  })
  output$wr_table <- renderTable({
    app_data_totals |>
      filter(position == "WR") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_wr)
  })
  output$rb_plot <- renderPlot({
    app_data_totals |>
      filter(position == "RB") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_rb) |>
      ggplot(aes(x = fct_reorder(player_display_name, desc(total_fantasy_points_ppr)), y = total_fantasy_points_ppr, fill = team_color, color = team_color2)) +
      geom_col() +
      scale_fill_identity() +
      scale_color_identity() +
      geom_text(
        aes(label = total_fantasy_points_ppr, color = "white"),
        position = position_stack(0.5)
      )
  })
  output$rb_table <- renderTable({
    app_data_totals |>
      filter(position == "RB") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_rb)
  })
  output$te_plot <- renderPlot({
    app_data_totals |>
      filter(position == "TE") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_te) |>
      ggplot(aes(x = fct_reorder(player_display_name, desc(total_fantasy_points_ppr)), y = total_fantasy_points_ppr, fill = team_color, color = team_color2)) +
      geom_col() +
      scale_fill_identity() +
      scale_color_identity() +
      geom_text(
        aes(label = total_fantasy_points_ppr, color = "white"),
        position = position_stack(0.5)
      )
  })
  output$te_table <- renderTable({
    app_data_totals |>
      filter(position == "TE") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = input$obs_te)
  })
  output$k_plot <- renderPlot({
    app_data_totals |>
      filter(position == "K") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = 5) |>
      ggplot(aes(x = fct_reorder(player_display_name, desc(total_fantasy_points_ppr)), y = total_fantasy_points_ppr, fill = team_color, color = team_color2)) +
      geom_col() +
      scale_fill_identity() +
      scale_color_identity() +
      geom_text(
        aes(label = total_fantasy_points_ppr, color = "white"),
        position = position_stack(0.5)
      )
  })
  output$k_table <- renderTable({
    app_data_totals |>
      filter(position == "K") |>
      ungroup() |>
      mutate(rank_total_fantasy_points_ppr = dense_rank(desc(total_fantasy_points_ppr))) |>
      slice_max(order_by = total_fantasy_points_ppr, n = 5)
  })
  output$gwfg <- renderPlot({
    teams_stats_gwfg <- app_data_teams |>
      left_join(teams_colors_logos, by = c("team" = "team_abbr")) |>
      filter(gwfg_distance > 0, gwfg_made == 1) |>
      arrange(desc(gwfg_distance)) |>
      mutate(game_id = fct_inorder(game_id)) |>
      select(week, team, opponent_team, gwfg_distance, team_color, team_color2, game_id)
    
    ggplot(teams_stats_gwfg, aes(x = game_id, y = gwfg_distance)) +
      geom_col(aes(fill = team, color = team)) +
      scale_fill_nfl() +
      scale_color_nfl() +
      scale_x_discrete(labels = teams_stats_gwfg$team) +
      theme(
        axis.text.x = element_nfl_logo()
      ) +
      geom_hline(aes(yintercept = mean(gwfg_distance, na.rm = TRUE)),
                 color = "red",
                 linetype = "dotted",
                 linewidth = 1) +
      geom_text(aes(x = 20, y = mean(gwfg_distance), label = "Average Game Winning FG Distance"),
                vjust = -0.5, hjust = 0, color = "red")
  })
}
shinyApp(ui, server)
