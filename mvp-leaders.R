library(hoopR)
library(dplyr)
library(readr)
library(stringr)

# ---------- Get Seasons ----------
get_seasons <- function(start, end) paste0(start:end, "-", substr(start:end + 1, 3, 4))

seasons_pts <- get_seasons(1951, 2024)
seasons_reb <- get_seasons(1951, 2024)
seasons_ast <- get_seasons(1951, 2024)
seasons_stl <- get_seasons(1974, 2024)  # STL tracked from 1973–74
seasons_blk <- get_seasons(1974, 2024)  # BLK tracked from 1973–74

all_pts_leaders <- data.frame()
all_reb_leaders <- data.frame()
all_ast_leaders <- data.frame()
all_stl_leaders <- data.frame()
all_blk_leaders <- data.frame()

# ---------- POINTS ----------
for (season in seasons_pts) {
  print(paste("Getting PTS", season))
  result <- nba_leagueleaders(season = season, stat_category = "PTS", per_mode = "Totals", season_type = "Regular Season")
  if (!is.null(result$LeagueLeaders) && nrow(result$LeagueLeaders) >= 5) {
    top5 <- result$LeagueLeaders %>%
      arrange(as.numeric(RANK)) %>%
      slice(1:5) %>%
      mutate(SEASON = season) %>%
      select(SEASON, RANK, PLAYER, TEAM, GP, PTS)
    all_pts_leaders <- bind_rows(all_pts_leaders, top5)
  }
}

# ---------- REBOUNDS ----------
for (season in seasons_reb) {
  print(paste("Getting REB", season))
  result <- nba_leagueleaders(season = season, stat_category = "REB", per_mode = "Totals", season_type = "Regular Season")
  if (!is.null(result$LeagueLeaders) && nrow(result$LeagueLeaders) >= 5) {
    top5 <- result$LeagueLeaders %>%
      arrange(as.numeric(RANK)) %>%
      slice(1:5) %>%
      mutate(SEASON = season) %>%
      select(SEASON, RANK, PLAYER, TEAM, GP, REB)
    all_reb_leaders <- bind_rows(all_reb_leaders, top5)
  }
}

# ---------- ASSISTS ----------
for (season in seasons_ast) {
  print(paste("Getting AST", season))
  result <- nba_leagueleaders(season = season, stat_category = "AST", per_mode = "Totals", season_type = "Regular Season")
  if (!is.null(result$LeagueLeaders) && nrow(result$LeagueLeaders) >= 5) {
    top5 <- result$LeagueLeaders %>%
      arrange(as.numeric(RANK)) %>%
      slice(1:5) %>%
      mutate(SEASON = season) %>%
      select(SEASON, RANK, PLAYER, TEAM, GP, AST)
    all_ast_leaders <- bind_rows(all_ast_leaders, top5)
  }
}

# ---------- STEALS ----------
for (season in seasons_stl) {
  print(paste("Getting STL", season))
  result <- nba_leagueleaders(season = season, stat_category = "STL", per_mode = "Totals", season_type = "Regular Season")
  if (!is.null(result$LeagueLeaders) && nrow(result$LeagueLeaders) >= 5) {
    top5 <- result$LeagueLeaders %>%
      arrange(as.numeric(RANK)) %>%
      slice(1:5) %>%
      mutate(SEASON = season) %>%
      select(SEASON, RANK, PLAYER, TEAM, GP, STL)
    all_stl_leaders <- bind_rows(all_stl_leaders, top5)
  }
}

# ---------- BLOCKS ----------
for (season in seasons_blk) {
  print(paste("Getting BLK", season))
  result <- nba_leagueleaders(season = season, stat_category = "BLK", per_mode = "Totals", season_type = "Regular Season")
  if (!is.null(result$LeagueLeaders) && nrow(result$LeagueLeaders) >= 5) {
    top5 <- result$LeagueLeaders %>%
      arrange(as.numeric(RANK)) %>%
      slice(1:5) %>%
      mutate(SEASON = season) %>%
      select(SEASON, RANK, PLAYER, TEAM, GP, BLK)
    all_blk_leaders <- bind_rows(all_blk_leaders, top5)
  }
}

# ---------- MVPs ----------
mvps <- read_csv("mvps.csv")

# Clean MVP names and seasons
mvps <- mvps %>%
  mutate(Player = str_replace(Player, "Nikola Joki_", "Nikola Jokić")) %>%
  rename(SEASON = Season, PLAYER = Player) %>%
  mutate(SEASON = str_trim(SEASON),
         PLAYER = str_trim(PLAYER))

# ---------- Count Top 5 Appearances ----------
mvp_stats <- mvps %>%
  rowwise() %>%
  mutate(
    Top5_PTS = any(PLAYER %in% all_pts_leaders$PLAYER[all_pts_leaders$SEASON == SEASON]),
    Top5_REB = any(PLAYER %in% all_reb_leaders$PLAYER[all_reb_leaders$SEASON == SEASON]),
    Top5_AST = any(PLAYER %in% all_ast_leaders$PLAYER[all_ast_leaders$SEASON == SEASON]),
    Top5_STL = any(PLAYER %in% all_stl_leaders$PLAYER[all_stl_leaders$SEASON == SEASON]),
    Top5_BLK = any(PLAYER %in% all_blk_leaders$PLAYER[all_blk_leaders$SEASON == SEASON])
  ) %>%
  mutate(
    Top5_Count = Top5_PTS + Top5_REB + Top5_AST + Top5_STL + Top5_BLK
  ) %>%
  ungroup()

# View results
View(mvp_stats %>% select(SEASON, PLAYER, Top5_PTS, Top5_REB, Top5_AST, Top5_STL, Top5_BLK, Top5_Count))

# Check top 3 appearances in each category
mvp_stats <- mvp_stats %>%
  rowwise() %>%
  mutate(
    Top3_PTS = any(PLAYER %in% all_pts_leaders$PLAYER[all_pts_leaders$SEASON == SEASON & as.numeric(all_pts_leaders$RANK) <= 3]),
    Top3_REB = any(PLAYER %in% all_reb_leaders$PLAYER[all_reb_leaders$SEASON == SEASON & as.numeric(all_reb_leaders$RANK) <= 3]),
    Top3_AST = any(PLAYER %in% all_ast_leaders$PLAYER[all_ast_leaders$SEASON == SEASON & as.numeric(all_ast_leaders$RANK) <= 3]),
    Top3_STL = any(PLAYER %in% all_stl_leaders$PLAYER[all_stl_leaders$SEASON == SEASON & as.numeric(all_stl_leaders$RANK) <= 3]),
    Top3_BLK = any(PLAYER %in% all_blk_leaders$PLAYER[all_blk_leaders$SEASON == SEASON & as.numeric(all_blk_leaders$RANK) <= 3])
  ) %>%
  mutate(
    Top3_Count = Top3_PTS + Top3_REB + Top3_AST + Top3_STL + Top3_BLK,
    NotTop5_PTS_REB_AST = !Top5_PTS & !Top5_REB & !Top5_AST,
    NotTop3_PTS_REB_AST = !Top3_PTS & !Top3_REB & !Top3_AST
  ) %>%
  ungroup()

# View summary
View(mvp_stats %>%
       select(SEASON, PLAYER, Top3_Count, NotTop5_PTS_REB_AST, NotTop3_PTS_REB_AST))

# MVPs who missed top 5 in PTS/REB/AST
mvps_missed_top5_core <- mvp_stats %>% 
  filter(NotTop5_PTS_REB_AST) %>%
  select(SEASON, PLAYER, Top3_Count, NotTop5_PTS_REB_AST, NotTop3_PTS_REB_AST)

# MVPs who missed top 3 in PTS/REB/AST
mvps_missed_top3_core <- mvp_stats %>% 
  filter(NotTop3_PTS_REB_AST) %>%
  select(SEASON, PLAYER, Top3_Count, NotTop5_PTS_REB_AST, NotTop3_PTS_REB_AST)
