url_constructor <- function(start_date, end_date) {
    url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C=&hfSea=&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=", start_date,"&game_date_lt=", end_date,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
    return(url)
}
# =================================
scrape_statcast <- function(start_date, end_date = NULL) {
    require(tidyverse)
    # inputs: 
    #  start_date: the first day to download
    #  end_dat: the last day to download; if empty, use today's date
    # outputs:
    # return statcast data frame
    start_date <- as.Date(start_date)
    if(length(end_date) == 0) {
        end_date <- Sys.Date()
    } else {
        end_date <- as.Date(end_date)
    }
    if(start_date > end_date) {
        stop("\n start_date has to come before end_date")
    }
    # use sc_col_types to adapt to changes in Statcast
    sc_col_types <- list(
        col_character(), # pitch_type
        col_date("%Y-%m-%d"), # game_date
        col_double(), # release_speed
        col_double(), # release_pos_x
        col_double(), # release_pos_z
        col_character(), # player_name
        col_character(), # batter
        col_character(), # pitcher
        col_character(), # events
        col_character(), # description
        
        col_skip(), # spin_dir
        col_skip(), # spin_rate_depcrecated
        col_skip(), # break_angle_deprecated
        col_skip(), # break_length_deprecated
        col_integer(), # zone
        col_character(), # des
        col_character(), # game_type
        col_character(), # stand
        col_character(), # p_throw
        col_character(), # home_team
        
        col_character(), # away_team
        col_character(), # type
        col_integer(), # hit_location
        col_character(), # bb_type
        col_integer(), # balls
        col_integer(), # strikes
        col_integer(), # game_year
        col_double(), # pfx_x
        col_double(), # pfx_z
        col_double(), # plate_x
        
        col_double(), # plate_z
        col_character(), # on_3b
        col_character(), # on_2b
        col_character(), # on_1b
        col_integer(), # outs_when_up
        col_integer(), # inning
        col_character(), # inning_topbot
        col_double(), # hc_x
        col_double(), # hc_y
        col_skip(), # tfs_depcrecated
        
        col_skip(), # tfs_zulu_deprecated
        col_character(), # fielder_2
        col_character(), # umpire
        col_character(), # sv_id
        col_double(), # vx0
        col_double(), # vy0
        col_double(), # vz0
        col_double(), # ax
        col_double(), # ay
        col_double(), # az
        
        col_double(), # sz_top
        col_double(), # sz_bot
        col_double(), # hit_distance_sc
        col_double(), # launch_speed
        col_double(), # launch_angle
        col_double(), # effective_speed
        col_double(), # release_spin_rate
        col_double(), # release_extension
        col_character(), # game_pk
        col_character(), # pitcher.1
        
        col_character(), # fielder_2.1
        col_character(), # fielder_3
        col_character(), # fielder_4
        col_character(), # fielder_5
        col_character(), # fielder_6
        col_character(), # fielder_7
        col_character(), # fielder_8
        col_character(), # fielder_9
        col_double(), # release_pos_y
        col_double(), # estimated_ba_using_speedangle
        
        col_double(), # estimated_woba_using_speedangle
        col_double(), # woba_value
        col_double(), # woba_denom
        col_double(), # babip_value
        col_double(), # iso_value
        col_double(), # launch_speed_angle
        col_integer(), # at_bat_number
        col_integer(), # pitch_number
        col_character(), # pitch_name
        col_integer(), # home_score
        
        col_integer(), # away_score
        col_integer(), # bat_score
        col_integer(), # fld_score
        col_integer(), # post_away_score
        col_integer(), # post_home_score
        col_integer(), # post_bat_score
        col_integer(), # post_fld_score
        col_character(), # if_fielding_alignment
        col_character()  # of_fielding_alignment
    )
    if(end_date - start_date > 7) {
        # break up into chunk if the query is too large
        cat("\n Due to query limits by baseball savant, break up into 6-day chunks\n")
        
        dt0 <-seq(start_date, end_date, 6)
        dt1 <- dt0 + 5
        dt1[which.max(dt1)] <- end_date
        
        l <- length(dt0)
        downloads <- vector(l, mode = "list")
        pb <- txtProgressBar(min = 0, max = l, style = 3)
        for(i in seq_len(l)) {
            downloads[[i]] <-suppressWarnings(
                read_csv(url_constructor(dt0[i], dt1[i]),
                         col_types = sc_col_types)
            )
            setTxtProgressBar(pb, i)
        }
        out <- do.call('bind_rows', downloads)
    } else {
        out <- suppressWarnings(
            read_csv(url_constructor(start_date, end_date), 
                     col_types = sc_col_types)
        )
    }
    return(out)
}