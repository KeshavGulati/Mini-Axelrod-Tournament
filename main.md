# Simulating a Tournament for Different Strategies in an Iterated Prisoner’s Dilemma

## Tit For Tat

    #' @param player_1_history All moves that player 1 has made so far
    #' @param player_2_history All moves that player 2 has made so far
    #' @param player Which player is using titfortat
    #' @return <list> player_1_history or player_2_history depending on which player uses titfortat
    tit_for_tat <- function(player_1_history, player_2_history, player) {
      if (player == "player1") {
        # If this is the first move, then we co-operate
        if (length(player_2_history) == 0) {
          player_1_history <- append(player_1_history, 1)

        } else {
          # otherwise, copy the other player's last move
          player_1_history <- append(player_1_history, player_2_history[length(player_2_history)])

        }

        player_1_history

      } else {
        # If this is the first move, then we co-operate
        if (length(player_2_history) == 0) {
          player_2_history <- append(player_2_history, 1)

        } else {
          # otherwise, copy the oher player's last move
          player_2_history <- append(player_2_history, player_1_history[length(player_1_history)])

        }

        player_2_history

      }

    }

## Grim Trigger

    #' @param player_1_history All moves that player 1 has made so far
    #' @param player_2_history All moves that player 2 has made so far
    #' @param player Which player is using grim trigger
    #' @return <list> player_1_history or player_2_history depending on which player uses titfortat
    grim_trigger <- function(player_1_history, player_2_history, player) {
      if (player == "player1") {
        # If this is the first move, then we co-operate
        if (length(player_1_history) == 0) {
          player_1_history <- append(player_1_history, 1)

        } else {
          # Otherwise, we defect if the oter player has defected even once
          if (0 %in% player_2_history) {
          player_1_history <- append(player_1_history, 0)  # Player 1 defects if Player 2 has defected
          
          } else {
            player_1_history <- append(player_1_history, 1)  # Player 1 cooperates if Player 2 has not defected

          }

        }

        return(player_1_history)

      } else {
        # If this is the first move, then we co-operate
        if (length(player_2_history) == 0) {
          player_2_history <- append(player_2_history, 1)

        } else {
          # Otherwise, we defect if the other player has defected even once
          if (0 %in% player_1_history) {
            player_2_history <- append(player_2_history, 0)  # Player 2 defects if Player 1 has defected
          
          } else {
            player_2_history <- append(player_2_history, 1)  # Player 2 cooperates if Player 1 has not defected

          }

        }

        return(player_2_history)

      }

    }

## Always Defect

    #' @param player_1_history All moves that player 1 has made so far
    #' @param player_2_history All moves that player 2 has made so far
    #' @param player Which player is using always defect
    #' @return <list> player_1_history or player_2_history depending on which player uses always defect
    always_defect <- function(player_1_history, player_2_history, player) {
      if (player == "player1") {
        player_1_history <- append(player_1_history, 0)  # Player 1 always defects
        return(player_1_history)

      } else {
        player_2_history <- append(player_2_history, 0)  # Player 2 always defects
        return(player_2_history)

      }

    }

## Always Cooperate

    #' @param player_1_history All moves that player 1 has made so far
    #' @param player_2_history All moves that player 2 has made so far
    #' @param player Which player is using always cooperate
    #' @return <list> player_1_history or player_2_history depending on which player uses always cooperate
    always_cooperate <- function(player_1_history, player_2_history, player) {
      if (player == "player1") {
        player_1_history <- append(player_1_history, 1)  # Player 1 always cooperates
        return(player_1_history)

      } else {
        player_2_history <- append(player_2_history, 1)  # Player 2 always cooperates
        return(player_2_history)

      }

    }

## Random Strategy

    #' @param player_1_history All moves that player 1 has made so far
    #' @param player_2_history All moves that player 2 has made so far
    #' @param player Which player is using random strategy
    #' @return <list> player_1_history or player_2_history depending on which player uses random strategy
    random_strategy <- function(player_1_history, player_2_history, player) {
      if (player == "player1") {
        player_1_history <- append(player_1_history, sample(0:1, 1))  # Player 1 randomly chooses to cooperate or defect
        return(player_1_history)

      } else {
        player_2_history <- append(player_2_history, sample(0:1, 1))  # Player 2 randomly chooses to cooperate or defect
        return(player_2_history)

      }

    }

## Evaluation

    evaluate <- function(v1, v2) {
      # Initialize scores
      score1 <- score2 <- 0
      score_1_arr <- numeric(length(v1))
      score_2_arr <- numeric(length(v2))
      
      # Initialize scores array
      score_1_arr[1] <- score1
      score_2_arr[1] <- score2
      
      # Iterate through the moves
      for (i in seq_along(v1)) {
        if (v1[i] == 1 && v2[i] == 1) {
          score1 <- score1 + 3
          score2 <- score2 + 3
        } else if (v1[i] == 0 && v2[i] == 0) {
          score1 <- score1 + 1
          score2 <- score2 + 1
        } else if (v1[i] == 1 && v2[i] == 0) {
          score2 <- score2 + 5
        } else {
          score1 <- score1 + 5
        }

        # Update scores array
        score_1_arr[i] <- score1
        score_2_arr[i] <- score2

      }

      print("inside evaluate")
      cat("score_1_arr = ", score_1_arr, "\n")
      cat("score_2_arr = ", score_2_arr, "\n")
      
      return(list(score1 = score1, 
                  score2 = score2, 
                  score_1_arr = score_1_arr, 
                  score_2_arr = score_2_arr
                )
              )

    }

    evaluate_and_plot <- function(player_1_moves, player_2_moves) {
      temp <- evaluate(player_1_moves, player_2_moves)
      player_1_score <- temp$score1
      player_2_score <- temp$score2
      player_1_score_arr <- temp$score_1_arr
      player_2_score_arr <- temp$score_2_arr

      xValue <- 1:100
      yValue <- c(1, max(player_1_score, player_2_score))

      plot(player_1_score_arr ~ xValue, type="b", 
        col=rgb(0, 0, 1, 1), bty="l", xlab="Iterations", ylab="Score", lwd=3, pch=17, ylim=yValue
      )

      lines(player_2_score_arr ~ xValue, col=rgb(1, 0, 0, 0.3), lwd=3, pch=19, type="b")

      legend("bottomleft",
            legend = c("Player 1", "Player 2"),
            pch = c(17, 19),
            col=c("blue", "red"))

      text(x = 5, y = max(player_1_score_arr, player_2_score_arr) - 2,
           labels = paste("Player 1 Score:", player_1_score), col = "black", cex = 1.5)
      
      text(x = 5, y = max(player_1_score_arr, player_2_score_arr) - 20,
           labels = paste("Player 2 Score:", player_2_score), col = "black", cex = 1.5)

      return(list(player_1_score = player_1_score, 
                  player_2_score = player_2_score, 
                  player_1_score_arr = player_1_score_arr, 
                  player_2_score_arr = player_2_score_arr
                )
              )

      
    }

## The Tournament

    library(ggplot2)
    tft_final_score <- 0
    gt_final_score <- 0
    ac_final_score <- 0
    ad_final_score <- 0
    rs_final_score <- 0

### Tit for Tat

    # Tit for Tat vs Grim Trigger
    player_1_moves <- player_2_moves <- c()


    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- tit_for_tat(player_1_moves, player_2_moves, "player1")
      player_2_moves <- grim_trigger(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/titfortat vs grim trigger.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    tft_final_score <- tft_final_score + temp$player_1_score
    gt_final_score <- gt_final_score + temp$player_2_score

    dev.off()  # Close device and write file

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for titfortat score and
grim trigger score against
iterations](./plots/titfortat%20vs%20grim%20trigger.png)

    # Tit for Tat vs Always Co-operate
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- tit_for_tat(player_1_moves, player_2_moves, "player1")
      player_2_moves <- always_cooperate(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/titfortat vs always co-operate.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    tft_final_score <- tft_final_score + temp$player_1_score
    ac_final_score <- ac_final_score + temp$player_2_score

    dev.off()  # Close device and write file

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for titfortat score and
always co-operate score against
iterations](./plots/titfortat%20vs%20always%20co-operate.png)

    # Tit for Tat vs Always Defect
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- tit_for_tat(player_1_moves, player_2_moves, "player1")
      player_2_moves <- always_defect(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/titfortat vs always defect.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    tft_final_score <- tft_final_score + temp$player_1_score
    ad_final_score <- ad_final_score + temp$player_2_score

    dev.off()  # Close device and write file

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for titfortat score and
always defect score against
iterations](./plots/titfortat%20vs%20always%20defect.png)

    # Tit for Tat vs Random
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- tit_for_tat(player_1_moves, player_2_moves, "player1")
      player_2_moves <- random_strategy(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/titfortat vs random.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 3 8 11 14 14 19 19 24 27 30 30 31 36 39 39 44 47 50 50 51 52 53 58 61 64 67 67 68 73 76 79 82 82 87 90 90 95 98 98 103 106 106 107 108 113 113 114 115 116 117 122 125 125 126 131 131 136 139 142 145 145 150 153 153 158 161 164 167 167 172 172 177 177 178 183 186 186 191 191 196 196 201 204 207 210 210 215 218 221 224 227 227 232 235 235 240 240 245 245 
    ## score_2_arr =  3 8 8 11 14 19 19 24 24 27 30 35 36 36 39 44 44 47 50 55 56 57 58 58 61 64 67 72 73 73 76 79 82 87 87 90 95 95 98 103 103 106 111 112 113 113 118 119 120 121 122 122 125 130 131 131 136 136 139 142 145 150 150 153 158 158 161 164 167 172 172 177 177 182 183 183 186 191 191 196 196 201 201 204 207 210 215 215 218 221 224 227 232 232 235 240 240 245 245 250

    tft_final_score <- tft_final_score + temp$player_1_score
    rs_final_score <- rs_final_score + temp$player_2_score

    dev.off()  # Close device and write file

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for titfortat score and
random score against iterations](./plots/titfortat%20vs%20random.png)

## Grim Trigger

    # Grim Trigger vs Always Co-operate
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- grim_trigger(player_1_moves, player_2_moves, "player1")
      player_2_moves <- always_cooperate(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/grim trigger vs always co-operate.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    gt_final_score <- gt_final_score + temp$player_1_score
    ac_final_score <- ac_final_score + temp$player_2_score

    dev.off()  # Close device and write file

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for grim trigger score and
always co-operate score against
iterations](./plots/grim%20trigger%20vs%20always%20co-operate.png)

    # Grim Trigger vs Always Defect
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- grim_trigger(player_1_moves, player_2_moves, "player1")
      player_2_moves <- always_defect(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/grim trigger vs always defect.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    gt_final_score <- gt_final_score + temp$player_1_score
    ad_final_score <- ad_final_score + temp$player_2_score

    dev.off()  # Close device and write file

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for grim trigger score and
always co-operate score against
iterations](./plots/grim%20trigger%20vs%20always%20defect.png)

    # Grim Trigger vs Random
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- grim_trigger(player_1_moves, player_2_moves, "player1")
      player_2_moves <- random_strategy(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/grim trigger vs random.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 6 11 16 21 26 31 36 37 38 39 44 45 46 47 52 53 58 63 64 69 70 71 72 77 82 83 84 89 94 99 104 105 106 111 116 117 118 119 124 129 130 131 132 133 138 139 144 145 146 147 148 149 150 155 160 165 170 171 176 181 186 191 196 197 202 203 208 209 210 211 212 213 218 219 220 225 226 231 236 241 242 247 248 253 258 263 268 269 274 275 276 281 282 283 288 289 290 295 
    ## score_2_arr =  3 6 11 11 11 11 11 11 11 12 13 14 14 15 16 17 17 18 18 18 19 19 20 21 22 22 22 23 24 24 24 24 24 25 26 26 26 27 28 29 29 29 30 31 32 33 33 34 34 35 36 37 38 39 40 40 40 40 40 41 41 41 41 41 41 42 42 43 43 44 45 46 47 48 48 49 50 50 51 51 51 51 52 52 53 53 53 53 53 54 54 55 56 56 57 58 58 59 60 60

    gt_final_score <- gt_final_score + temp$player_1_score
    rs_final_score <- rs_final_score + temp$player_2_score

    dev.off()  # Close device and write file

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for grim trigger score and
random score against
iterations](./plots/grim%20trigger%20vs%20random.png)

### Always Co-operate

    # Always Co-operate vs Always Defect
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- always_cooperate(player_1_moves, player_2_moves, "player1")
      player_2_moves <- always_defect(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/always co-operate vs always defect.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    ## score_2_arr =  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250 255 260 265 270 275 280 285 290 295 300 305 310 315 320 325 330 335 340 345 350 355 360 365 370 375 380 385 390 395 400 405 410 415 420 425 430 435 440 445 450 455 460 465 470 475 480 485 490 495 500

    ac_final_score <- ac_final_score + temp$player_1_score
    ad_final_score <- ad_final_score + temp$player_2_score

    dev.off()  # Close device and write filealways co-operate

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for alays co-operate score
and always defect score against
iterations](./plots/always%20co-operate%20vs%20always%20defect.png)

    # Always Co-operate vs Random
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- always_cooperate(player_1_moves, player_2_moves, "player1")
      player_2_moves <- random_strategy(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/always co-operate vs random.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 6 9 9 9 9 12 15 15 18 21 21 24 24 27 27 27 27 27 27 27 27 30 30 33 36 39 39 42 45 45 45 45 45 48 51 51 51 54 57 60 63 63 63 66 69 72 72 75 75 78 81 84 87 87 90 93 96 99 102 102 105 108 108 111 111 111 114 117 120 120 123 123 126 126 129 129 129 129 129 132 135 135 138 141 141 144 147 150 150 150 150 153 156 156 156 156 159 159 
    ## score_2_arr =  3 6 11 14 19 24 29 32 35 40 43 46 51 54 59 62 67 72 77 82 87 92 97 100 105 108 111 114 119 122 125 130 135 140 145 148 151 156 161 164 167 170 173 178 183 186 189 192 197 200 205 208 211 214 217 222 225 228 231 234 237 242 245 248 253 256 261 266 269 272 275 280 283 288 291 296 299 304 309 314 319 322 325 330 333 336 341 344 347 350 355 360 365 368 371 376 381 386 389 394

    ac_final_score <- ac_final_score + temp$player_1_score
    rs_final_score <- rs_final_score + temp$player_2_score

    dev.off()  # Close device and write filealways co-operate

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for alays co-operate score
and always defect score against
iterations](./plots/always%20co-operate%20vs%20random.png)

### Always Defect

    # Always Defect vs Random
    player_1_moves <- player_2_moves <- c()

    # We assume player 1 plays tit for tat and player 2 plays grim trigger
    for (i in 1:100) {
      player_1_moves <- always_defect(player_1_moves, player_2_moves, "player1")
      player_2_moves <- random_strategy(player_1_moves, player_2_moves, "player2")

    }

    png("./plots/always defect vs random.png", width = 1200, height = 600)

    temp <- evaluate_and_plot(player_1_moves, player_2_moves)

    ## [1] "inside evaluate"
    ## score_1_arr =  5 6 11 12 17 18 19 24 25 30 31 36 37 42 43 44 45 46 51 52 53 54 55 60 65 70 71 76 81 82 83 84 85 86 87 92 97 98 99 100 101 106 107 112 117 122 127 128 133 138 143 144 145 150 155 160 165 170 175 176 177 182 187 188 189 194 199 204 209 210 211 216 217 218 223 228 233 234 239 244 245 246 251 256 257 262 267 272 273 278 283 288 289 290 295 300 305 310 315 320 
    ## score_2_arr =  0 1 1 2 2 3 4 4 5 5 6 6 7 7 8 9 10 11 11 12 13 14 15 15 15 15 16 16 16 17 18 19 20 21 22 22 22 23 24 25 26 26 27 27 27 27 27 28 28 28 28 29 30 30 30 30 30 30 30 31 32 32 32 33 34 34 34 34 34 35 36 36 37 38 38 38 38 39 39 39 40 41 41 41 42 42 42 42 43 43 43 43 44 45 45 45 45 45 45 45

    ad_final_score <- ad_final_score + temp$player_1_score
    rs_final_score <- rs_final_score + temp$player_2_score

    dev.off()  # Close device and write filealways co-operate

    ## png 
    ##   2

**Results** ![A graph showing two lines plots for always defect score
and random score against
iterations](./plots/always%20defect%20vs%20random.png)

## Comparison

    # Standardizing the scores
    mean_x <- mean(c(tft_final_score, gt_final_score, ac_final_score, ad_final_score, rs_final_score))
    var_x <- var(c(tft_final_score, gt_final_score, ac_final_score, ad_final_score, rs_final_score))
    tft <- (tft_final_score - mean_x) / var_x
    gt <- (gt_final_score - mean_x) / var_x
    ac <- (ac_final_score - mean_x) / var_x
    ad <- (ad_final_score - mean_x) / var_x
    rs <- (rs_final_score - mean_x) / var_x

    png("comparison.png", width=1200, height=600)

    data <- data.frame(
      x = c("Tit for Tat", "Grim Trigger", "Always Co-operate", "Always Defect", "Random"),
      y = c(tft, gt, ac, ad, rs)
    )

    ggplot(data, aes(x = x, y = y)) + 
      geom_segment(aes(x = x, xend = x, y = 0, yend = y), size = 2, color = "blue", linetype = "dotdash") + 
      geom_point(size=5) + 
      labs(x = "Strategy", y = "Final Score") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 16),     # x-axis label size
        axis.text.y = element_text(size = 14),     # y-axis label size
        axis.title.x = element_text(size = 18),    # x-axis title size
        axis.title.y = element_text(size = 18),    # y-axis title size
        plot.title = element_text(size = 20, face = "bold")
      
      )

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

    dev.off()

    ## png 
    ##   2

**Results** ![A lollipop chart showing each strategy’s total
point](comparison.png)

<br> <br> <br> <br> <br>
