# Mini-Axelrod-Tournament
A humble replication of Axelrod's tournament involving different strategies for an iterated Prisoner's Dilemma
**(in progress)**

## Simulating a Tournament for Different Strategies in an Iterated Prisoner’s Dilemma

### Tit For Tat

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

### Grim Trigger

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

### Always Defect

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

### Always Cooperate

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

### Random Strategy

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

### Evaluation

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

    evaluate_and_plot <- function(player_1_moves, player_2_moves, player1, player2) {
      # cat("inide evaluate_and_plot, player_1_moves = ", player_1_moves, "\n")
      # cat("inide evaluate_and_plot, player_2_moves = ", player_2_moves, "\n")
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
            legend = c(player1, player2),
            pch = c(17, 19),
            col=c("blue", "red"))

      text(x = 50, y = max(player_1_score_arr, player_2_score_arr) - 2,
           labels = paste(player1, " Score:", player_1_score), col = "black", cex = 1.5)
      
      text(x = 50, y = max(player_1_score_arr, player_2_score_arr) - 20,
           labels = paste(player2, " Score:", player_2_score), col = "black", cex = 1.5)

      return(list(strat_1_score = player_1_score, 
                  strat_2_score = player_2_score, 
                  strat_1_score_arr = player_1_score_arr, 
                  strat_2_score_arr = player_2_score_arr
                )
              )

      
    }

### The Tournament

    library(ggplot2)
    tft_final_score <- 0
    gt_final_score <- 0
    ac_final_score <- 0
    ad_final_score <- 0
    rs_final_score <- 0

### Tit for Tat

    play <- function(strat1_func, strat2_func, strat1_name, strat2_name, strat1_final_score, strat2_final_score) {
      results <- list()

      # Four rounds
      for (i in 1:4) {
        strat1_moves <- strat2_moves <- c()

        # First round titfortat goes first, then alternate
        if (i == 1 || i == 3) {
          # 100 iterations
          for (j in 1:100) {
            # Player 1 = tft, Player 2 = gt
            strat1_moves <- strat1_func(strat1_moves, strat2_moves, "player1")
            strat2_moves <- strat2_func(strat1_moves, strat2_moves, "player2")

          }

        } else {
          # 100 iterations
          for (j in 1:100) {
            # Player 1 = gt, Player 2 = tft
            strat2_moves <- strat2_func(strat2_moves, strat1_moves, "player1")
            strat1_moves <- strat1_func(strat2_moves, strat1_moves, "player2")

          }

        }

        # cat("inside round, gt_moves = ", gt_moves)
        results[[i]] <- list(strat1_moves = strat1_moves, strat2_moves = strat2_moves)

      }

      # cat("results[[1]]$gt_moves = ", results[[1]]$gt_moves)

      plot_path <- paste("./plots/", strat1_name, " vs ", strat2_name, ".png", sep="")
      png(plot_path, width = 1200, height = 600)
      par(mfrow = c(1, 4))

      for (i in 1:4) {
        temp <- evaluate_and_plot(
                  results[[i]]$strat1_moves, 
                  results[[i]]$strat2_moves,
                  strat1_name,
                  strat2_name 
                )

      }

      par(mfrow = c(1, 1))
      dev.off()  # Close device and write file

      strat1_final_score <- strat1_final_score + temp$strat_1_score
      strat2_final_score <- strat2_final_score + temp$strat_2_score
      return(c(strat1_final_score, strat2_final_score))

    }

    temp <- play(tit_for_tat, grim_trigger, "Tit for Tat", "Grim Trigger", tft_final_score, gt_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    tft_final_score <- temp[1]
    gt_final_score <- temp[2]

**Results** ![A graph showing two lines plots for titfortat score and
grim trigger score against
iterations](./plots/Tit%20for%20Tat%20vs%20Grim%20Trigger.png)

    temp <- play(tit_for_tat, always_cooperate, "Tit for Tat", "Always Co-operate", tft_final_score, ac_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    tft_final_score <- temp[1]
    ac_final_score <- temp[2]

**Results** ![A graph showing two lines plots for titfortat score and
grim trigger score against
iterations](./plots/Tit%20for%20Tat%20vs%20Always%20Co-operate.png)

    temp <- play(tit_for_tat, always_defect, "Tit for Tat", "Always Defect", tft_final_score, ad_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    tft_final_score <- temp[1]
    ad_final_score <- temp[2]

**Results** ![A graph showing two lines plots for titfortat score and
grim trigger score against
iterations](./plots/Tit%20for%20Tat%20vs%20Always%20Defect.png)

    temp <- play(tit_for_tat, random_strategy, "Tit for Tat", "Random", tft_final_score, rs_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 5 5 6 7 12 12 13 14 15 16 17 22 25 25 30 33 33 34 35 40 40 41 42 47 47 52 55 58 61 64 64 65 70 73 76 79 79 80 85 88 91 94 94 95 100 100 101 106 106 111 114 114 115 120 123 123 124 125 126 131 134 134 139 142 142 147 150 153 153 158 158 163 166 169 169 170 171 176 179 179 180 181 186 189 189 190 195 198 198 203 206 209 209 210 215 218 218 219 224 
    ## score_2_arr =  5 5 10 11 12 12 17 18 19 20 21 22 22 25 30 30 33 38 39 40 40 45 46 47 47 52 52 55 58 61 64 69 70 70 73 76 79 84 85 85 88 91 94 99 100 100 105 106 106 111 111 114 119 120 120 123 128 129 130 131 131 134 139 139 142 147 147 150 153 158 158 163 163 166 169 174 175 176 176 179 184 185 186 186 189 194 195 195 198 203 203 206 209 214 215 215 218 223 224 224

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 5 8 9 12 13 16 19 20 23 26 27 30 33 34 37 40 43 46 47 48 49 50 53 54 55 56 59 60 63 66 69 72 75 76 79 82 85 88 91 92 95 96 99 102 105 106 107 108 111 112 115 116 119 122 123 124 127 128 129 130 133 134 137 138 139 142 143 144 147 150 153 156 157 160 163 164 167 170 173 176 179 180 183 186 189 192 193 194 195 198 201 202 203 204 207 210 213 
    ## score_2_arr =  5 6 7 10 13 14 17 18 21 24 25 28 31 32 35 38 39 42 45 48 51 52 53 54 55 58 59 60 61 64 65 68 71 74 77 80 81 84 87 90 93 96 97 100 101 104 107 110 111 112 113 116 117 120 121 124 127 128 129 132 133 134 135 138 139 142 143 144 147 148 149 152 155 158 161 162 165 168 169 172 175 178 181 184 185 188 191 194 197 198 199 200 203 206 207 208 209 212 215 218

    ## [1] "inside evaluate"
    ## score_1_arr =  0 5 5 10 13 16 16 21 24 24 25 30 30 35 38 38 43 43 48 48 53 53 58 58 59 64 64 69 72 75 78 81 84 87 87 92 95 98 101 101 102 103 104 109 109 114 117 120 120 121 122 127 127 132 135 138 141 144 147 147 152 155 158 158 163 166 169 172 172 173 174 175 176 177 182 182 183 184 189 189 190 191 192 193 198 198 199 200 205 205 206 211 214 217 220 220 221 226 226 231 
    ## score_2_arr =  5 5 10 10 13 16 21 21 24 29 30 30 35 35 38 43 43 48 48 53 53 58 58 63 64 64 69 69 72 75 78 81 84 87 92 92 95 98 101 106 107 108 109 109 114 114 117 120 125 126 127 127 132 132 135 138 141 144 147 152 152 155 158 163 163 166 169 172 177 178 179 180 181 182 182 187 188 189 189 194 195 196 197 198 198 203 204 205 205 210 211 211 214 217 220 225 226 226 231 231

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 7 10 13 16 17 18 19 20 21 22 25 26 29 32 35 36 37 40 41 42 43 44 45 46 47 50 51 52 53 56 57 58 61 64 67 70 73 76 77 78 81 84 87 90 93 96 97 100 103 106 107 108 111 114 115 116 117 118 119 122 125 126 129 132 135 136 139 140 141 144 147 148 151 152 153 156 157 158 159 160 163 164 167 168 169 170 171 174 175 178 181 182 185 188 189 190 191 194 
    ## score_2_arr =  3 6 7 10 13 16 17 18 19 20 21 22 25 26 29 32 35 36 37 40 41 42 43 44 45 46 47 50 51 52 53 56 57 58 61 64 67 70 73 76 77 78 81 84 87 90 93 96 97 100 103 106 107 108 111 114 115 116 117 118 119 122 125 126 129 132 135 136 139 140 141 144 147 148 151 152 153 156 157 158 159 160 163 164 167 168 169 170 171 174 175 178 181 182 185 188 189 190 191 194

    tft_final_score <- temp[1]
    rs_final_score <- temp[2]

**Results** ![A graph showing two lines plots for titfortat score and
grim trigger score against
iterations](./plots/Tit%20for%20Tat%20vs%20Random.png)

### Grim Trigger

    temp <- play(grim_trigger, always_cooperate, "Grim Trigger", "Always Co-operate", gt_final_score, ac_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300 
    ## score_2_arr =  3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 195 198 201 204 207 210 213 216 219 222 225 228 231 234 237 240 243 246 249 252 255 258 261 264 267 270 273 276 279 282 285 288 291 294 297 300

    gt_final_score <- temp[1]
    ac_final_score <- temp[2]

**Results** ![A graph showing two lines plots for grim trigger score and
always co-operate score against
iterations](./plots/Grim%20Trigger%20vs%20Always%20Co-operate.png)

    temp <- play(grim_trigger, always_defect, "Grim Trigger", "Always Defect", gt_final_score, ad_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
    ## score_2_arr =  5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104

    gt_final_score <- temp[1]
    ad_final_score <- temp[2]

**Results** ![A graph showing two lines plots for grim trigger score and
always defect score against
iterations](./plots/Grim%20Trigger%20vs%20Always%20Defect.png)

    temp <- play(grim_trigger, random_strategy, "Grim Trigger", "Random", gt_final_score, rs_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 1 2 3 4 9 14 15 16 17 18 23 24 25 30 35 40 41 46 51 52 53 58 63 64 69 74 75 76 81 86 91 92 93 94 95 100 101 102 103 108 109 110 111 116 121 126 127 132 137 138 143 148 149 154 155 160 165 170 171 172 173 174 175 176 181 182 183 184 185 190 195 196 201 202 203 204 209 210 211 212 217 222 223 224 225 226 227 232 237 238 239 244 245 246 247 252 257 258 259 
    ## score_2_arr =  5 6 7 8 9 9 9 10 11 12 13 13 14 15 15 15 15 16 16 16 17 18 18 18 19 19 19 20 21 21 21 21 22 23 24 25 25 26 27 28 28 29 30 31 31 31 31 32 32 32 33 33 33 34 34 35 35 35 35 36 37 38 39 40 41 41 42 43 44 45 45 45 46 46 47 48 49 49 50 51 52 52 52 53 54 55 56 57 57 57 58 59 59 60 61 62 62 62 63 64

    ## [1] "inside evaluate"
    ## score_1_arr =  3 4 9 14 15 20 25 26 27 28 33 38 43 44 45 46 51 52 57 58 59 64 69 74 75 80 81 86 87 88 93 98 103 104 105 106 107 112 117 118 119 120 125 126 127 128 133 134 139 140 145 146 147 152 157 158 163 164 165 170 171 172 177 182 187 188 193 198 203 204 205 210 215 220 221 226 231 232 233 234 239 244 249 250 255 260 261 266 271 276 281 282 287 288 289 294 295 300 301 302 
    ## score_2_arr =  3 4 4 4 5 5 5 6 7 8 8 8 8 9 10 11 11 12 12 13 14 14 14 14 15 15 16 16 17 18 18 18 18 19 20 21 22 22 22 23 24 25 25 26 27 28 28 29 29 30 30 31 32 32 32 33 33 34 35 35 36 37 37 37 37 38 38 38 38 39 40 40 40 40 41 41 41 42 43 44 44 44 44 45 45 45 46 46 46 46 46 47 47 48 49 49 50 50 51 52

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 9 12 15 15 16 17 22 23 24 25 26 31 32 37 42 47 48 53 58 59 64 65 70 75 80 85 90 95 100 101 102 107 108 109 114 115 116 121 126 127 132 133 134 139 140 145 150 151 156 157 158 159 160 165 170 171 172 173 178 179 180 185 186 187 192 197 198 203 204 205 206 207 212 217 218 219 220 221 226 231 236 241 246 251 256 261 266 267 272 273 274 279 280 285 286 287 288 293 
    ## score_2_arr =  3 6 9 12 15 20 21 22 22 23 24 25 26 26 27 27 27 27 28 28 28 29 29 30 30 30 30 30 30 30 30 31 32 32 33 34 34 35 36 36 36 37 37 38 39 39 40 40 40 41 41 42 43 44 45 45 45 46 47 48 48 49 50 50 51 52 52 52 53 53 54 55 56 57 57 57 58 59 60 61 61 61 61 61 61 61 61 61 61 62 62 63 64 64 65 65 66 67 68 68

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 7 12 17 18 19 20 21 26 27 32 33 34 39 40 45 50 55 56 57 62 63 68 73 78 83 88 93 98 103 104 109 110 115 120 121 122 127 132 137 142 147 152 157 162 163 164 165 170 175 176 181 186 187 188 193 198 203 208 213 214 215 220 225 230 231 236 241 242 247 248 253 258 263 264 265 266 267 268 269 274 275 276 281 282 287 292 293 294 295 296 301 306 311 312 313 318 319 320 
    ## score_2_arr =  3 6 7 7 7 8 9 10 11 11 12 12 13 14 14 15 15 15 15 16 17 17 18 18 18 18 18 18 18 18 18 19 19 20 20 20 21 22 22 22 22 22 22 22 22 22 23 24 25 25 25 26 26 26 27 28 28 28 28 28 28 29 30 30 30 30 31 31 31 32 32 33 33 33 33 34 35 36 37 38 39 39 40 41 41 42 42 42 43 44 45 46 46 46 46 47 48 48 49 50

    gt_final_score <- temp[1]
    rs_final_score <- temp[2]

**Results** ![A graph showing two lines plots for grim trigger score and
random score against
iterations](./plots/Grim%20Trigger%20vs%20Random.png)

### Always Co-operate

    temp <- play(always_cooperate, always_defect, "Always Co-operate", "Always Defect", ac_final_score, ad_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    ## score_2_arr =  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250 255 260 265 270 275 280 285 290 295 300 305 310 315 320 325 330 335 340 345 350 355 360 365 370 375 380 385 390 395 400 405 410 415 420 425 430 435 440 445 450 455 460 465 470 475 480 485 490 495 500

    ## [1] "inside evaluate"
    ## score_1_arr =  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    ## score_2_arr =  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250 255 260 265 270 275 280 285 290 295 300 305 310 315 320 325 330 335 340 345 350 355 360 365 370 375 380 385 390 395 400 405 410 415 420 425 430 435 440 445 450 455 460 465 470 475 480 485 490 495 500

    ## [1] "inside evaluate"
    ## score_1_arr =  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    ## score_2_arr =  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250 255 260 265 270 275 280 285 290 295 300 305 310 315 320 325 330 335 340 345 350 355 360 365 370 375 380 385 390 395 400 405 410 415 420 425 430 435 440 445 450 455 460 465 470 475 480 485 490 495 500

    ## [1] "inside evaluate"
    ## score_1_arr =  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    ## score_2_arr =  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 145 150 155 160 165 170 175 180 185 190 195 200 205 210 215 220 225 230 235 240 245 250 255 260 265 270 275 280 285 290 295 300 305 310 315 320 325 330 335 340 345 350 355 360 365 370 375 380 385 390 395 400 405 410 415 420 425 430 435 440 445 450 455 460 465 470 475 480 485 490 495 500

    ac_final_score <- temp[1]
    ad_final_score <- temp[2]

**Results** ![A graph showing two lines plots for alays co-operate score
and always defect score against
iterations](./plots/Always%20Co-operate%20vs%20Always%20Defect.png)

    temp <- play(always_cooperate, random_strategy, "Always Co-operate", "Random", ac_final_score, rs_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  0 3 3 6 6 9 12 12 12 12 12 12 15 18 21 24 24 27 27 27 27 30 33 33 36 39 39 42 45 45 45 48 48 51 54 54 57 57 57 60 60 63 63 63 63 66 69 72 75 78 78 81 84 84 84 87 90 93 96 96 96 96 99 99 99 99 99 99 102 102 105 108 108 108 108 108 108 108 111 114 117 117 117 117 117 120 120 120 120 120 123 123 123 123 126 126 126 129 132 132 
    ## score_2_arr =  5 8 13 16 21 24 27 32 37 42 47 52 55 58 61 64 69 72 77 82 87 90 93 98 101 104 109 112 115 120 125 128 133 136 139 144 147 152 157 160 165 168 173 178 183 186 189 192 195 198 203 206 209 214 219 222 225 228 231 236 241 246 249 254 259 264 269 274 277 282 285 288 293 298 303 308 313 318 321 324 327 332 337 342 347 350 355 360 365 370 373 378 383 388 391 396 401 404 407 412

    ## [1] "inside evaluate"
    ## score_1_arr =  0 0 3 6 9 12 15 18 18 21 21 24 24 27 30 30 33 36 39 39 42 42 45 45 45 48 51 54 57 60 60 60 63 66 66 69 72 72 75 78 78 78 78 78 81 81 81 81 81 81 84 87 90 93 96 96 99 99 102 102 105 108 108 111 114 117 120 120 120 123 123 123 126 126 129 129 132 135 135 138 138 138 141 144 147 147 147 147 150 150 150 153 153 156 156 156 159 159 159 159 
    ## score_2_arr =  5 10 13 16 19 22 25 28 33 36 41 44 49 52 55 60 63 66 69 74 77 82 85 90 95 98 101 104 107 110 115 120 123 126 131 134 137 142 145 148 153 158 163 168 171 176 181 186 191 196 199 202 205 208 211 216 219 224 227 232 235 238 243 246 249 252 255 260 265 268 273 278 281 286 289 294 297 300 305 308 313 318 321 324 327 332 337 342 345 350 355 358 363 366 371 376 379 384 389 394

    ## [1] "inside evaluate"
    ## score_1_arr =  3 6 6 9 12 12 12 12 12 12 12 12 15 18 21 21 24 27 27 27 30 30 33 36 39 39 42 42 45 48 48 48 48 48 51 51 51 54 57 60 60 60 60 60 60 60 60 60 60 63 63 66 69 69 72 75 78 78 81 81 84 87 87 87 90 93 93 93 96 96 96 99 102 102 105 108 108 111 114 117 120 120 123 126 126 126 126 129 129 129 132 135 138 141 141 144 144 147 150 153 
    ## score_2_arr =  3 6 11 14 17 22 27 32 37 42 47 52 55 58 61 66 69 72 77 82 85 90 93 96 99 104 107 112 115 118 123 128 133 138 141 146 151 154 157 160 165 170 175 180 185 190 195 200 205 208 213 216 219 224 227 230 233 238 241 246 249 252 257 262 265 268 273 278 281 286 291 294 297 302 305 308 313 316 319 322 325 330 333 336 341 346 351 354 359 364 367 370 373 376 381 384 389 392 395 398

    ## [1] "inside evaluate"
    ## score_1_arr =  3 3 3 6 9 9 12 15 18 21 24 24 27 30 33 33 36 39 42 45 45 45 48 48 51 51 51 51 51 54 54 54 54 57 57 57 57 57 60 63 66 69 69 69 69 69 72 75 75 75 78 78 78 81 84 87 90 93 93 93 93 96 96 99 99 102 102 105 108 111 111 114 114 114 117 117 117 120 120 120 123 126 126 129 132 132 135 138 138 138 138 138 141 144 144 144 144 144 144 144 
    ## score_2_arr =  3 8 13 16 19 24 27 30 33 36 39 44 47 50 53 58 61 64 67 70 75 80 83 88 91 96 101 106 111 114 119 124 129 132 137 142 147 152 155 158 161 164 169 174 179 184 187 190 195 200 203 208 213 216 219 222 225 228 233 238 243 246 251 254 259 262 267 270 273 276 281 284 289 294 297 302 307 310 315 320 323 326 331 334 337 342 345 348 353 358 363 368 371 374 379 384 389 394 399 404

    ac_final_score <- temp[1]
    rs_final_score <- temp[2]

**Results** ![A graph showing two lines plots for alays co-operate score
and random score against
iterations](./plots/Always%20Co-operate%20vs%20Random.png)

### Always Defect

    temp <- play(always_defect, random_strategy, "Always Defect", "Random", ad_final_score, rs_final_score)

    ## [1] "inside evaluate"
    ## score_1_arr =  5 10 11 12 13 18 23 28 33 34 39 44 49 54 55 56 61 62 63 68 69 74 79 84 89 94 99 100 101 106 111 116 117 118 119 124 125 130 135 140 145 146 147 152 157 158 159 164 165 166 167 172 177 178 183 188 189 194 199 200 201 206 211 212 217 222 223 224 229 230 235 236 241 242 243 244 245 250 251 256 257 258 259 260 261 266 267 268 269 270 275 276 277 278 283 288 293 298 299 304 
    ## score_2_arr =  0 0 1 2 3 3 3 3 3 4 4 4 4 4 5 6 6 7 8 8 9 9 9 9 9 9 9 10 11 11 11 11 12 13 14 14 15 15 15 15 15 16 17 17 17 18 19 19 20 21 22 22 22 23 23 23 24 24 24 25 26 26 26 27 27 27 28 29 29 30 30 31 31 32 33 34 35 35 36 36 37 38 39 40 41 41 42 43 44 45 45 46 47 48 48 48 48 48 49 49

    ## [1] "inside evaluate"
    ## score_1_arr =  5 6 11 12 13 14 15 16 21 22 23 24 25 26 27 32 37 42 47 52 57 62 63 68 69 74 79 84 85 86 91 92 97 102 103 108 113 118 123 124 125 130 135 140 145 150 155 156 157 162 167 172 177 182 183 188 193 198 199 200 205 206 211 216 221 222 223 228 233 238 239 244 245 246 247 252 253 254 255 256 257 258 259 264 269 270 275 276 281 282 283 284 289 290 291 292 297 298 299 304 
    ## score_2_arr =  0 1 1 2 3 4 5 6 6 7 8 9 10 11 12 12 12 12 12 12 12 12 13 13 14 14 14 14 15 16 16 17 17 17 18 18 18 18 18 19 20 20 20 20 20 20 20 21 22 22 22 22 22 22 23 23 23 23 24 25 25 26 26 26 26 27 28 28 28 28 29 29 30 31 32 32 33 34 35 36 37 38 39 39 39 40 40 41 41 42 43 44 44 45 46 47 47 48 49 49

    ## [1] "inside evaluate"
    ## score_1_arr =  5 6 7 8 9 10 15 16 21 22 27 28 33 34 39 44 49 54 59 64 69 74 75 80 81 82 83 84 85 90 95 100 105 106 107 108 113 118 123 128 133 134 139 144 149 150 151 152 153 158 163 164 165 170 175 180 185 186 187 192 197 202 203 204 209 210 215 216 221 226 231 236 241 242 247 248 249 254 259 260 265 270 275 276 281 282 287 292 293 294 299 300 305 306 311 312 313 314 319 320 
    ## score_2_arr =  0 1 2 3 4 5 5 6 6 7 7 8 8 9 9 9 9 9 9 9 9 9 10 10 11 12 13 14 15 15 15 15 15 16 17 18 18 18 18 18 18 19 19 19 19 20 21 22 23 23 23 24 25 25 25 25 25 26 27 27 27 27 28 29 29 30 30 31 31 31 31 31 31 32 32 33 34 34 34 35 35 35 35 36 36 37 37 37 38 39 39 40 40 41 41 42 43 44 44 45

    ## [1] "inside evaluate"
    ## score_1_arr =  1 6 11 12 13 18 19 24 25 26 27 28 29 34 39 44 49 50 55 56 61 62 67 68 73 78 79 84 85 86 87 88 89 90 95 100 101 106 111 116 117 118 123 128 129 130 131 132 133 134 135 136 137 142 143 148 153 154 159 160 161 166 171 172 177 178 179 184 189 194 199 204 209 214 215 216 221 222 223 228 233 234 235 236 237 238 239 244 249 250 255 260 265 266 267 272 277 282 287 292 
    ## score_2_arr =  1 1 1 2 3 3 4 4 5 6 7 8 9 9 9 9 9 10 10 11 11 12 12 13 13 13 14 14 15 16 17 18 19 20 20 20 21 21 21 21 22 23 23 23 24 25 26 27 28 29 30 31 32 32 33 33 33 34 34 35 36 36 36 37 37 38 39 39 39 39 39 39 39 39 40 41 41 42 43 43 43 44 45 46 47 48 49 49 49 50 50 50 50 51 52 52 52 52 52 52

    ad_final_score <- temp[1]
    rs_final_score <- temp[2]

**Results** ![A graph showing two lines plots for always defect score
and random score against
iterations](./plots/Always%20Defect%20vs%20Random.png)

### Comparison

    cat("tft_final_score = ", tft_final_score, "\n")

    ## tft_final_score =  893

    cat("gt_final_score = ", gt_final_score, "\n")

    ## gt_final_score =  1019

    cat("ac_final_score = ", ac_final_score, "\n")

    ## ac_final_score =  744

    cat("ad_final_score = ", ad_final_score, "\n")

    ## ad_final_score =  1000

    cat("rs_final_score = ", rs_final_score, "\n")

    ## rs_final_score =  700

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

    dev.off()

    ## pdf 
    ##   2

**Results** ![A lollipop chart showing each strategy’s total
point](comparison.png)

<br> <br> <br> <br> <br>
