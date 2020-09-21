#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param
#'   no arguments are used by the function.
#'
#' @return
#'   The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#'   Select Door Function.
#'
#' @description
#'   `select_door()` is the function which simulates the user selecting a
#'   random door.
#'
#' @details
#'   The initial door selection simulates the player choosing one of three
#'   closed doors.  There are goats behind two of the doors and a car
#'   behind one of the doors.  At this point in the game all three doors are
#'   still closed so the player doesn't know if they picked the car door or
#'   a goat door.
#'
#' @param
#'   No arguments are used by the function.
#'
#' @return
#'   The function returns a length one character vector which is a number
#'   between 1 and 3 and represents the number of the initial door selected.
#'   The returned value is assigned to the variable `a.pick`.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open Goat Door Function.
#'
#' @description
#'   `open_goat_door` is the function which simulates the game show host
#'   opening one of the doors that is known to have a goat behind it.
#'
#' @details
#'   After the player makes their initial door selection, the host will oepn
#'   one of the goat doors.  At this point in the game there are still two
#'   closed doors; the one picked by the contestant and door #3.
#'
#' @param
#'   This function contains two arguments.  The host should never open the door
#'   picked by the contestant and should never open the car door.  To ensure
#'   this, the argument uses the 'not equal' operator. The door the host opens
#'   is not equal to `a.pick` and not equal to "car".
#'
#' @return
#'   This function returns a length one character vector which is a number
#'   between 1 and 3 and represents a known goat door. The returned value is
#'   assigned to variable `opened.door`.
#'
#' @examples
#'   open_goat_door
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change Door Function
#'
#' @description
#'   The `change_door` function is where the player makes the decision to
#'   stay with their initial choice, `a.pick`, or change to the other
#'   unopened door.  They do not know what is behind either of the remaining
#'   closed doors.
#'
#' @details
#'   if the player decides to stay, then the host will move forward with
#'   opening the door associated with `a.pick`. If the player decides to
#'   switch doors, the host will open the third door.  The third door is
#'   not `a.pick` or `opened.door`.  The final choice of the player is
#'   then assigned to variable `final.pick`.
#'
#' @param
#'   This function contains two basic arguments.  The first is that if the
#'   player decides to stay, then their initial choice `a.pick` becomes
#'   their `final.pick`. If the player decides to switch, then the host
#'   cannot select `opened.door` or `a.pick`.  To perform this, the function
#'   uses the argument that final.pick is not equal to `opened.door` or
#'   `a.pick`.
#'
#' @return
#'   This function returns a length one character vector that is a number
#'   between 1 and 3.  That number is assigned to variable `final.pick`.
#'
#' @examples
#'   change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine Winner Function
#'
#' @description
#'   The function `determine_winner()`, determines whether the player's final
#'   pick is a goat door or a car door.
#'
#' @details
#'   If the player's `final.pick` is equal to "car" then they win.  If their
#'   `final.pick` is equal to "goat" then they lose.
#'
#' @param
#'   This function contains two arguments as described in the details. If
#'   `final.pick` equals "car", then the player wins. If `final.pick`
#'   equals "goat", thenn the player loses.
#'
#' @return
#'   This function returns a length one logical character vector. The choices
#'   are "WIN" or "LOSE".
#'
#' @examples
#'   determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#'   Play Game Function
#'
#' @description
#'   The `play_game()` function combines all the steps described above
#'   into a single function to play one round of the game.
#'
#' @details
#'   This function allows the user to simulate game play through the use
#'   of a single function as described in the parameters below. Each
#'   simulation is the equivalent of one round of the game.
#'
#' @param
#'   To start play_game() the variable `new.game` is created using the function
#'   create_game(). The next variable created is `first.pick`. `first.pick`
#'   uses the function select_door() and simulates the initial door choice
#'   of the player. After the initial choice is made, the variable
#'   `opened.door` is introduced using the function open_goat_door(). The
#'   variables `new.game` and `first.pick` are passed through the function
#'   open_goat_door() to ensure that the door opened by this function is
#'   not equal to the door chosen by the contestant and not equal to a car
#'   door. The next variables in this function are `final.pick.stay` and
#'   `final.pick.switch`. These variables represent the player's choice
#'   of staying with their initial choice `first.pick` or switching to
#'   the third door. In `final.pick.stay` the player chooses not to switch
#'   and in `final.pick.stay`, the player chooses to switch. This decision
#'   is then passed through the next variable `strategy`. The next variable
#'   is `outcome`. The play_game() function compares outcomes using both
#'   strategies, "stay" and "switch".  This allows the function to decide
#'   which strategy is more effective at winning. The variable `game.results`
#'   is the final variable determined by the play_game() function.
#'
#' @return
#'   The variable `game.results` is a data frame returned by the function
#'   play_game(). The results are a single character vector that indicate
#'   "WIN" or "LOSE".
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play N Games
#'
#' @description
#'   The play_n_games() function allows the user to simulate any number of
#'   games in a single function. The user must only specify how many
#'   iterations of the Monty Hall Game they want the simulation to run.
#'
#' @details
#'   The play_n-game() function uses a loop simulation to run a user defined
#'   number of game iterations and returns the quantitative results. For
#'   example, if the user wishes to simulate 10,000 iterations of the game,
#'   they simply only need to change the number n to equal 10,000.
#'
#' @param
#'   This function relies on multiple arguments to work effectively.  Refore,
#'   running this function, the user must ensure they have installed the
#'   package dplyr.  The function works by passing the function play_game()
#'   through an iterator where n equals the number of times the game is played.
#'   The variable `results.list` then compiles the results of `game.outcome`
#'   for the appropriate number of loops identified in the variable
#'   `loop.count`. These results are identified as a table of the results
#'
#' @return
#'   results.df
#'
#' @examples
#'   play_n_games
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}
