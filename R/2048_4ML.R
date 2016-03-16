
############################################################################## #
# Name: Game "2048" adapted for machine learning tests ('2048_4ML.R')
# Description: Port of "2048.c" to R with added 'machine learning' API
# Ported from C to R by cloudcell
# 
# Author of the original implementation of the game in C: Maurits van der Schee
# Original C Source: https://github.com/mevdschee/2048.c/blob/master/2048.c
############################################################################## #

# Possible Roadmap: 
# -> add tests
# -> make into a package (someday/maybe)
# -> attach the original C code directly to R (for speed)
# -> add 'keypress' (and 'crayon') support for instances run in terminals (S/M)
# -> optionally make a graphical clickable interface (S/M)
# 
# TODO: add tests

# Interactive Mode Issues/Solutions with Various Packages/Approaches:
#   require(keypress) # not everywhere supported
#   require(tcltk) # does not get focus automatically
#   possible solution -- a graph with point & click detection

require(bitops)

# order: by column!
# p.env$board <- matrix(data=c(1,  0, 1, 1,
#                              1,  0, 2, 0,
#                              0,  2, 2, 1,
#                              2,  0, 1, 3), nrow=4)

rotateCW.m <- function(x) t(apply(x, 2, rev))
rotateCCW.m <- function(x) apply(t(x), 2, rev)

#' find a target cell to collapse another cell with
findTarget <- function(array, x, stop) 
{
    #// if the position is already on the first, don't evaluate
    if (x==1) {
        return (x)
    }
    
    for(t in (x-1):1) {
        if (array[t]!=0) {
            if (array[t]!=array[x]) {
                # // merge is not possible, take next position
                return (t+1)
            }
            return (t)
        } else {
            # // we should not slide further, return this one
            if (t==stop) {
                return (t)
            }
        }
    }
    # // we did not find a
    return (x)
}

#' @param f.env test 'process' environment
# slideArray <- function(array, row, p.env) {
slideArray <- function(row, p.env, verbose=FALSE) 
{
    array <- p.env$board[row,]
    score <- p.env$score
    success = FALSE
    x <- t <- stop <- 1
    if(verbose) message("array_in: ")
    if(verbose) print(array)
    for (x in 1:length(array)) {
        if (array[x]!=0) {
            t = findTarget(array,x,stop)
            # // if target is not original position, then move or merge
            if (t!=x) {
                # // if target is zero, this is a move
                if (array[t]==0) {
                    array[t]=array[x]
                } else if (array[t]==array[x]) {
                    # // merge (increase power of two)
                    array[t] <- array[t] + 1
                    # // increase score
                    # score+=1<<array[t]
                    if(verbose) message("score_before: ", score)
                    score <- score + (bitShiftL(1, array[t]))
                    if(verbose) message("score_after: ", score)
                    # // set stop to avoid double merge
                    stop = t+1
                }
                array[x]=0
                success = TRUE
            }
        }
    }
    if(verbose) message("array_out: ")
    if(verbose) print(array)
    p.env$board[row,] <- array
    p.env$score <- score
    return (success)
}

moveLeft <- function(p.env, verbose=FALSE) 
{
    # board <- get(x="board", envir = f.env)
    board <- p.env$board
    if(verbose) message("board_in: ")
    if(verbose) print(p.env$board)
    if(verbose) message("===========")
    
    success = FALSE
    # x
    for (x in 1:p.env$SIZE) {
        if(verbose) message("board[x,]")
        if(verbose) message(board[x,])
        if(verbose) message("---------")
        # success <- bitOr(success, slideArray(board[x,]), p.env)
        success <- bitOr(success, slideArray(row=x, p.env))
    }
    if(verbose) message("board_out: ")
    if(verbose) print(p.env$board)
    if(verbose) message("===========")
    
    return (success)
}

rotateBoard <- function(p.env)
{
    p.env$board <- rotateCW.m(p.env$board)
}

moveDown <- function(p.env) 
{
    rotateBoard(p.env)
    success = moveLeft(p.env)
    rotateBoard(p.env)
    rotateBoard(p.env)
    rotateBoard(p.env)
    return (success)
}

moveRight <- function(p.env) 
{
    rotateBoard(p.env)
    rotateBoard(p.env)
    success = moveLeft(p.env)
    rotateBoard(p.env)
    rotateBoard(p.env)
    return (success)
}

moveUp <- function(p.env) 
{
    rotateBoard(p.env)
    rotateBoard(p.env)
    rotateBoard(p.env)
    success = moveLeft(p.env)
    rotateBoard(p.env)
    return (success)
}

drawBoard <- function(p.env, simple=FALSE) 
{
    board <- p.env$board
    SIZE <- p.env$SIZE
    
    message("-----------------------------------------------")
    message("score: ", p.env$score)
    # print(board,    row.names=F, col.names=F, quote=F)
    if(simple) {
        write.table(board,    row.names=F, col.names=F, quote=F)
    } else {
        for (x in 1:SIZE) {
            for (y in 1:SIZE) {
                cat("       ")
            }
            cat('\n')
            
            for (y in 1:SIZE) {
                if (board[x,y]!=0) {
                    val <- as.character( bitShiftL(1, board[x,y]) )
                    txt_out <- format(x=val, justify="centre", width = 7)
                    cat( txt_out )
                } else {
                    cat("   ·   ")
                }
            }
            cat("\n")
        }
    }
    
    message()
    # cat("       { < > v ^ q r } + enter     \n")
}

#' counts empty cells
countEmpty <- function(p.env) 
{
    board <- p.env$board
    count <- 0
    for (x in 1:p.env$SIZE) {
        for (y in 1:p.env$SIZE) {
            if (board[x,y]==0) {
                count <- count + 1
            }
        }
    }
    return (count)
}

#' checks whether empty columns or collapsible cells exist
gameEnded <- function(p.env) 
{
    board=p.env$board
    ended <- TRUE
    if (countEmpty(p.env)>0) return(FALSE)
    if (findPairDown(p.env)) return(FALSE)
    rotateBoard(p.env)
    if (findPairDown(p.env)) ended = FALSE
    
    rotateBoard(p.env)
    rotateBoard(p.env)
    rotateBoard(p.env)
    return(ended)
}

addRandom <- function(p.env) 
{
    board <- p.env$board
    SIZE <- p.env$SIZE
    r <- 0
    len <- 0
    # empty cells
    blanks <- list()
    
    if (!p.env$initialized) {
        # set.seed(0)
        set.seed(as.numeric(Sys.time()))
        p.env$initialized = TRUE
    }
    
    # record coordinates of empty cells
    for (x in 1:SIZE) {
        for (y in 1:SIZE) {
            if (board[x,y]==0) {
                len <- len + 1
                blanks[[len]] <- c(x=x,y=y)
            }
        }
    }
    
    # rand() 
    # The C library function int rand(void) returns a pseudo-random number in
    # the range of 0 to RAND_MAX. RAND_MAX is a constant whose default value may
    # vary between implementations but it is granted to be at least 32767.
    
    if (len>0) {
        # r = rand()%len
        sample_range <- 1:len
        # r <- round( sample(sample_range, 1), digits = 0)
        r <- sample(sample_range, 1)
        x = blanks[[r]][1]
        y = blanks[[r]][2]
        
        # n = (rand()%10)/9+1
        sample_range <- 0:9
        n <- round( sample(sample_range, 1)/9+1, digits = 0)
        board[x,y]=n
        
        # update only if necessary
        p.env$board <- board
    }
}

#' checks whether 'collapsible' pairs of cells exist
#' in one direction (rotation is used to check for both rows and columns)
findPairDown <- function(p.env) 
{
    board <- p.env$board
    success <- FALSE
    for (x in 1:p.env$SIZE) {
        for (y in 1:(p.env$SIZE-1)) {
            if (board[x,y]==board[x,y+1]) return(TRUE)
        }
    }
    return(success)
}

#' init board & internal vars
#' @param p.env process environment ("p" for "process", create externally)
initBoard <- function(p.env) 
{
    # RNG initialization state (TRUE/FALSE)
    p.env$initialized <- FALSE # moved out of 'addRandom()' function
    
    p.env$score <- 0
    p.env$game_over <- FALSE # game end flag
    p.env$illegal_mov_nbr <- 0 # number of illegal moves made
    
    vect <- rep(0,length.out=p.env$SIZE^2)
    p.env$board <- matrix(data=vect, nrow = p.env$SIZE)
    
    addRandom(p.env)
    addRandom(p.env)
    
    drawBoard(p.env)
}

#' launch in interactive mode
#' @param p.env process environment (create externally)
main_interactive <- function(p.env, board_size=4) 
{
    # TODO: bring those 'globals' (above) in here or into 'initBoard()'
    p.env$SIZE <- board_size
    
    initBoard(p.env)
    
    while(TRUE) {
        c <- readline(prompt="Enter {Ww|Aa|Ss|Dd} | {Hh|Ll|Kk|Jj} + Enter ")
        switch(c,
            "A"=, "a"=,
            "H"=, "h"={ 
                success = moveLeft(p.env)
            },
            "D"=, "d"=,
            "L"=, "l"={ 
                success = moveRight(p.env)
            },
            "W"=, "w"=,
            "K"=, "k"={ 
                success = moveUp(p.env)
            },
            "S"=, "s"=,
            "J"=, "j"={ 
                success = moveDown(p.env)
            },
            # default:
            {
                success = FALSE
            }
        )
        
        if (success) {
            # drawBoard(p.env)
            # drawing and pause removed intentionally from here (too verbose)
            addRandom(p.env)
            drawBoard(p.env)
            if (gameEnded(p.env)) {
                message("         GAME OVER          ")
                break
            }
        }
        if (c=='q' || c=='Q') {
            message("        QUIT? (y/n)         ");
            c <- readline(prompt="Enter {Yy|Nn} + Enter ")
            if (c=='y') {
                break
            }
            drawBoard(p.env);
        }
        if (c=='r') {
            message("       RESTART? (y/n)       \n");
            c <- readline(prompt="Enter {Yy|Nn} + Enter ")
            if (c=='y') {
                initBoard(p.env);
            }
            drawBoard(p.env);
        }
    }
    return(0)
}


#' step 1 of 2: init in ML benchmark mode
#' @param p.env process environment (create externally)
#' @param show_board -- to be implemented
#' @param verbose -- to be implemented
main_ML_init <- function(p.env, board_size=4, limit_ill_mov=3, verbose=FALSE)
{#' @author cloudcell
    p.env$SIZE <- board_size
    p.env$limit_ill_mov <- limit_ill_mov # max illegal moves to force game end
    
    initBoard(p.env)
    
}


#' step 2 of 2: run in ML benchmark mode
#' @param m direction of the move: {R|U|L|D}
#' @param limit_ill_mov limit illegal moves (moves to game end)
#' @param p.env process environment (create externally)
main_ML_run <- function(p.env, m=c("R","U","L","D"), show_board=TRUE)
{#' @author cloudcell
    m <- m[1]
    
    ################################################################## #        
    switch(m,
           "L"={ success = moveLeft(p.env)  },
           "R"={ success = moveRight(p.env) },
           "U"={ success = moveUp(p.env)    },
           "D"={ success = moveDown(p.env)  },
           {    
               warning("Illegal move: '", m, "'" )
               success = FALSE
           }
    )
    
    if (success) {
        # drawBoard(p.env)
        addRandom(p.env)
        
        if(show_board) drawBoard(p.env)
        
        if (gameEnded(p.env)) {
            message("GAME OVER")
            p.env$game_over <- TRUE
        }
    }
    
    if(success) {
        
        p.env$illegal_mov_nbr <- 0
        
    } else {
        
        p.env$illegal_mov_nbr <- p.env$illegal_mov_nbr + 1
        
        message("Illegal move: ",p.env$illegal_mov_nbr," out of ",
                p.env$limit_ill_mov, " (max)")
        
        if(p.env$illegal_mov_nbr > p.env$limit_ill_mov) {
            p.env$game_over <- TRUE
        }
    }

    ################################################################## #   

    result <- list(game_over=p.env$game_over, score=p.env$score)
    return(result)
}

#------------------------------------------------------------------------------#
# demo/test area
#------------------------------------------------------------------------------#
if(0){
    p.env <- new.env()
    main_interactive(p.env)
}

if(0){
    p.env <- new.env()
    main_ML_init(p.env)

    main_ML_run(p.env, m ="L" )
    # use p.env$board to retrieve board state
    p.env$board
    
    main_ML_run(p.env, m ="D", show_board = FALSE)
    # use p.env$board to retrieve board state
    p.env$board
}


#------------------------------------------------------------------------------#
# SANDBOX
#------------------------------------------------------------------------------#
# 
if(0) {
# sandbox
if(0) {
    has_keypress_support()
    x <- keypress()
    cat("You pressed key", x, "\n")
    
    # use graphics
    
    getGraphicsEvent(prompt = prompt, 
                     onMouseDown = NULL, onMouseMove = NULL,
                     onMouseUp = NULL, onKeybd = onKeybd,
                     consolePrompt = "[click on graph then follow top prompt to continue]")
    
}

# sandbox: interactive tcltk -- fails to set focus 
if(0) {
    # borrowed from here
    # http://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r
    waitKeyPress <- function() {
        tt <- tktoplevel()
        # tcl("wm", "attributes", tt, topmost=TRUE)
        tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
                side='bottom')
        tkbind(tt,'<Key>', function()tkdestroy(tt) )
        
        # tkraise(tt)
        tkfocus(tt)
        tkwait.window(tt)
        return("hello")
    }
    
    waitKeyPress()
}

# sandbox
if(0) {
    
    initial <- matrix(data=c(1,0,0,0,
                             2,0,0,0,
                             3,0,0,0,
                             4,0,0,0),nrow=4)
    board   <- matrix(data=c(2,  0, 2, 2,
                             2,  0, 4, 0,
                             0,  4, 4, 2,
                             4,  0, 2, 8),nrow=4)
    
    initial
    rotateCW.m(initial)
    rotateCCW.m(initial)
    initial <- rotateCCW.m(initial)
    initial
    
}

# sandbox
if(0) {
    initBoard(p.env)
    p.env$board
}

# sandbox
if(0){
    p.env$board
    addRandom(p.env)
    p.env$board
}

# sandbox
if(0) {
    p.env$board
    moveUp(p.env)
    p.env$board
    p.env$score
    
    moveRight(p.env)
    p.env$board
    
    moveLeft(p.env)
    p.env$board
    
    p.env$board
    moveDown(p.env)
    p.env$board
}


}

