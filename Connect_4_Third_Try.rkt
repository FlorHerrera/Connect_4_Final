;Flor Herrera, Program 13 (Connect 4 Player Bot)
;Able to play a game of Connect 4 

;global variable to store current state of the game
(define FIHGame '())

;begin a new game
(define (FIHStartGame)
     (begin
                ;6 rows, each represented by a list
                (set! FIHGame '("X" ( "-" "-" "-" "-" "-" "-" "-") 
                                    ( "-" "-" "-" "-" "-" "-" "-")
                                    ( "-" "-" "-" "-" "-" "-" "-")
                                    ( "-" "-" "-" "-" "-" "-" "-")
                                    ( "-" "-" "-" "-" "-" "-" "-")
                                    ( "-" "-" "-" "-" "-" "-" "-"))
                 )
                (display "Let's get to connecting the dots!\n")
                #t
                
     )
  )


  
;make a move specified by the player given column number
(define (FIHMarkMove Col)
  (begin
        (set! FIHGame
              (cons (FIHOppositeTurn)
                    (FIHsetCell (cdr FIHGame) (FIHNextFreeRow 1 Col) Col (car FIHGame) )
              )
        )
    Col
      )
)

;determine which is the next available row
(define (FIHNextFreeRow Row Col)
  
      (if (string=? (FIHgetCell (cdr FIHGame) Row Col) "-")
          Row
          (FIHNextFreeRow (+ Row 1) Col)
       )
)

;change whose turn it is
(define (FIHOppositeTurn)
  (if (string=? (car FIHGame) "X")
      "O"
      "X"
  )
)
  
;change whose turn it is
;(define (FIHChangeTurn)
;  (if (car
;displays simple graphical display of current game grid
(define (FIHShowGame)
  (begin
    (display " --------------")
    (newline)
    (display (car (cdr (cdr (cdr (cdr (cdr (cdr FIHGame))))))))
    (newline)
    (display (car (cdr (cdr (cdr (cdr (cdr FIHGame)))))))
    (newline)
    (display (car (cdr (cdr (cdr (cdr FIHGame))))))
    (newline)
    (display (car (cdr (cdr (cdr FIHGame)))))
    (newline)
    (display (car (cdr (cdr FIHGame))))
    (newline)
    (display (car (cdr FIHGame)))
    (newline)
    (display " --------------")
    (newline)

    #t
  )
)

;ask the program to make a move, returns the chosen column for the next move
(define (FIHMakeMove)
  (FIHMarkMove (FIHChooseMove 1))
)

;choose the next move
(define (FIHChooseMove Col)
  (if (FIHLegalMoveP (FIHWinningColumn Col))
      (FIHWinningColumn Col)
      (if (FIHLegalMoveP (FIHAvailBlock Col))
          (FIHAvailBlock Col)
          (FIHChooseRandomLegalMove (+ 1 (random 7)))
      )
   )
)

;choose random legal move on board
(define (FIHChooseRandomLegalMove Col)
  (if (FIHLegalMoveP Col)
      Col
      (FIHChooseRandomLegalMove  (+ 1 (random 7)))
  )
)

;return the column on an available win, or zero
(define (FIHWinningColumn Col)
  (if (FIHLegalMoveP Col)      
      (if (FIHWillWinP Col)
          Col
          (FIHWinningColumn (+ Col 1))
      )
      0
  )
)
                           
;return the column on an available block, or zero
(define (FIHAvailBlock Col)
  (if (FIHLegalMoveP Col)      
      (if (FIHWillOtherWin Col)
          Col
          (FIHAvailBlock (+ Col 1))
      )
      0
  )
)     
  

;test a given move (column) to see if it's legal w/ respect to current game state
(define (FIHLegalMoveP col)
  (if (or (< col 1)
          (> col 7)
      )
      #f
      (string=? "-" (FIHgetCell (cdr FIHGame) 6 col))
  )
)

;returns the item at the given grid location is a list of lists
(define (FIHgetCell Matrix Row Column )
     (FIHgetColumn (FIHgetRow Matrix Row) Column)
)

;returns the specified row (as a list)
(define (FIHgetRow Matrix rowNumber)
     (if (= rowNumber 1)
          (car Matrix)
          (FIHgetRow (cdr Matrix) (- rowNumber 1))
     )
)

;returns the item in specified column (given the row as a list)
(define (FIHgetColumn List columnNumber)
     (if (= columnNumber 1)
          (car List)
          (FIHgetColumn (cdr List) (- columnNumber 1))
     )
)

;return a new matrix with the given grid location reset to the given value 'Item'
(define (FIHsetCell Matrix Row Column Item )
     (if (= Row 1)
          (cons (FIHsetColumn (car Matrix) Column Item) (cdr Matrix))
          (cons (car Matrix) (FIHsetCell (cdr Matrix) (- Row 1) Column Item))
     )
)

;return a row (List) with the new Item in the desired column position
(define (FIHsetColumn List columnNumber Item)
     (if (= columnNumber 1)
          (cons Item (cdr List))
          (cons (car List) (FIHsetColumn (cdr List) (- columnNumber 1) Item))
     )
)


;test the current game grid to see if the last move resulted in a win
     ;accept one argument that represents the column of the latest move
(define (FIHWinP col)
  (FIHWinPX (cdr FIHGame) col (FIHOppositeTurn))
)

;test the current game grid to see if the given move will result in a win
(define (FIHWillWinP Col)
  (FIHWinPX (FIHsetCell (cdr FIHGame) (FIHNextFreeRow 1 Col) Col (car FIHGame))
            Col
            (car FIHGame))
)

;test the current game grid to see if there is a block available                          
(define (FIHWillOtherWin Col)
  (FIHWinPX (FIHsetCell (cdr FIHGame) (FIHNextFreeRow 1 Col) Col (FIHOppositeTurn))
            Col
            (FIHOppositeTurn))
)

;Helper function that tests any game board for a win (not just current state)
(define (FIHWinPX Matrix col Symbol)
  (or
   (FIHVerticalWin Matrix col Symbol)
   (FIHHorizontalWin Matrix col Symbol)
   (FIHRightDiagonalWin Matrix col Symbol)
   (FIHLeftDiagonalWin Matrix col Symbol)
  )
)

;check for a vertical win
(define (FIHVerticalWin Matrix Col Symbol)
  
  (if (> (FIHConsecutiveVertical
                Matrix
               (FIHGetTopRow Matrix Col)
                Col
               Symbol)       
          2)
      #t
      #f
   )
)

;helper for vertical win checker, returns number of times a matching
  ;symbol is found
(define (FIHConsecutiveVertical Matrix Row Col Symbol)
  (if (FIHValidCell (- Row 1) Col)
      (if (string=? Symbol (FIHgetCell Matrix (- Row 1) Col))
          (+ (FIHConsecutiveVertical Matrix (- Row 1) Col Symbol)
             1
          )
          0
      )
      0
   )
)

;check for a horizontal win
(define (FIHHorizontalWin Matrix Col Symbol)
  (> (FIHHorizontalWinX
         Matrix
        (FIHGetTopRow Matrix Col) 
         1
        Symbol
        0
      )
     3)
)
  
   
;helper for horizontal win function
(define (FIHHorizontalWinX Matrix Row Col Symbol Count)
  (if (> Count 3)
      Count
      (if (FIHValidCell Row Col)
          (if (string=? Symbol (FIHgetCell Matrix Row Col))
              (FIHHorizontalWinX Matrix Row (+ Col 1) Symbol (+ Count 1))
              (FIHHorizontalWinX Matrix Row (+ Col 1) Symbol 0)
          )
          Count
      )     
  )
)

;get row number for most recent chip in given column
(define (FIHGetTopRow  Matrix Col)
  (FIHGetTopRowX Matrix 6 Col)
)
      
;helper for FIHGetTopRow, ensures user doesn't have to
       ;input parameter 6 for the row manually to start
       ;the search at the top row
(define (FIHGetTopRowX Matrix Row Col)
  (if (string=? "-" (FIHgetCell Matrix Row Col))
      (if (FIHValidCell (- Row 1) Col)
          (FIHGetTopRowX Matrix (- Row 1) Col)
          Row
      )
      Row
  )
)

;find the starting point when checking the left diagonal
(define (FIHStartPtLeftDiagonal Row Col)
  (if (FIHValidCell (+ Row 1) (- Col 1))
      (FIHStartPtLeftDiagonal (+ Row 1) (- Col 1))
      (cons Row Col)
  )
)

;returns true or false depending on if there is a win in the left diagonal
(define (FIHLeftDiagonalWin Matrix Col Symbol)
  (>
   (FIHLeftDiagonalWinX
         Matrix
         (car (FIHStartPtLeftDiagonal (FIHGetTopRow Matrix Col) Col))
         (cdr (FIHStartPtLeftDiagonal (FIHGetTopRow Matrix Col) Col))
         0
         Symbol)
   
   3
  )
)

;helper function for FIHLeftDiagonalWin
(define (FIHLeftDiagonalWinX Matrix Row Col Count Symbol)
  (if (> Count 3)
      Count
      (if (FIHValidCell Row Col)
          (if (string=? Symbol (FIHgetCell Matrix Row Col))
              (FIHLeftDiagonalWinX Matrix (- Row 1) (+ Col 1) (+ Count 1) Symbol)
              (FIHLeftDiagonalWinX Matrix (- Row 1) (+ Col 1) 0 Symbol)
          )
          Count
      )
  )
)

;find the starting point when checking the left diagonal
(define (FIHStartPtRightDiagonal Row Col)
  (if (FIHValidCell (+ Row 1) (+ Col 1))
      (FIHStartPtRightDiagonal (+ Row 1) (+ Col 1))
      (cons Row Col)
  )
)

;returns true or false depending on if there is a win in the left diagonal
(define (FIHRightDiagonalWin Matrix Col Symbol)
  (>
   (FIHRightDiagonalWinX
          Matrix
         (car (FIHStartPtRightDiagonal (FIHGetTopRow Matrix Col) Col))
         (cdr (FIHStartPtRightDiagonal (FIHGetTopRow Matrix Col) Col))
         0
         Symbol)
   
   3
  )
)

;helper function for FIHLeftDiagonalWin
(define (FIHRightDiagonalWinX Matrix Row Col Count Symbol)
  (if (> Count 3)
      Count
      (if (FIHValidCell Row Col)
          (if (string=? Symbol (FIHgetCell Matrix Row Col))
              (FIHRightDiagonalWinX Matrix (- Row 1) (- Col 1) (+ Count 1) Symbol)
              (FIHRightDiagonalWinX Matrix (- Row 1) (- Col 1) 0 Symbol)
          )
          Count
      )
  )
)
;check if proposed grid spot is valid (in bounds)
(define (FIHValidCell Row Col)
  (if (or (< Col 1)
          (> Col 7)
      )
      #f
      (if (or (< Row 1)
              (> Row 6)
          )
          #f
          #t
       )
   )
)
    
(FIHStartGame)
(FIHShowGame)