simplex <- function(tableau) {
# Assumptions: Formula to max/min is on the last row
# Second to the last column is Z
# Last column is the solution
# All nonzero numbers on the last row before Z are the number of unknowns

    constraintsCount <- (nrow(tableau) - 1)
    unknownsCount <- 0
    bottomRow <- tableau[(nrow(tableau)), ]
    answers <- c()
    bottomHasNegative <- (find_min(bottomRow) < 0)

    # Find the number of unknowns using the bottom row
    for(i in bottomRow) {
        if(i == 0) {
            break
        }
        unknownsCount <- unknownsCount + 1
    }

    answers <- rep(0, unknownsCount)

    print("Tableau:")
    print(tableau)
    print(paste("constraintsCount: ", constraintsCount))
    print(paste("unknownsCount: ", unknownsCount))
    print("answers:")
    print(answers)
    print(bottomHasNegative)

    # Eliminate all negative numbers in the
    while(bottomHasNegative) {
        # Find the index of the minimum negative number in the bottom row
        pivot_col_number <- find_min_index(bottomRow)

        # Get array of the ratio (solution/tableau[, i])
        # Do not include the last element
        ratio <- head(tableau[, ncol(tableau)] / tableau[, pivot_col_number], -1)
        pivot_row_number <- find_min_index(ratio)

        print(ratio)
        print(pivot_row_number)

        break
        bottomHasNegative <- (find_min(bottomRow) < 0)
    }
}

find_min <- function(array) {
# Return the index of the smallest number in the array
    min = array[1]

    for(i in array) {
        if(i < min) {
            min = i
        }
    }

    return(min)
}

find_min_index <- function(array) {
# Return the index of the smallest number in the array
    min = array[1]
    min_index = 1
    current_index = 1

    for(i in array) {
        if(i < min) {
            min = i
            min_index <- current_index
        }

        current_index <- current_index + 1
    }

    return(min_index)
}

tableau <- matrix(c(
    7, 11, 1, 0, 0, 0, 0, 77,
    10, 8, 0, 1, 0, 0, 0, 80,
    1, 0, 0, 0, 1, 0, 0, 9,
    0, 1, 0, 0, 0, 1, 0, 6,
    -150, -175, 0, 0, 0, 0, 1, 0
), byrow=T, nrow=5)

S <- function() {
    return(simplex(tableau))
}