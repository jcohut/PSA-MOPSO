# operates on given matrix
# m - number of partitions
# returns psaSets
psa <- function(A, m) {
    calc_diam <- function(A) {
        if (class(A)=="numeric") { # A has only one row
            # aj == bj
            # to avoid coincidence with zero-valued differences
            return(-Inf)
        }
        aj <- apply(A, 2, min)
        bj <- apply(A, 2, max)
        max(bj - aj)
    }
    calc_coord <- function(A) {
        if (class(A)=="numeric") { # A has only one row
            # aj == bj
            return(0)
        }
        aj <- apply(A, 2, min)
        bj <- apply(A, 2, max)
        which.max(bj - aj)
    }

    sets <- list(); diams <- c()
    sets[[1]] = 1:dim(A)[1] # initial set is the whole matrix
    diams[1] = calc_diam(A[sets[[1]],])
    while(length(sets) < m) {
        j <- which.max(diams)
        if(length(sets[[j]]) > 1) {
            pj <- calc_coord(A[sets[[j]],])
            aj <- min(A[sets[[j]],pj])
            j1 <- c(); j2 <- c()
            for(k in 1:length(sets[[j]])) {
                l <- sets[[j]][k]
                if (A[l,pj] <= aj + diams[j]/2) {
                  j1 = c(l, j1)
                } else {
                  j2 = c(l, j2)
                }
            }
            sets[[j]] = NULL
            diams = diams[-j]
            if (length(j1) > 0) {
                sets[[length(diams)+1]] = j1
                diams[length(diams)+1] = calc_diam(A[j1,])
            }
            if (length(j2) > 0) {
                sets[[length(diams)+1]] = j2
                diams[length(diams)+1] = calc_diam(A[j2,])
            }
        }
    }
    sets
}
