H <- function(x) {
    return(-sum(x * log(x, base = 3)))
}


find_trust <- function(x) {
    return(0.3 * (1 - H(x)))
}


find_new_rep <- function(o, nr, omega = 0.98) {
    return(omega*o + omega*0.1**(omega*abs(o))*nr)
}


rep_fnr <- function(o, nr, reps, omega = 0.98) {
    for (i in 1:reps) {
        o <- find_new_rep(o, nr, omega)
    }
    return(o)
}
