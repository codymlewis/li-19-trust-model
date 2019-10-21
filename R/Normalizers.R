normalize <- function(context)
{
    normalizers <- c(
        normalize.time,
        normalize.capability,
        normalize.location,
        normalize.velocity
    )

    return (
        sapply(
            1:length(context),
            function(i) {
                normalizers[[i]](context[[i]])
            }
        )
    )
}


normalize.time <- function(time)
{
    return (time)
}


normalize.capability <- function(capability)
{
    return (1 - (capability / params$max.capability))
}


normalize.location <- function(distance)
{
    return (1 - (distance / sqrt(params$map.width**2 + params$map.height**2)))
}


normalize.velocity <- function(velocity)
{
    return (1 - (velocity / params$max.velocity))
}

