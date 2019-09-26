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
    return (time / Params$time.now)
}


normalize.capability <- function(capability)
{
    return (1 - (capability / Params$max.capability))
}


normalize.location <- function(location)
{
    return (1 - (location / sqrt(Params$map.width**2 + Params$map.height**2)))
}


normalize.velocity <- function(velocity)
{
    return (1 - (velocity / Params$max.velocity))
}

