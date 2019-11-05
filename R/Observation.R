Observation <- setRefClass(
  "Observation",
  fields = list(
    context = "numeric",
    trust = "numeric",
    id.sender = "numeric"
  ),

  methods = list(
    initialize = function(context, trust, id.sender) {
      context <<- context
      trust <<- trust
      id.sender <<- id.sender
    }
  )
)
