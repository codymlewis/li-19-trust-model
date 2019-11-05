Tile <- setRefClass(
  "Tile",
  fields = list(
    objects = "list",
    signals = "list",
    terrain = "numeric",
    base.station = "list",
    signal.edge = "logical"
  ),

  methods = list(
    initialize = function(terrain) {
      objects <<- list()
      signals <<- list()
      terrain <<- terrain
      base.station <<- list()
      signal.edge <<- FALSE
    },

    add.device = function(device) {
      "Add a device here"
      objects[[device$id]] <<- device
    },

    add.base.station = function(base.station) {
      "Add the base station here"
      base.station[[1]] <<- base.station
    },

    get.base.station = function() {
      "Get the base station from here"
      return(base.station[[1]])
    },

    rm.device = function(id) {
      "Remove a device from here"
      objects[[id]] <<- 0
    },

    has.devices = function() {
      "TRUE if there are devices on this tile, else FALSE"
      for (object in objects) {
        if (!is.numeric(object) && !is.null(object)) {
          return(TRUE)
        }
      }
      return(FALSE)
    },

    add.signal = function(base.station, is.edge) {
      "Add a signal from a base station here"
      signals[[length(signals) + 1]] <<- base.station
      if (is.edge) {
        signal.edge <<- TRUE
      }
    }
  )
)
