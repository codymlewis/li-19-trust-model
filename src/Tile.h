#ifndef TILE_h
#define TILE_h

#include <memory>
#include <vector>
#include <Rcpp.h>

#include "Device.h"

namespace li19trustmodel
{
    class Tile {
        private:
            std::vector<Device> devices;
            int terrain;
        public:
            Tile();
            Tile(int new_terrain);
            // void add_device(int id, Rcpp::refClass device);
    };
}
#endif
