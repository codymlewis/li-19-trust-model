#ifndef MAP_h
#define MAP_h

#include <vector>
#include <memory>

#include "Tile.h"

namespace li19trustmodel
{
    class Map {
        private:
            std::unique_ptr<std::vector<std::vector<li19trustmodel::Tile>>> tiles;
        public:
            Map();
            std::vector<int> shape();
            Tile get_tile(std::vector<int> loc);
    };
}

#endif
