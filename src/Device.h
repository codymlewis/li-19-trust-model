#ifndef DEVICE_h
#define DEVICE_h

#include <vector>
#include <Rcpp.h>

namespace li19trustmodel
{
    class Device {
        private:
            int id;
            std::vector<int> contacts;
            std::vector<int> location;
            std::vector<int> current_goal;
            int cur_time;
            int capability;
            double velocity;
            int trust;
            int distrust;
            int unknown;
            int domain;
        public:
            Device();
            // Device(int id, int no_contacts=200);
            //
            // void trust_inc();
            // void distrust_inc();
            // void unknown_inc();
            //
            // void move(int time_change);
            // void communicate(std::vector<int> dev_locs);
    };
}

#endif
