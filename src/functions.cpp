#include "functions.h"

std::vector<double> calculateConnectivity(std::vector<std::size_t> &id1, std::vector<std::size_t> &id2, std::vector<double> &boundary, IntegerMatrix &selection) {
	std::vector<double> x(3,0.0);
	for (std::size_t i=0; i<id1.size(); ++i) {
		if (selection(0,id1[i]) + selection(0,id2[i]) == 2) {
			x[0]+=boundary[i];
		} else if (selection(0,id1[i]) + selection(0,id2[i]) == 0) {
			x[1]+=boundary[i];
		} else {
			x[2]+=boundary[i];
		}
	}
	return(x);
}
