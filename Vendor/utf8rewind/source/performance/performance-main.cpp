#include "performance-base.hpp"

int main(int argc, char** argv)
{
	std::cout << "utf8rewind: " << UTF8_VERSION_STRING << std::endl;

	return PERF_RUN_ALL(argc, argv);
}