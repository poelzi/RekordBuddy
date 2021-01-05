#include "tests-base.hpp"

int main(int argc, char** argv)
{
	::testing::InitGoogleTest(&argc, argv);

	std::cout << "utf8rewind: " << UTF8_VERSION_STRING << std::endl;

	int result = RUN_ALL_TESTS();

	if (result != 0)
	{
		std::cout << std::endl;
		std::cout << "Press any key to continue.";

		std::cin.get();
	}

	return result;
}