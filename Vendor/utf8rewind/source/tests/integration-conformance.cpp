#include "tests-base.hpp"

class ConformanceSuite
	: public ::testing::Test
{

protected:

	void SetUp()
	{
		errors = UTF8_ERR_NONE;
		input = nullptr;
		input_size = 0;
		output = nullptr;

		// load file into memory

		file.open("testdata/UTF-8-test.txt", std::ios_base::in);
		ASSERT_TRUE(file.is_open());

		file.seekg(0, std::ios::end);
		input_size = (size_t)file.tellg();
		file.seekg(0, std::ios::beg);

		ASSERT_EQ(20337, input_size);

		input = new char[input_size];
		memset(input, 0, input_size);
		file.read(input, input_size);

		file.close();

		size_t decoded_size = 0;

		decoded_size = utf8toutf32(input, input_size, nullptr, 0, &errors);
		ASSERT_EQ(80608, decoded_size);
		ASSERT_EQ(0, errors);

		output = new unicode_t[decoded_size + 1];

		utf8toutf32(input, input_size, output, decoded_size, &errors);
		ASSERT_EQ(0, errors);

		output[decoded_size] = 0;
	}

	void TearDown()
	{
		if (input != nullptr)
		{
			delete [] input;
		}

		if (output != nullptr)
		{
			delete [] output;
		}
	}

	char* input;
	size_t input_size;
	unicode_t* output;
	size_t output_size;
	std::fstream file;
	int32_t errors;

};

TEST_F(ConformanceSuite, ReadTheEnd)
{
	unicode_t* the_end = output + 20072;
	EXPECT_EQ('T', the_end[0]);
	EXPECT_EQ('H', the_end[1]);
	EXPECT_EQ('E', the_end[2]);
	EXPECT_EQ(' ', the_end[3]);
	EXPECT_EQ('E', the_end[4]);
	EXPECT_EQ('N', the_end[5]);
	EXPECT_EQ('D', the_end[6]);
}

TEST_F(ConformanceSuite, EveryLineIs79Codepoints)
{
#define CHECK_LINE(_start, _endCharacter) \
	const char* line_at_ ## _start = utf8seek(input + _start, strlen(input + _start), input + _start, 78, SEEK_CUR); \
	EXPECT_EQ(_endCharacter, line_at_ ## _start[0]); \

	// Start

	CHECK_LINE(3274, '|');
	CHECK_LINE(3354, '|');
	
	// 1  Some correct UTF-8 text

	CHECK_LINE(3434, '|');
	CHECK_LINE(3514, '|');
	CHECK_LINE(3594, '|');
	CHECK_LINE(3680, '|');

	// 2  Boundary condition test cases

	CHECK_LINE(3760, '|');
	CHECK_LINE(3840, '|');

	// 2.1  First possible sequence of a certain length

	CHECK_LINE(3920, '|');
	CHECK_LINE(4000, '|');
	CHECK_LINE(4080, 0);
	CHECK_LINE(4160, '|');
	CHECK_LINE(4241, '|');
	CHECK_LINE(4323, '|');
	CHECK_LINE(4406, '|');
	CHECK_LINE(4490, '|');
	CHECK_LINE(4575, '|');

	// 2.2  Last possible sequence of a certain length

	CHECK_LINE(4655, '|');
	CHECK_LINE(4735, '|');
	CHECK_LINE(4815, ' ');
	CHECK_LINE(4895, '|');
	CHECK_LINE(4976, '|');
	CHECK_LINE(5058, '|');
	CHECK_LINE(5225, '|');
	CHECK_LINE(5310, '|');

	// 2.3  Other boundary conditions

	CHECK_LINE(5390, '|');
	CHECK_LINE(5470, '|');
	CHECK_LINE(5550, '|');
	CHECK_LINE(5632, '|');
	CHECK_LINE(5714, '|');
	CHECK_LINE(5796, '|');
	CHECK_LINE(5879, '|');
	CHECK_LINE(5962, '|');

	// 3  Malformed sequences

	CHECK_LINE(6042, '|');
	CHECK_LINE(6122, '|');

	// 3.1  Unexpected continuation bytes

	CHECK_LINE(6202, '|');
	CHECK_LINE(6282, '|');
	CHECK_LINE(6362, '|');
	CHECK_LINE(6522, '|');
	CHECK_LINE(6602, '|');
	CHECK_LINE(6682, '|');
	CHECK_LINE(6762, '|');
	CHECK_LINE(6842, '|');
	CHECK_LINE(6922, '|');
	CHECK_LINE(7002, '|');
	CHECK_LINE(7082, '|');
	CHECK_LINE(7162, '|');
	CHECK_LINE(7242, '|');
	CHECK_LINE(7322, '|');
	CHECK_LINE(7402, '|');
	CHECK_LINE(7482, '|');
	CHECK_LINE(7562, '|');
	CHECK_LINE(7642, '|');
	CHECK_LINE(7722, '|');
	CHECK_LINE(7802, '|');
	CHECK_LINE(7882, '|');

	// 3.2  Lonely start characters

	CHECK_LINE(7962, '|');
	CHECK_LINE(8042, '|');
	CHECK_LINE(8122, '|');
	CHECK_LINE(8202, '|');
	CHECK_LINE(8282, '|');
	CHECK_LINE(8362, '|');
	CHECK_LINE(8442, '|');
	CHECK_LINE(8522, '|');
	CHECK_LINE(8602, '|');
	CHECK_LINE(8682, '|');
	CHECK_LINE(8762, '|');
	CHECK_LINE(8842, '|');
	CHECK_LINE(8922, '|');
	CHECK_LINE(9002, '|');
	CHECK_LINE(9082, '|');
	CHECK_LINE(9162, '|');
	CHECK_LINE(9242, '|');
	CHECK_LINE(9322, '|');
	CHECK_LINE(9402, '|');
	CHECK_LINE(9482, '|');
	CHECK_LINE(9562, '|');
	CHECK_LINE(9642, '|');
	CHECK_LINE(9722, '|');
	CHECK_LINE(9802, '|');
	CHECK_LINE(9882, '|');
	CHECK_LINE(9962, '|');
	CHECK_LINE(10042, '|');
	CHECK_LINE(10122, '|');

	// 3.3  Sequences with last continuation byte missing

	CHECK_LINE(10202, '|');
	CHECK_LINE(10282, '|');
	CHECK_LINE(10362, '|');
	CHECK_LINE(10442, '|');
	CHECK_LINE(10522, '|');
	CHECK_LINE(10602, '|');
	CHECK_LINE(10682, '|');
	CHECK_LINE(10762, '|');
	CHECK_LINE(10843, '|');
	CHECK_LINE(10925, '|');
	CHECK_LINE(11008, '|');
	CHECK_LINE(11092, '|');
	CHECK_LINE(11172, '|');
	CHECK_LINE(11253, '|');
	CHECK_LINE(11335, '|');
	CHECK_LINE(11418, '|');
	CHECK_LINE(11502, '|');

	// 3.4  Concatenation of incomplete sequences

	CHECK_LINE(11582, '|');
	CHECK_LINE(11662, '|');
	CHECK_LINE(11742, '|');
	CHECK_LINE(11822, '|');
	CHECK_LINE(11902, '|');
	CHECK_LINE(11982, '|');
	CHECK_LINE(12082, '|');

	// 3.5  Impossible bytes

	CHECK_LINE(12162, '|');
	CHECK_LINE(12242, '|');
	CHECK_LINE(12322, '|');
	CHECK_LINE(12402, '|');
	CHECK_LINE(12482, '|');
	CHECK_LINE(12562, '|');
	CHECK_LINE(12642, '|');
	CHECK_LINE(12722, '|');

	// 4  Overlong sequences

	CHECK_LINE(12802, '|');
	CHECK_LINE(12882, '|');
	CHECK_LINE(12962, '|');
	CHECK_LINE(13042, '|');
	CHECK_LINE(13122, '|');
	CHECK_LINE(13202, '|');
	CHECK_LINE(13282, '|');
	CHECK_LINE(13362, '|');
	CHECK_LINE(13442, '|');
	CHECK_LINE(13522, '|');
	CHECK_LINE(13602, '|');
	CHECK_LINE(13682, '|');
	CHECK_LINE(13762, '|');
	CHECK_LINE(13842, '|');
	CHECK_LINE(13922, '|');
	CHECK_LINE(14002, '|');
	CHECK_LINE(14082, '|');
	CHECK_LINE(14162, '|');
	CHECK_LINE(14242, '|');
	CHECK_LINE(14322, '|');

	// 4.1  Examples of an overlong ASCII character

	CHECK_LINE(14402, '|');
	CHECK_LINE(14482, '|');
	CHECK_LINE(14562, '|');
	CHECK_LINE(14642, '|');
	CHECK_LINE(14722, '|');
	CHECK_LINE(14802, '|');
	CHECK_LINE(14882, '|');
	CHECK_LINE(14962, '|');
	CHECK_LINE(15042, '|');
	CHECK_LINE(15123, '|');
	CHECK_LINE(15205, '|');
	CHECK_LINE(15288, '|');
	CHECK_LINE(15372, '|');
	CHECK_LINE(15457, '|');

	// 4.2  Maximum overlong sequences

	CHECK_LINE(15537, '|');
	CHECK_LINE(15617, '|');
	CHECK_LINE(15697, '|');
	CHECK_LINE(15777, '|');
	CHECK_LINE(15857, '|');
	CHECK_LINE(15937, '|');
	CHECK_LINE(16017, '|');
	CHECK_LINE(16097, '|');
	CHECK_LINE(16178, '|');
	CHECK_LINE(16260, '|');
	CHECK_LINE(16343, '|');
	CHECK_LINE(16427, '|');
	CHECK_LINE(16512, '|');

	// 4.3  Overlong representation of the NUL character

	CHECK_LINE(16592, '|');
	CHECK_LINE(16672, '|');
	CHECK_LINE(16752, '|');
	CHECK_LINE(16832, '|');
	CHECK_LINE(16912, '|');
	CHECK_LINE(16992, '|');
	CHECK_LINE(17072, '|');
	CHECK_LINE(17153, '|');
	CHECK_LINE(17235, '|');
	CHECK_LINE(17318, '|');
	CHECK_LINE(17402, '|');
	CHECK_LINE(17487, '|');

	// 5  Illegal code positions

	CHECK_LINE(17567, '|');
	CHECK_LINE(17647, '|');
	CHECK_LINE(17727, '|');
	CHECK_LINE(17807, '|');
	CHECK_LINE(17887, '|');
	CHECK_LINE(17967, '|');
	CHECK_LINE(18047, '|');

	// 5.1 Single UTF-16 surrogates

	CHECK_LINE(18127, '|');
	CHECK_LINE(18207, '|');
	CHECK_LINE(18287, '|');
	CHECK_LINE(18369, '|');
	CHECK_LINE(18451, '|');
	CHECK_LINE(18533, '|');
	CHECK_LINE(18615, '|');
	CHECK_LINE(18697, '|');
	CHECK_LINE(18779, '|');
	CHECK_LINE(18861, '|');

	// 5.2 Paired UTF-16 surrogates

	CHECK_LINE(18941, '|');
	CHECK_LINE(19021, '|');
	CHECK_LINE(19101, '|');
	CHECK_LINE(19185, '|');
	CHECK_LINE(19269, '|');
	CHECK_LINE(19353, '|');
	CHECK_LINE(19437, '|');
	CHECK_LINE(19521, '|');
	CHECK_LINE(19605, '|');
	CHECK_LINE(19689, '|');
	CHECK_LINE(19773, '|');

	// 5.3 Other illegal code positions

	CHECK_LINE(19853, '|');
	CHECK_LINE(19933, '|');
	CHECK_LINE(20013, '|');
	CHECK_LINE(20095, '|');
	CHECK_LINE(20177, '|');

	// THE END

	CHECK_LINE(20257, '|');

#undef CHECK_LINE
}