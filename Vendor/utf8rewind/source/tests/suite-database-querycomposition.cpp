#include "tests-base.hpp"

extern "C" {
	#include "../internal/database.h"
}

TEST(QueryComposition, Found)
{
	EXPECT_EQ(0x00001FF2, database_querycomposition(0x00001F7C, 0x00000345));
}

TEST(QueryComposition, FoundFirst)
{
	EXPECT_EQ(0x0000226E, database_querycomposition(0x0000003C, 0x00000338));
}

TEST(QueryComposition, FoundLast)
{
	EXPECT_EQ(0x000115BB, database_querycomposition(0x000115B9, 0x000115AF));
}

TEST(QueryComposition, FoundPivot)
{
	EXPECT_EQ(0x00001F39, database_querycomposition(0x00000399, 0x00000314));
}

TEST(QueryComposition, FoundPivotUp)
{
	EXPECT_EQ(0x0000012D, database_querycomposition(0x00000069, 0x00000306));
}

TEST(QueryComposition, FoundPivotDown)
{
	EXPECT_EQ(0x00001F80, database_querycomposition(0x00001F00, 0x00000345));
}

TEST(QueryComposition, FoundPivotDownDown)
{
	EXPECT_EQ(0x00001FDE, database_querycomposition(0x00001FFE, 0x00000301));
}

TEST(QueryComposition, FoundMaxDepth)
{
	EXPECT_EQ(0x00002284, database_querycomposition(0x00002282, 0x00000338));
}

TEST(QueryComposition, Missing)
{
	EXPECT_EQ(0, database_querycomposition(0x00001F28, 0x00001D16));
}

TEST(QueryComposition, MissingOutOfLowerBounds)
{
	EXPECT_EQ(0, database_querycomposition(0x00000001, 0x0000011D));
}

TEST(QueryComposition, MissingOutOfUpperBounds)
{
	EXPECT_EQ(0, database_querycomposition(0xABABABAB, 0xDADADADA));
}