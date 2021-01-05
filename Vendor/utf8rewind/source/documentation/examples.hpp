/*!
	\page examples Examples

	\tableofcontents

	\section example-changes Changes to existing code

	Suppose you maintain a client-server application written in C. The client
	connects to a central server and allows the user to manipulate the database
	in some way. In order for clients to login, the server needs to check their
	credentials. This is accomplished by the client hashing the specified
	password with a salt before sending it over to the server:

	\code
		uint8_t Login_CheckCredentials(const char* username, const char* password)
		{
			const char* salt;
			const char* hashed_password;
			char verify_password[256];
			char hashed_verify_password[256];

			// For the purposes of brevity, ignore the fact that this is a
			// terrible way of generating a salt.
			memset(verify_password, 0, sizeof(verify_password));
			strcpy(verify_password, md5(password));
			strcat(verify_password, md5(username));

			memset(hashed_verify_password, 0, sizeof(hashed_verify_password));
			strcpy(hashed_verify_password, md5(verify_password));

			return Database_CheckLogin(username, hashed_verify_password);
		}
	\endcode

	We want to improve the security of the application by allowing the full
	range of Unicode code points in the passwords. We can accomplish this by
	encoding the code points using UTF-8. The beauty of this change is that the
	password algorithm does *not* have to be modified. Because all functions
	already work on `char` arrays, we can encode Unicode as UTF-8, but treat
	the string as valid ASCII.

	A password like \"M&ocirc;ntiPyth&ocirc;nikdenH&ocirc;lieGr&acirc;ilen\"
	would be encoded as

		"M\xC3\xB4Pyth\xC3\xB4" "denH\xC3\xB4Gr\xC3\xA2" "en"

	which is backwards-compatible with ASCII. Calls to `strcpy` and `strcat`
	still work as expected, because the string does not contain NUL-terminators
	(`\0`), except to signify the end of data.

	When converting your project to work with UTF-8 encoded text, there are
	only two surface areas you will have to concern yourself with: input and
	display.

	We'll look at these individually.

	\section example-user-input Dealing with user input

	Continuing with our previous example, the password field in the client
	application only accepted ASCII. This is how the password field is currently
	implemented:

	\code{.c}
		static const size_t g_PasswordInputMaximum = 255;
		static char g_PasswordInput[256];

		uint8_t PasswordField_EnterCharacter(char input)
		{
			char text_input[2];

			if ((strlen(g_PasswordInput) + 1) > g_PasswordInputMaximum)
			{
				return 0;
			}

			text_input[0] = input;
			text_input[1] = 0;

			strcat(g_PasswordInput, text_input);

			return 1;
		}
	\endcode

	What we want to do is make sure the password field can accept Unicode input.
	To that end, we'll change the input type to #unicode_t, which can encode
	every valid Unicode code point.

	\code{.c}
		uint8_t PasswordField_EnterCharacter(unicode_t* input, size_t inputSize)
	\endcode

	Something that surprises many people working with Unicode is that although
	UTF-32 can contain all possible valid code points, Unicode *itself* is a
	variable-length encoding! For instance, consider a string like "U+0399
	U+0308" (GREEK CAPITAL LETTER IOTA and COMBINING DIAERESIS). Although each
	encodes a separate code point, they can be combined (normalized) into U+03AA
	(GREEK CAPITAL LETTER IOTA WITH DIALYTIKA). This means that splitting the
	string after U+0399 is not valid, as it would change the meaning of the
	encoded text.

	There are also many instances of code points that can be combined when
	rendering, but aren't explicitly listed in the Unicode database. This
	allowed the Unicode Consortium to encode many more characters without taking
	up space in the database. The downside for developers is of course that it
	can be difficult to anticipate how much space will be required to store any
	particular amount of text.

	Back to our example.

	Every location that calls `PasswordField_EnterCharacter` will have to cast
	the parameter to #unicode_t, but luckily this is backwards-compatible. All
	values in ASCII (0x00 to 0x7F) encode the same characters in Unicode.

	Inside the function, we'll want to convert the UTF-32 code point to UTF-8.
	To that end, we'll use #utf32toutf8.

	\code{.c}
		char* text_input = NULL;
		size_t text_input_size;
		size_t converted_size;
		int32_t errors = UTF8_ERR_NONE;

		if ((text_input_size = utf32toutf8(input, inputSize, NULL, 0, &errors)) == 0 ||
			errors != UTF8_ERR_NONE)
		{
			goto cleanup;
		}

		text_input = (char*)malloc(text_input_size + 1);
		if (text_input == NULL)
		{
			goto cleanup;
		}
		text_input[text_input_size] = 0;

		if (utf32toutf8(input, inputSize, text_input, text_input_size, &errors) == 0 ||
			errors != UTF8_ERR_NONE)
		{
			goto cleanup;
		}
	\endcode

	We first determine the length of the input before we allocate memory for it.
	This way, we ensure that we're protected from buffer-overflow attacks. If
	you're not too keen on dynamic memory allocations, you could limit the input
	sequence to six code points, which would require a maximum of 32 bytes
	to encode.

	It's good practice to always check the error code after each call. However,
	never explicitly check for any particular error code! `utf8rewind` may
	introduce new error codes on every release, so you should check if the error
	code does _not_ equal `UTF8_ERR_NONE`.

	Because the converted string can now consist of more than one byte, we'll
	have to change the check to see if we're out of bounds of the password input
	array.

	\code{.c}
		if ((strlen(g_PasswordInput) + strlen(text_input)) > g_PasswordInputMaximum)
		{
			return 0;
		}
	\endcode

	The rewritten version is now fully compatible with Unicode.

	\code{.c}
		static const size_t g_PasswordInputMaximum = 255;
		static char g_PasswordInput[256];

		uint8_t PasswordField_EnterCharacter(unicode_t input)
		{
			uint8_t result = 1;
			char* text_input = NULL;
			size_t text_input_size;
			size_t converted_size;
			int32_t errors = UTF8_ERR_NONE;

			if ((text_input_size = utf32toutf8(input, inputSize, NULL, 0, &errors)) == 0 ||
				errors != UTF8_ERR_NONE)
			{
				result = 0;

				goto cleanup;
			}

			text_input = (char*)malloc(text_input_size + 1);
			if (text_input == NULL)
			{
				result = 0;

				goto cleanup;
			}
			text_input[text_input_size] = 0;

			if (utf32toutf8(input, inputSize, text_input, text_input_size, &errors) == 0 ||
				errors != UTF8_ERR_NONE)
			{
				result = 0;

				goto cleanup;
			}

			if ((strlen(g_PasswordInput) + strlen(text_input)) > g_PasswordInputMaximum)
			{
				result = 0;

				goto cleanup;
			}

			strcat(g_PasswordInput, text_input);

		cleanup:
			if (text_input != NULL)
			{
				free(text_input);
			}

			return result;
		}
	\endcode

	With only a few changes, we've upgraded a text field that previously only
	accepted ASCII to accept the full range of Unicode. And we didn't even have
	to change the actual algorithm.

	\section example-display Displaying Unicode text

	Even though the user is now able to enter Unicode text, it won't show up
	right on her screen. That's because the font rendering implementation
	expects ASCII instead of Unicode.

	Let's take a look at the offending function:

	\code{.c}
		void InputField_Draw(int x, int y, const char* text)
		{
			const char* src = text;
			size_t i;

			FontBatch_Start("Arial20");

			for (i = 0; i < strlen(text); ++i)
			{
				FontBatch_AddCharacter(*src);

				src++;
			}

			FontBatch_End();
			FontBatch_Draw(x, y);
		}
	\endcode

	The issue here is that `FontBatch_AddCharacter` expects code points encoded
	as one byte per code point. Because UTF-8 is a variable-length encoding,
	this isn't necessarily true anymore.

	In order for the font renderer to display Unicode text, we'll need to
	convert the UTF-8 encoded text to UTF-32. To that end, we'll use
	#utf8toutf32.

	\code{.c}
		void InputField_Draw(int x, int y, const char* text)
		{
			size_t text_size = strlen(text);
			unicode_t* converted = NULL;
			size_t converted_size;
			int32_t errors = UTF8_ERR_NONE;
			size_t i;
			unicode_t* src;

			if ((converted_size = utf8toutf32(text, text_size, NULL, 0, &errors)) == 0 ||
				errors != UTF8_ERR_NONE)
			{
				goto cleanup;
			}

			converted = (unicode_t*)malloc(converted_size);

			if (utf8toutf32(text, text_size, converted, converted_size, &errors) == 0 ||
				errors != UTF8_ERR_NONE)
			{
				goto cleanup;
			}

			FontBatch_Start("Arial20");

			src = decoded;

			for (i = 0; i < decoded_size / sizeof(unicode_t); ++i)
			{
				FontBatch_AddCharacter(*src);

				src++;
			}

			FontBatch_End();
			FontBatch_Draw(x, y);

		cleanup:
			if (converted != NULL)
			{
				free(converted);
				converted = NULL;
			}
		}
	\endcode

	After this conversion, the `FontBatch_AddCharacter` function would have to
	be modified in order to handle Unicode code points. Fortunately this is a
	string handling library, not a font rendering one, so I can get away with
	saying I'm leaving it as an exercise to the reader.

	One thing to keep in mind is that even though we convert the entire input
	string to UTF-32 before rendering it in this example, it's equally valid to
	read only one codepoint at a time. However, you'll need to use #utf8seek to
	move the cursor to the next codepoint.

	\section example-users Comparing usernames in a case-insensitive manner

	After the user has entered their username and password, we'll need to verify
	their identity. This is done on the back-end, with a straightforward
	function:

	\code{.c}
		typedef struct UserData_ {
			const char* username;
			const char* passwordHash;
		} UserData;

		static const UserData Users[] = {
			{ "qlansu", "11111" },
			{ "Admin", "ASDFSF" },
			{ "User1", "u99123" }
		};

		uint8_t Database_IsUserValid(const char* username, const char* passwordHash)
		{
			size_t i;

			for (i = 0; i < sizeof(Users) / sizeof(UserData); ++i)
			{
				if (!strcmp(Users[i].username, username) &&
					!strcmp(Users[i].passwordHash, passwordHash))
				{
					return 1;
				}
			}

			return 0;
		}
	\endcode

	Unfortunately, users being users, they end up forgetting the exact
	combination of upper- and lowercase characters they used for their
	username.

	Luckily, we can use #utf8casefold to do case-insensitive text comparison.
	Case folding is an operation that erases case distinction between code
	points. This process allows you to compare and match strings that wouldn't
	be considered equivalent otherwise.

	First, we ensure that the usernames stored in our "database" are casefolded:

	\code{.c}
		static const Users[] = {
			{ "qlansu", "11111" },
			{ "admin", "asdfsf" },
			{ "user1", "u99123" }
		};
	\endcode

	Next, we casefold the incoming string before we compare it to the username:

	\code{.c}
		uint8_t Database_IsUserValid(const char* username, const char* passwordHash)
		{
			uint8_t result;
			char* compare_username = NULL;
			size_t compare_username_size = 0;
			int32_t errors;
			size_t i;

			if ((compare_username_size = utf8casefold(username, strlen(username), NULL, 0, UTF8_LOCALE_DEFAULT, &errors)) == 0 ||
				errors != UTF8_ERR_NONE)
			{
				result = 0;

				goto cleanup;
			}

			compare_username = (char*)malloc(compare_username_size + 1);
			if (compare_username == 0)
			{
				result = 0;

				goto cleanup;
			}
			compare_username[compare_username_size] = 0;

			if (utf8casefold(username, strlen(username), compare_username, compare_username_size, UTF8_LOCALE_DEFAULT, &errors) == 0 ||
				errors != UTF8_ERR_NONE)
			{
				result = 0;

				goto cleanup;
			}

			for (i = 0; i < sizeof(Users) / sizeof(UserData); ++i)
			{
				if (!strcmp(Users[i].username, compare_username) &&
					!strcmp(Users[i].passwordHash, passwordHash))
				{
					result = 1;

					goto cleanup;
				}
			}

		cleanup:
			if (compare_username != NULL)
			{
				free(compare_username);
			}

			return result;
		}
	\endcode

	One thing to keep in mind is that case folding (and uppercasing, lowercasing
	and titlecasing) are locale-sensitive. That is, their behavior may be
	changed by the specified locale. Something to keep in mind when dealing
	with, for example, Turkish or Azerbaijani text.
*/