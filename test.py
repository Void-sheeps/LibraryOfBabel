import unittest
from library_of_babel import LibraryOfBabel, LENGTH_OF_PAGE, NUMBER_OF_ALPHANUM

class LibraryOfBabelTest(unittest.TestCase):

    def test_string_to_number_conversion(self):
        """
        Tests the conversion from string to number.
        """
        self.assertEqual(LibraryOfBabel.string_to_number('a'), 0)
        self.assertEqual(LibraryOfBabel.string_to_number('ba'), 29)

    def test_page_retrieval_and_length(self):
        """
        Tests that a retrieved page has the correct length.
        """
        address = 'asaskjkfsdf:2:2:2:33'
        page_content = LibraryOfBabel.get_page(address)
        self.assertEqual(len(page_content), LENGTH_OF_PAGE)

    def test_text_to_int_to_text_roundtrip(self):
        """
        Tests that converting text to a number and back results in the original text.
        """
        original_text = 'hello kitty'
        numerical_representation = LibraryOfBabel.string_to_number(original_text)
        base36_representation = LibraryOfBabel.int_to_base(numerical_representation, NUMBER_OF_ALPHANUM)
        reconverted_number = int(base36_representation, NUMBER_OF_ALPHANUM)
        reconverted_text = LibraryOfBabel.to_text(reconverted_number)
        self.assertEqual(original_text, reconverted_text)

    def test_int_to_base_conversion(self):
        """
        Tests the integer to base conversion logic.
        """
        self.assertEqual(LibraryOfBabel.int_to_base(4, 36), '4')
        self.assertEqual(LibraryOfBabel.int_to_base(10, 36), 'A')

    def test_search_functionality(self):
        """
        Tests that the search function finds a page containing the searched string.
        """
        test_string = '.................................................'
        address = LibraryOfBabel.search(test_string)
        page_content = LibraryOfBabel.get_page(address)
        self.assertIn(test_string, page_content)

    def test_page_printing(self):
        """
        Tests the page printing function to ensure it runs without errors.
        """
        # This is a simple test to ensure the function executes.
        # More complex validation would require redirecting stdout.
        try:
            LibraryOfBabel.print_page('HELLO:0:0:0:0')
        except Exception as e:
            self.fail(f"print_page raised an exception: {e}")

if __name__ == '__main__':
    unittest.main()
