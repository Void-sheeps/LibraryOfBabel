import string
import random
import sys
from typing import Set

# --- Constants ---
# Character sets
DIGITS: str = 'abcdefghijklmnopqrstuvwxyz, .'
ALPHANUM: str = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

# Library dimensions
NUMBER_OF_WALLS: int = 4
NUMBER_OF_SHELVES: int = 5
NUMBER_OF_VOLUMES: int = 32
NUMBER_OF_PAGES: int = 410
NUMBER_OF_LINES: int = 40

# Calculated constants
NUMBER_OF_DIGITS: int = len(DIGITS)
NUMBER_OF_ALPHANUM: int = len(ALPHANUM)
LENGTH_OF_TITLE: int = 25
LENGTH_OF_LINE: int = 80
LENGTH_OF_PAGE: int = LENGTH_OF_LINE * NUMBER_OF_LINES

# Multipliers for encoding/decoding
LOC_MULT: int = pow(30, LENGTH_OF_PAGE)
TITLE_MULT: int = pow(30, LENGTH_OF_TITLE)

# ANSI color codes
COLOR_RED: str = '\033[93m'
COLOR_DEFAULT: str = '\033[0m'


class LibraryOfBabel:
    """
    A class that encapsulates the logic for the Library of Babel.
    All methods are static as they do not depend on the state of an instance.
    """
    @staticmethod
    def text_prep(text: str) -> str:
        """
        Prepares text for use in the library by filtering for allowed characters.
        """
        allowed_chars: Set[str] = set(DIGITS)
        prepared = ''
        for letter in text:
            if letter in allowed_chars:
                prepared += letter
            elif letter.lower() in allowed_chars:
                prepared += letter.lower()
            elif letter == '\n':
                prepared += ' '
        return prepared

    @staticmethod
    def print_search_page(key_str: str, search_str: str) -> str:
        """
        Formats and returns a page with its title, used for search results.
        """
        page = LibraryOfBabel.get_title(key_str) + '\n'
        text = LibraryOfBabel.get_page(key_str)
        for i in range(0, len(text), LENGTH_OF_LINE):
            page += text[i:i + LENGTH_OF_LINE] + '\n'
        return page

    @staticmethod
    def print_page(key_str: str) -> str:
        """
        Prints a page and its title to the console.
        """
        title = LibraryOfBabel.get_title(key_str)
        print(title)
        text = LibraryOfBabel.get_page(key_str)
        page_content = title + '\n'
        for i in range(0, len(text), LENGTH_OF_LINE):
            line = text[i:i + LENGTH_OF_LINE]
            print(line)
            page_content += line + '\n'
        print('\n' + key_str)
        return page_content

    @staticmethod
    def search(search_str: str) -> str:
        """
        Searches for a specific string in the library and returns its address.
        """
        wall = str(random.randint(0, NUMBER_OF_WALLS - 1))
        shelf = str(random.randint(0, NUMBER_OF_SHELVES - 1))
        volume = str(random.randint(0, NUMBER_OF_VOLUMES - 1)).zfill(2)
        page = str(random.randint(0, NUMBER_OF_PAGES - 1)).zfill(3)
        loc_str = page + volume + shelf + wall
        loc_int = int(loc_str)

        depth = random.randint(0, LENGTH_OF_PAGE - len(search_str))
        front_padding = ''.join(random.choices(DIGITS, k=depth))
        back_padding = ''.join(random.choices(DIGITS, k=LENGTH_OF_PAGE - (depth + len(search_str))))

        full_page_text = front_padding + search_str + back_padding
        page_as_number = LibraryOfBabel.string_to_number(full_page_text)
        encoded_number = page_as_number + (loc_int * LOC_MULT)
        hex_addr = LibraryOfBabel.int_to_base(encoded_number, NUMBER_OF_ALPHANUM)

        key_str = f"{hex_addr}:{wall}:{shelf}:{volume}:{page}"

        retrieved_page_text = LibraryOfBabel.get_page(key_str)
        assert retrieved_page_text == full_page_text, f"\nPage Text:\n{retrieved_page_text}\nStrings:\n{full_page_text}"
        return key_str

    @staticmethod
    def get_title(address: str) -> str:
        """
        Retrieves the title of a book from its address.
        """
        parts = address.split(':')
        hex_addr = parts[0]
        wall = parts[1]
        shelf = parts[2]
        volume = parts[3].zfill(2)
        loc_int = int(volume + shelf + wall)

        key = int(hex_addr, NUMBER_OF_ALPHANUM)
        key -= loc_int * TITLE_MULT

        str_an = LibraryOfBabel.int_to_base(key, NUMBER_OF_ALPHANUM)
        result = LibraryOfBabel.to_text(int(str_an, NUMBER_OF_ALPHANUM))

        random.seed(result)
        while len(result) < LENGTH_OF_TITLE:
            result += random.choice(DIGITS)

        return result[:LENGTH_OF_TITLE]

    @staticmethod
    def search_title(search_str: str) -> str:
        """
        Searches for a specific title and returns the address of the volume.
        """
        wall = str(random.randint(0, NUMBER_OF_WALLS - 1))
        shelf = str(random.randint(0, NUMBER_OF_SHELVES - 1))
        volume = str(random.randint(0, NUMBER_OF_VOLUMES - 1)).zfill(2)
        loc_str = volume + shelf + wall
        loc_int = int(loc_str)

        padded_search_str = search_str[:LENGTH_OF_TITLE].ljust(LENGTH_OF_TITLE)
        title_as_number = LibraryOfBabel.string_to_number(padded_search_str)
        encoded_number = title_as_number + (loc_int * TITLE_MULT)
        hex_addr = LibraryOfBabel.int_to_base(encoded_number, NUMBER_OF_ALPHANUM)

        key_str = f"{hex_addr}:{wall}:{shelf}:{volume}"
        assert padded_search_str == LibraryOfBabel.get_title(key_str)
        return key_str

    @staticmethod
    def get_page(address: str) -> str:
        """
        Retrieves the full text of a page from its address.
        """
        hex_addr, wall, shelf, volume, page = address.split(':')
        volume = volume.zfill(2)
        page = page.zfill(3)
        loc_int = int(page + volume + shelf + wall)

        key = int(hex_addr, NUMBER_OF_ALPHANUM)
        key -= loc_int * LOC_MULT
        str_an = LibraryOfBabel.int_to_base(key, NUMBER_OF_ALPHANUM)
        result = LibraryOfBabel.to_text(int(str_an, NUMBER_OF_ALPHANUM))

        random.seed(result)
        while len(result) < LENGTH_OF_PAGE:
            result += random.choice(DIGITS)

        return result[:LENGTH_OF_PAGE]

    @staticmethod
    def to_text(x: int) -> str:
        """
        Converts a number to its base-29 representation using the DIGITS character set.
        """
        if x == 0:
            return DIGITS[0]
        sign = -1 if x < 0 else 1
        x *= sign
        digits = []
        while x:
            digits.append(DIGITS[x % NUMBER_OF_DIGITS])
            x //= NUMBER_OF_DIGITS
        if sign < 0:
            digits.append('-')
        return "".join(reversed(digits))

    @staticmethod
    def string_to_number(iString: str) -> int:
        """
        Converts a string from the DIGITS character set to its numerical representation.
        """
        result = 0
        for i, char in enumerate(reversed(iString)):
            result += DIGITS.index(char) * (NUMBER_OF_DIGITS ** i)
        return result

    @staticmethod
    def int_to_base(x: int, base: int) -> str:
        """
        Converts an integer to a string in a given base using the ALPHANUM character set.
        """
        if x == 0:
            return ALPHANUM[0]
        sign = -1 if x < 0 else 1
        x *= sign
        digits = []
        while x:
            digits.append(ALPHANUM[x % base])
            x //= base
        if sign < 0:
            digits.append('-')
        return "".join(reversed(digits))
