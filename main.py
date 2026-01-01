import os
import sys
import argparse
from library_of_babel import LibraryOfBabel, LENGTH_OF_PAGE
from book import Book

__author__ = 'AneoPsy'
__version__ = 0.1

def _cli_opts():
    """
    Parse command line options.
    @returns the arguments
    """
    mepath = os.path.abspath(sys.argv[0])
    mebase = os.path.basename(mepath)

    description = (
        "An open-source Python implementation of the Library of Babel.\n"
        "Allows for searching text, titles, and browsing specific pages."
    )

    parser = argparse.ArgumentParser(
        prog=mebase,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=description
    )

    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('-c', '--checkout', action='store', help='Get a page by its address.')
    group.add_argument('-s', '--search', action='store', help='Search for text anywhere on a page.')
    group.add_argument('-os', '--onlysearch', action='store', help='Search for a page containing only this text.')
    group.add_argument('-ts', '--titlesearch', action='store', help='Search for a book by its title.')
    group.add_argument('-b', '--browse', action='store', help='Browse a book page by page.')

    parser.add_argument(
        '-V', '--version',
        action='version',
        version=f'%(prog)s v{__version__} by {__author__}'
    )

    return parser.parse_args()

def _run_checkout(args):
    LibraryOfBabel.print_page(args.checkout)

def _run_search(args):
    search_str = LibraryOfBabel.text_prep(args.search)
    key_str = LibraryOfBabel.search(search_str)
    LibraryOfBabel.print_page(key_str)

def _run_only_search(args):
    search_str = LibraryOfBabel.text_prep(args.onlysearch)
    padded_search_str = search_str.ljust(LENGTH_OF_PAGE)
    only_key_str = LibraryOfBabel.search(padded_search_str)
    LibraryOfBabel.print_page(only_key_str)

def _run_title_search(args):
    search_str = LibraryOfBabel.text_prep(args.titlesearch)
    print(LibraryOfBabel.search_title(search_str))

def _run_browse(args):
    book = Book()
    book.new(args.browse)

    while True:
        book.printPage()
        try:
            cmd = input('> ')
            if '>' in cmd:
                book.next()
            elif '<' in cmd:
                book.prev()
            elif cmd.lower() in ['q', 'quit', 'exit']:
                break
        except (EOFError, KeyboardInterrupt):
            print("\nExiting browse mode.")
            break

def main():
    args = _cli_opts()
    if args.checkout:
        _run_checkout(args)
    elif args.search:
        _run_search(args)
    elif args.onlysearch:
        _run_only_search(args)
    elif args.titlesearch:
        _run_title_search(args)
    elif args.browse:
        _run_browse(args)

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print('\nExit...')
