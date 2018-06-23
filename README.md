# Welcome to PrologPDF!

PrologPDF provides utilities for working with PDF-files. Currently, it supports:

- Listing objects (and their types) in a PDF-file
- Printing the characters representing a given PDF-object
- Decoding and printing the contents of PDF streams

In the future PrologPDF will support converting the text of the PDF documents into e.g XML / HTML so that it can be used to generate EPUB-books.

## Example usage
Here are some example usages. Note that the examples refer to a *prologpdf* command line tool which will be availabel as soon as I figure out how to make SWI Prolog build it. In the meantime, you can call the predicate `main` directly inside *main.pl* and it will behave as the examples.

### Listing pdf objects:
```bash
prologpdf list my.pdf
```

### Printing a PDF object
```bash
prologpdf raw my.pdf "10 0 R"
```

### Decoding a PDF stream object
```bash
prologpdf print my.pdf "11 0 R"
```

## Building
Load the file *load.pl* inside SWI prolog. You can then call `main` or `main_with_args` inside *main.pl* to run the applications.

In the future, I will figure out how to actually produce an application that you can run directly.