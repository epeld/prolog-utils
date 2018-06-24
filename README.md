# Welcome to PrologPDF!

PrologPDF provides utilities for working with PDF-files. Currently, it supports:

- Listing objects (and their types) in a PDF-file
- Printing the characters representing a given PDF-object
- Decoding and printing the contents of PDF streams

In the future PrologPDF will support converting the text of the PDF documents into e.g XML / HTML so that it can be used to generate EPUB-books.

## Example usage
Here are some example usages. Note that the examples refer to a *prologpdf* command line tool which will be availabel as soon as I figure out how to make SWI Prolog build it. In the meantime, you can call the predicate `prologpdf` directly inside *prologpdf.pl* and it will behave as the examples.

### Listing pdf objects:
```bash
prologpdf list my.pdf
```

Example output:
```
1 0 R      resources
2 0 R           page
3 0 R         stream
4 0 R      font_desc
5 0 R         stream
6 0 R           font
7 0 R      font_desc
8 0 R         stream
9 0 R           font
```

### Printing a PDF object
```bash
prologpdf raw my.pdf "10 0 R"
```
Example output:
```
10 0 obj <<
/Ascent 694
/CapHeight 683
/Descent -194
/FontName /OJKWII+CMR10
/ItalicAngle 0
/StemV 69
/XHeight 431
/FontBBox [-251 -250 1009 969]
/Flags 4
/CharSet (/Sigma/ff/fi/fl/ffi/exclam/quotedblright/quoteright/parenleft/parenright/asterisk/plus/comma/hyphen/period/zero/one/two/three/four/five/six/seven/eight/nine/colon/semicolon/equal/question/A/B/C/D/E/F/G/H/I/J/L/M/N/O/P/Q/R/S/T/U/V/W/Y/bracketleft/quotedblleft/bracketright/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z/emdash)
/FontFile 11 0 R
>> endobj
```

### Decoding a PDF stream object
```bash
prologpdf print my.pdf "11 0 R"
```

Example output:
```
BT
/F30 9.963 Tf 211.283 683.997
[...]
ET
```

## Building
Typing `make` should produce an app called `prologpdf`, provided you have `swipl` installed.

### For Development
The file *load.pl* will load and compile all the source files for you.