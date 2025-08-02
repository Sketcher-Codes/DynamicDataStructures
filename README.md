# Dynamic Data Structures (For R)

### Raison d'etre

This package exists support working with big data, especially big dynamic data in R.
By 'Big' data I mean everything upwards of 50,000 rows where R's pass by value nature can severly hurt performance.

This package is built with the following precepts in mind:

1. Your time is expensive.
2. You cannot afford to spend hours, let alone days on optimisation.
3. You want to code C-style, and not in R-jargon.
4. Your system may only be able to hold a fraction of your data in RAM at a time.
5. You want to vett the package before you use it, and have the option to maintain the code yourself.
6. You want to use this package only where it excells at performance, and easily transition to fast elements base R where they are optimal.

### Vision

Core features
- Dynamically resizing pass-by-reference vectors and lists that can be used fully in-RAM or streamed from the hard drive.
- Hashtable for fast key-value lookup - complete with capabilities to scan for key collision.

Stretch features
- Dictionary for key-value lookup - strictly promising no key collision, at the cost of some performance.
- Spatial partitioning trees - perhaps working in arbitary dimientions, and maybe even with combined numeric and character vector input.

Future
- Brainstorm some fun new cool ideas to code...
