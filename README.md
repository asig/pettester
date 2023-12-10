# pettester
pettester is a test ROM for your PET. It is based on the PETTESTE2K
written by David Roberts. All the versions that he shared in 
[this post](https://forum.vcfed.org/index.php?threads/pettester-versions-and-manuals.1238265/post-1252044)
are contained in the "petteste2k" subdirectory.

Two changes where added to the original PETTESTE2K:
   - Checksums are not computed over the full 4KB anymore, but rather over
     the 2KB that are contained in a single ROM.
   - For every ROM, the first 16 bytes are shown on the screen.

I added these changes to find the issue that prevented my stubborn 2001 from
working. Maybe they are useful for other people, too.

## How to build
To build pettester, you need the following tools:
   - `make`
   - [cbmasm](https://github.com/asig/cbmasm)

Then, just run `make` to build `pettester.bin`

## License
Copyright (c) 2023 Andreas Signer.
Licensed under [GPLv3](https://www.gnu.org/licenses/gpl-3.0).
