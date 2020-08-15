# org-davep-2bit

This is the early stages of some "for fun" code I'm writing in Common Lisp.
The code allows access to [`2bit` format
files](https://genome.ucsc.edu/FAQ/FAQformat.html#format7). These are files
that can hold DNA sequences in a compressed format. You'll see examples of
this if you look at the [human genome
data](http://hgdownload.cse.ucsc.edu/goldenPath/hg38/bigZips/), for example.

Right now this repo is in active development, giving me a fun project to
de-rust my rusty Common Lisp knowledge and to get used to
[SLIME](https://common-lisp.net/project/slime/) again.

Things left to do include at least:

- [X] Add masking support.
- [ ] Handle big-endian files.
- [ ] Documentation

[//]: # (README.md ends here)
