# org-davep-2bit

## Introduction

`org-davep-2bit` is a small Common Lisp library that provides code for
reading data from [`2bit` format
files](https://genome.ucsc.edu/FAQ/FAQformat.html#format7). These are files
that can hold DNA sequences in a compressed format. You'll see examples of
this if you look at the [human genome
data](http://hgdownload.cse.ucsc.edu/goldenPath/hg38/bigZips/), for example.
I believe there are other bodies of code out there for doing this, I wrote
this one with two main motivations in mind:

1. To de-rust my rusty Common Lisp -- I'd not written anything in CL for a
   while and wanted to refresh my memory about CL itself, and also using
   [SLIME](https://common-lisp.net/project/slime/) in Emacs.
2. I wanted to write a 2bit reader that could provide different routes to
   reading the code. So, not just reading from a local file, but also
   reading via HTTP(S) from a remote file (as you'll see happen in a
   web-based genome browser such as
   [Biodalliance](https://www.biodalliance.org/)).

Note that, as of the time of writing, the code is designed such that the
latter can happen, but only reading from a local file has been implemented.
Adding support for remote reading of 2bit files is left as an exercise for
me to tinker with over time.

## Usage

The library is broken up into two main classes, a reader class and a
sequence class. The sequence class (called `2bit-sequence`) is what provides
access to the content of an actual sequence within the 2bit file, whereas
the reader class does the work of reading data from a 2bit source.

Note that all code lives within a package called `org.davep.2bit`. See
[`packages.lisp`](./packages.lisp) for a list of the exported symbols.

### The reader class

For now the main reader class to use is `file-reader`. A helper function
called `make-file-reader` is available to quickly and easily create an
instance of a `file-reader`. For example:

```lisp
CL-USER> (in-package :2bit)
#<PACKAGE "ORG.DAVEP.2BIT">
2BIT> (make-file-reader "hg38.2bit")
#<FILE-READER "hg38.2bit" :masking T>
```

The reader class offers the following methods:

#### open-reader

Given an instance of a reader, this method opens the connection to the
source data. For example:

```lisp
CL-USER> (in-package :2bit)
#<PACKAGE "ORG.DAVEP.2BIT">
2BIT> (make-file-reader "hg38.2bit")
#<FILE-READER "hg38.2bit" :masking T>
2BIT> (open-reader *)
#<FILE-READER "hg38.2bit" :masking T>
```

Once the reader is open the content can be accessed (see below).

#### close-reader

Given an instance of a reader, this method closes the connection to the
source data. For example:

```lisp
CL-USER> (in-package :2bit)
#<PACKAGE "ORG.DAVEP.2BIT">
2BIT> (make-file-reader "hg38.2bit")
#<FILE-READER "hg38.2bit" :masking T>
2BIT> (open-reader *)
#<FILE-READER "hg38.2bit" :masking T>
2BIT> (close-reader *)
T
2BIT>
```

#### sequences

Given an open reader object, this method returns a list of all of names of
all of the sequences held in the 2bit data. For example:

```lisp
2BIT> (open-reader (make-file-reader "hg38.2bit"))
#<FILE-READER "hg38.2bit" :masking T>
2BIT> (sequences *)
("chr1" "chr10" "chr11" "chr11_KI270721v1_random" "chr12" "chr13" "chr14"
 [...SNIP...] "chrY_KI270740v1_random")
2BIT>
```

#### seq

Given an open reader object, this method provides access to a particular
named sequence. For example:

```lisp
2BIT> (open-reader (make-file-reader "hg38.2bit"))
#<FILE-READER "hg38.2bit" :masking T>
2BIT> (seq * "chr1")
#<2BIT-SEQUENCE "chr1" 10487>
2BIT>
```

### The sequence class

As mentioned above, access to the actual sequence data is done via the
`2bit-sequence` class -- this is what is returned by the call to `seq`
mentioned above. Currently there is just one main method associated with
this class:

#### bases

This method is used to get bases from the sequence. The parameters are the
sequence to pull the bases from, the start location within the sequence, and
the end location within the sequence. Note that, as is the usual convention,
what's returned is *inclusive* of the start location but *exclusive* of the
end location.

For example:

```lisp
2BIT> (open-reader (make-file-reader "hg38.2bit"))
#<FILE-READER "hg38.2bit" :masking T>
2BIT> (seq * "chr1")
#<2BIT-SEQUENCE "chr1" 10487>
2BIT> (bases * 10000 10010)
"taaccctaac"
2BIT>
```

### Helper macro

Note that, when creating and using a reader object yourself, it's very
important that you open it, only use it while open, and then close it once
you're finished. No sequence access can happen on a reader that is no longer
open. Closing once you're finished is just good resource use.

To help with smaller calls to the data a `with-2bit-file` macro is made
available. For example:

```lisp
2BIT> (with-2bit-file (human "hg38.2bit")
        (format t "~S" (bases (seq human "chr1") 10000 10010)))
"taaccctaac"
NIL
2BIT>
```

This takes care of opening the reader for you, and closing it again
afterwards.

[//]: # (README.md ends here)
