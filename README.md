<p align="center">
<img src="beatn1k.png" width=400>
</p>

# beatn1k

beatn1k is a program that creates interesting text using the [fold-up and cut-up](https://en.wikipedia.org/wiki/Cut-up_technique) techniques. In case you haven't already clicked on that link, this is a technique that William S. Burroughs used to write weird books. Dadaists pioneered cut-ups to write weird, but interesting poems (see [How to Make a Dadaist Poem](https://www.writing.upenn.edu/~afilreis/88v/tzara.html)). 

The text that it creates can be weird and interesting. There is no other reason for this, other than for fun (and as a fun project for me to play with ReasonML), and to stare at the wonders of randomness.

If you just want to read some interesting text, head over to [beatn1k.com](https://www.beatn1k.com/). One new fold-up and one new cut-up is posted every 12 hours (or whenever I want to generate new text otherwise).

## How does it work?

Give `beatn1k` two URLs for online texts and a number of words per line, and beatn1k will pull the text and work its magic. 

For instance:

```
the things we  magnitude when
In 1902 this only animal most instance. Problem solved.
a rational explanation, the free exercise
that this was this approach has   an unusually
be directed environment, something like notice, intervening
understand how it
with many vaguely was no longer more, fish universe is made
why fun the hoary molecules to self-organize
or puppies, but that I don’t  what sort of it traced in don’t
playing? the air freely—if
are, so to of immediately recoil at appears to be,
```

Weird is cool!

### Build it

You will need dune and [opam](https://opam.ocaml.org/) and [dune](https://dune.build/). From the root directory of the project simply run `dune build src/beatn1k.exe`. You should now have am executable under `_build/default/src/beatn1k.exe`. 

### Use it

You can either get the text in the command line or saved it as HTML using the `-w` or `--web` flag. The default logic will choose random sources from [web/sources.txt](web/sources.txt). You can generate either a fold-up using `-m fold` or a cut-up using `-m cut`. Other options can be seen by using `--help` (output shown below).

```shell
BEATN1K(1)                      Beatn1k Manual                      BEATN1K(1)


NAME
       beatn1k - beatn1k is an application that creates weird dadaist and beat
       poems from URLs

SYNOPSIS
       beatn1k [OPTION]…

OPTIONS
       -c CUT_SIZE, --cuts=CUT_SIZE (absent=4)
           Size of word word cut for cut-ups

       -l LINE_SIZE, --line=LINE_SIZE (absent=10)
           Size of each line

       -m MODE, --mode=MODE (absent=cut)
           Mode can be cut (for cut-up) or fold (for fold-ups

       -s STANZA_SIZE, --stanza=STANZA_SIZE (absent=12)
           Size of stanza

       --url_1=URL_1, --u1=URL_1
           First for cut-ups, source for left side of fold-ups

       --url_2=URL_2, --u2=URL_2
           Source for right side of fold-ups

       -w, --web
           Web mode
```
