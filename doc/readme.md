Instructions for Building All Documentation
===========================================

Do this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You'll need [Pandoc][] though.

[Pandoc]: http://johnmacfarlane.net/pandoc/

Getting Pandoc Ready for Your Shit
----------------------------------

> *Install pandoc and its bullshit dependencies.*

Installing Pandoc is a bitch. I like you, so I wrote up some instructions to make it easy for you. If you follow these exactly, you should be okay if you're on a Mac. If you're not on a Mac, you're fucked. *

First, install MacTex:

    $ brew tap phinze/cask
    $ brew install brew-cask
    $ brew cask install mactex

Add `/usr/texbin` to your `PATH` environment variable. In this version, `pdflatex` is broken, so go to your Applications/TeX directory and run FixMaxTeX2013.pkg.

Also, add `texbin` to your path:

    $ sudo ln -s /usr/local/texlive/2014/bin/x86_64-darwin /usr/bin/texbin

Now you're ready to install `pandoc`:

    $ brew update
    $ brew install pandoc

Note that `pandoc` will work in a limited fashion by itself, but the other shit we install is so it can also do pretty things and LaTeX things.

Finally, all you need to do to make a pretty PDF from a markdown file (for example, in.md) is:

    $ pandoc in.md -o out.pdf

You're welcome.

[stack exchange link]: http://tex.stackexchange.com/questions/163849

* Actually, if you're on linux, you're not fucked.
--------------------------------------------------

If you're on Debian/Ubuntu, just run:

    $ sudo apt-get install texlive
    $ cabal install pandoc
    $ sudo ln -s /home/{your username}/.cabal/bin/pandoc /usr/bin/pandoc

Waay easier than on Mac. Fuck Mac.
