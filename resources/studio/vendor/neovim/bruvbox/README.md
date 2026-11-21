**STILL VERY WIP**

Despite what you may think, Bruvbox is heavily inspired by - and in fact,
forked from - [Gruvbox][].
Turns out the Gruvbox I love ain't the original, but a massively
desaturated version courtesy of perma-cranked iTerm minimum contrast. With
the entire web similarly washed out (an aside of the excellent
[Deluminate][] inversion extension) I never noticed. Apart from that,
needs more blue.

Same retro feel, partly overlapping hues, but that red-yellow-orange dominance
flipped towards shades of blue, moss green, red, and bleached yellow and green.
Truecolor gets properly leveraged through a six-shade palette for each
color, rounded out with 20 shades of gray.
Such overkill wankery lets every color type be appropriately used no matter 
the background luminance:
Very faint text background let diffs retain original syntax highlight while
still clearly differentiating + and -, makes it easy to spot the exact column
of that linter error, even in a terminal, without looking like shit. 
And subtly raising the background of important syntax keywords makes them stand
out without compromising the foreground or resorting to loud colors.


* Multiple levels of "non-active" area background, giving a rare glue and
	coherency for non-buffer elements without losing separation from the actual
	buffer.
	

Ordinary syntax also makes heavy use of subtle color and style variations to
differentiate grouped syntax items, such as conditionals and loops.

This means clarity can be retained, or even furthered, even though the colors
are nicely faded and contrast low.



For the full experience out of the box, you will need
--------------
* A terminal (and Vim) setup allowing 24 bit color
* DevIcons (including powerline, etc)
* Airline
* that fade inactive pane plug

Recommended:
--------------
* [NeoVim][]
* [Fira Code][] or similar ligature-supporting font
* No, or just a hint of, transparency. Heavy transparency will probably work
	better with a higher-contrast theme than this one...
* Enabling the full theme including symbols and glyphs.
* If using iTerm or other terminal with similar features, go easy on the 
	"minimum contrast" and "cursor guide" options.  If you're like me and can't 
	live without them in your shell, wrap your vim invocation in a profile switch
* NERDTree, Startify
* [tmux][]


* line numbers auto change color
* Focus on maintaining look across languages

The code is also modified, and there are _many_ new features and customizations available - much more is exposed through variables.

Little attention has been paid to the light variant as of yet.

   [gruvbox]: https://github.com/morhetz/gruvbox
	 [Deluminate]: https://github.com/abstiles/deluminate

Attention
---------

1. [Read this first](https://github.com/morhetz/gruvbox/wiki/Terminal-specific)

Screenshots
-----------

Yes, fix

### Dark mode

Beest

### Light mode

Shitty

### Airline theme

Yes

Palette
-------

### Dark mode

![Palette Dark]()

### Light mode

![Palette Light]()


Contrast options
----------------

One of the things dropped for now, might return. Plan is a full scale instead of discrete steps.

Documentation
-------------


Features
--------

* Looks fine as hell
* Most existing Gruvbox options and customizations
* Lots of further options, all with thought-out defaults predefined
* Airline theme much extended
* Palette totals 6 shades of each color, and 16 dark/gray shades. All are fully
	exported not just as predifined highlight groups, but individually as well,
	so that extending functionality or making minor tweaks doesn¨t require
	digging into the code or looking up hex codes.
* Code is much more dense and (imo) easy to get an overview of

* Also available (or soon to be) for [tmux][] and [Fish shell][], as approximate 256color versions for [ranger][], [highlight][],
and (basic variant) as terminal colors for [iTerm2][]
		Fix when finished: grc, 

* Extended filetype highlighting: Html, Xml, Vim (and ES6 with [yajs.vim](https://github.com/othree/yajs.vim)), Clojure, C, Python, JavaScript, CoffeeScript, Ruby, Objective-C, Go, Lua, MoonScript, Java, Markdown, Haskell
* Supported plugins, from Gruvbox: [EasyMotion][], [vim-sneak][], [Indent Guides][], [indentLine][], [Rainbow Parentheses][], [Airline][], [Lightline][], [GitGutter][], [Signify][], [ShowMarks][], [Signature][], [Syntastic][], [CtrlP][], [Startify][]
* New: [Lista][], [Metabuffer][], [ALE][], [Undotree][], [HighlightYankedregion][], [NERDTree][]
* Improved: GitGutter, vim-sneak, EasyMotion


   [EasyMotion]: https://github.com/Lokaltog/vim-easymotion
   [vim-sneak]: https://github.com/justinmk/vim-sneak
   [Indent Guides]: https://github.com/nathanaelkane/vim-indent-guides
   [indentLine]: https://github.com/Yggdroot/indentLine
   [Rainbow Parentheses]: https://github.com/kien/rainbow_parentheses.vim
   [Airline]: https://github.com/bling/vim-airline
   [Lightline]: https://github.com/itchyny/lightline.vim
   [GitGutter]: https://github.com/airblade/vim-gitgutter
   [Signify]: https://github.com/mhinz/vim-signify
   [ShowMarks]: http://www.vim.org/scripts/script.php?script_id=152
   [Signature]: https://github.com/kshenoy/vim-signature
   [Syntastic]: https://github.com/scrooloose/syntastic
   [CtrlP]: https://github.com/kien/ctrlp.vim
   [Startify]: https://github.com/mhinz/vim-startify


Known shortcomings
-------------
Vim-related:
* CursorLine gets overruled by IndentGuides, which (I find) actually makes
	things easier to see, but breaks the coherent look a bit. 
	In the same vein, underlined CursorLine (in insert mode) does not get cut off
	by ColorColumn, and is shown in all windows, not just the active one. 
	That is of course true for the CursorLine in general, but the background 
	dimming hides it nicely.
* Search text fg, in 'inactive' areas (other windows, past ColorColumn etc)
	is black instead of white, ie hard to read on a default dark blue bg.
	Gives a rather nice effect, and usually not an issue, but not ideal when one
	has multiple search matches.


Contributions
-------------

Welcome

ToDo
----
* Clean up enough for others to use
* Complete readme


* HIGH++ contrast mode for dark mode outdoors

* More plugs - SemanticHighlight, proper for Rainbow, NERDTree git plugin, DevIcons (first just Coldevicons palette then properly?)
General helper plug doing all the little things a colorscheme really really oughtn't get involved with,
but that's still needed for the proper "skin" experience. Like: implement the auto ColorColumn-based fade instead of relying on external plug...



License
-------
[MIT/X11][]

   [MIT/X11]: https://en.wikipedia.org/wiki/MIT_License
