scriptencoding utf-8
" -----------------------------------------------------------------------------
" File: colors/bruvbox.vim
" Description: Faded groove color schenable mobilebackupseme for Vim. Shamelessly forked from Gruvbox.
" Author: Bruvbox by @tolgraven.    Gruvbox by morhetz <morhetz@gmail.com>
" Source: https://github.com/tolgraven/bruvbox
" Last Modified: Apr 2017
" -----------------------------------------------------------------------------
" Supporting code -------------------------------------------------------------
"{{{1         Initialisation:
hi clear
if exists('syntax_on')
  syntax reset
endif
let g:colors_name='bruvbox'
if !has('gui_running') && &t_Co != 256  | finish   | endif

"{{{1         Global Settings:
let g:bruvbox_bold      = get(g:, 'bruvbox_bold',      1)
let g:bruvbox_undercurl = get(g:, 'bruvbox_undercurl', 1)
let g:bruvbox_underline = get(g:, 'bruvbox_underline', 1)
let g:bruvbox_inverse   = get(g:, 'bruvbox_inverse  ', 1)
if !exists('g:bruvbox_italic')
  if has('gui_running') || $TERM_ITALICS ==? 'true'
    let      g:bruvbox_italic   = 1    "also check for like tmux way of enabling, tho...
  else | let g:bruvbox_italic   = 0     | endif
endif
if !exists('g:bruvbox_guisp_fallback') || index(['fg', 'bg'], g:bruvbox_guisp_fallback) == -1
  let g:bruvbox_guisp_fallback='NONE'
endif

let g:bruvbox_termcolors           = get(g:, 'bruvbox_termcolors', 256)
let g:bruvbox_invert_indent_guides = get(g:, 'bruvbox_invert_indent_guides', 0)
let g:bruvbox_contrast_dark        = get(g:, 'bruvbox_contrast_dark', 'medium')
let g:bruvbox_contrast_light       = get(g:, 'bruvbox_contrast_light', 'medium')
let s:is_dark=(&background ==# 'dark')  " works, but leads to first initing light if bg not set before colorscheme in vimrc
" let s:is_dark = 1

"{{{1         Palette:
let s:bb = {}  " setup palette dictionary
"{{{        NOTES
"IDEA: instead of bg/base gray, turn it (so bg, also comments, etc) ever so faintly bluish hue!! could be shit but suspect might be very pretty?
"PER "start with purely faded gruvbox, before tweaking further":
"BRUVBOX:	bg				  red 			green 		yellow 		blue 			purple 		aqua 			gray
"normal 	#393939 	#a4ad49 	#939233 	#c6943a 	#4d8487 	#a56784 	#6b976e
"bright 						    #cb5d4e 	#aaae4a 	#d8ac67 	#829d93 	#c38495 	#8bb47d
"    				bg0_h			  bg0 			bg1 			bg2 			bg3 			bg4 			gray 			orange
"bg 	  		#1d2821  	#282828 	#3c3836 	#505945 	#665c54 	#7c6f64 	#928374 	#c5632c
"fg 					#32302f 	 #a89984 	#bdae93 	#d5c4a1 	#ebdbb2 	#fbf1c7 	#d68035

"GRUVBOX: bg				red 			green 		yellow 		blue 			purple 		aqua 			gray
"normal 	#282828 	#cc241d 	#b8bb26
"bright
" 				bg0_h			bg0 			bg1 			bg2 			bg3 			bg4 			gray 			orange
"bg 			#1d2821  	#282828 	#3c3836 	#505945 	#665c54 	#7c6f64 	#928374 	#d68035d0e
"fg 								#32302f 	#a89984 	#bdae93 	#d5c4a1 	#ebdbb2 	#fbf1c7 	#fe8019
"}}}
" {{{2         DARK / LIGHT
let s:bb.black       = ['#111111', 233]     "
let s:bb.black2      = ['#151414', 233]     "

" need more variants down here lol.
let s:bb.dark0_hard  = ['#191818', 234]     "
let s:bb.dark0_hard2 = ['#232222', 234]     "
let s:bb.dark0       = ['#262525', 235]     " 282828 is orig gb dark0
let s:bb.dark0_medium= ['#2a292b', 236]     "
let s:bb.dark0_soft  = ['#2d3030', 236]     "

let s:bb.dark1_hard  = ['#34363a', 237]     "
let s:bb.dark1       = ['#3a393d', 237]     "
let s:bb.dark1_soft  = ['#3d3f42', 238]     "
let s:bb.dark2_hard  = ['#42464c', 239]     "
let s:bb.dark2       = ['#46494c', 239]     "
let s:bb.dark2_soft  = ['#515458', 240]     "
let s:bb.dark3       = ['#605c57', 241]     "
let s:bb.dark4       = ['#7a6f69', 243]     "

let s:bb.dark4_256   = ['#7c6f64', 243]     "
let s:bb.gray_244    = ['#908682', 244]     "
let s:bb.gray_245    = ['#90868a', 245]     "
let s:bb.gray_248    = ['#8a8e8f', 248]     "


let s:bb.white       = ['#fdfbe8', 255]

let s:bb.light0_hard2= ['#faf8e0', 255]     "
let s:bb.light0_hard = ['#f9f1d9', 255]     "
let s:bb.light0      = ['#faecc9', 254]     "
let s:bb.light0_medium=['#f8e8bf', 253]     "
let s:bb.light0_soft = ['#f0e3ba', 253]     "

let s:bb.light1_hard = ['#efddb5', 252]     "
let s:bb.light1      = ['#ebdbb2', 252]     "
let s:bb.light1_soft = ['#e4d5a9', 252]     "
let s:bb.light2_hard = ['#dccca9', 250]     "
let s:bb.light2      = ['#d5c4a1', 250]     "
let s:bb.light2_soft = ['#c6bc9b', 250]     "
let s:bb.light3      = ['#bdae93', 248]     "
let s:bb.light4      = ['#a89984', 246]     "

let s:bb.light4_256  = ['#a89984', 246]     "
let s:bb.bright_gray = ['#af9f91', 244]     "
"}}}
" XXX instead of contrast hi/mid/low have 1-50-100 scale for saturation and contrast! calc bounds and math around
" let s:bb.bright_red       = ['#c55d4e', 167]     " #a4ad49 aint rea lol... errorrr.  #d33a27 or so is gruv orig
" let s:bb.bright_red       = ['#CD6D5F', 167]
" let s:bb.bright_red       = ['#d17669', 167]
let s:bb.bright_red       = ['#d18479', 167]  
" let s:bb.bright_green     = ['#a0a172', 142]
let s:bb.bright_green     = ['#b4b88d', 142]  
let s:bb.bright_yellow    = ['#dabd7a', 214]  
" let s:bb.bright_blue      = ['#528895', 109]
" let s:bb.bright_blue      = ['#589aa7', 109]
let s:bb.bright_blue      = ['#7692ab', 109]  
let s:bb.bright_blue_alt  = ['#6995b6', 109]  
" let s:bb.bright_purple    = ['#a58a99', 175]
let s:bb.bright_purple    = ['#bd979d', 175]  
" let s:bb.bright_aqua      = ['#6e916e', 108]
let s:bb.bright_aqua      = ['#91b8a4', 108]  
let s:bb.bright_orange    = ['#cca687', 208]  

let s:bb.neutral_red      = ['#d76b69', 124] "
let s:bb.neutral_green    = ['#919545', 106] "
let s:bb.neutral_yellow   = ['#d9a961', 172] "
let s:bb.neutral_blue     = ['#458588', 66]  "
let s:bb.neutral_blue_alt = ['#4d8487', 66]  "
let s:bb.neutral_purple   = ['#a38392', 132] "
let s:bb.neutral_aqua     = ['#70907a', 72]  "
let s:bb.neutral_orange   = ['#d2886d', 166] "
" used for light mode, repurpose:
let s:bb.faded_red        = ['#975250', 88]  "
let s:bb.faded_green      = ['#79743e', 100] "
let s:bb.faded_yellow     = ['#957644', 136] "
let s:bb.faded_blue       = ['#6b768b', 24]  "
let s:bb.faded_purple     = ['#7f6f7f', 96]  "
let s:bb.faded_aqua       = ['#3a5853', 66]  "
let s:bb.faded_orange     = ['#99613c', 130] "
" bright, desat
let s:bb.special_red      = ['#af7370', 124] "
let s:bb.special_green    = ['#8b8c63', 10]  "
let s:bb.special_yellow   = ['#d0af80', 11]  "
let s:bb.special_blue     = ['#80a0b3', 111] "
let s:bb.special_purple   = ['#af8589', 172] "
let s:bb.special_aqua     = ['#9ab499', 172] "
let s:bb.special_orange   = ['#dba184', 172] "
" very dark colors, only for use as background and with light text (no matter what mode)
" prob slghtly overkill...
let s:bb.bg_dark_red      = ['#281913', 88]  "
let s:bb.bg_dark_green    = ['#1f281c', 100] "
let s:bb.bg_dark_yellow   = ['#2f2b19', 136] "
let s:bb.bg_dark_blue     = ['#172533', 24]  "
let s:bb.bg_dark_purple   = ['#342225', 96]  "
let s:bb.bg_dark_aqua     = ['#1c2f29', 66]  "
let s:bb.bg_dark_orange   = ['#3c2d1f', 130] "
" let s:bb.bg_dark_red      = ['#321513', 88]  "
" let s:bb.bg_dark_green    = ['#30362a', 100] "
" let s:bb.bg_dark_yellow   = ['#392f25', 136] "
" let s:bb.bg_dark_blue     = ['#2c3a52', 24]  "
" let s:bb.bg_dark_purple   = ['#4c3a3c', 96]  "
" let s:bb.bg_dark_aqua     = ['#30423b', 66]  "
" let s:bb.bg_dark_orange   = ['#372512', 130] "
" super faint tingle of a hue...
let s:bb.bg_light_red     = ['#ea9a98', 124]  
let s:bb.bg_light_green   = ['#ccdaa8', 10]   
let s:bb.bg_light_yellow  = ['#d9c894', 11]   
let s:bb.bg_light_blue    = ['#a6c0cd', 111]  
let s:bb.bg_light_purple  = ['#d9b7b7', 172]  
let s:bb.bg_light_aqua    = ['#B6D1B4', 172]  
let s:bb.bg_light_orange  = ['#d9a58a', 172]  
" more color talk  #86AB89 gre #AEC0A1 vlight gr / turqu   {{{
" so the eerily spot on accidental colors were:
" #84825A light(?) gree   nice desat... maybe fade a tiny bit more and will be nice for strings
" #D7875F really good creamy orangey
" #AF8786 faded purp - tho think that one's defined like that? - nope gruvbox is #a76783
" #D7AF60 Seems almost perfect. faded yello - or is it the same as gruvbox? -
" so bright is for dark bg, faded for light But we should use all groups, whatever mode, for inverted text etc....
"
" gruvbox 256 ble vs aqu bug: super close colors. Ble already has lots of gree in it but theyre not remotely this close...
" #7e9bd0    my color #4 shows > #80a0b3  110 might be alright actually. 109 is too gree and 111 maybe bit too ble/bright, but eh
" #799ca1 my tmux statusline

" #7b9ec3 bubblegum airline insert
" #536E97 dark blu.fish #2C3B8A other values depending on where in antialiasing looking...
" #5579E3
" #314061 nice deep dark blu #49838D nice medium ble  #ACCFBD turquois / lightlight ble:
" #5f8787 gruvboxes basic I guess...
" #6D7833 #7f9d4a   green candidates
" Green looks nice but doesnt contrast enough with comments. Maybe Need a bit more brightness in the color, but def not more saturation.
" Aqua is too strong
" Red prob tiny bit too strong
" Orange def tiiiiny bit too strong (and too similar to red)
 "}}}


"{{{1         Glyphs:
let s:bb.glyph={}
let s:bb.glyph.arrow_opened_fill    ='▾'    "for nerdtree, tagbar and similar
let s:bb.glyph.arrow_closed_fill    ='▸'
let s:bb.glyph.arrow_opened_lined   =''
let s:bb.glyph.arrow_closed_lined   =' '   "''
let s:bb.glyph.arrow_dense          ='➧'    "any point to these?
let s:bb.glyph.arrow_heavy          ='➨'
let s:bb.glyph.pipe                 ='│'
let s:bb.glyph.pinned               =''    "font awesome? for taboo, renamed tabs
let s:bb.glyph.branch               =''
let s:bb.glyph.warning              =''
let s:bb.glyph.error                =''
let s:bb.glyph.modified             ='◈'
let s:bb.glyph.linenr               =''
let s:bb.glyph.whitespace           ='☲'
let s:bb.glyph.paste                ='ρ'       "curr 'PASTE'
let s:bb.glyph.spell                ='Ꞩ'       " 'SPELL'

let s:bb.glyph.recycle              =' '
let s:bb.glyph.again                ='↺'
let s:bb.glyph.back                 ='⤸'
let s:bb.glyph.arrow_circled        ='➲'

let s:bb.glyph.vim                  =''
let s:bb.glyph.file                 =''

let s:bb.glyph.separator_arrow_fill          =''
let s:bb.glyph.separator_arrow_fill_back     =''
let s:bb.glyph.separator_triangle_left_above =''
let s:bb.glyph.separator_triangle_left_below =''
let s:bb.glyph.separator_triangle_right_above=''
let s:bb.glyph.separator_triangle_right_below=''
let s:bb.glyph.separator_slash               =''
let s:bb.glyph.separator_slash_back          =''
let s:bb.glyph.separator_arrow_lined         =''
let s:bb.glyph.separator_arrow_lined_back    =''

" let g:airline_left_sep 														=''    "  vs ⮀ ''
" let g:airline_left_alt_sep 												=' '    "'│' 	
" let g:airline_right_sep 													=''     "''
" let g:airline_right_alt_sep 											=''     "'│'


" signs:
let s:bb.glyph.sign_added           ='✚'
let s:bb.glyph.sign_changed         ='✹'
let s:bb.glyph.sign_deleted         =''    "XXX find better
let s:bb.glyph.sign_changed_deleted =''  "is weird on kitty
let s:bb.glyph.sign_warning         =s:bb.glyph.warning
let s:bb.glyph.sign_error           =s:bb.glyph.error

let s:bb.glyph.type_buffer          =''    " ℬ  b B
let s:bb.glyph.type_tab             =''    "no but srsly find something usable
" let s:bb.glyph.type_window          =''
let s:bb.glyph.type_terminal        =''

let s:bb.glyph.mode_normal          ='N'
let s:bb.glyph.mode_insert          =''
let s:bb.glyph.mode_linewise        =''
let s:bb.glyph.mode_colwise         ='▢'

let s:bb.glyph.source_buffer        =s:bb.glyph.type_buffer
let s:bb.glyph.source_around        =''    "''
let s:bb.glyph.source_web           =''
let s:bb.glyph.source_vim           =s:bb.glyph.vim
let s:bb.glyph.source_path          ='./'.s:bb.glyph.file      "''
let s:bb.glyph.source_omni          =''
let s:bb.glyph.source_jedi          =''
let s:bb.glyph.source_tern          =''
let s:bb.glyph.source_tag           =''
let s:bb.glyph.source_snippet       =''
let s:bb.glyph.source_syntax        ='♯'

let g:bruvbox_glyph     = s:bb.glyph
let g:bruvbox_internal  = s:bb

"{{{1         Setup Recommended Glyphs:
" if s:bruvbox_use_recommended_glyphs
" etc etc

"{{{1         Setup Recommended Settings:
" if s:bruvbox_use_recommended_settings
" etc etc


" let g:taboo_renamed_tab_format      =' %l%m'


"{{{1         Setup Emphasis:
" should be opposite lol no
let s:bold      = (g:bruvbox_bold      ? 'bold,'      : '')
let s:italic    = (g:bruvbox_italic    ? 'italic,'    : '')
let s:underline = (g:bruvbox_underline ? 'underline,' : '')
let s:undercurl = (g:bruvbox_undercurl ? 'undercurl,' : '')
let s:inverse   = (g:bruvbox_inverse   ? 'inverse,'   : '')
" let s:inverse   = (g:bruvbox_inverse   ? 'reverse,'   : '')
"{{{1         Setup Colors:
let s:vim_bg = ['bg', 'bg']     | let s:vim_fg = ['fg', 'fg']  | let s:none = ['NONE', 'NONE']

if s:is_dark  " determine relative colors
  " if g:bruvbox_contrast_dark     ==? 'soft'  | let s:bg0 = s:bb.dark0_soft
  " elseif g:bruvbox_contrast_dark ==? 'hard'  | let s:bg0 = s:bb.dark0_hard | endif
  let s:bg_hard = s:bb.black
  let s:fg_hard = s:bb.white

  let s:bg0_h2 = s:bb.dark0_hard2
  let s:bg0  = s:bb.dark0 | let s:bg0_s = s:bb.dark0_soft    | let s:bg0_m = s:bb.dark0_medium     | let s:bg0_h  = s:bb.dark0_hard
  let s:bg1  = s:bb.dark1 | let s:bg1_s = s:bb.dark1_soft    | let s:bg1_h = s:bb.dark1_hard
  let s:bg2  = s:bb.dark2 | let s:bg2_s = s:bb.dark2_soft    | let s:bg2_h = s:bb.dark2_hard
  let s:bg3  = s:bb.dark3 | let s:bg4   = s:bb.dark4

  let s:gray = s:bb.gray_244 | let s:gray2 = s:bb.gray_248

  let s:fg0  = s:bb.light0 | let s:fg0_s   = s:bb.light0_soft  | let s:fg0_m = s:bb.light0_medium  | let s:fg0_h = s:bb.light0_hard
  let s:fg1  = s:bb.light1 | let s:fg1_s = s:bb.light1_soft | let s:fg1_h = s:bb.light1_hard
  let s:fg2  = s:bb.light2 | let s:fg2_s = s:bb.light2_soft | let s:fg2_h = s:bb.light2_hard
  let s:fg3  = s:bb.light3 | let s:fg4   = s:bb.light4

  let s:fg4_256 = s:bb.light4_256

  let s:red    = s:bb.bright_red
  let s:green  = s:bb.bright_green
  let s:yellow = s:bb.bright_yellow "s:bb.special_yellow
  let s:blue   = s:bb.bright_blue
  let s:purple = s:bb.bright_purple "special_purple
  let s:aqua   = s:bb.bright_aqua
  let s:orange = s:bb.bright_orange

  let s:bg_red    = s:bb.bg_dark_red
  let s:bg_green  = s:bb.bg_dark_green
  let s:bg_yellow = s:bb.bg_dark_yellow
  let s:bg_blue   = s:bb.bg_dark_blue
  let s:bg_purple = s:bb.bg_dark_purple
  let s:bg_aqua   = s:bb.bg_dark_aqua
  let s:bg_orange = s:bb.bg_dark_orange

  let s:hue_red    = s:bb.bg_light_red
  let s:hue_green  = s:bb.bg_light_green
  let s:hue_yellow = s:bb.bg_light_yellow
  let s:hue_blue   = s:bb.bg_light_blue
  let s:hue_purple = s:bb.bg_light_purple
  let s:hue_aqua   = s:bb.bg_light_aqua
  let s:hue_orange = s:bb.bg_light_orange

  let s:search_background = s:bb.faded_blue

else "{{{     LIGHT THEME
  " if     g:bruvbox_contrast_light ==? 'soft' | let s:bg0  = s:bb.light0_soft
  " elseif g:bruvbox_contrast_light ==? 'hard' | let s:bg0  = s:bb.light0_hard | endif
  let s:bg_hard = s:bb.white
  let s:fg_hard = s:bb.black

  let s:bg0  = s:bb.light0 | let s:bg0_s = s:bb.light0_soft | let s:bg0_m = s:bb.light0_medium | let s:bg0_h = s:bb.light0_hard
  " XXX fix actual shades for these
  let s:bg0_h2 = s:bb.light0_hard
  let s:bg1  = s:bb.light1 | let s:bg1_s  = s:bb.light1 | let s:bg1_h  = s:bb.light1 |
  let s:bg2  = s:bb.light2 | let s:bg2_s  = s:bb.light2 | let s:bg2_h  = s:bb.light2 |
  let s:bg3  = s:bb.light3 | let s:bg4    = s:bb.light4

  let s:gray = s:bb.gray_244 | let s:gray2 = s:bb.gray_248

  let s:fg0 = s:bb.dark0 | let s:fg0_s = s:bb.dark0_soft | let s:fg0_m = s:bb.dark0_medium | let s:fg0_h = s:bb.dark0_hard
  let s:fg1 = s:bb.dark1 | let s:fg1_s = s:bb.dark1_soft | let s:fg1_h = s:bb.dark1_hard
  let s:fg2 = s:bb.dark2 | let s:fg2_s = s:bb.dark2_soft | let s:fg2_h = s:bb.dark2_hard
  let s:fg3 = s:bb.dark3 | let s:fg4 = s:bb.dark4

  let s:fg4_256 = s:bb.dark4_256

  let s:red    = s:bb.faded_red
  let s:green  = s:bb.faded_green
  let s:yellow = s:bb.faded_yellow
  let s:blue   = s:bb.faded_blue
  let s:purple = s:bb.faded_purple
  let s:aqua   = s:bb.faded_aqua
  let s:orange = s:bb.faded_orange

  let s:bg_red    = s:bb.bg_light_red
  let s:bg_green  = s:bb.bg_light_green
  let s:bg_yellow = s:bb.bg_light_yellow
  let s:bg_blue   = s:bb.bg_light_blue
  let s:bg_purple = s:bb.bg_light_purple
  let s:bg_aqua   = s:bb.bg_light_aqua
  let s:bg_orange = s:bb.bg_light_orange

  let s:hue_red    = s:bb.bg_dark_red
  let s:hue_green  = s:bb.bg_dark_green
  let s:hue_yellow = s:bb.bg_dark_yellow
  let s:hue_blue   = s:bb.bg_dark_blue
  let s:hue_purple = s:bb.bg_dark_purple
  let s:hue_aqua   = s:bb.bg_dark_aqua
  let s:hue_orange = s:bb.bg_dark_orange

  let s:search_background = s:bb.special_blue

endif "}}}

let s:special_red    = s:bb.special_red
let s:special_green  = s:bb.special_green
let s:special_yellow = s:bb.special_yellow
let s:special_blue   = s:bb.special_blue
let s:special_purple = s:bb.special_purple
let s:special_aqua   = s:bb.special_aqua
let s:special_orange = s:bb.special_orange

let s:neutral_red    = s:bb.neutral_red
let s:neutral_green  = s:bb.neutral_green
let s:neutral_yellow = s:bb.neutral_yellow
let s:neutral_blue   = s:bb.neutral_blue
let s:neutral_purple = s:bb.neutral_purple
let s:neutral_aqua   = s:bb.neutral_aqua
let s:neutral_orange = s:bb.neutral_orange

" set up 16 colors terminal fallback {{{
if g:bruvbox_termcolors == 16
  let s:bg0[1]    = 0
  let s:fg4[1]    = 7
  let s:gray[1]   = 8
  let s:red[1]    = 9
  let s:green[1]  = 10
  let s:yellow[1] = 11
  let s:blue[1]   = 12
  let s:purple[1] = 13
  let s:aqua[1]   = 14
  let s:fg1[1]    = 15
endif
"}}}
" save current relative colors back to palette dictionary {{{
let s:bb.bg_hard = s:bg_hard
let s:bb.bg0 = s:bg0 | let s:bb.bg0_s = s:bg0_s | let s:bb.bg0_m = s:bg0_m | let s:bb.bg0_h = s:bg0_h
let s:bb.bg1 = s:bg1 | let s:bb.bg1_h = s:bg1_h
let s:bb.bg2 = s:bg2 | let s:bb.bg2_h = s:bg2_h
let s:bb.bg3 = s:bg3 | let s:bb.bg4   = s:bg4

let s:bb.gray = s:gray | let s:bb.gray2 = s:gray2

let s:bb.fg_hard = s:fg_hard
let s:bb.fg0 = s:fg0 | let s:bb.fg0_s = s:fg0 | let s:bb.fg0_m = s:fg0 | let s:bb.fg0_h = s:fg0
let s:bb.fg1 = s:fg1 | let s:bb.fg2   = s:fg2 | let s:bb.fg3   = s:fg3 | let s:bb.fg4   = s:fg4

let s:bb.fg4_256 = s:fg4_256

let s:bb.red    = s:red
let s:bb.green  = s:green
let s:bb.yellow = s:yellow
let s:bb.blue   = s:blue
let s:bb.purple = s:purple
let s:bb.aqua   = s:aqua
let s:bb.orange = s:orange
"}}}

"{{{1           Export palette as global var, for the enjoyment of the public
let g:bruvbox_palette = s:bb
" i guess so we can hook into our colors backwards from vimrc/other scripts instead of just going from origin, out
" cool thing actually if going all out bloat, and thinking grander...  would be sourcing our palette from a file in the first place, and if user
" modifies global var, save that to like a ~/.config/bruvbox type ting, and offer to source palette from there instead next time. Full dynamic + then easy to adapt to other envs
"}}}

"{{{1         Overload Setting:
let s:hls_cursor           = get(s:bb, get(g:, 'bruvbox_hls_cursor',     'faded_blue'))
" let s:search_background    = get(s:bb, get(g:, 'bruvbox_search_background', 'faded_blue'))
let s:cursorline_bg        = get(s:,   get(g:, 'bruvbox_cursorline_bg',  'bg0_s')) " cline and ccolumn have been 0_s but tiring
let s:underline_cursorline =           get(g:, 'bruvbox_underline_cursorline',  0)
" trad colors for these were: h, s, s, s
let s:number_column        = get(s:,   get(g:, 'bruvbox_number_column',  'bg0_s'))
let s:color_column         = get(s:,   get(g:, 'bruvbox_color_column',   'bg0'))
let s:sign_column          = get(s:,   get(g:, 'bruvbox_sign_column',    'bg0_s'))
let s:fold_column          = get(s:,   get(g:, 'bruvbox_fold_column',    'bg0_h'))
if get(g:, 'gitgutter_override_sign_column_highlight', 0) == 1
  let s:sign_column = s:number_column  | let g:gitgutter_override_sign_column_highlight = 0 | endif
let s:vert_split           = get(s:,   get(g:, 'bruvbox_vert_split',     'bg0_h'))  "bg1


let s:invert_signs         = (get(g:, 'bruvbox_invert_signs',     0) == 1 ? s:inverse  : '')
let s:invert_selection     = (get(g:, 'bruvbox_invert_selection', 1) == 1 ? s:inverse  : '')
" let s:invert_search        = (get(g:, 'bruvbox_invert_search',    1) == 1 ? s:inverse  : '')
let s:invert_tabline       = (get(g:, 'bruvbox_invert_tabline',   0) == 1 ? s:inverse  : '')
let s:invert_errors        = (get(g:, 'bruvbox_invert_errors',    0) == 1 ? s:inverse  : '')
"for motion/search/jump plugs
let s:invert_targets       =  get(g:, 'bruvbox_invert_targets_when_rest_faded',  0)
let s:italicize_comments   = (get(g:, 'bruvbox_italicize_comments', 1) == 1 ? s:italic : '')
let s:italicize_strings    = (get(g:, 'bruvbox_italicize_strings',  0) == 1 ? s:italic : '')

" not used yet
let s:color_mode_normal    = get(g:, 'bruvbox_normal_mode_color',     s:green)
let s:color_mode_insert    = get(g:, 'bruvbox_insert_mode_color',     s:red)
let s:color_mode_visual    = get(g:, 'bruvbox_visual_mode_color',     s:purple)  "purple sign actually... iz same?
let s:replace_mode_color   = get(g:, 'bruvbox_replace_mode_color',    s:yellow)  "yellow sign, dito

" }}}
"{{{1         Highlighting Function:

function! s:HL(group, fg, ...)  " Arguments: group, guifg, guibg, gui, guisp
  let fg = a:fg
  if a:0 >= 1                | let bg = a:1   | else | let bg = s:none   | endif
  if a:0 >= 2 && strlen(a:2) | let mod = a:2  | else | let mod = 'NONE,' | endif

  if a:0 >= 3
    if g:bruvbox_guisp_fallback !=? 'NONE'  | let fg = a:3          | endif
    " bg fallback mode should invert higlighting
    if g:bruvbox_guisp_fallback ==? 'bg'    | let mod .= 'inverse,' | endif
    let l:special = 'guisp=' . a:3[0]
  endif

  let hi = ['hi', a:group, 'guifg='.fg[0],   'guibg='.bg[0],   'gui='.mod[:-2],
                      \  'ctermfg='.fg[1], 'ctermbg='.bg[1], 'cterm='.mod[:-2],
                      \   get(l:, 'special', '')]
  execute join(hi, ' ')
endfunction

function! s:HLCategory(group_suffix, var_prefix, ...)
" Should work like above but only one call. eg
 call s:HLCategory('', '') "runs BruvboxRed, s:red through BruvboxOrange, s:orange
 call s:HLCategory('Bold', '', s:none, s:bold)
 call s:HLCategory('Neutral', 'neutral')
 " will have to massively redo lots tho. Focus on finishing the colorscheme then use that builder for vim,
 " develop various scripts for tmux, alacritty, iterm, fish etc?
 " just have core set of (2-3?) hues per color and some base saturations/brightnesses
 " then rest should self-generate from there, including in vim...
 
endfunction
" }}}
"{{{1         Bruvbox Hi Groups:
"{{{          FG / BG
call s:HL('BruvboxFg0',  s:fg0)  | call s:HL('BruvboxFg0_h', s:fg0_h) | call s:HL('BruvboxFg0_m', s:fg0_m) | call s:HL('BruvboxFg0_s', s:fg0_s)
call s:HL('BruvboxFg1',  s:fg1)  | call s:HL('BruvboxFg1_h', s:fg1_h)
call s:HL('BruvboxFg2',  s:fg2)  | call s:HL('BruvboxFg2_h', s:fg2_h)
call s:HL('BruvboxFg3',  s:fg3)  | call s:HL('BruvboxFg4',   s:fg4)
call s:HL('BruvboxGray', s:gray) | call s:HL('BruvboxGray2', s:gray2)
call s:HL('BruvboxBrightGray',     s:bb.bright_gray)
call s:HL('BruvboxBgHard',         s:none,        s:bg_hard)
call s:HL('BruvboxFgHard',         s:fg_hard,     s:none,           s:bold)
call s:HL('BruvboxHardOnHard',     s:fg_hard,     s:bg_hard)

call s:HL('BruvboxBg0', s:bg0)  | call s:HL('BruvboxBg0_h', s:bg0_h) | call s:HL('BruvboxBg0_m', s:bg0_m) | call s:HL('BruvboxBg0_s', s:bg0_s)
call s:HL('BruvboxBg1', s:bg1)  | call s:HL('BruvboxBg1_h', s:bg1_h) | call s:HL('BruvboxBg1_s', s:bg1_s)
call s:HL('BruvboxBg2', s:bg2)  | call s:HL('BruvboxBg2_h', s:bg2_h) | call s:HL('BruvboxBg2_s', s:bg2_s)
call s:HL('BruvboxBg3', s:bg3)  | call s:HL('BruvboxBg4',   s:bg4)
"}}}
" standard colors: for regular dark, based on 'bright' palette.
call s:HL('BruvboxRed',            s:red)
call s:HL('BruvboxGreen',          s:green)
call s:HL('BruvboxYellow',         s:yellow)
call s:HL('BruvboxBlue',           s:blue)
call s:HL('BruvboxPurple',         s:purple)
call s:HL('BruvboxAqua',           s:aqua)
call s:HL('BruvboxOrange',         s:orange)
call s:HL('BruvboxRedBold',        s:red,            s:none,             s:bold)
call s:HL('BruvboxGreenBold',      s:green,          s:none,             s:bold)
call s:HL('BruvboxYellowBold',     s:yellow,         s:none,             s:bold)
call s:HL('BruvboxBlueBold',       s:blue,           s:none,             s:bold)
call s:HL('BruvboxPurpleBold',     s:purple,         s:none,             s:bold)
call s:HL('BruvboxAquaBold',       s:aqua,           s:none,             s:bold)
call s:HL('BruvboxOrangeBold',     s:orange,         s:none,             s:bold)

call s:HL('BruvboxRedNeutral',     s:neutral_red)
call s:HL('BruvboxGreenNeutral',   s:neutral_green)
call s:HL('BruvboxYellowNeutral',  s:neutral_yellow)
call s:HL('BruvboxBlueNeutral',    s:neutral_blue)
call s:HL('BruvboxPurpleNeutral',  s:neutral_purple)
call s:HL('BruvboxAquaNeutral',    s:neutral_aqua)
call s:HL('BruvboxOrangeNeutral',  s:neutral_orange)

" will have to experiment with light or dark fg...
call s:HL('BruvboxRedFadedBg',       s:bb.light2,      s:bb.faded_red,     s:bold)
call s:HL('BruvboxGreenFadedBg',     s:bb.light2,      s:bb.faded_green,   s:bold)
call s:HL('BruvboxYellowFadedBg',    s:bb.dark0,       s:bb.faded_yellow,  s:bold)
call s:HL('BruvboxBlueFadedBg',      s:bb.light2,      s:bb.faded_blue,    s:bold)
call s:HL('BruvboxPurpleFadedBg',    s:bb.light2,      s:bb.faded_purple,  s:bold)
call s:HL('BruvboxAquaFadedBg',      s:bb.light2,      s:bb.faded_aqua,    s:bold)
call s:HL('BruvboxOrangeFadedBg',    s:bb.dark0,       s:bb.faded_orange,  s:bold)
" call s:HL('BruvboxRedFadedBg',       s:bb.light0,      s:bb.faded_red,     s:bold)
" call s:HL('BruvboxGreenFadedBg',     s:bb.light0,      s:bb.faded_green,   s:bold)
" call s:HL('BruvboxYellowFadedBg',    s:bb.dark0,       s:bb.faded_yellow,  s:bold)
" call s:HL('BruvboxBlueFadedBg',      s:bb.light0,      s:bb.faded_blue,    s:bold)
" call s:HL('BruvboxPurpleFadedBg',    s:bb.light0,      s:bb.faded_purple,  s:bold)
" call s:HL('BruvboxAquaFadedBg',      s:bb.light0,      s:bb.faded_aqua,    s:bold)
" call s:HL('BruvboxOrangeFadedBg',    s:bb.dark0,       s:bb.faded_orange,  s:bold)

" for sign column, also other minor standout highlights since sign column (by default anyways) has light bg than Normal
call s:HL('BruvboxRedSign',        s:red,            s:sign_column,      s:invert_signs)
call s:HL('BruvboxGreenSign',      s:green,          s:sign_column,      s:invert_signs)
call s:HL('BruvboxYellowSign',     s:yellow,         s:sign_column,      s:invert_signs)
call s:HL('BruvboxBlueSign',       s:blue,           s:sign_column,      s:invert_signs)
call s:HL('BruvboxPurpleSign',     s:purple,         s:sign_column,      s:invert_signs)
call s:HL('BruvboxAquaSign',       s:aqua,           s:sign_column,      s:invert_signs)
call s:HL('BruvboxOrangeSign',     s:orange,         s:sign_column,      s:invert_signs)
call s:HL('BruvboxFgSign',         s:fg_hard,        s:sign_column,      s:invert_signs)

" alt sign. fixup faded tho
call s:HL('BruvboxRedSignFaded',        s:bb.faded_red,          s:sign_column,      s:invert_signs)
call s:HL('BruvboxGreenSignFaded',      s:bb.faded_green,        s:sign_column,      s:invert_signs)
call s:HL('BruvboxYellowSignFaded',     s:bb.faded_yellow,       s:sign_column,      s:invert_signs)
call s:HL('BruvboxBlueSignFaded',       s:bb.faded_blue,         s:sign_column,      s:invert_signs)
call s:HL('BruvboxPurpleSignFaded',     s:bb.faded_purple,       s:sign_column,      s:invert_signs)
call s:HL('BruvboxAquaSignFaded',       s:bb.faded_aqua,         s:sign_column,      s:invert_signs)
call s:HL('BruvboxOrangeSignFaded',     s:bb.faded_orange,       s:sign_column,      s:invert_signs)

" light-ish background, text regular. prob should make sure it's not fully the
" same as any other bg color, so it will always stand out at least a bit...
call s:HL('BruvboxRedInfo',        s:red,            s:bg1_h,            s:italic)
call s:HL('BruvboxGreenInfo',      s:green,          s:bg1_h,            s:italic)
call s:HL('BruvboxYellowInfo',     s:yellow,         s:bg1_h,            s:italic)
call s:HL('BruvboxBlueInfo',       s:blue,           s:bg1_h,            s:italic)
call s:HL('BruvboxPurpleInfo',     s:purple,         s:bg1_h,            s:italic)
call s:HL('BruvboxAquaInfo',       s:aqua,           s:bg1_h,            s:italic)
call s:HL('BruvboxOrangeInfo',     s:orange,         s:bg1_h,            s:italic)
call s:HL('BruvboxFgInfo',         s:fg_hard,        s:bg1,              s:italic)

" faint as fuck, bg only, text must be proper white/black
call s:HL('BruvboxRedBg',          s:none,      s:bg_red      )
call s:HL('BruvboxGreenBg',        s:none,      s:bg_green    )
call s:HL('BruvboxYellowBg',       s:none,      s:bg_yellow,  )
call s:HL('BruvboxBlueBg',         s:none,      s:bg_blue     )
call s:HL('BruvboxPurpleBg',       s:none,      s:bg_purple,  )
call s:HL('BruvboxAquaBg',         s:none,      s:bg_aqua     )
call s:HL('BruvboxOrangeBg',       s:none,      s:bg_orange,  )

" same color but fg "no that's super pointless
call s:HL('BruvboxRedHue',         s:hue_red)
call s:HL('BruvboxGreenHue',       s:hue_green)
call s:HL('BruvboxYellowHue',      s:hue_yellow)
call s:HL('BruvboxBlueHue',        s:hue_blue)
call s:HL('BruvboxPurpleHue',      s:hue_purple)
call s:HL('BruvboxAquaHue',        s:hue_aqua)
call s:HL('BruvboxOrangeHue',      s:hue_orange)

call s:HL('BruvboxRedSpecial',     s:bb.special_red,       s:none)
call s:HL('BruvboxGreenSpecial',   s:bb.special_green,     s:none)
call s:HL('BruvboxYellowSpecial',  s:bb.special_yellow,    s:none)
call s:HL('BruvboxBlueSpecial',    s:bb.special_blue,      s:none)
call s:HL('BruvboxPurpleSpecial',  s:bb.special_purple,    s:none)
call s:HL('BruvboxAquaSpecial',    s:bb.special_aqua,      s:none)
call s:HL('BruvboxOrangeSpecial',  s:bb.special_orange,    s:none)

" mode color groups
call s:HL('BruvboxNormalModeColor', s:color_mode_normal,   s:color_column, s:bold)
call s:HL('BruvboxInsertModeColor', s:color_mode_insert,   s:color_column, s:bold)
call s:HL('BruvboxVisualModeColor', s:color_mode_visual,   s:color_column, s:bold)
call s:HL('BruvboxCursorLineNr',    s:color_mode_normal,   s:color_column, s:bold)

" }}}


" Vanilla colorscheme ---------------------------------------------------------
"{{{1         General UI:
" call s:HL('Normal',         s:fg1_h,            s:bg0)  " got errors when not defining both fg and bg for normal. Undef bg seems better for neovim though, from reading through issues. Check that gruvbox PR allowing this.
call s:HL('Normal',         s:fg0_s,            s:bg0)  " got errors when not defining both fg and bg for normal. Undef bg seems better for neovim though, from reading through issues. Check that gruvbox PR allowing this.
if s:is_dark | set background=dark   | else | set background=light    | endif   "workaround for vim being a shithead

call s:HL('CursorLine',     s:none,           s:cursorline_bg, s:bold)
call s:HL('CursorLineInvert', s:none,         s:bb.bg_dark_purple)
" hi!  link  CursorColumn     CursorLine
" below aint working, dunno why... so augroup still in vimrc for now.
if s:underline_cursorline == 1
  augroup CursorLine | autocmd!
    autocmd InsertEnter * 	highlight CursorLine gui=underline,bold cterm=underline,bold | hi! link CursorLineNr BruvboxRedSign      | hi! link VertSplit BruvboxInsertModeColor
    autocmd InsertLeave * 	highlight CursorLine gui=bold           cterm=bold           | hi! link CursorLineNr BruvboxCursorLineNr | hi! link VertSplit BruvboxNormalModeColor
  augroup END
endif

call s:HL('TabLineFill',    s:fg4,            s:bg0_h,          s:invert_tabline)
call s:HL('TabLineSel',     s:vim_bg,         s:bg4,            s:bold . s:invert_tabline)
call s:HL('TabLine',        s:fg4,            s:bg0_m)

call s:HL('QuickFixLine',   s:vim_fg,         s:vim_bg,         s:underline)

call s:HL('ColorColumn',    s:none,           s:color_column)
call s:HL('Conceal',        s:bb.faded_blue,  s:none)          " Concealed element: \lambda → λ

hi! link   SpecialKey       BruvboxBg3              "'unprintable characters' whatever that means. ^c sorta ting?
" call s:HL('NonText',        s:bg0_h2,          s:none)  "eol, TODO: support for if want colorcolumn/nontext bg to fade darker instead of lighter, fg needs to follow along
call s:HL('NonText',        s:bg0_s,          s:none)  "eol, TODO: support for if want colorcolumn/nontext bg to fade darker instead of lighter, fg needs to follow along
call s:HL('Whitespace',     s:bg0_s,          s:none)  "tab chars and stuff. links to NonText by default
call s:HL('EndOfBuffer',    s:color_column,   s:color_column)  "fg completely invisible, so dont have to see the stupid ~s after EOF
" call s:HL('Visual',         s:none,           s:bg0_s,          s:invert_selection)
call s:HL('Visual',         s:none,           s:none,          s:invert_selection)
hi! link   VisualNOS        Visual                             "removed from neovim but meh, no harm

" call s:HL('Search',         s:bb.light0,          s:hls_cursor,)  "     s:invert_search)
call s:HL('Search',         s:fg0,          s:search_background,)  "     s:invert_search)
" call s:HL('Search',         s:fg0,          s:bg_blue,)  "     s:invert_search)
" call s:HL('IncSearch',      s:hls_cursor,            s:fg1,       s:underline) ". s:invert_search)  " current match: flipped from Search. Blue on white instead of white on blue...
hi! link  IncSearch         Visual
" call s:HL('IncSearchCursor', s:fg0_s,         s:bb.faded_blue,    s:bold . s:invert_search)
call s:HL('IncSearchCursor', s:hls_cursor,           s:fg0_s,       s:bold) " . s:invert_search)

call s:HL('Underlined',     s:none,           s:none,             s:underline)

call s:HL('StatusLine',     s:bg0_h,          s:fg4,              s:inverse)      " try blending LineNr-VertSplit-TabLine-StatusLine... as bg0_h
call s:HL('StatusLineNC',   s:bg0_h,          s:color_mode_normal, s:inverse)
" call s:HL('VertSplit',      s:bg0_s,          s:bg0_h) "shifts focus from fg to fg turning it into more like a double pipe, looks fucking great!
" call s:HL('VertSplit',      s:vert_split,     s:bg0_h) "shifts focus from fg to fg turning it into more like a double pipe, looks fucking great!
call s:HL('VertSplit',      s:none,           s:bg0_h) "shifts focus from fg to fg turning it into more like a double pipe, looks fucking great!
" was breaking some stuff

" hi! link   MatchParen       BruvboxFgHard "maybe add underline too? Visual no good atm cause bug that makes cursor disappear on parens...ugh
hi! link   MatchParen       Visual
" call s:HL('MatchParen',         s:none,           s:bg0_s,          inverse)
" hi! link   MatchParen       Underlined


hi! link   Directory        BruvboxBlueSign  "BruvboxBlueSpecial    " Directory names, special names in listing
hi! link   Title            BruvboxPurpleSign     " Titles for output from :set all, :autocmd, etc.
hi! link   ErrorMsg         BruvboxRedSign
hi! link   WarningMsg       BruvboxYellowSign    "BruvboxYellowNeutral
hi! link   Question         BruvboxYellowInfo     " 'Press enter' prompt and yes/no questions
hi! link   MoreMsg          BruvboxFg4            " More prompt: -- More --
hi! link   ModeMsg          BruvboxFg2            " Current mode message: -- INSERT --

"{{{1         Gutter:
call s:HL('LineNr',         s:bg2_s,          s:number_column)
" call s:HL('LineNr',         s:bg3,          s:number_column)
hi!  link  CursorLineNr     BruvboxCursorLineNr   " Linked his way because we need the original stored somewhere when switching fg color dynamically for insert mode, via autocmds.
call s:HL('SignColumn',     s:none,           s:sign_column)

call s:HL('Folded',         s:bb.special_blue, s:fold_column,  s:italic) "Line used for closed folds
call s:HL('FoldColumn',     s:bg0,             s:fold_column)            "Best compromise between legibility and not looking like shit I've come up with thus far
"{{{1         Cursor:
" call s:HL('Cursor',         s:none,           s:none,    s:inverse) " Character under cursor
hi! link  Cursor            Visual
" hi! Cursor gui=s:inverse cterm=s:inverse
call s:HL('vCursor',        s:none,           s:none,    s:inverse . s:underline . s:bold)  " Visual mode cursor, needs more stuff to stand out since regular cursor hl same as Visual hl
hi! link  iCursor           Cursor                              " Input mode cursor
hi! link  lCursor           Cursor                              " Language mapping cursor
hi! link  TermCursor        Cursor
" call s:HL('TermCursorNC')  "cleared

" {{{1       Syntax Highlighting:

hi! link   Special          BruvboxOrange

hi! link   Todo             BruvboxPurpleSign
hi! link   Error            ErrorMsg

" FLOW
hi! link   Statement        BruvboxBlue             " Generic statement. def, return
" hi! link   Conditional      BruvboxBlueBold         " if, then, else, endif, switch, etc
hi! link   Conditional      BruvboxBlueHue          " if, then, else, endif, switch, etc
hi! link   Repeat           BruvboxBlueInfo         " for, do, while, etc.
hi! link   Label            BruvboxBlueSign         " case, default, etc.
hi! link   Keyword          BruvboxBlueSpecial      " any other keyword

hi! link   Exception        BruvboxRedInfo          " try, catch, throw, except, etc.

hi! link   Operator         BruvboxFgHard            " sizeof, "+", "*", etc.
" hi! link   Delimiter        BruvboxFg2
hi! link   Delimiter        BruvboxOrangeSign

hi! link   Identifier       BruvboxRed              " Variable name
hi! link   Function         BruvboxOrangeSpecial
" call s:HL('Function',       s:bb.bright_orange,  s:none,  s:bold)
" call s:HL('Function',       s:bb.bright_orange,  s:none)

" IMPORT / INCLUDE / PREPROCESSOR / MACROS
hi! link   PreProc          BruvboxAqua             " Generic   preprocessor
hi! link   Include          BruvboxAquaSpecial
hi! link   Define           BruvboxAquaSign
hi! link   Macro            BruvboxAquaSign         " Same as Define
hi! link   PreCondit        BruvboxAquaBold

" NUMBERS N SHIT
hi! link   Constant         BruvboxPurpleSign       " Generic   constant
hi! link   Character        BruvboxPurpleHue        " Character constant: 'c', '/n'
hi! link   Number           BruvboxPurple
" hi! link   Float            BruvboxPurpleSpecial
hi! link   Float            BruvboxPurpleBold
hi! link   Boolean          BruvboxRedSign

hi! link   Type             BruvboxYellowHue        " Generic type
hi! link   StorageClass     BruvboxYellowNeutral    " static, register, volatile, etc
hi! link   Structure        BruvboxYellowSign       " struct, union, enum, etc.
hi! link   Typedef          BruvboxYellowBold
call s:HL('String',         s:green,         s:none,               s:italicize_strings)
call s:HL('Comment',        s:gray,          s:none,               s:italicize_comments)

" {{{1        Completion:
" call s:HL('Pmenu',          s:fg1,           s:cursorline_bg)     " Popup menu: normal item, makes sense to link bg to whatever used for CursorLine, so drops down smoothly :)
call s:HL('Pmenu',          s:fg1,           s:bg1_s)     " Popup menu: normal item, makes sense to link bg to whatever used for CursorLine, so drops down smoothly :)
call s:HL('PmenuSel',       s:fg0,           s:neutral_blue,       s:bold)  " selected item     s:bg0_s, s:blue
call s:HL('PmenuSbar',      s:none,          s:bg0_h)             " scrollbar
call s:HL('PmenuThumb',     s:none,          s:bb.bg_light_blue)   " scrollbar thumb       s:none, s:bg2

call s:HL('WildMenu',       s:fg0,           s:bb.faded_blue,      s:bold)  " Current match in wildmenu completion     s:blue,  s:bg1,
"{{{1         Diffs:
call s:HL('DiffAdd',        s:none,          s:bb.bg_dark_green)
" hi! link   DiffAdd          BruvboxGreenFadedBg
call s:HL('DiffDelete',     s:none,          s:bb.bg_dark_red)
" hi! link   DiffDelete       BruvboxRedFadedBg
call s:HL('DiffChange',     s:none,          s:bb.bg_dark_yellow)
" hi! link   DiffChange       BruvboxYellowFadedBg
call s:HL('DiffText',       s:none,          s:none,     s:undercurl . s:italic, s:yellow)  " The actual changed text in changed line...

"{{{1         Spelling:
if has('spell')
  call s:HL('SpellCap',     s:green,         s:none,               s:bold . s:italic)   " Not capitalised word, or compile warnings
  call s:HL('SpellBad',     s:none,          s:none,               s:undercurl,   s:blue)
  call s:HL('SpellLocal',   s:none,          s:none,               s:undercurl,   s:aqua)
  call s:HL('SpellRare',    s:none,          s:none,               s:undercurl,   s:purple)
endif
" }}}

"{{{1         Diagnostic:
hi! link   DiagnosticHint    BruvboxFg4
hi! link   DiagnosticFloatingHint    BruvboxFgSignFaded
hi! link   DiagnosticFloatingError    BruvboxRedSignFaded
hi! link   DiagnosticFloatingWarning    BruvboxYellowSignFaded
hi! link   DiagnosticFloatingInfo    BruvboxFgSign
hi! link   FloatBorder       BruvboxBg4
call s:HL('NormalFloat', s:fg0_s, s:bg0_m) " Normal text in floating windows
" }}}
"{{{1         Neovim Terminal Colors:
if has('nvim')
  " couple of these are repeating now since yellow is already defined as special_yellow etc, fix
  " wasnt working prev bc all defs are rgb+256-index, so must grab the first one and not entire thing

  " base 8 colors:
  let g:terminal_color_0  = s:bg1[0]
  let g:terminal_color_1  = s:red[0]
  let g:terminal_color_2  = s:green[0]
  let g:terminal_color_3  = s:yellow[0]
  let g:terminal_color_4  = s:blue[0]
  let g:terminal_color_5  = s:purple[0]
  let g:terminal_color_6  = s:aqua[0]
  let g:terminal_color_7  = s:fg2_s[0]
  " bright/bold dittos:
  let g:terminal_color_8  = s:bg2[0]
  let g:terminal_color_9  = s:orange[0]
  let g:terminal_color_10 = s:special_green[0]
  let g:terminal_color_11 = s:special_yellow[0]
  let g:terminal_color_12 = s:special_blue[0]
  let g:terminal_color_13 = s:special_purple[0]
  let g:terminal_color_14 = s:special_aqua[0]
  let g:terminal_color_15 = s:fg0_s[0]
endif

"}}}



"{{{1 Plugin specific -------------------------------------------------------------

hi! link ExtraWhitespace BruvboxRedBg



"{{{2          Lsp
hi! link DiagnosticInfo  BruvboxInfoUndercurl
" hi @lsp.type.class      guifg=Aqua
" hi @lsp.type.function   guifg=Yellow
" hi @lsp.type.method     guifg=Green
" hi @lsp.type.parameter  guifg=Purple
hi link @lsp.type.variable         BruvboxFgSign
" hi @lsp.type.property   guifg=Green

" hi @lsp.typemod.function.classScope  guifg=Orange
" hi @lsp.typemod.variable.classScope  guifg=Orange
" hi @lsp.typemod.variable.fileScope   guifg=Orange
" hi @lsp.typemod.variable.globalScope guifg=Red
" hi link @lsp.typemod.variable.
hi link @lsp.typemod.function.async BruvboxBlueSignFaded " async functions are blue

"{{{2          ColDevIcons test

let g:coldevicons_colormap = {
        \'Brown'        : '#905532',
        \'Aqua'         : s:aqua[0],
        \'Blue'         : s:blue[0],
        \'Darkblue'     : s:bb.faded_blue[0],
        \'Purple'       : s:purple[0],
        \'Red'          : s:red[0],
        \'Beige'        : s:special_orange[0],
        \'Yellow'       : s:special_yellow[0],
        \'Orange'       : s:orange[0],
        \'Darkorange'   : s:bb.bg_dark_orange[0],
        \'Pink'         : s:special_purple[0],
        \'Salmon'       : '#EE6E73',
        \'Green'        : s:bb.bright_green[0],
        \'Lightgreen'   : s:green[0],
        \'White'        : s:fg0_h[0]
    \}
"}}}

" FIX:    FZF,    Denite

" jump plugin group def
" XXX doesnt work with light bg
if s:invert_targets == 0 | call s:HL('JumpTarget',  s:bb.special_blue,  s:bg_hard,    s:bold)
else                     | call s:HL('JumpTarget',  s:fg0_h,            s:bb.faded_blue)    | endif

" background/undercurl highlighting
" call s:HL('BruvboxErrorUndercurl',   s:none, s:bg_red,    s:undercurl, s:red)
" call s:HL('BruvboxWarningUndercurl', s:none, s:bg_yellow, s:undercurl, s:yellow)
" call s:HL('BruvboxInfoUndercurl',    s:none, s:bg_green,  s:undercurl, s:green)
" call s:HL('BruvboxHintUndercurl',    s:none, s:bg_blue,   s:undercurl, s:blue)
" do have undercurl working in Kitty. And set fixed in tmux yet not showing up (same w italics)
" hi BruvboxErrorUndercurl      cterm=undercurl ctermbg=NONE guisp=s:red
" hi BruvboxWarningUndercurl    cterm=undercurl ctermbg=NONE guisp=s:yellow
" hi BruvboxInfoUndercurl       cterm=undercurl ctermbg=NONE guisp=s:green
" hi BruvboxHintUndercurl       cterm=undercurl ctermbg=NONE guisp=s:blue
call s:HL('BruvboxErrorUndercurl',   s:fg3, s:none,  s:undercurl, s:red)
call s:HL('BruvboxWarningUndercurl', s:fg3, s:none,  s:undercurl, s:yellow)
call s:HL('BruvboxInfoUndercurl',    s:fg3, s:none,  s:undercurl, s:green)
call s:HL('BruvboxHintUndercurl',    s:fg3, s:none,  s:undercurl, s:blue)
hi! link BruvboxSignAdd             BruvboxGreenSign
hi! link BruvboxSignChange          BruvboxAquaSign
hi! link BruvboxSignDelete          BruvboxRedSign
hi! link BruvboxSignChangeDelete    BruvboxOrangeSign

hi! link BruvboxSignError           BruvboxRedSign
hi! link BruvboxSignWarning         BruvboxYellowSign
hi! link BruvboxSignInfo            BruvboxFgSign
hi! link BruvboxSignHint            BruvboxBlueSign

"{{{2         Nvim general subtypes:
hi! link NvimDarkBlue               BruvboxBlue
hi! link NvimLightBlue              BruvboxBlueSpecial
hi! link NvimDarkCyan               BruvboxCyan
hi! link NvimLightCyan              BruvboxCyanSpecial
hi! link NvimDarkGreen              BruvboxGreen
hi! link NvimLightGreen             BruvboxGreenSpecial
hi! link NvimDarkMagenta            BruvboxMagenta
hi! link NvimLightMagenta           BruvboxMagentaSpecial
hi! link NvimDarkRed                BruvboxRed
hi! link NvimLightRed               BruvboxRedSpecial
hi! link NvimDarkYellow             BruvboxYellow
hi! link NvimLightYellow            BruvboxYellowSpecial

hi! link NvimDarkGray1              BruvboxBg0
hi! link NvimLightGray1             BruvboxFg0
hi! link NvimDarkGray2              BruvboxBg0
hi! link NvimLightGray2             BruvboxFg1
hi! link NvimDarkGray3              BruvboxFg2
hi! link NvimLightGray3             BruvboxFg2
hi! link NvimDarkGray4              BruvboxFg3
hi! link NvimLightGray4             BruvboxFg3



"{{{2         ChooseWin:
let g:choosewin_color_land            ={'gui': ['', '', s:inverse], 'cterm': ['', '', s:inverse]}
let g:choosewin_color_other           ={'gui': [s:bg0_h[0], s:green[0], ''], 'cterm': ['', s:green[1], '']}
let g:choosewin_color_label           ={'gui': [s:orange[0], s:bg0_h[0], s:bold],
                                    \ 'cterm': [s:orange[1], s:bg0_h[1], s:bold]}
let g:choosewin_color_label_current   ={'gui': [s:green[0], s:bg0_h[0], s:bold],
                                    \ 'cterm': [s:green[1], s:bg0_h[1], s:bold]}
" let g:choosewin_color_shade           =
" let g:choosewin_color_overlay_current =
" let g:choosewin_color_overlay         =

"{{{2         EasyMotion:
"for motion/search/jump plugs
hi! link  EasyMotionTarget   JumpTarget
hi! link  EasyMotionShade    Comment

"{{{2         Sneak:
hi! link  Sneak              JumpTarget
hi! link  SneakScope         BruvboxBgHard
" hi! link SneakLabel         Search        "defaults to mirroring Sneak
hi! link  SneakLabelMask     Comment

"{{{2         Lista:
hi! link  ListaSearchHitBuffer   JumpTarget
" hi! link  ListaSearchHitFuzzy   "make something like, HL each char properly, then somewhere between faded and hit for the span between.
" hi! link  ListaSearchShade       Comment

"{{{2         Indent Guides:
if !exists('g:indent_guides_auto_colors') | let g:indent_guides_auto_colors = 0 | endif
if g:indent_guides_auto_colors == 0
  if g:bruvbox_invert_indent_guides == 0
    call s:HL('IndentGuidesEven', s:vim_bg, s:bg0)
    call s:HL('IndentGuidesOdd',  s:vim_bg, s:bg0_h2)
  else
    call s:HL('IndentGuidesEven', s:vim_bg, s:bg3, s:inverse)
    call s:HL('IndentGuidesOdd',  s:vim_bg, s:bg2, s:inverse)
  endif
endif
" IndentLine: {{{2
if !exists('g:indentLine_color_term') | let g:indentLine_color_term = s:bg2[1] | endif
if !exists('g:indentLine_color_gui')  | let g:indentLine_color_gui  = s:bg2[0] | endif

"{{{2         Rainbow Parentheses:
" if !exists('g:rbpt_colorpairs')
"   let g:rbpt_colorpairs = [
"       \ ['blue', '#458588'],  ['magenta', '#b16286'],
"       \ ['red',  '#cc241d'],  ['166',     '#d65d0e']  ]
" endif

" let g:rainbow_guifgs    = [ '#d65d0e', '#cc241d', '#b16286', '#458588' ]
" let g:rainbow_ctermfgs  = [ '166', 'red', 'magenta', 'blue' ]

" if !exists('g:rainbow_conf')              | let g:rainbow_conf = {}     | endif
" if !has_key(g:rainbow_conf, 'guifgs')     | let g:rainbow_conf['guifgs']    = g:rainbow_guifgs    | endif
" if !has_key(g:rainbow_conf, 'ctermfgs')   | let g:rainbow_conf['ctermfgs']  = g:rainbow_ctermfgs  | endif

" let g:niji_dark_colours   = g:rbpt_colorpairs
" let g:niji_light_colours  = g:rbpt_colorpairs

"{{{2         GitGutter:
hi! link GitGutterAdd           BruvboxGreenSign
hi! link GitGutterChange        BruvboxAquaSign
hi! link GitGutterDelete        BruvboxRedSign
hi! link GitGutterChangeDelete  BruvboxOrangeSign
"{{{2        GitCommit:
hi! link gitcommitSelectedFile  BruvboxGreen
hi! link gitcommitDiscardedFile BruvboxRed
"{{{2         Signify:
hi! link SignifySignAdd         BruvboxGreenSign
hi! link SignifySignChange      BruvboxAquaSign
hi! link SignifySignDelete      BruvboxRedSign
"{{{2         ALE:
hi! link ALEError         BruvboxErrorUndercurl
hi! link ALEErrorSign     BruvboxRedSign
hi! link ALEWarning       BruvboxWarningUndercurl
hi! link ALEWarningSign   BruvboxFgSign "BruvboxYellowSign
" set dynamically or w/e, only if that setting...
" hi! link ALEErrorLine     BruvboxRedBg
" hi! link ALEWarningLine   BruvboxYellowBg
"{{{2         Coc:
hi! link CocErrorHighlight      BruvboxErrorUndercurl
hi! link CocErrorSign           BruvboxSignError
hi! link CocErrorFloat          BruvboxRedSpecial
hi! link CocErrorVirtualText    ErrorMsg

hi! link CocWarningHighlight    BruvboxWarningUndercurl
hi! link CocWarningSign         BruvboxSignWarning
hi! link CocWarningFloat        BruvboxYellowSpecial
hi! link CocWarningVirtualText  WarningMsg

hi! link CocInfoHighlight      BruvboxInfoUndercurl
hi! link CocInfoSign           BruvboxSignInfo
hi! link CocInfoFloat          BruvboxFg3
hi! link CocInfoVirtualText    CocInfoFloat

hi! link CocHintHighlight      BruvboxHintUndercurl
hi! link CocHintSign           BruvboxSignHint
hi! link CocHintFloat          BruvboxBlueSpecial
hi! link CocHintVirtualText    BruvboxSignHint

hi! link CocCodeLens           BruvboxPurpleSignFaded
" hi! link CocCodeLens           BruvboxGray

call s:HL('CocFloating',        s:fg3,  s:bg0_h)
call s:HL('CocFloatingBorder',  s:red,  s:bg0_m)
call s:HL('BruvboxBg4Line',     s:none,    s:bg0_h)
call s:HL('None',     s:none,    s:none)


hi! link CocGitAddedSign            BruvboxGreenSign 
hi! link CocGitChangeRemovedSign    BruvboxAquaSign  
hi! link CocGitChangedSign          BruvboxOrangeSign 
hi! link CocGitRemovedSign          BruvboxRedSign    
" hi! link CocGitTopRemovedSign       


hi! link CocSem_namespace       BruvboxPurpleHue
hi! link CocSem_type            Type
hi! link CocSem_class           BruvboxYellowHue
hi! link CocSem_enum            Type
hi! link CocSem_interface       Type
hi! link CocSem_struct          Structure
hi! link CocSem_typeParameter   Type
hi! link CocSem_parameter       Normal
hi! link CocSem_variable        Normal
hi! link CocSem_property        Normal
hi! link CocSem_enumMember      Constant
hi! link CocSem_event           Identifier
hi! link CocSem_function        Function
hi! link CocSem_method          Function
hi! link CocSem_macro           Macro
hi! link CocSem_keyword         Keyword
hi! link CocSem_modifier        StorageClass
hi! link CocSem_comment         Comment
hi! link CocSem_string          String
hi! link CocSem_number          Number
hi! link CocSem_regexp          Normal
hi! link CocSem_operator        Operator

"{{{2         Syntastic:
hi! link SyntasticError       ALEError
hi! link SyntasticWarning     ALEWarning
hi! link SyntasticErrorSign   ALEErrorSign
hi! link SyntasticWarningSign ALEWarningSign
"{{{2         Signature:
hi! link SignatureMarkText   BruvboxBlueSign
hi! link SignatureMarkerText BruvboxPurpleSign
"{{{2         ShowMarks:
hi! link ShowMarksHLl     BruvboxBlueSign
hi! link ShowMarksHLu     BruvboxBlueSign
hi! link ShowMarksHLo     BruvboxBlueSign
hi! link ShowMarksHLm     BruvboxBlueSign
"{{{2         CtrlP:
hi! link CtrlPMatch       BruvboxBlue
hi! link CtrlPNoEntries   BruvboxRedInfo
hi! link CtrlPPrtBase     BruvboxBg1
hi! link CtrlPPrtCursor   BruvboxOrange
hi! link CtrlPLinePre     BruvboxBg2
call s:HL('CtrlPMode1',   s:blue, s:bg2,  s:bold)
call s:HL('CtrlPMode2',   s:bg0,  s:blue, s:bold)
call s:HL('CtrlPStats',   s:fg4,  s:bg2,  s:bold)
"{{{2         Startify:
hi! link StartifyBracket  BruvboxBg4
hi! link StartifyFile     BruvboxBlueSign
hi! link StartifyNumber   BruvboxOrange
hi! link StartifyPath     BruvboxFg4
hi! link StartifySlash    BruvboxFg2
hi! link StartifySection  BruvboxYellow
hi! link StartifySelect   BruvboxPurple
hi! link StartifySpecial  BruvboxBg2
hi! link StartifyHeader   BruvboxBlueSpecial
hi! link StartifyFooter   BruvboxBg4
hi! link StartifyVar      BruvboxRedSpecial
    " StartifyBracket  |  [,]                     |  linked to Delimiter
    " StartifyFile     |  the actual file         |  linked to Identifier
    " StartifyFooter   |  the custom footer       |  linked to Title
    " StartifyHeader   |  the custom header       |  linked to Title
    " StartifyNumber   |  the numbers between []  |  linked to Number
    " StartifyPath     |  the path to a file      |  linked to Directory
    " StartifySection  |  section headers         |  linked to Statement
    " StartifySelect   |  selected entries        |  linked to Title
    " StartifySlash    |  slashes in paths        |  linked to Delimiter
    " StartifySpecial  |  <empty buffer>,<quit>   |  linked to Comment
    " StartifyVar      |  environment variables   |  linked to StartifyPath
"{{{2         NERDTree:
hi! link	 NERDTreeOpenable        BruvboxBlueSign
hi! link  NERDTreeClosable        BruvboxOrangeSign
hi! link  NERDTreeDir             Directory
hi! link  NERDTreeLinkDir         BruvboxAquaSign
hi! link  NERDTreeLinkFile        BruvboxFg3
hi! link  NERDTreeLinkTarget      Comment
hi! link  NERDTreeDirSlash        Function
hi! link  NERDTreeDirFlags        BruvboxBlueSign "folder icon
hi! link  NERDTreeCWD             BruvboxOrangeSign
hi! link  NERDTreeBookmarkName    BruvboxBlueSpecial
hi! link  NERDTreeBookmarksLeader BruvboxOrangeHue
hi! link  NERDTreeBookmark        BruvboxFg3
hi! link  NERDTreeHelpTitle       BruvboxBlue
hi! link  NERDTreeHelp            Comment

"{{{2          Undotree:
hi! link UndotreeAdd      DiffAdd
hi! link UndotreeChange   DiffChange
" let g:undotree_TreeNodeShape 									=s:bb.glyph_source_omni
"{{{2          Tagbar:

hi! link  TagbarKind             BruvboxRedInfo     "header of generic 'kinds', functions/variables.
hi! link  TagbarNestedKind       BruvboxRedSpecial  "kind headers, within scopes.
hi! link  TagbarScope            BruvboxOrangeSpecial  "scope, like class, struct.   OrangeSpecial matches code, for py anyways
hi! link  TagbarType             Statement          "type of a tag or scope.    Default Title / purpz
hi! link  TagbarSignature        BruvboxFg4         "Function signatures.     Links to SpecialKey = dark
hi! link  TagbarPseudoID         BruvboxFg3         "asterisk* that signifies a pseudo-tag.  Links to NonText = no go
hi! link  TagbarFoldIcon         BruvboxBlueInfo    "Folded  "fold icon left of foldable tags.   Default Statement / blue
hi! link  TagbarHighlight        BruvboxBlueSpecial "Colour for auto-hl current tag.   Default Search = nah
call s:HL('TagbarAccessPublic',     s:aqua,   s:none,  s:bold)
call s:HL('TagbarAccessProtected',  s:yellow, s:none,  s:bold)
call s:HL('TagbarAccessPrivate',    s:red,    s:none,  s:bold)
" hi! link  TagbarAccessPublic     BruvboxGreenBg    "public access symbol.   Default hard green. TagbarVisibilityPublic links
" hi! link  TagbarAccessProtected  BruvboxYellowSign   "protected access symbol.  Default?. TagbarVisibilityProtected links
" hi! link  TagbarAccessPrivate    BruvboxRedBg      "private access symbol.  Default hard red. TagbarVisibilityPrivate links

hi! link  TagbarHelpTitle        BruvboxBlue        "prettier/matches nerdtree
" hi! link  TagbarComment        Comment " The help at the top of the buffer.


"{{{2          Matchmaker:
hi       Matchmaker       cterm=underline   gui=underline
" HighlightedyankRegion: {{{2
hi! link  HighlightedyankRegion   Visual
" Vimshell: {{{2
let g:vimshell_escape_colors = [
  \ s:bg4[0],  s:red[0],    s:green[0], s:yellow[0],
  \ s:blue[0], s:purple[0], s:aqua[0],  s:fg4[0],
  \ s:bg0[0],  s:red[0],    s:green[0], s:orange[0],
  \ s:blue[0], s:purple[0], s:aqua[0],  s:fg0[0]
  \ ]
" BufTabLine: {{{2
call s:HL('BufTabLineCurrent', s:bg0, s:fg4)
call s:HL('BufTabLineActive',  s:fg4, s:bg2)
call s:HL('BufTabLineHidden',  s:bg4, s:bg1)
call s:HL('BufTabLineFill',    s:bg0, s:bg0)
"}}}1
"{{{1         Filetype specific -----------------------------------------------------------
" PLAN: generally more blue more often, relegate aqua to imports and shit.
" Green can stay as string color but should be more suble. Less yellow...
" so yellow and red for like vars and stuff?
" Diff: {{{2
hi! link diffAdded        DiffAdd         "
hi! link diffRemoved      DiffDelete      "
hi! link diffChanged      DiffChange      "

hi! link diffFile         BruvboxGreen    "BruvboxOrangeBg
hi! link diffNewFile      BruvboxRed

hi! link diffLine         BruvboxBlueSpecial     "BruvboxBlue
hi! link diffSubname      BruvboxPurpleSign       "BruvboxBlue

" Html: {{{2

" hi! link htmlTag            BruvboxOrangeSpecial  "opening brackets
hi! link htmlTag            BruvboxFg3  "opening brackets
hi! link htmlEndTag         BruvboxOrangeSpecial         "closing brackets
" hi! link htmlEndTag         BruvboxRed         "closing brackets
hi! link htmlTagN           BruvboxFg3            "unknown tag?
hi! link htmlTagName        BruvboxAquaSpecial    "div etc
" hi! link htmlArg            BruvboxRedSpecial
hi! link htmlArg            BruvboxBlueBold "class, id

hi! link htmlScriptTag      BruvboxPurple
hi! link htmlSpecialTagName BruvboxAquaBold

" hi! link htmlTag            BruvboxBlue
" hi! link htmlEndTag         BruvboxWhite
" hi! link htmlTagN           BruvboxOrange "BruvboxFg1
" hi! link htmlTagName        BruvboxAquaBold
" " hi! link htmlArg            BruvboxAqua
" hi! link htmlArg            BruvboxRedSpecial
"
" hi! link htmlScriptTag      BruvboxPurple
" hi! link htmlSpecialTagName BruvboxAquaBold
"
call s:HL('htmlLink', s:fg4, s:none, s:underline)

hi! link htmlSpecialChar BruvboxOrange

call s:HL('htmlBold',                s:vim_fg, s:vim_bg, s:bold)
call s:HL('htmlBoldUnderline',       s:vim_fg, s:vim_bg, s:bold . s:underline)
call s:HL('htmlBoldItalic',          s:vim_fg, s:vim_bg, s:bold . s:italic)
call s:HL('htmlBoldUnderlineItalic', s:vim_fg, s:vim_bg, s:bold . s:underline . s:italic)

call s:HL('htmlUnderline',       s:vim_fg, s:vim_bg, s:underline)
call s:HL('htmlUnderlineItalic', s:vim_fg, s:vim_bg, s:underline . s:italic)
call s:HL('htmlItalic',          s:vim_fg, s:vim_bg, s:italic)

" Xml: {{{2

hi! link xmlTag               BruvboxFg3
hi! link xmlEndTag            BruvboxFg1
hi! link xmlTagName           BruvboxBlue
hi! link xmlEqual             BruvboxRed
hi! link docbkKeyword         BruvboxAquaBold

hi! link xmlDocTypeDecl       BruvboxGray
hi! link xmlDocTypeKeyword    BruvboxPurple
hi! link xmlCdataStart        BruvboxGray
hi! link xmlCdataCdata        BruvboxPurple
hi! link dtdFunction          BruvboxGray
hi! link dtdTagName           BruvboxPurple

hi! link xmlAttrib            BruvboxAqua
hi! link xmlProcessingDelim   BruvboxGray
hi! link dtdParamEntityPunct  BruvboxGray
hi! link dtdParamEntityDPunct BruvboxGray
hi! link xmlAttribPunct       BruvboxGray

hi! link xmlEntity            BruvboxOrange
hi! link xmlEntityPunct       BruvboxOrange
" Vim: {{{2

call s:HL('vimCommentTitle', s:fg4_256, s:none, s:bold . s:italicize_comments)

hi! link vimMap            BruvboxBlueNeutral "BruvboxBlueHue
"either keys or fns orange, yeah? choose which?
hi! link vimMapModKey      BruvboxPurpleSpecial          "<Leader> etc
hi! link vimNotation       BruvboxOrangeSpecial  "<CR> in mapping etc
hi! link vimBracket        BruvboxFg3        "<>
hi! link vimMapMod         BruvboxFg3   "map brackets?
" hi! link vimBracket        BruvboxOrange         "<>
" hi! link vimMapMod         BruvboxOrange    "map brackets?

hi! link vimParenSep       BruvboxFg4
hi! link vimSep            BruvboxFg3
hi! link vimLet            BruvboxBlueHue
hi! link vimSetEqual       BruvboxFg1_h
" hi! link vimSetSep         BruvboxFg3  "does nothing?
hi! link vimUserCommand    BruvboxBlueNeutral  "tho really just why not same blue as rest?
hi! link vimIsCommand      BruvboxFg0  "fg0_h showing horrible greenish hue suddnely, prob my monitor/angle but beware...  "BruvboxBlueHue
" call s:HL('vimMapLhs',   s:none, s:none)  "nah ugly and useless
" call s:HL('vimMapRhs',   s:none, s:bg1_s)
hi! link vimFuncSID        BruvboxFg3
hi! link vimUserFunc       BruvboxOrange "BruvboxFgHard "when called, should be orange somehow, to match regular ones. or swap those?
hi! link vimFunction       BruvboxOrangeSign "BruvboxFgHard "when defined
hi! link vimFuncName       BruvboxOrangeBold "built-in fns...
" hi! link vimFuncBody  BruvboxFg1
hi! link vimFuncVar        BruvboxRedSpecial
" 
hi! link vimContinue       BruvboxFg3
hi! link vimGroup          BruvboxYellow
hi! link vimHiGroup        BruvboxYellowSpecial

hi! link vimSynError       Error

" Tmux: {{{2

" hi! link tmuxFormatString 
" hi! link tmuxAction               Boolean
" hi! link tmuxBoolean              Boolean
" hi! link tmuxOptionValue          Constant
" hi! link tmuxLayoutOptionValue    Constant
" hi! link tmuxClientSessionCmds    Keyword
" hi! link tmuxWindowPaneCmds       Keyword
" hi! link tmuxBindingCmds          Keyword
" hi! link tmuxEnvironmentCmds      Keyword
" hi! link tmuxStatusLineCmds       Keyword
" hi! link tmuxBufferCmds           Keyword
" hi! link tmuxMiscCmds             Keyword
" hi! link tmuxOptsSet              PreProc
" hi! link tmuxUserOptsSet          Identifier
" hi! link tmuxOptsSetw             PreProc
" hi! link tmuxModeCmds             Keyword
" hi! link tmuxKeySymbol            Special
" hi! link tmuxSpecialCmds          Type
" hi! link tmuxURL                  Underlined
" hi! link tmuxKey                  Special
" hi! link tmuxSelWindowOption      Number
" hi! link tmuxOptions              Operator
" hi! link tmuxVariable             Constant
hi! link tmuxVariableExpansion    Character
" hi! link tmuxAdditionalCommand    Special
" hi! link tmuxKeyTable             PreProc
hi! link tmuxColor                BruvboxBlueHue
hi! link tmuxStyle                PreProc "Constant
" hi! link tmuxPromptInpol          Special
" hi! link tmuxDateInpol            Special

hi! link tmuxFmtAlias             Special
" hi! link tmuxFmtInpolDelimiter    Delimiter
hi! link tmuxFmtInpolDelimiter    BruvboxRedHue
hi! link tmuxFmtVariable          Identifier "Constant
" hi! link tmuxFmtConditional       Conditional
hi! link tmuxFmtLimit             Operator
" hi! link tmuxFmtInpol            

hi! link tmuxAttrInpolDelimiter   Delimiter
" hi! link tmuxAttrInpolDelimiter   BruvboxYellowHue
hi! link tmuxAttrBgFg             Macro
" hi! link tmuxAttrEquals           Operator
" hi! link tmuxAttrSeparator        Operator
" hi! link tmuxAttrInpol           Type
hi! link tmuxAttrInpol           Character
" hi! link tmuxAttrInpol           BruvboxYellowSign

" hi! link tmuxShellInpolDelimiter  Delimiter
" hi! link tmuxShellInpol           String
" hi! link tmuxStringDelimiter      Delimiter
" hi! link tmuxString               String

" Clojure: {{{2

hi! link clojureSymbol       BruvboxFg0
hi! link clojureKeyword      BruvboxBlue
hi! link clojureCond         BruvboxBlueHue
hi! link clojureSpecial      BruvboxPurpleHue "BruvboxOrangeHue "def, let..
hi! link clojureDefine       BruvboxPurpleSpecial  "BruvboxFgHard "BruvboxYellow "Special  "BruvboxRedHue "BruvboxOrangeSpecial "defn
hi! link clojureFunc         BruvboxOrangeSign "Function "BruvboxYellow
hi! link clojureMacro        Function "BruvboxOrangeSign "BruvboxRedHue "Aqua "BruvboxOrange
"define syntax for functions! w/ side-effects?
"and tint (functions?) red a la bool?
"Number purp, "clojure.lang" / "java.lang" etc smthn
hi! link clojureDispatch     BruvboxAquaSign "BruvboxFgHard  "BruvboxYellowBold "clojureSpecial "macro hash ting
hi! link clojureAnonArg      BruvboxYellowBold "BruvboxYellowSign

hi! link clojureRepeat       BruvboxBlueSpecial "BruvboxYellowSpecial
hi! link clojureCharacter    BruvboxAqua
hi! link clojureConstant     BruvboxRedSpecial
" hi! link clojureNumber       BruvboxPurple "default
hi! link clojureStringEscape BruvboxAquaSign
" hi! link clojureException    BruvboxRed
hi! link clojureException    BruvboxRedNeutral

hi! link clojureRegexp       BruvboxAqua
hi! link clojureRegexpEscape BruvboxAquaHue
call s:HL('clojureRegexpCharClass', s:fg3, s:none, s:bold)
hi! link clojureRegexpMod        clojureRegexpCharClass
hi! link clojureRegexpQuantifier clojureRegexpCharClass

hi! link clojureParen        BruvboxFg4
hi! link clojureVarArg       BruvboxFgHard "BruvboxBlueHue
hi! link clojureVariable     BruvboxRedHue  "BruvboxBlue

hi! link clojureMeta         BruvboxYellow
hi! link clojureDeref        BruvboxAqua "BruvboxFgHard  "BruvboxYellow
hi! link clojureQuote        BruvboxGreenNeutral "YellowSpecial
hi! link clojureUnquote      BruvboxAquaSpecial "YellowSign

hi! link clojureStringDelimiter BruvboxGreenSymbol

"tol specials
hi! link clojureRecede       BruvboxFg3      "dashes, slashes, dots...
hi! link clojureStandout     BruvboxFgHard   "#, !...
hi! link clojureCommentDelimiter BruvboxBg3

" C: {{{2
hi! link cOperator           BruvboxPurple
hi! link cStructure          BruvboxOrange
hi! link cCustomParen        BruvboxFg4
" hi! link cFloat              BruvboxPurpleBold
" hi! link cCustomFunc         BruvboxOrangeHue
" hi! link cCustomFunc         BruvboxRedHue
" hi! link cCustomFunc         BruvboxRedSpecial
hi! link cParen              BruvboxFg4
" hi! link cPreProc            BruvboxAquaSpecial
hi! link cIncluded           BruvboxAquaHue

" C++: {{{2
hi! link cStorageClass       BruvboxFg4
hi! link cCustomClass        BruvboxPurpleHue
" hi! link cCustomClassName    BruvboxPurpleHue "i like variation but guess makes sense keep yellowish...
hi! link cCustomClassName    BruvboxYellowHue "i like variation but guess makes sense keep yellowish...
hi! link cCustomClassKey     BruvboxPurpleHue
hi! link cCustomMemVar       BruvboxOrangeNeutral

" hi! link cppSTLFunction      BruvboxOrangeSign "think want something more differing
hi! link cppSTLFunction      BruvboxOrangeBold "think want something more differing
" hi! link cppSTLFunction      BruvboxOrangeInfo "think want something more differing
hi! link cppSTLFunctional    BruvboxOrangeNeutral "Typedef, makes sense i guess
hi! link cppSTLType          BruvboxYellowBold
" bold is good tho should use more for more
" hi! link cppSTLConstant      "nullptr etc
" hi! link cppSTLNamespace     BruvboxPurpleHue
hi! link cppSTLNamespace     BruvboxPurpleInfo
hi! link cppType             BruvboxYellowNeutral   "bool

" hi! link cppStructure        BruvboxAquaFadedBG   "default Structure. template, typename... ironically doesnt include struct.
hi! link cppStructure        BruvboxAquaSpecial   "default Structure. template, typename... ironically doesnt include struct.

" hi! link cppModifier         BruvboxYellowSpecial
" hi! link cppModifier         BruvboxRedHue
hi! link cppModifier         BruvboxRedInfo
hi! link cppAccess           BruvboxBlueInfo
hi! link cppStatement        BruvboxBlueBold "this, new etc
hi! link cStatement          BruvboxBlueSpecial    "return, heh.
hi! link cppCustomParen      BruvboxFg4
hi! link cppParen            BruvboxFg2
" hi! link cppOperator           BruvboxPurple
"Custom in my syntax file. actual operators, +-=* yada
hi! link cppSymbol         BruvboxFgHard
" hi! link cBlock            BruvboxFgHard "que?
hi! link cCustomPtr        BruvboxFg4
hi! link cCustomDot        BruvboxFg4
" recede and more is in my cpp syntax after...
hi! link cppRecede         BruvboxFg4
hi! link cppTernary        BruvboxBlueBold
hi! link cCustomScope       BruvboxFg3       "dots between namespaces. fade out..
hi! link cBitField         BruvboxFg2
" hi! link cppPointerDeref     BruvboxFg4  "is already linked (straight to Fg4), but how??


" Python: {{{2
hi! link pythonBuiltin     BruvboxOrangeBold      "Sign
hi! link pythonBuiltinObj  BruvboxOrangeNeutral
hi! link pythonBuiltinFunc BruvboxOrangeSign
hi! link pythonFunction    Function
hi! link pythonDecorator   BruvboxRedHue
hi! link pythonDottedName  Type
hi! link pythonInclude     Include      "BruvboxAqua
hi! link pythonImport      Define      "BruvboxAqua
" hi! link pythonRun         BruvboxBlue
" hi! link pythonCoding      BruvboxBlue
" hi! link pythonOperator    BruvboxRedSpecial   "Identifier     "BruvboxRed
call s:HL('pythonOperator',       s:fg2,      s:sign_column,    s:underline . s:bold)
hi! link pythonExtraOperator    Operator  "BruvboxRed
hi! link pythonExceptions  Exception
hi! link pythonDot         BruvboxFg3
hi! link pythonComma       BruvboxFg3

hi! link pythonRawString   BruvboxGreenInfo

hi! link pythonBrackets    Special
hi! link pythonBraces      Special
" hi! link pythonBraces

highlight! link pythonSelf BruvboxFg3   "not default

hi! link djangoModelField       BruvboxBlueHue "type
hi! link djangoModelsModule     BruvboxFg3
" hi! link djangoSettingsModule      
" hi! link djangoQueryObject      Bruvbox "special
hi! link djangoTagBlock     Special
" hi! link djangoStatement     BruvboxFg3

" CSS: {{{2

" hi! link cssAtKeyword               
hi! link cssBraces               BruvboxFg3
hi! link cssFunctionName         Function
hi! link cssAttrRegion           Function   "fns wrongly end up as this when args multiline?
" hi! link cssIdentifier           BruvboxOrange
hi! link cssClassName            BruvboxYellow
hi! link cssPseudoClass          BruvboxBlueInfo
" hi! link cssColor                BruvboxBlue
" hi! link cssSelectorOp           BruvboxBlue
" hi! link cssSelectorOp2          BruvboxBlue
" hi! link cssImportant            BruvboxRedHue
hi! link cssVendor               BruvboxFg4
 
hi! link cssUnitDecorators       BruvboxBlueHue

" hi! link cssValueNumber          Number
" hi! link cssValueLength          BruvboxBlueSpecial
" hi! link cssValueTime            BruvboxPurpleSpecial
hi! link cssValueAngle           Number

hi! link cssCustomProp           Normal   "vars ending up w this group for some reason. white seems more reasonable since not used for shit hah

" hi! link cssAnimationProp        BruvboxAqua
" hi! link cssUIProp               BruvboxYellow
hi! link cssUIAttr               BruvboxFg2
" hi! link cssTransformProp        BruvboxAqua
" hi! link cssTransitionProp       BruvboxAqua
hi! link cssPositioningProp      BruvboxPurpleHue
" hi! link cssBoxProp              BruvboxAqua
hi! link cssFlexibleBoxProp      BruvboxBlueHue
" hi! link cssBorderOutlineProp    BruvboxAqua
" hi! link cssBackgroundProp       BruvboxAqua
" hi! link cssMarginProp           BruvboxAqua
" hi! link cssListProp             BruvboxAqua
" hi! link cssTableProp            BruvboxAqua
" hi! link cssFontProp             BruvboxAqua
" hi! link cssFontDescriptorProp   BruvboxAqua
" hi! link cssTextProp             BruvboxAqua
" hi! link cssPrintProp            BruvboxAqua
hi! link cssFontAttr             BruvboxFg2
" hi! link cssPaddingProp          BruvboxAqua
" hi! link cssDimensionProp        BruvboxAqua
" hi! link cssRenderProp           BruvboxAqua
" hi! link cssColorProp            BruvboxAqua
hi! link cssColor            Normal "i mean should show the color uh..
" hi! link cssGeneratedContentProp BruvboxAqua

" JavaScript: {{{2

hi! link javaScriptBraces     BruvboxFg1
hi! link javaScriptFunction   BruvboxBlue
hi! link javaScriptIdentifier BruvboxRed
hi! link javaScriptMember     BruvboxAqua
hi! link javaScriptNumber     BruvboxPurple
hi! link javaScriptNull       BruvboxRedInfo
hi! link javaScriptParens     BruvboxFg3

" YAJS: {{{2

hi! link javascriptImport               BruvboxAqua
hi! link javascriptExport               BruvboxAqua
hi! link javascriptClassKeyword         BruvboxAqua
hi! link javascriptClassExtends         BruvboxAqua
hi! link javascriptDefault              BruvboxAqua

hi! link javascriptClassName            BruvboxYellow
hi! link javascriptClassSuperName       BruvboxYellow
hi! link javascriptGlobal               BruvboxYellow

hi! link javascriptEndColons            BruvboxFg1
hi! link javascriptFuncArg              BruvboxFg1
hi! link javascriptGlobalMethod         BruvboxFg1
hi! link javascriptNodeGlobal           BruvboxFg1

" hi! link javascriptVariable BruvboxOrange
hi! link javascriptVariable             BruvboxRed
" hi! link javascriptIdentifier BruvboxOrange
" hi! link javascriptClassSuper BruvboxOrange
hi! link javascriptIdentifier           BruvboxOrange
hi! link javascriptClassSuper           BruvboxOrange

" hi! link javascriptFuncKeyword BruvboxOrange
" hi! link javascriptAsyncFunc BruvboxOrange
hi! link javascriptFuncKeyword          BruvboxAqua
hi! link javascriptAsyncFunc            BruvboxAqua
hi! link javascriptClassStatic          BruvboxOrange

hi! link javascriptOperator             BruvboxRed
hi! link javascriptForOperator          BruvboxRed
hi! link javascriptYield                BruvboxRed
hi! link javascriptExceptions           BruvboxRed
hi! link javascriptMessage              BruvboxRed

hi! link javascriptTemplateSB           BruvboxAqua
hi! link javascriptTemplateSubstitution BruvboxFg1

" hi! link javascriptLabel BruvboxBlue
" hi! link javascriptObjectLabel BruvboxBlue
" hi! link javascriptPropertyName BruvboxBlue
hi! link javascriptLabel                BruvboxFg1
hi! link javascriptObjectLabel          BruvboxFg1
hi! link javascriptPropertyName         BruvboxFg1

hi! link javascriptLogicSymbols         BruvboxFg1
hi! link javascriptArrowFunc            BruvboxFg1

hi! link javascriptDocParamName         BruvboxFg4
hi! link javascriptDocTags              BruvboxFg4
hi! link javascriptDocNotation          BruvboxFg4
hi! link javascriptDocParamType         BruvboxFg4
hi! link javascriptDocNamedParamType    BruvboxFg4

" TypeScript: {{{2

hi! link typeScriptReserved     BruvboxAqua
hi! link typeScriptLabel        BruvboxAqua
hi! link typeScriptIdentifier   BruvboxOrange
hi! link typeScriptBraces       BruvboxFg1
hi! link typeScriptEndColons    BruvboxFg1
hi! link typeScriptDOMObjects   BruvboxFg1
hi! link typeScriptAjaxMethods  BruvboxFg1
hi! link typeScriptLogicSymbols BruvboxFg1
hi! link typeScriptDocSeeTag    Comment
hi! link typeScriptDocParam     Comment
hi! link typeScriptDocTags      vimCommentTitle

" CoffeeScript: {{{2

hi! link coffeeExtendedOp BruvboxFg3
hi! link coffeeSpecialOp BruvboxFg3
hi! link coffeeCurly BruvboxOrange
hi! link coffeeParen BruvboxFg3
hi! link coffeeBracket BruvboxOrange

" Ruby: {{{2
hi! link rubyStringDelimiter BruvboxGreen
hi! link rubyInterpolationDelimiter BruvboxAqua

" ObjectiveC: {{{2
hi! link objcTypeModifier BruvboxRed
hi! link objcDirective BruvboxBlue

" Go: {{{2
hi! link goDirective BruvboxAqua
hi! link goConstants BruvboxPurple
hi! link goDeclaration BruvboxRed
hi! link goDeclType BruvboxBlue
hi! link goBuiltins BruvboxOrange

" Lua: {{{2
hi! link luaIn BruvboxRed
hi! link luaFunction BruvboxBlue
hi! link luaTable BruvboxOrange

" MoonScript: {{{2
hi! link moonSpecialOp BruvboxFg3
hi! link moonExtendedOp BruvboxFg3
hi! link moonFunction BruvboxFg3
hi! link moonObject BruvboxYellow

" Java: {{{2
hi! link javaAnnotation BruvboxBlue
hi! link javaDocTags BruvboxAquaSign
hi! link javaCommentTitle vimCommentTitle
hi! link javaParen BruvboxFg3
hi! link javaParen1 BruvboxFg3
hi! link javaParen2 BruvboxFg3
hi! link javaParen3 BruvboxFg3
hi! link javaParen4 BruvboxFg3
hi! link javaParen5 BruvboxFg3
hi! link javaOperator BruvboxOrange

hi! link javaVarArg BruvboxYellow

" Elixir: {{{2
hi! link elixirDocString Comment

hi! link elixirStringDelimiter BruvboxGreen
hi! link elixirInterpolationDelimiter BruvboxAqua

hi! link elixirModuleDeclaration BruvboxYellow

" Scala: {{{2
" NB: scala vim syntax file is kinda horrible
hi! link scalaNameDefinition BruvboxFg1
hi! link scalaCaseFollowing BruvboxFg1
hi! link scalaCapitalWord BruvboxFg1
hi! link scalaTypeExtension BruvboxFg1

hi! link scalaKeyword BruvboxRed
hi! link scalaKeywordModifier BruvboxRed

hi! link scalaSpecial BruvboxAqua
hi! link scalaOperator BruvboxFg1

hi! link scalaTypeDeclaration BruvboxYellow
hi! link scalaTypeTypePostDeclaration BruvboxYellow

hi! link scalaInstanceDeclaration BruvboxFg1
hi! link scalaInterpolation BruvboxAqua

" Markdown: {{{2
call s:HL('markdownItalic', s:fg3, s:none, s:italic)

hi! link markdownH1 BruvboxGreenBold
hi! link markdownH2 BruvboxGreenBold
hi! link markdownH3 BruvboxYellowBold
hi! link markdownH4 BruvboxYellowBold
hi! link markdownH5 BruvboxYellow
hi! link markdownH6 BruvboxYellow

hi! link markdownCode BruvboxAqua
hi! link markdownCodeBlock BruvboxAqua
hi! link markdownCodeDelimiter BruvboxAqua

hi! link markdownBlockquote BruvboxGray
hi! link markdownListMarker BruvboxGray
hi! link markdownOrderedListMarker BruvboxGray
hi! link markdownRule BruvboxGray
hi! link markdownHeadingRule BruvboxGray

hi! link markdownUrlDelimiter BruvboxFg3
hi! link markdownLinkDelimiter BruvboxFg3
hi! link markdownLinkTextDelimiter BruvboxFg3

hi! link markdownHeadingDelimiter BruvboxOrange
hi! link markdownUrl BruvboxPurple
hi! link markdownUrlTitleDelimiter BruvboxGreen

call s:HL('markdownLinkText', s:gray, s:none, s:underline)
hi! link markdownIdDeclaration markdownLinkText

" Haskell: {{{2
" hi! link haskellType BruvboxYellow
" hi! link haskellOperators BruvboxOrange
" hi! link haskellConditional BruvboxAqua
" hi! link haskellLet BruvboxOrange
"
hi! link haskellType BruvboxFg1
hi! link haskellIdentifier BruvboxFg1
hi! link haskellSeparator BruvboxFg1
hi! link haskellDelimiter BruvboxFg4
hi! link haskellOperators BruvboxBlue
"
hi! link haskellBacktick BruvboxOrange
hi! link haskellStatement BruvboxOrange
hi! link haskellConditional BruvboxOrange

hi! link haskellLet BruvboxAqua
hi! link haskellDefault BruvboxAqua
hi! link haskellWhere BruvboxAqua
hi! link haskellBottom BruvboxAqua
hi! link haskellBlockKeywords BruvboxAqua
hi! link haskellImportKeywords BruvboxAqua
hi! link haskellDeclKeyword BruvboxAqua
hi! link haskellDeriving BruvboxAqua
hi! link haskellAssocType BruvboxAqua

hi! link haskellNumber BruvboxPurple
hi! link haskellPragma BruvboxPurple

hi! link haskellString BruvboxGreen
hi! link haskellChar BruvboxGreen

" Json: {{{2
hi! link jsonKeyword BruvboxGreen
hi! link jsonQuote BruvboxGreen
hi! link jsonBraces BruvboxFg3
hi! link jsonString BruvboxFg1


"}}}
" GraphQL: {{{2
hi! link graphqlVariable  BruvboxAqua
hi! link graphqlType      BruvboxOrange
hi! link graphqlBraces    BruvboxFg2
"}}}

hi! link CopilotSuggestion BruvboxBg3

"{{{1         Functions -------------------------------------------------------------------

" what's the point of this and why the fuck is it down here and and huh. from gruvbox. nuke?
"{{{2          Search Highlighting Cursor
function! BruvboxHlsShowCursor()
  call s:HL('Cursor', s:bg0, s:hls_cursor)
endfunction
function! BruvboxHlsHideCursor()
  call s:HL('Cursor', s:none, s:none, s:inverse)
endfunction
" }}}1
" vim: set sw=2 ts=2 sts=2 et tw=80 ft=vim fdm=marker:
