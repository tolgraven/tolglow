" -----------------------------------------------------------------------------
" File: bruvbox.vim
" Description: Retro groove color scheme for Vim
" Author: morhetz <morhetz@gmail.com>
" Source: https://github.com/morhetz/gruvbox
" Last Modified: 09 Apr 2014
" -----------------------------------------------------------------------------

function! bruvbox#invert_signs_toggle()
  if g:bruvbox_invert_signs == 0
    let g:bruvbox_invert_signs=1
  else
    let g:bruvbox_invert_signs=0
  endif

  colorscheme bruvbox
endfunction

" Search Highlighting {{{

function! bruvbox#hls_show()
  set hlsearch
  call BruvboxHlsShowCursor()
endfunction

function! bruvbox#hls_hide()
  set nohlsearch
  call BruvboxHlsHideCursor()
endfunction

function! bruvbox#hls_toggle()
  if &hlsearch
    call bruvbox#hls_hide()
  else
    call bruvbox#hls_show()
  endif
endfunction

" }}}

" vim: set sw=2 ts=2 sts=2 et tw=80 ft=vim fdm=marker:
