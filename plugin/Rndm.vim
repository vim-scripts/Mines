" Rndm:
"  Author:  Charles E. Campbell, Jr.
"  Date:    Feb 20, 2007
"  Version: 4e	ASTRO-ONLY
"
"  Discussion:  algorithm developed at MIT
"
"           <Rndm.vim> uses three pseudo-random seed variables
"              g:rndm_m1 g:rndm_m2 g:rndm_m3
"           Each should be on the interval 0 - 100,000,000
"
" RndmInit(s1,s2,s3): takes three arguments to set the three seeds (optional)
" Rndm()            : generates a pseudo-random variate on [0,100000000)
" Urndm(a,b)        : generates a uniformly distributed pseudo-random variate
"                     on the interval [a,b]
" Dice(qty,sides)   : emulates a variate from sum of "qty" user-specified
"                     dice, each of which can take on values [1,sides]
"
"Col 2:8: Be careful that you don't let anyone rob you through his philosophy
"         and vain deceit, after the tradition of men, after the elements of
"         the world, and not after Christ.

" ---------------------------------------------------------------------
"  Load Once: {{{1
if &cp || exists("loaded_Rndm")
 finish
endif
let g:loaded_Rndm = "v4e"
let s:keepcpo     = &cpo
set cpo&vim

" ---------------------------------------------------------------------
" Randomization Variables: {{{1
" with a little extra randomized start from localtime()
let g:rndm_m1 = 32007779 + (localtime()%100 - 50)
let g:rndm_m2 = 23717810 + (localtime()/86400)%100
let g:rndm_m3 = 52636370 + (localtime()/3600)%100

" ---------------------------------------------------------------------
" RndmInit: allow user to initialize pseudo-random number generator seeds {{{1
fun! RndmInit(...)
"  call Dfunc("RndmInit() a:0=".a:0)

  if a:0 >= 3
   " set seed to specified values
   let g:rndm_m1 = a:1
   let g:rndm_m2 = a:2
   let g:rndm_m3 = a:3
"   call Decho("set seeds to [".g:rndm_m1.",".g:rndm_m2.",".g:rndm_m3."]")

  elseif filereadable($HOME."/.seed")
   " initialize the pseudo-random seeds by reading the .seed file
   " when doing this, one should also save seeds at the end-of-script
   " by calling RndmSave().
   let keeplz= &lz
   let eikeep= &ei
   set lz ei=all

   1split
   exe "silent! e ".expand("$HOME")."/.seed"
   let curbuf= bufnr("%")
"   call Decho("curbuf=".curbuf." fname<".expand("%").">")
   silent! s/ /\r/g
   exe "let g:rndm_m1=".getline(1)
   exe "let g:rndm_m2=".getline(2)
   exe "let g:rndm_m3=".getline(3)
"   call Decho("set seeds to [".g:rndm_m1.",".g:rndm_m2.",".g:rndm_m3."]")
   silent! q!
   if bufexists(curbuf)
    exe curbuf."bw!"
   endif

   let &lz= keeplz
   let &ei= eikeep
  endif
"  call Dret("RndmInit")
endfun

" ---------------------------------------------------------------------
" RndmSave: this function saves the current pseudu-random number seeds {{{2
fun! RndmSave()
"  call Dfunc("RndmSave()")
  if expand("$HOME") != "" && exists("g:rndm_m1") && exists("g:rndm_m2") && exists("g:rndm_m3")
   let keeplz= &lz
   let eikeep= &ei
   set lz ei=all

   1split
   enew
   call setline(1,"".g:rndm_m1." ".g:rndm_m2." ".g:rndm_m3)
   exe "w! ".expand("$HOME")."/.seed"
   let curbuf= bufnr(".")
   silent! q!
   if curbuf > 0
    exe "silent! ".curbuf."bw!"
   endif

   let &lz= keeplz
   let &ei= eikeep
  endif
"  call Dret("RndmSave")
endfun

" ---------------------------------------------------------------------
" Rndm: generate pseudo-random variate on [0,100000000) {{{1
fun! Rndm()
  let m4= g:rndm_m1 + g:rndm_m2 + g:rndm_m3
  if( g:rndm_m2 < 50000000 )
    let m4= m4 + 1357
  endif
  if( m4 >= 100000000 )
    let m4= m4 - 100000000
    if( m4 >= 100000000 )
      let m4= m4 - 100000000
    endif
  endif
  let g:rndm_m1 = g:rndm_m2
  let g:rndm_m2 = g:rndm_m3
  let g:rndm_m3 = m4
  return g:rndm_m3
endfun

" ---------------------------------------------------------------------
" Urndm: generate uniformly-distributed pseudo-random variate on [a,b] {{{1
fun! Urndm(a,b)

  " sanity checks
  if a:b < a:a
   return 0
  endif
  if a:b == a:a
   return a:a
  endif

  " Using modulus: rnd%(b-a+1) + a  loses high-bit information
  " and makes for a poor random variate.  Following code uses
  " rejection technique to adjust maximum interval range to
  " a multiple of (b-a+1)
  let amb       = a:b - a:a + 1
  let maxintrvl = 100000000 - ( 100000000 % amb)
  let isz       = maxintrvl / amb

  let rnd= Rndm()
  while rnd > maxintrvl
   let rnd= Rndm()
  endw

  return a:a + rnd/isz
endfun

" ---------------------------------------------------------------------
" Dice: assumes one is rolling a qty of dice with "sides" sides. {{{1
"       Example - to roll 5 four-sided dice, call Dice(5,4)
fun! Dice(qty,sides)
  let roll= 0

  let sum= 0
  while roll < a:qty
   let sum = sum + Urndm(1,a:sides)
   let roll= roll + 1
  endw

  return sum
endfun

let &cpo= s:keepcpo
unlet s:keepcpo
" ---------------------------------------------------------------------
"  Modelines: {{{1
"  vim: fdm=marker
