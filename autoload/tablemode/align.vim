" Borrowed from Tabular
" Private Functions {{{1
" function! s:StripTrailingSpaces(string) - Remove all trailing spaces {{{2
" from a string.
function! s:StripTrailingSpaces(string)
    if g:debug
        echo "StripTrailingSpaces(string)" . " argu: " . a:string
        "sleep 500m
    endif
  return matchstr(a:string, '^.\{-}\ze\s*$')
endfunction

function! s:Padding(string, length, where) "{{{3
    if g:debug
        echo "Padding(string, length, where)" . " argu: <" . a:string . ">  <". a:length . ">  <". a:where . ">"
        "sleep 500m
    endif
  let gap_length = a:length - tablemode#utils#StrDisplayWidth(a:string)
  "echo "gap_length: " . gap_length
  if a:where =~# 'l'
    return a:string . repeat(" ", gap_length)
  elseif a:where =~# 'r'
    return repeat(" ", gap_length) . a:string
  elseif a:where =~# 'c'
    let right = gap_length / 2
    let left = right + (right * 2 != gap_length)
    return repeat(" ", left) . a:string . repeat(" ", right)
  endif
endfunction

" Public Functions {{{1
" function! tablemode#align#Split() - Split a string into fields and delimiters {{{2
" Like split(), but include the delimiters as elements
" All odd numbered elements are delimiters
" All even numbered elements are non-delimiters (including zero)
function! tablemode#align#Split(string, delim)
    if g:debug
    echo "calling tablemode#align#Split(), argu: " . a:string . "  delim: " . a:delim
    endif
  let rv = []
  let beg = 0

  let len = len(a:string)
  let searchoff = 0

  while 1
    let mid = match(a:string, a:delim, beg + searchoff, 1)
    if mid == -1 || mid == len
      break
    endif

    let matchstr = matchstr(a:string, a:delim, beg + searchoff, 1)
    let length = strlen(matchstr)

    if length == 0 && beg == mid
      " Zero-length match for a zero-length delimiter - advance past it
      let searchoff += 1
      continue
    endif

    if beg == mid
      let rv += [ "" ]
    else
      let rv += [ a:string[beg : mid-1] ]
    endif

    let rv += [ matchstr ]

    let beg = mid + length
    let searchoff = 0
  endwhile

  let rv += [ strpart(a:string, beg) ]

  return rv
endfunction

function! tablemode#align#alignments(lnum, ncols) "{{{2
    if g:debug
    echo "calling tablemode#align#alignments(lnum, ncols), argu: " . a:lnum . "  " . a:ncols
    endif
  let achr = g:table_mode_align_char
  "echo "achr: " . achr
  let alignments = []
  if tablemode#table#IsBorder(a:lnum+1)
    let hcols = tablemode#align#Split(getline(a:lnum+1), '[' . g:table_mode_corner . g:table_mode_corner_corner . ']')
    "echo "hcols: " . hcols
    for idx in range(len(hcols))
        "echo "idx: " . idx
      " Right align if header
      call add(alignments, 'l')
      if hcols[idx] =~# achr . '[^'.achr.']\+' . achr
        let alignments[idx] = 'c'
      elseif hcols[idx] =~# achr . '$'
        let alignments[idx] = 'r'
      endif
      " if hcols[idx] !~# '[^0-9\.]' | let alignments[idx] = 'r' | endif
    endfor
  end
  return alignments
endfunction

" Maybe one short cell line or several wrapped lines which need to be
" concatenated
function! tablemode#align#Align(lines) "{{{2
    if g:debug
    echo "calling tablemode#align#Align(lines), argu: "
    echo "number of argument is: " . len(a:lines)
    for line in a:lines
        echo line
    endfor
    endif
  if empty(a:lines) | return [] | endif
  let lines = map(a:lines, 'map(v:val, "v:key =~# \"text\" ? tablemode#align#Split(v:val, g:table_mode_separator) : v:val")')

  " there's no baseline in lines now, we get it from vim buffer
  let baseline = []
  call add(baseline, {'lnum': g:table_mode_baseline+1, 'text': getline(g:table_mode_baseline+1)})
  let baselinesplit = map(baseline, 'map(v:val, "v:key =~# \"text\" ? tablemode#align#Split(v:val, g:table_mode_separator) : v:val")')
  if g:debug
  echo "baseline: "
  for item in baselinesplit[0].text
      echo "<" . item . ">"
  endfor
  endif
  " concatenate the lines to one if needed
  let originalHeight = len(lines)
  let l:all = lines[0].text[:]
  for i in range(2, len(l:all)-1, 2)    " ignore the delimiter
      let l:all[i] = tablemode#utils#strip(l:all[i])
  endfor

  if originalHeight > 1      " to avoid cross the boundary
      for line in lines[1:]
          "echo line
          let stext = line.text
          if len(stext) <= 1 | continue | endif

          if stext[0] !~ tablemode#table#StartExpr()
              let stext[0] = s:StripTrailingSpaces(stext[0])
          endif
          if len(stext) >= 2
              for i in range(2, len(stext)-1, 2)    " ignore the delimiter
                  let stext[i] = tablemode#utils#strip(stext[i])
                  "echo "stext[i]: " . i . " <" . stext[i] . ">"
                  if !empty(stext[i])
                      let l:all[i] .= stext[i]
                  endif
              endfor
          endif
      endfor
  endif
  if g:debug
  for i in l:all
      echo "i in l:all: <" . i . ">"
  endfor
  endif

  let b:maxes = []
  let line = baselinesplit[0]
  "echo line
  let stext = line.text
  "if len(stext) <= 1 | continue | endif
  if len(stext) > 1 && empty(b:maxes)
      for i in range(len(stext))
          "echo "i: " . i
          if i == len(b:maxes)
              "let b:maxes += [ tablemode#utils#StrDisplayWidth(stext[i]) ]
              " Remove the spaces around the cell, as below will add it back
              " 这样直接减2,会出现负数，|不需要减2,只需要对单元格减2,因为没有
              " 移除其两边的空格
              "let b:maxes += [ tablemode#utils#StrDisplayWidth(stext[i]) -2 ]
              "return strdisplaywidth(a:string)
              if i == 0
                  let b:maxes += [ tablemode#utils#StrDisplayWidth(stext[i])]
              else
                  let tmp = tablemode#utils#StrDisplayWidth(stext[i])
                  if tmp <= 1   " handle the separator
                      let b:maxes += [ tablemode#utils#StrDisplayWidth(stext[i])]
                  else
                      let b:maxes += [ tablemode#utils#StrDisplayWidth(stext[i]) -2 ]
                  endif
              endif
              "echo "if, b:maxes[i]: " . b:maxes[i]
          else
              let b:maxes[i] = max([ b:maxes[i], tablemode#utils#StrDisplayWidth(stext[i]) ])
              "echo "else max, b:maxes[i]: " . b:maxes[i]
          endif
      endfor
  endif
  if g:debug
  for x in b:maxes
      echo " " . x
  endfor
  endif

  " what's this for???
  "echo "tablemode#align#alignments(lines[0].lnum, len(lines[0].text))"
  let alignments = tablemode#align#alignments(lines[0].lnum, len(l:all))
  if g:debug
  for ii in alignments
      echo "ii in alignments: " . ii
  endfor
  endif

  " To ignore the baseline row, need to keep the baseline in lines to join
  " them to a string, realign's setline()
  let offset = 0    " record the following row's offset

    let tlnum = lines[0].lnum
    let tline = l:all
    let tleftline = repeat([''], len(tline)) " 保存长于列宽的剩余数据，准备新起一行
    " 偶数位置是｜，分隔符，奇数位置是数据，需要保证分隔符就位
    for jdx in range(1, len(tline)-1, 2) " right containing is very disgusting!!!
        let tleftline[jdx] = g:table_mode_separator
    endfor
    "for ii in tleftline
    "    echo "in tleftline: <" . ii . ">"
    "endfor

    let loop = 0
    "if len(tline) <= 1 | continue | endif
    while 1
        let left = 0
        "echo "loop: " . loop
        " 遍历行内各列
        for jdx in range(len(tline))
            "echo "jdx: " . jdx
            " Dealing with the header being the first line
            if jdx >= len(alignments) | call add(alignments, 'l') | endif
            " wrap first, then padding
            if len(tline[jdx]) > b:maxes[jdx]
                "echo "cell line too long, processing it now ..."
                let left = 1
                let tleftline[jdx] = tline[jdx][b:maxes[jdx]:]
                "右包含，所以需要减1
                let tline[jdx] = tline[jdx][:b:maxes[jdx]-1]
            endif
            "echo "let field = s:Padding(tline[jdx], b:maxes[jdx], alignments[jdx])"
            let field = s:Padding(tline[jdx], b:maxes[jdx], alignments[jdx])
            "echo "field: <" . field . ">"
            let tline[jdx] = field . (jdx == 0 || jdx == len(tline) ? '' : ' ')
            if g:debug
            for ii in tline
                echo "in tline: <" . ii . ">"
            endfor
            for ii in tleftline
                echo "in tleftline: <" . ii . ">"
            endfor
            endif
        endfor

        if loop < originalHeight
            " the first one use the original line buffer, the others will be
            " put at the end
            let lines[loop].text = s:StripTrailingSpaces(join(tline, ''))
        else
            call add(lines, {'lnum': tlnum + loop, 'text': s:StripTrailingSpaces(join(tline, ''))})
        endif
        let offset = 1 + loop - originalHeight

        if left
            "let tline = tleftline  " Error, like a reference
            let tline = tleftline[:]
            "for ii in tline
            "    echo "in tline: <" . ii . ">"
            "endfor
            "let offset += 1
            " Clean the tleftline
            for jdx in range(0, len(tline)-1, 2)
                let tleftline[jdx] = ''
            endfor
        else
            break
        endif
        let loop += 1
    endwhile

  return [lines[:originalHeight+offset-1], offset]
endfunction
