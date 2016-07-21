" Private Functions {{{1
function! s:BorderExpr() "{{{2
  return tablemode#table#StartExpr() .
        \ '[' . g:table_mode_corner . g:table_mode_corner_corner . ']' .
        \ '[' . escape(g:table_mode_fillchar . g:table_mode_header_fillchar . g:table_mode_corner . g:table_mode_align_char, '-') . ']\+' .
        \ '[' . g:table_mode_corner . g:table_mode_corner_corner . ']' .
        \ tablemode#table#EndExpr()
endfunction

function! s:DefaultBorder() "{{{2
  if tablemode#IsActive()
    return g:table_mode_corner_corner . g:table_mode_fillchar . g:table_mode_corner . g:table_mode_fillchar . g:table_mode_corner_corner
  else
    return ''
  endif
endfunction

function! s:GenerateHeaderBorder(line) "{{{2
    if g:debug
        echo "calling s:GenerateHeaderBorder(), argu: " . a:line
    endif
  let line = tablemode#utils#line(a:line)
  echo "line: " . line
  if tablemode#table#IsRow(line - 1) || tablemode#table#IsRow(line + 1)
    let line_val = ''
    "if tablemode#table#IsRow(line + 1)
    "  let line_val = getline(line + 1)
    "  echo "line_val 1: " . line_val
    "endif
    "if tablemode#table#IsRow(line - 1) && tablemode#utils#strlen(line_val) < tablemode#utils#strlen(getline(line - 1))
    "  let line_val = getline(line - 1)
    "  echo "line_val 2: " . line_val
    "endif
    "echo "line_val 3: " . line_val
    let line_val = getline(g:table_mode_baseline + 1) " getline(0) returns empty
    echo "line_val 4: " . line_val
    if tablemode#utils#strlen(line_val) <= 1 | return s:DefaultBorder() | endif

    let border = substitute(line_val[stridx(line_val, g:table_mode_separator):strridx(line_val, g:table_mode_separator)], g:table_mode_separator, g:table_mode_corner, 'g')
    echo "border 1: " . border
    " To accurately deal with unicode double width characters
    if tablemode#table#IsHeader(line - 1)
      let fill_columns = map(split(border, g:table_mode_corner),  'repeat(g:table_mode_header_fillchar, tablemode#utils#StrDisplayWidth(v:val))')
    else
      let fill_columns = map(split(border, g:table_mode_corner),  'repeat(g:table_mode_fillchar, tablemode#utils#StrDisplayWidth(v:val))')
    endif
    let border = g:table_mode_corner . join(fill_columns, g:table_mode_corner) . g:table_mode_corner
    echo "border 2: " . border
    let border = substitute(border, '^' . g:table_mode_corner . '\(.*\)' . g:table_mode_corner . '$', g:table_mode_corner_corner . '\1' . g:table_mode_corner_corner, '')
    echo "border 3: " . border

    " Incorporate header alignment chars
    if getline(line) =~# g:table_mode_align_char
        if g:debug
            echo "if getline(line) =~# g:table_mode_align_char"
        endif
      let pat = '[' . g:table_mode_corner_corner . g:table_mode_corner . ']'
      let hcols = tablemode#align#Split(getline(line), pat)
      let gcols = tablemode#align#Split(border, pat)

      for idx in range(len(hcols))
          echo "hcols[idx]: " . hcols[idx]
          echo "gcols[idx]: " . gcols[idx]
        if hcols[idx] =~# g:table_mode_align_char
          " center align
          if hcols[idx] =~# g:table_mode_align_char . '[^'.g:table_mode_align_char.']\+' . g:table_mode_align_char
            let gcols[idx] = g:table_mode_align_char . gcols[idx][1:-2] . g:table_mode_align_char
          elseif hcols[idx] =~# g:table_mode_align_char . '$'
            let gcols[idx] = gcols[idx][:-2] . g:table_mode_align_char
          else
            let gcols[idx] = g:table_mode_align_char . gcols[idx][1:]
          endif
        endif
      endfor
      let border = join(gcols, '')
      echo "border: " . border
    endif

    if g:debug
        echo "let cstartexpr = tablemode#table#StartCommentExpr()"
    endif
    let cstartexpr = tablemode#table#StartCommentExpr()
    if tablemode#utils#strlen(cstartexpr) > 0 && getline(line) =~# cstartexpr
      let sce = matchstr(line_val, tablemode#table#StartCommentExpr())
      let ece = matchstr(line_val, tablemode#table#EndCommentExpr())
      echo "return sce . border . ece"
      return sce . border . ece
    elseif getline(line) =~# tablemode#table#StartExpr()
      let indent = matchstr(line_val, tablemode#table#StartExpr())
      echo "return indent . border"
      return indent . border
    else
      echo "return border"
      return border
    endif
  else
    echo "return s:DefaultBorder()"
    return s:DefaultBorder()
  endif
endfunction

" Public Functions {{{1
function! tablemode#table#GetCommentStart() "{{{2
  let cstring = &commentstring
  if tablemode#utils#strlen(cstring) > 0
    return substitute(split(cstring, '%s')[0], '[^()]', '\\\0', 'g')
  else
    return ''
  endif
endfunction

function! tablemode#table#StartCommentExpr() "{{{2
  let cstartexpr = tablemode#table#GetCommentStart()
  if tablemode#utils#strlen(cstartexpr) > 0
    return '^\s*' . cstartexpr . '\s*'
  else
    return ''
  endif
endfunction

function! tablemode#table#GetCommentEnd() "{{{2
  let cstring = &commentstring
  if tablemode#utils#strlen(cstring) > 0
    let cst = split(cstring, '%s')
    if len(cst) == 2
      return substitute(cst[1], '[^()]', '\\\0', 'g')
    else
      return ''
    endif
  else
    return ''
  endif
endfunction

function! tablemode#table#EndCommentExpr() "{{{2
  let cendexpr = tablemode#table#GetCommentEnd()
  if tablemode#utils#strlen(cendexpr) > 0
    return '.*\zs\s\+' . cendexpr . '\s*$'
  else
    return ''
  endif
endfunction

function! tablemode#table#StartExpr() "{{{2
  let cstart = tablemode#table#GetCommentStart()
  if tablemode#utils#strlen(cstart) > 0
    return '^\s*\(' . cstart . '\)\?\s*'
  else
    return '^\s*'
  endif
endfunction

function! tablemode#table#EndExpr() "{{{2
  let cend = tablemode#table#GetCommentEnd()
  if tablemode#utils#strlen(cend) > 0
    return '\s*\(\s\+' . cend . '\)\?\s*$'
  else
    return '\s*$'
  endif
endfunction

function! tablemode#table#IsBorder(line) "{{{2
  return !empty(getline(a:line)) && getline(a:line) =~# s:BorderExpr()
endfunction

function! tablemode#table#IsHeader(line) "{{{2
  let line = tablemode#utils#line(a:line)
  " if line <= 0 || line > line('$') | return 0 | endif
  return tablemode#table#IsRow(line)
        \ && !tablemode#table#IsRow(line-1)
        \ && !tablemode#table#IsRow(line-2)
        \ && !tablemode#table#IsBorder(line-2)
        \ && tablemode#table#IsBorder(line+1)
endfunction

function! tablemode#table#IsRow(line) "{{{2
  return !tablemode#table#IsBorder(a:line) && getline(a:line) =~# (tablemode#table#StartExpr() . g:table_mode_separator) . '[^' . g:table_mode_separator . ']\+'
endfunction

function! tablemode#table#IsTable(line) "{{{2
  return tablemode#table#IsRow(a:line) || tablemode#table#IsBorder(a:line)
endfunction

function! tablemode#table#AddBorder(line) "{{{2
    if g:debug
        echo "calling tablemode#table#AddBorder(), argu: " . a:line
    endif
  call setline(a:line, s:GenerateHeaderBorder(a:line))
endfunction

function! tablemode#table#Realign(line) "{{{2
    if g:debug
        echo "calling tablemode#table#Realign(line), argu: " . a:line
    endif
  let line = tablemode#utils#line(a:line)
  echo "line num: " . line

  let lines = []
  let [lnum, blines] = [line, []]
  " process the current lines and the lines wrapped before this line, may be
  " wrapped, leaving the others untouched maybe need to move the following
  " contents' line as we may add or delete some lines here
  while tablemode#table#IsTable(lnum)
    if tablemode#table#IsBorder(lnum)
      echo "lnum in if: " . lnum
      "call insert(blines, lnum)
      let lnum -= 1
      break
    endif
    echo "lnum after if: " . lnum
    call insert(lines, {'lnum': lnum, 'text': getline(lnum)})
    let lnum -= 1
  endwhile
  for ii in lines
      echo "in lines: <" . ii.lnum . "> <" . ii.text . ">"
  endfor

  " process the wrapped lines follow the current line
  let lnum = line + 1
  echo "lnum: " . lnum
  while tablemode#table#IsTable(lnum)
    if tablemode#table#IsBorder(lnum)
      echo "lnum in if: " . lnum
      "call add(blines, lnum)
      let lnum += 1
      break
    endif
    echo "lnum after if: " . lnum
    call add(lines, {'lnum': lnum, 'text': getline(lnum)})
    let lnum += 1
  endwhile
  for ii in lines
      echo "in lines: <" . ii.lnum . "> <" . ii.text . ">"
  endfor

  echo "call tablemode#align#Align(lines)"
  let [lines, offset] = tablemode#align#Align(lines)
  echo "offset in Realign: " . offset

  for aline in lines
      echo "aline.lnum: " . aline.lnum
      echo "aline.text: " . aline.text
    call setline(aline.lnum, aline.text)
  endfor

  for bline in blines
      echo "bline: " . bline
    call tablemode#table#AddBorder(bline)
  endfor
endfunction
