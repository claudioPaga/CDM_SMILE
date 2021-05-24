pro simulate_splits
  ;;;
  ;;; CP, 21 Aug 2020
  ;;;
  ;;; Super-simple simulation to estimate the amout of split events in
  ;;; 6x6 binned pixels.
  ;;; Based on George Randall plot in email 20/8/2020, showing ~50% of
  ;;; singles in un-binned data.
  ;;; I assume the remaining 50% are doubles, with grades as seen in
  ;;; the XRT (up, down, left, right)
  ;;;
  ;;;

  eventsN = 1000
  x = fix(randomu(seed, eventsN) * 4510 + 1)
  y = fix(randomu(seed, eventsN) * 4510 + 1)

  xb = x mod 6
  yb = y mod 6

  count_split = 0

  for i = 0, eventsN-1 do begin
     ; Determine if split is up/right/down/left (1/2/3/4)
     grade = fix(randomu(seed, 1)*4)+1

     ; Count splits when charge spread over binned pixels boundary
     if xb[i] eq 0 and grade eq 4 then count_split +=1
     if xb[i] eq 5 and grade eq 2 then count_split +=1
     if yb[i] eq 0 and grade eq 1 then count_split +=1
     if yb[i] eq 5 and grade eq 3 then count_split +=1

  endfor

  print, 'Splits: ',count_split
  print, 'Splits fraction: ', count_split*1./eventsN

  end
