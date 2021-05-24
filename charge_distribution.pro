function charge_distribution, energy_grid, rawy, lengthRawy
  ;;;
  ;;; Reads in a 9 values energy grid, representing a 3x3 binned
  ;;; pixels X-ray event, its RAWY binned coordinate and the unbinned length of
  ;;; the image section
  ;;;
  ;;; Distribute the charge over the unbinned pixels, returning it as
  ;;; an 3x6=18 columns array

  event_18columns = dblarr(18, lengthRawy)
  pixelsOverIndex = where(energy_grid gt 0., pixelsOverN)
  ;;; Distribute charge according to the pixel grade

  ;;; Grade 0, distribute charge within the central binned pixel
  if pixelsOverN eq 1 then begin
     ; Find area of charge (in mu) within binned pixel.
     xmu = fix(randomu(seed, 1)*18.*6.)
     ymu = fix(randomu(seed, 1)*18.*6.)
     rmu = abs(RANDOMN(SEED, 1)*4.)

     ; Constrain charge within central binned pixel
     xmumin = max([0,xmu-rmu])
     xmumax = min([18*6-1,xmu+rmu])
     ymumin = max([0,ymu-rmu])
     ymumax = min([18*6-1,ymu+rmu])

     ; Min and max in pixel coordinate
     xmin = fix(xmumin/18.+6)
     ymin = fix(ymumin/18.)
     xmax = fix(xmumax/18.+6)
     ymax = fix(ymumax/18.)

     ; Number of pixels with charge along X and Y
     xdiff = xmax-xmin+1
     ydiff = ymax-ymin+1
     ;print, [xmin, xmax, ymin, ymax]
     ; Split energy over available pixels
     splitEn = randomu(seed,xdiff*ydiff)
     splitEn = splitEn/total(splitEn) *  total(energy_grid[pixelsOverIndex])
     counterSplit = 0
     for indexX = xmin, xmax do begin
        for indexY = ymin, ymax do begin
           event_18columns[indexX,indexY+rawy*6] = splitEn[counterSplit]
           counterSplit+=1
        endfor
     endfor
     
  endif


;;; Double grades, assume charge is split over 2 pixels in X and Y at
;;; the boundry of two pixels.
  
  if pixelsOverN eq 2 then begin
     ;;; Grade 1 - Double up
     if energy_grid[1] gt 0. then begin
        ; Split energy in central binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[4])
        event_18columns[8,5+rawy*6] = splitEn[0]
        event_18columns[9,5+rawy*6] = splitEn[1]

        ; Split energy in above binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[1])
        event_18columns[8,6+rawy*6] = splitEn[0]
        event_18columns[9,6+rawy*6] = splitEn[1]
     endif

    ;;; Grade 2 - Double right
     if energy_grid[5] gt 0. then begin
        ; Split energy in central binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[4])
        event_18columns[11,2+rawy*6] = splitEn[0]
        event_18columns[11,3+rawy*6] = splitEn[1]

        ; Split energy in left binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[5])
        event_18columns[12,2+rawy*6] = splitEn[0]
        event_18columns[12,3+rawy*6] = splitEn[1]
     endif

     ;;; Grade 3 - Double down
     if energy_grid[7] gt 0. then begin
        ; Split energy in central binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[4])
        event_18columns[8,rawy*6] = splitEn[0]
        event_18columns[9,rawy*6] = splitEn[1]

        ; Split energy in below binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[7])
        event_18columns[8,rawy*6-1] = splitEn[0]
        event_18columns[9,rawy*6-1] = splitEn[1]
     endif

     ;;; Grade 4 - Double left
     if energy_grid[3] gt 0. then begin
        ; Split energy in central binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[4])
        event_18columns[6,rawy*6+2] = splitEn[0]
        event_18columns[6,rawy*6+3] = splitEn[1]

        ; Split energy in below binned pixel
        splitEn = randomu(seed,2)
        splitEn = splitEn/total(splitEn) *  total(energy_grid[3])
        event_18columns[5,rawy*6+2] = splitEn[0]
        event_18columns[5,rawy*6+3] = splitEn[1]
     endif
  endif


  ;;; Triple grades, assume charge is split over the 3 pixels at the
  ;;; corner 
  
  if pixelsOverN eq 3 then begin
     ;;; Grade 5 - Triple up+right
     if energy_grid[1] gt 0. and energy_grid[5] gt 0. then begin
        event_18columns[11,5+rawy*6] = energy_grid[4]
        event_18columns[11,6+rawy*6] = energy_grid[1]
        event_18columns[12,5+rawy*6] = energy_grid[5]
     endif

     ;;; Grade 6 - Triple down+right
     if energy_grid[5] gt 0. and energy_grid[7] gt 0. then begin
        event_18columns[11,rawy*6] = energy_grid[4]
        event_18columns[12,rawy*6] = energy_grid[5]
        event_18columns[11,rawy*6-1] = energy_grid[7]
     endif

     ;;; Grade 7 - Triple down+left
     if energy_grid[3] gt 0. and energy_grid[7] gt 0. then begin
        event_18columns[6,rawy*6] = energy_grid[4]
        event_18columns[6,rawy*6-1] = energy_grid[7]
        event_18columns[5,rawy*6] = energy_grid[3]
     endif

 ;;; Grade 8 - Triple up+left
     if energy_grid[1] gt 0. and energy_grid[3] gt 0. then begin
        event_18columns[6,rawy*6+5] = energy_grid[4]
        event_18columns[6,rawy*6+6] = energy_grid[1]
        event_18columns[5,rawy*6+5] = energy_grid[3]
     endif
  endif
  
     ;;; Quadruple grades, assume charge is plit over 4 pixels at the corners

  if pixelsOverN eq 4 then begin
       
     ;;; Grade 9 - Quadruple up+right
     if energy_grid[1] gt 0. and energy_grid[5] gt 0. and energy_grid[2] gt 0. then begin
        event_18columns[11,5+rawy*6] = energy_grid[4]
        event_18columns[11,6+rawy*6] = energy_grid[1]
        event_18columns[12,5+rawy*6] = energy_grid[5]
        event_18columns[12,6+rawy*6] = energy_grid[2]
     endif

          ;;; Grade 10 - Quadruple down+right
     if energy_grid[5] gt 0. and energy_grid[7] gt 0. and energy_grid[8] gt 0. then begin
        event_18columns[11,rawy*6] = energy_grid[4]
        event_18columns[11,rawy*6-1] = energy_grid[7]
        event_18columns[12,rawy*6] = energy_grid[5]
        event_18columns[12,rawy*6-1] = energy_grid[8]
     endif
     
       ;;; Grade 11 - Quadruple down+left
     if energy_grid[3] gt 0. and energy_grid[6] gt 0. and energy_grid[7] gt 0. then begin
        event_18columns[6,rawy*6] = energy_grid[4]
        event_18columns[6,rawy*6-1] = energy_grid[7]
        event_18columns[5,rawy*6] = energy_grid[3]
        event_18columns[5,rawy*6-1] = energy_grid[6]
     endif

       ;;; Grade 12 - Quadruple up+left
     if energy_grid[0] gt 0. and energy_grid[3] gt 0. and energy_grid[4] gt 0. then begin
        event_18columns[6,rawy*6+5] = energy_grid[4]
        event_18columns[6,rawy*6+6] = energy_grid[1]
        event_18columns[5,rawy*6+5] = energy_grid[3]
        event_18columns[5,rawy*6+6] = energy_grid[0]
     endif
  endif
  
     
  return, event_18columns
     
end
