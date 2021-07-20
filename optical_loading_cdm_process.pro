function optical_loading_cdm_process, flux_line, image_lines, store_lines, serial_columns, readout_nodes, readout_time_image, readout_time_serial, trap_density_column, trap_density_filled_column, trap_density_free_column, trap_density_row, trap_density_filled_row, trap_density_free_row, ccd_mode_binning, release_image_time, capture_cross_section, charge_volume_coeff,  flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release = flag_same_pixel_release

;
;+
; NAME: optical_loading_cdm_process.pro
;	
; PURPOSE: CTI distortion of an array of flux values representing the optical loading of the SMILE CCD
;
; CATEGORY: CDM
;
; INPUTS:
;
; flux_line = Array of flux values corresponding to the optical loading in the image area and the pixels in the store section. Length = image_lines+store_lines
;
; readout_time_image = transfer period in image 
; 
; readout_time_serial = serial clocking time
;;
; decay_index = array of charge release timescales
;
; capture_cross_section = array of trap capture cross section
;
; charge_volume_coeff = single value, models volume of charge in pixel
;as a function of flux. Same value for all pixels and charge types. 
; In the Libray datatype it's an array for generality, in this
;version of the code I use first value of the array for the beta parameter
;
; OPTIONAL INPUTS:
;	
; KEYWORD PARAMETERS:
; flag_retrapping 0- Released electrons not retrapped 
;	          1- Released electrons retrapped
;
; flag_iteration 0- Losses calculated 1 time only
;                1- Use iterative process, adjusting flux during
;                   transfer with estimated losses
;
; iter_max - Max number if iterations. If not set, default to 1.
;
; threshold - Condition on successive trap losses estimates to stop iteration
; 
; short_bernoulli - 0- If estimated losses > flux => losses = flux
;                   1- Use analytical integral solution from bernoulli
;                      probability distributions to estimate losses
;
; long_bernoulli -  0- If estimated losses > flux => losses = flux
;                   1- Use analytical integral solution from bernoulli
;                      probability distributions to estimate losses
;                      for each pixel separately
;
; flag_binomial - Activate binomial treatment of losses. Function is
;                 called to simulate capture process from a binomial
;                 distribution (a bernoulli process of capture/non capture) with
;                 probability equals to the capture probability for pixels in
;                 column/row over which the charge is transferred.
;
; flag_same_pixel_release - Code will calculate the charge released in
;                           the same pixel where capture happened as
;                           well as the charge released in the
;                           following pixel. It is assumed that capture on average happens
;                           halfwary thru the packet transfer. Charge will be released in the
;                           same pixel if release within 1/2 transfer period and in the following
;                           pixel if in between [0.5-1.5] pixels transfers.
;
;  
; OUTPUTS: Array of distorted flux, representing a column or a row.
;	
; OPTIONAL OUTPUTS:
;	
; COMMON BLOCKS:
;	
; SIDE EFFECTS:
;	
; RESTRICTIONS:
;	
; NOTES:
; Retrapping - Charge released from traps can be retrapped. 
; Retrapping can be turned ON/OFF in CDM.
; With retrapping ON, previously released charge must be added to the
; model charge, so it can become trapped again
; With retrapping OFF, previously released charge is added to the
; damaged flux, and is not retrapped.
; Retrapping ON: Damaged(N) = Model(N) - Losses(N)
;                Model(N) = Model(N) + Release(N-1)
; Retrapping OFF:  Damaged(N) = Model(N) - Losses(N) + Release(N-1)
;                  Model(N) = Model(N)
;
; SUMMARY - Adaptation of CDM developed for Gaia to SMILE
;  
;For each pixel
;1 - Calculate the component of the measured flux due to released e-
;    and subtract it from the measured value
;2 - Calculate trap losses
;3 - Get reconstructed signal
;4 - Update net number of filled traps

;	
; EXAMPLE:

; optical_loading_return_array = optical_loading_process(flux_line_electrons, readout_time, trap_density, release_image_time, capture_cross_section_image, charge_volume_coeff_image, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release=flag_same_pixel_release)  
;	
; MODIFICATION HISTORY:
; 	Written by:	
;		Claudio Pagani
;
;  13 July - Function adapted from cdm_process to specifically process a line of optical loading
;Graphics
;DEVICE,DECOMPOSED=0.
;tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1
;entry_device = !d.name


;Parameters initialization
overscan_lines = 85
flux_segment_length = n_elements(flux_line) 
dimensions = size(trap_density_filled_column) ;size is IDL function, returned array dimensions will be dimensions[1] = line/column length dimensions[2] = number of trap species
if dimensions[0] eq 1 then trap_types = 1 else trap_types = dimensions[2]
flux_initial = flux_line

;As a placeholder, set the filled traps to zero at the start.
;In Java calibrations, this might be a function of the subtracted
;background, or some other initialization process
;filled_traps_array = trap_initialization_function(trap_density, background, bias etc..)
;But it might as well be that the initial status at the beginning of a
;line readout is indipendent of the observations, as all the trapped
;charge might have been released within 2 line readouts


;Arrays initialization
release_following_pixel_column = dblarr(flux_segment_length, trap_types)
release_following_pixel_row = dblarr(serial_columns/readout_nodes, trap_types)
damaged_signal_line = dblarr(n_elements(flux_initial))

;In case of only 1 trap specie the arrays must be reformatted to become formally Array[flux_segment_length, 1]
;This is needed to correctly apply total function when totalling over
;trap species with trap_types eq 1.

if trap_types eq 1 then begin
   trap_density_filled_column = reform(trap_density_filled_column, flux_segment_length, trap_types)
   trap_density_free_column = reform(trap_density_free_column, flux_segment_length, trap_types)
   release_following_pixel_column = reform(release_following_pixel_column, flux_segment_length, trap_types)
   
   trap_density_filled_row = reform(trap_density_filled_row, flux_segment_length, trap_types)
   trap_density_free_row = reform(trap_density_free_row, flux_segment_length, trap_types)
   release_following_pixel_row = reform(release_following_pixel_row, flux_segment_length, trap_types)
endif

;Total released charge for all trap species over the serial register
total_release_following_pixel_column = total(release_following_pixel_column, 2)

;Set the readout sequence over the serial register equal to the unbinned readout_time_serial clock value.
readout_sequence_serial = dblarr(serial_columns) + readout_time_serial/6
;Initialise array to store transfer periods in store section. This is Y-coordinate dependent.
readout_sequence_store = dblarr(store_lines)

readout_sequence_image = dblarr(image_lines) + readout_time_image

; PART A - PARALLEL READOUT
; 
; Flux will be distorted for each pixel in the image section. For each
; pixel, the losses, released charge, trap occupancy are calculated
; once over the column from the bottom of the store section to the pixel unbinned Y position
;
for y_counter = 0, store_lines+image_lines-1 do begin
;Determine capture probabilities and losses in each pixel for each
;trap specie

 ;;; Build the readout sequence made up of the sequence over the store section (Y-dependent) + image section.
 store_phaseA_lines = overscan_lines + round((image_lines - (y_counter-store_lines))/6)
 store_phaseB_lines = store_lines - store_phaseA_lines
 readout_sequence_store[0:store_phaseB_lines] = 385*readout_time_serial
 readout_sequence_store[store_phaseB_lines+1:store_lines-1] = readout_time_image*6
 readout_sequence = [readout_sequence_store, readout_sequence_image]
   ; Calculate losses if flux non-negligible
   ;;; Losses are calculated over the image segment and over the store segment separately and then concatenated
   ;;; The first index (index 0) in the array corresponds to the first line (bottom) of the store section.
   ;;; Index 718 is the last line of the store section
   ;;; Index 719 is the first line of the image section
   ;;; Index 4509 is the last line of the image section
   if flux_line[y_counter] gt 0. then begin
    
     

      if keyword_set(flag_iteration) then begin
        losses_pixel_image = capture_serial_java_betavol_iterate_v0(flux_line[y_counter], y_counter+1-store_lines, trap_density_free_column[store_lines:flux_segment_length-1,*], capture_cross_section, readout_sequence_image, charge_volume_coeff, total_release_following_pixel_column[store_lines:flux_segment_length-1],  flag_retrapping = flag_retrapping, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli,long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
        losses_pixel_store = capture_serial_java_betavol_iterate_v0(flux_line[y_counter]*ccd_mode_binning, store_lines, trap_density_free_column[0:store_lines-1,*], capture_cross_section, readout_sequence_store, charge_volume_coeff, total_release_following_pixel_column[0:store_lines-1],  flag_retrapping = flag_retrapping, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli,long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
        losses_pixel_column = [losses_pixel_store, losses_pixel_image]
      endif else begin
         losses_pixel_image = capture_serial_java_betavol_v0(flux_line[y_counter], y_counter+1-store_lines, trap_density_free_column[store_lines:flux_segment_length-1,*], capture_cross_section, readout_sequence_image, charge_volume_coeff, total_release_following_pixel_column[store_lines:flux_segment_length-1],  flag_retrapping = flag_retrapping, short_bernoulli = short_bernoulli,long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
         losses_pixel_store = capture_serial_java_betavol_v0(flux_line[y_counter]*ccd_mode_binning, store_lines, trap_density_free_column[0:store_lines-1,*], capture_cross_section, readout_sequence_store, charge_volume_coeff, total_release_following_pixel_column[0:store_lines-1],  flag_retrapping = flag_retrapping, short_bernoulli = short_bernoulli,long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
         losses_pixel_column = [losses_pixel_store, losses_pixel_image]
      endelse
   endif else losses_pixel_column = dblarr(flux_segment_length, trap_types)
   
   ;;; Adjust losses if greater than available flux.
   ;;; To achieve this replace losses with zero losses starting from the bottom of the column until losses are below the available flux.
   
   total_losses_column = total(losses_pixel_column)
   excess_losses = total_losses_column - flux_line[y_counter]
   if excess_losses gt 0.0 then begin
      coly = 0
      while excess_losses gt 0.0 do begin
        tot_losses_line = total(losses_pixel_column[coly, *])
        excess_losses -= tot_losses_line
        losses_pixel_column[coly, *] = 0.0
        coly += 1
      endwhile
   endif
   
   ;;; CASE 1 - COMPUTE BOTH SAME PIXEL RELEASE + FOLLOWING PIXEL RELEASE

   if flag_same_pixel_release then begin ;;; START OF SAME+FOLLOWING PIXEL treatment

                                ; PART A - Calculate release in same pixel, update trap stats.
      
      trap_density_filled_column = trap_density_filled_column + losses_pixel_column
      total_trap_density_filled_column = total(trap_density_filled_column)

      ;;; Determine charge release in same pixel if the number of
      ;;; filled traps is above a threshold
      
       if total_trap_density_filled_column gt 0.1 then begin
          release_same_pixel_column = trap_density_filled_column * (1. - exp(-rebin(0.5*readout_sequence, dimensions[1], dimensions[2]) / rebin(transpose(release_image_time), dimensions[1], dimensions[2])))
          if trap_types gt 1 then total_release_same_pixel_column = total(release_same_pixel_column,2) else total_release_same_pixel_column = release_same_pixel_column
          ;if flux_line[y_counter] gt 0. then print, 'relese same pixel column -= ', total(release_same_pixel_column)
       endif else begin
          
          ;;; If number of filled traps is below a threshold just set
          ;;; the release into the same pixel to zero.
          release_same_pixel_column = dblarr(flux_segment_length, trap_types)          
       endelse
       trap_density_filled_column = trap_density_filled_column - release_same_pixel_column
       trap_density_free_column = trap_density_free_column - losses_pixel_column + release_same_pixel_column
       total_trap_density_filled_column = total(trap_density_filled_column)
                                ; PART B - Calculate release in following pixel, update trap stats.

       total_losses_pixel_column = total(losses_pixel_column - release_same_pixel_column)
       ;;;Determine damaged signal  - Step 6
       damaged_signal_line[y_counter] = flux_line[y_counter] - total_losses_pixel_column + total(release_following_pixel_column)
       
       ;;; the trap occupancy stats have been updated.
       ;;; the charge released in the following pixel will be
       ;;; calculated only if there are filled traps left, above the
       ;;; threshold of 0.1
       if total_trap_density_filled_column gt 0.1 then begin
          release_following_pixel_column = trap_density_filled_column * (1. - exp(-rebin(readout_sequence, dimensions[1], dimensions[2]) / rebin(transpose(release_image_time), dimensions[1], dimensions[2])))
          if trap_types gt 1 then total_release_following_pixel_column = total(release_following_pixel_column,2) else total_release_following_pixel_column = release_following_pixel_column
 
   ;;; Update number of trapped electons in each pixel by each trap
   ;;; specie - Step 7, part B, relative to the next charge release
          trap_density_filled_column -= release_following_pixel_column

   ;;; Update density of free traps
          trap_density_free_column += release_following_pixel_column
       endif else begin  ;;; Case where the number of filled traps is low, below a set threshold

          ;;; If the number of filled traps is below a set threshold
          if trap_types gt 1 then total_release_following_pixel_column = total(trap_density_filled_column, 2) else total_release_following_pixel_column = trap_density_filled_column
          release_following_pixel_column = dblarr(flux_segment_length, trap_types)
          trap_density_filled_column = dblarr(flux_segment_length, trap_types)
          trap_density_free_column = trap_density_column

       endelse
       
    endif else begin ;;; END OF TREATMENT OF SAME PIXEL + FOLLOWING PIXEL RELEASE   
          
      ;;; CASE 2 - NO CHARGE RELEASED IN SAME PIXEL, FOLLOWING PIXEL RELEASE ONLY
       
          total_losses_pixel_column = total(losses_pixel_column)
   
   ;;;Determine damaged signal  - Step 6
          damaged_signal_line[y_counter] = flux_line[y_counter] - total_losses_pixel_column + total(release_following_pixel_column)
   
;;; Update number of trapped electons in each pixel by each trap
;;; specie - Step 7, part A, relative o the losses.
          trap_density_filled_column += losses_pixel_column

          total_trap_density_filled_column = total(trap_density_filled_column)
   
;;; Calculate number of released e- in following line.   Step 5
;;; Calculated only if the losses derived earlier are above a
;;; significant threshold, otherwise reset the trap statistics and set
;;; the released charge in following pixel as equal to the total
;;; trappped charge
   
          if total_trap_density_filled_column gt 0.1 then begin
   
             release_following_pixel_column = trap_density_filled_column * (1. - exp(-rebin(readout_sequence, dimensions[1], dimensions[2]) / rebin(transpose(release_image_time), dimensions[1], dimensions[2])))
             if trap_types gt 1 then total_release_following_pixel_column = total(release_following_pixel_column,2) else total_release_following_pixel_column = release_following_pixel_column
            
   ;;; Update number of trapped electons in each pixel by each trap
   ;;; specie - Step 7, part B, relative to the next charge release
             trap_density_filled_column -= release_following_pixel_column

   ;;; Update density of free traps
             trap_density_free_column += release_following_pixel_column
          endif else begin
                   ;;; If the amount of trapped charge over all the
                   ;;; column is below a threshold just realease it
                   ;;; all and reset the trap occupancy.
      
             if trap_types gt 1 then total_release_following_pixel_column = total(trap_density_filled_column, 2) else total_release_following_pixel_column = trap_density_filled_column
             release_following_pixel_column = dblarr(flux_segment_length, trap_types)
             trap_density_filled_column = dblarr(flux_segment_length, trap_types)
             trap_density_free_column = trap_density_column
             
          endelse
       endelse
       
endfor

;;; PART B - SERIAL READOUT
;;; Serial register is filled with the same amout of charge in each pixel due to contribution of optical loading.
;;;
;;; Setup the serial transfer

serial_register_readout_length = serial_columns/readout_nodes
optical_flux_serial = dblarr(serial_register_readout_length) + damaged_signal_line[store_lines+1]
readout_sequence = dblarr(serial_register_readout_length) + readout_time_serial/ccd_mode_binning
damaged_signal_row = dblarr(serial_register_readout_length)


; Damage serial row
;
for x_counter = 0, serial_register_readout_length-1 do begin

  ; Calculate losses if flux non-negligible
  if optical_flux_serial[x_counter] gt 0. then begin
    if keyword_set(flag_iteration) then losses_pixel_row =  capture_serial_java_betavol_iterate_v0(optical_flux_serial[x_counter], x_counter+1, trap_density_free_column, capture_cross_section, readout_sequence, charge_volume_coeff, total_release_following_pixel_column,  flag_retrapping = flag_retrapping, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli,long_bernoulli = long_bernoulli, flag_binomial = flag_binomial) $
    else losses_pixel_row =  capture_serial_java_betavol_v0(optical_flux_serial[x_counter], x_counter+1, trap_density_free_column, capture_cross_section, readout_sequence, charge_volume_coeff, total_release_following_pixel_column, flag_retrapping = flag_retrapping, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
  endif else losses_pixel_row = dblarr(flux_segment_length, trap_types)


  total_losses_row = total(losses_pixel_row)
  excess_losses = total_losses_row - optical_flux_serial[x_counter]
  if excess_losses gt 0.0 then begin
    colx = 0
    while excess_losses gt 0.0 do begin
      tot_losses_line = total(losses_pixel_row[colx, *])
      excess_losses -= tot_losses_line
      losses_pixel_row[colx, *] = 0.0
      colx += 1
    endwhile
  endif


  ; Is there a need to check that the losses are below the flux?
  ;;; TODO - Check this!!

  ;;; CASE 1 - COMPUTE BOTH SAME PIXEL RELEASE + FOLLOWING PIXEL RELEASE

  if flag_same_pixel_release then begin ;;; START OF SAME+FOLLOWING PIXEL treatment

    ; PART A - Calculate release in same pixel, update trap stats.

    trap_density_filled_row = trap_density_filled_row + losses_pixel_row
    total_trap_density_filled_row = total(trap_density_filled_row)

    ;;; Determine charge release in same pixel if the number of
    ;;; filled traps is above a threshold

    if total_trap_density_filled_row gt 0.1 then begin
      release_same_pixel_row = trap_density_filled_row * (1. - exp(-rebin(0.5*readout_sequence, dimensions[1], dimensions[2]) / rebin(transpose(release_image_time), dimensions[1], dimensions[2])))
      if trap_types gt 1 then total_release_same_pixel_row = total(release_same_pixel_row,2) else total_release_same_pixel_row = release_same_pixel_row
      ;if optical_flux_serial[x_counter] gt 0. then print, 'relese same pixel row -= ', total(release_same_pixel_row)
    endif else begin

      ;;; If number of filled traps is below a threshold just set
      ;;; the release into the same pixel to zero.
      release_same_pixel_row = dblarr(flux_segment_length, trap_types)
    endelse
    trap_density_filled_row = trap_density_filled_row - release_same_pixel_row
    trap_density_free_row = trap_density_free_row - losses_pixel_row + release_same_pixel_row
    total_trap_density_filled_row = total(trap_density_filled_row)
    ; PART B - Calculate release in following pixel, update trap stats.

    total_losses_pixel_row = total(losses_pixel_row - release_same_pixel_row)
    ;;;Determine damaged signal  - Step 6
    damaged_signal_row[x_counter] = optical_flux_serial[x_counter] - total_losses_pixel_row + total(release_following_pixel_row)

    ;;; the trap occupancy stats have been updated.
    ;;; the charge released in the following pixel will be
    ;;; calculated only if there are filled traps left, above the
    ;;; threshold of 0.1
    if total_trap_density_filled_row gt 0.1 then begin
      release_following_pixel_row = trap_density_filled_row * (1. - exp(-rebin(readout_sequence, dimensions[1], dimensions[2]) / rebin(transpose(release_image_time), dimensions[1], dimensions[2])))
      if trap_types gt 1 then total_release_following_pixel_row = total(release_following_pixel_row,2) else total_release_following_pixel_row = release_following_pixel_row

      ;;; Update number of trapped electons in each pixel by each trap
      ;;; specie - Step 7, part B, relative to the next charge release
      trap_density_filled_row -= release_following_pixel_row

      ;;; Update density of free traps
      trap_density_free_row += release_following_pixel_row
    endif else begin  ;;; Case where the number of filled traps is low, below a set threshold

      ;;; If the number of filled traps is below a set threshold
      if trap_types gt 1 then total_release_following_pixel_row = total(trap_density_filled_row, 2) else total_release_following_pixel_row = trap_density_filled_row
      release_following_pixel_row = dblarr(serial_register_readout_length, trap_types)
      trap_density_filled_row = dblarr(serial_register_readout_length, trap_types)
      trap_density_free_row = trap_density_row

    endelse

  endif else begin ;;; END OF TREATMENT OF SAME PIXEL + FOLLOWING PIXEL RELEASE

    ;;; CASE 2 - NO CHARGE RELEASED IN SAME PIXEL, FOLLOWING PIXEL RELEASE ONLY

    total_losses_pixel_row = total(losses_pixel_row)

    ;;;Determine damaged signal  - Step 6
    damaged_signal_row[x_counter] = optical_flux_serial[x_counter] - total_losses_pixel_row + total(release_following_pixel_row)

    ;;; Update number of trapped electons in each pixel by each trap
    ;;; specie - Step 7, part A, relative o the losses.
    trap_density_filled_row += losses_pixel_row

    total_trap_density_filled_row = total(trap_density_filled_row)

    ;;; Calculate number of released e- in following line.   Step 5
    ;;; Calculated only if the losses derived earlier are above a
    ;;; significant threshold, otherwise reset the trap statistics and set
    ;;; the released charge in following pixel as equal to the total
    ;;; trappped charge

    if total_trap_density_filled_row gt 0.1 then begin

      release_following_pixel_row = trap_density_filled_row * (1. - exp(-rebin(readout_sequence, dimensions[1], dimensions[2]) / rebin(transpose(release_image_time), dimensions[1], dimensions[2])))
      if trap_types gt 1 then total_release_following_pixel_row = total(release_following_pixel_row,2) else total_release_following_pixel_row = release_following_pixel_row

      ;;; Update number of trapped electons in each pixel by each trap
      ;;; specie - Step 7, part B, relative to the next charge release
      trap_density_filled_row -= release_following_pixel_row

      ;;; Update density of free traps
      trap_density_free_row += release_following_pixel_row
    endif else begin
      ;;; If the amount of trapped charge over all the
      ;;; row is below a threshold just realease it
      ;;; all and reset the trap occupancy.

      if trap_types gt 1 then total_release_following_pixel_row = total(trap_density_filled_row, 2) else total_release_following_pixel_row = trap_density_filled_row
      release_following_pixel_row = dblarr(serial_register_readout_length, trap_types)
      trap_density_filled_row = dblarr(serial_register_readout_length, trap_types)
      trap_density_free_row = trap_density_row

    endelse
  endelse

endfor 
 
trap_density_filled_combined = dblarr(flux_segment_length+serial_register_readout_length, trap_types)
trap_density_filled_combined[0:flux_segment_length-1, *] = trap_density_filled_column
trap_density_filled_combined[flux_segment_length:flux_segment_length+serial_register_readout_length-1, *] = trap_density_filled_row

return, trap_density_filled_combined
end

  
  


      
