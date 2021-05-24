FUNCTION assign_grade_simple, event_binned_grid_ev, split_thresh_ev, node_side
  
  ;;; Input - event_binned_grid_keV = the event energy in the binned
  ;;;         3x3 window
  ;;;         split_thresh_ev = split event energy threshold 
  ;;;         node_side = L/R depending on left or right readout node used.
  ;;; Returns - event_binned_grid_graded_ev, graded energy in the 3x3 binned window
 
  ;;; Simple method assumes generally that most likely an event is
  ;;; single, grade 0. Therefore charge seen in pixels trailing the
  ;;; central pixels is assumed to be due to trails. Charge in other
  ;;; pixels is assumed to be genuine: 
  ;;;
  ;;; 1 - Check if the grade is valid
  ;;;
  ;;; 2 - If charge in non-valid pixels, re-assign to pixels that will
  ;;;     result in a valid grade
  ;;;

  event_binned_grid_graded_ev = event_binned_grid_ev

  index_above_split = where(event_binned_grid_graded_ev gt split_thresh_ev, n_above_split)
        ;;; Single pixel event, all charge in central pixel
  if n_above_split eq 1 and index_above_split[0] ne 4 then begin
     event_binned_grid_graded_ev[*] = 0
     event_binned_grid_graded_ev[4] = total(event_binned_grid_ev)
  endif

  case node_side of
     'L': begin
        
        ;;; Split 2 pixels-events
        if n_above_split eq 2 then begin
           
        ; Grade 1, likely trail, re-assign as Grade 0
           if index_above_split[0] eq 1 and index_above_split[1] eq 4 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif
        ; Grade 2, likely trail, re-assign as Grade 0
           if index_above_split[0] eq 4 and index_above_split[1] eq 5 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif       
        ; Grade 3, likely real
           if index_above_split[0] eq 4 and index_above_split[1] eq 7 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
              event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
           endif
          ; Grade 4, likely real
           if index_above_split[0] eq 3 and index_above_split[1] eq 4 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[3] = event_binned_grid_ev[3]
              event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
           endif
                                ; All other split events, assign charge to central pixel
           if index_above_split[0] eq 0 or index_above_split[0] eq 2 or index_above_split[1] eq 6 or index_above_split[1] eq 8 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif
        endif ;;; END OF 2PIXELS EVENTS
        
        ;;; Split 3 pixels-events
        if n_above_split eq 3 then begin
           
        ; Grade 5, likely trail, re-assign as Grade 0
           if index_above_split[0] eq 1 and index_above_split[1] eq 4  and index_above_split[2] eq 5 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif else begin
                                ; Grade 6, likely trail, re-assign as Grade 0
              if index_above_split[0] eq 4 and index_above_split[1] eq 5 and index_above_split[2] eq 7 then begin
                 likely_trails_en = total(event_binned_grid_graded_ev[4:5])
                 event_binned_grid_graded_ev[*] = 0
                 event_binned_grid_graded_ev[4] = likely_trails_en
                 event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
              endif else begin
        ; Grade 7, likely real
                 if index_above_split[0] eq 3 and index_above_split[1] eq 4 and index_above_split[2] eq 7 then begin
                    likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
                    event_binned_grid_graded_ev[*] = 0
                    event_binned_grid_graded_ev[3] = event_binned_grid_ev[3]
                    event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
                    event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
                 endif else begin
                                ; Grade 8, top pixel in [1] is likely trail
                    if index_above_split[0] eq 1 and index_above_split[1] eq 3 and index_above_split[2] eq 4 then begin
                       likely_trails_en = total(event_binned_grid_graded_ev[[1,4]])
                       event_binned_grid_graded_ev[*] = 0
                       event_binned_grid_graded_ev[3] = event_binned_grid_ev[3]
                       event_binned_grid_graded_ev[4] = likely_trails_en
                    endif else begin
                                ; All other 2-pixels split events, assign charge to central pixel
                          likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
                          event_binned_grid_graded_ev[*] = 0
                          event_binned_grid_graded_ev[4] = likely_trails_en
                       endelse
                 endelse
              endelse
           endelse
        endif ;;; END OF 3 PIXELS SPLIT EVENTS
        
        ;;; Split 4 pixels-events
        if n_above_split eq 4 then begin
           
        ; Grade 9, likely trail, re-assign as Grade 0
           if index_above_split[0] eq 1 and index_above_split[1] eq 2  and index_above_split[2] eq 4 and index_above_split[3] eq 5 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif else begin
                                ; Grade 10, likely trail, re-assign as Grade 3
              if index_above_split[0] eq 4 and index_above_split[1] eq 5  and index_above_split[2] eq 7 and index_above_split[3] eq 8 then begin
                 likely_trails_en_c = total(event_binned_grid_graded_ev[4:5])
                 likely_trails_en_b = total(event_binned_grid_graded_ev[7:8])
                 event_binned_grid_graded_ev[*] = 0
                 event_binned_grid_graded_ev[4] = likely_trails_en_c
                 event_binned_grid_graded_ev[7] = likely_trails_en_b
              endif else begin
        ; Grade 11, likely real
                 if index_above_split[0] eq 3 and index_above_split[1] eq 4  and index_above_split[2] eq 6 and index_above_split[3] eq 7 then begin                    
                    event_binned_grid_graded_ev[*] = 0
                    event_binned_grid_graded_ev[3] = event_binned_grid_ev[3]
                    event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
                    event_binned_grid_graded_ev[6] = event_binned_grid_ev[6]
                    event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
                 endif else begin
                                ; Grade 12, top pixel in [1] is likely trail
                    if index_above_split[0] eq 0 and index_above_split[1] eq 1 and index_above_split[2] eq 3 and index_above_split[3] eq 4 then begin
                       likely_trails_en_l = total(event_binned_grid_graded_ev[[0,3]])
                       likely_trails_en_c = total(event_binned_grid_graded_ev[[1,4]])
                       event_binned_grid_graded_ev[*] = 0
                       event_binned_grid_graded_ev[3] = likely_trails_en_l
                       event_binned_grid_graded_ev[4] = likely_trails_en_c
                    endif else begin
                                ; All other 2-pixels split events, assign charge to central pixel
                          likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
                          event_binned_grid_graded_ev[*] = 0
                          event_binned_grid_graded_ev[4] = likely_trails_en
                       endelse
                 endelse
              endelse
           endelse
        endif;;; END OF 4 PIXELS SPLIT EVENTS
     end ;;; END OF LEFT NODE OUTPUT
     
     'R': begin
;;; Split 2 pixels-events
        if n_above_split eq 2 then begin
           
        ; Grade 1, likely trail, re-assign as Grade 0
           if index_above_split[0] eq 1 and index_above_split[1] eq 4 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif
        ; Grade 2, likely real
           if index_above_split[0] eq 4 and index_above_split[1] eq 5 then begin
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
              event_binned_grid_graded_ev[5] = event_binned_grid_ev[5]
           endif       
        ; Grade 3, likely real
           if index_above_split[0] eq 4 and index_above_split[1] eq 7 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
              event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
           endif
          ; Grade 4, likely trail
           if index_above_split[0] eq 3 and index_above_split[1] eq 4 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif
                                ; All other split events, assign charge to central pixel
           if index_above_split[0] eq 0 or index_above_split[0] eq 2 or index_above_split[1] eq 6 or index_above_split[1] eq 8 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
           endif
        endif ;;; END OF 2PIXELS EVENTS
        
        ;;; Split 3 pixels-events
        if n_above_split eq 3 then begin
           
        ; Grade 5, likely trail in pixel [1], re-assign as Grade 2
           if index_above_split[0] eq 1 and index_above_split[1] eq 4  and index_above_split[2] eq 5 then begin
              likely_trails_en = total(event_binned_grid_graded_ev[[1,4]])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en
              event_binned_grid_graded_ev[5] = event_binned_grid_ev[5]
           endif else begin
                                ; Grade 6, likely real
              if index_above_split[0] eq 4 and index_above_split[1] eq 5 and index_above_split[2] eq 7 then begin
                 event_binned_grid_graded_ev[*] = 0
                 event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
                 event_binned_grid_graded_ev[5] = event_binned_grid_ev[5]
                 event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
              endif else begin
                                ; Grade 7, likely trail in pixel [3],
                                ; reassign it to the central pixel
                 if index_above_split[0] eq 3 and index_above_split[1] eq 4 and index_above_split[2] eq 7 then begin
                    likely_trails_en = total(event_binned_grid_graded_ev[[3,4]])
                    event_binned_grid_graded_ev[*] = 0
                    event_binned_grid_graded_ev[4] = likely_trails_en
                    event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
                 endif else begin
                                ; Grade 8, likely all trail, reassing
                                ; as Grade 0
                    if index_above_split[0] eq 1 and index_above_split[1] eq 3 and index_above_split[2] eq 4 then begin
                       likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
                       event_binned_grid_graded_ev[*] = 0
                       event_binned_grid_graded_ev[4] = likely_trails_en
                    endif else begin
                                ; All other 2-pixels split events, assign charge to central pixel
                          likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
                          event_binned_grid_graded_ev[*] = 0
                          event_binned_grid_graded_ev[4] = likely_trails_en
                       endelse
                 endelse
              endelse
           endelse
        endif ;;; END OF 3 PIXELS SPLIT EVENTS
        
        ;;; Split 4 pixels-events
        if n_above_split eq 4 then begin
           
        ; Grade 9, likely trail at the top, re-assign as Grade 2
           if index_above_split[0] eq 1 and index_above_split[1] eq 2  and index_above_split[2] eq 4 and index_above_split[3] eq 5 then begin
              likely_trails_en_c = total(event_binned_grid_graded_ev[[1,4]])
              likely_trails_en_r = total(event_binned_grid_graded_ev[[2,5]])
              event_binned_grid_graded_ev[*] = 0
              event_binned_grid_graded_ev[4] = likely_trails_en_c
              event_binned_grid_graded_ev[5] = likely_trails_en_r
           endif else begin
                                ; Grade 10, likely real
              if index_above_split[0] eq 4 and index_above_split[1] eq 5  and index_above_split[2] eq 7 and index_above_split[3] eq 8 then begin
                 likely_trails_en_c = total(event_binned_grid_graded_ev[4:5])
                 likely_trails_en_b = total(event_binned_grid_graded_ev[7:8])
                 event_binned_grid_graded_ev[*] = 0
                 event_binned_grid_graded_ev[4] = event_binned_grid_ev[4]
                 event_binned_grid_graded_ev[5] = event_binned_grid_ev[5]
                 event_binned_grid_graded_ev[7] = event_binned_grid_ev[7]
                 event_binned_grid_graded_ev[8] = event_binned_grid_ev[8]
              endif else begin
        ; Grade 11, likely trail, reassing as G3
                 if index_above_split[0] eq 3 and index_above_split[1] eq 4  and index_above_split[2] eq 6 and index_above_split[3] eq 7 then begin
                     likely_trails_en_c = total(event_binned_grid_graded_ev[[3,4]])
                     likely_trails_en_b = total(event_binned_grid_graded_ev[[6,7]])
                     event_binned_grid_graded_ev[*] = 0
                     event_binned_grid_graded_ev[4] = likely_trails_en_c
                     event_binned_grid_graded_ev[7] = likely_trails_en_b
                 endif else begin
                                ; Grade 12, top pixel in [1] is likely trail
                    if index_above_split[0] eq 0 and index_above_split[1] eq 1 and index_above_split[2] eq 3 and index_above_split[3] eq 4 then begin
                       likely_trails_en_t = total(event_binned_grid_graded_ev[[0,1]])
                       likely_trails_en_c = total(event_binned_grid_graded_ev[[3,4]])
                       event_binned_grid_graded_ev[*] = 0
                       event_binned_grid_graded_ev[1] = likely_trails_en_t
                       event_binned_grid_graded_ev[4] = likely_trails_en_c
                    endif else begin
                                ; All other 2-pixels split events, assign charge to central pixel
                          likely_trails_en = total(event_binned_grid_graded_ev[index_above_split])
                          event_binned_grid_graded_ev[*] = 0
                          event_binned_grid_graded_ev[4] = likely_trails_en
                       endelse
                 endelse              
              endelse
           endelse
        endif ;;; END OF 4 PIXELS SPLIT EVENTS
     end
  endcase

  ;;; IF MORE THAN 4 PIXELS, PUT ALL CHARGE IN CENTRAL PIXEL
        if n_above_split gt 4 then begin
           event_binned_grid_graded_ev[4] = total(event_binned_grid_ev)
        endif
  
  return, event_binned_grid_graded_ev
END

FUNCTION assign_grade_advanced, event_binned_grid_ev, split_thresh_ev, node_side
  
  ;;; Input - event_binned_grid_keV = the event energy in the binned
  ;;;         3x3 window
  ;;;         node_side = L/R depending on left or right readout node used.
  ;;; Returns - event_binned_grid_graded_ev, graded energy in the 3x3 binned window
 
  ;;; Advanced method, just a place-holder at the moment until I
  ;;; actually develp something sensible, for now the returned array
  ;;; matches the simple method.

  event_binned_grid_graded_ev = event_binned_grid_ev

  case node_side of
     'L': begin
        likely_trails_en = total(event_binned_grid_ev[[1,2,4,5]])
        event_binned_grid_graded_ev[[1,2,4,5]] = 0
     end
     'R': begin
        likely_trails_en = total(event_binned_grid_ev[[0,1,3,4]])
        event_binned_grid_graded_ev[[0,1,3,4]] = 0
     end
  endcase
  event_binned_grid_graded_ev[4] = likely_trails_en

  return, event_binned_grid_graded_ev

end

function chargeInjectionFillingNLines, flux_ci, n_lines, transfer_time, trap_pixel_density, capture_cross_section, charge_volume_coeff
;	
; PURPOSE: Returns the number of trapped electrons (losses) per pixel per trap
; species for multiple CTI lines.
;
; CATEGORY: CDM
;
; INPUTS:
; flux_ci = CI flux value
; n_lines = number of consecutive CI lines
; transfer_time = tranfer time value
; trap_pixel_density = [s] array of trap density, [s] = number of trap species.
; capture_cross_section = [s] array of trap capture cross section, [s] = number of trap species.
; charge_volume_coeff = single value, models volume of charge in pixel
;as a function of flux. Same value for all pixels and trap types.
;
; OUPUT:   
;  losses_array = Losses per pixel per trap species, array of size [s] = number of trap species.
;
; DESCRIPTION: These assumptions were made to model the effect of CI lines:
;              1 - CI lines are consecutive
;              2 - CI flux is the same for all CI lines and along the
;                  row
;              3 - There is no trapped charge released from one CI
;                  line to the next
;              4 - CI line flux is constant during its transfer, ie,
;                  it loses a small amount of charge due to traps
;                  compared to its initial flux. This wasy, the
;                  capture probability and CI volume are the same for
;                  all pixels in the image/store.  
;
; NOTE - This function is effectively equivalent to
;        chargeInjectionFilling1Line() when n_lines = 1, making the
;        1Line function obsolete.
;  
  n_species = n_elements(trap_pixel_density)
  losses_array = dblarr(n_species)
  losses_array_line = dblarr(n_species)
  trap_pixel_density_update = trap_pixel_density
  
  sbc_fwc = 2.0e+4              ; Full well capacity of the supplementary buried channel.
  flux_norm = flux_ci/sbc_fwc
  ;;; The CI flux can at fill a volume greater then the supplementary
  ;;; buried channel volume. But traps outside the SBC won't
  ;;; affect X-rays, as their charge is confined in the SBC. Therfore
  ;;; flux_norm can be 1 at maximu.  
  flux_norm = min([1, flux_norm])
  fr_capture = flux_norm^(1.-charge_volume_coeff)

  for trap_species = 0, n_species-1 do begin
     capture_prob = 1. - exp(-capture_cross_section[trap_species] * transfer_time * flux_ci^charge_volume_coeff)
     for lines = 1, n_lines do begin
        ;;; Calculate the losses for a line, add it to the total
        ;;; losses and update the empty trap density value after each
        ;;; CI line.
        losses_array_line[trap_species] = trap_pixel_density_update[trap_species] * fr_capture * capture_prob
        trap_pixel_density_update[trap_species] -= losses_array_line[trap_species]
        losses_array[trap_species] += losses_array_line[trap_species]
     endfor
  endfor

return,  losses_array
end




function chargeInjectionReleaseCharge, rawy, store_section_lines, ci_losses_pixel_species_image, readout_image_time, readout_store_time, release_image_time, release_store_time
;;;
;;; Summary - Derives the CI total (all species) released charge into
;;;           pixel detected at coordinate rawy.
;;;  
;;; Inputs -
;;;   rawy = X-ray Y coordinate, binned
;;;   store_section_lines = Lines in store frame area.
;;;   ci_losses_pixel_species_image = The amount of charge lost in a
;;;   pixel from the CI line.
;;;   readout_image_time, readout_store_time = transfer time of charge
;;;   over a pixel in image and store section respectively
;;;   release_image_time, release_store_time = double[trap_species_n],
;;;   the release timescale of the trap species in the image and store section 
;;;
;;; Output -  
;;;   total_ci_release =  doubl[3] array store the total charge relesed
;;;   from CI trapped charge in
;;;   total_ci_release[0] = preceeding y-binned pixel
;;;   total_ci_release[1] = preceeding y-binned pixel
;;;   total_ci_release[2] = preceeding y-binned pixel
        
  ci_release_preceding = dblarr(n_elements(release_image_time))
  ci_release_central = dblarr(n_elements(release_image_time))
  ci_release_following = dblarr(n_elements(release_image_time))
  
  for speciesIndex = 0, n_elements(release_image_time)-1 do begin
           ; CI total losses per specie over the segment from bottom of store to event coordinate
     ci_losses_image_tot_specie = rawy * 6 * ci_losses_pixel_species_image[speciesIndex]
     ci_losses_store_tot_specie = store_section_lines * ci_losses_pixel_species_image[speciesIndex]

; Compute charge released in Xray event preceding pixel
           ;time_interval = charge_injection_time_gap + readout_image_time * (tab[event_index].RAWY * 6 + store_section_lines)
           ;ci_released_store = ci_losses_pixel_species_store[speciesIndex] * (1. - exp(-time_interval/release_store_time[speciesIndex]))
           
     t_start = store_section_lines * readout_store_time + (rawy*6-7) * readout_image_time
     t_stop = store_section_lines * readout_store_time + (rawy*6-1) * readout_image_time           
     ci_release_preceding[speciesIndex] = ci_losses_image_tot_specie * (exp(-t_start/release_image_time[speciesIndex]) - exp(-t_stop/release_image_time[speciesIndex])) + ci_losses_store_tot_specie * (exp(-t_start/release_store_time[speciesIndex]) - exp(-t_stop/release_store_time[speciesIndex]))

           ; Compute charge released in Xray event central pixel 
     t_start = store_section_lines * readout_store_time + (rawy*6) * readout_image_time
     t_stop = store_section_lines * readout_store_time + (rawy*6+5) * readout_image_time           
     ci_release_central[speciesIndex] = ci_losses_image_tot_specie * (exp(-t_start/release_image_time[speciesIndex]) - exp(-t_stop/release_image_time[speciesIndex])) + ci_losses_store_tot_specie * (exp(-t_start/release_store_time[speciesIndex]) - exp(-t_stop/release_store_time[speciesIndex]))
           
           ; Compute charge released in X-ray event following pixel 
     t_start = store_section_lines * readout_store_time + (rawy*6+6) * readout_image_time
     t_stop = store_section_lines * readout_store_time + (rawy*6+11) * readout_image_time           
     ci_release_following[speciesIndex] = ci_losses_image_tot_specie * (exp(-t_start/release_image_time[speciesIndex]) - exp(-t_stop/release_image_time[speciesIndex])) + ci_losses_store_tot_specie * (exp(-t_start/release_store_time[speciesIndex]) - exp(-t_stop/release_store_time[speciesIndex]))
  endfor

  total_ci_release = [total(ci_release_preceding), total(ci_release_central), total(ci_release_following)]
  return, total_ci_release

end

  
FUNCTION cdm_function, xray_unbinned_18cols, rawx, rawy, transfer_length, readout_image_time, readout_store_time, readout_serial_time, trap_species_parallel, trap_species_image_density, trap_species_store_density, trap_species_serial, trap_species_serial_density, charge_injection_flag, charge_injection_time_gap, store_section_lines, ci_losses_pixel_species_image, release_image_time, release_store_time, release_serial_time, ci_released_image_pixel, capture_cross_section_image, charge_volume_coeff_image, capture_cross_section_store, charge_volume_coeff_store, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping, flag_iteration, iter_max, threshold,  short_bernoulli, long_bernoulli, rawymax, serialL, readout_nodes, serial_columns, ci_losses_pixel_species_store

  ;;; ****** Image section parallel CTI damage ******

     ;;; pCTI in image section if X-ray event not at bottom of image section
     if rawy gt 0 then begin
;        if tab[event_index].RAWY gt 1 then continue
;        if list_energy_grid[7,event_index] eq 0. then continue
     ;;; 1- Set up CTI transfer parameters and arrays.
        xray_unbinned_18cols_pcti_image = dblarr(18, transfer_length)     
        readout_time = dblarr(transfer_length) + readout_image_time

        ;;; Trap density setting, including CI release + trap density update if CI on.
        trap_density = dblarr(transfer_length, trap_species_parallel)
        
        for speciesIndex = 0, trap_species_parallel-1 do begin
           trap_density[*, speciesIndex] = trap_species_image_density[speciesIndex]
           ;;; If CI in ON, update the trap_density by adding the
           ;;; released electrons after CI capture.
           if charge_injection_flag then begin
              ; 1 - Evaluate time from CI transit to X-ray event
              time_interval = charge_injection_time_gap + readout_image_time * (rawy * 6 + store_section_lines)
                                ; 2 - Evaluate amount of trapped CI
                                ;     released e- by time_interval
                                ;                 for a pixel
              ci_released_image_pixel = ci_losses_pixel_species_image[speciesIndex] * (1. - exp(-time_interval/release_image_time[speciesIndex]))
                                ; 3 - The released charge effectively frees traps
              updated_density_value_image_species = trap_species_image_density[speciesIndex] + ci_released_image_pixel
                           ; Update trap density used for parallel image frame transfer
              trap_density[*, speciesIndex] = updated_density_value_image_species
           endif         
        endfor

     ;;; 2- Transfer of unbinned 6*3 columns.
        for columnIndex = 0, 17 do begin
           fluxSearch = where(xray_unbinned_18cols[columnIndex, *] gt 0., nFlux)
           ;;; Only call the distortion if flux in the column,
           ;;; (if not called, damaged output array is already
           ;;; correctly filled with zeros.
           if nFlux gt 0 then begin
              flux_input_electrons = xray_unbinned_18cols[columnIndex, *]*1000./3.65
              ;stop
              xray_unbinned_18cols_pcti_image[columnIndex, *] = cdm_process(flux_input_electrons, readout_time, trap_density, release_image_time, capture_cross_section_image, charge_volume_coeff_image, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli)
           endif 
        ;stop
        endfor

     ;;; ****** Bin the lines at the start of the store section.*****

     ;;; Initialise output binned array
     ;;; xray_binned_18cols_pcti_image is flux in electrons
        xray_binned_18cols_pcti_image = dblarr(18, rawymax+2)
     ;;; Binning
        for columnIndex = 0, 17 do begin
           for rawy_index = 0, rawymax+1 do begin
              xray_binned_18cols_pcti_image[columnIndex, rawy_index] = total(xray_unbinned_18cols_pcti_image[columnIndex,rawy_index*6 : (rawy_index+1)*6-1])
           endfor
        endfor

     
     ;;; ******** Store section parallel CTI damage   *******

     ;;; 1- Set up CTI transfer parameters and arrays.
     ;;; Note - cdm_process processes an array of fluxes representing
     ;;;        a segment of a column/row at once, with flux detected
     ;;;        at DET/ROW positions along the segment. In the store
     ;;;        transfer case charge has been transferred at the top
     ;;;        of the store section and binned in 6 lines. All needs
     ;;;        to be transferred thru all the store section
     ;;;        lines. The cti process needs to be set up to represent
     ;;;        this transfer. Only lines with charge at locations
     ;;;        below the X-ray event line need to be
     ;;;        processed. All lines above are not stored

        
        xray_binned_18cols_store_input = dblarr(18, store_section_lines+3)
     ;;; Populate the 3 virtual lines at the top of the store section with charge from the pCTI
     ;;; processed X-ray binned event.
        for columnIndex = 0, 17 do begin
           xray_binned_18cols_store_input[columnIndex,store_section_lines+2] = xray_binned_18cols_pcti_image[columnIndex, rawy+1]
           xray_binned_18cols_store_input[columnIndex,store_section_lines+1] = xray_binned_18cols_pcti_image[columnIndex, rawy]
           xray_binned_18cols_store_input[columnIndex,store_section_lines] = xray_binned_18cols_pcti_image[columnIndex, rawy-1]
        endfor
        
     ;;; RAXY = 0 case, no pCTI image section, processing, lines
     ;;; transferred directly into store section.
     endif else begin        
        
     ;;; If X-ray at the bottomw of the image section directly Y-bin the
     ;;; array and prepare for pCTI in store section

        xray_binned_18cols_store_input = dblarr(18, store_section_lines+3)
     ;;; Y-binning
        for columnIndex = 0, 17 do begin
           for rawy_index = 0, 2 do begin
              xray_binned_18cols_store_input[columnIndex,store_section_lines+rawy_index] = total(xray_unbinned_18cols[columnIndex,rawy_index*6 : (rawy_index+1)*6-1])*1000./3.65
           endfor
        endfor
     endelse
     
     xray_binned_18cols_pcti_store = dblarr(18, store_section_lines+3)
     readout_time = dblarr(store_section_lines+3) +readout_store_time
     ;;; Note - When CI is implemented the trap density will need to
     ;;;        be updated with the trap stats after CI charge
     ;;;        transfer, using for example an effective trap density
     ;;;        equal to the trap density of empty traps.
     trap_density = dblarr(store_section_lines+3, trap_species_parallel)
     for speciesIndex = 0, trap_species_parallel-1 do begin
        trap_density[*, speciesIndex] = trap_species_store_density[speciesIndex]

        ;;; If CI in ON, update the trap_density by adding the
        ;; released electrons after CI capture.
        if charge_injection_flag then begin
              ; 1 - Evaluate time from CI transit to X-ray even
           time_interval = charge_injection_time_gap + readout_image_time * (rawy * 6 + store_section_lines)
                                ; 2 - Evaluate amount of released e-
                                ;     by time_interval
           ci_released_store = ci_losses_pixel_species_store[speciesIndex] * (1. - exp(-time_interval/release_store_time[speciesIndex]))
              ; 3 - The released charge effectively frees traps
           updated_density_value_species = trap_species_store_density[speciesIndex] + ci_released_store
           trap_density[*, speciesIndex] = updated_density_value_species
           ;stop
        endif
                                ;Set the density of the 3 virtual
                                ;lines above the store section to to zero
        trap_density[store_section_lines:store_section_lines+2, speciesIndex] = 0.0
     endfor

     ;;; 2- Transfer of Y-binned 18*3 columns.
     for columnIndex = 0, 17 do begin
        fluxSearch = where(xray_binned_18cols_store_input[columnIndex, *] gt 0., nFlux)
           ;;; Only call the distortion if flux in the column,
           ;;; (if not called, damaged output array is already
           ;;; correctly filled with zeros.
        if nFlux gt 0 then begin
           flux_input_electrons = xray_binned_18cols_store_input[columnIndex, *]
           xray_binned_18cols_pcti_store[columnIndex, *] = cdm_process(flux_input_electrons, readout_time, trap_density, release_store_time, capture_cross_section_store, charge_volume_coeff_store, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli)
        endif
     endfor
     
     ;;; ********* Serial CTI transfer   **********

     ;;; Apply the tranfer of the three binned lines relative to the
     ;;; X-ray event

     ;;; 1 - Setup the transfer, checkin the output node.
     
     ;Array to store indices of X-ray flux
     event_index_start = rawx*6
     serial_line_preceding_in = dblarr(serialL)
     serial_line_central_in = dblarr(serialL)
     serial_line_following_in = dblarr(serialL)

     ;;; Populate the serial register with the X-ray flux values of the
     ;;; 18 columns relative to unbinned X-ray event, adding CI
     ;;; released charge if CI is on.

     ;;; Part A - If CI on, compute CI trail in pixel.
     ;;; Trail(t) = Tot_trapped_specie * 1-exp(-t/tau)

    ;;; Initialise array to store total charge releases in X-ray event
    ;;; preceding, central and following pixel

     total_ci_release = dblarr(3)     
     if charge_injection_flag then begin       
        total_ci_release = chargeInjectionReleaseCharge(rawy, store_section_lines, ci_losses_pixel_species_image, readout_image_time, readout_store_time, release_image_time, release_store_time)
        ;;; total_ci_release[0] = CI trail charge in preceeding y-binned pixel
        ;;; total_ci_release[1] = CI trail charge in preceeding y-binned pixel
        ;;; total_ci_release[2] = CI trail charge in preceeding y-binned pixel      
     endif

                
     ;;; Part B - Populate the serial register, adding CI released
     ;;;          charge if CI ON (otherwise total release will be
     ;;;          zero)
     
     for rawxIndex = 0, 17 do begin   
        serial_line_preceding_in[rawx*6+rawxIndex] = xray_binned_18cols_pcti_store[rawxIndex,store_section_lines] + total_ci_release[0]
        serial_line_central_in[rawx*6+rawxIndex] = xray_binned_18cols_pcti_store[rawxIndex,store_section_lines+1] + total_ci_release[1]
        serial_line_following_in[rawx*6+rawxIndex] = xray_binned_18cols_pcti_store[rawxIndex,store_section_lines+2] + total_ci_release[2]
     endfor

    ;;; Reformat the serial register to prepare for serial transfer
    ;;; based on node 1/2 readout and RAWX coordinate of the X-ray event

    ;;;; If 2 nodes, and the RAWX coord is past the centre, rearrange
    ;;;; the array for readout by selecting only the second part of
    ;;;; the register and reversing the array to read out from node2 
    
     if readout_nodes eq 2 and rawx * 6 ge serial_columns/2 then begin
        serial_line_preceding_in = serial_line_preceding_in[serial_columns/2:serialL-1]
        serial_line_preceding_in = reverse(serial_line_preceding_in)
       
        serial_line_central_in = serial_line_central_in[serial_columns/2:serialL-1]
        serial_line_central_in = reverse(serial_line_central_in)

        serial_line_following_in = serial_line_following_in[serial_columns/2:serialL-1]
        serial_line_following_in = reverse(serial_line_following_in)

        length_sr = n_elements(serial_line_preceding_in)
;       event_index_start = length_sr - (rawx * 6 + 17 - serial_columns/2)
        event_index_start = serialL - 1 - (rawx * 6 + 17)
                                ;store_event_index = length_sr - store_event_index
        readout_time = dblarr(serialL- serial_columns/2) +readout_serial_time
        trap_density = dblarr(serialL- serial_columns/2, trap_species_serial)
        for speciesIndex = 0, trap_species_serial-1 do begin
           trap_density[*, speciesIndex] = trap_species_serial_density[speciesIndex]
        endfor
     endif

    ;;; If 2 nodes, and RAWX is before central, readout from node1,
    ;;; select first half of register and populate serial CTI accordingly.
     if readout_nodes eq 2 and rawx * 6 lt serial_columns/2 then begin
        indexSerialArrayCut = rawx * 6 + 17

        serial_line_preceding_in = serial_line_preceding_in[0: indexSerialArrayCut]
        serial_line_central_in = serial_line_central_in[0: indexSerialArrayCut]
        serial_line_following_in = serial_line_following_in[0: indexSerialArrayCut]

        readout_time = dblarr(indexSerialArrayCut+1) +readout_serial_time
        trap_density = dblarr(indexSerialArrayCut+1, trap_species_serial)
        for speciesIndex = 0, trap_species_serial-1 do begin
           trap_density[*, speciesIndex] = trap_species_serial_density[speciesIndex]
        endfor
     endif

     if readout_nodes eq 1 then begin
        readout_time = dblarr(serialL) +readout_serial_time
        trap_density = dblarr(serialL, trap_species_serial)
        for speciesIndex = 0, trap_species_serial-1 do begin
           trap_density[*, speciesIndex] = trap_species_serial_density[speciesIndex]
        endfor
     endif
                                ; 2 - Serial transfer 

     serial_line_preceding_serial_cti = cdm_process(serial_line_preceding_in, readout_time, trap_density, release_serial_time, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli)

     serial_line_central_serial_cti = cdm_process(serial_line_central_in, readout_time, trap_density, release_serial_time, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli)

     serial_line_following_serial_cti = cdm_process(serial_line_following_in, readout_time, trap_density, release_serial_time, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli)

     ;;; Initialise output damaged 9 pixel window array
     energy_grid_cdm = dblarr(9)
     ;;; Binning of the serial line, store CTI-damaged event in output
     if readout_nodes eq 2 and rawx * 6 ge serial_columns/2 then begin
        energy_grid_cdm[0:2] = [total(serial_line_following_serial_cti[event_index_start+12:event_index_start+17]), total(serial_line_following_serial_cti[event_index_start+6:event_index_start+11]), total(serial_line_following_serial_cti[event_index_start:event_index_start+5])]
        energy_grid_cdm[3:5] = [total(serial_line_central_serial_cti[event_index_start+12:event_index_start+17]), total(serial_line_central_serial_cti[event_index_start+6:event_index_start+11]), total(serial_line_central_serial_cti[event_index_start:event_index_start+5])]
        energy_grid_cdm[6:8] = [total(serial_line_preceding_serial_cti[event_index_start+12:event_index_start+17]), total(serial_line_preceding_serial_cti[event_index_start+6:event_index_start+11]), total(serial_line_preceding_serial_cti[event_index_start:event_index_start+5])]
     endif else begin
        energy_grid_cdm[0:2] = [total(serial_line_following_serial_cti[event_index_start:event_index_start+5]), total(serial_line_following_serial_cti[event_index_start+6:event_index_start+11]), total(serial_line_following_serial_cti[event_index_start+12:event_index_start+17])]
        energy_grid_cdm[3:5] = [total(serial_line_central_serial_cti[event_index_start:event_index_start+5]), total(serial_line_central_serial_cti[event_index_start+6:event_index_start+11]), total(serial_line_central_serial_cti[event_index_start+12:event_index_start+17])]
        energy_grid_cdm[6:8] = [total(serial_line_preceding_serial_cti[event_index_start:event_index_start+5]), total(serial_line_preceding_serial_cti[event_index_start+6:event_index_start+11]), total(serial_line_preceding_serial_cti[event_index_start+12:event_index_start+17])]
     endelse

  ;;; Convert flux from electron to eV
  damaged_9pixels_array_ev = energy_grid_cdm*3.65

return, damaged_9pixels_array_ev   

end

pro correct_iter_cdm_fits, damaged_fits, iter_threshold = iter_threshold, grade_method = grade_method, DEBUG_FLAG = debug_flag, trap_density_offset = trap_density_offset, release_time_offset = release_time_offset, capture_cross_section_offset = capture_cross_section_offset, charge_volume_offset = charge_volume_offset

;;;
;;; CP, 20 Jan 2021
;;; Summary - Corrects X-ray events based on an iterative process using a CDM model
;;;  
;;; Input: fits_filename - CTI-damaged X-ray events fits file.
;;;  
;;; KEYWORD PARAMETERS:
;;;        iter_threshold = The convergence threshold of the
;;;        iterative CTI correction algorithm.
;;;        grade_method = method to assign X-ray grades, either "S"
;;;        for simple or "A" for Advanced. If not
;;;        entered the default (simple) assignment routine is used.
;;;        DEBUG_FLAG = Used in debugging mode, to print out extra
;;;        info and stop iterations if solution is diverging.  
;;;  
;;; Output: CTI corrected fits file
;;;  
;;; HISTORY - Written by CP, 20 Jan 2021
;;;           30 March 2021 - Updated with offset keywords for trap
;;;                           density, release timescale, cross
;;;                           section and charge volume
;;;                           coeffcient. The relative offsets to the
;;;                           input pars will allow the analysis of
;;;                           the calibration accuracy requirements.
;;;
;;;  
;;;  
;;;    The code has three parts:
;;;    1 - Grade assignment
;;;    2 - Assign charge in the 18x18 unbinned window
;;;    3 - CTI correction iterative process
;;;
;;;    1 - GRADES
;;;    A grade is assigned to the X-ray
;;;    event based on the chosen algorithm.
;;;    For some x-ray events the grade is unambiguous but due to CTI
;;;    losses/trails/binning in other cases the loss of information
;;;    makes the grade assignment non-deterministic.
;;;    The choice of method to assign grades can be the default one,
;;;    being the simple one described in the assign_grade function
;;;    itself or the advanced one, based on a statistical analysis of
;;;    the grade distribution as a function of Xray position, energy
;;;    and CTI level
;;;    
;;;    2 - CHARGE
;;;    With binning the original charge distribution is lost. Assign
;;;    the charge based on the grade and trying to replicate what was
;;;    done in the CDM. This would be a best-case scenario for
;;;    corrections, as in reality we won't have any info on this.
;;;    
;;;    3 - ITERATIVE CTI CORRECTION
;;;    For details, see https://space.mit.edu/ACIS/cticor.pdf
;;;      Townsley et al 2000, ApJ 534 L139 
;;;    Refines the estimate of the corrected energy by converging on
;;;    the damaged energy comparing the measured energy with the
;;;    CDM-processed energy.
;;;    The initial guess is the measured energy, to which a delta
;;;    equal to the difference between measured and cdm-damaged energy
;;;    is added at each refined step.
;;;    D = Damaged
;;;    M = Measured
;;;    C = Corrected
;;;    O = Offsets, M-D
;;;    T = Threshold  
;;;  
;;;    Hp0: C = M
;;;    C --> CDM --> D
;;;    O = M-D is O < T? IF so, we've found C, otherwise
;;;    
;;;    Hp1 C = C + O
;;;    C --> CDM --> D
;;;    O = M-D is O < T? IF so, we've found C, otherwise
;;;
;;;    Hp2 C C + 0  
;;;    C --> CDM --> D
;;;    O = M-D
;;;    And so on until we get a convergence and O<T  
;;;
;;;   EXAMPLE: correct_iter_cdm_fits,'P100001SXI0033IMEVLI0000_CI_CDM_BOL.FIT', iter_threshold = 1.0, grade_method = 'S'
  


  if n_params() ne 1 then begin
     print,"Please enter correct parameters: correct_cdm_fits, fits_filename"
     print,"Please enter correct parameters: correct_cdm_fits, 'P100001SXI0033IMEVLI0000_CDM.FIT'"
     return
  endif  

  ;;; Set and restore ccd operations mode, trap properties, cdm
  ;;; options.
  ;;; In case trap properties need updating, manually edit new values
  ;;; in setup_cdm_variables.pro
  
  setup_cdm_variables
  restore, 'distort_cdm_properties.sav'

     
  ;;; Assign default iterative correction threshold to 1 eV if not set in call
  if not(keyword_set(iter_threshold)) then iter_threshold = 1 
  
  ;;; Assign default simple grade method if not set in call
  if not(keyword_set(grade_method)) then grade_method = 'S' 
  
  ;;; Output fits name
  splitString = strsplit(damaged_fits, '.', /extract)

  fits_filename_output = splitString[0]+'_G'+strtrim(grade_method, 2)+'_corr.FIT' 
  ps_filename_output = splitString[0]+'_G'+strtrim(grade_method, 2)+'_corr.ps'

  debug_filename_output = splitString[0]+'_G'+strtrim(grade_method, 2)+'_corr_debug.txt'
  openw, lu_debug, debug_filename_output, /get_lun , WIDTH=250
  
  ;;; Offset the input parameters to study calibration requirements if
  ;;; offset keywords are set.
  ;;; Update output FITS filename 

  ;;; START
  if keyword_set(trap_density_offset) then begin
     trap_species_image_density += trap_species_image_density * trap_density_offset
     trap_species_store_density += trap_species_store_density * trap_density_offset
     trap_species_serial_density += trap_species_serial_density * trap_density_offset
     fits_filename_output = splitString[0]+'_G'+strtrim(grade_method, 2)+'_corr_offset_d'+strtrim(string(fix(trap_density_offset*100)),2)+'.FIT' 
  endif

  if keyword_set(release_time_offset) then begin
     release_image_time += release_image_time * release_time_offset
     release_store_time += release_store_time * release_time_offset
     release_serial_time += release_serial_time * release_time_offset
     fits_filename_output = splitString[0]+'_G'+strtrim(grade_method, 2)+'_corr_offset_r'+strtrim(string(fix(release_time_offset*100)),2)+'.FIT' 
  endif

  if keyword_set(capture_cross_section_offset) then begin
     capture_cross_section_image += capture_cross_section_image * capture_cross_section_offset
     capture_cross_section_store += capture_cross_section_store * capture_cross_section_offset
     capture_cross_section_serial += capture_cross_section_serial * capture_cross_section_offset
     fits_filename_output = splitString[0]+'_G'+strtrim(grade_method, 2)+'_corr_offset_c'+strtrim(string(fix(capture_cross_section_offset*100)),2)+'.FIT' 
  endif

  if keyword_set(charge_volume_offset) then begin
     charge_volume_coeff_image += charge_volume_coeff_image * charge_volume_offset
     charge_volume_coeff_store += charge_volume_coeff_store * charge_volume_offset
     charge_volume_coeff_serial += charge_volume_coeff_serial * charge_volume_offset
     fits_filename_output = splitString[0]+'_G'+strtrim(grade_method, 2)+'_corr_offset_v'+strtrim(string(fix(charge_volume_offset*100)),2)+'.FIT' 
  endif
  ;;; END  OFFSETING INPUT PARS.
 

  ;;; Read in fits file
  tab = mrdfits(damaged_fits,1,hd1)
  tab0 = mrdfits(damaged_fits,0,hd0)
  xraysTot = 0l
  xraysTot = n_elements(tab.PI)
  serialL = fix(max([tab.RAWX*6+18,serial_columns]))
  transfer_length = fix(max([(tab.RAWY)*6., image_section_lines]))+12
  rawymax = max(tab.RAWY)

  ;;; Initialise array to store the corrected 3x3 energies for all X-rays
  list_energy_grid_corrected = dblarr(9, xraysTot)

  ;;; Initialise array to store the corrected total energy in eV for all X-rays
  pi_corrected_ev = dblarr(xraysTot)

  ;;; Time the correction
  start_processing = systime(/seconds)

  ;;;  **************** CHARGE INJECTION TREATMENT  ************
  ;;;
  ;;; Evaluate the CI captured electrons and adjust the trap density accodingly.
  ;;; 

  if charge_injection_flag then begin
     ci_losses_pixel_species_image = chargeInjectionFillingNLines(charge_injection_electrons, charge_injection_block_lines, readout_image_time, trap_species_image_density, capture_cross_section_image, charge_volume_coeff_image)
     ci_losses_pixel_species_store = chargeInjectionFillingNLines(charge_injection_electrons, charge_injection_block_lines, readout_store_time, trap_species_store_density, capture_cross_section_store, charge_volume_coeff_store)
     ; The effective density of traps is lowered by the captured electrons.
     trap_species_image_density = trap_species_image_density-ci_losses_pixel_species_image
     trap_species_store_density = trap_species_store_density-ci_losses_pixel_species_store
  endif

  ;;; LOOP FOR PROCESSING OF INDIVIDUAL X-RAY EVENTS 
  ;;; START
   ; for event_index = 6868, xraysTot-1 do begin
  for event_index = 0, xraysTot-1 do begin

     measured_binned_energy = tab[event_index].PHASCTI

     ;;; Clean the window, making sure all pixels are above the split
     ;;; threshold
     index_below_split_th = where(measured_binned_energy lt detection_ll_split_eV, n_low_split)
     if n_low_split gt 0 then measured_binned_energy[index_below_split_th] = 0.0
     
     ;;; Determine if event will be readout from R or L node
     if tab[event_index].RAWX gt serialL/2 and readout_nodes eq 2 then node_side = 'R' else node_side = 'L'

     ;;; Determine the grade of the corrected event.
     case grade_method of
        'S': measured_binned_energy_graded = assign_grade_simple(measured_binned_energy, detection_ll_split_eV, node_side)
        'A': measured_binned_energy_graded = assign_grade_advanced(measured_binned_energy, detection_ll_split_eV, node_side)
     endcase
     e_measured_ev = total(measured_binned_energy_graded)
     ;;; Convert the graded eV energies to keV as the CMD code works
     ;;; in keV
     measured_binned_energy_graded = measured_binned_energy_graded/1000.
     
     ;;; Spread charge over un-binned window
     xray_unbinned_18cols = charge_distribution(measured_binned_energy_graded, tab[event_index].RAWY, transfer_length)
     pstart_index = where(xray_unbinned_18cols gt 0, npstart)
     
     ;;; Stop when the measured energy is above zero but the starting
     ;;; guess to the corrected energy is zero. This can happen if the
     ;;; charge distribution code hasn't worked propery.
     if e_measured_ev gt 0.0 and npstart eq 0 then begin
        print, 'Warning, charge splitting returns columns without electrons, likely caused by non-Xray event grade.'
        continue
     endif
      
     ;;; CDM correction iterative process
     ;;; START

     ;;; Initialise iterative method counter and losses
     counter = 0
     losses_ev = 0.
     offset_ev = 10000.
  
     repeat begin
        losses_previous = losses_ev
        offset_previous = offset_ev
        ;;; 1 - Run CDM
        cdm_damaged_3x3 = cdm_function(xray_unbinned_18cols, tab[event_index].RAWX, tab[event_index].RAWY, transfer_length, readout_image_time, readout_store_time, readout_serial_time, trap_species_parallel, trap_species_image_density, trap_species_store_density, trap_species_serial, trap_species_serial_density, charge_injection_flag, charge_injection_time_gap, store_section_lines, ci_losses_pixel_species_image, release_image_time, release_store_time, release_serial_time, ci_released_image_pixel, capture_cross_section_image, charge_volume_coeff_image, capture_cross_section_store, charge_volume_coeff_store, capture_cross_section_serial, charge_volume_coeff_serial,  flag_retrapping, flag_iteration, iter_max, threshold,  short_bernoulli, long_bernoulli, rawymax, serialL, readout_nodes, serial_columns, ci_losses_pixel_species_store)

        ;;; Clean the window, making sure all pixels are above the split
        ;;; threshold
        index_below_split_th = where(cdm_damaged_3x3 lt detection_ll_split_eV, n_low_split)
        if n_low_split gt 0 then cdm_damaged_3x3[index_below_split_th] = 0.0
     
        ;;; 2 - Compare CDM-damaged 3x3 with measured one
        offset_ev = total(measured_binned_energy_graded*1000. - cdm_damaged_3x3)
        if keyword_set(DEBUG_FLAG) then print, 'Offset (eV) = ', offset_ev
        losses_ev = total(xray_unbinned_18cols*1000.) - total(cdm_damaged_3x3)

        ;;; The difference between measured energy and CDM damaged
        ;;; energy must decrease at each iteration. Otherwise
        ;;; something went wrong with the iteratative correction
        ;;; process and we are diverging from the correct solution,
        ;;; likely a case of under correction
        if keyword_set(DEBUG_FLAG) and offset_ev gt offset_previous then stop

        ;;; If difference between measured energy and CDM damaged
        ;;; energy goes negative the losses are too large, we are
        ;;; over-correcting
        ;;;if keyword_set(DEBUG_FLAG) and offset_ev lt 0. then stop
        
        ;;; 3 - Adjust energies, increasing the energy in pixels above
        ;;;     zero after the charge spreading function.
        index_pix_gt0 = where(xray_unbinned_18cols gt 0.0, npositive)
        if keyword_set(DEBUG_FLAG) and npositive eq 0 and e_measured_ev gt 0.0  then stop
        fractional_increase_ev = (losses_ev-losses_previous)/npositive ; Charge (in KeV) is spread evenly over all pixels originally with charge.
        xray_unbinned_18cols[index_pix_gt0] = xray_unbinned_18cols[index_pix_gt0]+fractional_increase_ev/1000.

        ;;; reconstruct input 3x3 to CDM
       ; if keyword_set(DEBUG_FLAG) then begin
       ;    corrected_binned_energy = dblarr(9)
       ;    corrected_binned_energy[0] = total(xray_unbinned_18cols[0:5, 6*tab[event_index].RAWY+6 : 6*tab[event_index].RAWY+11])
       ;    corrected_binned_energy[1] = total(xray_unbinned_18cols[6:11, 6*tab[event_index].RAWY+6 : 6*tab[event_index].RAWY+11])
       ;    corrected_binned_energy[2] = total(xray_unbinned_18cols[12:17, 6*tab[event_index].RAWY+6 : 6*tab[event_index].RAWY+11])
       ;    corrected_binned_energy[3] = total(xray_unbinned_18cols[0:5, 6*tab[event_index].RAWY : 6*tab[event_index].RAWY+5])
       ;    corrected_binned_energy[4] = total(xray_unbinned_18cols[6:11, 6*tab[event_index].RAWY : 6*tab[event_index].RAWY+5])
       ;    corrected_binned_energy[5] = total(xray_unbinned_18cols[12:17, 6*tab[event_index].RAWY : 6*tab[event_index].RAWY+5])
       ;    corrected_binned_energy[6] = total(xray_unbinned_18cols[0:5, 6*tab[event_index].RAWY-6 : 6*tab[event_index].RAWY-1])
       ;    corrected_binned_energy[7] = total(xray_unbinned_18cols[6:11, 6*tab[event_index].RAWY-6 : 6*tab[event_index].RAWY-1])
       ;    corrected_binned_energy[8] = total(xray_unbinned_18cols[12:17, 6*tab[event_index].RAWY-6 : 6*tab[event_index].RAWY-1])
        ;   print, 'Input CDM, observed 3x3: '
        ;   outout = [transpose(corrected_binned_energy*1000.), transpose(tab[event_index].PHAS)]
        ;   print, outout
       ; endif    
           ;;; Save to debug file the info on the 18x18 pixels above
           ;;; threshold for the first iteration
           if keyword_set(DEBUG_FLAG) and  counter eq 0 then begin
              inputcdm_unbinned_index = where(xray_unbinned_18cols gt 0.0)
              output_string = [event_index, inputcdm_unbinned_index, fix(xray_unbinned_18cols[inputcdm_unbinned_index]*1000)]
              printf, lu_debug, output_string
           endif
        
        ;;; if DEBUG_FLAG and offset_ev lt 0. or offset_ev gt offset_previous then begin
        if keyword_set(DEBUG_FLAG) and total(measured_binned_energy_graded*1000.) gt 0.0 then begin
        ;print, 'Measured_graded_ev,  PHASCTI total_damaged_energy    Offset(M-CDM)  total_new_corrected_guess PI    Losses_ev    L-Lprev    npositive     frac_increase'
        ;print, total(measured_binned_energy_graded*1000.), total(tab[event_index].PHASCTI), total(cdm_damaged_3x3), offset_ev, total(xray_unbinned_18cols*1000.), tab[event_index].PI, losses_ev, losses_ev-losses_previous, npositive, fractional_increase_ev
                              ;   print, 'Type to continue'
          ;k = get_kbrd(10)
        endif
        counter = counter + 1  
     ;print, 'Counter, losses, e_iter, cti'
     ;stop
     endrep until (counter eq iter_max) or (offset_ev lt iter_threshold)
     ;;; CDM correction iterative process
     ;;; END

     ;;; Reconstruct the CDM-corrected, 3x3 binned window from the 18
     ;;; unbinned corrected columns, store it.
  
     corrected_binned_energy = dblarr(9)
     corrected_binned_energy[0] = total(xray_unbinned_18cols[0:5, 6*tab[event_index].RAWY+6 : 6*tab[event_index].RAWY+11])
     corrected_binned_energy[1] = total(xray_unbinned_18cols[6:11, 6*tab[event_index].RAWY+6 : 6*tab[event_index].RAWY+11])
     corrected_binned_energy[2] = total(xray_unbinned_18cols[12:17, 6*tab[event_index].RAWY+6 : 6*tab[event_index].RAWY+11])
     corrected_binned_energy[3] = total(xray_unbinned_18cols[0:5, 6*tab[event_index].RAWY : 6*tab[event_index].RAWY+5])
     corrected_binned_energy[4] = total(xray_unbinned_18cols[6:11, 6*tab[event_index].RAWY : 6*tab[event_index].RAWY+5])
     corrected_binned_energy[5] = total(xray_unbinned_18cols[12:17, 6*tab[event_index].RAWY : 6*tab[event_index].RAWY+5])
     corrected_binned_energy[6] = total(xray_unbinned_18cols[0:5, 6*tab[event_index].RAWY-6 : 6*tab[event_index].RAWY-1])
     corrected_binned_energy[7] = total(xray_unbinned_18cols[6:11, 6*tab[event_index].RAWY-6 : 6*tab[event_index].RAWY-1])
     corrected_binned_energy[8] = total(xray_unbinned_18cols[12:17, 6*tab[event_index].RAWY-6 : 6*tab[event_index].RAWY-1])
     list_energy_grid_corrected[*, event_index] = corrected_binned_energy
     pi_corrected_ev[event_index] = total(corrected_binned_energy)*1000.
     outcompa = [transpose(measured_binned_energy_graded*1000.), transpose(corrected_binned_energy*1000.), transpose(tab[event_index].PHAS)]
     print,'Measured/Corrected/Observed  3x3 window, progress = ', event_index, '/', xraysTot
     print, transpose(outcompa)
     print, 'Measured/Corrected/Observed eV energies iter_counter'
     print, total(measured_binned_energy_graded*1000.), total(corrected_binned_energy*1000.), tab[event_index].PI, counter
     ;;; Stop when the measured energy is above threshold but the
     ;;; derived CTI corrected energy is zero, and therefore something
     ;;; has not worked as expected in the code.
     if keyword_set(DEBUG_FLAG) and total(measured_binned_energy_graded*1000.) gt 0.0 and total(corrected_binned_energy*1000.) le 0.0 then stop

     ;;; Stop if the corrected energy guess is way off the PI value
     ;if keyword_set(DEBUG_FLAG) and total(measured_binned_energy_graded*1000.) gt 0.0 and abs(total(corrected_binned_energy*1000.)-tab[event_index].PI) gt 100. and counter eq 10 then begin
     ;   print, 'Correction way off! N tries ' + strtrim(string(counter),2) + ' E_corr ' + strtrim(string(total(corrected_binned_energy*1000.)),2) + ' PI ' + strtrim(string(tab[event_index].PI),2)
     ;   stop
     ;endif
   ;  stop
  endfor

  if keyword_set(DEBUG_FLAG) then close, lu_debug
  
  ;;; END X-ray events correction loop

  ;;; OUTPUT FITS FILE

  finish_processing = systime(/seconds)
  print, start_processing, format='(d15.2)'
  print, finish_processing, format='(d15.2)'
  elapsed = finish_processing-start_processing
  print, 'Processing time:', elapsed
  print, 'Writing FITS file with corrected energies now... '


  ;;; *** Save processed events to fits file  ***
  ;;;
  mwrfits, tab0, fits_filename_output, hd0, /create
  ;;; Create structure newtab to store first X-ray event data
  newtab = {EVTS, TIME: tab[0].TIME, RAWX: tab[0].RAWX, RAWY: tab[0].RAWY, DETX: tab[0].DETX, DETY: tab[0].DETY, X: tab[0].X, Y: tab[0].Y , PHA:tab[0].PHA, PI:tab[0].PI, PHAS:tab[0].PHAS, PICTI:tab[0].PICTI, PHASCTI:tab[0].PHASCTI, PICORR:pi_corrected_ev[0], PHASCORR:list_energy_grid_corrected[*, 0]*1000.0, PATTERN: tab[0].PATTERN, ENERGYE1: tab[0].ENERGYE1}
  ;;; Create structure array to store all X-ray events
  outtab = replicate({EVTS}, xraysTot)
  ;;; Populate the array of structures.
  for count= 0l, xraysTot-1 do outtab[count] = {TIME: tab[count].TIME, RAWX: tab[count].RAWX, RAWY: tab[count].RAWY, DETX: tab[count].DETX, DETY: tab[count].DETY, X: tab[count].X, Y: tab[count].Y ,PHA:tab[count].PHA, PI:tab[count].PI, PHAS:tab[count].PHAS, PICTI:tab[count].PICTI, PHASCTI:tab[count].PHASCTI, PICORR:pi_corrected_ev[count], PHASCORR:list_energy_grid_corrected[*, count]*1000.0, PATTERN: tab[count].PATTERN, ENERGYE1: tab[count].ENERGYE1 }
  mwrfits, outtab, fits_filename_output, hd1

  end


