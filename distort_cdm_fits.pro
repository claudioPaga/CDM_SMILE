FUNCTION event_splitting_smart, events_list_energies_keV, split_ll_keV

  ;;; Input - events_list_energies_keV = the list of original X-ray
  ;;;         energies, in KeV
  ;;;         split_ll = the energy lower limit for a valid energy in
  ;;;         a pixel (lower is considered noise)
  ;;; Returns - the list of split X-ray energies, in KeV, returned as
  ;;;           an array of [9,n_elements(events_list)]
  ;;; Note - Attemp at efficient way to split the events compared to
  ;;;        FUNCTION event_splitting used in distort_cti_fits.pro
  
  ;;; Splits the energies of each event based on Georges' table
  ;;; of single/doubles/triples/quadruples probabilities

  ;;; Grade splitting info
   split_energy_ev = [100,150.0,200.0,250.0,300.0,350.0,400.0,500.0,600.0,700.0,800.0,900.0,1000.0,1250.0,1500.0,1750.0,1800.0,1838,1840.0,1900.0,1950.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0,5500.0,6000.0]
   singles = [1.0,0.99944496,0.99246104	,0.972028602	,0.951572566	,0.935876794	,0.922449224	,0.90012	,0.88703	,0.88074	,0.880257605	,0.882645967	,0.889813599	,0.904895411	,0.910234142	,0.911368865	,0.913263473	,0.820366407	,0.817104997	,0.816808102	,0.817316251	,0.818198182	,0.83563004	,0.856692606	,0.869273067	,0.87486931	,0.881303336	,0.882872356	,0.881611937	,0.881277892]
   doubles =  [0.0	,0.00055504	,0.00753896	,0.027971398	,0.04841743	,0.064083204	,0.077360774	,0.09916	,0.11158	,0.11725	,0.117392348	,0.114673255	,0.1075092	,0.092571941	,0.087334121	,0.086247985	,0.084134719	,0.172153443	,0.175035404	,0.175366264	,0.17484273	,0.173791738	,0.156767762	,0.136603495	,0.124458375	,0.119324961	,0.112935666	,0.110901534	,0.112925253	,0.113173243]
   triples = [0.0, 0.0, 0.0, 0.0, 1.00E-05, 4.00E-05, 0.000160002, 0.0006	,0.0011, 0.00151, 0.001650033, 0.001820528, 0.001644423, 0.001459139, 0.001505361, 0.001515298 ,0.001581209 ,0.004190084 ,0.004531661, 0.004613322	,0.004630602	, 0.004730047	,0.004142095	,0.003849866	,0.003211261	,0.003296474	,0.00314628	,0.003071325	,0.00287409	,0.002786239]
   quadruples = [0.0, 0.0, 0.0, 0.0,	0.0, 0.0, 3.00E-05 ,0.00012	,0.00029	,0.0005	,0.000700014	,0.000860249	,0.001032778	,0.001073509	,0.000926376	,0.000867852	,0.001020599	,0.003290066	,0.003327938	,0.003212313	,0.003210417	,0.003280033	,0.003460104	,0.002854034	,0.003057297	,0.002509256	,0.002614717	,0.003154785	,0.00258872	,0.002762627]

   ;;; Array to count number of splits
   countSplitsArray = [0,0,0]
   
   ;;; Array to store the energies in the 9 pixles X-ray event 3x3
  ;;; pixel window
  ;;; Top left pixel index = 0, bottom right pixel index = 8
  ;;; Pixels 0, 1, 2 won't contribute to the downstream charge as it would
  ;;; be outside the window
  totEvents = n_elements(events_list_energies_keV)
  list_energy_grid = dblarr(9, totEvents)
  
  for countEvents = 0, totEvents - 1 do begin
     ;;; Determine if the event is a single, double, triple or
     ;;; quadruple drawing from a random uniform distrib comparing
     ;;; with George's grade probability tables.
     
     ran = randomu(seed, 1)    
     resultSingle = INTERPOL(singles, split_energy_ev, events_list_energies_keV[countEvents]*1000.)
     resultDoubles = INTERPOL(doubles, split_energy_ev, events_list_energies_keV[countEvents]*1000.)
     resultTriples = INTERPOL(triples, split_energy_ev, events_list_energies_keV[countEvents]*1000.)
     
     CASE 1 OF
        ;;; SINGLE
        (ran le  resultSingle): list_energy_grid[4,countEvents] = events_list_energies_keV[countEvents]
   
        ;;; DOUBLE
        (ran le  resultSingle+resultDoubles): BEGIN
           ;;; Check if enough energy in PHA for a split, if so, split
           ;;; energy between pixels assuring its above the split
           ;;; threshold split_ll_keV
           over_split = events_list_energies_keV[countEvents] - 2.*split_ll_keV
           if over_split gt 0. then begin
              countSplitsArray[0] +=1
              splitEn = randomu(seed,2)
              splitEn = splitEn/total(splitEn) * over_split + split_ll_keV
              list_energy_grid[4, countEvents] = max(splitEn, indexmax)
              position = fix(4.*randomu(seed, 1))
              CASE position OF
                 0: list_energy_grid[1,countEvents] = min(splitEn) ; Double up
                 1: list_energy_grid[5,countEvents] = min(splitEn) ; Double right
                 2: list_energy_grid[7,countEvents] = min(splitEn) ; Double down
                 3: list_energy_grid[3,countEvents] = min(splitEn) ; Double left
              ENDCASE
           endif else list_energy_grid[4, countEvents] = events_list_energies_keV[countEvents] ; If not enough energy for a split keep it a single event.
        END
        
        ;;;TRIPLE
        (ran le  resultSingle+resultDoubles+resultTriples): BEGIN
           ;;; Check if enough energy in PHA for a split, if so, split
           ;;; energy between pixels assuring its above the split
           ;;; threshold split_ll_keV
           over_split = events_list_energies_keV[countEvents] - 3.*split_ll_keV
           if over_split gt 0. then begin
              countSplitsArray[1] +=1
              splitEn = randomu(seed,3)
              splitEn = splitEn/total(splitEn) * over_split + split_ll_keV         
              centralValue = max(splitEn, indexMax)
              list_energy_grid[4, countEvents] = centralValue
           
           ;;; Indices of split pixels (bit convoluted, possibly there
           ;;; is a better way to get these indices.)         
              indexNoMax = [0 ne indexMax, 1 ne indexMax, 2 ne indexMax]
              indexNoMax = where(indexNoMax eq 1)
           
           ;;; Pick the pattern at random by the possible 4 valid
           ;;; 3-pixel orientations 
           position = fix(4.*randomu(seed, 1))
           CASE position OF
              0: list_energy_grid[[1,5], [countEvents,countEvents]] = splitEn[indexNoMax] ; Up and right
              1: list_energy_grid[[5,7], [countEvents,countEvents]] = splitEn[indexNoMax] ; Right and Down
              2: list_energy_grid[[7,3], [countEvents,countEvents]] = splitEn[indexNoMax] ; Down and left
              3: list_energy_grid[[3,1], [countEvents,countEvents]] = splitEn[indexNoMax] ; Left and Up
           END
           endif else list_energy_grid[4, countEvents] = events_list_energies_keV[countEvents] ; If not enough energy for a split keep it a single event.
        END

        ;;; QUADRUPLE
        ELSE: BEGIN
           ;;; Check if enough energy in PHA for a split, if so, split
           ;;; energy between pixels assuring its above the split
           ;;; threshold split_ll_keV
           over_split = events_list_energies_keV[countEvents] - 4.*split_ll_keV
           if over_split gt 0. then begin
              countSplitsArray[2] +=1
              splitEn = randomu(seed,4)
              splitEn = splitEn/total(splitEn) * over_split + split_ll_keV          
              centralValue = max(splitEn, indexMax)
              list_energy_grid[4, countEvents] = centralValue
           
           ;;; Indices of split pixels (bit convoluted, possibly there
           ;;; is a better way to get these indices.)         
              indexNoMax = [0 ne indexMax, 1 ne indexMax, 2 ne indexMax, 3 ne indexMax]
              indexNoMax = where(indexNoMax eq 1)

              ;;; Pick the pattern at random (4 valid 4-pixels orientations)
              position = fix(4.*randomu(seed, 1))
              CASE position OF
                 0: list_energy_grid[[1,2,5], [countEvents,countEvents,countEvents]] = splitEn[indexNoMax] ; Top right box
                 1: list_energy_grid[[5,7,8], [countEvents,countEvents,countEvents]] = splitEn[indexNoMax] ; Bottom right box
                 2: list_energy_grid[[3,6,7], [countEvents,countEvents,countEvents]] = splitEn[indexNoMax] ; Bottom left box
                 3: list_energy_grid[[0,1,3], [countEvents,countEvents,countEvents]] = splitEn[indexNoMax] ; Top left obx
              END
              endif else list_energy_grid[4, countEvents] = events_list_energies_keV[countEvents]; If not enough energy for a split keep it a single event.
           END
     ENDCASE
  endfor

  print, 'Singles, Doubles, Triples, Quadruples = ' , totEvents - total(countSplitsArray), ' ', countSplitsArray[0], ' ', countSplitsArray[1],' ', countSplitsArray[2]
  return, list_energy_grid

end



function chargeInjectionFilling1Line, flux_ci, transfer_time, trap_pixel_density, capture_cross_section, charge_volume_coeff
;	
; PURPOSE: Returns the number of trapped electrons (losses) per pixel per trap
; species for a single CTI line.
;
; CATEGORY: CDM
;
; INPUTS:
;
; flux_ci = CI flux value
;
; transfer_time = tranfer time value
;
; trap_pixel_density = [s] array of trap density, [s] = number of trap species.
;
; capture_cross_section = [s] array of trap capture cross section, [s] = number of trap species.
;
; charge_volume_coeff = single value, models volume of charge in pixel
;as a function of flux. Same value for all pixels and trap types.
;
; OUPUT:   
;
;  losses_array = Losses per pixel per trap species, array of size [s] = number of trap species.
  
  n_species = n_elements(trap_pixel_density)
  losses_array = dblarr(n_species)
  
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
     losses_array[trap_species] = trap_pixel_density[trap_species] * fr_capture * capture_prob
  endfor

return,  losses_array
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

  


FUNCTION cdm_function, xray_unbinned_18cols, rawx, rawy, transfer_length, readout_image_time, readout_store_time, readout_serial_time, trap_species_parallel, trap_species_image_density, trap_species_store_density, trap_species_serial, trap_species_serial_density, charge_injection_flag, charge_injection_time_gap, store_section_lines, ci_losses_pixel_species_image, release_image_time, release_store_time, release_serial_time, ci_released_image_pixel, capture_cross_section_image, charge_volume_coeff_image, capture_cross_section_store, charge_volume_coeff_store, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping, flag_iteration, iter_max, threshold,  short_bernoulli, long_bernoulli, flag_binomial, rawymax, serialL, readout_nodes, serial_columns, ci_losses_pixel_species_store

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
              xray_unbinned_18cols_pcti_image[columnIndex, *] = cdm_process(flux_input_electrons, readout_time, trap_density, release_image_time, capture_cross_section_image, charge_volume_coeff_image, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
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
           xray_binned_18cols_pcti_store[columnIndex, *] = cdm_process(flux_input_electrons, readout_time, trap_density, release_store_time, capture_cross_section_store, charge_volume_coeff_store, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
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

     serial_line_preceding_serial_cti = cdm_process(serial_line_preceding_in, readout_time, trap_density, release_serial_time, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)

     serial_line_central_serial_cti = cdm_process(serial_line_central_in, readout_time, trap_density, release_serial_time, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)

     serial_line_following_serial_cti = cdm_process(serial_line_following_in, readout_time, trap_density, release_serial_time, capture_cross_section_serial, charge_volume_coeff_serial, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)

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





function cti_process, events_list_energies_keV, rawx, rawy, f_parallel_trap_density_pixel, f_serial_trap_density_pixel, f_cti_alpha, f_cti_beta, f_serial_cti_alpha, f_serial_cti_beta, f_key_release

;;;
;;; Input = List of energies in a specific pixel position of the 3x3
;;; window
;;; Output = 3 x n_events array that includes:
;;;          Damaged pixels
;;;          Parallel downstream pixel trailing charge
;;;          Serial downstream pixel trailing charge
;;;
 ; COMMON shareCTI, trap_species_fraction_parallel, trap_species_release_constant_parallel, transfer_time_parallel, trap_species_fraction_serial, trap_species_release_constant_serial, transfer_time_serial, cti_alpha, cti_beta, cti_corr_alpha, cti_corr_beta, serial_cti_alpha, serial_cti_beta, serial_cti_corr_alpha, serial_cti_corr_beta, pixel_binning, key_release

  ;;; Initialise trap parameters
  pixel_binning = 6
  trap_species_fraction_parallel = [0.6, 0.4]
  trap_species_release_constant_parallel = [1., 10.]
  transfer_time_parallel = 1.
  trap_species_fraction_serial = [0.5, 0.1]
  trap_species_release_constant_serial = [1., 10.]
  transfer_time_serial = 1.

  ;;; Spread 
  
  list_energies = events_list_energies_keV
    
  xraysTot = n_elements(list_energies)
  list_energies_cti = dblarr(xraysTot)
  losses_array_ev = dblarr(xraysTot)
  parallel_recovered_kev_stored = dblarr(xraysTot)
  parallel_downstream_kev = dblarr(xraysTot)
  parallel_downstream_kev_stored = dblarr(xraysTot)
  serial_recovered_kev_stored = dblarr(xraysTot)
  serial_downstream_kev = dblarr(xraysTot)
  serial_downstream_kev_stored = dblarr(xraysTot)
  serial_cti_damaged_stored = dblarr(xraysTot)
  
 ;;; *** PARALLEL TRANSFER, IMAGE FRAME'

  
 ;;; Simple model of electron capture model.
 ;;; capture_prob = prob of capturing an electron from a charge packet
 ;;; of size list_energies
 ;;; losses_mean = expected electron losses, given the capture
 ;;; probability, the number of transfers and the density of traps in
 ;;; a pixel
 ;;; losses_sigma = standard deviation of expected caputes (model the
 ;;; probabilistic nature of trapping)
 
 capture_prob = 1. - exp(-f_cti_alpha * list_energies^(1-f_cti_beta))

                                ;Distribution of number of transfers
                                ;(this corresponds to the DETY
                                ;position of the X-ray event. The
                                ;assumption is of a uniform
                                ;distribution along the column.
 ;;; Transform the input DETY from binned Row Coordinates to Pixel
 ;;; coordinates, randomising its location within the 6 pixels, and
 ;;; assuming all charge is in a pixel (not realistic)
 
 n_transfers = rawy*6 + fix(randomu(Seed, xraysTot)*6) + 1
 ;;; Losses in electrons
 losses_mean = n_transfers * capture_prob * f_parallel_trap_density_pixel 
 losses_sigma = sqrt(n_transfers * f_parallel_trap_density_pixel * capture_prob * (1. - capture_prob))
 random_array = randomn(seed, n_elements(list_energies))
 ;;; Losses in eV
 losses_array_ev = (random_array * losses_sigma + losses_mean) * 3.65 

                                ;Avoid "negative" losses (can be
                                ;caused by low losses_mean value and a
                                ;large losses_sigma value
 index_l0 = where(losses_array_ev lt 0., nl0)
 if nl0 gt 0 then losses_array_ev[index_l0] = 0
 
;;; Apply parallel losses, avoid negative energies replacing negative
;;; values with zeros and replacing the losses with the total energy
;;; in that pixel
 list_energies_cti  = (list_energies*1000. - losses_array_ev)/1000. ; Damaged flux in keV
 index_neg = where(list_energies_cti lt 0., nneg)
 if nneg gt 0 then begin
    list_energies_cti[index_neg] = 0.0
    losses_array_ev[index_neg] = list_energies[index_neg]*1000.
 endif
 list_energies_cti_pre_recovery = list_energies_cti 
 
  if f_key_release then begin
    ;;; Number of pixels [0-5] within which released charge will be
    ;;; within the 6 pixel binning and added back to the initial energy.
    pixels_released = n_transfers mod pixel_binning
    parallel_en_losses_kev = list_energies - list_energies_cti
                                ;Sum recoveries over the trap species
    for counter_species_parallel = 0, n_elements(trap_species_fraction_parallel) - 1 do begin
       parallel_charge_released_within_binning =  trap_species_fraction_parallel[counter_species_parallel]  * parallel_en_losses_kev * (1. - exp(pixels_released*transfer_time_parallel/ trap_species_release_constant_parallel[counter_species_parallel]))
       parallel_recovered_kev_stored += parallel_charge_released_within_binning
    endfor
    list_energies_cti  += parallel_recovered_kev_stored
 endif
 
    ;;; ********** Release of charge in downstream 6x-binned pixel **********

    ;;; To contribute to downstream pixel, the released charge will
    ;;; need to be released within [pixel_released - pixel_released+6]
    ;;; Formula (1-exp(-pixel_released + 6 /tau)) - (1-exp(-pixel_released/tau)) 
    ;;; = exp(-pixel_released/ tau) - exp(-pixel_released+6/ tau)

    ;;; Sum over the trap species of the charge released in downstream pixels
    for counter_species_parallel = 0, n_elements(trap_species_fraction_parallel) - 1 do begin
       parallel_charge_released_downstream =  trap_species_fraction_parallel[counter_species_parallel]  * parallel_en_losses_kev * (exp(pixels_released* transfer_time_parallel/ trap_species_release_constant_parallel[counter_species_parallel]) - exp((6+pixels_released)* transfer_time_parallel/ trap_species_release_constant_parallel[counter_species_parallel]))
       parallel_downstream_kev += parallel_charge_released_downstream
    endfor
    
    ;;; Charge at this stage is transferred to the store section ,
    ;;; accumulated over 6 pixels and transferred over 719 more rows.

    ;;; list_energies_cti already represents the energy in the 6
    ;;; pixels and simply needs to be trasferred to the bottom of the
    ;;; store section

    ;;; *********** Apply store section parallel CTI losses ********

    capture_prob = 1. - exp(-f_cti_alpha * list_energies_cti^(1-f_cti_beta))

                                ;Distribution of number of transfers
                                ;(this corresponds to the DETY
                                ;position of the X-ray event. The
                                ;assumption is of a uniform
                                ;distribution along the column.
 ;;; Transform the input DETY from binned Row Coordinates to Pixel
 ;;; coordinates, randomising its location within the 6 pixels, and
 ;;; assuming all charge is in a pixel (not realistic)
 
 n_transfers_framestore = 719
 ;;; Losses in electrons
 losses_mean = n_transfers_framestore * capture_prob * f_parallel_trap_density_pixel 
 losses_sigma = sqrt(n_transfers_framestore * f_parallel_trap_density_pixel * capture_prob * (1. - capture_prob))
 random_array = randomn(seed, n_elements(list_energies_cti))
 ;;; Losses in eV
 losses_array_ev = (random_array * losses_sigma + losses_mean) * 3.65 

                                ;Avoid "negative" losses (can be
                                ;caused by low losses_mean value and a
                                ;large losses_sigma value
 index_l0 = where(losses_array_ev lt 0., nl0)
 if nl0 gt 0 then losses_array_ev[index_l0] = 0
 
 ;;; Apply parallel losses, avoid negative energies
 
 list_energies_cti_framestore  = (list_energies_cti*1000. - losses_array_ev)/1000. ; Damaged flux in keV
 index_neg = where(list_energies_cti_framestore lt 0., nneg)
 if nneg gt 0 then begin
    list_energies_cti_framestore[index_neg] = 0.0
    losses_array_ev[index_neg] = list_energies_cti[index_neg]*1000.
 endif
 list_energies_cti_pre_recovery_framestore = list_energies_cti_framestore

  ;;; Losses (keV) during frame store parallel transfers.
  parallel_en_losses_framestore_kev = list_energies_cti - list_energies_cti_framestore

   
  ;;; Downstream pixel - Derive electrons released in downstream pixel
  ;;;                    from charge lost during store parallel
  ;;;                    transfers and add it to the flux in the
  ;;;                    downstream pixel.   

  ;;; Sum over the trap species of the charge released in downstream
  ;;; pixel
  ;;; In store section this should simply correspond to the following 1
  ;;; pixel, so it's charge released within 1 transfer
  for counter_species_parallel = 0, n_elements(trap_species_fraction_parallel) - 1 do begin
     parallel_charge_released_downstream =  trap_species_fraction_parallel[counter_species_parallel]  * parallel_en_losses_framestore_kev * (1. - exp(-transfer_time_parallel/ trap_species_release_constant_parallel[counter_species_parallel]))
     parallel_downstream_kev += parallel_charge_released_downstream
  endfor

  ;;; list_energies_cti_framestore ==> energies after image + store sections
  ;;; parallel_downstream_kev  ==> charge accumulated in downstream pixel.
 
 ;;; Apply serial CTI distortion

 ;;; Charge is transferred using two nodes, so a max of 2250 serial
 ;;; transfers occour

 ;;; Transform the input DETY from binned Row Coordinates to Pixel
 ;;; coordinates, randomising its location within the 6 pixels, and
 ;;; assuming all charge is in a pixel (not realistic)
 ;;; Calculate the number of serial transfers from the closest node,
 ;;; store it in serial_n_transfers.

  RAWX_unbinned = rawx * 6 + fix(randomu(Seed, xraysTot)*6) + 1
  serial_n_transfers = intarr(xraysTot)
  for xrayCounter = 0, xraysTot-1 do serial_n_transfers[xrayCounter] = min([RAWX_unbinned[xrayCounter], 4510-RAWX_unbinned[xrayCounter]])


 serial_capture_prob = 1. - exp(-f_serial_cti_alpha * list_energies_cti_framestore^(1-f_serial_cti_beta))
 serial_losses_mean = serial_n_transfers * serial_capture_prob * f_serial_trap_density_pixel 
 serial_losses_sigma = sqrt(serial_n_transfers * f_serial_trap_density_pixel * serial_capture_prob * (1. - serial_capture_prob))
 serial_random_array = randomn(seed, n_elements(list_energies_cti))
 serial_losses_array_ev = (serial_random_array * serial_losses_sigma + serial_losses_mean) * 3.65
                                  ;Avoid "negative" losses (can be
                                ;caused by low losses_mean value and a
                                ;large losses_sigma value
 index_l0 = where(serial_losses_array_ev lt 0., nl0)
 if nl0 gt 0 then serial_losses_array_ev[index_l0] = 0
 
 ;;; Apply serial losses, avoid negative energies
 list_energies_cti  = (list_energies_cti_framestore*1000. - serial_losses_array_ev)/1000.
 index_neg = where(list_energies_cti lt 0., nneg)
 if nneg gt 0 then begin
    list_energies_cti[index_neg] = 0.0
    serial_losses_array_ev[index_neg] = list_energies_cti[index_neg]*1000.
 endif
 
 list_energies_cti_pre_serial_recovery = list_energies_cti_framestore
 
; Recovery of release charge in binning
 if f_key_release then begin
    pixels_released = serial_n_transfers mod pixel_binning
    serial_en_losses_kev = list_energies_cti_framestore - list_energies_cti
                                ;Sum recoveries over the trap species
    for counter_species_serial = 0, n_elements(trap_species_fraction_serial) - 1 do begin
       serial_charge_released_within_binning =  trap_species_fraction_serial[counter_species_serial]  * serial_en_losses_kev * (1. - exp(-pixels_released*transfer_time_serial/ trap_species_release_constant_serial[counter_species_serial]))
       serial_recovered_kev_stored += serial_charge_released_within_binning
    endfor
    list_energies_cti  += serial_recovered_kev_stored
 endif
 
  ;;; ********** Release of charge in downstream serial 6x-binned pixel **********

    ;;; To contribute to downstream pixel, the released charge will
    ;;; need to be released within [pixel_released - pixel_released+6]
    ;;; Formula (1-exp(-pixel_released + 6 /tau)) - (1-exp(-pixel_released/tau)) 
    ;;; = exp(-pixel_released/ tau) - exp(-pixel_released+6/ tau)

    ;;; Sum over the trap species of the charge released in downstream pixels
 for counter_species_serial = 0, n_elements(trap_species_fraction_serial) - 1 do begin
    serial_charge_released_binning = trap_species_fraction_serial[counter_species_serial]  * serial_en_losses_kev * (exp(-pixels_released*transfer_time_serial/ trap_species_release_constant_serial[counter_species_serial]) - exp(-(6+pixels_released)*transfer_time_serial/ trap_species_release_constant_serial[counter_species_serial]))
    serial_downstream_kev += serial_charge_released_binning
 endfor
    
;;; serial_downstream_kev is the charge in the binned downstream
;;; serial pixel (binned pixel to the right/left of central pixel,
;;; depending on the node).
 

 ;;; Derive serial losses of downstream electrons (electrons in the
 ;;; binned pixel just above)

 serial_capture_prob = 1. - exp(-f_serial_cti_alpha * parallel_downstream_kev^(1-f_serial_cti_beta))
 serial_losses_mean = serial_n_transfers * serial_capture_prob * f_serial_trap_density_pixel 
 serial_losses_sigma = sqrt(serial_n_transfers * f_serial_trap_density_pixel * serial_capture_prob * (1. - serial_capture_prob))
 serial_random_array = randomn(seed, n_elements(parallel_downstream_kev))
 serial_losses_array_ev = (serial_random_array * serial_losses_sigma + serial_losses_mean) * 3.65
                                  ;Avoid "negative" losses (can be
                                ;caused by low losses_mean value and a
                                ;large losses_sigma value
 index_l0 = where(serial_losses_array_ev lt 0., nl0)
 if nl0 gt 0 then serial_losses_array_ev[index_l0] = 0
 
 ;;; Apply serial losses, avoid negative energies 
 serial_downstream_damaged_kev = (parallel_downstream_kev*1000.- serial_losses_array_ev)/1000.
 findNegIndex = where(serial_downstream_damaged_kev lt 0, negTot)
 if negTot gt 0 then begin
    serial_downstream_damaged_kev[findNegIndex] = 0.0
    serial_losses_array_ev[index_neg] = parallel_downstream_kev[index_neg]*1000.
 endif
  
 ;;; Recovery of release charge in binning downstream pixel
 if f_key_release then begin
    pixels_released = serial_n_transfers mod pixel_binning
    serial_en_losses_kev = parallel_downstream_kev - serial_downstream_damaged_kev
                                ;Sum recoveries over the trap species
    serial_recovered_kev_stored = dblarr(xraysTot)
    for counter_species_serial = 0, n_elements(trap_species_fraction_serial) - 1 do begin
       serial_charge_released_within_binning =  trap_species_fraction_serial[counter_species_serial]  * serial_en_losses_kev * (1. - exp(-pixels_released*transfer_time_serial/ trap_species_release_constant_serial[counter_species_serial]))
       serial_recovered_kev_stored += serial_charge_released_within_binning
    endfor
    serial_downstream_damaged_kev  += serial_recovered_kev_stored
 endif
 ;findNegIndex = where(serial_downstream_damaged_kev lt 0, negTot)
 ;if negTot gt 0. then  serial_downstream_damaged_kev[findNegIndex] = 0.0;
  
 ;;; serial_downstream_damaged_kev = the charge in the downstream
 ;;; pixel (above the central pixel) after serial losses + release in
 ;;; the binned pixel

 
 ;;; Remove X-rays with energy in central pixel below event threshold
 ;;; (zero to start with) due to losses 

  ;;; Replace negative energies if present with 0 energy
  index_negative_energies = where(list_energies_cti lt 0., n_negative)
  if n_negative gt 0 then begin
     list_energies_cti[index_negative_energies] = 0.
     serial_downstream_kev[index_negative_energies] = 0.
     serial_downstream_damaged_kev[index_negative_energies] = 0. 
  endif
  serial_cti_damaged_stored = list_energies_cti

  ; Report if CTI too high and all energies lost.
 index_positive_energies = where(list_energies_cti gt 0., n_positive)
 ;print, 'N events, N positive after serial readout: ', n_elements(list_energies_cti), n_positive
 ;if n_positive eq 0 then begin
 ;   print, 'CTI very high, all events are lost!!!'
    ;return
 ;endif
 
 ;;; *** REPORT ON EVENT ENERGIES AFTER PARALLEL+SERIAL CTI DISTORTION ****

 ;;; Damaged energies at this stage:
 ;;; serial_cti_damaged_stored ==> Damaged flux in central pixel,
 ;;; binned, after parallel image+store section damage, serial damage,
 ;;; and recovery from released charge within binned pixel from both
 ;;; parallel and serial transfers
 ;;; serial_downstream_kev ==> Charge released in binned pixel after
 ;;; central pixel.
 ;;; serial_downstream_damaged_kev ==> Charge released in downstream binned pixel
 ;;; during parallel transfer, serial-cti- damaged with electrons
 ;;; recovered from charge
 ;;; released within binned
 ;;; pixel during serial transfer.

 ;;; The combination of these three charges is the binned split event.
 ;;; serial_cti_damaged_stored == Central pixel
 ;;; serial_downstream_kev == Trailing serial pixel
 ;;; serial_downstream_damaged_kev == Trailing parallel pixel

 damaged_array = dblarr(3,xraysTot)
 damaged_array[0,*] = serial_cti_damaged_stored
 damaged_array[1,*] = serial_downstream_damaged_kev
 damaged_array[2,*] = serial_downstream_kev
                                ; damaged_array = [serial_cti_damaged_stored, serial_downstream_damaged_kev, serial_downstream_kev]
  
  return, damaged_array

end



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


function reformat_input_unbinned, energy_grid, rawy, lengthRawy
  ;;;
  ;;; Reads in the 10x10 central unbinned pixels of the 18x18 window.
  ;;; Reformat into 18 columns with flux at rawy row binned
  ;;; coordinate.
  
  event_18columns = dblarr(18, lengthRawy)

  ;;; Fill the central 10 columns. Other columns have no flux
  ;;; Trasform energies into keV
  for indexY = 0, 9 do begin
     for indexX = 4, 13 do begin
        ;;; NOTES on assigning 10x10 grid to 18 columns:
        ;;; First row of 10x10 grid is at unbinner event_18columns
        ;;; rowy_index = 13
        ;;; min(rawy) = 1
        ;;; energy_grid corresponds to the central 10x10 of the 18x18
        ;;; event window
        ;;; lowerindex y_unbinned = 7 + rawy * 6 - indexY
        ;;;                       = 7 +  1   * 6 - 9      = 4 
        event_18columns[indexX, 7 + rawy*6 -indexY] = energy_grid[indexX-4+indexY*10]/1000. ;(energies into keV)
     endfor
  endfor
  return, event_18columns
     
end

function get_unbinned_grade, phas

  ;;; Determines the grade of the input phas when input file includes
  ;;; unbinned phas values generated using George Randall's code.
  ;;; The phas are the central 10x10 pixels of the 18x18 event pixels
  ;;;
  flux_3by3 = dblarr(9)
  p0 = [0,1,10,11]
  p1 = [2, 3, 4, 5, 6, 7, 12, 14, 14, 15, 15, 17]
  p2 = [8, 9, 18, 19]
  p3 = [20, 21, 30, 31, 40, 41, 50, 51, 60, 61, 70, 71]
  p4 = [22, 23, 24, 25, 26, 27, 32, 33, 34, 35, 36, 37, 42, 43, 44, 45, 46, 47, 52, 53, 54, 55, 56, 57, 62, 63, 64, 65, 66, 67, 72, 73, 74, 75, 76, 77]
  p5 = [28, 29, 38, 39, 48, 49, 58, 59, 68, 69, 78, 79]
  p6 = [80, 81, 90, 91]
  p7 = [82, 83, 84, 85, 86, 87, 92, 93, 94, 95, 96, 97]
  p8 = [88, 89, 98, 99]
  flux_3by3[0] = total(phas[p0])
  flux_3by3[1] = total(phas[p1])
  flux_3by3[2] = total(phas[p2])
  flux_3by3[3] = total(phas[p3])
  flux_3by3[4] = total(phas[p4])
  flux_3by3[5] = total(phas[p5])
  flux_3by3[6] = total(phas[p6])
  flux_3by3[7] = total(phas[p7])
  flux_3by3[8] = total(phas[p8])
  
  index = where(flux_3by3 gt 0., n_over)
  if n_over eq 1 then grade = 'Single'
  if n_over eq 2 then grade = 'Double'
  if n_over eq 3 then grade = 'Triple'
  if n_over eq 4 then grade = 'Quadruple'
  if n_over gt 4 then grade = 'Multiple'
  return, grade
end

     
     


pro distort_cdm_fits, fits_filename, key_release = key_release, key_plot = key_plot, key_cti_corr = key_cti_corr, DEBUG_FLAG = debug_flag

;;;
;;; CP, 4 Sept 2020
;;; Summary - Distorts Xray events from a fits file using a CDM model
;;;  
;;; Input: fits_filename - X-ray events fits file
;;;        expo - Exposure time of fits file
;;;        parallel_trap_density_pixel = number of traps in a pixel in
;;;        image section
;;;        serial_trap_density_pixel = number of traps in a pixel in
;;;        the readout register 
;;;        cti_alpha, cti_beta = parallel CTI distortion parameters
;;;        cti_corr_alpha, cti_corr_beta = CTI correction pars, not the
;;;        same as distortion, from SMILE EoL
;;;        preditions,
;;;        serial_cti_alpha, serial_cti_beta = serial CTI distortion parameters
;;;        serial_cti_corr_alpha, serial_cti_corr_beta = CTI correction pars, not the
;;;        same as distortion, from SMILE EoL
;;;        preditions,  
;;;        cmax, threshold = CTI correction iteration parameters, cmax
;;;        = max number of trials in iteration, threshold = min
;;;        differenct between derived value of corrected energy in the
;;;        iteration, in eV  
;;;        flag_split = If set, simulate split (non-isolated) events based
;;;        on George's info.
;;;        flag_unbinned = If set, input FITS file in unbinned, it has been processed
;;;        using George Randall's code to apply charge distribution.
;;;  
;;; KEYWORD PARAMETERS:
;;;        key_release = recover energy within the 6-pixel binning,
;;;        default = 0
;;;        key_plot = generate plots
;;;        DEBUG_FLAG = Used in debugging mode, to print out extra
;;;        info and stop iterations if solution is diverging.    
;;;        
;;;  
;;; Output: CTI distorted fits file
;;;  
;;; HISTORY - Written by CP, 14 Aug 2020
;;;  
;;;           Modified from distort_cti_qdp_dety_min_max
;;;           added procedure to correct for CTI losses after CTI
;;;           damage, to check how well energies can be recovered
;;;    
;;;           Modified from distort_cti_qdp_pixel.pro
;;;           Distortion over a range of dety values, dety_min, dety_max
;;;           Distortion assumes observations are uniformely
;;;           distributed between dety_min and dety_max
;;;
;;;          CP, 18/1/2019
;;;          Modified from distort_cti_qdp_dety_min_max to implement serialCTI
;;;          distortion for a source imaged between detx_min and detx_max
;;;          and by added low threshold discriminator (keyword detection_ll) to
;;;          exclude low-energy X-rays that the CCD will not be able to
;;;          detect. it's the low level discriminator in XRT, to
;;;          separate noise from real X-rays  
;;;
;;;          CP, 14 Aug 2020
;;;          Modified from distort_cti_qdp_detx_dety_min_max_and_cti_correction.pro 
;;;          Uses input fits file instead of qdp file
;;;          Option to generate non-isolated X-rays (non grade0)
;;;          Binning 6x6 implemented
;;;          Charge release treatment implemented
;;;
;;;          CP, 12 Mar 2021
;;;          Modified to process unbinned input FITS files generated
;;;          by George Randall's modeling. In this case, no
;;;          charge splitting and spreading needs to be applied. The
;;;          input data need to be a reformatted from the 10x10
;;;          central unbinned flux values.
  
  
  if n_params() ne 1 then begin
     print,"Please enter correct parameters: distort_cdm_fits, fits_filename"
     print,"Please enter correct parameters: distort_cdm_fits, 'P100001SXI0033IMEVLI0000.FIT'"
     return
  endif  

  DEVICE,DECOMPOSED=0.
  tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1

  ;;; Set and restore ccd operations mode, trap properties, cdm
  ;;; options.
  ;;; In case trap properties need updating, manually edit new values
  ;;; in setup_cdm_variables.pro

  setup_cdm_variables
  restore, 'distort_cdm_properties.sav'
  print, 'trap density image = ', trap_species_image_density
  print, 'charge_injection_flag = ', charge_injection_flag
  print, 'capture_cross_section_image = ', capture_cross_section_image
  print, 'flag_unbinned = ', flag_unbinned

  ;;; Output fits, ps, debug filenames
  splitString = strsplit(fits_filename, '.', /extract)
  if charge_injection_flag then fits_filename_output = splitString[0]+'_CI'+strtrim(string(charge_injection_block_lines),2)+'_CDM.FIT' else fits_filename_output = splitString[0]+'_CDM.FIT'
  if charge_injection_flag then ps_filename_output = splitString[0]+'_CI'+strtrim(string(charge_injection_block_lines),2)+'_CDM.ps' else ps_filename_output = splitString[0]+'_CDM.ps'
  if charge_injection_flag then debug_filename_output = splitString[0]+'_CI'+strtrim(string(charge_injection_block_lines),2)+'_CDM_debug.txt' else debug_filename_output = splitString[0]+'_CDM_debug.txt'
  if keyword_set(DEBUG_FLAG) then openw, lu_debug, debug_filename_output, /get_lun , WIDTH=250 

  ;;; Read in fits file
  tab = mrdfits(fits_filename,1,hd1)
  tab0 = mrdfits(fits_filename,0,hd0)
  xraysTot = 0l
  xraysTot = n_elements(tab.PI)
  serialL = fix(max([tab.RAWX*6+18,serial_columns]))

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

  ;;; Initialise arrays to store the 3x3 binned input and damaged pixels energies.
  list_energy_grid = dblarr(9, xraysTot)
  list_energy_grid_cdm_ev = dblarr(9, xraysTot)
 
   ;;; ***   EVENT SPLITTING and UNBINNED FITS processing  ****
  ;;;
  ;;; Call the event_splitting function to split the energy of the
  ;;; events between the allowed X-ray grades.
  ;;; The returned array is a 9 x N_tot_events array, with the 9
  ;;; representing the pixels in the 3x3 window.
  ;;; If no grade splitting requested assign all energy to the central
  ;;; pixels
  ;;; Generate event splitting

  if flag_unbinned eq 0 then begin
     if flag_split eq 1 then begin
        split_ll_keV = detection_ll_split_eV/1000.
        list_energy_grid = event_splitting_smart(tab.PI/1000., split_ll_keV)
     endif else list_energy_grid[4,*] = tab.PI/1000.
     ;;; If the input FITS is unbinned the event splitting/charge
     ;;; distribution has already been applied.  
  endif
  
  ;;;
  ;;;  *** END of EVENT SPLITTING ***
  
  ;;; Array represents the unbinned 3x3 event energies, in keV, resulting in a 9x6
  ;;; unbinned pixels array
  ;;; list_energy_grid_unbinned = dblarr(9*6, xraysTot)

  ;;; **** CDM processing of individual x-ray events  *****

  transfer_length = fix(max([(tab.RAWY)*6., image_section_lines]))+12
 
  for event_index = 0, xraysTot-1 do begin
     
     splitindex = where(list_energy_grid[*,event_index] gt 0, n)
     if n eq 1 then grade = 'single'
     if n eq 2 then grade = 'Double'
     if n eq 3 then grade = 'Triple'
     if n eq 4 then grade = 'Quadruple'

     ;;; --- CHARGE DISTRIBUTION ---
     ;;; 1 - If input is binned FITS file distribute charge using
     ;;;     charge_distribution function
     ;;; 2 - If input FITS file is binned call reformat_input_unbinned
     ;;;     to reformat input 10x10 central pixels into 18 unbinned
     ;;;     columns
     

     if flag_unbinned eq 0 then begin
     ;;; Distribute charge within a binned pixel, returns an array of
     ;;; 18 columns where charge of the event is distributed.
     
     ;;; Shift input Y if Y eq 0 (event at bottom line of image
     ;;; section, should not be a valid xray in real data)
        distribRawy = max([1,  tab[event_index].RAWY])
        xray_unbinned_18cols = charge_distribution(list_energy_grid[*,event_index], distribRawy, transfer_length)
        ;;; When debugging ON store info on number of unbinned pixels
        ;;; with charge after charge spreading and value of highest energy.
        if DEBUG_FLAG then begin
           index_split_unbinned = where(xray_unbinned_18cols gt 0, n_split_unbinned)
           tab[event_index].PATTERN = n_split_unbinned
           tab[event_index].ENERGYE1 = max(xray_unbinned_18cols)*1000.
        endif
     endif else begin
        ;;; Populate xray_unbinned_18cols with unbinned input pixels
        distribRawy = max([1,  tab[event_index].RAWY])
        xray_unbinned_18cols = reformat_input_unbinned(tab[event_index].PHAS, distribRawy, transfer_length)
        grade = get_unbinned_grade(tab[event_index].PHAS)
         ;;; When debugging ON store info on number of unbinned pixels
        ;;; with charge after charge spreading and value of highest energy.
        if DEBUG_FLAG then begin
           index_split_unbinned = where(xray_unbinned_18cols gt 0, n_split_unbinned)
           tab[event_index].PATTERN = n_split_unbinned
           tab[event_index].ENERGYE1 = max(xray_unbinned_18cols)
        endif
     endelse
    
     ;;; If debugging, save to debug file the info on the 18x18 pixels
     ;;; above threshold
     
     inputcdm_unbinned_index = where(xray_unbinned_18cols gt 0.0, npixels_th)
     if keyword_set(DEBUG_FLAG) then begin 
        output_string = [event_index, npixels_th, inputcdm_unbinned_index, fix(xray_unbinned_18cols[inputcdm_unbinned_index]*1000)]
        printf, lu_debug, output_string
     endif
     
     ;;; Apply CDM function
     cdm_damaged_3x3_ev = cdm_function(xray_unbinned_18cols, tab[event_index].RAWX, tab[event_index].RAWY, transfer_length, readout_image_time, readout_store_time, readout_serial_time, trap_species_parallel, trap_species_image_density, trap_species_store_density, trap_species_serial, trap_species_serial_density, charge_injection_flag, charge_injection_time_gap, store_section_lines, ci_losses_pixel_species_image, release_image_time, release_store_time, release_serial_time, ci_released_image_pixel, capture_cross_section_image, charge_volume_coeff_image, capture_cross_section_store, charge_volume_coeff_store, capture_cross_section_serial, charge_volume_coeff_serial,  flag_retrapping, flag_iteration, iter_max, threshold,  short_bernoulli, long_bernoulli, flag_binomial, max(tab.RAWY), serialL, readout_nodes, serial_columns, ci_losses_pixel_species_store)

     ;;; Store damaged 3x3 in list
     list_energy_grid_cdm_ev[*, event_index] = cdm_damaged_3x3_ev
     ;;; Print events info to track progress.
     print, 'Event grade, n, n_unbinned, time  RAWX, RAWY, PI  PICTI: ', grade, event_index, npixels_th, systime(/seconds)-start_processing, tab[event_index].RAWX, tab[event_index].RAWY, tab[event_index].PI, total(cdm_damaged_3x3_ev)
     ;print, npixels_th, total(cdm_damaged_3x3_ev), output_string
  endfor                        ; End of X-ray events loop

  if keyword_set(DEBUG_FLAG) then close, lu_debug 
  
  ;;; *** Event reconstruction with energy LL enforced ****
  ;;;
  ;;; NOTE - This is not part of the CTI processing but if requested
  ;;;        damaged event energies can be cleaned by applying these
  ;;;        two criteria in order:
  ;;;
  ;;; 1 - Split threshold: only include non-central pixels above the
  ;;;     split threshold, otherwise consider charge as noise, set it to zero.
  ;;; 2 - PICTI threshold: After appling split threshold, total energy
  ;;;     in 3x3 must be above PICTI energy threshold, otherwise set
  ;;;     all energies to zero.
 

  if keyword_set(flag_event_rec_ll) then begin
     for events_counter = 0l, xraysTot - 1 do begin
        for pixel_index = 0, 8 do begin
           if pixel_index ne 4 then begin  
              if list_energy_grid_cdm_ev[pixel_index,events_counter] lt detection_ll_split_eV then list_energy_grid_cdm_ev[pixel_index,events_counter] = 0.
           endif
        endfor
        damaged_event_total_ev = total(list_energy_grid_cdm_ev[*,events_counter])
        if damaged_event_total_ev lt detection_ll_eV then list_energy_grid_cdm_ev[*,events_counter] = 0
     endfor
  endif
  
  damaged_total_array_ev = total(list_energy_grid_cdm_ev, 1)

  finish_processing = systime(/seconds)
  print, start_processing, format='(d15.2)'
  print, finish_processing, format='(d15.2)'
  elapsed = finish_processing-start_processing
  print, 'Processing time:', elapsed
  print, 'Writing FITS file now... '
  
     ;;; *** Save processed events to fits file  ***
 
  ;;; Create structure newtab to store first X-ray event data
     newtab = {EVENTS, TIME: tab[0].TIME, RAWX: tab[0].RAWX, RAWY: tab[0].RAWY, DETX: tab[0].DETX, DETY: tab[0].DETY, X: tab[0].X, Y: tab[0].Y , PHA:tab[0].PHA, PI:tab[0].PI, PHAS:list_energy_grid[*,0]*1000., PICTI:damaged_total_array_ev[0], PHASCTI:list_energy_grid_cdm_ev[*,0], PATTERN: tab[0].PATTERN, ENERGYE1: tab[0].ENERGYE1}
  ;;; Create structure array to store all X-ray events
     outtab = replicate({EVENTS}, xraysTot)
  ;;; Populate the array of structures.
     for count= 0l, xraysTot-1 do outtab[count] = {TIME: tab[count].TIME, RAWX: tab[count].RAWX, RAWY: tab[count].RAWY, DETX: tab[count].DETX, DETY: tab[count].DETY, X: tab[count].X, Y: tab[count].Y ,PHA:tab[count].PHA, PI:tab[count].PI, PHAS:list_energy_grid[*,count]*1000., PICTI:damaged_total_array_ev[count], PHASCTI:list_energy_grid_cdm_ev[*,count], PATTERN: tab[count].PATTERN, ENERGYE1: tab[count].ENERGYE1}
     mwrfits, outtab, fits_filename_output, hd1

  plothist, tab.PI, /xlog, /ylog
  plothist, damaged_total_array_ev, /overplot, color=2
  plothist, list_energy_grid_cdm_ev[4,*], /overplot, color=3
  xyouts, 0.6, 0.85, 'Input PI', /norm, color = 0, charsize = 1.2
  xyouts, 0.6, 0.8, 'Summed CTI-damaged PIS', /norm, color = 2, charsize = 1.2
  xyouts, 0.6, 0.75, 'Central CTI-damaged PHA', /norm, color = 3, charsize = 1.2

  if keyword_set(key_plot) then begin
       entry_device = !d.name
       set_plot,'ps'
       device, filename = ps_filename_output, /color
       ;titstring = 'Input [black] + distorted [red] + corrected [blue] '+ string_split[0]
       plothist, tab.PHA, /xlog, /ylog
       plothist, damaged_total_array_ev, /overplot, color=2
       plothist, list_energy_grid_cdm_ev[4,*], /overplot, color=3
       xyouts, 0.6, 0.85, 'Input PHA', /norm, color = 0
       xyouts, 0.6, 0.8, 'Summed CTI-damaged PHAS', /norm, color = 2
       xyouts, 0.6, 0.75, 'Central CTI-damaged PHA', /norm, color = 3
       device,/close  
       set_plot,entry_device

    endif
  
  stop
 

 
 ;en6kev = 6.
 ;losses6keV= (1. - exp(-cti_alpha * en6kev^(1-cti_beta))) * n_transfers * trap_density_pixel * 3.65
 ;en1kev = 1.
 ;losses1keV= (1. - exp(-cti_alpha * en1kev^(1-cti_beta))) * n_transfers * trap_density_pixel * 3.65
 ;en500ev = 0.5
 ;losses500ev = (1. - exp(-cti_alpha * en500ev^(1-cti_beta))) * n_transfers * trap_density_pixel * 3.65
 
 ;losses_corrections = losses_array_ev - cti_corrections_ev_storage
 ;plot, list_energies, losses_corrections, psym=1, background=1, color=0, ytit='Losses-corrections'
 ;print, 'Mean and median of losses-corrections'
 ;print, mean(losses_corrections, /nan), median(losses_corrections)
  
; print, 'Losses at 6 keV = ' ,losses6keV
; print, 'Losses at 1 keV = ' ,losses1keV
; print, 'Losses at .5 keV = ',losses500ev
end

 
