function optical_loading_ci_pixel_process, flux_line, image_lines, store_lines, serial_columns, readout_nodes, readout_time_image, readout_time_serial, trap_density_column, trap_density_filled_column, trap_density_free_column, trap_density_row, trap_density_filled_row, trap_density_free_row, ccd_mode_binning, release_image_time, capture_cross_section, charge_volume_coeff, fwc, overscan_cols, image_expo_time, frames_n, flag_retrapping = flag_retrapping, flag_same_pixel_release = flag_same_pixel_release

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
; flux_line = 719*6 = 4314 pixel long Array of input flux values. The
; top 523 are 'virtual pixels' that can include CI lines. The following
; 3791 pixels are the exposed image section.
; For each exposed frame the lines from the image are transferred into the store until the bottom
; line of the image reach the bottom line of the store.
; The store is 719 lines long, therefore with 6pixels binning 719*6 =
; 4314 lines must be transferred in.
; The first 3791 lines in are from the exposed image, the following
; lines are "virtual", not exposed. CI can be injected in those lines. 
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
; frames_n = The frame being processed, starting with frame1. Only used to name output files storing trap filling stats.
; 
; OPTIONAL INPUTS:
;	
; KEYWORD PARAMETERS:
; flag_retrapping 0- Released electrons not retrapped 
;	                1- Released electrons can be retrapped
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

  trap_types = n_elements(capture_cross_section)

  image_frame_n_transfers = store_lines * ccd_mode_binning ; [This should be 719 * 6 = 4314]
  
                                ; Trap occupancy stats setup
  
  image_traps = trap_density_column[0:image_lines-1, *]
  image_traps_filled = trap_density_filled_column[0:image_lines-1, *]
  image_traps_free = trap_density_free_column[0:image_lines-1, *]

  store_traps = trap_density_column[image_lines : image_lines+store_lines-1, *]
  store_traps_filled = trap_density_filled_column[image_lines : image_lines+store_lines-1, *]
  store_traps_free = trap_density_free_column[image_lines : image_lines+store_lines-1, *]

  serial_traps = trap_density_row
  serial_traps_filled = trap_density_filled_row
  serial_traps_free = trap_density_free_row
  
  ;;; Flux in image, store, serial
  ;;; register setup after exposure, at the start of readout
  ;;; flux_image[0] = Bottom (first) pixel of image segment
  ;;; flux_store[0] = Bottom (first) pixel of store section
  ;;; At image/store boundary flux from the bottom pixel of the image, flux_image[0] is transferred into the top pixel of store flux_store[718]
  
  flux_input = flux_line 
  flux_image = flux_line[0:image_lines-1]
  
  ;flux_image = flux_line[store_lines*ccd_mode_binning - image_lines : n_elements(flux_line)-1]
  flux_store = dblarr(store_lines)
  flux_serial = dblarr(serial_columns/readout_nodes)

  image_transfer_time = readout_time_image
  store_transfer_time = readout_time_image * ccd_mode_binning ;;; Flux in store is accumulated over ccd_mode_binning lines and transferred once every ccd_mode_binning 
  store_phase2_transfer_time = readout_time_serial * (serial_columns/readout_nodes/ccd_mode_binning + 1 + overscan_cols)
  serial_transfer_time = readout_time_serial / ccd_mode_binning  ;;; The readout_time_serial of 7.14micros is the transfer rate of binned pixels. Unbinned transfer rate is readout_time_serial / ccd_mode_binning 

                                ; Contribution to flux of charge releaed in following pixel.

  release_following_pixel_flux_image = dblarr(image_lines, trap_types)
  release_following_pixel_flux_store = dblarr(store_lines, trap_types)
  release_following_pixel_flux_serial = dblarr(serial_columns/readout_nodes, trap_types)
  
  ;;; Calculate and store release timescales to avoid repetion of exponentials computations
  release_image_amplitude_by_specie = (1. - exp(-image_transfer_time / release_image_time))
  release_store_amplitude_by_specie = (1. - exp(-store_transfer_time / release_image_time))
  release_store_phase2_amplitude_by_specie = (1. - exp(-store_phase2_transfer_time / release_image_time))
  release_serial_amplitude_by_specie = (1. - exp(-serial_transfer_time / release_image_time))

  release_image_amplitude_by_specie_same_pix = 1. - exp(-0.5 * image_transfer_time / release_image_time)
  release_store_amplitude_by_specie_same_pix = 1. - exp(-0.5 * store_transfer_time / release_image_time)
  release_store_phase2_amplitude_by_specie_same_pix = 1. - exp(-0.5 * store_phase2_transfer_time / release_image_time)
  release_serial_amplitude_by_specie_same_pix = 1. - exp(-0.5 * serial_transfer_time / release_image_time)
  
  ;;; For debugging purposes store the flux along the image column at each transfer step
  flux_ima_evo = dblarr(n_elements(flux_image), image_frame_n_transfers+1)
  flux_ima_evo[*,0] = flux_image
  
  ;;; For debugging purposes store trap occupancy along image at each transfer step
  image_traps_filled_evo = dblarr([image_lines, trap_types, image_frame_n_transfers+1])  
  
  ;;; For debugging puproses store trap occupancy during readout
  image_trap0_filled_evo = dblarr(image_lines, image_frame_n_transfers)
  image_trap1_filled_evo = dblarr(image_lines, image_frame_n_transfers)
  image_trap2_filled_evo = dblarr(image_lines, image_frame_n_transfers)
  image_trap3_filled_evo = dblarr(image_lines, image_frame_n_transfers)
  
  ;;; Store store frame traps evo during image readout (first store_lines transfers) and during store readout (second store_line transfers)
  store_trap0_filled_evo = dblarr(store_lines, store_lines*2)
  store_trap1_filled_evo = dblarr(store_lines, store_lines*2)
  store_trap2_filled_evo = dblarr(store_lines, store_lines*2)
  store_trap3_filled_evo = dblarr(store_lines, store_lines*2)

   ;;; Store serial register traps evo during store readout
  serial_trap0_filled_evo = dblarr(serial_columns/readout_nodes, store_lines)
  serial_trap1_filled_evo = dblarr(serial_columns/readout_nodes, store_lines)
  serial_trap2_filled_evo = dblarr(serial_columns/readout_nodes, store_lines)
  serial_trap3_filled_evo = dblarr(serial_columns/readout_nodes, store_lines)
  
  ; Store total number of free traps during image readout
  total_free_image_evo = dblarr(trap_types, image_frame_n_transfers)
  total_free_store_evo = dblarr(trap_types, image_frame_n_transfers)
  total_free_store_evo_phase2 = dblarr(trap_types, store_lines)
  total_free_serial_evo = dblarr(trap_types, store_lines)
  
  frames_n_string = strtrim(string(frames_n), 2)

;;; --- IMAGE FRAME TRANSFER ---

  for ima_transfers_counter = 1, image_frame_n_transfers do begin

     ;;; Generate random numbers for each image frame pixel. These
     ;;; will be used to determine the success of the trapping/release
     seed = ima_transfers_counter + frames_n
     rv_image = randomu(seed, image_lines, trap_types)
     ;rv_release = randomu(-seed, image_lines, trap_types)
     ;rv_release_following = randomu(seed*2, image_lines, trap_types)
     
     ;;; Update trap stats in image segment, looping over the traps in
     ;;; each pixel and the trap types

     for image_index = 0, image_lines -1 do begin
        ;;; Add to flux the contribution from charge released from
        ;;; filled traps.
        flux_iteration = flux_image[image_index]
        flux_image[image_index] += total(release_following_pixel_flux_image[image_index,*])
        ;;; If flux below a threshold only calculate the release of charge from previously occupied traps, otherwise calculate the capture as well.
        if flux_image[image_index] lt 0.001 then begin
          ;;; Compute charge that will be released in following pixel
          ;;; during next transfer and update the traps occupancy.
      ;     release_outcome = rv_release[image_index,*] le release_image_amplitude_by_specie
       ;    release_following_pixel_flux_image[image_index, *] = image_traps_filled[image_index, *] * release_outcome
           ;;; Previous version of calculation, without randomizing
           ;;; the release process
           release_following_pixel_flux_image[image_index, *] = image_traps_filled[image_index, *] * release_image_amplitude_by_specie
           image_traps_filled[image_index, *] -= release_following_pixel_flux_image[image_index, *]
           image_traps_free[image_index, *] = image_traps[image_index, *] - image_traps_filled[image_index, *]
        endif else begin  
          
        
        ;;; flag_retrapping = ON when charge that has just been
        ;;; released into the pixel can be retrapped 
        if keyword_set(flag_retrapping) then flux_iteration += total(release_following_pixel_flux_image[image_index,*]) 
        flux_norm = flux_iteration/fwc  
        fr_capture = flux_norm^(1.-charge_volume_coeff)
        
        for trap_index = 0, trap_types - 1 do begin
           capture_p = 1. - exp(-capture_cross_section[trap_index]  * image_transfer_time * flux_iteration^charge_volume_coeff)

           ;;; The flux is captured if a [0-1] random number is < Pc
           ;;; In that case, update the trap occupancy stats and the
           ;;; remaining flux in the pixel.
           if rv_image[image_index, trap_index] lt capture_p then begin
              ;;; If capture process is succesful, at max the losses can be equal to the lower value between the flux or the number of free traps 
              losses = min([image_traps_free[image_index, trap_index] * fr_capture, flux_iteration])

              ;;; If charge released in same pixel is implemented
              ;;; (flag_same_pixel_relaese = 1) two sources contribute
              ;;; to the flux:
              ;;; 1 - Charge just captured
              ;;; 2 - Filled traps.
              if flag_same_pixel_release then begin 
                 losses -= losses * release_image_amplitude_by_specie_same_pix[trap_index]
                 release_same_pixel_charge = image_traps_filled[image_index, trap_index] * release_image_amplitude_by_specie_same_pix[trap_index]
                 image_traps_filled[image_index, trap_index] -= release_same_pixel_charge
                 image_traps_free[image_index, trap_index] +=  release_same_pixel_charge
                 flux_image[image_index] += release_same_pixel_charge
              endif
              if losses lt 0.0 then stop
              ;;; Update occupancy and flux value after capture
              image_traps_filled[image_index, trap_index] += losses
              image_traps_free[image_index, trap_index] = max([0.0, image_traps[image_index, trap_index] - image_traps_filled[image_index, trap_index]])
              flux_image[image_index] = max([0.0, flux_image[image_index]-losses])
           endif     
             
           ;;; Compute charge that will be released in following pixel
           ;;; during next transfer and update the traps occupancy.

           release_following_pixel_flux_image[image_index, trap_index] = image_traps_filled[image_index, trap_index] * release_image_amplitude_by_specie[trap_index]
           image_traps_filled[image_index, trap_index] -= release_following_pixel_flux_image[image_index, trap_index]
           image_traps_free[image_index, trap_index] = image_traps[image_index, trap_index] - image_traps_filled[image_index, trap_index]  
                     
           ;;; Store image trap filling stats for debugging purposes
           image_traps_filled_evo[image_index, trap_index, ima_transfers_counter] = image_traps_filled[image_index, trap_index] 
                         
        endfor ;;; End of trap specie Loop     
        endelse     ;;; End of case where enough flux in the pixel and trap capturing is computed 
        
        ;;; DEGUG/TEST - store trap2 occupancy in image
        image_trap0_filled_evo[image_index, ima_transfers_counter-1] = image_traps_filled[image_index, 0]
        image_trap1_filled_evo[image_index, ima_transfers_counter-1] = image_traps_filled[image_index, 1]
        image_trap2_filled_evo[image_index, ima_transfers_counter-1] = image_traps_filled[image_index, 2]
        image_trap3_filled_evo[image_index, ima_transfers_counter-1] = image_traps_filled[image_index, 3]
        
     endfor;;; END loop of trap occupancy updates over the image frame
 
     ;;; Store total number of free traps along image per specie
     total_free_image_evo[*, ima_transfers_counter-1] =  total(image_traps_free, 1)
     ;print, 'N free traps image at transfer = ', ima_transfers_counter, total_free_image_evo[*, ima_transfers_counter-1]
     
     ;;; Generate random numbers for each store frame pixel. These
     ;;; will be used to determine the success of the trapping/release
     rv_store = randomu(seed, store_lines, trap_types)
      
     ;;; Update trap stats in store segment, looping over the traps in
     ;;; each pixel and the trap types
     ;;;
     ;;; NOTE - In the store charge is transferred once every
     ;;;        ccd_mode_binning image transfers. During this time the
     ;;;        flux is accumulated at the top of the store. 
     if (ima_transfers_counter mod ccd_mode_binning eq 0) then begin
     
        for store_index = 0, store_lines -1 do begin
        ;;; Add to flux the contribution from charge released from
        ;;; filled traps.

           flux_iteration = flux_store[store_index]
           flux_store[store_index] += total(release_following_pixel_flux_store[store_index,*])
           ;;; Don't evaluate losses if flux in pixel is below a threshold.
           ;;; Just evaluate the release in the following pixel          
           if flux_store[store_index] lt 0.001 then begin
             ;;; Compute charge that will be released in following pixel
             ;;; during next transfer and update the traps occupancy.
              release_following_pixel_flux_store[store_index, *] = store_traps_filled[store_index] * release_store_amplitude_by_specie
              store_traps_filled[store_index, *] -= release_following_pixel_flux_store[store_index, *]
              store_traps_free[store_index, *] = store_traps[store_index, *] - store_traps_filled[store_index, *]
           endif else begin
           
           
              if keyword_set(flag_retrapping) then flux_iteration += total(release_following_pixel_flux_store[store_index,*])
              flux_norm = flux_iteration/fwc
              fr_capture = flux_norm^(1.-charge_volume_coeff)
        
              for trap_index = 0, trap_types - 1 do begin
                 capture_p = 1. - exp(-capture_cross_section[trap_index]  * store_transfer_time * flux_iteration^charge_volume_coeff)

           ;;; The flux is captured if a [0-1] random number is < Pc
           ;;; In that case, update the trap occupancy stats and the
           ;;; remaining flux in the pixel.
                 if rv_store[store_index, trap_index] lt capture_p then begin
                
                ;;; If capture process is succesful, at max the losses can be equal to the lower value between the flux or the number of free traps
                    losses = min([store_traps_free[store_index, trap_index] * fr_capture, flux_iteration])

              ;;; If charge released in same pixel is implemented
              ;;; (flag_same_pixel_relaese = 1) two sources contribute
              ;;; to the flux:
              ;;; 1 - Charge just captured
              ;;; 2 - Filled traps.
                    if flag_same_pixel_release then begin                     
                       losses -= losses * release_store_amplitude_by_specie_same_pix[trap_index]
                       release_same_pixel_charge = store_traps_filled[store_index, trap_index] * release_store_amplitude_by_specie_same_pix[trap_index]
                       store_traps_filled[store_index, trap_index] -= release_same_pixel_charge
                       store_traps_free[store_index, trap_index] +=  release_same_pixel_charge
                       flux_store[store_index] += release_same_pixel_charge
                    endif

              ;;; Update occupancy and flux value after capture
                    store_traps_filled[store_index, trap_index] += losses
                    store_traps_free[store_index, trap_index] = max([0.0, store_traps[store_index, trap_index] - store_traps_filled[store_index, trap_index]])
                    flux_store[store_index] = max([0.0, flux_store[store_index]-losses])
                 endif
           
           ;;; Compute charge that will be released in following pixel
           ;;; during next transfer and update the traps occupancy.
                 release_following_pixel_flux_store[store_index, trap_index] = store_traps_filled[store_index, trap_index] * release_store_amplitude_by_specie[trap_index]
                 store_traps_filled[store_index, trap_index] -= release_following_pixel_flux_store[store_index, trap_index]
                 store_traps_free[store_index, trap_index] = store_traps[store_index, trap_index] - store_traps_filled[store_index, trap_index]           
              endfor   
           endelse
           
           ;;; DEGUG/TEST - store trap2 occupancy in store segment during readout.
           store_tr_step = ima_transfers_counter/ccd_mode_binning
           store_trap0_filled_evo[store_index, store_tr_step] = store_traps_filled[store_index, 0]
           store_trap1_filled_evo[store_index, store_tr_step] = store_traps_filled[store_index, 1]
           store_trap2_filled_evo[store_index, store_tr_step] = store_traps_filled[store_index, 2]
           store_trap3_filled_evo[store_index, store_tr_step] = store_traps_filled[store_index, 3]         
        endfor  ;;; END loop of trap occupancy updates over the store frame
     endif       ;;; Condition to only transfer store lines every ccd_mode_binning image transfers
     
     ;;; Store total number of free traps along image per specie
     total_free_store_evo[*, ima_transfers_counter-1] =  total(store_traps_free, 1)
    ; print, 'N free traps store at transfer = ', ima_transfers_counter, total_free_store_evo[*,ima_transfers_counter-1]
     
     ;;; Update flux:
     ;;; - Flux in image shifted by 1
     ;;; - Add charge in bottom line of image into the top of store
     ;;; - Every ccd_mode_binning lines the flux in store pixels is shifted by 1.
     ;;; - Add charge from virtual pixels (not exposed to sky, can be CI lines) at the top pixel of the image
     
     flux_store[n_elements(flux_store)-1] += flux_image[0]    
     flux_image = shift(flux_image, -1)
     if ima_transfers_counter lt store_lines * ccd_mode_binning - image_lines then begin
        flux_image[n_elements(flux_image)-1] = flux_input[image_lines+ima_transfers_counter-1]
     endif else flux_image[n_elements(flux_image)-1] =  0.0
     if (ima_transfers_counter mod ccd_mode_binning eq 0) then begin
        flux_store = shift(flux_store, -1)
        flux_store[store_lines-1] = 0.0  ;;; The top of store is now empty of charge.
     endif
     
     ;;; Store the distorted and shifted image in evolution image flux array for debugging purposes.
     flux_ima_evo[*, ima_transfers_counter] = flux_image
     ;;; DEBUG START
     if (ima_transfers_counter mod 200 eq 0) then begin
        flux_str = 'Flux in image at transfer ' + strtrim(string(ima_transfers_counter),2)
        plot, flux_image, yr=[8.0, 20.1], yst=1, xtit = 'DETY', tit = flux_str
        print, flux_str  
     endif  
     
  endfor 
;;; END of loop over the image lines PhaseA transfer and of modelling the IMAGE FRAME TRANSFER
  
  ;;; Debugging - Store filling in image segment during image readout phaseA
  
  openw, lu0, 't0_image_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu1, 't1_image_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu2, 't2_image_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu3, 't3_image_filled_frn'+frames_n_string+'.txt', /get_lun
  for transfer_counter = 1, image_frame_n_transfers-2 do begin
     for image_line_counter = 0, image_lines-1 do begin
        printf, lu0, transfer_counter, image_line_counter, image_trap0_filled_evo[image_line_counter, transfer_counter-1],format='(i,i,d9.6)' 
        printf, lu1, transfer_counter, image_line_counter, image_trap1_filled_evo[image_line_counter, transfer_counter-1],format='(i,i,d9.6)'
        printf, lu2, transfer_counter, image_line_counter, image_trap2_filled_evo[image_line_counter, transfer_counter-1],format='(i,i,d9.6)'
        printf, lu3, transfer_counter, image_line_counter, image_trap3_filled_evo[image_line_counter, transfer_counter-1],format='(i,i,d9.6)'
     endfor  
  endfor  
  free_lun, lu0
  free_lun, lu1
  free_lun, lu2
  free_lun, lu3
  
  ;;; Debugging - Save total number of filled traps during image readout phaseA 
  openw, lufreeimage, 'free_image_evo_phaseA_frn'+frames_n_string+'.txt', /get_lun, width=100
  openw, lufreestore, 'free_store_evo_phaseA_frn'+frames_n_string+'.txt', /get_lun, width=100
  for transfer_counter = 1, image_frame_n_transfers-1 do begin
     printf, lufreeimage, transfer_counter, total_free_image_evo[*,transfer_counter-1]
     printf, lufreestore, transfer_counter, total_free_store_evo[*,transfer_counter-1]
  endfor  
  free_lun, lufreeimage
  free_lun, lufreestore
  
  print, 'END OF IMAGE FRAME TRANSFER'
  print, 'STARTING FRAME TRANSFER NOW'

    
  ;;; UPDATE TRAP STATS IN SERIAL REGISTER DURING IMAGE FRAME TRANSFER
  ;;; TO ACCOUNT FOR RELEASED CHARGE IN SERIAL REGISTER DURING PHASE-A
  ;;; IMAGE READOUT (NOTE - This will be irrelevant during the first frame readout, but it will have an effect for later frames).
  ;;;
  ;;; NOTE - Charge is transferred over the serial register once
  ;;;        every ccd_mode_binning lines.
  ;;;        During the image frame transfer no flux goes into the
  ;;;        serial register. During this time filled traps can
  ;;;        release charge.
  ;;; ASSUMPTTION - Charge released by filled traps during image
  ;;;               frame transfer won't be re-trapped.
  ;;;             - Only calculate trap occupancy update in
  ;;;               serial register once at the end of the image
  ;;;               frame readout.
  ;;;
  ;;; Trap occupancy is updated by calculating:
  ;;; 1 - Total image readout time interval
  ;;; 2 - Fraction of released charge during that interval by trap
  ;;;     specie.
  ;;; 3 - The released charge per serial pixel and specie.
  ;;; 4 - Update trap occupancy per serial pixel and specie.
  

  ;;; 1 - Total image readout time interval
  image_readout_time_interval = store_lines * image_transfer_time * ccd_mode_binning ; This is equals to 0.116s if store_image_time = 27mus, store_lines = 719 and binning of 6
  ;;; 2 - Fraction of released charge during that interval by trap
  ;;;     specie.
  fraction_released_image_readout = dblarr(trap_types)
  fraction_released_image_readout = 1. - exp(-image_readout_time_interval/release_image_time)
  ;;; Update trap occupancy in serial register during image readout.
  for serial_index = 0, serial_columns/readout_nodes-1 do begin
     for trap_index = 0, trap_types-1 do begin
         ;;; 3 - The released charge per serial pixel and specie.
        released_trap_specie = serial_traps_filled[serial_index, trap_index] * fraction_released_image_readout[trap_index]
        ;;; 4 - Update trap occupancy per serial pixel and specie.
        serial_traps_filled[serial_index, trap_index] -= released_trap_specie
        serial_traps_free[serial_index, trap_index] = serial_traps[serial_index, trap_index] - serial_traps_filled[serial_index, trap_index]
     endfor
  endfor

  flux_store_start = flux_store

 ;;; --- STORE FRAME TRANSFER ---
 ;;; - In image the frame is being exposed. No line transfers but
 ;;;   charge is being released from traps
 ;;; - Store lines are trasferred at rate of 386 * 7.14mus (store_phase2_transfer_time)
 ;;; - After a store line transfer into serial register, pixels in
 ;;;   the serial register are transferred at a rate of
 ;;;   7.14ms/binning

 
  for store_transfers_counter = 1, store_lines do begin   ;;; Transfers counter

      ;;; Update trap stats in store segment

     ;;; Generate random numbers for each store frame pixel. These
     ;;; will be used to determine the success of the trapping/release
     seed = store_transfers_counter + frames_n
     rv_store = randomu(seed, store_lines, trap_types)
     
     ;;; Update trap stats in store segment, looping over the traps in
     ;;; each pixel and the trap types
    
     for store_index = 0, store_lines -1 do begin    ;;; For each line transfer, update store traps occupancy stats.
        ;;; Add to flux the contribution from charge released from
        ;;; filled traps.

        flux_iteration = flux_store[store_index]
        flux_store[store_index] += total(release_following_pixel_flux_store[store_index,*])
        
        if flux_store[store_index] lt 0.001 then begin
          ;;; Compute charge that will be released in following pixel
          ;;; during next transfer and update the traps occupancy.
          release_following_pixel_flux_store[store_index, *] = store_traps_filled[store_index, *] * release_store_phase2_amplitude_by_specie
          store_traps_filled[store_index, *] -= release_following_pixel_flux_store[store_index, *]
          store_traps_free[store_index, *] = store_traps[store_index, *] - store_traps_filled[store_index, *]
        endif else begin
           
           if keyword_set(flag_retrapping) then flux_iteration += total(release_following_pixel_flux_store[store_index,*])
           flux_norm = flux_iteration/fwc
           fr_capture = flux_norm^(1.-charge_volume_coeff)
        
           for trap_index = 0, trap_types - 1 do begin
              capture_p = 1. - exp(-capture_cross_section[trap_index]  * store_phase2_transfer_time * flux_iteration^charge_volume_coeff)

           ;;; The flux is captured if a [0-1] random number is < Pc
           ;;; In that case, update the trap occupancy stats and the
           ;;; remaining flux in the pixel.
              if rv_store[store_index, trap_index] lt capture_p then begin              
                 losses = min([store_traps_free[store_index, trap_index] * fr_capture, flux_iteration])

              ;;; If charge released in same pixel is implemented
              ;;; (flag_same_pixel_relaese = 1) two sources contribute
              ;;; to the flux:
              ;;; 1 - Charge just captured
              ;;; 2 - Filled traps.
                 if flag_same_pixel_release then begin
                    losses -= losses * release_store_phase2_amplitude_by_specie_same_pix[trap_index]
                    release_same_pixel_charge = store_traps_filled[store_index, trap_index] * release_store_phase2_amplitude_by_specie_same_pix[trap_index]
                    store_traps_filled[store_index, trap_index] -= release_same_pixel_charge
                    store_traps_free[store_index, trap_index] +=  release_same_pixel_charge
                    flux_store[store_index] += release_same_pixel_charge
                 endif

              ;;; Update occupancy and flux value after capture
                 store_traps_filled[store_index, trap_index] += losses
                 store_traps_free[store_index, trap_index] = max([0.0, store_traps[store_index, trap_index] - store_traps_filled[store_index, trap_index]])
                 flux_store[store_index] = max([0.0, flux_store[store_index]-losses])
              endif
           
           ;;; Compute charge that will be released in following pixel
           ;;; during next transfer and update the traps occupancy.
              release_following_pixel_flux_store[store_index, trap_index] = store_traps_filled[store_index, trap_index] * release_store_phase2_amplitude_by_specie[trap_index]
              store_traps_filled[store_index, trap_index] -= release_following_pixel_flux_store[store_index, trap_index]
              store_traps_free[store_index, trap_index] = store_traps[store_index, trap_index] - store_traps_filled[store_index, trap_index]           
           endfor    ;;; END of trap type iteration in a pixel    
        endelse
        ;;; DEGUG/TEST - store trap2 occupancy in store segment during readout.
        store_trap0_filled_evo[store_index, store_lines+store_transfers_counter-1] = store_traps_filled[store_index, 0]
        store_trap1_filled_evo[store_index, store_lines+store_transfers_counter-1] = store_traps_filled[store_index, 1]
        store_trap2_filled_evo[store_index, store_lines+store_transfers_counter-1] = store_traps_filled[store_index, 2]
        store_trap3_filled_evo[store_index, store_lines+store_transfers_counter-1] = store_traps_filled[store_index, 3]
     endfor  ;;; END loop of trap occupancy updates over the store frame
     
     ;;; Store total number of free traps along image per specie
     total_free_store_evo_phase2[*, store_transfers_counter-1] =  total(store_traps_free, 1)

     ;;; FLUX (shift) UPDATE

     ;;; - Flux in store shifted by 1
     ;;; - Bottom line in store into the serial register
     flux_serial = replicate(flux_store[0], serial_columns/readout_nodes)
     flux_store = shift(flux_store, -1)
     flux_store[store_lines-1] = 0
     
     ;;; ---******---
     ;;; UPDATE TRAP STATS IN SERIAL REGISTER
     ;;;
     ;;; After each line transfer from the store to the serial
     ;;; register the entire line must be transferred over the serial
     ;;; register.
     
     for serial_transfers_counter = 1, serial_columns/readout_nodes do begin   ;;; Serial Transfers counter

      ;;; Update trap stats in serial register

     ;;; Generate random numbers for each serial register pixel. These
     ;;; will be used to determine the success of the trapping/release
        seed = serial_transfers_counter + frames_n
        rv_serial = randomu(seed, serial_columns/readout_nodes, trap_types)
     
     ;;; Update trap stats in serial segment, looping over the traps in
     ;;; each pixel and the trap types
    
        for serial_index = 0, serial_columns/readout_nodes -1 do begin    ;;; For each line transfer, update serial traps occupancy stats.
        ;;; Add to flux the contribution from charge released from
        ;;; filled traps.

           flux_iteration = flux_serial[serial_index]
           flux_serial[serial_index] += total(release_following_pixel_flux_serial[serial_index,*])
           
           if flux_serial[serial_index] lt 0.001 then begin
             ;;; Compute charge that will be released in following pixel
             ;;; during next transfer and update the traps occupancy.
             release_following_pixel_flux_serial[serial_index, *] = serial_traps_filled[serial_index, *] * release_serial_amplitude_by_specie
             serial_traps_filled[serial_index, *] -= release_following_pixel_flux_serial[serial_index, *]
             serial_traps_free[serial_index, *] = serial_traps[serial_index, *] - serial_traps_filled[serial_index, *]
           endif else begin
                      
              if keyword_set(flag_retrapping) then flux_iteration += total(release_following_pixel_flux_serial[serial_index,*])
              flux_norm = flux_iteration/fwc
              fr_capture = flux_norm^(1.-charge_volume_coeff)
        
              for trap_index = 0, trap_types - 1 do begin
                 capture_p = 1. - exp(-capture_cross_section[trap_index]  * serial_transfer_time * flux_iteration^charge_volume_coeff)

           ;;; The flux is captured if a [0-1] random number is < Pc
           ;;; In that case, update the trap occupancy stats and the
           ;;; remaining flux in the pixel.
                 if rv_serial[serial_index, trap_index] lt capture_p then begin
                    losses = min([serial_traps_free[serial_index, trap_index] * fr_capture, flux_iteration])

              ;;; If charge released in same pixel is implemented
              ;;; (flag_same_pixel_relaese = 1) two sources contribute
              ;;; to the flux:
              ;;; 1 - Charge just captured
              ;;; 2 - Filled traps.
                    if flag_same_pixel_release then begin 
                       losses -= losses * release_serial_amplitude_by_specie_same_pix[trap_index]
                       release_same_pixel_charge = serial_traps_filled[serial_index, trap_index] * release_serial_amplitude_by_specie_same_pix[trap_index]
                       serial_traps_filled[serial_index, trap_index] -= release_same_pixel_charge
                       serial_traps_free[serial_index, trap_index] +=  release_same_pixel_charge
                       flux_serial[serial_index] += release_same_pixel_charge
                    endif
               
              ;;; Update occupancy and flux value after capture
                    serial_traps_filled[serial_index, trap_index] += losses
                    serial_traps_free[serial_index, trap_index] = max([0.0, serial_traps[serial_index, trap_index] - serial_traps_filled[serial_index, trap_index]])
                    flux_serial[serial_index] = max([0.0, flux_serial[serial_index]-losses])
                 endif
           
           ;;; Compute charge that will be released in following pixel
           ;;; during next transfer and update the traps occupancy.
                 release_following_pixel_flux_serial[serial_index, trap_index] = serial_traps_filled[serial_index, trap_index] * release_serial_amplitude_by_specie[trap_index]
                 serial_traps_filled[serial_index, trap_index] -= release_following_pixel_flux_serial[serial_index, trap_index]
                 serial_traps_free[serial_index, trap_index] = serial_traps[serial_index, trap_index] - serial_traps_filled[serial_index, trap_index]          
              endfor
           endelse
        endfor  ;;; END loop of trap occupancy updates over the serial frame

     ;;; Update flux:
     ;;; - Flux in serial register image shifted by 1
        flux_serial = shift(flux_serial, -1)
        flux_serial[n_elements(flux_serial)-1] = 0 ;;; Last pixel in serial register is now empty of charge
        
     endfor  ;;; END of serial transfers loop
     print, 'TRANSFERRED STORE LINE N.', store_transfers_counter
         
     ;;; Store total number of free traps along image per specie
     total_free_serial_evo[*, store_transfers_counter-1] =  total(serial_traps_free, 1)
    
     ;;; DEGUG/TEST - store trap2 occupancy in serial readout register at the end of each store readout steps
     for serial_index = 0, serial_columns/readout_nodes -1 do begin
        serial_trap0_filled_evo[serial_index, store_transfers_counter-1] = serial_traps_filled[serial_index, 0]
        serial_trap1_filled_evo[serial_index, store_transfers_counter-1] = serial_traps_filled[serial_index, 1]
        serial_trap2_filled_evo[serial_index, store_transfers_counter-1] = serial_traps_filled[serial_index, 2]
        serial_trap3_filled_evo[serial_index, store_transfers_counter-1] = serial_traps_filled[serial_index, 3]
     endfor
  endfor  ;;; END of STORE transfer loop

  ;;; DEGUG - store trap filling in store segment
  openw, lu0, 't0_store_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu1, 't1_store_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu2, 't2_store_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu3, 't3_store_filled_frn'+frames_n_string+'.txt', /get_lun
  for transfer_counter = 1, store_lines*2-1 do begin  ;;; Note - store lines are transferred both during image and store readout.
     for store_line_counter = 0, store_lines-1 do begin
        printf, lu0, transfer_counter, store_line_counter, store_trap0_filled_evo[store_line_counter, transfer_counter-1],format='(i,i,d9.6)'
        printf, lu1, transfer_counter, store_line_counter, store_trap1_filled_evo[store_line_counter, transfer_counter-1],format='(i,i,d9.6)'
        printf, lu2, transfer_counter, store_line_counter, store_trap2_filled_evo[store_line_counter, transfer_counter-1],format='(i,i,d9.6)'
        printf, lu3, transfer_counter, store_line_counter, store_trap3_filled_evo[store_line_counter, transfer_counter-1],format='(i,i,d9.6)'
     endfor
  endfor
  free_lun, lu0
  free_lun, lu1
  free_lun, lu2
  free_lun, lu3
  
  
  ;;; DEGUG - serial trap filling in store segment
  openw, lu0, 't0_serial_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu1, 't1_serial_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu2, 't2_serial_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu3, 't3_serial_filled_frn'+frames_n_string+'.txt', /get_lun
  for transfer_counter = 1, store_lines-1 do begin  ;;; Note - store lines are transferred both during image and store readout.
    for serial_line_counter = 0, serial_columns/readout_nodes-1 do begin
      printf, lu0, transfer_counter, serial_line_counter, serial_trap0_filled_evo[serial_line_counter, transfer_counter-1],format='(i,i,d9.6)'
      printf, lu1, transfer_counter, serial_line_counter, serial_trap1_filled_evo[serial_line_counter, transfer_counter-1],format='(i,i,d9.6)'
      printf, lu2, transfer_counter, serial_line_counter, serial_trap2_filled_evo[serial_line_counter, transfer_counter-1],format='(i,i,d9.6)'
      printf, lu3, transfer_counter, serial_line_counter, serial_trap3_filled_evo[serial_line_counter, transfer_counter-1],format='(i,i,d9.6)'
    endfor
  endfor
  free_lun, lu0
  free_lun, lu1
  free_lun, lu2
  free_lun, lu3
  
  
  ;;; Debugging - Save total number of filled traps during image readout phaseA
  openw, lufreestoreb, 'free_store_evo_phaseB_frn'+frames_n_string+'.txt', /get_lun, width=100
  openw, lufreeserial, 'free_serial_evo_frn'+frames_n_string+'.txt', /get_lun, width=100
  for transfer_counter = 1, store_lines-1 do begin
    printf, lufreestoreb, transfer_counter, total_free_store_evo_phase2[*,transfer_counter-1]
    printf, lufreeserial, transfer_counter, total_free_serial_evo[*,transfer_counter-1]
  endfor
  free_lun, lufreestoreb
  free_lun, lufreeserial

  ;;; DEBUG START STASTS storing
  ;;; Store stats on flux and trap occupancy at each transfer in 1column consecutive arrays of length 3791 (the image length).
  ;;; To read from these files select ranges relative to a transfer, for example:
  ;;; [long(3791.*105):long(3791.*106-1)] will select data from 105th transfer. 
  openw, lu, 'ima_evo_frn'+frames_n_string+'.txt',/get_lun, width = 100
  for i = 0, 4314 do printf, lu, transpose(flux_ima_evo[*,i])
  free_lun, lu
  ;;; END DEBUG STATS STORING

;;; UPATE OCCUPANCY STATS IN IMAGE SEGMENT DURING STORE READOUT + EDU TIME
;;; Traps relese trapped
;;; electrons during exposure
;;; The exposure time must be at least as long as the complete store
;;; readout (1.98s) + EDU time (not clear at the moment).
;;; In the setup file the exposure time is set as
;;; ima_expo_time = 2.5
;;; That value is used at this time.  
  
  ;;; 2 - Fraction of released charge during that interval by trap
  ;;;     specie.
  fraction_released_image_expo = dblarr(trap_types)
  fraction_released_image_expo = 1. - exp(-image_expo_time/release_image_time)
  
  ;;; DEBUG START STATS storing
  ;;; Save trap occupancy of traps in image segment at the end of the exposure.
  openw, lu0, 't0_image_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu1, 't1_image_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu2, 't2_image_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu3, 't3_image_final_filled_frn'+frames_n_string+'.txt', /get_lun
  for ima_index = 0, image_lines-1 do begin
     for trap_index = 0, trap_types-1 do begin
         ;;; 3 - The released charge per image pixel and specie.
        released_trap_specie = image_traps_filled[ima_index, trap_index] * fraction_released_image_expo[trap_index]
        ;;; 4 - Update trap occupancy per image pixel and specie.
        image_traps_filled[ima_index, trap_index] -= released_trap_specie
        image_traps_free[ima_index, trap_index] = image_traps[ima_index, trap_index] - image_traps_filled[ima_index, trap_index]
        ;;; DEBUG STATS storing
        if trap_index eq 0 then printf, lu0, ima_index, image_traps_filled[ima_index, trap_index], format='(i,d9.6)'
        if trap_index eq 1 then printf, lu1, ima_index, image_traps_filled[ima_index, trap_index], format='(i,d9.6)'
        if trap_index eq 2 then printf, lu2, ima_index, image_traps_filled[ima_index, trap_index], format='(i,d9.6)'
        if trap_index eq 3 then printf, lu3, ima_index, image_traps_filled[ima_index, trap_index], format='(i,d9.6)'
     endfor
  endfor
  free_lun, lu0
  free_lun, lu1
  free_lun, lu2
  free_lun, lu3


;;; UPDATE OCCUPANCY STATE IN STORE + SERIAL SEGMENT DURING EDU TIME
;;; After store readout transfer stops for EDU processing.
;;; Update trap stats during EDU time while the frame is compliting
;;; its exposure
;;; The exposure time must be at least as long as the complete store
;;; readout (1.98s) + EDU time (not clear at the moment).
;;; In the setup file the exposure time is set as
;;; ima_expo_time = 2.5
;;; The total store readout interval is equals to:
;;; store_intervalstore_lines * store_transfer_time * (fix(serial_columns/readout_nodes/ccd_mode_binning)+ overscan_cols + 1)
  
  ;;; 1 - Total image readout time interval
  store_interval = store_lines * store_phase2_transfer_time
  edu_time = image_expo_time -  store_interval
  ;;; 2 - Fraction of released charge during that interval by trap
  ;;;     specie.
  fraction_released_edu = 1. - exp(-edu_time/release_image_time)

  ;;; DEBUG START STATS storing
  ;;; Save trap occupancy of traps in image segment at the end of the exposure.
  openw, lu0, 't0_store_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu1, 't1_store_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu2, 't2_store_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu3, 't3_store_final_filled_frn'+frames_n_string+'.txt', /get_lun
  for store_index = 0, store_lines-1 do begin
     for trap_index = 0, trap_types-1 do begin
         ;;; 3 - The released charge per store pixel and specie.
        released_trap_specie = store_traps_filled[store_index, trap_index] * fraction_released_edu[trap_index]
        ;;; 4 - Update trap occupancy per store pixel and specie.
        store_traps_filled[store_index, trap_index] -= released_trap_specie
        store_traps_free[store_index, trap_index] = store_traps[store_index, trap_index] - store_traps_filled[store_index, trap_index]
        ;;; DEBUG STATS storing
        if trap_index eq 0 then printf, lu0, store_index, store_traps_filled[store_index, trap_index], format='(i,d9.6)'
        if trap_index eq 1 then printf, lu1, store_index, store_traps_filled[store_index, trap_index], format='(i,d9.6)'
        if trap_index eq 2 then printf, lu2, store_index, store_traps_filled[store_index, trap_index], format='(i,d9.6)'
        if trap_index eq 3 then printf, lu3, store_index, store_traps_filled[store_index, trap_index], format='(i,d9.6)'     
     endfor
  endfor
  free_lun, lu0
  free_lun, lu1
  free_lun, lu2
  free_lun, lu3

  ;;; DEBUG START STATS storing
  ;;; Save trap occupancy of traps in serial readout register at the end of the exposure.
  openw, lu0, 't0_serial_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu1, 't1_serial_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu2, 't2_serial_final_filled_frn'+frames_n_string+'.txt', /get_lun
  openw, lu3, 't3_serial_final_filled_frn'+frames_n_string+'.txt', /get_lun
  for serial_index = 0, serial_columns/readout_nodes-1 do begin
     for trap_index = 0, trap_types-1 do begin
         ;;; 3 - The released charge per store pixel and specie.
        released_trap_specie = serial_traps_filled[serial_index, trap_index] * fraction_released_edu[trap_index]
        ;;; 4 - Update trap occupancy per store pixel and specie.
        serial_traps_filled[serial_index, trap_index] -= released_trap_specie
        serial_traps_free[serial_index, trap_index] = serial_traps[serial_index, trap_index] - serial_traps_filled[serial_index, trap_index]
        ;;; DEBUG STATS storing
        if trap_index eq 0 then printf, lu0, serial_index, serial_traps_filled[serial_index, trap_index], format='(i,d9.6)'
        if trap_index eq 1 then printf, lu1, serial_index, serial_traps_filled[serial_index, trap_index], format='(i,d9.6)'
        if trap_index eq 2 then printf, lu2, serial_index, serial_traps_filled[serial_index, trap_index], format='(i,d9.6)'
        if trap_index eq 3 then printf, lu3, serial_index, serial_traps_filled[serial_index, trap_index], format='(i,d9.6)'       
     endfor
  endfor
  free_lun, lu0
  free_lun, lu1
  free_lun, lu2
  free_lun, lu3
  

  
;;; Populate the trap_density_filled_combined array that includes the
;;; density of filled traps in image, store and serial register.

  trap_density_filled_combined = dblarr(image_lines + store_lines + serial_columns/readout_nodes, trap_types)
  trap_density_filled_combined[0:image_lines-1, *] = image_traps_filled
  trap_density_filled_combined[image_lines:image_lines+store_lines-1, *] = store_traps_filled
  trap_density_filled_combined[image_lines+store_lines:image_lines + store_lines + serial_columns/readout_nodes-1, *] = serial_traps_filled
  
  return, trap_density_filled_combined
end
