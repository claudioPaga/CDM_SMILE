function cdm_process, flux_input, readout_time, trap_density, decay_index, capture_cross_section, charge_volume_coeff,  flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial

;
;+
; NAME: cdm_process.pro
;	
; PURPOSE: CTI distortion of an array of flux values
;
; CATEGORY: CDM
;
; INPUTS:
;
; flux_input = Array of flux values
;
; readout_sequence = array of transfer times of pixels in the line
;
; trap_density = [flux_input_length x s] array, s=number of trap species 	
;
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
; OUTPUTS: Distorted line
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
; cdm_process, flux_input, readout_time, trap_density, decay_index, capture_cross_section, charge_volume_coeff, flag_retrapping = 1, flag_iteration = 1, iter_max = 5, threshold = 0.1, short_bernoulli = 1, long_bernoulli = 0
;	
; MODIFICATION HISTORY:
; 	Written by:	
;		Claudio Pagani
;- 27 July 2017
;  Added comments before sending to Wolfgang in view of Java implementation
;
; - 9 Sept 2020
;   Updates to adapt for use with SMILE

;Graphics
;DEVICE,DECOMPOSED=0.
;tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1
;entry_device = !d.name


;Parameters initialization
flux_segment_length = n_elements(flux_input)  ;This might be GaiaParam>Satellite.CCD_NUMBEROFCOLUMNS 
dimensions = size(trap_density) ;size is IDL function, returned array dimensions will be dimensions[1] = line/column length dimensions[2] = number of trap species
trap_types = dimensions[2]  
flux_initial = flux_input

;As a placeholder, set the filled traps to zero at the start.
;In Java calibrations, this might be a function of the subtracted
;background, or some other initialization process
;filled_traps_array = trap_initialization_function(trap_density, background, bias etc..)
;But it might as well be that the initial status at the beginning of a
;line readout is indipendent of the observations, as all the trapped
;charge might have been released within 2 line readouts


;Arrays are represented to match the intuitive visualisation of the
;serial register, with an array of 1966 in length, and each array row for a
;specific trap species.

;Arrays initialization
trap_density_filled_array = dblarr(flux_segment_length, trap_types) 
trap_density_free_array = trap_density - trap_density_filled_array
release_following_pixel_array = dblarr(flux_segment_length, trap_types)
damaged_signal = dblarr(n_elements(flux_initial))

;In case of only 1 trap specie I need to reformat the arrays just
;created to become formally Array[flux_segment_length, 1]
;This is needed to correctly apply total function when totalling over
;trap species with trap_types eq 1.

if trap_types eq 1 then begin
   trap_density_filled_array = reform(trap_density_filled_array, flux_segment_length, trap_types)
   trap_density_free_array = reform(trap_density_free_array, flux_segment_length, trap_types)
   release_following_pixel_array = reform(release_following_pixel_array, flux_segment_length, trap_types)
endif

;Total released charge for all trap species over the serial register
total_release_following_pixel_array = total(release_following_pixel_array, 2)

;FILES used during testing phase
;openw, lu_losses, 'test_losses.txt', /get_lun, width=250
;openw, lu_density_filled,'test_density_filled.txt', /get_lun, width=250
;openw, lu_density_free,'test_density_free.txt', /get_lun, width=250
;openw, lu_released, 'test_released.txt', /get_lun, width=250
;
;openw, lu_specie_losses, 'test_specie_losses.txt', /get_lun, width=250
;openw, lu_specie_density_filled,'test_specie_density_filled.txt', /get_lun, width=250
;openw, lu_specie_density_free,'test_specie_density_free.txt', /get_lun, width=250
;openw, lu_specie_released, 'test_specie_released.txt', /get_lun, width=250

;openw, lu_report, 'report_stats.txt', /get_lun, width=250
;printf, lu_report, '       AC     Flux_model    Flux_plus_released     Losses       Damaged       Released           ALL_AC         Filled_AC        Empty_AC'

;Signal distortion

for ac_counter = 0, flux_segment_length-1 do begin

;Determine capture probabilities and losses in each pixel for each
;trap specie

   ;;; Procedure is called that will cover steps 2-3-4, returning the
   ;;; array of losses along serial register.
   ;;; There are actually at this stage 2 procs that are called
   ;;; depending on the ITERATION being on/off.
   ;;; In reality of iter_max =1 then
   ;;; capture_serial_java_betavol_iterate_v0.pro returns the same
   ;;; result as capture_serial_java_betavol_v0.pro.
   ;;; But for now I keep it like this as testing is needed to check
   ;;; if iteration is needed at all.

   ;losses_pixel_array = dblarr(flux_segment_length)


   ; Calculate losses if flux non-negligeble
   if flux_input[ac_counter] gt 0. then begin
      if keyword_set(flag_iteration) then losses_pixel_array =  capture_serial_java_betavol_iterate_v0(flux_input[ac_counter], ac_counter+1, trap_density_free_array, capture_cross_section, readout_time, charge_volume_coeff, total_release_following_pixel_array,  flag_retrapping = flag_retrapping, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli,long_bernoulli = long_bernoulli, flag_binomial = flag_binomial) $
      else losses_pixel_array =  capture_serial_java_betavol_v0(flux_input[ac_counter], ac_counter+1, trap_density_free_array, capture_cross_section, readout_time, charge_volume_coeff, total_release_following_pixel_array, flag_retrapping = flag_retrapping, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial)
   endif else losses_pixel_array = dblarr(flux_segment_length, trap_types)


   total_losses_pixel_array = total(losses_pixel_array)
;   printf, lu_losses, losses_pixel_array
;   printf, lu_specie_losses, total(losses_pixel_array, 2)
   
   ;;;Determine damaged signal  - Step 6
   damaged_signal[ac_counter] = flux_input[ac_counter] - total_losses_pixel_array + total(release_following_pixel_array)
      ;print, flux_input[ac_counter], damaged_signal[ac_counter]
      ;stop
      ;flux_and_release = flux_input[ac_counter]+total(release_following_pixel_array)
   
;;; Update number of trapped electons in each pixel by each trap
;;; specie - Step 7, part A, relative o the losses.
   trap_density_filled_array = trap_density_filled_array + losses_pixel_array

   total_trap_density_filled_array = total(trap_density_filled_array)
   
;;; Calculate number of released e- in following line.   Step 5
;;; Calculated only if the losses derived earlier are above a
;;; significant threshold, otherwise reset the trap statistics and set
;;; the released charge in following pixel as equal to the total
;;; trappped charge
   
   if total_trap_density_filled_array gt 0.1 then begin
   
      release_following_pixel_array = trap_density_filled_array * (1. - exp(-rebin(readout_time, dimensions[1], dimensions[2]) / rebin(transpose(decay_index), dimensions[1], dimensions[2])))
      total_release_following_pixel_array = total(release_following_pixel_array,2)
            
      ;;; Only calculate released electrons from rawy = 0 to rawy = ac
      ;for speciesIndex = 0, trap_types-1 do begin
      ;   specie_trap_density_filled_array = trap_density_filled_array[*, speciesIndex]
      ;   nonzeroIndex = where(specie_trap_density_filled_array gt 0., nn0)
      ;   if nn0 gt 0 then release_following_pixel_array[nonzeroIndex, speciesIndex] = trap_density_filled_array[nonzeroIndex, speciesIndex] * (1.0 - exp(readout_time[nonzeroIndex] * decay_index[speciesIndex]))
         ;for acsteps = 0, ac_counter do begin
         ;   release_following_pixel_array[acsteps, speciesIndex] = trap_density_filled_array[acsteps, speciesIndex] * (1.0 - exp(readout_time[acsteps] * decay_index[speciesIndex]))
         ;endfor
      ;endfor
      
      total_release_following_pixel_array = total(release_following_pixel_array,2)
      ;print, total(release_following_pixel_array)
      ;stop

;   printf, lu_released, release_following_pixel_array
;   printf, lu_specie_released, total(release_following_pixel_array,2)
;;; Update number of trapped electons in each pixel by each trap specie
;;; after release
 
   ;;; Update number of trapped electons in each pixel by each trap
;;; specie - Step 7, part B, relative to the next charge release
      trap_density_filled_array = trap_density_filled_array - release_following_pixel_array


;   printf, lu_density_filled, trap_density_filled_array
;   printf, lu_specie_density_filled, total(trap_density_filled_array,2)
;Update density of free traps
      trap_density_free_array = trap_density - trap_density_filled_array
      ;stop
   endif else begin

      ;;; If the 
      
      total_release_following_pixel_array = total(trap_density_filled_array, 2)
      release_following_pixel_array = dblarr(flux_segment_length, trap_types)
      trap_density_filled_array = dblarr(flux_segment_length, trap_types)
      trap_density_free_array = trap_density
      
      
   endelse

;   printf, lu_density_free, trap_density_free_array
;   printf, lu_specie_density_free, total(trap_density_free_array,2)
                                ;I don't need the following commands as if flag_retrapping is ON I add the flux directly in the capture_serial_java_betavol procedure
   ;if (ac_counter lt flux_segment_length-1 and keyword_set(flag_retrapping)) then begin
   ;   flux_input[ac_counter+1] = flux_input[ac_counter+1] + total(release_following_pixel_array)
   ;endif
   ;printf, lu_report, ac_counter+1, flux_input[ac_counter], flux_and_release, total(losses_pixel_array), damaged_signal[ac_counter], total(release_following_pixel_array),  total(trap_density[0:ac_counter,*]), total(trap_density_filled_array[0:ac_counter,*]), total(trap_density_free_array[0:ac_counter,*])
endfor
;free_lun, lu_report
;free_lun, lu_losses
;free_lun, lu_density_filled
;free_lun, lu_density_free
;free_lun, lu_released

;free_lun, lu_specie_losses
;free_lun, lu_specie_density_filled
;free_lun, lu_specie_density_free
;free_lun, lu_specie_released



;This final part is just for plotting purposes/saving files, not
;relevant to the CDM procedure itself

;!P.Multi = [0, 1, 2, 0, 1]
                                ;Plot
;yll = min([flux_initial, damaged_signal])-10
;yul = max([flux_initial, damaged_signal])+10
;plot, indgen(flux_segment_length)+1, flux_initial, background=1, color=0, xtit= 'AC', ytit='Flux', tit='Serial CDM Java v0 (Black=Model, Red=Damaged)', yr=[yll, yul]
;oplot, indgen(flux_segment_length)+1, damaged_signal, color=3
;plot, indgen(flux_segment_length)+1, flux_initial - damaged_signal, color=0, xtit= 'AC', ytit='Model-Damaged'


;PS version of plot begin
;entry_device = !d.name
;if keyword_set(flag_retrapping) then plotfilenameps = 'damaged_cdm_serial_java_v0_retrap.ps' else plotfilenameps = 'damaged_cdm_serial_java_v0.ps'
;set_plot,'ps'
;device, filename = plotfilenameps, /color,/tt_font, set_font='Times', font_size=10
;plot, indgen(flux_segment_length)+1, flux_initial, background=1, color=0, xtit= 'AC', ytit='Flux', tit='Serial CDM Java v0 (Black=Model, Red=Damaged)', yr=[yll, yul]
;oplot, indgen(flux_segment_length)+1, damaged_signal, color=3
;plot, indgen(flux_segment_length)+1, flux_initial - damaged_signal, color=0, xtit= 'AC', ytit='Model-Damaged'
;device,/close  
;set_plot,entry_device
;PS version of plot end

;!P.Multi = [0]

;Output flux file
;flags_text = 'retrap'+strtrim(string(flag_retrapping),2)+'_iter'+strtrim(string(flag_iteration),2)+'_bshort'+strtrim(string(short_bernoulli),2)+'_blong'+strtrim(string(long_bernoulli),2)+'.txt'

;out_file = 'damaged_cdm_serial_java_v0_'+flags_text
;out_print_file = [transpose(indgen(flux_segment_length)+1), transpose(flux_initial),  transpose(damaged_signal)]
;openw, lu, out_file,/get_lun, width=250
;printf, lu, 'AC   Model   Damaged'
;printf, lu, out_print_file
;free_lun, lu

;!P.Multi = [0, 1, 1, 0, 1]

return, damaged_signal
end

  
  


      
