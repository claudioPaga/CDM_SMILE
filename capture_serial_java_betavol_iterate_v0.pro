function capture_serial_java_betavol_iterate_v0, flux, ac_coord, trap_density_free_array, capture_cross_section, transfer_times_pixels, charge_volume_coeff, total_release_following_pixel_array,  flag_retrapping = flag_retrapping, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial=flag_binomial
;
; CP, 27 July 2017
;
; PURPOSE - Returns losses (number of captured electrons by different
;          trap types), based on Pcapture and fraction of
;          volume "seen" by charge cloud
;
; Inherited from capture_serial_java_betavol_v0
; Added iterative processing, with flux as a function of AC updated and
; losses re-evaluated.
;
; flux_iteration represents the flux of the charge packet that
; interacts with traps in the pixels of the serial register during
; transport
; At first, flux assumed constant during the transfer
; The estimate is updated correcting the flux with the cumulative
; calculated offsets
; Ex: Run0 Flux =   [5, 5, 5]
;          Losses = [1, 1, 1]
;          Cum_lo = [3, 2, 1]
;     Run1 Flux =   [3, 4, 5]
;          Losses = [0.8, 0.9, 1]
;          Cum_lo = [2.7, 1.9, 1]
;     Run2 Flux =   [3.1, 4, 5]
;          Losses = [0.81, 0.9, 1]
;     Until Losses(runN) - Losses(runN-1) < Threshold) or RunN >  Trial_max
;
; Cumulative losses calculation details:
;
; 1) Add losses of all species along AC
; 2) Calculate cumulative vs AC
; 3) Shift by -1, as losses will affect flux in the following pixel
; 4) Update estimated flux of packet as it's transferred
; Ex: 2 trap species
; Flux   = [15, 15, 15]
; Losses = [[1, 2, 2], 
;           [3, 3, 5]]
; L_spec = [4, 5, 7]
; L_cumu = [16, 12, 7]
; L_c_sh = [12, 7, 0]
; Flux   = [ 3, 8, 15]
;
;
;Capture probability: Probability that a trap will be filled after
;transit of a charge packet of flux=flux
;
;Low fluxes - Treated with two implementations, depending on bernoulli
;             parameter
;             A) short_bernoulli = 0, then if losses > flux ==> losses = flux
;             B) short_bernoulli = 1  
;             C) long_bernoulli = 1         

;             With the Bernoulli treatment I calculate the expected
;             number of losses assuming a bernoulli process of
;             capture/no capture during charge transfer.
;             Each trap specie has its own bernoulli distribution.
;             I assume the resulting process is the sum of the
;             distributions, with mean = sum of mean, stdev =
;             sqrt(sum(stdev^2))
;             I calculate the intergral of that distribution with
;             allowed losses up to flux.
;
;
;
;NOTE: the difference between bernoulli ON/OFF is relevant for fluxes
;around +/-3 sigma or so of the expected losses.
;NOTE: To reach the condition losses > flux the flux must be very low; if
;the flux is low than Pc is low, so the losses calculated as losses_array = trap_density_free_array * n_transfers * capture_prob_array  
;is also low, so that losses > flux is really a condition that
;is difficult to satisfy for the typical density of traps and cross setion.
;
;The Short Bernoulli treatment is better then the simplistic option of
;equating the losses to the flux.
;But I'm still making the assumption that the losses determine
;thru the Bernoulli formula are uniformely distributed over the
;pixels. This is not what really happens, but it might be close enough
;if what we are interested in is the amount of losses, and not how
;these losses are distributed over the register.
;
;An extended Bernoulli treatment is performed with long_bernoulli=1
;There the integral of the Bernoulli distribution is calculated for
;each pixel, and the remaining flux after that pixel transfer is
;updated before calculating the integral of the following AC pixel.
;This long Bernoulli treatment is very long, as it's a pixel by
;pixel processing, I don't believe it will ever by implemented but
;it's good to have it here for testing/evaluation.

;fr_captured = fraction of volume seen by signal cloud, between 0 and 1
;losses = fr_captured * trap_density * n_transfers

;NOTE on Volume occupied by charge in the captrure_prob_array equation
;In the equation it's OK to use flux (instead of fr_capture),
;as the probability is a function of the number of electrons (flux),
;and the fractional volume occupied by it is implicity included in the
;capture_cross_section values)
;The capture prob. is the probability that a trap will capture and electron

  ;Set standard parameters if not provided by user
  if not(keyword_set(iter_max)) then iter_max = 5
  if not(keyword_set(threshold)) then threshold = 0.01
  
  ;Set constants
  ; sfwc = 4.e+5                  ;Full well of serial pixel (from CDM docs)
  ; sfwc = 9.e+5 ;Full well of SMILE pixel, from https://www.teledyne-e2v.com/content/uploads/2017/08/ProcSPIE_PLATO.pdf
  sfwc = 2.e+4 ; Capacity of Supplementary buried channel.
  

  ;;; Create mask where electrons are transferred
  dimens = size(trap_density_free_array)
  transfer_mask = dblarr(dimens[1], dimens[2])
  transfer_mask[0:ac_coord-1,*] = 1.  

;Initialize flux_iteration array - Step 2 in PDF presentation
  flux_iteration = dblarr(dimens[1])
;;;RETRAPPING ON: the released electrons accumulate as the charge is readout
  if keyword_set(flag_retrapping) then flux_iteration[0:ac_coord-1] = flux + reverse(total(reverse(total_release_following_pixel_array[0:ac_coord-1]),/cumulative)) else flux_iteration[0:ac_coord-1] = flux
  flux_initial = flux_iteration

;Initialize variables used in procedure
  count_iteration = 0
  losses_start = 0.
  repeat begin   ;Iteration loop START

  ;;;The iteration derives the losses for each pixel and for each trap
  ;;;species. It covers Steps 3 and 4 of the PDF presentation. The
  ;;;calculations are performed with arrays, that need to be all of
  ;;;the same dimensions (array dimension settings with rebin function)

  
     flux_norm = flux_iteration/sfwc
     fr_capture = flux_norm^(1.-charge_volume_coeff)
     
                                ;Capture probability in each pixel for each trap specie (grid of serial_reg_length x trap species)
     capture_prob_array = 1. - exp(-transpose(rebin(capture_cross_section, dimens[2], dimens[1]))  * rebin(transfer_times_pixels, dimens[1], dimens[2]) * (rebin(flux_iteration, dimens[1], dimens[2]))^charge_volume_coeff)
                                     ;Losses in each pixel
     losses_pixel_array = trap_density_free_array * transfer_mask * rebin(fr_capture,dimens[1], dimens[2]) * capture_prob_array
                                     ;Total losses for each trap specie,
                                ;summed over the serial register
     losses_array_specie_tot = total(losses_pixel_array, 1)
          ;;; Calculate cumulative losses along register, update dynamic flux during charge transfer

     cumulative_losses = shift(reverse(total(reverse(total(losses_pixel_array, 2) ),/cumulative)),-1)
     cumulative_losses[dimens[1]-1] = 0
     flux_iteration = flux_initial - cumulative_losses
     flux_iteration[WHERE(flux_iteration LT 0., /NULL)] = 0. ;Fluxes can't be negative
     ;print, 'Losses along array'
     ;print, total(losses_pixel_array, 2)
     ;print, 'Cumulative losses'
     ;print, cumulative_losses
     ;print, 'Running flux'
     ;print, flux_iteration
     ;print, 'Iteration, flux, Losses_pre, new_losses'
     ;print, count_iteration, flux, losses_start, total(losses_pixel_array)
     diff_losses = abs(total(losses_pixel_array) - losses_start)
     count_iteration = count_iteration + 1
     losses_start = total(losses_pixel_array)
  endrep until count_iteration eq iter_max or diff_losses lt 0.001 ;Iteration loop STOP


  
  ;;; Binomial treatment
  ;;; Adjust the losses by introducing randomisation of the losses
  ;;; based on the binomial distribution of probability equals to the
  ;;; capture_prob.
  ;;; The capture/non-capture is stored in the updated transfer_mask array

  if keyword_set(flag_binomial) then begin
     for index_run = 0, n_elements(capture_prob_array) - 1 do begin
        transfer_mask[index_run] *= RANDOMU(seed, 1, binomial = [1, capture_prob_array[index_run]])   ; RANDOMU will return a single value (1/0) representing the capture/non capture of the electron with a probability drawn from the binomial distribution, and N = 1 Trial
     endfor
     ;;; Calculate the losses based on the modelled binomial process
     ;;; included in transfer_mask.
     ;;; NOTE - There is no need to multiply by the capture
     ;;;        probability as this has already been taken into
     ;;;        account by the binomial treatment included in trasfer_mask
     losses_pixel_array = trap_density_free_array * transfer_mask * rebin(fr_capture,dimens[1], dimens[2])
  endif

;;; End of binomial treatment

;;; Bernoulli treatment, SHORT version

  if keyword_set(flag_retrapping) then flux_quota = flux + total(total_release_following_pixel_array[0:ac_coord-1]) else flux_quota = flux  

 ; print, total(losses_pixel_array), flux_quota

  if (KEYWORD_SET(short_bernoulli)) then begin
     fractional_losses = losses_pixel_array/total(losses_pixel_array)
     bernoulli_mean = total(losses_pixel_array) ;Expected total number of captures
     bernoulli_sigma = sqrt(total(((1. - capture_prob_array) *  capture_prob_array * trap_density_free_array * transfer_mask * rebin(fr_capture,dimens[1], dimens[2]))^2))
     if flux_quota lt bernoulli_mean+3.*bernoulli_sigma then begin
        total_losses = bernoulli_sigma/sqrt(2.*!PI) * (exp(-(bernoulli_mean)^2/(2.*bernoulli_sigma^2)) - exp(-(bernoulli_mean-flux_quota)^2/(2.*bernoulli_sigma^2))) + 0.5 * bernoulli_mean * (ERF((bernoulli_mean)/(sqrt(2.)*bernoulli_sigma)) - ERF((bernoulli_mean-flux_quota)/(sqrt(2.)*bernoulli_sigma))) + flux_quota * 0.5 * (ERF((bernoulli_mean-flux_quota)/(sqrt(2.)*bernoulli_sigma)) +1.)
        losses_pixel_array = total_losses * fractional_losses
     endif
  endif                         ;End SHORT bernoulli correction

  ;;;LONG Bernoulli processing
  ;;;Time-expensive, pixel by pixel processing. I don't think
  ;;;this will ever by implemented.

  flux_running = flux 
  if (KEYWORD_SET(long_bernoulli)) then begin
     for coordinate = ac_coord-1, 0, -1 do begin
        if keyword_set(flag_retrapping) then flux_running = flux_running + total(total_release_following_pixel_array[coordinate]) else flux_running = flux_running
        flux_norm = flux_running/sfwc
        fr_capture = flux_norm^(1.-charge_volume_coeff)
        capture_prob_pix = 1. - exp(-capture_cross_section * replicate(transfer_times_pixels[coordinate,*],dimens[2])  * replicate(flux_running, dimens[2])^charge_volume_coeff)
        losses_pix = trap_density_free_array[coordinate,*] * replicate(fr_capture, dimens[2]) * capture_prob_pix
        fractional_losses = losses_pix/total(losses_pix)
        bernoulli_mean = total(losses_pix)
        bernoulli_sigma = sqrt(total(((1. - capture_prob_pix) *  capture_prob_pix * trap_density_free_array[coordinate,*] * replicate(fr_capture, dimens[2]))^2))
        ;;;Check the condition when the charge losses derivation with
        ;;;integral becomes significantly different wrt
        ;;;approximation. But it's possible the condition just
        ;;;slows down the code, in that case it can be taken out. 
        if flux_running lt bernoulli_mean+3.*bernoulli_sigma then begin
           total_losses = bernoulli_sigma/sqrt(2.*!PI) * (exp(-(bernoulli_mean)^2/(2.*bernoulli_sigma^2)) - exp(-(bernoulli_mean-flux_running)^2/(2.*bernoulli_sigma^2))) + 0.5 * bernoulli_mean * (ERF((bernoulli_mean)/(sqrt(2.)*bernoulli_sigma)) - ERF((bernoulli_mean-flux_running)/(sqrt(2.)*bernoulli_sigma))) + flux_running * 0.5 * (ERF((bernoulli_mean-flux_running)/(sqrt(2.)*bernoulli_sigma)) +1.)
           losses_pixel_array[coordinate,*] = total_losses * fractional_losses
        endif else losses_pixel_array[coordinate,*] = losses_pix
        flux_running = flux_running - total(losses_pixel_array[coordinate,*])
     endfor
  endif                         ;End LONG bernoulli correction

  if not(KEYWORD_SET(short_bernoulli)) and not(KEYWORD_SET(long_bernoulli)) then begin
     if total(losses_pixel_array) gt flux_quota then losses_pixel_array = losses_pixel_array*flux_quota/total(losses_pixel_array) ; This ensures I have a max of flux e- losses
  endif
    ;print, losses_pixel_array
  return, losses_pixel_array
end
  
