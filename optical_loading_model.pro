function optical_loading_model, optical_load_flux, n_frames, ccd_integration_time, readout_image_time, readout_serial_time, trap_density, release_image_time, capture_cross_section, charge_volume_coeff, ccd_mode_binning, image_section_lines, store_section_lines, serial_columns, readout_nodes, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release = flag_same_pixel_release

;
;+
; NAME: optical_loading_process.pro
;
; PURPOSE: CTI distortion of an array of flux values representing the optical loading of the SMILE CCD
;
; CATEGORY: CDM
;
; INPUTS:
;
; optical_load_flux = Single flux value corresponding to the optical loading in the image area per pixel
; 
; n_frames = number of consecutive frames with diffuse optical signal to model
; 
; ccd_integration_time = A frame exposure time
; 
; readout_sequence = array of transfer times of pixels in the line
;
; trap_density = array of lenght s, s=number of trap species
;
; decay_index = array of lenght s of charge release timescales
;
; capture_cross_section = array of lenght s of trap capture cross section
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
;           1- Released electrons retrapped
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
; OUTPUTS: Array of trap occupancies statistics.
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

; optical_loading_trap_stats = optical_loading_cdm(flux_input_electrons, readout_time, trap_density, release_image_time, capture_cross_section_image, charge_volume_coeff_image, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release=flag_same_pixel_release)
;
; MODIFICATION HISTORY:
;   Written by:
;   Claudio Pagani
;
;  13 July - Function adapted from cdm_process to specifically process a line of optical loading
;Graphics
;DEVICE,DECOMPOSED=0.
;tvlct,[255,0,255,0,0],[255,255,0,0,0],[255,0,0,255,0],1
;entry_device = !d.name


;;; 1 - setup the input

flux_column = dblarr(image_section_lines + store_section_lines)
flux_column[store_section_lines: image_section_lines + store_section_lines-1] = optical_load_flux
trap_density_column = dblarr(n_elements(flux_column), n_elements(trap_density))
serial_register_readout_length = serial_columns/readout_nodes
trap_density_row = dblarr(serial_register_readout_length, n_elements(trap_density))


for trap_types = 0, n_elements(trap_density)-1 do begin
  trap_density_column[*,trap_types] = trap_density[trap_types]
  trap_density_row[*,trap_types] = trap_density[trap_types]
endfor

trap_density_free_column = trap_density_column
trap_density_filled_column = trap_density_column - trap_density_free_column

trap_density_free_row = trap_density_row
trap_density_filled_row = trap_density_row - trap_density_free_row

;;; Populate the image segment with optical flux
flux_line = dblarr(image_section_lines+store_section_lines)
flux_line[store_section_lines: store_section_lines+image_section_lines-1] = optical_load_flux


;;; 2 - Call the function to return the trap occupancy stats at the end of the readout of the first frame

trap_occupancy_ol = optical_loading_cdm_process(flux_line, image_section_lines, store_section_lines, serial_columns, readout_nodes, readout_image_time, readout_serial_time, trap_density_column, trap_density_filled_column, trap_density_free_column, trap_density_row, trap_density_filled_row, trap_density_free_row, ccd_mode_binning, release_image_time, capture_cross_section, charge_volume_coeff,  flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release = flag_same_pixel_release)

;;; Sub very low values with zeros.
low_occupancy_reset_index = where(trap_occupancy_ol lt 1.E-6, nlow)
if nlow gt 0 then trap_occupancy_ol[low_occupancy_reset_index] = 0.0

;;; 3 - Repeat the call for a number of frames, ideally until an equilibrium is reached

for frames_counter = 1, n_frames-1 do begin
  ;;; Get trap occupancy stats after frame readout
  trap_density_filled_column = trap_occupancy_ol[0:image_section_lines + store_section_lines -1, *]
  trap_density_filled_row = trap_occupancy_ol[image_section_lines + store_section_lines: image_section_lines + store_section_lines + serial_register_readout_length -1, *]
  print, 'Readout frame n. ', frames_counter
  print, 'Total filled traps in image+store column', total(trap_density_filled_column)
  print, 'Total filled traps in serial register', total(trap_density_filled_row)
  ;Calculate the interval between the passage of the diffuse optical charge of two successive frames.
  frames_interval = ccd_integration_time + store_section_lines * readout_image_time * ccd_mode_binning
  dimensions = size(trap_occupancy_ol)
  ; Calculate trapped charge released between two consecutive frames
  release_frames_intervals = trap_occupancy_ol * (1. - exp(-rebin(1.0*[frames_interval], dimensions[1], dimensions[2]) / rebin(transpose(release_image_time), dimensions[1], dimensions[2])))

;PRINT, WHERE(FINITE(trap_occupancy_ol, /NAN))
;PRINT, WHERE(FINITE(release_frames_intervals, /NAN))
  print, 'Total occupancy ',total(trap_occupancy_ol)
  print, 'Total release ', total(release_frames_intervals)
  ;print, 'update occupancy'
  ; Update occupancy after release and before the readout of the following frame
  trap_occupancy_ol -= release_frames_intervals

 ; PRINT, WHERE(FINITE(trap_occupancy_ol, /NAN))
  print, 'Total occupancy after release has freed some traps, ready for following frame', total(trap_occupancy_ol)

  print, 'prepare for next frame...'''
  trap_density_filled_column = trap_occupancy_ol[0:image_section_lines + store_section_lines -1, *]
  trap_density_filled_row = trap_occupancy_ol[image_section_lines + store_section_lines: image_section_lines + store_section_lines + serial_register_readout_length -1, *]
  trap_density_free_column = trap_density_column - trap_density_filled_column
  trap_density_free_row = trap_density_row - trap_density_filled_row
  
  ; Process readout of following frame
  trap_occupancy_ol = optical_loading_cdm_process(flux_line, image_section_lines, store_section_lines, serial_columns, readout_nodes, readout_image_time, readout_serial_time, trap_density_column, trap_density_filled_column, trap_density_free_column, trap_density_row, trap_density_filled_row, trap_density_free_row, ccd_mode_binning, release_image_time, capture_cross_section, charge_volume_coeff,  flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release = flag_same_pixel_release)
  low_occupancy_reset_index = where(trap_occupancy_ol lt 1.E-6, nlow)
  if nlow gt 0 then trap_occupancy_ol[low_occupancy_reset_index] = 0.0
endfor
print, 'End of optical loading'
return, trap_occupancy_ol

end






