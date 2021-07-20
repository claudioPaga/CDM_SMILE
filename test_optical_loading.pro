pro test_optical_loading, optical_load_flux, n_frames, ccd_integration_time

  ;
  ;+
  ; NAME: test_optical_loading.pro
  ;
  ; PURPOSE: Call the optical loading model function for testing
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
  ; SUMMARY - The procedure loads the trap paramters and calls the optical loading function.
  ;
  
  restore, 'distort_cdm_properties.sav'
  print, 'trap density image = ', trap_species_image_density
  print, 'charge_injection_flag = ', charge_injection_flag
  print, 'capture_cross_section_image = ', capture_cross_section_image
  print, 'flag_unbinned = ', flag_unbinned
  print, 'Optical load flux = ', optical_load_flux
  print, 'CCD frame expo time [s] = ', ccd_integration_time
  
  trap_occupancy_ol = optical_loading_model(optical_load_flux, n_frames, ccd_integration_time, readout_image_time, readout_serial_time, trap_species_image_density, release_image_time, capture_cross_section_image, charge_volume_coeff_image, ccd_mode_binning, image_section_lines, store_section_lines, serial_columns, readout_nodes, flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release = flag_same_pixel_release)
;  trap_occupancy_ol = optical_loading_model(optical_load_flux, readout_image_time, readout_serial_time, trap_species_image_density, release_image_time, capture_cross_section_image, charge_volume_coeff_image)
  ;, ccd_mode_binning, image_section_lines, store_section_lines, serial_columns,  flag_retrapping = flag_retrapping, flag_iteration = flag_iteration, iter_max = iter_max, threshold = threshold, short_bernoulli = short_bernoulli, long_bernoulli = long_bernoulli, flag_binomial = flag_binomial, flag_same_pixel_release = flag_same_pixel_release)

  stop
  end
  
