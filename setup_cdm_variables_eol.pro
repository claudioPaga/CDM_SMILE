pro setup_cdm_variables

;;;
;;; CP, 8 Sept 2020
;;; Summary - Writes the file to store trap parameters and properties
;;;           values to be used in the CDM processing
;;;           Trap densities are for EOL estimated CTI
;;;
;;; Output - distort_cdm_properties.sav file
;;;
;;; Times are in units of the arbitrary pixel transfer time unit.

image_section_lines = 3791
store_section_lines = 719
serial_columns = 4510
readout_nodes = 2
;ccd_mode_binning can be anything but should be any of 1/6/24
ccd_mode_binning = 6
;Charge injection info
charge_injection_flag = 0
;Convergence threshold (ev) of iterative CTI correction algorithm
iter_th = 1
;Makimum number of attempts for iterative CTI correction algorithm
iter_max = 10

charge_injection_period_frame = 1 ;
charge_injection_time_gap = 1000 ; This is time between the parallel CI readout ends (CI line in serial register) and the start of the image readout. It includes 1-full serial readout of CI line (will change depending on readout node 1/2), exposure time and any other wait time due to CCD operations.
charge_injection_block_lines = 1 ; Number of consecutive lines injected with charge
charge_injection_electrons = 10000 ; Number of electrons per pixel of injected charge.
;trap release timescales are in units of the readout periods, for
;simplicity assumed as 1.
readout_image_time = 1.
readout_store_time = 1.
readout_serial_time = 1.
trap_species_parallel = 2
trap_species_serial = 2  
trap_species_image_density = [0.1,0.1] ; EOL Densities derived from PL fit of CCD370_Meanfit.txt (email 6 Oct 2020)
trap_species_store_density = [0.1,0.1]
trap_species_serial_density = [0.02,0.02]
release_image_time = [1.0,10000.] ; For reference, the slow Si-E trap has release of ~1000s at -100C
release_store_time = [1.0,10000.]
release_serial_time = [1.0,10000.]
capture_cross_section_image = [0.01, 0.01] ; This values corresponds to a Pc = 0.99 at Fe55, Pc = 0.55 @ 0.6 keV
capture_cross_section_store = [0.01, 0.01]
capture_cross_section_serial = [0.01, 0.01]
;capture_cross_section_image = [0.005, 0.005] ; This values corresponds to a Pc = 0.9 at Fe55, Pc = 0.25 @ 0.6 keV
;capture_cross_section_store = [0.005, 0.005]
;capture_cross_section_serial = [0.005, 0.005]
charge_volume_coeff_image  = 0.83 ; Beta derived from PL fit of CCD370_Meanfit.txt (email 6 Oct 2020)
charge_volume_coeff_store  = 0.83
charge_volume_coeff_serial  = 0.83
flag_split = 1
flag_unbinned = 0
flag_event_rec_ll = 0 ; If set, apply energy LL to pixels in damaged event in output file, using lower limit thresholds for total energy and split pixels enegy
detection_ll_eV = 50 ; Threshold of total PICTI energy of the damaged event
detection_ll_split_eV = 20 ; Split pixel threshold.
flag_retrapping = 1
flag_iteration = 0
threshold = 0.1
short_bernoulli = 0
long_bernoulli = 0
flag_binomial = 0

SAVE, /VARIABLES, FILENAME = 'distort_cdm_properties.sav'

end

;list_energy_grid[*,event_index]*1000./3.65
;tab[event_index].RAWY
;index = where(xray_unbinned_18cols gt 0., n)
;dims = size(xray_unbinned_18cols,/dim)
;xi = index mod dims[0]
;yi = index / dims[0]
;xi
;yi
;xray_unbinned_18cols[xi[0], tab[event_index].RAWY*6-6:tab[event_index].RAWY*6+11]*1000./3.65
;xray_unbinned_18cols_pcti_image[xi[0], tab[event_index].RAWY*6-6:tab[event_index].RAWY*6+11]
;xray_binned_18cols_pcti_image[xi[0],tab[event_index].RAWY-1]
;xray_binned_18cols_pcti_image[xi[0],tab[event_index].RAWY]   
;xray_binned_18cols_pcti_image[xi[0],tab[event_index].RAWY+1]   
;xray_binned_18cols_store_input[*,store_section_lines]
;xray_binned_18cols_store_input[*,store_section_lines+1]
;xray_binned_18cols_store_input[*,store_section_lines+2]
