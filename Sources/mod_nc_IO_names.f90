!***********************************************************************
!>
!> Module for netCDF input/output definition of veriable names (only)
!> @author
!> Arnau Folch
!>
!**********************************************************************
  MODULE nc_IO_names
  use KindType
  !
  !*** Names of dimensions
  !
  character(len=16) :: x_nc_name       = 'x'
  character(len=16) :: y_nc_name       = 'y'
  character(len=16) :: z_nc_name       = 'z'
  character(len=16) :: lon_nc_name     = 'lon'
  character(len=16) :: lat_nc_name     = 'lat'
  character(len=16) :: sig_nc_name     = 'lev'
  character(len=16) :: bin_nc_name     = 'bin'
  character(len=16) :: bin_spe_nc_name = 'bin'  ! specie bin; prefix SPE_TAG added later
  character(len=16) :: xcut_nc_name    = 'xcut'
  character(len=16) :: ycut_nc_name    = 'ycut'
  character(len=16) :: zcut_nc_name    = 'zcut'
  character(len=16) :: zflcut_nc_name  = 'fl'
  character(len=16) :: npm_nc_name     = 'pm_bin'
  character(len=16) :: str_nc_name     = 'strlen'
  character(len=16) :: tim_nc_name     = 'time'
  character(len=16) :: ens_nc_name     = 'ens'
  !
  character(len=32)  :: th_con_nc_name         = 'intensity_measure_con'
  character(len=32)  :: th_col_mass_nc_name    = 'intensity_measure_col_mass'
  character(len=32)  :: th_col_mass_DU_nc_name = 'intensity_measure_col_mass_DU'
  character(len=32)  :: th_grn_load_nc_name    = 'intensity_measure_grn_load'
  character(len=32)  :: val_per_nc_name        = 'exceedance_probability'
  !
  !*** Names of variables in res.nc file
  !
  character(len=16)  :: date_nc_name       = 'date'
  character(len=16)  :: h_nc_name          = 'z_grn'
  character(len=16)  :: zs_nc_name         = 'z_sigma'
  character(len=16)  :: c_total_nc_name    = 'con'
  character(len=16)  :: c_bin_nc_name      = 'con_bin'
  character(len=16)  :: cutx_nc_name       = 'con_yz'
  character(len=16)  :: cuty_nc_name       = 'con_xz'
  character(len=16)  :: cutz_nc_name       = 'con_xy'
  character(len=16)  :: fl_nc_name         = 'fl'
  character(len=16)  :: col_nc_name        = 'col_mass'
  character(len=16)  :: clh_nc_name        = 'cloud_top'
  character(len=16)  :: pmc_nc_name        = 'col_mass_pm'
  character(len=16)  :: grn_nc_name        = 'grn_load'
  character(len=16)  :: grn_bin_nc_name    = 'grn_load_bin'
  character(len=16)  :: wet_nc_name        = 'wet_dep'
  !
  character(len=8), dimension(nspe_max) :: SPE_TAG = &
                                                 (/ 'tephra_ ', &
                                                    'dust_   ', &
                                                    'H2O_    ', &
                                                    'SO2_    ', &
                                                    'Cs134_  ', &
                                                    'Cs137_  ', &
                                                    'I131_   ', &
                                                    'Sr90_   ', &
                                                    'Y90_    '/) 
  !
  !*** Names of variables in dbs.nc file
  !
  character(len=16)  :: lmask_nc_name      = 'lmask'
  character(len=16)  :: luse_nc_name       = 'luse'
  character(len=16)  :: z0_nc_name         = 'zo'
  character(len=16)  :: pblh_nc_name       = 'pblh'
  character(len=16)  :: ust_nc_name        = 'ust'
  character(len=16)  :: smoi_nc_name       = 'smoi'
  character(len=16)  :: prec_nc_name       = 'prec'
  character(len=16)  :: u10_nc_name        = 'u10'
  character(len=16)  :: v10_nc_name        = 'v10'
  character(len=16)  :: t2_nc_name         = 't2'
  character(len=16)  :: mon_nc_name        = 'mon'
  character(len=16)  :: p_nc_name          = 'p'
  character(len=16)  :: t_nc_name          = 't'
  character(len=16)  :: tp_nc_name         = 'tp'
  character(len=16)  :: tv_nc_name         = 'tv'
  character(len=16)  :: u_nc_name          = 'u'
  character(len=16)  :: v_nc_name          = 'v'
  character(len=16)  :: w_nc_name          = 'w'
  character(len=16)  :: qv_nc_name         = 'qv'
  character(len=16)  :: rho_nc_name        = 'rho'
  !
  !*** Names of variables in rst.nc file
  !
  character(len=16)  :: nx_nc_name   = 'nx'
  character(len=16)  :: ny_nc_name   = 'ny'
  character(len=16)  :: nz_nc_name   = 'nz'
  character(len=16)  :: nx2_nc_name  = 'nx_2h'
  character(len=16)  :: ny2_nc_name  = 'ny_2h'
  character(len=16)  :: nz2_nc_name  = 'nz_2h'
  character(len=16)  :: nb_nc_name   = 'nbins'
  !
  !*** Names of variables in ens.nc file
  !
  character(len=32)  :: ens_mean_nc_name    = '_mean'
  character(len=32)  :: ens_logmean_nc_name = '_logmean'
  character(len=32)  :: ens_std_nc_name     = '_std'
  character(len=32)  :: ens_median_nc_name  = '_median'
  character(len=32)  :: ens_prb_nc_name     = '_exceedance_probability'
  character(len=32)  :: ens_per_nc_name     = '_intensity_measure'
  !
  !*** attribute names
  !
  character(len=48 )  :: attr_short
  character(len=48 )  :: attr_units 
  character(len=48 )  :: attr_units_vol 
  character(len=48 )  :: attr_units_col 
  character(len=48 )  :: attr_units_grn
  character(len=48 )  :: attr_axis 
  character(len=128)  :: attr_title        ! arnau remove (from rst file)
  character(len=128)  :: attr_desc  
  !
  character(len=16)  :: attr_conventions_name  = 'conventions'
  character(len=16)  :: attr_title_name        = 'title'
  character(len=16)  :: attr_source_name       = 'source'
  character(len=16)  :: attr_history_name      = 'history'
  character(len=16)  :: attr_references_name   = 'references'
  !
  character(len=16)  :: attr_long_name         = 'long_name'
  character(len=16)  :: attr_short_name        = 'standard_name'
  character(len=16)  :: attr_units_name        = 'units'
  character(len=16)  :: attr_axis_name         = 'axis'
  character(len=16)  :: attr_ztop_name         = 'ztop'
  character(len=16)  :: attr_min_name          = 'minimum'
  character(len=16)  :: attr_max_name          = 'maximum'
  character(len=16)  :: attr_cell_name         = 'cell_measures'
  character(len=16)  :: attr_projection_name   = 'projection'
  character(len=16)  :: attr_map_h_name        = 'map_h'
  character(len=16)  :: attr_map_v_name        = 'map_v'
  character(len=16)  :: attr_calendar_name     = 'calendar'
  character(len=16)  :: attr_year_name         = 'start_year'
  character(len=16)  :: attr_month_name        = 'start_month'
  character(len=16)  :: attr_day_name          = 'start_day'
  character(len=16)  :: attr_dbs_start_name    = 'start_second'
  character(len=16)  :: attr_dbs_end_name      = 'end_second'
  character(len=16)  :: attr_run_start_name    = 'RUN_START_TIME'
  character(len=16)  :: attr_run_end_name      = 'RUN_END_TIME'
  character(len=16)  :: attr_run_time_name     = 'CURRENT_TIME'
  character(len=16)  :: attr_mass_ground_name  = 'MASS_GROUND'
  character(len=16)  :: attr_mass_lateral_name = 'MASS_LATERAL'
  character(len=16)  :: attr_mass_sink_name    = 'MASS_SINK'
  character(len=16)  :: attr_mass_in_name      = 'MASS_INJECTED'
  !
  character(len=16)  :: attr_lonmin_name     = 'LONMIN' ! arnau deprecate (from rst file)
  character(len=16)  :: attr_lonmax_name     = 'LONMAX'
  character(len=16)  :: attr_latmin_name     = 'LATMIN'
  character(len=16)  :: attr_latmax_name     = 'LATMAX'
  !
  END MODULE nc_IO_names
