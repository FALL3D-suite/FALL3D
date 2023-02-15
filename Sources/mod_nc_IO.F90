!***********************************************************************
!>
!> Module for netCDF input/output operations
!> @author
!> Arnau Folch
!>
!**********************************************************************
MODULE nc_IO
  use KindType
  use netcdf
  use Domain
  use Grid
  use Parallel
  use Time
  implicit none
  save
  !
  !    LIST OF PRIVATE VARIABLES
  !
  logical, PRIVATE :: PARALLEL_IO                     !< if .true. all tasks write netCDF; if .false. only master writes netCDF
  logical, PRIVATE :: out_con_total                   !< if .true. outputs total concentration on sigma planes (sum over all bins of a given substance)
  logical, PRIVATE :: out_con_bins                    !< if .true. outputs bin   concentration on sigma planes (         all bins of a given substance)
  logical, PRIVATE :: out_col_load                    !< if .true. outputs column  mass load                   (sum over all bins of a given substance)
  logical, PRIVATE :: out_cloud_top                   !< if .true. outputs cloud top height
  logical, PRIVATE :: out_grn_total                   !< if .true. outputs total deposit mass load             (sum over all bins of a given substance)
  logical, PRIVATE :: out_grn_bins                    !< if .true. outputs bin   deposit mass load             (         all bins of a given substance)
  logical, PRIVATE :: out_wet_total                   !< if .true. outputs total wet deposition                (sum over all bins of a given substance)
  !
  integer(ip), PRIVATE :: NF90_MYTYPE = NF90_FLOAT    ! NF90_FLOAT / NF90_DOUBLE. Note that NF90_FLOAT halves output file size but leads to an
  !                                                     inconsistency in variable values (5th decimal digit) between tasks
  character(len=16), PRIVATE :: x_nc_name       = 'x'
  character(len=16), PRIVATE :: y_nc_name       = 'y'
  character(len=16), PRIVATE :: z_nc_name       = 'z'
  character(len=16), PRIVATE :: lon_nc_name     = 'lon'
  character(len=16), PRIVATE :: lat_nc_name     = 'lat'
  character(len=16), PRIVATE :: sig_nc_name     = 'sigma'
  character(len=16), PRIVATE :: bin_nc_name     = 'bin'
  character(len=16), PRIVATE :: bin_spe_nc_name = 'bin'  ! specie bin; prefix SPE_TAG added later
  character(len=16), PRIVATE :: xcut_nc_name    = 'xcut'
  character(len=16), PRIVATE :: ycut_nc_name    = 'ycut'
  character(len=16), PRIVATE :: zcut_nc_name    = 'zcut'
  character(len=16), PRIVATE :: zflcut_nc_name  = 'fl'
  character(len=16), PRIVATE :: npm_nc_name     = 'pm_bin'
  character(len=16), PRIVATE :: str_nc_name     = 'strlen'
  character(len=16), PRIVATE :: tim_nc_name     = 'time'
  !
  character(len=16), PRIVATE :: date_nc_name       = 'date'
  character(len=16), PRIVATE :: h_nc_name          = 'z_grn'
  character(len=16), PRIVATE :: zs_nc_name         = 'z_sigma'
  character(len=16), PRIVATE :: c_total_nc_name    = 'con'
  character(len=16), PRIVATE :: c_bin_nc_name      = 'con_bin'
  character(len=16), PRIVATE :: cutx_nc_name       = 'con_yz'
  character(len=16), PRIVATE :: cuty_nc_name       = 'con_xz'
  character(len=16), PRIVATE :: cutz_nc_name       = 'con_xy'
  character(len=16), PRIVATE :: fl_nc_name         = 'fl'
  character(len=16), PRIVATE :: col_nc_name        = 'col_mass'
  character(len=16), PRIVATE :: clh_nc_name        = 'cloud_top'
  character(len=16), PRIVATE :: pmc_nc_name        = 'col_mass_pm'
  character(len=16), PRIVATE :: grn_nc_name        = 'grn_load'
  character(len=16), PRIVATE :: grn_bin_nc_name    = 'grn_load_bin'
  character(len=16), PRIVATE :: wet_nc_name        = 'wet_dep'
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
  character(len=16), PRIVATE :: lmask_nc_name      = 'lmask'
  character(len=16), PRIVATE :: luse_nc_name       = 'luse'
  character(len=16), PRIVATE :: z0_nc_name         = 'z0'
  character(len=16), PRIVATE :: pblh_nc_name       = 'pblh'
  character(len=16), PRIVATE :: ust_nc_name        = 'ust'
  character(len=16), PRIVATE :: smoi_nc_name       = 'smoi'
  character(len=16), PRIVATE :: prec_nc_name       = 'prec'
  character(len=16), PRIVATE :: u10_nc_name        = 'u10'
  character(len=16), PRIVATE :: v10_nc_name        = 'v10'
  character(len=16), PRIVATE :: t2_nc_name         = 't2'
  character(len=16), PRIVATE :: mon_nc_name        = 'mon'
  character(len=16), PRIVATE :: p_nc_name          = 'p'
  character(len=16), PRIVATE :: t_nc_name          = 't'
  character(len=16), PRIVATE :: tp_nc_name         = 'tp'
  character(len=16), PRIVATE :: tv_nc_name         = 'tv'
  character(len=16), PRIVATE :: u_nc_name          = 'u'
  character(len=16), PRIVATE :: v_nc_name          = 'v'
  character(len=16), PRIVATE :: w_nc_name          = 'w'
  character(len=16), PRIVATE :: qv_nc_name         = 'qv'
  character(len=16), PRIVATE :: rho_nc_name        = 'rho'
  !
  character(len=16), PRIVATE :: nx_nc_name   = 'nx'
  character(len=16), PRIVATE :: ny_nc_name   = 'ny'
  character(len=16), PRIVATE :: nz_nc_name   = 'nz'
  character(len=16), PRIVATE :: nx2_nc_name  = 'nx_2h'
  character(len=16), PRIVATE :: ny2_nc_name  = 'ny_2h'
  character(len=16), PRIVATE :: nz2_nc_name  = 'nz_2h'
  character(len=16), PRIVATE :: nb_nc_name   = 'nbins'

  integer(ip), PRIVATE  :: ncID
  !                                      dimensions
  integer(ip), PRIVATE  :: nx_ncID
  integer(ip), PRIVATE  :: ny_ncID
  integer(ip), PRIVATE  :: nz_ncID
  integer(ip), PRIVATE  :: nx2_ncID
  integer(ip), PRIVATE  :: ny2_ncID
  integer(ip), PRIVATE  :: nz2_ncID
  integer(ip), PRIVATE  :: ns_ncID
  integer(ip), PRIVATE  :: nb_ncID
  integer(ip), PRIVATE  :: nb_spe_ncID(nspe_max)
  integer(ip), PRIVATE  :: ncutx_ncID
  integer(ip), PRIVATE  :: ncuty_ncID
  integer(ip), PRIVATE  :: ncutz_ncID
  integer(ip), PRIVATE  :: nfl_ncID
  integer(ip), PRIVATE  :: npm_ncID
  integer(ip), PRIVATE  :: nstr_ncID
  integer(ip), PRIVATE  :: nt_ncID
  !                                     coordinate variables
  integer(ip), PRIVATE  :: lon_ncID
  integer(ip), PRIVATE  :: lat_ncID
  integer(ip), PRIVATE  :: z_ncID
  integer(ip), PRIVATE  :: sig_ncID
  integer(ip), PRIVATE  :: bin_ncID
  integer(ip), PRIVATE  :: bin_spe_ncID(nspe_max)
  integer(ip), PRIVATE  :: xcut_ncID
  integer(ip), PRIVATE  :: ycut_ncID
  integer(ip), PRIVATE  :: zcut_ncID
  integer(ip), PRIVATE  :: zflcut_ncID
  integer(ip), PRIVATE  :: pm_ncID
  integer(ip), PRIVATE  :: tim_ncID
  !                                     other variables
  integer(ip), PRIVATE  :: date_ncID
  integer(ip), PRIVATE  :: h_ncID
  integer(ip), PRIVATE  :: zs_ncID
  integer(ip), PRIVATE  :: con_ncID
  integer(ip), PRIVATE  :: con_bin_ncID
  integer(ip), PRIVATE  :: col_ncID
  integer(ip), PRIVATE  :: clh_ncID
  integer(ip), PRIVATE  :: fl_ncID
  integer(ip), PRIVATE  :: cutx_ncID
  integer(ip), PRIVATE  :: cuty_ncID
  integer(ip), PRIVATE  :: cutz_ncID
  integer(ip), PRIVATE  :: grn_ncID
  integer(ip), PRIVATE  :: grn_bin_ncID
  integer(ip), PRIVATE  :: wet_ncID
  integer(ip), PRIVATE  :: pmc_ncID
  !                                     other variables (dbs)
  integer(ip), PRIVATE  :: lmask_ncID
  integer(ip), PRIVATE  :: luse_ncID
  integer(ip), PRIVATE  :: z0_ncID
  integer(ip), PRIVATE  :: pblh_ncID
  integer(ip), PRIVATE  :: ust_ncID
  integer(ip), PRIVATE  :: smoi_ncID
  integer(ip), PRIVATE  :: prec_ncID
  integer(ip), PRIVATE  :: u10_ncID
  integer(ip), PRIVATE  :: v10_ncID
  integer(ip), PRIVATE  :: t2_ncID
  integer(ip), PRIVATE  :: mon_ncID
  integer(ip), PRIVATE  :: p_ncID
  integer(ip), PRIVATE  :: t_ncID
  integer(ip), PRIVATE  :: tp_ncID
  integer(ip), PRIVATE  :: tv_ncID
  integer(ip), PRIVATE  :: u_ncID
  integer(ip), PRIVATE  :: v_ncID
  integer(ip), PRIVATE  :: w_ncID
  integer(ip), PRIVATE  :: qv_ncID
  integer(ip), PRIVATE  :: rho_ncID
  !                                           attributes
  character(len=48 ), PRIVATE :: attr_units
  character(len=128), PRIVATE :: attr_title
  character(len=128), PRIVATE :: attr_desc
  !
  character(len=16), PRIVATE :: attr_year_name         = 'YEAR'
  character(len=16), PRIVATE :: attr_month_name        = 'MONTH'
  character(len=16), PRIVATE :: attr_day_name          = 'DAY'
  character(len=16), PRIVATE :: attr_dbs_start_name    = 'START_TIME'
  character(len=16), PRIVATE :: attr_dbs_end_name      = 'END_TIME'
  character(len=16), PRIVATE :: attr_run_start_name    = 'RUN_START_TIME'
  character(len=16), PRIVATE :: attr_run_end_name      = 'RUN_END_TIME'
  character(len=16), PRIVATE :: attr_run_time_name     = 'CURRENT_TIME'
  character(len=16), PRIVATE :: attr_mass_ground_name  = 'MASS_GROUND'
  character(len=16), PRIVATE :: attr_mass_lateral_name = 'MASS_LATERAL'
  character(len=16), PRIVATE :: attr_mass_sink_name    = 'MASS_SINK'
  character(len=16), PRIVATE :: attr_mass_in_name      = 'MASS_INJECTED'
  !
  character(len=16), PRIVATE :: attr_lonmin_name     = 'LONMIN'
  character(len=16), PRIVATE :: attr_lonmax_name     = 'LONMAX'
  character(len=16), PRIVATE :: attr_latmin_name     = 'LATMIN'
  character(len=16), PRIVATE :: attr_latmax_name     = 'LATMAX'
  character(len=16), PRIVATE :: attr_dlon_name       = 'DLON'
  character(len=16), PRIVATE :: attr_dlat_name       = 'DLAT'
  character(len=16), PRIVATE :: attr_maph_name       = 'MAP_H'
  character(len=16), PRIVATE :: attr_mapv_name       = 'MAP_V'
  character(len=16), PRIVATE :: attr_ztop_name       = 'ZTOP'
  !
  integer(ip),PRIVATE, parameter :: npm = 4                                                ! number of PM
  real(rp),   PRIVATE            :: pm_value(npm) = (/2.5_rp,10.0_rp,20.0_rp,64.0_rp/)     ! PM values (in microns)
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: nc_IO_read_inp_output
  PUBLIC :: nc_IO_bcast_inp_output
  PUBLIC :: nc_IO_check_dbs
  PUBLIC :: nc_IO_read_dbs
  PUBLIC :: nc_IO_out_dbs
  PUBLIC :: nc_IO_out_grid
  PUBLIC :: nc_IO_out_res
  PUBLIC :: nc_IO_out_pts
  PUBLIC :: nc_IO_out_rst
  PUBLIC :: nc_IO_read_rst
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine nc_IO_read_inp_output
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads the MODEL_OUTPUT block form the input file
  !
  subroutine nc_IO_read_inp_output(MY_FILES,MY_OUT,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(MODEL_OUTPUT),intent(INOUT) :: MY_OUT
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: ifl
    real(rp)              :: file_version
    character(len=s_file) :: file_inp, word
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_read_inp_output'
    MY_ERR%message = ' '
    !
    file_inp = MY_FILES%file_inp
    !
    !*** Input file version
    !
    call inpout_get_rea (file_inp, 'CODE','VERSION', file_version, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       return
    elseif(file_version < MIN_REQUIRED_VERSION) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version'
       return
    end if
    !
    !*** Reads MODEL_OUTPUT block
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','PARALLEL_IO',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
#ifndef WITH_MPI
       MY_ERR%flag    = 1
       MY_ERR%message = 'Cannot use PARALLEL_IO without MPI'
       return
#endif
       MY_OUT%parallel_IO = .true.
    else
       MY_OUT%parallel_IO = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','LOG_FILE_LEVEL',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'NONE') then
       MY_OUT%log_level = LOG_LEVEL_NONE
    else if(TRIM(word).eq.'NORMAL') then
       MY_OUT%log_level = LOG_LEVEL_NORMAL
    else if(TRIM(word).eq.'FULL') then
       MY_OUT%log_level = LOG_LEVEL_FULL
    else
       MY_OUT%log_level = LOG_LEVEL_NORMAL
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','RESTART_TIME_INTERVAL_(HOURS)',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'NONE') then
       MY_OUT%out_rst = .false.
       MY_OUT%rst     = 0.0_rp
    else if(TRIM(word).eq.'END_ONLY') then
       MY_OUT%out_rst = .true.
       MY_OUT%rst     = 0.0_rp
    else
       MY_OUT%out_rst = .true.
       call inpout_get_rea (file_inp,'MODEL_OUTPUT','RESTART_TIME_INTERVAL_(HOURS)',MY_OUT%rst,1,MY_ERR)
       if(MY_ERR%flag.ne.0) return
       MY_OUT%rst = MY_OUT%rst*3600.0_rp  !  h --> s
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_INTERMEDIATE_FILES',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'NO'.and.TASK_FLAG(TASK_RUN_ALL).eq.1)then
       MY_OUT%out_dbs_file = .false.
    else
       MY_OUT%out_dbs_file = .true.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_TIME_START_(HOURS)',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'RUN_START') then
       MY_OUT%out_start = 0.0_rp
    else
       call inpout_get_rea (file_inp,'MODEL_OUTPUT','OUTPUT_TIME_START_(HOURS)',MY_OUT%out_start,1,MY_ERR)
       if(MY_ERR%flag.ne.0) MY_OUT%out_start = 0.0_rp
       MY_OUT%out_start = MY_OUT%out_start*3600.0_rp  !  h --> s
    end if
    !
    call inpout_get_rea (file_inp,'MODEL_OUTPUT','OUTPUT_TIME_INTERVAL_(HOURS)',MY_OUT%dt,1,MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_OUT%dt = MY_OUT%dt*3600.0_rp  !  h --> s
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_3D_CONCENTRATION',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%out_con_total = .true.
    else
       MY_OUT%out_con_total = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_3D_CONCENTRATION_BINS',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%out_con_bins  = .true.
    else
       MY_OUT%out_con_bins  = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_COLUMN_LOAD',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%out_col_load  = .true.
    else
       MY_OUT%out_col_load  = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_CLOUD_TOP',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%out_cloud_top = .true.
    else
       MY_OUT%out_cloud_top = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_GROUND_LOAD',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%out_grn_total  = .true.
    else
       MY_OUT%out_grn_total  = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_GROUND_LOAD_BINS',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%out_grn_bins  = .true.
    else
       MY_OUT%out_grn_bins  = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_WET_DEPOSITION',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%out_wet_total  = .true.
    else
       MY_OUT%out_wet_total  = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','TRACK_POINTS',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_OUT%track_points  = .true.
       call inpout_get_cha (file_inp, 'MODEL_OUTPUT','TRACK_POINTS_FILE',word, 1, MY_ERR, .false.)
       !
       MY_FILES%file_pts = TRIM(MY_FILES%problempath)//'/'//TRIM(word)
       call inpout_get_file_pts(MY_FILES%file_pts,MY_OUT%MY_PTS,MY_ERR)
       if(MY_ERR%flag.ne.0) return
    else
       MY_OUT%track_points  = .false.
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_CONCENTRATION_AT_XCUTS',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       !
       call inpout_get_npar(file_inp, 'MODEL_OUTPUT','X-VALUES', MY_OUT%MY_CUTS%ncutx, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       allocate(MY_OUT%MY_CUTS%x_cut(MY_OUT%MY_CUTS%ncutx))
       call inpout_get_rea (file_inp,'MODEL_OUTPUT','X-VALUES',MY_OUT%MY_CUTS%x_cut,MY_OUT%MY_CUTS%ncutx,MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
    else
       MY_OUT%MY_CUTS%ncutx = 0
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_CONCENTRATION_AT_YCUTS',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       !
       call inpout_get_npar(file_inp, 'MODEL_OUTPUT','Y-VALUES', MY_OUT%MY_CUTS%ncuty, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       allocate(MY_OUT%MY_CUTS%y_cut(MY_OUT%MY_CUTS%ncuty))
       call inpout_get_rea (file_inp,'MODEL_OUTPUT','Y-VALUES',MY_OUT%MY_CUTS%y_cut,MY_OUT%MY_CUTS%ncuty,MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
    else
       MY_OUT%MY_CUTS%ncuty = 0
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_CONCENTRATION_AT_ZCUTS',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       !
       call inpout_get_npar(file_inp, 'MODEL_OUTPUT','Z-VALUES', MY_OUT%MY_CUTS%ncutz, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       allocate(MY_OUT%MY_CUTS%z_cut(MY_OUT%MY_CUTS%ncutz))
       call inpout_get_rea (file_inp,'MODEL_OUTPUT','Z-VALUES',MY_OUT%MY_CUTS%z_cut,MY_OUT%MY_CUTS%ncutz,MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
    else
       MY_OUT%MY_CUTS%ncutz = 0
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_OUTPUT','OUTPUT_CONCENTRATION_AT_FL',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       !
       call inpout_get_npar(file_inp, 'MODEL_OUTPUT','FL-VALUES', MY_OUT%MY_CUTS%nfl, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       allocate(MY_OUT%MY_CUTS%fl_cut (MY_OUT%MY_CUTS%nfl))
       allocate(MY_OUT%MY_CUTS%zfl_cut(MY_OUT%MY_CUTS%nfl))
       !
       call inpout_get_rea (file_inp,'MODEL_OUTPUT','FL-VALUES',MY_OUT%MY_CUTS%fl_cut,MY_OUT%MY_CUTS%nfl,MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       do ifl = 1,MY_OUT%MY_CUTS%nfl
          MY_OUT%MY_CUTS%zfl_cut(ifl) = MY_OUT%MY_CUTS%fl_cut(ifl)*30.48_rp  ! FL in m
       end do
       !
    else
       MY_OUT%MY_CUTS%nfl = 0
    end if
    !
    return
  end subroutine nc_IO_read_inp_output
  !
  !-----------------------------------------
  !    subroutine nc_IO_bcast_inp_output
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts MODEL_OUTPUT block from input file
  !
  subroutine nc_IO_bcast_inp_output(MY_OUT,MY_ERR)
    implicit none
    !
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_ERR    error handler
    !
    type(MODEL_OUTPUT),intent(INOUT) :: MY_OUT
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    integer(ip) :: ncutx,ncuty,ncutz,nfl,ipts
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_bcast_inp_output'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_OUT%parallel_IO    ,1,0)
    call parallel_bcast(MY_OUT%log_level      ,1,0)
    call parallel_bcast(MY_OUT%out_rst        ,1,0)
    call parallel_bcast(MY_OUT%out_con_total  ,1,0)
    call parallel_bcast(MY_OUT%out_dbs_file   ,1,0)
    call parallel_bcast(MY_OUT%out_con_bins   ,1,0)
    call parallel_bcast(MY_OUT%out_col_load   ,1,0)
    call parallel_bcast(MY_OUT%out_cloud_top  ,1,0)
    call parallel_bcast(MY_OUT%out_grn_total  ,1,0)
    call parallel_bcast(MY_OUT%out_grn_bins   ,1,0)
    call parallel_bcast(MY_OUT%out_wet_total  ,1,0)
    call parallel_bcast(MY_OUT%out_start      ,1,0)
    call parallel_bcast(MY_OUT%dt             ,1,0)
    call parallel_bcast(MY_OUT%rst            ,1,0)
    !
    call parallel_bcast(MY_OUT%MY_CUTS%ncutx  ,1,0)
    call parallel_bcast(MY_OUT%MY_CUTS%ncuty  ,1,0)
    call parallel_bcast(MY_OUT%MY_CUTS%ncutz  ,1,0)
    call parallel_bcast(MY_OUT%MY_CUTS%nfl    ,1,0)
    !
    ncutx = MY_OUT%MY_CUTS%ncutx
    ncuty = MY_OUT%MY_CUTS%ncuty
    ncutz = MY_OUT%MY_CUTS%ncutz
    nfl   = MY_OUT%MY_CUTS%nfl
    !
    if(.not.master) then
       if(ncutx.gt.0) allocate(MY_OUT%MY_CUTS%x_cut  (ncutx))
       if(ncuty.gt.0) allocate(MY_OUT%MY_CUTS%y_cut  (ncuty))
       if(ncutz.gt.0) allocate(MY_OUT%MY_CUTS%z_cut  (ncutz))
       if(nfl  .gt.0) allocate(MY_OUT%MY_CUTS%zfl_cut(nfl  ))
       if(nfl  .gt.0) allocate(MY_OUT%MY_CUTS%fl_cut (nfl  ))
    end if
    !
    if(ncutx.gt.0) call parallel_bcast(MY_OUT%MY_CUTS%x_cut  ,ncutx,0)
    if(ncuty.gt.0) call parallel_bcast(MY_OUT%MY_CUTS%y_cut  ,ncuty,0)
    if(ncutz.gt.0) call parallel_bcast(MY_OUT%MY_CUTS%z_cut  ,ncutz,0)
    if(nfl  .gt.0) call parallel_bcast(MY_OUT%MY_CUTS%zfl_cut,nfl  ,0)
    if(nfl  .gt.0) call parallel_bcast(MY_OUT%MY_CUTS%fl_cut ,nfl  ,0)
    !
    call parallel_bcast(MY_OUT%track_points ,1,0)
    call parallel_bcast(MY_OUT%MY_PTS%npts  ,1,0)
    !
    if(MY_OUT%MY_PTS%npts.gt.0) then
       if(.not.master) then
          allocate(MY_OUT%MY_PTS%name_pts(MY_OUT%MY_PTS%npts))
          allocate(MY_OUT%MY_PTS%xpts    (MY_OUT%MY_PTS%npts))
          allocate(MY_OUT%MY_PTS%ypts    (MY_OUT%MY_PTS%npts))
          allocate(MY_OUT%MY_PTS%zpts    (MY_OUT%MY_PTS%npts))
       end if
       do ipts = 1,MY_OUT%MY_PTS%npts
          call parallel_bcast(MY_OUT%MY_PTS%name_pts(ipts),MY_OUT%MY_PTS%npts,0)
       end do
       call parallel_bcast(MY_OUT%MY_PTS%xpts    ,MY_OUT%MY_PTS%npts,0)
       call parallel_bcast(MY_OUT%MY_PTS%ypts    ,MY_OUT%MY_PTS%npts,0)
       call parallel_bcast(MY_OUT%MY_PTS%zpts    ,MY_OUT%MY_PTS%npts,0)
    end if
    !
    return
  end subroutine nc_IO_bcast_inp_output
  !
  !---------------------------------
  !    subroutine nc_IO_check_dbs
  !---------------------------------
  !
  !>   @brief
  !>   Checks consistency between input and dbs files and reads topography
  !
  subroutine nc_IO_check_dbs(MY_FILES,MY_GRID,MY_TIME,MY_OUT,my_hc,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_GRID       ARAKAWA_C_GRID structure already filled
    !>   @param MY_TIME       RUN_TIME       structure already filled
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param my_hc         values of topography at my processor cell corners
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(ARAKAWA_C_GRID),  intent(INOUT) :: MY_GRID
    type(RUN_TIME),        intent(INOUT) :: MY_TIME
    type(MODEL_OUTPUT),    intent(INOUT) :: MY_OUT
    real(rp),              intent(INOUT) :: my_hc(my_ibs:my_ibe,my_jbs:my_jbe)
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: nc_file,dim_name
    !
    integer(ip) :: mode_flag,ival,istat,k
    integer(ip) :: nbx,nby,nbz
    integer(ip) :: start2d(2)
    integer(ip) :: count2d(2)
    real(rp)    :: rval
    !
    real(rp), allocatable :: work1d(:)
    real(rp), allocatable :: work2d(:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_check_dbs'
    MY_ERR%message = ' '
    !
    nc_file     = MY_FILES%file_dbs
    PARALLEL_IO = MY_OUT%PARALLEL_IO
    !
    nbx  = gl_nbx
    nby  = gl_nby
    nbz  = gl_nbz
    !
    !*** Open file
    !
#if defined WITH_MPI
    if(PARALLEL_IO) then
       mode_flag = IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL)
       mode_flag = IOR(mode_flag, NF90_MPIIO)
       mode_flag = IOR(mode_flag, NF90_NOWRITE)
       istat     = nf90_open_par(TRIM(nc_file), mode_flag , comm=COMM_WORLD, info = MPI_INFO_NULL, ncid=ncID)
    else if(master) then
       mode_flag = NF90_NOWRITE
       istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
    end if
#else
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
#endif

    !
    !*** Master checks dimensions
    !
    if(master) then
       istat = nf90_inq_dimid        (ncID, lon_nc_name, nx_ncID)
       istat = nf90_inquire_dimension(ncID, nx_ncID, dim_name, ival)
       if(ival.ne.nbx) MY_ERR%flag = 1
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for dimension '//TRIM(lon_nc_name)//' between input and dbs files'
       return
    end if
    !
    if(master) then
       istat = nf90_inq_dimid        (ncID, lat_nc_name, ny_ncID)
       istat = nf90_inquire_dimension(ncID, ny_ncID, dim_name, ival)
       if(ival.ne.nby) MY_ERR%flag = 1
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for dimension '//TRIM(lat_nc_name)//' between input and dbs files'
       return
    end if
    !
    if(master) then
       istat = nf90_inq_dimid        (ncID, sig_nc_name, ns_ncID)
       istat = nf90_inquire_dimension(ncID, ns_ncID, dim_name, ival)
       if(ival.ne.nbz) MY_ERR%flag = 1
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for dimension '//TRIM(sig_nc_name)//' between input and dbs files'
       return
    end if
    !
    !*** Checks consistency in MY_TIME variables
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_year_name, ival)
       if(ival.ne.MY_TIME%start_year) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_year_name, ival)
          if(ival.ne.MY_TIME%start_year) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_year_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_month_name, ival)
       if(ival.ne.MY_TIME%start_month) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_month_name, ival)
          if(ival.ne.MY_TIME%start_month) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_month_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_day_name, ival)
       if(ival.ne.MY_TIME%start_day) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_day_name, ival)
          if(ival.ne.MY_TIME%start_day) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_day_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dbs_start_name, rval)
       if(rval.ne.MY_TIME%dbs_start) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dbs_start_name, rval)
          if(rval.ne.MY_TIME%dbs_start) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values between '//TRIM(attr_dbs_start_name)// &
            ' attribute in dbs and DBS_BEGIN_METEO_DATA_(HOURS_AFTER_00) in inut file'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dbs_end_name, rval)
       if(rval.ne.MY_TIME%dbs_end) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dbs_end_name, rval)
          if(rval.ne.MY_TIME%dbs_end) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values between '//TRIM(attr_dbs_end_name)// &
            ' attribute in dbs and DBS_END_METEO_DATA_(HOURS_AFTER_00) in inut file'
       return
    end if
    !
    !*** Checks that the dbs covers the simulation time
    !
    if(MY_TIME%run_start.lt.MY_TIME%dbs_start) then
       MY_ERR%flag = 1
       MY_ERR%message = 'Run start time lower than dbs start time'
       return
    end if
    !
    if(MY_TIME%run_end.gt.MY_TIME%dbs_end) then
       MY_ERR%flag = 1
       MY_ERR%message = 'Run end time larger than dbs end time'
       return
    end if
    !
    !*** Checks consistency in MY_GRID variables
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmin_name, rval)
       if(rval.ne.MY_GRID%lonmin) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmin_name, rval)
          if(rval.ne.MY_GRID%lonmin) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_lonmin_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmax_name, rval)
       if(rval.ne.MY_GRID%lonmax) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmax_name, rval)
          if(rval.ne.MY_GRID%lonmax) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_lonmax_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmin_name, rval)
       if(rval.ne.MY_GRID%latmin) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmin_name, rval)
          if(rval.ne.MY_GRID%latmin) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_latmin_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmax_name, rval)
       if(rval.ne.MY_GRID%latmax) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmax_name, rval)
          if(rval.ne.MY_GRID%latmax) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_latmax_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dlon_name, rval)
       if(rval.ne.MY_GRID%dlon) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dlon_name, rval)
          if(rval.ne.MY_GRID%dlon) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_dlon_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dlat_name, rval)
       if(rval.ne.MY_GRID%dlat) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_dlat_name, rval)
          if(rval.ne.MY_GRID%dlat) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_dlat_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_mapv_name, ival)
       if(ival.ne.MY_GRID%map_v) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_mapv_name, ival)
          if(ival.ne.MY_GRID%map_v) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_mapv_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_maph_name, ival)
       if(ival.ne.MY_GRID%map_h) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_maph_name, ival)
          if(ival.ne.MY_GRID%map_h) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_maph_name)//' between input and dbs files'
       return
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_ztop_name, rval)
       if(rval.ne.MY_GRID%X3max) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_ztop_name, rval)
          if(rval.ne.MY_GRID%X3max) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_ztop_name)//' between input and dbs files'
       return
    end if
    !
    !*** Check sigma values
    !
    if(master) then
       allocate(work1d(nbz))
       istat = nf90_inq_varid(ncID, sig_nc_name,sig_ncID)
       istat = nf90_get_var  (ncID, sig_ncID, work1d, start=(/1/),count=(/nbz/))
       !
       do k = 1,gl_nbz
          if( abs(work1d(k)-MY_GRID%gl_sigma(k)).gt.1d-4 ) MY_ERR%flag = 1
       end do
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values of sigma coordinate between input and dbs files'
       return
    end if
    !
    !*** Read topography at cell corners
    !
    if(PARALLEL_IO) then
       !
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_inq_varid(ncID, h_nc_name,h_ncID)
       istat = nf90_get_var  (ncID, h_ncID, my_hc,start=start2d,count=count2d)
       !
    else
       allocate(work2d(nbx,nby))
       if(master) then
          istat = nf90_inq_varid(ncID, h_nc_name,h_ncID)
          istat = nf90_get_var  (ncID, h_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
       end if
       call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,my_hc)
       deallocate(work2d)
       !
    end if
    !
    return
  end subroutine nc_IO_check_dbs
  !
  !---------------------------------
  !    subroutine nc_IO_read_dbs
  !---------------------------------
  !
  !>   @brief
  !>   Reads a dbs file in netCDF format
  !
  subroutine nc_IO_read_dbs(MY_FILES,MY_MET,MY_OUT,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_MET        variables related to meteorology in MY_GRID
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(METEOROLOGY),     intent(INOUT) :: MY_MET
    type(MODEL_OUTPUT),    intent(IN   ) :: MY_OUT
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: nc_file
    !
    integer(ip)  :: nbx,nby,nbz,nt
    integer(ip)  :: istat,mode_flag,it
    integer(ip)  :: start1d(1),start2d(2),start3d(3),start4d(4)
    integer(ip)  :: count1d(1),count2d(2),count3d(3),count4d(4)
    !
    real(rp),    allocatable :: work2d(:,:)
    real(rp),    allocatable :: work3d(:,:,:)
    real(rp),    allocatable :: work4d(:,:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_read_dbs'
    MY_ERR%message = ' '
    !
    nc_file     = MY_FILES%file_dbs
    PARALLEL_IO = MY_OUT%PARALLEL_IO
    !
    !*** Open file
    !
#if defined WITH_MPI
    if(PARALLEL_IO) then
       mode_flag = IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL)
       mode_flag = IOR(mode_flag, NF90_MPIIO)
       mode_flag = IOR(mode_flag, NF90_NOWRITE)
       istat     = nf90_open_par(TRIM(nc_file), mode_flag , comm=COMM_WORLD, info = MPI_INFO_NULL, ncid=ncID)
    else if(master) then
       mode_flag = NF90_NOWRITE
       istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
    end if
#else
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
#endif
    !
    !*** Dimensions
    !
    if(PARALLEL_IO) then
       istat = nf90_inq_dimid        (ncID, tim_nc_name, nt_ncID)
       istat = nf90_inquire_dimension(ncID, nt_ncID,tim_nc_name,nt)
    else
       if(master) then
          istat = nf90_inq_dimid        (ncID, tim_nc_name, nt_ncID)
          istat = nf90_inquire_dimension(ncID, nt_ncID,tim_nc_name,nt)
       end if
       call parallel_bcast(nt,1,0)
    end if
    !
    nbx          = gl_nbx
    nby          = gl_nby
    nbz          = gl_nbz
    MY_MET%nt    = nt
    MY_MET%npoin = (my_ibe-my_ibs+1)*(my_jbe-my_jbs+1)
    !
    !*** 1D variables
    !
    allocate(MY_MET%timesec(MY_MET%nt))
    !
    ! timesec
    !
    if(PARALLEL_IO.or.master) then
       start1d=(/1/)
       count1d=(/nt/)
       istat = nf90_inq_varid(ncID, tim_nc_name, tim_ncID)
       istat = nf90_get_var(ncID, tim_ncID, MY_MET%timesec,start=start1d,count=count1d)
    end if
    if(.not.PARALLEL_IO) call parallel_bcast(MY_MET%timesec,nt,0)
    !
    !*** 2D variables at corners
    !
    allocate(MY_MET%my_lmaskc(my_ibs:my_ibe,my_jbs:my_jbe))
    allocate(MY_MET%my_lusec (my_ibs:my_ibe,my_jbs:my_jbe))
    allocate(MY_MET%my_z0c   (my_ibs:my_ibe,my_jbs:my_jbe))
    !
    ! land mask
    !
    if(PARALLEL_IO) then
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_inq_varid(ncID, lmask_nc_name, lmask_ncID)
       istat = nf90_get_var  (ncID, lmask_ncID, MY_MET%my_lmaskc,start=start2d,count=count2d)
    else
       allocate(work2d(nbx,nby))
       if(master) then
          istat = nf90_inq_varid(ncID, lmask_nc_name, lmask_ncID)
          istat = nf90_get_var  (ncID, lmask_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
       end if
       call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_lmaskc)
       deallocate(work2d)
    end if
    !
    ! z0
    !
    if(PARALLEL_IO) then
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_inq_varid(ncID, z0_nc_name, z0_ncID)
       istat = nf90_get_var  (ncID, z0_ncID, MY_MET%my_z0c,start=start2d,count=count2d)
    else
       allocate(work2d(nbx,nby))
       if(master) then
          istat = nf90_inq_varid(ncID, z0_nc_name, z0_ncID)
          istat = nf90_get_var  (ncID, z0_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
       end if
       call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_z0c)
       deallocate(work2d)
    end if
    !
    ! land use
    !
    if(PARALLEL_IO) then
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_inq_varid(ncID, luse_nc_name, luse_ncID)
       istat = nf90_get_var  (ncID, luse_ncID, MY_MET%my_lusec,start=start2d,count=count2d)
    else
       allocate(work2d(nbx,nby))
       if(master) then
          istat = nf90_inq_varid(ncID, luse_nc_name, luse_ncID)
          istat = nf90_get_var  (ncID, luse_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
       end if
       call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_lusec)
       deallocate(work2d)
    end if
    !
    !*** 3D variables at corners
    !
    allocate(MY_MET%my_pblhc(my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    allocate(MY_MET%my_ustc (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    allocate(MY_MET%my_smoic(my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    allocate(MY_MET%my_prec (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    allocate(MY_MET%my_u10  (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    allocate(MY_MET%my_v10  (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    allocate(MY_MET%my_t2   (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    allocate(MY_MET%my_monc (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt))
    !
    ! pblh
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, pblh_nc_name, pblh_ncID)
       istat = nf90_get_var  (ncID, pblh_ncID, MY_MET%my_pblhc,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, pblh_nc_name, pblh_ncID)
          istat = nf90_get_var  (ncID, pblh_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_pblhc(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    ! u*
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, ust_nc_name, ust_ncID)
       istat = nf90_get_var  (ncID, ust_ncID, MY_MET%my_ustc,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, ust_nc_name, ust_ncID)
          istat = nf90_get_var  (ncID, ust_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_ustc(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    ! smoi
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, smoi_nc_name, smoi_ncID)
       istat = nf90_get_var  (ncID, smoi_ncID, MY_MET%my_smoic,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, smoi_nc_name, smoi_ncID)
          istat = nf90_get_var  (ncID, smoi_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_smoic(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    ! prec
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, prec_nc_name, prec_ncID)
       istat = nf90_get_var  (ncID, prec_ncID, MY_MET%my_prec,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, prec_nc_name, prec_ncID)
          istat = nf90_get_var  (ncID, prec_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_prec(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    ! u10
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, u10_nc_name, u10_ncID)
       istat = nf90_get_var  (ncID, u10_ncID, MY_MET%my_u10,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, u10_nc_name, u10_ncID)
          istat = nf90_get_var  (ncID, u10_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_u10(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    ! v10
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, v10_nc_name, v10_ncID)
       istat = nf90_get_var  (ncID, v10_ncID, MY_MET%my_v10,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, v10_nc_name, v10_ncID)
          istat = nf90_get_var  (ncID, v10_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_v10(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    ! T2 (surface)
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, t2_nc_name, t2_ncID)
       istat = nf90_get_var  (ncID, t2_ncID, MY_MET%my_t2,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, t2_nc_name, t2_ncID)
          istat = nf90_get_var  (ncID, t2_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_t2(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    ! Monin
    !
    if(PARALLEL_IO) then
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_inq_varid(ncID, mon_nc_name, mon_ncID)
       istat = nf90_get_var  (ncID, mon_ncID, MY_MET%my_monc,start=start3d,count=count3d)
    else
       allocate(work2d(nbx,nby))
       allocate(work3d(nbx,nby,nt))
       if(master) then
          istat = nf90_inq_varid(ncID, mon_nc_name, mon_ncID)
          istat = nf90_get_var  (ncID, mon_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       do it = 1,nt
          if(master) work2d(:,:) = work3d(:,:,it)
          call domain_scatter_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_monc(:,:,it))
       end do
       deallocate(work2d)
       deallocate(work3d)
    end if
    !
    !*** 4D variables at corners
    !
    allocate (MY_MET%my_pc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_tc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_tpc (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_tvc (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_uc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_vc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_wc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_qvc (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    allocate (MY_MET%my_rhoc(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt))
    !
    ! pressure
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, p_nc_name, p_ncID)
       istat = nf90_get_var  (ncID, p_ncID, MY_MET%my_pc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, p_nc_name, p_ncID)
          istat = nf90_get_var  (ncID, p_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_pc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! temperature
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, t_nc_name, t_ncID)
       istat = nf90_get_var  (ncID, t_ncID, MY_MET%my_tc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, t_nc_name, t_ncID)
          istat = nf90_get_var  (ncID, t_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_tc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! potential temperature
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, tp_nc_name, tp_ncID)
       istat = nf90_get_var  (ncID, tp_ncID, MY_MET%my_tpc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, tp_nc_name, tp_ncID)
          istat = nf90_get_var  (ncID, tp_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_tpc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! virtual potential temperature
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, tv_nc_name, tv_ncID)
       istat = nf90_get_var  (ncID, tv_ncID, MY_MET%my_tvc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, tv_nc_name, tv_ncID)
          istat = nf90_get_var  (ncID, tv_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_tvc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! u velocity
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, u_nc_name, u_ncID)
       istat = nf90_get_var  (ncID, u_ncID, MY_MET%my_uc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, u_nc_name, u_ncID)
          istat = nf90_get_var  (ncID, u_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_uc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! v velocity
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, v_nc_name, v_ncID)
       istat = nf90_get_var  (ncID, v_ncID, MY_MET%my_vc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, v_nc_name, v_ncID)
          istat = nf90_get_var  (ncID, v_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_vc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! w velocity
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, w_nc_name, w_ncID)
       istat = nf90_get_var  (ncID, w_ncID, MY_MET%my_wc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, w_nc_name, w_ncID)
          istat = nf90_get_var  (ncID, w_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_wc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! specific humidity
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, qv_nc_name, qv_ncID)
       istat = nf90_get_var  (ncID, qv_ncID, MY_MET%my_qvc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, qv_nc_name, qv_ncID)
          istat = nf90_get_var  (ncID, qv_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_qvc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    ! density
    !
    if(PARALLEL_IO) then
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_inq_varid(ncID, rho_nc_name, rho_ncID)
       istat = nf90_get_var  (ncID, rho_ncID, MY_MET%my_rhoc,start=start4d,count=count4d)
    else
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
          istat = nf90_inq_varid(ncID, rho_nc_name, rho_ncID)
          istat = nf90_get_var  (ncID, rho_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          if(master) work3d(:,:,:) = work4d(:,:,:,it)
          call domain_scatter_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_rhoc(:,:,:,it))
       end do
       deallocate(work3d)
       deallocate(work4d)
    end if
    !
    !*** Close the file
    !
    if(PARALLEL_IO.or.master) then
       istat = nf90_close(ncID)
    end if
    !
    return
  end subroutine nc_IO_read_dbs
  !
  !---------------------------------
  !    subroutine nc_IO_out_dbs
  !---------------------------------
  !
  !>   @brief
  !>   Outputs a dbs file in netCDF format
  !
  subroutine nc_IO_out_dbs(MY_FILES,MY_GRID,MY_MET,MY_TIME,MY_OUT,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_GRID       ARAKAWA_C_GRID structure already filled
    !>   @param MY_MET        variables related to meteorology in MY_GRID
    !>   @param MY_TIME       RUN_TIME       structure already filled
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(ARAKAWA_C_GRID),  intent(IN   ) :: MY_GRID
    type(METEOROLOGY),     intent(IN   ) :: MY_MET
    type(RUN_TIME),        intent(IN   ) :: MY_TIME
    type(MODEL_OUTPUT),    intent(IN   ) :: MY_OUT
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: nc_file
    !
    integer(ip)  :: nbx,nby,nbz,nt
    integer(ip)  :: istat,i,j,k,it
    integer(ip)  :: mode_flag
    integer(ip)  :: start1d(1),start2d(2),start3d(3),start4d(4)
    integer(ip)  :: count1d(1),count2d(2),count3d(3),count4d(4)
    !
    real(rp),    allocatable :: work1d(:)
    real(rp),    allocatable :: work2d(:,:)
    real(rp),    allocatable :: work3d(:,:,:)
    real(rp),    allocatable :: work4d(:,:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_out_dbs'
    MY_ERR%message = ' '
    !
    nc_file     = MY_FILES%file_dbs
    PARALLEL_IO = MY_OUT%PARALLEL_IO
    !
    nbx  = gl_nbx
    nby  = gl_nby
    nbz  = gl_nbz
    nt   = MY_MET%nt
    !
    !*** Create file
    !
#if defined WITH_MPI
    if(PARALLEL_IO) then
       mode_flag = IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL)
       mode_flag = IOR(mode_flag, NF90_MPIIO)
       mode_flag = IOR(mode_flag, NF90_CLOBBER)
       istat     = nf90_create_par(TRIM(nc_file), cmode=mode_flag , comm=COMM_WORLD, info = MPI_INFO_NULL, ncid=ncID)
    else if(master) then
       mode_flag = IOR(NF90_CLOBBER,NF90_NETCDF4)
       istat     = nf90_create(TRIM(nc_file),cmode=mode_flag, ncid=ncID)
    end if
#else
    mode_flag = IOR(NF90_CLOBBER,NF90_NETCDF4)
    istat     = nf90_create(TRIM(nc_file),cmode=mode_flag, ncid=ncID)
#endif
    !
    !*** Check errors
    !
    if(PARALLEL_IO.and.istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Unable to create '//TRIM(nc_file)
       return
    else
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%message = 'Unable to create '//TRIM(nc_file)
          return
       end if
    end if
    !
    !*** File dimensions
    !
    if(PARALLEL_IO.or.master) then
       !
       !  Define dimensions
       !
       select case(MY_GRID%map_h)
       case(MAP_H_CARTESIAN)
          istat = nf90_def_dim(ncID, x_nc_name, nbx , nx_ncID )
          istat = nf90_def_dim(ncID, y_nc_name, nby , ny_ncID )
          !
       case(MAP_H_SPHERICAL)
          istat = nf90_def_dim(ncID, lon_nc_name, nbx , nx_ncID )
          istat = nf90_def_dim(ncID, lat_nc_name, nby , ny_ncID )
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          !
       end select
       !
       istat = nf90_def_dim(ncID, sig_nc_name,nbz   , ns_ncID  )
       istat = nf90_def_dim(ncID, str_nc_name,s_name, nstr_ncID)
       istat = nf90_def_dim(ncID, tim_nc_name,nt    , nt_ncID  )
       !
       ! Define coordinate variables
       !
       select case(MY_GRID%map_h)
       case(MAP_H_CARTESIAN)
          attr_desc  = 'x-coordinate. East positive'
          attr_units = 'm'
          istat = nf90_def_var(ncID, x_nc_name ,NF90_MYTYPE, (/nx_ncID/), lon_ncID)
          istat = nf90_put_att(ncID, lon_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lon_ncID, 'units',       attr_units)
          !
          attr_desc  = 'y-coordinate. North positive'
          attr_units = 'm'
          istat = nf90_def_var(ncID, y_nc_name ,NF90_MYTYPE, (/ny_ncID/), lat_ncID)
          istat = nf90_put_att(ncID, lat_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lat_ncID, 'units',       attr_units)
          !
       case(MAP_H_SPHERICAL)
          attr_desc  = 'longitude'
          attr_units = 'degrees_east'
          istat = nf90_def_var(ncID, lon_nc_name ,NF90_MYTYPE, (/nx_ncID/), lon_ncID)
          istat = nf90_put_att(ncID, lon_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lon_ncID, 'units',       attr_units)
          !
          attr_desc  = 'latitude'
          attr_units = 'degrees_north'
          istat = nf90_def_var(ncID, lat_nc_name ,NF90_MYTYPE, (/ny_ncID/), lat_ncID)
          istat = nf90_put_att(ncID, lat_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lat_ncID, 'units',       attr_units)
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          !
       end select
       !
       attr_desc  = 'sigma level'
       attr_units = '-'
       istat = nf90_def_var(ncID, sig_nc_name ,NF90_MYTYPE, (/ns_ncID/), sig_ncID)
       istat = nf90_put_att(ncID, sig_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, sig_ncID, 'units',       attr_units)
       !
       attr_desc  = 'time'
       write(attr_units,"(A,1X,I4.4,'-',I2.2,'-',I2.2,1X,A)") "seconds since",     &
            MY_TIME%start_year,  &
            MY_TIME%start_month, &
            MY_TIME%start_day,   &
            '0:0:0'
       !        attr_units = 's'
       istat = nf90_def_var(ncID, tim_nc_name ,NF90_MYTYPE, (/nt_ncID/), tim_ncID)
       istat = nf90_put_att(ncID, tim_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, tim_ncID, 'units',       attr_units)
       istat = nf90_put_att(ncID, tim_ncID, 'calendar','proleptic_gregorian')
       !
       ! Define rest of variables
       !
       attr_desc  = 'terrain elevation (a.s.l.)'
       attr_units = 'm'
       istat = nf90_def_var(ncID, h_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID/), h_ncID)
       istat = nf90_put_att(ncID, h_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, h_ncID, 'units',       attr_units)
       !
       attr_desc  = 'land mask (1 for land, 0 for water)'
       attr_units = '-'
       istat = nf90_def_var(ncID, lmask_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID/), lmask_ncID)
       istat = nf90_put_att(ncID, lmask_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, lmask_ncID, 'units',       attr_units)
       !
       attr_desc  = 'USGS 24 land use category'
       attr_units = '-'
       istat = nf90_def_var(ncID, luse_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID/), luse_ncID)
       istat = nf90_put_att(ncID, luse_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, luse_ncID, 'units',       attr_units)
       !
       attr_desc  = 'Surface roughness length'
       attr_units = 'm'
       istat = nf90_def_var(ncID, z0_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID/), z0_ncID)
       istat = nf90_put_att(ncID, z0_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, z0_ncID, 'units',       attr_units)
       !
       attr_desc  = 'z coordinate of sigma levels (a.s.l.)'
       attr_units = 'm'
       istat = nf90_def_var(ncID, zs_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,ns_ncID/), zs_ncID)
       istat = nf90_put_att(ncID, zs_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, zs_ncID, 'units',       attr_units)
       !
       attr_desc  = 'planetary boundary layer height'
       attr_units = 'm'
       istat = nf90_def_var(ncID, pblh_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), pblh_ncID)
       istat = nf90_put_att(ncID, pblh_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, pblh_ncID, 'units',       attr_units)
       !
       attr_desc  = 'friction velocity u*'
       attr_units = 'm/s'
       istat = nf90_def_var(ncID, ust_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), ust_ncID)
       istat = nf90_put_att(ncID, ust_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, ust_ncID, 'units',       attr_units)
       !
       attr_desc  = 'soil moisture (first soil layer)'
       attr_units = 'm3/m3'
       istat = nf90_def_var(ncID, smoi_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), smoi_ncID)
       istat = nf90_put_att(ncID, smoi_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, smoi_ncID, 'units',       attr_units)
       !
       attr_desc  = 'precipitation rate'
       attr_units = 'mm/h'
       istat = nf90_def_var(ncID, prec_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), prec_ncID)
       istat = nf90_put_att(ncID, prec_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, prec_ncID, 'units',       attr_units)
       !
       attr_desc  = 'u-component (zonal) of wind at 10m'
       attr_units = 'm/s'
       istat = nf90_def_var(ncID, u10_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), u10_ncID)
       istat = nf90_put_att(ncID, u10_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, u10_ncID, 'units',       attr_units)
       !
       attr_desc  = 'v-component (meridional) of wind at 10m'
       attr_units = 'm/s'
       istat = nf90_def_var(ncID, v10_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), v10_ncID)
       istat = nf90_put_att(ncID, v10_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, v10_ncID, 'units',       attr_units)
       !
       attr_desc  = 'temperature at 2m (surface)'
       attr_units = 'K'
       istat = nf90_def_var(ncID, t2_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), t2_ncID)
       istat = nf90_put_att(ncID, t2_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, t2_ncID, 'units',       attr_units)
       !
       attr_desc  = 'Monin-Obukhov lenght'
       attr_units = 'm'
       istat = nf90_def_var(ncID, mon_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), mon_ncID)
       istat = nf90_put_att(ncID, mon_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, mon_ncID, 'units',       attr_units)
       !
       attr_desc  = 'pressure'
       attr_units = 'Pa'
       istat = nf90_def_var(ncID, p_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), p_ncID)
       istat = nf90_put_att(ncID, p_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, p_ncID, 'units',       attr_units)
       !
       attr_desc  = 'temperature'
       attr_units = 'K'
       istat = nf90_def_var(ncID, t_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), t_ncID)
       istat = nf90_put_att(ncID, t_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, t_ncID, 'units',       attr_units)
       !
       attr_desc  = 'potential temperature'
       attr_units = 'K'
       istat = nf90_def_var(ncID, tp_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), tp_ncID)
       istat = nf90_put_att(ncID, tp_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, tp_ncID, 'units',       attr_units)
       !
       attr_desc  = 'virtual temperature'
       attr_units = 'K'
       istat = nf90_def_var(ncID, tv_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), tv_ncID)
       istat = nf90_put_att(ncID, tv_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, tv_ncID, 'units',       attr_units)
       !
       attr_desc  = 'u velocity'
       attr_units = 'm/s'
       istat = nf90_def_var(ncID, u_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), u_ncID)
       istat = nf90_put_att(ncID, u_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, u_ncID, 'units',       attr_units)
       !
       attr_desc  = 'v velocity'
       attr_units = 'm/s'
       istat = nf90_def_var(ncID, v_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), v_ncID)
       istat = nf90_put_att(ncID, v_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, v_ncID, 'units',       attr_units)
       !
       attr_desc  = 'w velocity'
       attr_units = 'm/s'
       istat = nf90_def_var(ncID, w_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), w_ncID)
       istat = nf90_put_att(ncID, w_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, w_ncID, 'units',       attr_units)
       !
       attr_desc  = 'specific humidity'
       attr_units = 'kg/kg'
       istat = nf90_def_var(ncID, qv_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), qv_ncID)
       istat = nf90_put_att(ncID, qv_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, qv_ncID, 'units',       attr_units)
       !
       attr_desc  = 'density'
       attr_units = 'kg/m3'
       istat = nf90_def_var(ncID, rho_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,sig_ncID,nt_ncID/), rho_ncID)
       istat = nf90_put_att(ncID, rho_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, rho_ncID, 'units',       attr_units)
       !
       ! Put global attributes
       !
       attr_title = 'Meteo database for Fall3d from '//TRIM(MY_MET%meteo_data_type)
       istat = nf90_put_att(ncID, NF90_GLOBAL, 'TITLE', attr_title)
       !
       attr_title = VERSION
       istat = nf90_put_att(ncID, NF90_GLOBAL, 'CODE_VERSION', attr_title)
       !
       select case(MY_GRID%map_h)
       case(MAP_H_CARTESIAN)
          attr_title = 'cartesian'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_H_TYPE', attr_title)
          !
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_maph_name,   MY_GRID%map_h)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmin_name, MY_GRID%lonmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmax_name, MY_GRID%lonmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_dlon_name,   MY_GRID%dlon)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_latmin_name, MY_GRID%latmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_latmax_name, MY_GRID%latmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_dlat_name,   MY_GRID%dlat)
          !
       case(MAP_H_SPHERICAL)
          attr_title = 'spherical'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_H_TYPE', attr_title)
          !
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_maph_name,   MY_GRID%map_h)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmin_name, MY_GRID%lonmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmax_name, MY_GRID%lonmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_dlon_name,   MY_GRID%dlon)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_latmin_name, MY_GRID%latmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_latmax_name, MY_GRID%latmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_dlat_name,   MY_GRID%dlat)
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          !
       end select
       !
       select case(MY_GRID%map_v)
       case(MAP_V_SIGMA_NO_DECAY)
          attr_title = 'sigma_no_decay'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_V_TYPE', attr_title)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_mapv_name, MY_GRID%map_v)

          !
       case(MAP_V_SIGMA_LINEAR_DECAY)
          attr_title = 'sigma_linear_decay'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_V_TYPE', attr_title)
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_mapv_name, MY_GRID%map_v)
          !
       case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect vertical mapping'
          !
       end select
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_ztop_name, MY_GRID%X3max)
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_year_name     , MY_TIME%start_year)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_month_name    , MY_TIME%start_month)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_day_name      , MY_TIME%start_day)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_dbs_start_name, MY_TIME%dbs_start)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_dbs_end_name  , MY_TIME%dbs_end)
       !
       istat = nf90_enddef(ncID)
       !
    end if
    !
    if(.not.PARALLEL_IO) call parallel_bcast(istat,1,0)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Error defining variables'
       return
    end if
    !
    !*** Write variables
    !
    if(PARALLEL_IO) then
       !
       !                 PARALLEL IO
       !
       !
       !  lon (or x)
       !
       start1d=(/my_ibs/)
       count1d=(/my_ibe-my_ibs+1/)
       istat = nf90_put_var(ncID, lon_ncID, MY_GRID%lon_c,start=start1d,count=count1d)
       !
       !  lat (or y)
       !
       start1d=(/my_jbs/)
       count1d=(/my_jbe-my_jbs+1/)
       istat = nf90_put_var(ncID, lat_ncID, MY_GRID%lat_c,start=start1d,count=count1d)
       !
       ! sigma level
       !
       allocate(work1d(my_kbs:my_kbe))
       do k = my_kbs,my_kbe
          work1d(k) = MY_GRID%gl_sigma(k)
       end do
       start1d=(/my_kbs/)
       count1d=(/my_kbe-my_kbs+1/)
       istat = nf90_put_var(ncID, sig_ncID, work1d,start=start1d,count=count1d)
       deallocate(work1d)
       !
       !  time
       !
       start1d=(/1/)
       count1d=(/nt/)
       istat = nf90_put_var(ncID, tim_ncID, MY_MET%timesec,start=start1d,count=count1d)
       !
       ! z coordinate of sigma layers
       !
       start3d=(/my_ibs,my_jbs,my_kbs/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1/)
       istat = nf90_put_var(ncID, zs_ncID, MY_GRID%z_c,start=start3d,count=count3d)
       !
       ! terrain
       !
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_put_var(ncID, h_ncID, MY_GRID%h_c,start=start2d,count=count2d)
       !
       ! land mask
       !
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_put_var(ncID, lmask_ncID, MY_MET%my_lmaskc,start=start2d,count=count2d)
       !
       ! z0
       !
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_put_var(ncID, z0_ncID, MY_MET%my_z0c,start=start2d,count=count2d)
       !
       ! land use
       !
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_put_var(ncID, luse_ncID, MY_MET%my_lusec,start=start2d,count=count2d)
       !
       ! pblh
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, pblh_ncID, MY_MET%my_pblhc,start=start3d,count=count3d)
       !
       ! u*
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, ust_ncID, MY_MET%my_ustc,start=start3d,count=count3d)
       !
       ! smoi
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, smoi_ncID, MY_MET%my_smoic,start=start3d,count=count3d)
       !
       ! prec
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, prec_ncID, MY_MET%my_prec,start=start3d,count=count3d)
       !
       ! u10
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, u10_ncID, MY_MET%my_u10,start=start3d,count=count3d)
       !
       ! v10
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, v10_ncID, MY_MET%my_v10,start=start3d,count=count3d)
       !
       ! T2 (surface)
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, t2_ncID, MY_MET%my_t2,start=start3d,count=count3d)
       !
       ! Monin
       !
       start3d=(/my_ibs,my_jbs,1/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,nt/)
       istat = nf90_put_var(ncID, mon_ncID, MY_MET%my_monc,start=start3d,count=count3d)
       !
       ! pressure
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, p_ncID, MY_MET%my_pc,start=start4d,count=count4d)
       !
       ! temperature
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, t_ncID, MY_MET%my_tc,start=start4d,count=count4d)
       !
       ! potential temperature
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, tp_ncID, MY_MET%my_tpc,start=start4d,count=count4d)
       !
       ! virtual potential temperature
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, tv_ncID, MY_MET%my_tvc,start=start4d,count=count4d)
       !
       ! u velocity
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, u_ncID, MY_MET%my_uc,start=start4d,count=count4d)
       !
       ! v velocity
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, v_ncID, MY_MET%my_vc,start=start4d,count=count4d)
       !
       ! w velocity
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, w_ncID, MY_MET%my_wc,start=start4d,count=count4d)
       !
       ! specific humidity
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, qv_ncID, MY_MET%my_qvc,start=start4d,count=count4d)
       !
       ! density
       !
       start4d=(/my_ibs,my_jbs,my_kbs,1/)
       count4d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1,nt/)
       istat = nf90_put_var(ncID, rho_ncID, MY_MET%my_rhoc,start=start4d,count=count4d)
       !
       istat = nf90_close(ncID)
       !
    else
       !
       !                    SERIAL_IO
       !
       !  lon (or x)
       !
       if(master) then
          allocate(work1d(nbx))
          do i = 1,nbx
             work1d(i) = MY_GRID%lonmin + (i-1)*MY_GRID%dlon
          end do
          !
          !  If necessary, correct longitudes to be in the range (-180,180)
          !
          !if(MY_GRID%map_h.eq.MAP_H_SPHERICAL) then
          !   do i = 1,nbx
          !      if(work1d(i).gt.180.0_rp) work1d(i) = work1d(i) - 360.0_rp
          !   end do
          !end if
          istat = nf90_put_var(ncID, lon_ncID, work1d,start=(/1/),count=(/nbx/))
          deallocate(work1d)
       end if
       !
       !  lat (or y)
       !
       if(master) then
          allocate(work1d(nby))
          do j = 1,nby
             work1d(j) = MY_GRID%latmin + (j-1)*MY_GRID%dlat
          end do
          istat = nf90_put_var(ncID, lat_ncID, work1d,start=(/1/),count=(/nby/))
          deallocate(work1d)
       end if
       !
       !  sigma levels
       !
       if(master) then
          allocate(work1d(nbz))
          do k = 1,nbz
             work1d(k) = MY_GRID%gl_sigma(k)
          end do
          istat = nf90_put_var(ncID, sig_ncID, work1d, start=(/1/),count=(/nbz/))
          deallocate(work1d)
       end if
       !
       !  time
       !
       if(master) then
          istat = nf90_put_var(ncID, tim_ncID, MY_MET%timesec, start=(/1/),count=(/nt/))
       end if
       !
       ! z coordinate of sigma layers
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
       else
          allocate(work3d(1,1,1))
       end if
       call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_GRID%z_c)
       if(master) then
          istat = nf90_put_var(ncID, zs_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nbz/))
       end if
       !
       ! terrain
       !
       if(master) then
          allocate(work2d(nbx,nby))
          work2d(:,:) = work3d(:,:,1)
          istat = nf90_put_var(ncID, h_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
          deallocate(work2d)
       end if
       deallocate(work3d)
       !
       ! land mask
       !
       if(master) then
          allocate(work2d(nbx,nby))
       else
          allocate(work2d(1,1))
       end if
       call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_lmaskc)
       if(master) then
          istat = nf90_put_var(ncID, lmask_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
       end if
       deallocate(work2d)
       !
       ! z0
       !
       if(master) then
          allocate(work2d(nbx,nby))
       else
          allocate(work2d(1,1))
       end if
       call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_z0c)
       if(master) then
          istat = nf90_put_var(ncID, z0_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
       end if
       deallocate(work2d)
       !
       ! land use
       !
       if(master) then
          allocate(work2d(nbx,nby))
       else
          allocate(work2d(1,1))
       end if
       call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_lusec)
       if(master) then
          istat = nf90_put_var(ncID, luse_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
       end if
       deallocate(work2d)
       !
       ! pblh
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_pblhc(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, pblh_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! u*
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_ustc(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, ust_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! smoi
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_smoic(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, smoi_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! prec
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_prec(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, prec_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! u10
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_u10(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, u10_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! v10
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_v10(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, v10_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! T2 (surface)
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_t2(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, t2_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! Monin
       !
       if(master) then
          allocate(work2d(nbx,nby))
          allocate(work3d(nbx,nby,nt))
       else
          allocate(work2d(1,1))
          allocate(work3d(1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo_2D(work2d,nbx,nby,MY_MET%my_monc(:,:,it))
          if(master) work3d(:,:,it) = work2d(:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, mon_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nt/))
       end if
       deallocate(work2d)
       deallocate(work3d)
       !
       ! pressure
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_pc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, p_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! temperature
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_tc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, t_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! potential temperature
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_tpc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, tp_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! virtual temperature
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_tvc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, tv_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! U
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_uc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, u_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! V
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_vc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, v_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! W
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_wc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, w_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! specific humidity
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_qvc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, qv_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! density
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
          allocate(work4d(nbx,nby,nbz,nt))
       else
          allocate(work3d(1,1,1))
          allocate(work4d(1,1,1,1))
       end if
       do it = 1,nt
          call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_MET%my_rhoc(:,:,:,it))
          if(master) work4d(:,:,:,it) = work3d(:,:,:)
       end do
       if(master) then
          istat = nf90_put_var(ncID, rho_ncID, work4d, start=(/1,1,1,1/),count=(/nbx,nby,nbz,nt/))
       end if
       deallocate(work3d)
       deallocate(work4d)
       !
       ! close file
       !
       if(master) then
          istat = nf90_close(ncID)
       end if
       !
    end if ! PARALLEL_IO
    !
    return
  end subroutine nc_IO_out_dbs
  !
  !---------------------------------
  !    subroutine nc_IO_out_grid
  !---------------------------------
  !
  !>   @brief
  !>   Outputs grid (time independent variables) in netCDF format
  !
  subroutine nc_IO_out_grid(MY_FILES,MY_GRID,MY_CUTS,MY_SPE,MY_TRA,MY_TIME,MY_OUT,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_GRID       ARAKAWA_C_GRID structure already filled
    !>   @param MY_CUTS       CUTS           structure already filled
    !>   @param MY_SPE        list of parameters defining species and categories
    !>   @param MY_TRA        TRACERS        structure already filled
    !>   @param MY_TIME       RUN_TIME       structure already filled
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(ARAKAWA_C_GRID),  intent(IN   ) :: MY_GRID
    type(CUTS),            intent(IN   ) :: MY_CUTS
    type(SPECIES_PARAMS),  intent(IN   ) :: MY_SPE
    type(TRACERS),         intent(IN   ) :: MY_TRA
    type(RUN_TIME),        intent(IN   ) :: MY_TIME
    type(MODEL_OUTPUT),    intent(IN   ) :: MY_OUT
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: nc_file,sblock,name_nc
    integer(ip)  :: nbx,nby,nbz,nbins
    integer(ip)  :: istat,i,j,k,ib
    integer(ip)  :: mode_flag
    integer(ip)  :: ispe, spe_code, cat_code
    integer(ip)  :: nbins_spe(nspe_max)
    integer(ip)  :: nb_ncID
    integer(ip)  :: start1d(1),start2d(2),start3d(3)
    integer(ip)  :: count1d(1),count2d(2),count3d(3)
    !
    integer(ip), allocatable :: iwork1d(:)
    real(rp),    allocatable :: work1d(:)
    real(rp),    allocatable :: work2d(:,:)
    real(rp),    allocatable :: work3d(:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_out_grid'
    MY_ERR%message = ' '
    !
    nc_file = MY_FILES%file_res
    !
    PARALLEL_IO   = MY_OUT%PARALLEL_IO
    out_con_total = MY_OUT%out_con_total
    out_con_bins  = MY_OUT%out_con_bins
    out_col_load  = MY_OUT%out_col_load
    out_cloud_top = MY_OUT%out_cloud_top
    out_grn_total = MY_OUT%out_grn_total
    out_grn_bins  = MY_OUT%out_grn_bins
    out_wet_total = MY_OUT%out_wet_total
    !
    nbx       = gl_nbx
    nby       = gl_nby
    nbz       = gl_nbz
    nbins     = MY_TRA%nbins
    !
    nbins_spe(:) = 0
    do ib = 1,nbins
       spe_code = MY_TRA%MY_BIN%bin_spe(ib)
       nbins_spe(spe_code) = nbins_spe(spe_code) + 1
    end do
    !
    !*** Create file
    !
#if defined WITH_MPI
    if(PARALLEL_IO) then
       mode_flag = IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL)
       mode_flag = IOR(mode_flag, NF90_MPIIO)
       mode_flag = IOR(mode_flag, NF90_CLOBBER)
       istat     = nf90_create_par(TRIM(nc_file), cmode=mode_flag , comm=COMM_WORLD, info = MPI_INFO_NULL, ncid=ncID)
    else if(master) then
       mode_flag = IOR(NF90_CLOBBER,NF90_NETCDF4)
       istat     = nf90_create(TRIM(nc_file),cmode=mode_flag, ncid=ncID)
    end if
#else
    mode_flag = IOR(NF90_CLOBBER,NF90_NETCDF4)
    istat     = nf90_create(TRIM(nc_file),cmode=mode_flag, ncid=ncID)
#endif
    !
    !*** Check errors
    !
    if(PARALLEL_IO.and.istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Unable to create '//TRIM(nc_file)
       return
    else
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%message = 'Unable to create '//TRIM(nc_file)
          return
       end if
    end if
    !
    !*** File dimensions
    !
    if(PARALLEL_IO.or.master) then
       !
       !  Define dimensions
       !
       select case(MY_GRID%map_h)
       case(MAP_H_CARTESIAN)
          istat = nf90_def_dim(ncID, x_nc_name, nbx , nx_ncID )
          istat = nf90_def_dim(ncID, y_nc_name, nby , ny_ncID )
          !
       case(MAP_H_SPHERICAL)
          istat = nf90_def_dim(ncID, lon_nc_name, nbx , nx_ncID )
          istat = nf90_def_dim(ncID, lat_nc_name, nby , ny_ncID )
          !
       case(MAP_H_POLAR)
          istat = nf90_def_dim(ncID, x_nc_name, nbx , nx_ncID )
          istat = nf90_def_dim(ncID, y_nc_name, nby , ny_ncID )
          !
       case(MAP_H_MERCATOR)
          istat = nf90_def_dim(ncID, x_nc_name, nbx , nx_ncID )
          istat = nf90_def_dim(ncID, y_nc_name, nby , ny_ncID )
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          !
       end select
       !
       istat = nf90_def_dim(ncID, sig_nc_name    ,nbz      ,ns_ncID     )
       istat = nf90_def_dim(ncID, z_nc_name      ,nbz      ,nz_ncID     )
       istat = nf90_def_dim(ncID, bin_nc_name    ,nbins    ,nb_ncID     )
       !
       do ispe = 1,MY_SPE%nspe
          spe_code = MY_SPE%code(ispe)
          sblock   = SPE_TAG    (spe_code)
          name_nc  = TRIM(sblock)//TRIM(bin_spe_nc_name)
          istat    = nf90_def_dim(ncID, name_nc, nbins_spe(spe_code), nb_spe_ncID(spe_code) )
       end do
       !
       if(MY_CUTS%ncutx.gt.0) istat = nf90_def_dim(ncID, xcut_nc_name,    MY_CUTS%ncutx, ncutx_ncID  )
       if(MY_CUTS%ncuty.gt.0) istat = nf90_def_dim(ncID, ycut_nc_name,    MY_CUTS%ncuty, ncuty_ncID  )
       if(MY_CUTS%ncutz.gt.0) istat = nf90_def_dim(ncID, zcut_nc_name,    MY_CUTS%ncutz, ncutz_ncID  )
       if(MY_CUTS%nfl  .gt.0) istat = nf90_def_dim(ncID, zflcut_nc_name,  MY_CUTS%nfl,   nfl_ncID    )
                              istat = nf90_def_dim(ncID, npm_nc_name ,    npm,           npm_ncID    )
       !
       istat = nf90_def_dim(ncID, str_nc_name, s_name, nstr_ncID )
       istat = nf90_def_dim(ncID, tim_nc_name, NF90_UNLIMITED, nt_ncID)
       !
       ! Define coordinate variables
       !
       select case(MY_GRID%map_h)
       case(MAP_H_CARTESIAN, MAP_H_POLAR, MAP_H_MERCATOR)
          attr_desc  = 'x-coordinate. East positive'
          attr_units = 'm'
          istat = nf90_def_var(ncID, x_nc_name ,NF90_MYTYPE, (/nx_ncID/), lon_ncID)
          istat = nf90_put_att(ncID, lon_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lon_ncID, 'units',       attr_units)
          !
          attr_desc  = 'y-coordinate. North positive'
          attr_units = 'm'
          istat = nf90_def_var(ncID, y_nc_name ,NF90_MYTYPE, (/ny_ncID/), lat_ncID)
          istat = nf90_put_att(ncID, lat_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lat_ncID, 'units',       attr_units)
          !
       case(MAP_H_SPHERICAL)
          attr_desc  = 'longitude'
          attr_units = 'degrees_east'
          istat = nf90_def_var(ncID, lon_nc_name ,NF90_MYTYPE, (/nx_ncID/), lon_ncID)
          istat = nf90_put_att(ncID, lon_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lon_ncID, 'units',       attr_units)
          !
          attr_desc  = 'latitude'
          attr_units = 'degrees_north'
          istat = nf90_def_var(ncID, lat_nc_name ,NF90_MYTYPE, (/ny_ncID/), lat_ncID)
          istat = nf90_put_att(ncID, lat_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, lat_ncID, 'units',       attr_units)
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          !
       end select
       !
       attr_desc  = 'sigma level'
       attr_units = '-'
       istat = nf90_def_var(ncID, sig_nc_name ,NF90_MYTYPE, (/ns_ncID/), sig_ncID)
       istat = nf90_put_att(ncID, sig_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, sig_ncID, 'units',       attr_units)
       !
       attr_desc  = 'z coordinate (a.s.l)'
       attr_units = 'm'
       istat = nf90_def_var(ncID, z_nc_name ,NF90_MYTYPE, (/nz_ncID/), z_ncID)
       istat = nf90_put_att(ncID, z_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, z_ncID, 'units',       attr_units)
       !
       attr_desc  = 'bin number'
       attr_units = '-'
       istat = nf90_def_var(ncID, bin_nc_name ,NF90_INT, (/nb_ncID/), bin_ncID)
       istat = nf90_put_att(ncID, bin_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, bin_ncID, 'units',       attr_units)
       !
       attr_desc  = 'species bin number'
       attr_units = '-'
       do ispe = 1,MY_SPE%nspe
          spe_code = MY_SPE%code(ispe)
          sblock   = SPE_TAG    (spe_code)
          name_nc  = TRIM(sblock)//TRIM(bin_spe_nc_name)
          !
          istat = nf90_def_var(ncID, name_nc ,NF90_INT, (/nb_spe_ncID(spe_code)/), bin_spe_ncID(spe_code))
          istat = nf90_put_att(ncID, bin_spe_ncID(spe_code), 'description', attr_desc)
          istat = nf90_put_att(ncID, bin_spe_ncID(spe_code), 'units',       attr_units)
       end do
       !
       if(MY_CUTS%ncutx.gt.0) then
          select case(MY_GRID%map_h)
          case(MAP_H_CARTESIAN, MAP_H_POLAR, MAP_H_MERCATOR)
             attr_desc  = 'x coordinate of y-z plane cuts '
             attr_units = 'm'
          case(MAP_H_SPHERICAL)
             attr_desc  = 'longitude of latitue-z plane cuts'
             attr_units = 'deg'
          end select
          istat = nf90_def_var(ncID, xcut_nc_name ,NF90_MYTYPE, (/ncutx_ncID/), xcut_ncID)
          istat = nf90_put_att(ncID, xcut_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, xcut_ncID, 'units',       attr_units)
       end if
       !
       if(MY_CUTS%ncuty.gt.0) then
          select case(MY_GRID%map_h)
          case(MAP_H_CARTESIAN, MAP_H_POLAR, MAP_H_MERCATOR)
             attr_desc  = 'y coordinate of x-z plane cuts '
             attr_units = 'm'
          case(MAP_H_SPHERICAL)
             attr_desc  = 'latitude of longitude-z plane cuts'
             attr_units = 'deg'
          end select
          istat = nf90_def_var(ncID, ycut_nc_name ,NF90_MYTYPE, (/ncuty_ncID/), ycut_ncID)
          istat = nf90_put_att(ncID, ycut_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, ycut_ncID, 'units',       attr_units)
       end if
       !
       if(MY_CUTS%ncutz.gt.0) then
          attr_desc  = 'z coordinate of x-y plane cuts'
          attr_units = 'm'
          istat = nf90_def_var(ncID, zcut_nc_name ,NF90_MYTYPE, (/ncutz_ncID/), zcut_ncID)
          istat = nf90_put_att(ncID, zcut_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, zcut_ncID, 'units',       attr_units)
       end if
       !
       if(MY_CUTS%nfl.gt.0) then
          attr_desc  = 'flight level value'
          attr_units = '-'
          istat = nf90_def_var(ncID, zflcut_nc_name, NF90_MYTYPE, (/nfl_ncID/), zflcut_ncID)
          istat = nf90_put_att(ncID, zflcut_ncID, 'description', attr_desc)
          istat = nf90_put_att(ncID, zflcut_ncID, 'units',       attr_units)
       end if
       !
       attr_desc  = 'PM bin number'
       attr_units = '-'
       istat = nf90_def_var(ncID, npm_nc_name ,NF90_INT, (/npm_ncID/), pm_ncID)
       istat = nf90_put_att(ncID, pm_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, pm_ncID, 'units',       attr_units)
       !
       attr_desc  = 'time'
       write(attr_units,"(A,1X,I4.4,'-',I2.2,'-',I2.2,1X,A)") "seconds since",     &
            MY_TIME%start_year,  &
            MY_TIME%start_month, &
            MY_TIME%start_day,   &
            '0:0:0'
       !attr_units = 'h'
       istat = nf90_def_var(ncID, tim_nc_name ,NF90_INT, (/nt_ncID/), tim_ncID)
       istat = nf90_put_att(ncID, tim_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, tim_ncID, 'units',       attr_units)
       istat = nf90_put_att(ncID, tim_ncID, 'calendar','proleptic_gregorian')
       !
       attr_desc  = 'date in format DDmonYYYY_HH:MM'
       attr_units = '-'
       istat = nf90_def_var(ncID, date_nc_name ,NF90_CHAR, (/nstr_ncID,nt_ncID/), date_ncID)
       istat = nf90_put_att(ncID, date_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, date_ncID, 'units',       attr_units)
       !
       ! Define time-independent variables
       !
       attr_desc  = 'terrain elevation (a.s.l.)'
       attr_units = 'm'
       istat = nf90_def_var(ncID, h_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID/), h_ncID)
       istat = nf90_put_att(ncID, h_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, h_ncID, 'units',       attr_units)
       !
       attr_desc  = 'z coordinate of sigma levels (a.s.l.)'
       attr_units = 'm'
       istat = nf90_def_var(ncID, zs_nc_name ,NF90_MYTYPE, (/nx_ncID,ny_ncID,ns_ncID/), zs_ncID)
       istat = nf90_put_att(ncID, zs_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, zs_ncID, 'units',       attr_units)
       !
       ! Define time-dependent variables depending on each specie
       !
       do ispe = 1,MY_SPE%nspe
          spe_code = MY_SPE%code    (ispe)
          cat_code = MY_SPE%category(ispe)
          sblock   = SPE_TAG        (spe_code)
          !
          !  1. CON
          if(out_con_total) then
             attr_desc  = TRIM(sblock)//'concentration on sigma planes'
             attr_units = 'gr/m3'
             name_nc    = TRIM(sblock)//TRIM(c_total_nc_name)
             istat = nf90_def_var(ncID, name_nc, NF90_MYTYPE, (/nx_ncID,ny_ncID,ns_ncID,nt_ncID/), con_ncID)
             istat = nf90_put_att(ncID, con_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, con_ncID, 'units',       attr_units)
          end if
          !
          !  2. CON_BIN
          if(out_con_bins) then
             attr_desc  = TRIM(sblock)//'concentration on sigma planes per bin'
             attr_units = 'gr/m3'
             name_nc    = TRIM(sblock)//TRIM(c_bin_nc_name)
             nb_ncID    = nb_spe_ncID(spe_code)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, &
                                 (/nx_ncID,ny_ncID,ns_ncID,nb_ncID,nt_ncID/), con_bin_ncID)
             istat = nf90_put_att(ncID, con_bin_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, con_bin_ncID, 'units',       attr_units)
          end if
          !
          !  3. CON_YZ
          if((MY_CUTS%ncutx.gt.0)) then
             attr_desc  = TRIM(sblock)//'concentration on x-cut planes'
             attr_units = 'gr/m3'
             name_nc    = TRIM(sblock)//TRIM(cutx_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/ny_ncID,nz_ncID,ncutx_ncID,nt_ncID/), cutx_ncID)
             istat = nf90_put_att(ncID, cutx_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, cutx_ncID, 'units',       attr_units)
          end if
          !
          !  4. CON_XZ
          if((MY_CUTS%ncuty.gt.0)) then
             attr_desc  = TRIM(sblock)//'concentration on y-cut planes'
             attr_units = 'gr/m3'
             name_nc    = TRIM(sblock)//TRIM(cuty_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,nz_ncID,ncuty_ncID,nt_ncID/), cuty_ncID)
             istat = nf90_put_att(ncID, cuty_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, cuty_ncID, 'units',       attr_units)
          end if
          !
          !  5. CON_XY
          if((MY_CUTS%ncutz.gt.0)) then
             attr_desc  = TRIM(sblock)//'concentration on z-cut planes'
             attr_units = 'gr/m3'
             name_nc    = TRIM(sblock)//TRIM(cutz_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,ncutz_ncID,nt_ncID/), cutz_ncID)
             istat = nf90_put_att(ncID, cutz_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, cutz_ncID, 'units',       attr_units)
          end if
          !
          !  6. FL
          if((MY_CUTS%nfl.gt.0)) then
             attr_desc  = TRIM(sblock)//'concentration at flight levels'
             attr_units = 'gr/m3'
             name_nc    = TRIM(sblock)//TRIM(fl_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,nfl_ncID,nt_ncID/), fl_ncID)
             istat = nf90_put_att(ncID, fl_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, fl_ncID, 'units',       attr_units)
          end if
          !
          ! 7. COL_MASS
          if(out_col_load) then
             attr_desc  = TRIM(sblock)//'column mass load'
             if(spe_code.eq.SPE_SO2) then
                attr_units = 'DU'
             else
                attr_units = 'gr/m2'
             end if
             name_nc    = TRIM(sblock)//TRIM(col_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), col_ncID)
             istat = nf90_put_att(ncID, col_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, col_ncID, 'units',       attr_units)
          end if
          !
          ! 8. COL_MASS_PM
          if(out_col_load.and.(cat_code.ne.CAT_AEROSOL)) then
             attr_desc  = TRIM(sblock)//'column mass load PM bins'
             attr_units = 'gr/m2'
             name_nc    = TRIM(sblock)//TRIM(pmc_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,npm_ncID,nt_ncID/), pmc_ncID)
             istat = nf90_put_att(ncID, pmc_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, pmc_ncID, 'units',       attr_units)
          end if
          !
          ! 9. CLOUD_TOP
          if(out_cloud_top) then
             attr_desc  = TRIM(sblock)//'cloud top height'
             attr_units = 'm (a.s.l.)'
             name_nc    = TRIM(sblock)//TRIM(clh_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), clh_ncID)
             istat = nf90_put_att(ncID, clh_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, clh_ncID, 'units',       attr_units)
          end if
          !
          !  10. GRN_LOAD
          if(out_grn_total) then
             attr_desc  = TRIM(sblock)//'ground mass load'
             attr_units = 'kg/m2'
             name_nc    = TRIM(sblock)//TRIM(grn_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), grn_ncID)
             istat = nf90_put_att(ncID, grn_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, grn_ncID, 'units',       attr_units)
          end if
          !
          !  11. GRN_LOAD_BIN
          if(out_grn_bins) then
             attr_desc  = TRIM(sblock)//'ground mass load per bin'
             attr_units = 'kg/m2'
             name_nc    = TRIM(sblock)//TRIM(grn_bin_nc_name)
             nb_ncID    = nb_spe_ncID(spe_code)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,nb_ncID,nt_ncID/), grn_bin_ncID)
             istat = nf90_put_att(ncID, grn_bin_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, grn_bin_ncID, 'units',       attr_units)
          end if
          !
          !  12. WET_DEP
          if(out_wet_total) then
             attr_desc  = TRIM(sblock)//'mass removed by wet deposition'
             attr_units = 'kg/m2'
             name_nc    = TRIM(sblock)//TRIM(wet_nc_name)
             istat = nf90_def_var(ncID, name_nc,NF90_MYTYPE, (/nx_ncID,ny_ncID,nt_ncID/), wet_ncID)
             istat = nf90_put_att(ncID, wet_ncID, 'description', attr_desc)
             istat = nf90_put_att(ncID, wet_ncID, 'units',       attr_units)
          end if
          !
       end do  ! ispe = 1,MY_SPE%nspe
       !
       ! Put global attributes
       !
       attr_title = 'FALL3D model results'
       istat = nf90_put_att(ncID, NF90_GLOBAL, 'TITLE', attr_title)
       !
       attr_title = VERSION
       istat = nf90_put_att(ncID, NF90_GLOBAL, 'CODE_VERSION', attr_title)
       !
       select case(MY_GRID%map_h)
       case(MAP_H_CARTESIAN)
          attr_title = 'cartesian'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_H', attr_title)
          !
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmin_name, MY_GRID%lonmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'XMAX', MY_GRID%lonmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DX',   MY_GRID%dlon)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'YMIN', MY_GRID%latmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'YMAX', MY_GRID%latmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DY',   MY_GRID%dlat)
          !
       case(MAP_H_SPHERICAL)
          attr_title = 'spherical'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_H', attr_title)
          !
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmin_name, MY_GRID%lonmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'LONMAX', MY_GRID%lonmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DLON',   MY_GRID%dlon)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'LATMIN', MY_GRID%latmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'LATMAX', MY_GRID%latmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DLAT',   MY_GRID%dlat)
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          !
       end select
       !
       select case(MY_GRID%map_v)
       case(MAP_V_CARTESIAN)
          attr_title = 'cartesian'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_V', attr_title)
          !
       case(MAP_V_SIGMA_NO_DECAY)
          attr_title = 'sigma_no_decay'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_V', attr_title)
          !
       case(MAP_V_SIGMA_LINEAR_DECAY)
          attr_title = 'sigma_linear_decay'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_V', attr_title)
          !
       case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect vertical mapping'
          !
       end select
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_ztop_name, MY_GRID%X3max)
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_year_name , MY_TIME%start_year)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_month_name, MY_TIME%start_month)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_day_name  , MY_TIME%start_day)
       !
       istat = nf90_enddef(ncID)
       !
    end if  ! if(PARALLEL_IO.or.master)
    !
    if(.not.PARALLEL_IO) call parallel_bcast(istat,1,0)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Error defining variables'
       return
    end if
    !
    !*** Write time-independent variables
    !
    if(PARALLEL_IO) then
       !
       !  lon (or x)
       !
       start1d=(/my_ibs/)
       count1d=(/my_ibe-my_ibs+1/)
       istat = nf90_put_var(ncID, lon_ncID, MY_GRID%lon_c,start=start1d,count=count1d)
       !
       !  lat (or y)
       !
       start1d=(/my_jbs/)
       count1d=(/my_jbe-my_jbs+1/)
       istat = nf90_put_var(ncID, lat_ncID, MY_GRID%lat_c,start=start1d,count=count1d)
       !
       ! sigma level
       !
       allocate(work1d(my_kbs:my_kbe))
       do k = my_kbs,my_kbe
          work1d(k) = MY_GRID%gl_sigma(k)
       end do
       start1d=(/my_kbs/)
       count1d=(/my_kbe-my_kbs+1/)
       istat = nf90_put_var(ncID, sig_ncID, work1d,start=start1d,count=count1d)
       deallocate(work1d)
       !
       !  z coordinate
       !
       start1d=(/my_kbs/)
       count1d=(/my_kbe-my_kbs+1/)
       istat = nf90_put_var(ncID, z_ncID  , MY_GRID%X3_c,start=start1d,count=count1d)
       !
       ! bins
       !
       allocate(iwork1d(nbins))
       do i = 1,nbins
          iwork1d(i) = i
       end do
       start1d=(/1/)
       count1d=(/nbins/)
       istat = nf90_put_var(ncID, bin_ncID, iwork1d,start=start1d,count=count1d)
       deallocate(iwork1d)
       !
       ! species bins
       !
       do ispe = 1,MY_SPE%nspe
          spe_code = MY_SPE%code    (ispe)
          cat_code = MY_SPE%category(ispe)
          sblock   = SPE_TAG        (spe_code)
          !
          allocate(iwork1d(nbins_spe(spe_code)))
          start1d=(/1/)
          count1d=(/nbins_spe(spe_code)/)
          istat = nf90_put_var(ncID, bin_spe_ncID(spe_code), iwork1d,start=start1d,count=count1d)
          deallocate(iwork1d)
       end do
       !
       !  cuts
       !
       if(MY_CUTS%ncutx.gt.0) istat = nf90_put_var(ncID, xcut_ncID,   MY_CUTS%x_cut,  start=(/1/),count=(/MY_CUTS%ncutx/))
       if(MY_CUTS%ncuty.gt.0) istat = nf90_put_var(ncID, ycut_ncID,   MY_CUTS%y_cut,  start=(/1/),count=(/MY_CUTS%ncuty/))
       if(MY_CUTS%ncutz.gt.0) istat = nf90_put_var(ncID, zcut_ncID,   MY_CUTS%z_cut,  start=(/1/),count=(/MY_CUTS%ncutz/))
       if(MY_CUTS%nfl  .gt.0) istat = nf90_put_var(ncID, zflcut_ncID, MY_CUTS%fl_cut, start=(/1/),count=(/MY_CUTS%nfl/))
       !
       istat = nf90_put_var(ncID, npm_ncID,  pm_value,       start=(/1/),count=(/npm/))
       !
       ! z coordinate of sigma layers
       !
       start3d=(/my_ibs,my_jbs,my_kbs/)
       count3d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1,my_kbe-my_kbs+1/)
       istat = nf90_put_var(ncID, zs_ncID, MY_GRID%z_c,start=start3d,count=count3d)
       !
       ! terrain
       !
       start2d=(/my_ibs,my_jbs/)
       count2d=(/my_ibe-my_ibs+1,my_jbe-my_jbs+1/)
       istat = nf90_put_var(ncID, h_ncID, MY_GRID%h_c,start=start2d,count=count2d)
       !
       istat = nf90_close(ncID)
       !
    else  ! SERIAL_IO
       !
       !  lon (or x)
       !
       if(master) then
          allocate(work1d(nbx))
          do i = 1,nbx
             work1d(i) = MY_GRID%lonmin + (i-1)*MY_GRID%dlon
          end do
          !
          !  If necessary, correct longitudes to be in the range (-180,180)
          !
          !if(MY_GRID%map_h.eq.MAP_H_SPHERICAL) then
          !   do i = 1,nbx
          !      if(work1d(i).gt.180.0_rp) work1d(i) = work1d(i) - 360.0_rp
          !   end do
          !end if
          istat = nf90_put_var(ncID, lon_ncID, work1d,start=(/1/),count=(/nbx/))
          deallocate(work1d)
       end if
       !
       !  lat (or y)
       !
       if(master) then
          allocate(work1d(nby))
          do j = 1,nby
             work1d(j) = MY_GRID%latmin + (j-1)*MY_GRID%dlat
          end do
          istat = nf90_put_var(ncID, lat_ncID, work1d,start=(/1/),count=(/nby/))
          deallocate(work1d)
       end if
       !
       !  sigma levels
       !
       if(master) then
          allocate(work1d(nbz))
          do k = 1,nbz
             work1d(k) = MY_GRID%gl_sigma(k)
          end do
          istat = nf90_put_var(ncID, sig_ncID, work1d, start=(/1/),count=(/nbz/))
          deallocate(work1d)
       end if
       !
       !  X3 coordinate
       !
       if(master) then
          allocate(work1d(nbz))
          do k = 1,nbz
             work1d(k) = MY_GRID%gl_sigma(k)*MY_GRID%X3max
          end do
          istat = nf90_put_var(ncID, z_ncID  , work1d, start=(/1/),count=(/nbz/))
          deallocate(work1d)
       end if
       !
       !  bins and species bins
       !
       if(master) then
          allocate(iwork1d(nbins))
          do i = 1,nbins
             iwork1d(i) = i
          end do
          istat = nf90_put_var(ncID, bin_ncID, iwork1d, start=(/1/),count=(/nbins/))
          !
          do ispe = 1,MY_SPE%nspe
             spe_code = MY_SPE%code    (ispe)
             !
             start1d=(/1/)
             count1d=(/nbins_spe(spe_code)/)
             istat = nf90_put_var(ncID, bin_spe_ncID(spe_code), iwork1d,start=start1d,count=count1d)
          end do
          deallocate(iwork1d)
       end if
       !
       !  cuts
       !
       if(master.and.MY_CUTS%ncutx.gt.0) istat = nf90_put_var(ncID, xcut_ncID,   &
            MY_CUTS%x_cut,  start=(/1/),count=(/MY_CUTS%ncutx/))
       if(master.and.MY_CUTS%ncuty.gt.0) istat = nf90_put_var(ncID, ycut_ncID,   &
            MY_CUTS%y_cut,  start=(/1/),count=(/MY_CUTS%ncuty/))
       if(master.and.MY_CUTS%ncutz.gt.0) istat = nf90_put_var(ncID, zcut_ncID,   &
            MY_CUTS%z_cut,  start=(/1/),count=(/MY_CUTS%ncutz/))
       if(master.and.MY_CUTS%nfl  .gt.0) istat = nf90_put_var(ncID, zflcut_ncID, &
            MY_CUTS%fl_cut, start=(/1/),count=(/MY_CUTS%nfl/))
       if(master)                        istat = nf90_put_var(ncID, npm_ncID,    &
            pm_value,       start=(/1/),count=(/npm/))
       !
       ! z coordinate of sigma layers
       !
       if(master) then
          allocate(work3d(nbx,nby,nbz))
       else
          allocate(work3d(1,1,1))
       end if
       call domain_gather_corner_points_0halo(work3d,nbx,nby,nbz,MY_GRID%z_c)
       if(master) then
          istat = nf90_put_var(ncID, zs_ncID, work3d, start=(/1,1,1/),count=(/nbx,nby,nbz/))
       end if
       !
       ! terrain
       !
       if(master) then
          allocate(work2d(nbx,nby))
          work2d(:,:) = work3d(:,:,1)
          istat = nf90_put_var(ncID, h_ncID, work2d, start=(/1,1/),count=(/nbx,nby/))
          deallocate(work2d)
       end if
       deallocate(work3d)
       !
       if(master) then
          istat = nf90_close(ncID)
       end if
       !
    end if ! PARALLEL_IO
    !
  return
  end subroutine nc_IO_out_grid
  !
  !---------------------------------
  !    subroutine nc_IO_out_res
  !---------------------------------
  !
  !>   @brief
  !>   Outputs time dependent variables at a given time step in netCDF format
  !
  subroutine nc_IO_out_res(MY_FILES,MY_GRID,MY_CUTS,MY_SPE,MY_TRA,MY_TIME,MY_OUT,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_GRID       ARAKAWA_C_GRID structure already filled
    !>   @param MY_CUTS       CUTS           structure already filled
    !>   @param MY_SPE        list of parameters defining species and categories
    !>   @param MY_TRA        TRACERS        structure already filled
    !>   @param MY_TIME       RUN_TIME       structure already filled
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(ARAKAWA_C_GRID),  intent(IN   ) :: MY_GRID
    type(CUTS),            intent(INOUT) :: MY_CUTS
    type(SPECIES_PARAMS),  intent(IN   ) :: MY_SPE
    type(TRACERS),         intent(INOUT) :: MY_TRA
    type(RUN_TIME),        intent(IN   ) :: MY_TIME
    type(MODEL_OUTPUT),    intent(IN   ) :: MY_OUT
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    logical               :: found
    integer(ip), save     :: ipass = 0
    integer(ip)           :: istat,mode_flag
    integer(ip)           :: nbx,nby,nbz,nbins,nb_spe
    integer(ip)           :: i,j,k,ib,jb,ii,jj,kk,ibin,ipm
    integer(ip)           :: ispe, spe_code, cat_code
    integer(ip)           :: nbins_spe(nspe_max)
    integer(ip)           :: my_nbx,my_nby,my_nbz
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: start3d(3),start4d(4),start5d(5)
    integer(ip)           :: count3d(3),count4d(4),count5d(5)
    real(rp)              :: Hm1,Hm2,Hm3,X3,my_max
    character(len=16)     :: out_date
    character(len=s_file) :: nc_file,sblock,name_nc
    !
    integer(ip), allocatable :: bin_cat(:)
    integer(ip), allocatable :: bin_spe(:)
    !
    real(rp), allocatable :: my_work2d(:,:)
    real(rp), allocatable :: my_work3d(:,:,:)
    real(rp), allocatable :: my_work3d2(:,:,:)
    real(rp), allocatable :: gl_work1d(:)
    real(rp), allocatable :: gl_work2d(:,:)
    real(rp), allocatable :: gl_work3d(:,:,:)
    real(rp), allocatable :: gl_zc    (:,:,:)
    real(rp), allocatable :: gl_c     (:,:,:)
    real(rp), allocatable :: gl_cz    (:,:,:)
    real(rp), allocatable :: gl_work4d(:,:,:,:)
    real(rp), allocatable :: my_cs    (:,:,:,:)     ! at mass points (scaled)
    real(rp), allocatable :: my_cc    (:,:,:,:)     ! at corners
    real(rp), allocatable :: my_ac    (:,:,:  )     ! at corners
    real(rp), allocatable :: my_awetc (:,:    )     ! at corners
    !
    integer(ip), save, allocatable :: indexz(:,:,:)
    real(rp),    save, allocatable :: shapez(:,:,:)
    !
    !*** Initializations
    !
    nc_file = MY_FILES%file_res
    !
    ipass = ipass + 1
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_out_res'
    MY_ERR%message = ' '
    !
    nbx       = gl_nbx
    nby       = gl_nby
    nbz       = gl_nbz
    nbins     = MY_TRA%nbins
    !
    nbins_spe(:) = 0
    do ibin = 1,nbins
       spe_code = MY_TRA%MY_BIN%bin_spe(ibin)
       nbins_spe(spe_code) = nbins_spe(spe_code) + 1
    end do
    !
    allocate(bin_cat(nbins))
    allocate(bin_spe(nbins))
    bin_cat(:) = MY_TRA%MY_BIN%bin_cat(:)
    bin_spe(:) = MY_TRA%MY_BIN%bin_spe(:)
    !
    !*** If necessary, compute interpolation factors for cuts (including FLs). Note that this is done
    !*** by master on a global domain because mesh partitions in sigma and z levels are different
    !
    if( (ipass.eq.1).and.((MY_CUTS%ncutx.gt.0).or.(MY_CUTS%ncuty.gt.0).or.(MY_CUTS%ncutz.gt.0).or.(MY_CUTS%nfl.gt.0)) ) then
       !
       !  Get global zc
       !
       if(master) then
          allocate(gl_zc(nbx,nby,nbz))
       else
          allocate(gl_zc(1,1,1))
       end if
       call domain_gather_corner_points_0halo(gl_zc,nbx,nby,nbz,MY_GRID%z_c)
       !
       !  Get factors to interpolate from the postprocess mesh on z levels to the computational
       !  mesh on sigma levels
       !
       if(master) then
          !
          allocate(indexz(nbx,nby,nbz))
          allocate(shapez(nbx,nby,nbz))
          allocate(gl_work1d(nbz))
          !
          do j = 1,nby
             do i = 1,nbx
                gl_work1d(1:nbz) = gl_zc(i,j,1:nbz)
                do k = 1,nbz
                   X3 = MY_GRID%gl_sigma(k)*MY_GRID%X3max
                   call grid_get_shapez(1,nbz,gl_work1d,X3,indexz(i,j,k),shapez(i,j,k))
                end do
             end do
          end do
          deallocate(gl_work1d)
       end if
       !
       deallocate(gl_zc)
       !
       ! Factors for cut interpolation on z levels mesh
       !
       if(MY_CUTS%ncutx.gt.0) then
          allocate(MY_CUTS%index_x(MY_CUTS%ncutx))
          allocate(MY_CUTS%shape_x(MY_CUTS%ncutx))
          allocate(gl_work1d(nbx))
          !
          do i = 1,nbx
             gl_work1d(i) = MY_GRID%lonmin + (i-1)*MY_GRID%dlon
          end do
          do i = 1,MY_CUTS%ncutx
             call grid_get_shapez(1,nbx,gl_work1d,MY_CUTS%x_cut(i), &
                  MY_CUTS%index_x(i),MY_CUTS%shape_x(i))
             if((MY_CUTS%index_x(i).eq.1).or.(MY_CUTS%index_x(i).eq.nbx-1)) &
                  call task_wriwarn(MY_ERR,'x-cut not found')
          end do
          deallocate(gl_work1d)
       end if
       !
       if(MY_CUTS%ncuty.gt.0) then
          allocate(MY_CUTS%index_y(MY_CUTS%ncuty))
          allocate(MY_CUTS%shape_y(MY_CUTS%ncuty))
          allocate(gl_work1d(nby))
          !
          do j = 1,nby
             gl_work1d(j) = MY_GRID%latmin + (j-1)*MY_GRID%dlat
          end do
          do j = 1,MY_CUTS%ncuty
             call grid_get_shapez(1,nby,gl_work1d,MY_CUTS%y_cut(j), &
                  MY_CUTS%index_y(j),MY_CUTS%shape_y(j))
             if((MY_CUTS%index_y(j).eq.1).or.(MY_CUTS%index_y(j).eq.nby-1)) &
                  call task_wriwarn(MY_ERR,'y-cut not found')
          end do
          deallocate(gl_work1d)
       end if
       !
       if(MY_CUTS%ncutz.gt.0) then
          allocate(MY_CUTS%index_z(MY_CUTS%ncutz))
          allocate(MY_CUTS%shape_z(MY_CUTS%ncutz))
          allocate(gl_work1d(nbz))
          !
          do k = 1,nbz
             gl_work1d(k) = MY_GRID%gl_sigma(k)*MY_GRID%X3max
          end do
          do k = 1,MY_CUTS%ncutz
             call grid_get_shapez(1,nbz,gl_work1d,MY_CUTS%z_cut(k), &
                  MY_CUTS%index_z(k),MY_CUTS%shape_z(k))
             if((MY_CUTS%index_z(k).eq.1).or.(MY_CUTS%index_z(k).eq.nbz-1)) &
                  call task_wriwarn(MY_ERR,'z-cut not found')
          end do
          deallocate(gl_work1d)
       end if
       !
       if(MY_CUTS%nfl.gt.0) then
          allocate(MY_CUTS%index_zfl(MY_CUTS%nfl))
          allocate(MY_CUTS%shape_zfl(MY_CUTS%nfl))
          allocate(gl_work1d(nbz))
          !
          do k = 1,nbz
             gl_work1d(k) = MY_GRID%gl_sigma(k)*MY_GRID%X3max
          end do
          do k = 1,MY_CUTS%nfl
             call grid_get_shapez(1,nbz,gl_work1d,MY_CUTS%zfl_cut(k), &
                  MY_CUTS%index_zfl(k),MY_CUTS%shape_zfl(k))
             if((MY_CUTS%index_zfl(k).eq.1).or.(MY_CUTS%index_zfl(k).eq.nbz-1)) &
                  call task_wriwarn(MY_ERR,'FL not found')
          end do
          deallocate(gl_work1d)
       end if
       !
    end if
    !
    !*** Scale back my_tracer concentration at mass points and convert units
    !
    allocate(my_cs(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h,1:nbins))
    !
    do ib = 1,nbins
       do k = my_kps_2h,my_kpe_2h
          do j = my_jps_2h,my_jpe_2h
             jj = max(my_jps,min(j,my_jpe))
             Hm1 = MY_GRID%Hm1_p(jj)
             Hm2 = MY_GRID%Hm2_p(jj)
             do i = my_ips_2h,my_ipe_2h
                ii = max(my_ips,min(i,my_ipe))
                Hm3 = MY_GRID%Hm3_p (ii,jj)
                !
                my_cs(i,j,k,ib) = 1e3_rp*MY_TRA%my_c(i,j,k,ib)/(Hm1*Hm2*Hm3)   ! scale back and convert from kg/m3 to gr/m3
                my_cs(i,j,k,ib) = max(0.0_rp,my_cs(i,j,k,ib))                  ! clip negative values
                !
             end do
          end do
       end do
    end do
    !
    !*** Get values of my tracer concentration at cell corner points (my_cc in gr/m3)
    !
    call domain_swap_mass_points_2halo_x (my_cs)
    call domain_swap_mass_points_2halo_y (my_cs)
    call domain_swap_mass_points_2halo_z (my_cs)
    !
    allocate(my_cc(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,1:nbins))
    do ib = 1,nbins
       call Grid_p2c(my_cs(:,:,:,ib),my_cc(:,:,:,ib))
    end do
    deallocate(my_cs)
    !
    !*** Get ground accumulation at corner points (my_ac in kg/m2)
    !
    allocate(my_ac(my_ibs:my_ibe,my_jbs:my_jbe,1:nbins))
    do ib = 1,nbins
       call Grid_p2c_2D(MY_TRA%my_acum(:,:,ib),my_ac(:,:,ib))
    end do
    !
    !*** Get wet deposition at corner points (my_wetc in kg/m2)
    !
    call domain_swap_mass_points_2halo_2Dx ( MY_TRA%my_awet )
    call domain_swap_mass_points_2halo_2Dx ( MY_TRA%my_awet )
    !
    allocate(my_awetc(my_ibs:my_ibe,my_jbs:my_jbe))
    call Grid_p2c_2D(MY_TRA%my_awet(:,:),my_awetc(:,:))
    !
    !*** If necessary, write the tracking points files
    !
    if(MY_OUT%track_points) call nc_IO_out_pts(MY_FILES,MY_TIME,MY_SPE,MY_OUT,MY_TRA%MY_BIN,my_cc,my_ac,MY_ERR)
    !
    !*** Open file
    !
#if defined WITH_MPI
    if(PARALLEL_IO) then
       mode_flag = IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL)
       mode_flag = IOR(mode_flag, NF90_MPIIO)
       mode_flag = IOR(mode_flag, NF90_WRITE)
       istat     = nf90_open_par(TRIM(nc_file), mode_flag , comm=COMM_WORLD, info = MPI_INFO_NULL, ncid=ncID)
    else if(master) then
       mode_flag = NF90_WRITE
       istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
    end if
#else
    mode_flag = NF90_WRITE
    istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
#endif
    !
    !*** Print variable time (in s). Note that for unlimited dimensions all parallel IO must be collective
    !
    if(PARALLEL_IO) then
       istat = nf90_inq_varid(ncID,tim_nc_name,tim_ncID)
       istat = nf90_var_par_access(ncID, tim_ncID, access=NF90_COLLECTIVE)
       istat = nf90_put_var(ncID,tim_ncID,INT(MY_TIME%time),start=(/ipass/))
    else if(master) then
       istat = nf90_inq_varid(ncID,tim_nc_name,tim_ncID)
       istat = nf90_put_var(ncID,tim_ncID,INT(MY_TIME%time),start=(/ipass/))
    end if
    !
    !*** Compute and print variable date
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month,MY_TIME%start_day,0, &
         iyr,imo,idy,ihr,imi,ise,MY_TIME%time,MY_ERR)
    !
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,2_ip,out_date,MY_ERR)
    !
    if(PARALLEL_IO) then
       istat = nf90_inq_varid(ncID,date_nc_name,date_ncID)
       istat = nf90_var_par_access(ncID, date_ncID, access=NF90_COLLECTIVE)
       istat = nf90_put_var(ncID,date_ncID,out_date,start=(/1,ipass/))
    else if(master) then
       istat = nf90_inq_varid(ncID,date_nc_name,date_ncID)
       istat = nf90_put_var(ncID,date_ncID,out_date,start=(/1,ipass/))
    end if
    !
    ! Time-dependent variables depending on each specie
    !
    do ispe = 1,MY_SPE%nspe
       spe_code = MY_SPE%code    (ispe)
       cat_code = MY_SPE%category(ispe)
       sblock   = SPE_TAG        (spe_code)
       !
       !*** 1. CON : compute and output specie 3D concentration in gr/m3 on sigma levels at corner points
       !
       if(out_con_total) then
         !
         allocate(my_work3d(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe))
         my_work3d = 0.0_rp
         do ib = 1,nbins
            if(bin_spe(ib).eq.spe_code) my_work3d(:,:,:) = my_work3d(:,:,:) + my_cc(:,:,:,ib)
         end do
         !
         if(PARALLEL_IO) then
           !
           my_nbx = my_ibe-my_ibs+1
           my_nby = my_jbe-my_jbs+1
           my_nbz = my_kbe-my_kbs+1
           !
           start4d=(/my_ibs,my_jbs,my_kbs,ipass/)
           count4d=(/my_nbx,my_nby,my_nbz,1/)
           !
           allocate(gl_work3d(my_nbx,my_nby,my_nbz))
           gl_work3d(1:my_nbx,1:my_nby,1:my_nbz) = my_work3d(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe)
           !
           name_nc = TRIM(sblock)//TRIM(c_total_nc_name)
           istat = nf90_inq_varid(ncID,name_nc,con_ncID)
           istat = nf90_var_par_access(ncID, con_ncID, access=NF90_COLLECTIVE)
           istat = nf90_put_var(ncID, con_ncID, gl_work3d, start=start4d,count=count4d)
           !
           deallocate(gl_work3d)
           !
         else   ! SERIAL_IO
           !
           if(master) then
              allocate(gl_work3d(nbx,nby,nbz))
           else
              allocate(gl_work3d(1,1,1))
           end if
           call domain_gather_corner_points_0halo(gl_work3d,nbx,nby,nbz,my_work3d)
           !
           if(master) then
             !
             start4d=(/1,1,1,ipass/)
             count4d=(/nbx,nby,nbz,1/)
             !
             name_nc = TRIM(sblock)//TRIM(c_total_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,con_ncID)
             istat = nf90_put_var(ncID, con_ncID, gl_work3d, start=start4d,count=count4d)
             !
           end if
           deallocate(gl_work3d)
           !
         end if
         !
         deallocate(my_work3d)
         !
       end if ! if(out_con_total)
       !
       !***  2. CON_BIN : output 3D specie bin concentration in gr/m3  on sigma levels at corner points
       !
       if(out_con_bins) then
         !
         if(PARALLEL_IO) then
          !
          my_nbx = my_ibe-my_ibs+1
          my_nby = my_jbe-my_jbs+1
          my_nbz = my_kbe-my_kbs+1
          !
          allocate(gl_work4d(my_nbx,my_nby,my_nbz,nbins_spe(spe_code)))
          jb = 0
          do ib = 1,nbins
             if(bin_spe(ib).eq.spe_code) then
                jb = jb + 1
                gl_work4d(1:my_nbx,1:my_nby,1:my_nbz,jb) = my_cc(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,ib)
             end if
          end do
          !
          nb_spe = nbins_spe(spe_code)
          start5d=(/my_ibs,my_jbs,my_kbs,1     ,ipass/)
          count5d=(/my_nbx,my_nby,my_nbz,nb_spe,1    /)
          !
          name_nc = TRIM(sblock)//TRIM(c_bin_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,con_bin_ncID)
          istat = nf90_var_par_access(ncID, con_bin_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, con_bin_ncID, gl_work4d, start=start5d,count=count5d)
          !
          deallocate(gl_work4d)
          !
       else   ! SERIAL_IO
          !
          if(master) then
             allocate(gl_work4d(nbx,nby,nbz,nbins_spe(spe_code)))
          else
             allocate(gl_work4d(1,1,1,1))
          end if
          !
          jb = 0
          do ib = 1,nbins
             if(bin_spe(ib).eq.spe_code) then
                jb = jb + 1
                call domain_gather_corner_points_0halo(gl_work4d(:,:,:,jb),nbx,nby,nbz,my_cc(:,:,:,ib))
             end if
          end do
          !
          if(master) then
             !
             nb_spe = nbins_spe(spe_code)
             start5d=(/1,1,1,1,ipass/)
             count5d=(/nbx,nby,nbz,nb_spe,1/)
             !
             name_nc = TRIM(sblock)//TRIM(c_bin_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,con_bin_ncID)
             istat = nf90_put_var(ncID, con_bin_ncID, gl_work4d, start=start5d,count=count5d)
          end if
          deallocate(gl_work4d)
          !
        end if
        !
      end if   ! if(out_con_bin)
      !
      !***  7. COL_MASS : compute and output specie column mass load in gr/m2  or DU at corner points
      !
      if(out_col_load) then
         !
         allocate(my_work2d(my_ibs:my_ibe,my_jbs:my_jbe))
         allocate(my_work3d(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe))
         my_work3d = 0.0_rp
         do ib = 1,nbins
            if(bin_spe(ib).eq.spe_code) my_work3d(:,:,:) = my_work3d(:,:,:) + my_cc(:,:,:,ib)
         end do
         !
         my_work2d = 0.0_rp
         do k = my_kbs,my_kbe-1
            my_work2d(:,:) = my_work2d(:,:) + &
                     0.5_rp*(my_work3d(:,:,k)+my_work3d(:,:,k+1))*(MY_GRID%z_c(:,:,k+1)-MY_GRID%z_c(:,:,k))
         end do
         !
         call parallel_sum(my_work2d,COMM_GRIDZ)  ! only along z
         !
         !  if necessary, convert from gr/m2 to DU. Molecular mass = 64 gr/mol
         !
         if(spe_code.eq.SPE_SO2) my_work2d(:,:) = my_work2d(:,:)/64.0_rp*2.238e3_rp  ! Avogadro/1DU = 2.238e3_rp
         !
         if(PARALLEL_IO) then
           !
           my_nbx = my_ibe-my_ibs+1
           my_nby = my_jbe-my_jbs+1
           !
           start3d=(/my_ibs,my_jbs,ipass/)
           count3d=(/my_nbx,my_nby,1/)
           !
           allocate(gl_work2d(my_nbx,my_nby))
           gl_work2d(1:my_nbx,1:my_nby) = my_work2d(my_ibs:my_ibe,my_jbs:my_jbe)
           !
           name_nc = TRIM(sblock)//TRIM(col_nc_name)
           istat = nf90_inq_varid(ncID,name_nc,col_ncID)
           istat = nf90_var_par_access(ncID, col_ncID, access=NF90_COLLECTIVE)
           istat = nf90_put_var(ncID, col_ncID, gl_work2d, start=start3d,count=count3d)
           !
           deallocate(gl_work2d)
           !
        else   ! SERIAL_IO
           !
           if(master) then
             allocate(gl_work2d(nbx,nby))
           else
             allocate(gl_work2d(1,1))
           end if
           call domain_gather_corner_points_0halo_2D(gl_work2d,nbx,nby,my_work2d)
           !
           if(master) then
             !
             start3d=(/1,1,ipass/)
             count3d=(/nbx,nby,1/)
             !
             name_nc = TRIM(sblock)//TRIM(col_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,col_ncID)
             istat = nf90_put_var(ncID, col_ncID, gl_work2d, start=start3d,count=count3d)
             !
           end if
           deallocate(gl_work2d)
        end if
        !
        deallocate(my_work3d)
        deallocate(my_work2d)
        !
       end if ! if(out_col_load)
       !
       !***  8. COL_MASS_PM : compute and output specie PM column mass load in gr/m2 at corner points
       !
       if(out_col_load.and.(cat_code.ne.CAT_AEROSOL)) then
         !
         allocate(my_work2d (my_ibs:my_ibe,my_jbs:my_jbe))
         allocate(my_work3d (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe))
         allocate(my_work3d2(my_ibs:my_ibe,my_jbs:my_jbe,1:npm))
         !
         do ipm = 1,npm
          !
          my_work3d = 0.0_rp
          do ib = 1,nbins
            if(bin_spe(ib).eq.spe_code) then
               if( MY_TRA%MY_BIN%bin_diam(ib) .le. (pm_value(ipm)*1d-6) ) &  ! particles < PM(ipm) (convert to m)
                    my_work3d(:,:,:) = my_work3d(:,:,:) + my_cc(:,:,:,ib)
            end if
          end do
          !
          my_work2d = 0.0_rp
          do k = my_kbs,my_kbe-1
             my_work2d(:,:) = my_work2d(:,:) + 0.5_rp*(my_work3d(:,:,k)+my_work3d(:,:,k+1))* &
                  (MY_GRID%z_c(:,:,k+1)-MY_GRID%z_c(:,:,k))
          end do
          !
          call parallel_sum(my_work2d,COMM_GRIDZ)  ! only along z
          !
          my_work3d2(:,:,ipm) = my_work2d(:,:)
        end do
        !
        if(PARALLEL_IO) then
          !
          my_nbx = my_ibe-my_ibs+1
          my_nby = my_jbe-my_jbs+1
          !
          start4d=(/my_ibs,my_jbs,1,ipass/)
          count4d=(/my_nbx,my_nby,npm,1/)
          !
          allocate(gl_work3d(my_nbx,my_nby,npm))
          gl_work3d(1:my_nbx,1:my_nby,1:npm) = my_work3d2(my_ibs:my_ibe,my_jbs:my_jbe,1:npm)
          !
          name_nc = TRIM(sblock)//TRIM(pmc_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,pmc_ncID)
          istat = nf90_var_par_access(ncID, pmc_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, pmc_ncID, gl_work3d, start=start4d,count=count4d)
          !
          deallocate(gl_work3d)
          !
         else   ! SERIAL_IO
          !
          if(master) then
             allocate(gl_work3d(nbx,nby,npm))
          else
             allocate(gl_work3d(1,1,1))
          end if
          do ipm = 1,npm
             call domain_gather_corner_points_0halo_2D(gl_work3d(:,:,ipm),nbx,nby,my_work3d2(:,:,ipm))
          end do
          !
          if(master) then
             !
             start4d=(/1,1,1,ipass/)
             count4d=(/nbx,nby,npm,1/)
             !
             name_nc = TRIM(sblock)//TRIM(pmc_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,pmc_ncID)
             istat = nf90_put_var(ncID, pmc_ncID, gl_work3d, start=start4d,count=count4d)
             !
          end if
          deallocate(gl_work3d)
        end if
        !
        deallocate(my_work3d)
        deallocate(my_work3d2)
        deallocate(my_work2d)
        !
      end if !  if(out_col_load.and.(cat_code.ne.CAT_AEROSOL))
      !
      !*** 9. CLOUD_TOP : compute and output species cloud top height (in m) assuming a concentration threshold of 50 ug/m3
      !
      if(out_cloud_top) then
       !
       allocate(my_work2d(my_ibs:my_ibe,my_jbs:my_jbe))
       allocate(my_work3d(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe))
       my_work3d = 0.0_rp
       do ib = 1,nbins
           if(bin_spe(ib).eq.spe_code) my_work3d(:,:,:) = my_work3d(:,:,:) + my_cc(:,:,:,ib) ! (my_cc in gr/m3)
       end do
       !
       my_work2d = 0.0_rp
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             !
             !   at each point of my slice, compute maximum height (if any)
             !
             found = .false.
             do k = my_kbe,my_kbs,-1
                if(.not.found) then
                   if(my_work3d(i,j,k).gt.50e-6_rp) then
                      found = .true.
                      my_work2d(i,j) = MY_GRID%z_c(i,j,k)
                   end if
                end if
             end do
          end do
       end do
       !
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             my_max = my_work2d(i,j)
             call parallel_max(my_max, my_work2d(i,j), COMM_GRIDZ)
          end do
       end do
       !
       if(PARALLEL_IO) then
          !
          my_nbx = my_ibe-my_ibs+1
          my_nby = my_jbe-my_jbs+1
          !
          start3d=(/my_ibs,my_jbs,ipass/)
          count3d=(/my_nbx,my_nby,1/)
          !
          allocate(gl_work2d(my_nbx,my_nby))
          gl_work2d(1:my_nbx,1:my_nby) = my_work2d(my_ibs:my_ibe,my_jbs:my_jbe)
          !
          name_nc = TRIM(sblock)//TRIM(clh_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,clh_ncID)
          istat = nf90_var_par_access(ncID, clh_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, clh_ncID, gl_work2d, start=start3d,count=count3d)
          !
          deallocate(gl_work2d)
          !
        else   ! SERIAL_IO
          !
          if(master) then
             allocate(gl_work2d(nbx,nby))
          else
             allocate(gl_work2d(1,1))
          end if
          call domain_gather_corner_points_0halo_2D(gl_work2d,nbx,nby,my_work2d)
          !
          if(master) then
             !
             start3d=(/1,1,ipass/)
             count3d=(/nbx,nby,1/)
             !
             name_nc = TRIM(sblock)//TRIM(clh_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,clh_ncID)
             istat = nf90_put_var(ncID, clh_ncID, gl_work2d, start=start3d,count=count3d)
             !
          end if
          deallocate(gl_work2d)
        end if
        !
        deallocate(my_work3d)
        deallocate(my_work2d)
        !
      end if ! if(out_cloud_top)
      !
      !*** 10. GRN_LOAD : compute and output species total accumulation at ground (all species bins)
      !
      if(out_grn_total) then
        !
        allocate(my_work2d(my_ibs:my_ibe,my_jbs:my_jbe))
        my_work2d = 0.0_rp
        do ib = 1,nbins
           if(bin_spe(ib).eq.spe_code) my_work2d(:,:) = my_work2d(:,:) + my_ac(:,:,ib)
        end do
        !
        call parallel_sum(my_work2d,COMM_GRIDZ)  ! only along z
        !
        if(PARALLEL_IO) then
          !
          my_nbx = my_ibe-my_ibs+1
          my_nby = my_jbe-my_jbs+1
          !
          start3d=(/my_ibs,my_jbs,ipass/)
          count3d=(/my_nbx,my_nby,1/)
          !
          allocate(gl_work2d(my_nbx,my_nby))
          gl_work2d(1:my_nbx,1:my_nby) = my_work2d(my_ibs:my_ibe,my_jbs:my_jbe)
          !
          name_nc = TRIM(sblock)//TRIM(grn_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,grn_ncID)
          istat = nf90_var_par_access(ncID, grn_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, grn_ncID, gl_work2d, start=start3d,count=count3d)
          !
          deallocate(gl_work2d)
          !
        else   ! SERIAL_IO
          !
          if(master) then
             allocate(gl_work2d(nbx,nby))
          else
             allocate(gl_work2d(1,1))
          end if
          call domain_gather_corner_points_0halo_2D(gl_work2d,nbx,nby,my_work2d)
          !
          if(master) then
             !
             start3d=(/1,1,ipass/)
             count3d=(/nbx,nby,1/)
             !
             name_nc = TRIM(sblock)//TRIM(grn_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,grn_ncID)
             istat = nf90_put_var(ncID, grn_ncID, gl_work2d, start=start3d,count=count3d)
             !
           end if
           deallocate(gl_work2d)
         end if
         !
        deallocate(my_work2d)
        !
      end if ! if(out_grn_load)
      !
      !*** 11. GRN_LOAD_BIN : compute and output species bin accumulation at ground
      !
      if(out_grn_bins) then
        !
        allocate(my_work3d(my_ibs:my_ibe,my_jbs:my_jbe,nbins_spe(spe_code)))
        jb = 0
        do ib = 1,nbins
           if(bin_spe(ib).eq.spe_code) then
              jb = jb + 1
              my_work3d(:,:,jb) = my_ac(:,:,ib)
              call parallel_sum(my_work3d(:,:,jb),COMM_GRIDZ)  ! only along z
           end if
        end do
        !
        if(PARALLEL_IO) then
          !
          my_nbx = my_ibe-my_ibs+1
          my_nby = my_jbe-my_jbs+1
          !
          start4d=(/my_ibs,my_jbs,1,ipass/)
          count4d=(/my_nbx,my_nby,nbins_spe(spe_code),1/)
          !
          allocate(gl_work3d(my_nbx,my_nby,nbins_spe(spe_code)))
          gl_work3d(1:my_nbx,1:my_nby,:) = my_work3d(my_ibs:my_ibe,my_jbs:my_jbe,:)
          !
          name_nc = TRIM(sblock)//TRIM(grn_bin_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,grn_bin_ncID)
          istat = nf90_var_par_access(ncID, grn_bin_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, grn_bin_ncID, gl_work3d, start=start4d,count=count4d)
          !
          deallocate(gl_work3d)
          !
         else   ! SERIAL_IO
          !
          if(master) then
             allocate(gl_work3d(nbx,nby,nbins_spe(spe_code)))
          else
             allocate(gl_work3d(1,1,1))
          end if
          jb = 0
          do ib = 1,nbins
             if(bin_spe(ib).eq.spe_code) then
                jb = jb + 1
                call domain_gather_corner_points_0halo_2D(gl_work3d(:,:,jb),nbx,nby,my_work3d(:,:,jb))
             end if
          end do
          !
          if(master) then
             !
             start4d=(/1,1,1,ipass/)
             count4d=(/nbx,nby,nbins_spe(spe_code),1/)
             !
             name_nc = TRIM(sblock)//TRIM(grn_bin_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,grn_bin_ncID)
             istat = nf90_put_var(ncID, grn_bin_ncID, gl_work3d, start=start4d,count=count4d)
             !
          end if
          deallocate(gl_work3d)
       end if
       !
       deallocate(my_work3d)
       !
      end if ! if(out_grn_bin_load)
      !
      !*** 12. WET : compute and output total species mass removed by wet deposition (all species bins)
      !
      if(out_wet_total) then
       !
       call parallel_sum(my_awetc,COMM_GRIDZ)  ! only along z
       !
       if(PARALLEL_IO) then
          !
          my_nbx = my_ibe-my_ibs+1
          my_nby = my_jbe-my_jbs+1
          !
          start3d=(/my_ibs,my_jbs,ipass/)
          count3d=(/my_nbx,my_nby,1/)
          !
          allocate(gl_work2d(my_nbx,my_nby))
          gl_work2d(1:my_nbx,1:my_nby) = my_awetc(my_ibs:my_ibe,my_jbs:my_jbe)
          !
          name_nc = TRIM(sblock)//TRIM(wet_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,wet_ncID)
          istat = nf90_var_par_access(ncID, wet_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, wet_ncID, gl_work2d, start=start3d,count=count3d)
          !
          deallocate(gl_work2d)
          !
       else   ! SERIAL_IO
          !
          if(master) then
             allocate(gl_work2d(nbx,nby))
          else
             allocate(gl_work2d(1,1))
          end if
          call domain_gather_corner_points_0halo_2D(gl_work2d,nbx,nby,my_awetc)
          !
          if(master) then
             !
             start3d=(/1,1,ipass/)
             count3d=(/nbx,nby,1/)
             !
             name_nc = TRIM(sblock)//TRIM(wet_nc_name)
             istat = nf90_inq_varid(ncID,name_nc,wet_ncID)
             istat = nf90_put_var(ncID, wet_ncID, gl_work2d, start=start3d,count=count3d)
             !
          end if
          deallocate(gl_work2d)
       end if
       !
     end if ! if(out_wet_total)
     !
     !*** If necessary, compute total 3D particle concentration on the postprocess mesh (i.e. on z levels a.s.l.) to extract cuts
     !
     if( ((MY_CUTS%ncutx.gt.0).or.(MY_CUTS%ncuty.gt.0).or.(MY_CUTS%ncutz.gt.0).or.(MY_CUTS%nfl.gt.0)) ) then
       !
       allocate(my_work3d(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe))
       my_work3d = 0.0_rp
       do ib = 1,nbins
          if(bin_spe(ib).eq.spe_code) my_work3d(:,:,:) = my_work3d(:,:,:) + my_cc(:,:,:,ib)
       end do
       !
       !  Get global c on sigma levels
       !
       if(master) then
          allocate(gl_c(nbx,nby,nbz))
       else
          allocate(gl_c(1,1,1))
       end if
       call domain_gather_corner_points_0halo(gl_c,nbx,nby,nbz,my_work3d)
       deallocate(my_work3d)
       !
       !  Get global c on z levels (master only)
       !
       if(master) then
          allocate(gl_cz(nbx,nby,nbz))
          do k = 1,nbz
             do j = 1,nby
                do i = 1,nbx
                   kk = indexz(i,j,k)
                   if(kk.gt.0) then
                      gl_cz(i,j,k) = (1.0_rp-shapez(i,j,k))*gl_c(i,j,kk) + shapez(i,j,k)*gl_c(i,j,kk+1)
                   else
                      gl_cz(i,j,k) = 0.0_rp
                   end if
                end do
             end do
          end do
       end if
       !
       deallocate(gl_c)
    end if
    !
    !*** Compute and output cuts along x (master only)
    !
    if((MY_CUTS%ncutx.gt.0)) then
       if(master) then
          !
          allocate(gl_work3d(nby,nbz,MY_CUTS%ncutx))
          !
          do i = 1,MY_CUTS%ncutx
             ii = MY_CUTS%index_x(i)
             do k = 1,nbz
                do j = 1,nby
                   gl_work3d(j,k,i) = (1.0_rp-MY_CUTS%shape_x(i))*gl_cz(ii,  j,k) + &
                        MY_CUTS%shape_x(i) *gl_cz(ii+1,j,k)
                end do
             end do
          end do
       else
          allocate(gl_work3d(1,1,1))
       end if
       !
       if(PARALLEL_IO) then
          !
          start4d=(/1,1,1,ipass/)
          if(master) then
             count4d=(/nby,nbz,MY_CUTS%ncutx,1/)  ! only master has values
          else
             count4d=(/0,0,0,0/)
          end if
          !
          name_nc = TRIM(sblock)//TRIM(cutx_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,cutx_ncID)
          istat = nf90_var_par_access(ncID, cutx_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, cutx_ncID, gl_work3d, start=start4d,count=count4d)
          !
       else if(master) then
          !
          start4d=(/1,1,1,ipass/)
          count4d=(/nby,nbz,MY_CUTS%ncutx,1/)
          !
          name_nc = TRIM(sblock)//TRIM(cutx_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,cutx_ncID)
          istat = nf90_put_var(ncID, cutx_ncID, gl_work3d, start=start4d,count=count4d)
          !
       end if
       !
       deallocate(gl_work3d)
       !
    end if  ! if(MY_CUTS%ncutx.gt.0)
    !
    !*** Compute and output cuts along y (master only)
    !
    if((MY_CUTS%ncuty.gt.0)) then
       if(master) then
          !
          allocate(gl_work3d(nbx,nbz,MY_CUTS%ncuty))
          !
          do j = 1,MY_CUTS%ncuty
             jj = MY_CUTS%index_y(j)
             do k = 1,nbz
                do i = 1,nbx
                   gl_work3d(i,k,j) = (1.0_rp-MY_CUTS%shape_y(j))*gl_cz(i,jj  ,k) + &
                        MY_CUTS%shape_y(j) *gl_cz(i,jj+1,k)
                end do
             end do
          end do
       else
          allocate(gl_work3d(1,1,1))
       end if
       !
       if(PARALLEL_IO) then
          !
          start4d=(/1,1,1,ipass/)
          if(master) then
             count4d=(/nbx,nbz,MY_CUTS%ncuty,1/)  ! only master has values
          else
             count4d=(/0,0,0,0/)
          end if
          !
          name_nc = TRIM(sblock)//TRIM(cuty_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,cuty_ncID)
          istat = nf90_var_par_access(ncID, cuty_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, cuty_ncID, gl_work3d, start=start4d,count=count4d)
          !
       else if(master) then
          !
          start4d=(/1,1,1,ipass/)
          count4d=(/nbx,nbz,MY_CUTS%ncuty,1/)
          !
          name_nc = TRIM(sblock)//TRIM(cuty_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,cuty_ncID)
          istat = nf90_put_var(ncID, cuty_ncID, gl_work3d, start=start4d,count=count4d)
          !
       end if
       deallocate(gl_work3d)
       !
    end if   ! if(MY_CUTS%ncuty.gt.0)
    !
    !*** Compute and output cuts along z (master only)
    !
    if((MY_CUTS%ncutz.gt.0)) then
       if(master) then
          !
          allocate(gl_work3d(nbx,nby,MY_CUTS%ncutz))
          !
          do k = 1,MY_CUTS%ncutz
             kk = MY_CUTS%index_z(k)
             do j = 1,nby
                do i = 1,nbx
                   gl_work3d(i,j,k) = (1.0_rp-MY_CUTS%shape_z(k))*gl_cz(i,j,kk  ) + &
                        MY_CUTS%shape_z(k) *gl_cz(i,j,kk+1)
                end do
             end do
          end do
       else
          allocate(gl_work3d(1,1,1))
       end if
       !
       if(PARALLEL_IO) then
          !
          start4d=(/1,1,1,ipass/)
          if(master) then
             count4d=(/nbx,nby,MY_CUTS%ncutz,1/) ! only master has values
          else
             count4d=(/0,0,0,0/)
          end if
          !
          name_nc = TRIM(sblock)//TRIM(cutz_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,cutz_ncID)
          istat = nf90_var_par_access(ncID, cutz_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, cutz_ncID, gl_work3d, start=start4d,count=count4d)
          !
       else if(master) then
          !
          start4d=(/1,1,1,ipass/)
          count4d=(/nbx,nby,MY_CUTS%ncutz,1/)
          !
          name_nc = TRIM(sblock)//TRIM(cutz_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,cutz_ncID)
          istat = nf90_put_var(ncID, cutz_ncID, gl_work3d, start=start4d,count=count4d)
          !
       end if
       deallocate(gl_work3d)
       !
    end if  ! if(MY_CUTS%ncutz.gt.0)
    !
    !*** Compute and output cuts along FLs (master only)
    !
    if((MY_CUTS%nfl.gt.0)) then
       if(master) then
          !
          allocate(gl_work3d(nbx,nby,MY_CUTS%nfl))
          !
          do k = 1,MY_CUTS%nfl
             kk = MY_CUTS%index_zfl(k)
             do j = 1,nby
                do i = 1,nbx
                   gl_work3d(i,j,k) = (1.0_rp-MY_CUTS%shape_zfl(k))*gl_cz(i,j,kk  ) + &
                        MY_CUTS%shape_zfl(k) *gl_cz(i,j,kk+1)
                end do
             end do
          end do
       else
          allocate(gl_work3d(1,1,1))
       end if
       !
       if(PARALLEL_IO) then
          !
          start4d=(/1,1,1,ipass/)
          if(master) then
             count4d=(/nbx,nby,MY_CUTS%nfl,1/) ! only master has values
          else
             count4d=(/0,0,0,0/)
          end if
          !
          name_nc = TRIM(sblock)//TRIM(fl_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,fl_ncID)
          istat = nf90_var_par_access(ncID, fl_ncID, access=NF90_COLLECTIVE)
          istat = nf90_put_var(ncID, fl_ncID, gl_work3d, start=start4d,count=count4d)
          !
       else if(master) then
          !
          start4d=(/1,1,1,ipass/)
          count4d=(/nbx,nby,MY_CUTS%nfl,1/)
          !
          name_nc = TRIM(sblock)//TRIM(fl_nc_name)
          istat = nf90_inq_varid(ncID,name_nc,fl_ncID)
          istat = nf90_put_var(ncID, fl_ncID, gl_work3d, start=start4d,count=count4d)
          !
       end if
       deallocate(gl_work3d)
       !
     end if  ! if(MY_CUTS%nfl.gt.0)
     !
     !*** If necessary, master deallocates for next species
     !
     if( ((MY_CUTS%ncutx.gt.0).or.(MY_CUTS%ncuty.gt.0).or.(MY_CUTS%ncutz.gt.0).or.(MY_CUTS%nfl.gt.0)) ) then
        if(master) deallocate(gl_cz)
     end if
     !
     !
    end do   ! loop over all species
    !
    !*** Close file
    !
    if(PARALLEL_IO.or.master) then
       istat = nf90_close(ncID)
    end if
    !
    return
  end subroutine nc_IO_out_res
  !
  !>   @dbrief
  !>   Writes the tracking point files
  !
  subroutine nc_IO_out_pts(MY_FILES,MY_TIME,MY_SPE,MY_OUT,MY_BIN,my_cc,my_ac,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_SPE    list of parameters defining species and categories
    !>   @param MY_OUT    model output and postprocess related parameters
    !>   @param MY_BIN    list of parameters defining bin granulometric properties
    !>   @param my_cc     my tracer concentration at cell corner points (my_cc   in gr/m3)
    !>   @param my_ac     my  ground accumulation at corner points      (my_acum in kg/m2)
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(RUN_TIME),       intent(IN   ) :: MY_TIME
    type(SPECIES_PARAMS), intent(IN   ) :: MY_SPE
    type(MODEL_OUTPUT),   intent(IN   ) :: MY_OUT
    type(BIN_PARAMS),     intent(IN   ) :: MY_BIN
    real(rp),             intent(IN   ) :: my_cc(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_BIN%nbins)
    real(rp),             intent(IN   ) :: my_ac(my_ibs:my_ibe,my_jbs:my_jbe,              MY_BIN%nbins)
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=24)     :: time_str
    character(len=s_file) :: fname
    character(len=s_name) :: spe_name
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: nbins,npts,ipts,ibin,i,j,k
    integer(ip)           :: ispe, spe_code, jb
    integer(ip), save     :: ipass = 0
    real(rp)              :: time,s,t,w,st,val(4)
    real(rp)              :: acum,ctot,pm05,pm10,pm20
    !
    real(rp), allocatable :: my_cp(:)         ! concentration at point
    real(rp), allocatable :: my_ap(:)         ! accumulation  at point
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_out_pts'
    MY_ERR%message = ' '
    !
    nbins = MY_BIN%nbins
    allocate(my_cp(nbins))
    allocate(my_ap(nbins))
    !
    !*** Current time
    !
    time = MY_TIME%time
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,time,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,2_ip, time_str, MY_ERR)
    !
    !*** Loop over species and points
    !
    npts = MY_OUT%MY_PTS%npts
    !
    do ispe = 1,MY_SPE%nspe
       !
       spe_code = MY_SPE%code(ispe)
       spe_name = MY_SPE%name(ispe)
       !
       do ipts = 1,npts
       !
       fname = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.'// &
               TRIM(MY_OUT%MY_PTS%name_pts(ipts))//'.'//TRIM(spe_name)//'.res'
       !
       !*** The processor hosting the point interpolates
       !
       if(mpime.eq.MY_OUT%MY_PTS%mproc(ipts)) then
          i = MY_OUT%MY_PTS%ipts(ipts)
          j = MY_OUT%MY_PTS%jpts(ipts)
          k = MY_OUT%MY_PTS%kpts(ipts)
          s = MY_OUT%MY_PTS%spts(ipts)
          t = MY_OUT%MY_PTS%tpts(ipts)
          w = MY_OUT%MY_PTS%wpts(ipts)
          st     = s*t
          val(1) = (1.0_rp-t-s+st)*0.25_rp                   !  4      3
          val(2) = (1.0_rp-t+s-st)*0.25_rp                   !
          val(3) = (1.0_rp+t+s+st)*0.25_rp                   !
          val(4) = (1.0_rp+t-s-st)*0.25_rp                   !  1      2
          !
          jb = 0
          do ibin = 1,nbins
             if(MY_BIN%bin_spe(ibin).eq.spe_code) then
                jb = jb + 1
                my_cp(jb) = (val(1)*my_cc(i  ,j  ,k  ,ibin) + &
                             val(2)*my_cc(i+1,j  ,k  ,ibin) + &
                             val(3)*my_cc(i+1,j+1,k  ,ibin) + &
                             val(4)*my_cc(i  ,j+1,k  ,ibin) )*(1.0_rp-w) + &
                            (val(1)*my_cc(i  ,j  ,k+1,ibin) + &
                             val(2)*my_cc(i+1,j  ,k+1,ibin) + &
                             val(3)*my_cc(i+1,j+1,k+1,ibin) + &
                             val(4)*my_cc(i  ,j+1,k+1,ibin) )*w
                my_ap(jb) =  val(1)*my_ac(i  ,j  ,ibin) + &
                             val(2)*my_ac(i+1,j  ,ibin) + &
                             val(3)*my_ac(i+1,j+1,ibin) + &
                             val(4)*my_ac(i  ,j+1,ibin)
             end if
          end do
          !
        else
           my_cp(:) = 0.0_rp
           my_ap(:) = 0.0_rp
           !
        end if
        !
        call parallel_sum(my_cp, COMM_WORLD)
        call parallel_sum(my_ap, COMM_WORLD)
       !
       !*** Master prints the file
       !
       if(master) then
          !
          if(ipass.eq.0) then
             open(90,file=TRIM(fname),status='unknown')
             write(90,10) TRIM(MY_OUT%MY_PTS%name_pts(ipts)), &
                  MY_OUT%MY_PTS%xpts    (ipts),  &
                  MY_OUT%MY_PTS%ypts    (ipts)
10           format(&
                  'Tracking point file for : ',a              ,/, &
                  'Coordinates             : ',f13.4,1x,f13.4 ,/, &
                  '                          '                ,/, &
                  ' DDMMYY_HH:MM       load        conc.      conc.PM5      conc.PM10    conc.PM20 ',/, &
                  '                   ground       ground      ground        ground       ground   ',/, &
                  '   (--)            (kg/m2)      (g/m3)       (g/m3)       (g/m3)       (g/m3)   ',/, &
                  '--------------------------------------------------------------------------------')
          else
             open(90,file=TRIM(fname),status='old',position='append')
          end if
          !
          ctot = 0.0_rp
          acum = 0.0_rp
          pm05 = 0.0_rp
          pm10 = 0.0_rp
          pm20 = 0.0_rp
          !
          jb = 0
          do ibin = 1,nbins
             if(MY_BIN%bin_spe(ibin).eq.spe_code) then
                jb = jb + 1
                acum = acum + my_ap(jb)
                ctot = ctot + my_cp(jb)
                if( MY_BIN%bin_diam(ibin) .le. ( 5.0_rp*1d-6) ) pm05 = pm05 + my_cp(jb) ! particles < 5  um
                if( MY_BIN%bin_diam(ibin) .le. (10.0_rp*1d-6) ) pm10 = pm10 + my_cp(jb) ! particles < 10 um
                if( MY_BIN%bin_diam(ibin) .le. (20.0_rp*1d-6) ) pm20 = pm20 + my_cp(jb) ! particles < 20 um
             end if
          end do
          !
          write(90,20) TRIM(time_str),acum,ctot,pm05,pm10,pm20
20        format(a,100(1x,E15.6E3))
          !
          close(90)
       end if
       !
    end do
    !
    end do   ! do ispe = 1,MY_SPE%nspe
    !
    ipass = 1
    !
    return
  end subroutine nc_IO_out_pts
  !
  !---------------------------------
  !    subroutine nc_IO_out_rst
  !---------------------------------
  !
  !>   @brief
  !>   Outputs a restart file
  !
  subroutine nc_IO_out_rst(MY_FILES,MY_GRID,MY_TRA,MY_OUT,MY_TIME,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_GRID       ARAKAWA_C_GRID structure already filled
    !>   @param MY_TRA        TRACERS        structure already filled
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param MY_TIME       RUN_TIME       structure already filled
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(INOUT) :: MY_FILES
    type(ARAKAWA_C_GRID),  intent(IN   ) :: MY_GRID
    type(TRACERS),         intent(IN   ) :: MY_TRA
    type(MODEL_OUTPUT),    intent(IN   ) :: MY_OUT
    type(RUN_TIME),        intent(IN   ) :: MY_TIME
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    character(len=24)     :: time_str
    character(len=s_file) :: nc_file
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: mode_flag, istat, ibin
    integer(ip)           :: npx,   npy,   npz,  nbins
    integer(ip)           :: npx_2h,npy_2h,npz_2h
    integer(ip)           :: my_npx_2h,my_npy_2h,my_npz_2h
    integer(ip)           :: start3d(3), start4d(4)
    integer(ip)           :: count3d(3), count4d(4)
    real(rp)              :: time,gl_mass_ground,gl_mass_lateral,gl_mass_in,gl_mass_sink
    !
    real(rp), allocatable :: gl_work2d(:,:)
    real(rp), allocatable :: gl_work3d(:,:,:)
    real(rp), allocatable :: gl_work4d(:,:,:,:)
    real(rp), allocatable :: my_acum  (:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_out_rst'
    MY_ERR%message = ' '
    !
    PARALLEL_IO = MY_OUT%PARALLEL_IO
    !
    npx   = gl_npx
    npy   = gl_npy
    npz   = gl_npz
    !
    npx_2h = gl_npx + 4
    npy_2h = gl_npy + 4
    npz_2h = gl_npz + 4
    !
    nbins = MY_TRA%nbins
    !
    !*** Current time
    !
    time = MY_TIME%time
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day, 0,  &
         iyr,imo,idy,ihr,imi,ise,time,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,4_ip, time_str, MY_ERR)
    !
    !*** Retrive global mass fluxes
    !
    call grid_get_mass_sink      (gl_mass_sink,                  MY_TRA,MY_GRID,MY_ERR)
    call grid_get_mass_boundaries(gl_mass_ground,gl_mass_lateral,MY_TRA,MY_ERR)
    gl_mass_sink    = gl_mass_sink    + MY_TRA%rst_mass_sink
    gl_mass_ground  = gl_mass_ground  + MY_TRA%rst_mass_ground
    gl_mass_lateral = gl_mass_lateral + MY_TRA%rst_mass_lateral
    gl_mass_in      =                   MY_TRA%gl_mass_in
    !
    !*** File name
    !
    nc_file = TRIM(MY_FILES%problempath)//'/'//TRIM(MY_FILES%problemname)//'.'//TRIM(time_str)//'.rst.nc'
    !
    !*** Create file
    !
#if defined WITH_MPI
    if(PARALLEL_IO) then
       mode_flag = IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL)
       mode_flag = IOR(mode_flag, NF90_MPIIO)
       mode_flag = IOR(mode_flag, NF90_CLOBBER)
       istat     = nf90_create_par(TRIM(nc_file), cmode=mode_flag , comm=COMM_WORLD, info = MPI_INFO_NULL, ncid=ncID)
     else if(master) then
       mode_flag = IOR(NF90_CLOBBER,NF90_NETCDF4)
       istat     = nf90_create(TRIM(nc_file),cmode=mode_flag, ncid=ncID)
    end if
#else
    mode_flag = IOR(NF90_CLOBBER,NF90_NETCDF4)
    istat     = nf90_create(TRIM(nc_file),cmode=mode_flag, ncid=ncID)
#endif
    !
    !*** Check errors
    !
    if(PARALLEL_IO.and.istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'nc_IO_out_rst'
       MY_ERR%message = nf90_strerror(istat)
       return
    else
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%source  = 'nc_IO_out_rst'
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
    end if
    !
    !*** File dimensions
    !
    if(PARALLEL_IO.or.master) then
       !
       !  Define dimensions
       !
       istat = nf90_def_dim(ncID, nx_nc_name , npx   , nx_ncID  )
       istat = nf90_def_dim(ncID, ny_nc_name , npy   , ny_ncID  )
       istat = nf90_def_dim(ncID, nz_nc_name , npz   , nz_ncID  )
       istat = nf90_def_dim(ncID, nx2_nc_name, npx_2h, nx2_ncID )
       istat = nf90_def_dim(ncID, ny2_nc_name, npy_2h, ny2_ncID )
       istat = nf90_def_dim(ncID, nz2_nc_name, npz_2h, nz2_ncID )
       istat = nf90_def_dim(ncID, nb_nc_name , nbins , nb_ncID  )
       !
       !  Define variables
       !
       attr_desc  = 'scaled tracer concentration'
       attr_units = 'kg/m3'
       istat = nf90_def_var(ncID, c_total_nc_name ,NF90_MYTYPE, (/nx2_ncID,ny2_ncID,nz2_ncID,nb_ncID/), con_ncID)
       istat = nf90_put_att(ncID, con_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, con_ncID, 'units',       attr_units)
       !
       attr_desc  = 'scaled deposit load'
       attr_units = 'kg/m2'
       istat = nf90_def_var(ncID, grn_nc_name ,NF90_MYTYPE, (/nx2_ncID,ny2_ncID,nb_ncID/), grn_ncID)
       istat = nf90_put_att(ncID, grn_ncID, 'description', attr_desc)
       istat = nf90_put_att(ncID, grn_ncID, 'units',       attr_units)
       !
       !  Put global attributes
       !
       attr_title = 'FALL3D model restart file'
       istat = nf90_put_att(ncID, NF90_GLOBAL, 'TITLE', attr_title)
       !
       attr_title = VERSION
       istat = nf90_put_att(ncID, NF90_GLOBAL, 'CODE_VERSION', attr_title)
       !
       select case(MY_GRID%map_h)
       case(MAP_H_CARTESIAN)
          attr_title = 'cartesian'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_H', attr_title)
          !
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmin_name, MY_GRID%lonmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'XMAX', MY_GRID%lonmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DX',   MY_GRID%dlon)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'YMIN', MY_GRID%latmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'YMAX', MY_GRID%latmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DY',   MY_GRID%dlat)
          !
       case(MAP_H_SPHERICAL)
          attr_title = 'spherical'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_H', attr_title)
          !
          istat = nf90_put_att(ncID, NF90_GLOBAL, attr_lonmin_name, MY_GRID%lonmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'LONMAX', MY_GRID%lonmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DLON',   MY_GRID%dlon)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'LATMIN', MY_GRID%latmin)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'LATMAX', MY_GRID%latmax)
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'DLAT',   MY_GRID%dlat)
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          !
       end select
       !
       select case(MY_GRID%map_v)
       case(MAP_V_SIGMA_NO_DECAY)
          attr_title = 'sigma_no_decay'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_V', attr_title)
          !
       case(MAP_V_SIGMA_LINEAR_DECAY)
          attr_title = 'sigma_linear_decay'
          istat = nf90_put_att(ncID, NF90_GLOBAL, 'MAP_V', attr_title)
          !
       case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect vertical mapping'
          !
       end select
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_ztop_name, MY_GRID%X3max)
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_year_name , MY_TIME%start_year)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_month_name, MY_TIME%start_month)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_day_name  , MY_TIME%start_day)
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_run_start_name, MY_TIME%run_start)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_run_end_name  , MY_TIME%run_end)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_run_time_name , MY_TIME%time)
       !
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_mass_ground_name,  gl_mass_ground )
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_mass_lateral_name, gl_mass_lateral)
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_mass_in_name,      gl_mass_in     )
       istat = nf90_put_att(ncID, NF90_GLOBAL, attr_mass_sink_name,    gl_mass_sink   )
       !
       istat = nf90_enddef(ncID)
       !
    end if
    !
    if(.not.PARALLEL_IO) call parallel_bcast(istat,1,0)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Error defining variables'
       return
    end if
    !
    !*** Computes my_acum across z-processors
    !
    allocate(my_acum(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,nbins))
    my_acum(:,:,1:nbins) = MY_TRA%my_acum(:,:,1:nbins)
    !
    call parallel_sum(my_acum,COMM_GRIDZ)  ! only along z
    !
    !*** Write variables
    !
    if(PARALLEL_IO) then
       !
       !  PARALLEL IO
       !
       my_npx_2h = my_ipe_2h-my_ips_2h+1
       my_npy_2h = my_jpe_2h-my_jps_2h+1
       my_npz_2h = my_kpe_2h-my_kps_2h+1
       !
       !  CON
       !
       start4d=(/my_ips_2h+2,my_jps_2h+2,my_kps_2h+2,1    /) ! note the shift to span (1,gl_npx+4)
       count4d=(/my_npx_2h,my_npy_2h,my_npz_2h,nbins/)
       !
       istat = nf90_inq_varid(ncID,c_total_nc_name,con_ncID)
       istat = nf90_var_par_access(ncID, con_ncID, access=NF90_COLLECTIVE)
       istat = nf90_put_var(ncID, con_ncID, MY_TRA%my_c, start=start4d,count=count4d)
       !
       !  LOAD
       !
       start3d=(/my_ips_2h+2,my_jps_2h+2,1    /) ! note the shift to span (1,gl_npx+4)
       count3d=(/my_npx_2h,my_npy_2h,nbins/)
       !
       istat = nf90_inq_varid(ncID,grn_nc_name,grn_ncID)
       istat = nf90_var_par_access(ncID, grn_ncID, access=NF90_COLLECTIVE)
       istat = nf90_put_var(ncID, grn_ncID, my_acum, start=start3d,count=count3d)
       !
       istat = nf90_close(ncID)
       !
    else
       !
       !  SERIAL_IO
       !
       !  CON
       !
       if(master) then
          allocate(gl_work3d(-1:npx+2,-1:npy+2,-1:npz+2))
          allocate(gl_work4d(npx_2h,npy_2h,npz_2h,nbins))
       else
          allocate(gl_work3d(1,1,1))
       end if
       !
       do ibin = 1,nbins
          call domain_gather_mass_points_2halo(gl_work3d(:,:,:),npx,npy,npz,MY_TRA%my_c(:,:,:,ibin))
          if(master) gl_work4d(1:npx_2h,1:npy_2h,1:npz_2h,ibin) = gl_work3d(-1:npx+2,-1:npy+2,-1:npz+2)
       end do
       !
       if(master) then
          !
          start4d=(/1,1,1,1/)
          count4d=(/npx_2h,npy_2h,npz_2h,nbins/)
          !
          istat = nf90_inq_varid(ncID,c_total_nc_name,con_ncID)
          istat = nf90_put_var  (ncID,con_ncID, gl_work4d, start=start4d,count=count4d)
          !
          deallocate(gl_work4d)
       end if
       deallocate(gl_work3d)
       !
       !  LOAD
       !
       if(master) then
          allocate(gl_work2d(-1:npx+2,-1:npy+2))
          allocate(gl_work3d(npx_2h,npy_2h,nbins))
       else
          allocate(gl_work2d(1,1))
       end if
       !
       do ibin = 1,nbins
          call domain_gather_mass_points_2halo_2D(gl_work2d(:,:),npx,npy,my_acum(:,:,ibin))
          if(master) gl_work3d(1:npx_2h,1:npy_2h,ibin) = gl_work2d(-1:npx+2,-1:npy+2)
       end do
       !
       if(master) then
          !
          start3d=(/1,1,1/)
          count3d=(/npx_2h,npy_2h,nbins/)
          !
          istat = nf90_inq_varid(ncID,grn_nc_name,grn_ncID)
          istat = nf90_put_var  (ncID,grn_ncID, gl_work3d, start=start3d,count=count3d)
          !
          deallocate(gl_work3d)
       end if
       deallocate(gl_work2d)
       !
       if(master) then
          istat = nf90_close(ncID)
       end if
       !
    end if
    !
    return
  end subroutine nc_IO_out_rst
  !
  !---------------------------------
  !    subroutine nc_IO_read_rst
  !---------------------------------
  !
  !>   @brief
  !>   Reads a restart file
  !
  subroutine nc_IO_read_rst(MY_FILES,MY_GRID,MY_TRA,MY_OUT,MY_TIME,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_GRID       ARAKAWA_C_GRID structure already filled
    !>   @param MY_TRA        TRACERS        structure
    !>   @param MY_OUT        model output and postprocess related parameters
    !>   @param MY_TIME       RUN_TIME       structure already filled
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(INOUT) :: MY_FILES
    type(ARAKAWA_C_GRID),  intent(IN   ) :: MY_GRID
    type(TRACERS),         intent(INOUT) :: MY_TRA
    type(MODEL_OUTPUT),    intent(IN   ) :: MY_OUT
    type(RUN_TIME),        intent(INOUT) :: MY_TIME
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: nc_file,dim_name
    integer(ip)           :: julday1,julday2
    integer(ip)           :: mode_flag, ival, istat, ibin
    integer(ip)           :: npx,   npy,   npz,  nbins
    integer(ip)           :: npx_2h,npy_2h,npz_2h
    integer(ip)           :: my_npx_2h,my_npy_2h,my_npz_2h
    integer(ip)           :: rst_year,rst_month,rst_day
    integer(ip)           :: start3d(3), start4d(4)
    integer(ip)           :: count3d(3), count4d(4)
    real(rp)              :: rval, rst_time_lag, rst_time
    !
    real(rp), allocatable :: gl_work2d(:,:)
    real(rp), allocatable :: gl_work3d(:,:,:)
    real(rp), allocatable :: gl_work4d(:,:,:,:)
    real(rp), allocatable :: my_work3d(:,:,:)
    real(rp), allocatable :: my_work4d(:,:,:,:)
    !
    !*** Initializations
    !
    istat          = 0
    MY_ERR%flag    = 0
    MY_ERR%source  = 'nc_IO_read_rst'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_FILES%file_rst  ,1,0)
    nc_file     = MY_FILES%file_rst
    PARALLEL_IO = MY_OUT%PARALLEL_IO
    !
    npx   = gl_npx
    npy   = gl_npy
    npz   = gl_npz
    !
    npx_2h = gl_npx + 4
    npy_2h = gl_npy + 4
    npz_2h = gl_npz + 4
    !
    nbins = MY_TRA%nbins
    !
    !*** Open file
    !
#if defined WITH_MPI
    if(PARALLEL_IO) then
       mode_flag = IOR(NF90_NETCDF4, NF90_CLASSIC_MODEL)
       mode_flag = IOR(mode_flag, NF90_MPIIO)
       mode_flag = IOR(mode_flag, NF90_NOWRITE)
       istat     = nf90_open_par(TRIM(nc_file), mode_flag , comm=COMM_WORLD, info = MPI_INFO_NULL, ncid=ncID)
    else if(master) then
       mode_flag = NF90_NOWRITE
       istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
    end if
#else
    mode_flag = NF90_NOWRITE
    istat = nf90_open(TRIM(nc_file), mode_flag, ncID)
#endif
    MY_ERR%flag = istat
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Unable to open restart file '//TRIM(nc_file)
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    !*** Master checks for consistency in dimensions
    !
    if(master) then
       istat = nf90_inq_dimid        (ncID, nx_nc_name, nx_ncID)
       istat = nf90_inquire_dimension(ncID, nx_ncID, dim_name, ival)
       if(ival.ne.npx) MY_ERR%flag = 1
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for dimension '//TRIM(nx_nc_name)//' between input and rst files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    if(master) then
       istat = nf90_inq_dimid        (ncID, ny_nc_name, ny_ncID)
       istat = nf90_inquire_dimension(ncID, ny_ncID, dim_name, ival)
       if(ival.ne.npy) MY_ERR%flag = 1
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for dimension '//TRIM(ny_nc_name)//' between input and rst files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    if(master) then
       istat = nf90_inq_dimid        (ncID, nz_nc_name, nz_ncID)
       istat = nf90_inquire_dimension(ncID, nz_ncID, dim_name, ival)
       if(ival.ne.npz) MY_ERR%flag = 1
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for dimension '//TRIM(nz_nc_name)//' between input and rst files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    if(master) then
       istat = nf90_inq_dimid        (ncID, nb_nc_name, nb_ncID)
       istat = nf90_inquire_dimension(ncID, nb_ncID, dim_name, ival)
       if(ival.ne.nbins) MY_ERR%flag = 1
    end if
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for dimension '//TRIM(nb_nc_name)//' between input and rst files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    !*** Master checks for consistency in grid attributes
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmin_name, rval)
       if(rval.ne.MY_GRID%lonmin) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmin_name, rval)
          if(rval.ne.MY_GRID%lonmin) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_lonmin_name)//' between input and restart files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmax_name, rval)
       if(rval.ne.MY_GRID%lonmax) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_lonmax_name, rval)
          if(rval.ne.MY_GRID%lonmax) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_lonmax_name)//' between input and restart files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmin_name, rval)
       if(rval.ne.MY_GRID%latmin) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmin_name, rval)
          if(rval.ne.MY_GRID%latmin) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_latmin_name)//' between input and restart files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    if(PARALLEL_IO) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmax_name, rval)
       if(rval.ne.MY_GRID%latmax) MY_ERR%flag = 1
    else
       if(master) then
          istat = nf90_get_att(ncID, NF90_GLOBAL, attr_latmax_name, rval)
          if(rval.ne.MY_GRID%latmax) MY_ERR%flag = 1
       end if
       call parallel_bcast(MY_ERR%flag,1,0)
    end if
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Inconsistent values for '//TRIM(attr_latmax_name)//' between input and restart files'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    !*** Master gets the restarting time (MY_TIME%run_start). Note that the origin of the restart file can refer
    !*** to a different day, i.e. need to check for eventual time lag
    !
    if(master) then
       !
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_year_name,    rst_year)
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_month_name,   rst_month)
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_day_name,     rst_day)
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_run_time_name,rst_time)
       !
       !*** Calculates time lag by iteration (that is, the time in seconds between the model origin
       !*** and the rst origin). Both origins are referred to 0000UTC, but may belong to different days
       !
       call time_julian_date(rst_year,           rst_month,           rst_day,           julday1, MY_ERR)
       call time_julian_date(MY_TIME%start_year, MY_TIME%start_month, MY_TIME%start_day, julday2, MY_ERR)
       !
       rst_time_lag = (julday2-julday1)*86400.0_rp
       !
       MY_TIME%run_start = rst_time - rst_time_lag
    end if
    !
    !*** Broadcast MY_TIME%run_start and perform additional checks
    !
    call parallel_bcast(MY_TIME%run_start,1,0)
    !
    if(MY_TIME%run_start.lt.0.0_rp) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Inconsistent values for restarting time '
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    if(MY_TIME%run_start.gt.MY_TIME%run_end) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'restarting time larger than rund end time'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    !*** Master reads and broadcasts mass balance attributes
    !
    if(master) then
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_mass_ground_name,  MY_TRA%rst_mass_ground )
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_mass_lateral_name, MY_TRA%rst_mass_lateral)
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_mass_sink_name,    MY_TRA%rst_mass_sink   )
       istat = nf90_get_att(ncID, NF90_GLOBAL, attr_mass_in_name,      MY_TRA%gl_mass_in      )
    end if
    call parallel_bcast(MY_TRA%rst_mass_ground, 1,0)
    call parallel_bcast(MY_TRA%rst_mass_lateral,1,0)
    call parallel_bcast(MY_TRA%rst_mass_sink   ,1,0)
    call parallel_bcast(MY_TRA%gl_mass_in,      1,0)
    !
    !*** Reads restarting values
    !
    if(PARALLEL_IO) then
       !
       !  PARALLEL IO
       !
       my_npx_2h = my_ipe_2h-my_ips_2h+1
       my_npy_2h = my_jpe_2h-my_jps_2h+1
       my_npz_2h = my_kpe_2h-my_kps_2h+1
       !
       allocate(my_work3d(my_npx_2h,my_npy_2h,          nbins))
       allocate(my_work4d(my_npx_2h,my_npy_2h,my_npz_2h,nbins))
       !
       !  CON
       !
       start4d=(/my_ips_2h+2,my_jps_2h+2,my_kps_2h+2,1    /)  ! note the shift
       count4d=(/my_npx_2h,my_npy_2h,my_npz_2h,nbins/)
       istat  = nf90_inq_varid(ncID,c_total_nc_name,con_ncID)
       istat  = nf90_get_var  (ncID, con_ncID, my_work4d, start=start4d,count=count4d)
       MY_TRA%my_c =  my_work4d
       !
       !  LOAD
       !
       start3d=(/my_ips_2h+2,my_jps_2h+2,1    /) ! note the shift
       count3d=(/my_npx_2h,my_npy_2h,nbins/)
       !
       istat = nf90_inq_varid(ncID,grn_nc_name,grn_ncID)
       istat = nf90_get_var(ncID, grn_ncID, my_work3d, start=start3d,count=count3d)
       MY_TRA%my_acum = my_work3d
       !
       istat = nf90_close(ncID)
       !
    else
       !
       !  SERIAL_IO
       !
       !  CON
       !
       if(master) then
          allocate(gl_work3d(-1:npx+2,-1:npy+2,-1:npz+2))
          allocate(gl_work4d(npx_2h,npy_2h,npz_2h,nbins))
          start4d=(/1,1,1,1/)
          count4d=(/npx_2h,npy_2h,npz_2h,nbins/)
          !
          istat = nf90_inq_varid(ncID,c_total_nc_name,con_ncID)
          istat = nf90_get_var  (ncID,con_ncID, gl_work4d, start=start4d,count=count4d)
       else
          allocate(gl_work3d(1,1,1))
          allocate(gl_work4d(1,1,1,1))
       end if
       do ibin = 1,nbins
          if(master) gl_work3d(-1:npx+2,-1:npy+2,-1:npz+2) = gl_work4d(1:npx_2h,1:npy_2h,1:npz_2h,ibin)
          call domain_scatter_mass_points_2halo(gl_work3d(:,:,:),npx,npy,npz,MY_TRA%my_c(:,:,:,ibin))
       end do
       deallocate(gl_work3d)
       deallocate(gl_work4d)
       !
       !  LOAD
       !
       if(master) then
          allocate(gl_work2d(-1:npx+2,-1:npy+2))
          allocate(gl_work3d(npx_2h,npy_2h,nbins))
          start3d=(/1,1,1/)
          count3d=(/npx_2h,npy_2h,nbins/)
          !
          istat = nf90_inq_varid(ncID,grn_nc_name,grn_ncID)
          istat = nf90_get_var  (ncID,grn_ncID, gl_work3d, start=start3d,count=count3d)
       else
          allocate(gl_work2d(1,1))
          allocate(gl_work3d(1,1,1))
       end if
       do ibin = 1,nbins
          if(master) gl_work2d(-1:npx+2,-1:npy+2) = gl_work3d(1:npx_2h,1:npy_2h,ibin)
          call domain_scatter_mass_points_2halo_2D(gl_work2d(:,:),npx,npy,MY_TRA%my_acum(:,:,ibin))
       end do
       deallocate(gl_work2d)
       deallocate(gl_work3d)
       !
       if(master) then
          istat = nf90_close(ncID)
       end if
       !
    end if
    !
    return
  end subroutine nc_IO_read_rst
  !
  !
  !
END MODULE nc_IO
