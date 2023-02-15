!***********************************************************************
!>
!> Module for Validation operations 
!> @author
!> Arnau Folch
!>
!**********************************************************************
MODULE Validation
  use KindType
  use InpOut
  use Parallel
  use netcdf
  use nc_IO_names
  use nc_IO
  use Time
  use Sat
  use Deposit
  use Grid
  use Maths
  use Postp
  implicit none
  save
  !
  !    LIST OF PUBLIC VARIABLES
  !
  integer(ip), parameter :: OBS_TYPE_SATELLITE_DETECTION = 1
  integer(ip), parameter :: OBS_TYPE_SATELLITE_RETRIEVAL = 2
  integer(ip), parameter :: OBS_TYPE_VAA                 = 3
  integer(ip), parameter :: OBS_TYPE_DEPOSIT_POINTS      = 4
  integer(ip), parameter :: OBS_TYPE_DEPOSIT_CONTOURS    = 5
  !
  integer(ip), parameter :: RES_TYPE_SINGLE_RUN   = 1
  integer(ip), parameter :: RES_TYPE_ENSEMBLE_RUN = 2
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: validation_read_inp 
  PUBLIC :: validation_bcast_inp_params
  PUBLIC :: validation_read_res_params 
  PUBLIC :: validation_bcast_res_params 
  PUBLIC :: validation_interpolate_sat_obs
  PUBLIC :: validation_interpolate_dep_obs
  PUBLIC :: validation_interpolate_dep_pts
  PUBLIC :: validation_get_time_factors
  PUBLIC :: validation_type_grid
  PUBLIC :: validation_type_pts
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: validation_get_metrics
  PRIVATE :: validation_get_histogram_grid
  PRIVATE :: validation_get_histogram_pts
  PRIVATE :: validation_allocate
  PRIVATE :: validation_obs_thresholds
  PRIVATE :: validation_res_thresholds
  PRIVATE :: validation_get_my_results_grid
  PRIVATE :: validation_get_my_results_pts
  PRIVATE :: validation_get_prob_con
  PRIVATE :: validation_get_GFMS
  PRIVATE :: validation_get_GFAR
  PRIVATE :: validation_get_GPOD
  PRIVATE :: validation_get_Brier_score
  PRIVATE :: validation_get_NRMSE_grid
  PRIVATE :: validation_get_NRMSE_pts
  !
  !    type OBS_PARAMS
  !
  type OBS_PARAMS
     !
     character(s_file) :: file_name        !< observations file
     character(s_file) :: file_tbl         !< observations TBL file
     !
     integer(ip) :: type                   !< type of observation
     integer(ip) :: tracer_code            !< tracer code
     !
     integer(ip) :: nt                     !< number time slabs read
     integer(ip) :: start_year             !< start year
     integer(ip) :: start_month            !< start month
     integer(ip) :: start_day              !< start day
     !
     real(rp) :: col_mass_obs_threshold    = 0.0_rp
     real(rp) :: col_mass_obs_threshold_DU = 0.0_rp
     real(rp) :: grn_load_obs_threshold    = 0.0_rp
     !
     real(rp), allocatable :: timesec(:)      !< timesec in sec after YYYY-MM-DD 00:00:00 UTC
     real(rp), allocatable :: mass   (:,:,:)  !< Observed mass(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt). Contains column mass loading or ground mass depending on the type
     real(rp), allocatable :: mask   (:,:,:)  !< Observed mask(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt). Contains column mass loading mask
     real(rp), allocatable :: var    (:,:,:)  !< generic  var (my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt)
     real(rp), allocatable :: prob (:,:,:,:)  !< generic  probability contours at observation times. prob(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,nth)
     !
     integer(ip), allocatable :: it_m(:)   !< model rank index
     real(rp),    allocatable :: st_m(:)   !< time interpolation factor
     !
     integer(ip)              :: npts           !< number points
     integer(ip), allocatable :: ipts(:)        !< global grid boundary index of the cell hosting the point
     integer(ip), allocatable :: jpts(:)        !< global grid boundary index of the cell hosting the point
     real(rp),    allocatable :: spts(:)        !< x-interpolation factor
     real(rp),    allocatable :: tpts(:)        !< y-interpolation factor
     real(rp),    allocatable :: mass_pts(:,:)  !< Observed mass(MY_OBS%npts,MY_OBS%nt)
     real(rp),    allocatable :: var_pts (:,:)  !< generic  var (MY_OBS%npts,MY_OBS%nt)
     !
  end type OBS_PARAMS
  !
  !    type RES_PARAMS
  !
  type RES_PARAMS
     !
     character(s_file) :: file_name        !< results file
     !
     logical     :: periodic         !< periodic domain
     !
     integer(ip) :: type             !< type of results
     integer(ip) :: nens             !< number ensemble members
     integer(ip) :: nbx              !< number x boundaries (global)
     integer(ip) :: nby              !< number y boundaries (global)
     integer(ip) :: npx              !< number x points     (global)
     integer(ip) :: npy              !< number y points     (global)
     integer(ip) :: nt               !< number time steps
     integer(ip) :: nth              !< number of thresholds
     integer(ip) :: start_year       !< start year
     integer(ip) :: start_month      !< start month
     integer(ip) :: start_day        !< start day
     !
     integer(ip) :: nth_con          !< number of thresholds (ensemble runs only)
     integer(ip) :: nth_col_mass
     integer(ip) :: nth_col_mass_DU
     integer(ip) :: nth_grn_load
     !
     real(rp), allocatable :: lon_c(:)           !< lon_c longitude  values at cell corners (global)
     real(rp), allocatable :: lon_p(:)           !< lon_p longitude  values at mass points  (global)
     real(rp), allocatable :: lat_c(:)           !< lat_c latitude   values at cell corners (global)
     real(rp), allocatable :: lat_p(:)           !< lat_p latitude   values at mass points  (global)
     real(rp), allocatable :: Hm1_p(:)           !< Hm1_p map factor values at mass points  (global)
     real(rp), allocatable :: timesec(:)         !< time in sec after YYYY-MM-DD 00:00:00 UTC
     real(rp), allocatable :: th_con(:)          !< threshold values
     real(rp), allocatable :: th_col_mass(:)
     real(rp), allocatable :: th_col_mass_DU(:)
     real(rp), allocatable :: th_grn_load(:)
     !
     real(rp), allocatable :: var (:,:,:,:)  !< generic results at mass points and observation times. var (my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,nth)
     real(rp), allocatable :: prob(:,:,:,:)  !< generic probability contours    at observation times. prob(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,nth)
     !
     real(rp), allocatable :: var_pts(:,:,:)  !< generic results at obs points and observation times. var_pts(MY_OBS%npts,MY_OBS%nt,MY_RES%nth)
     !
  end type RES_PARAMS
  !
  type VAL_PARAMS
    !
    logical :: metric_histogram           !< type of metric
    logical :: metric_categoric           !< type of metric
    logical :: metric_brier               !< type of metric 
    logical :: metric_quantitative_grid   !< type of metric
    logical :: metric_quantitative_pts    !< type of metric
    !
    character(s_file) :: var_name         !< current validated variable
    character(s_file) :: obs_name         !< current observatio type
    !
    integer(ip) :: nth                    !< number of thresholds 
    integer(ip) :: nens                   !< number of ensembles
    !
    real(rp) :: GFMS                      !< Generalised Figure Merit of Space        (GFMS)
    real(rp) :: GFAR                      !< Generalised False-Alarm Rate             (GFAR)
    real(rp) :: GPPV                      !< Generalised Positive Predictive Value    (GPPV)
    real(rp) :: GPOD                      !< Generalised Probability of Detection     (GPOD)
    real(rp) :: GCCM                      !< Generalised Composite Categorical Metric (GCCM)
    real(rp) :: BS                        !< Brier score                              (BS  )
    real(rp) :: NRMSE                     !< Normalized Root mean square error        (NRMSE)
    !
    integer(ip), allocatable :: histogram(:) 
    real(rp),    allocatable :: threshold(:) 
  end type VAL_PARAMS
  !
CONTAINS
  ! 
  !
  !-----------------------------------------
  !    subroutine validation_read_inp
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads MODEL_VALIDATION block from input file
  !
  subroutine validation_read_inp(MY_FILES, MY_OBS, MY_RES, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    real(rp)               :: file_version
    real(rp)               :: rvoid
    character(len=s_file)  :: file_inp, word
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_read_inp'
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
       MY_ERR%source  = 'validation_read_inp'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version '
       return
    end if
    !
    !*** Start reading
    !
    !
    !*** OBSERVATIONS_TYPE
    !
    call inpout_get_cha (file_inp,'MODEL_VALIDATION','OBSERVATIONS_TYPE',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('SATELLITE_DETECTION')
        MY_OBS%type = OBS_TYPE_SATELLITE_DETECTION
    case('SATELLITE_RETRIEVAL')
        MY_OBS%type = OBS_TYPE_SATELLITE_RETRIEVAL
    case('VAA')
        MY_OBS%type = OBS_TYPE_VAA
        MY_ERR%flag    = 1
        MY_ERR%message = 'Type of observation not implemented yet'
        return       
    case('DEPOSIT_CONTOURS')
        MY_OBS%type = OBS_TYPE_DEPOSIT_CONTOURS
    case('DEPOSIT_POINTS')
        MY_OBS%type = OBS_TYPE_DEPOSIT_POINTS
    case default
        MY_ERR%flag    = 1
        MY_ERR%message = 'Incorrect type of observation '
        return
    end select
    !
    !*** OBSERVATIONS_FILE
    !
    call inpout_get_cha (file_inp,'MODEL_VALIDATION','OBSERVATIONS_FILE',word,1,MY_ERR,.false.)
    if(MY_ERR%flag.eq.0) then
       MY_OBS%file_name = TRIM(word)
    else
       MY_ERR%flag    = 1
       MY_ERR%message = 'Observations file not specifyed '
       return
    end if
    !
    !*** OBSERVATIONS_DICTIONARY_FILE
    !
    call inpout_get_cha (file_inp,'MODEL_VALIDATION','OBSERVATIONS_DICTIONARY_FILE',word,1,MY_ERR,.false.)
    if(MY_ERR%flag.eq.0) then
       MY_OBS%file_tbl = TRIM(word) 
    else
       MY_OBS%file_tbl= '-'
       MY_ERR%flag = 0
    end if
    !
    !*** RESULTS_FILE
    !
    call inpout_get_cha (file_inp,'MODEL_VALIDATION','RESULTS_FILE',word,1,MY_ERR,.false.)
    if(MY_ERR%flag.eq.0) then
       MY_RES%file_name = TRIM(word)
    else
       MY_ERR%flag    = 1
       MY_ERR%message = 'Results file not specifyed '
       return
    end if
    !
    !*** THRESHOLDS
    !
    select case(MY_OBS%type)
    case(OBS_TYPE_SATELLITE_RETRIEVAL, OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_VAA)
        !
        call inpout_get_rea (file_inp,'IF_OBSERVATIONS_TYPE_SATELLITE','COLUMN_MASS_OBSERVATION_THRESHOLD_(G/M2)',& 
                             MY_OBS%col_mass_obs_threshold,1,MY_ERR)
        if(MY_ERR%flag.ne.0) MY_OBS%col_mass_obs_threshold = 0.1_rp
        !
        call inpout_get_rea (file_inp,'IF_OBSERVATIONS_TYPE_SATELLITE','COLUMN_MASS_OBSERVATION_THRESHOLD_(DU)',& 
                             MY_OBS%col_mass_obs_threshold_DU,1,MY_ERR)
        if(MY_ERR%flag.ne.0) MY_OBS%col_mass_obs_threshold_DU = 1.0_rp
        !
    case(OBS_TYPE_DEPOSIT_CONTOURS,OBS_TYPE_DEPOSIT_POINTS)
        !         
        call inpout_get_rea (file_inp,'IF_OBSERVATIONS_TYPE_DEPOSIT','GROUND_LOAD_OBSERVATION_THRESHOLD_(KG/M2)',& 
                             MY_OBS%grn_load_obs_threshold,1,MY_ERR)
        if(MY_ERR%flag.ne.0) MY_OBS%grn_load_obs_threshold = 0.1_rp
        !
    end select
    !
    !*** Convert thresholds to IS units (to be consistent with subsequent conversion of observations and variables)
    !
    MY_OBS%col_mass_obs_threshold    = MY_OBS%col_mass_obs_threshold * 1e-3_rp                          ! g/m2 --> kg/m2
    MY_OBS%col_mass_obs_threshold_DU = MY_OBS%col_mass_obs_threshold_DU * 64.0_rp / 2.238e3_rp / 1e3_rp ! DU --> g/m2 --> kg/m2  
    !
    return
  end subroutine validation_read_inp
  !
  !-----------------------------------------
  !    subroutine validation_bcast_inp_params
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts input parameters structure
  !
  subroutine validation_bcast_inp_params(MY_OBS, MY_RES, MY_ERR)
    implicit none
    !
    !>   @param MY_OBS    list of observation parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_ERR    error handler
    !
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_bcast_inp_params'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_OBS%file_name     ,1,0)
    call parallel_bcast(MY_OBS%file_tbl      ,1,0)
    call parallel_bcast(MY_OBS%type          ,1,0)
    call parallel_bcast(MY_RES%file_name     ,1,0)
    !
    call parallel_bcast(MY_OBS%col_mass_obs_threshold   ,1,0)
    call parallel_bcast(MY_OBS%col_mass_obs_threshold_DU,1,0)
    call parallel_bcast(MY_OBS%grn_load_obs_threshold   ,1,0)
    !
    return
  end subroutine validation_bcast_inp_params
  !
  !
  !-----------------------------------------
  !    subroutine validation_read_res_params
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads parameters from the results file
  !
  subroutine validation_read_res_params(MY_FILES,MY_RES,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: file_inp,word 
    character(len=24)     :: time_str
    integer(ip)           :: istat, lulog, i,j
    integer(ip)           :: ncID, dimID, varID
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: hm1_code
    real(rp)              :: lonmin,lonmax,latmin,latmax,resx,resy,colat
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_read_res_params'
    MY_ERR%message = ' '
    !
    file_inp = MY_RES%file_name
    !
    !*** Open netCDF file and get ncID
    !
    istat = nf90_open(TRIM(file_inp),NF90_NOWRITE, ncID)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Unable to open '//TRIM(file_inp)
       return
    end if
    !
    !*** Find out the type of results
    !
    istat = nf90_inq_dimid        (ncID,ens_nc_name,dimID)
    if(istat.eq.0) then
       istat = nf90_inquire_dimension(ncID,dimID,len = MY_RES%nens)
       MY_RES%type = RES_TYPE_ENSEMBLE_RUN
    else
       MY_RES%nens = 1
       MY_RES%type = RES_TYPE_SINGLE_RUN
    end if    
    !
    !*** Read dimensions
    !
    istat = nf90_inq_dimid        (ncID,lon_nc_name,dimID)
    istat = nf90_inquire_dimension(ncID,dimID,len = MY_RES%nbx)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    else
       MY_RES%npx = MY_RES%nbx - 1
    end if
    !
    istat = nf90_inq_dimid        (ncID,lat_nc_name,dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = MY_RES%nby)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    else
       MY_RES%npy = MY_RES%nby - 1
    end if
    !
    istat = nf90_inq_dimid        (ncID,tim_nc_name,dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = MY_RES%nt)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    select case(MY_RES%type)
    case(RES_TYPE_SINGLE_RUN)
      !
      MY_RES%nth_con         = 1
      MY_RES%nth_col_mass    = 1
      MY_RES%nth_col_mass_DU = 1
      MY_RES%nth_grn_load    = 1
      !
    case(RES_TYPE_ENSEMBLE_RUN)
      !
      istat = nf90_inq_dimid        (ncID,th_con_nc_name,dimID)
      istat = nf90_inquire_dimension(ncID, dimID, len = MY_RES%nth_con)
      if(istat.ne.0) MY_RES%nth_con = 1
      !
      istat = nf90_inq_dimid        (ncID,th_col_mass_nc_name,dimID)
      istat = nf90_inquire_dimension(ncID, dimID, len = MY_RES%nth_col_mass)
      if(istat.ne.0) MY_RES%nth_col_mass = 1
      !
      istat = nf90_inq_dimid        (ncID,th_col_mass_DU_nc_name,dimID)
      istat = nf90_inquire_dimension(ncID, dimID, len = MY_RES%nth_col_mass_DU)
      if(istat.ne.0) MY_RES%nth_col_mass_DU = 1
      !
      istat = nf90_inq_dimid        (ncID,th_grn_load_nc_name,dimID)
      istat = nf90_inquire_dimension(ncID, dimID, len = MY_RES%nth_grn_load)
      if(istat.ne.0) MY_RES%nth_grn_load = 1
      !
    end select
    !
    !*** Allocates (global arrays, no domain decomposition yet)
    !
    allocate(MY_RES%lon_c         (MY_RES%nbx            ))
    allocate(MY_RES%lon_p         (MY_RES%npx            ))
    allocate(MY_RES%lat_c         (MY_RES%nby            ))
    allocate(MY_RES%lat_p         (MY_RES%npy            ))
    allocate(MY_RES%Hm1_p         (MY_RES%npy            ))
    allocate(MY_RES%timesec       (MY_RES%nt             ))
    allocate(MY_RES%th_con        (MY_RES%nth_con        ))
    allocate(MY_RES%th_col_mass   (MY_RES%nth_col_mass   ))
    allocate(MY_RES%th_col_mass_DU(MY_RES%nth_col_mass_DU))
    allocate(MY_RES%th_grn_load   (MY_RES%nth_grn_load   ))
    !
    !*** Read attributes
    !
    istat = nf90_inq_varid(ncID,lon_nc_name,varID)
    istat = nf90_get_att  (ncID,varID,attr_min_name ,lonmin)
    istat = nf90_get_att  (ncID,varID,attr_max_name ,lonmax)
    istat = nf90_get_att  (ncID,varID,attr_cell_name,resx  )
    !
    istat = nf90_inq_varid(ncID,lat_nc_name,varID)
    istat = nf90_get_att  (ncID,varID,attr_min_name,  latmin   )
    istat = nf90_get_att  (ncID,varID,attr_max_name,  latmax   )
    istat = nf90_get_att  (ncID,varID,attr_map_h_name,hm1_code )
    istat = nf90_get_att  (ncID,varID,attr_cell_name, resy     )
    !
    istat = nf90_inq_varid(ncID,tim_nc_name,varID)
    istat = nf90_get_att  (ncID,varID,attr_year_name ,MY_RES%start_year )
    istat = nf90_get_att  (ncID,varID,attr_month_name,MY_RES%start_month)
    istat = nf90_get_att  (ncID,varID,attr_day_name  ,MY_RES%start_day  )
    !
    !*** Read variables
    !
    istat = nf90_inq_varid(ncID,lon_nc_name,varID)
    istat = nf90_get_var  (ncID,varID,MY_RES%lon_c,start=(/1/),count=(/MY_RES%nbx/))
    !
    istat = nf90_inq_varid(ncID,lat_nc_name,varID)
    istat = nf90_get_var  (ncID,varID,MY_RES%lat_c,start=(/1/),count=(/MY_RES%nby/))
    !
    istat = nf90_inq_varid(ncID,tim_nc_name,varID)
    istat = nf90_get_var  (ncID,varID,MY_RES%timesec,start=(/1/),count=(/MY_RES%nt/))
    !
    select case(MY_RES%type)
    case(RES_TYPE_SINGLE_RUN)
      !
      MY_RES%th_con         = 0.0_rp
      MY_RES%th_col_mass    = 0.0_rp
      MY_RES%th_col_mass_DU = 0.0_rp
      MY_RES%th_grn_load    = 0.0_rp
      !
    case(RES_TYPE_ENSEMBLE_RUN)
      !
      istat = nf90_inq_varid(ncID,th_con_nc_name,varID)
      istat = nf90_get_var  (ncID,varID,MY_RES%th_con,start=(/1/),count=(/MY_RES%nth_con/))
      !
      istat = nf90_inq_varid(ncID,th_col_mass_nc_name,varID)
      istat = nf90_get_var  (ncID,varID,MY_RES%th_col_mass,start=(/1/),count=(/MY_RES%nth_col_mass/))
      !
      istat = nf90_inq_varid(ncID,th_col_mass_DU_nc_name,varID)
      istat = nf90_get_var  (ncID,varID,MY_RES%th_col_mass_DU,start=(/1/),count=(/MY_RES%nth_col_mass_DU/))
      !
      istat = nf90_inq_varid(ncID,th_grn_load_nc_name,varID)
      istat = nf90_get_var  (ncID,varID,MY_RES%th_grn_load,start=(/1/),count=(/MY_RES%nth_grn_load/))
      !
    end select
    !
    !*** Compute other variables
    !
    do i = 1,MY_RES%npx
       MY_RES%lon_p(i) = 0.5_rp*(MY_RES%lon_c(i)+MY_RES%lon_c(i+1))   ! mass points
    end do
    !
    do j = 1,MY_RES%npy
       MY_RES%lat_p(j) = 0.5_rp*(MY_RES%lat_c(j)+MY_RES%lat_c(j+1))   ! mass points
    end do
    !
    if((lonmin.eq.-180.0_rp).and.(lonmax.eq.180.0_rp)) then
        MY_RES%periodic = .true.
    else
        MY_RES%periodic = .false.
    end if
    !
    select case(hm1_code)
    case(MAP_H_CARTESIAN)
        !
        MY_RES%Hm1_p(:) = 1.0_rp
        !
       case(MAP_H_SPHERICAL)
        !
        do j = 1,MY_RES%npy
           colat           = (90.0_rp- MY_RES%lat_p(j))*PI/180.0_rp   ! colatitude in rad
           MY_RES%Hm1_p(j) = sin(colat)
        end do
        ! 
     case(MAP_H_POLAR, MAP_H_MERCATOR)
        ! 
        MY_ERR%flag    = 1
        MY_ERR%message = 'Mapping not implemented yet'
        return
        !
    end select
    !
    !*** Print to log file
    !
    lulog = MY_FILES%lulog
    !
    write(lulog,10)
10  format(&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '                  MODEL RESULTS                     ',/,   &
         '                                                    ',/,   &
         '----------------------------------------------------')
    !
    call time_addtime(MY_RES%start_year,       &
                      MY_RES%start_month,      &
                      MY_RES%start_day,        &
                      0_ip,                    &
                      iyr,imo,idy,ihr,imi,ise, &
                      MY_RES%timesec(1),       &
                      MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,20) TRIM(time_str)
20  format(/,&
         'TIME RANGE OF MODEL RESULTS',/, &
         '  Initial time       : ',a)
    !
    call time_addtime(MY_RES%start_year,       &
                      MY_RES%start_month,      &
                      MY_RES%start_day,        &
                      0_ip,                    &
                      iyr,imo,idy,ihr,imi,ise, &
                      MY_RES%timesec(MY_RES%nt), &
                      MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,21) TRIM(time_str),MY_RES%nt
21  format('  Final   time       : ',a,/, &
           '  Number  time steps : ',i9)
    !
    select case(MY_RES%type)
    case(RES_TYPE_SINGLE_RUN)
       word = 'Single run'
    case(RES_TYPE_ENSEMBLE_RUN)
       word = 'Ensemble run'
    end select
    !
    write(lulog,30) TRIM(word), MY_RES%nens,lonmin,latmin,lonmax,latmax, &
                    resx,resy,MY_RES%nbx,MY_RES%nby
30  format(/, &
         'TYPE OF RUN AND COVERAGE',/, &
         '  Run type           : ',a               ,/,      &
         '  Ensemble members   : ',i9              ,/,      &
         '  Bottom-left corner : (',f9.4,2x,f9.4,')',/,     &
         '  Top-right   corner : (',f9.4,2x,f9.4,')',/,     &
         '  Resolution         : (',f9.4,2x,f9.4,')',/,     &
         '  Number points x    : ',i9  ,/,                  &
         '  Number points y    : ',i9)
    write(lulog,31) (MY_RES%th_con(i)        ,i=1,MY_RES%nth_con        )
    write(lulog,32) (MY_RES%th_col_mass(i)   ,i=1,MY_RES%nth_col_mass   )
    write(lulog,33) (MY_RES%th_col_mass_DU(i),i=1,MY_RES%nth_col_mass_DU)
    write(lulog,34) (MY_RES%th_grn_load(i)   ,i=1,MY_RES%nth_grn_load   )
31  format(/,&
         'THRESHOLDS (ensemble runs only)',/, &
         '  Concentration (mg/m3) : ',100(f8.3,1x))
32  format(&
         '  Column mass   (g/m2)  : ',100(f8.3,1x))
33  format(&
         '  Column mass   (DU)    : ',100(f8.3,1x))
34  format(&
         '  Ground load   (kg/m2) : ',100(f8.3,1x))
    !
    !*** Finally, convert thresholds to IS units
    !
    MY_RES%th_con        (:) = MY_RES%th_con(:)         * 1e-6_rp                          ! mg/m3 --> kg/m3
    MY_RES%th_col_mass   (:) = MY_RES%th_col_mass(:)    * 1e-3_rp                          ! g/m2  --> kg/m2
    MY_RES%th_col_mass_DU(:) = MY_RES%th_col_mass_DU(:) * 64.0_rp / 2.238e3_rp / 1e3_rp    ! DU    --> g/m2 --> kg/m2  
    !
    return
  end subroutine validation_read_res_params
  !
  !-----------------------------------------
  !    subroutine validation_bcast_res_params
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts MY_RES structure
  !
  subroutine validation_bcast_res_params(MY_RES, MY_ERR)
    implicit none
    !
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_ERR    error handler
    !
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_bcast_res_params'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_RES%periodic   ,1,0)
    call parallel_bcast(MY_RES%type       ,1,0)
    call parallel_bcast(MY_RES%nens       ,1,0)
    call parallel_bcast(MY_RES%nbx        ,1,0)
    call parallel_bcast(MY_RES%nby        ,1,0)
    call parallel_bcast(MY_RES%npx        ,1,0)
    call parallel_bcast(MY_RES%npy        ,1,0)
    call parallel_bcast(MY_RES%nt         ,1,0)
    call parallel_bcast(MY_RES%start_year ,1,0)
    call parallel_bcast(MY_RES%start_month,1,0)
    call parallel_bcast(MY_RES%start_day  ,1,0)
    !
    call parallel_bcast(MY_RES%nth_con        ,1,0)
    call parallel_bcast(MY_RES%nth_col_mass   ,1,0)
    call parallel_bcast(MY_RES%nth_col_mass_DU,1,0)
    call parallel_bcast(MY_RES%nth_grn_load   ,1,0)
    !
    if(.not.master_model) then
       allocate(MY_RES%lon_c         (MY_RES%nbx))
       allocate(MY_RES%lon_p         (MY_RES%npx))
       allocate(MY_RES%lat_c         (MY_RES%nby))
       allocate(MY_RES%lat_p         (MY_RES%npy))
       allocate(MY_RES%Hm1_p         (MY_RES%npy))
       allocate(MY_RES%timesec       (MY_RES%nt ))
       allocate(MY_RES%th_con        (MY_RES%nth_con        ))
       allocate(MY_RES%th_col_mass   (MY_RES%nth_col_mass   ))
       allocate(MY_RES%th_col_mass_DU(MY_RES%nth_col_mass_DU))
       allocate(MY_RES%th_grn_load   (MY_RES%nth_grn_load   ))
    end if
    !
    call parallel_bcast(MY_RES%lon_c  ,MY_RES%nbx,0)
    call parallel_bcast(MY_RES%lon_p  ,MY_RES%npx,0)
    call parallel_bcast(MY_RES%lat_c  ,MY_RES%nby,0)
    call parallel_bcast(MY_RES%lat_p  ,MY_RES%npy,0)
    call parallel_bcast(MY_RES%Hm1_p  ,MY_RES%npy,0)
    call parallel_bcast(MY_RES%timesec,MY_RES%nt ,0)
    !
    call parallel_bcast(MY_RES%th_con        ,MY_RES%nth_con        ,0)
    call parallel_bcast(MY_RES%th_col_mass   ,MY_RES%nth_col_mass   ,0)
    call parallel_bcast(MY_RES%th_col_mass_DU,MY_RES%nth_col_mass_DU,0)
    call parallel_bcast(MY_RES%th_grn_load   ,MY_RES%nth_grn_load   ,0)
    !
    return
  end subroutine validation_bcast_res_params
  !
  !---------------------------------------------
  !    subroutine validation_interpolate_sat_obs
  !---------------------------------------------
  !
  !>   @brief
  !>   Interpolates sat observations to the (local) model grid
  !
  subroutine validation_interpolate_sat_obs(MY_FILES,MY_OBS,MY_RES,GL_SAT_INFO,GL_SAT_DATA2D,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_OBS      list of observation parameters
    !>   @param MY_RES      list of model results parameters
    !>   @param GL_SAT_INFO attributes in satellite input file
    !>   @param GL_SAT      variables related to sat data
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(SAT_INFO),       intent(IN   ) :: GL_SAT_INFO
    type(SAT_DATA2D),     intent(IN   ) :: GL_SAT_DATA2D
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    type(Q1_GRID) :: OBS_MESH
    integer(ip)   :: i,j,ipoin,lulog
    integer(ip)   :: el_po(1),ielem,ix,iy,it
    integer(ip)   :: my_npoin,my_npoin_obs,my_npoin_mask
    integer(ip)   :: gl_npoin(1),gl_npoin_obs(1),gl_npoin_mask(1)
    real(rp)      :: s,t,st,myshape(4),mask
    real(dp)      :: lon(1),lat(1),s_po(1),t_po(1)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_interpolate_sat_obs'
    MY_ERR%message = ' '
    !
    MY_OBS%nt          = GL_SAT_INFO%nt
    MY_OBS%tracer_code = GL_SAT_INFO%tracer_code
    !
    allocate(MY_OBS%timesec(MY_OBS%nt))
    MY_OBS%timesec(:) = GL_SAT_INFO%timesec(:)
    !
    !*** Reference date (note that GL_SAT_INFO%time is not broadcasted)
    !
    if(master_model) then
       MY_OBS%start_year  = GL_SAT_INFO%time(1)%year
       MY_OBS%start_month = GL_SAT_INFO%time(1)%month
       MY_OBS%start_day   = GL_SAT_INFO%time(1)%day
    end if
    call parallel_bcast(MY_OBS%start_year,1,0)
    call parallel_bcast(MY_OBS%start_month,1,0)
    call parallel_bcast(MY_OBS%start_day  ,1,0)
    !
    !*** Allocate memory
    !
    allocate(MY_OBS%mass(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt))
    allocate(MY_OBS%mask(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt))
    allocate(MY_OBS%var (my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt))
    !
    !*** Build the observations Q1 mesh
    !
    OBS_MESH%nx    =  GL_SAT_INFO%nx
    OBS_MESH%ny    =  GL_SAT_INFO%ny
    OBS_MESH%npoin =  GL_SAT_INFO%nx    *  GL_SAT_INFO%ny
    OBS_MESH%nelem = (GL_SAT_INFO%nx-1) * (GL_SAT_INFO%ny-1)
    !
    allocate(OBS_MESH%coord(2,OBS_MESH%npoin))
    ipoin = 0
    do j = 1,OBS_MESH%ny
    do i = 1,OBS_MESH%nx
       ipoin = ipoin + 1
       !
       !*** longitudes are transformed to the FALL3D range [-180,180) to avoid errors of "grid point not found"
       !
       if(GL_SAT_DATA2D%lon(i,j).ge.180.0_rp) then
          OBS_MESH%coord(1,ipoin) = GL_SAT_DATA2D%lon(i,j) - 360.0_rp
       else
          OBS_MESH%coord(1,ipoin) = GL_SAT_DATA2D%lon(i,j)
       end if
       OBS_MESH%coord(2,ipoin) = GL_SAT_DATA2D%lat(i,j)
       !
    end do
    end do
    !
    call maths_set_lnodsQ1(OBS_MESH,MY_ERR)
    !
    !*** Loop over all my_mass_points    
    !
    do i = my_ips,my_ipe 
    do j = my_jps,my_jpe
       !
       lon(1) = MY_RES%lon_p(i)
       lat(1) = MY_RES%lat_p(j)
       call maths_get_host_elemQ1(OBS_MESH,1_ip,lon,lat,el_po,s_po,t_po,MY_ERR)
       !
       if(MY_ERR%flag.ne.0) then
          MY_OBS%mass(i,j,1:MY_OBS%nt) = 0.0_rp          ! point not in OBS_MESH
          MY_OBS%mask(i,j,1:MY_OBS%nt) = 0.0_rp
          !
       else                                              !  point is in OBS_MESH
          !
          ielem = el_po(1)                               ! host element
          iy    = (ielem-1)/(OBS_MESH%nx-1) + 1
          ix    = ielem - (iy-1)*(OBS_MESH%nx-1)
          s     = s_po(1)
          t     = t_po(1)
          st    = s*t
          !
          myshape(1) = (1.0_rp-t-s+st)*0.25_rp                           !  4     3
          myshape(2) = (1.0_rp-t+s-st)*0.25_rp                           !
          myshape(3) = (1.0_rp+t+s+st)*0.25_rp                           !
          myshape(4) = (1.0_rp+t-s-st)*0.25_rp                           !  1     2
          !
          do it = 1,MY_OBS%nt
             !                                      
             if( (GL_SAT_DATA2D%mass(ix  ,iy  ,it).eq.GL_SAT_DATA2D%fill_value).or.  &      ! Check if some FillValue exists
                 (GL_SAT_DATA2D%mass(ix+1,iy  ,it).eq.GL_SAT_DATA2D%fill_value).or.  &
                 (GL_SAT_DATA2D%mass(ix+1,iy+1,it).eq.GL_SAT_DATA2D%fill_value).or.  &
                 (GL_SAT_DATA2D%mass(ix  ,iy+1,it).eq.GL_SAT_DATA2D%fill_value) ) then
                MY_OBS%mass(i,j,it) = 0.0_rp
             else
                MY_OBS%mass(i,j,it) = myshape(1)*GL_SAT_DATA2D%mass(ix  ,iy  ,it) + &
                                      myshape(2)*GL_SAT_DATA2D%mass(ix+1,iy  ,it) + &
                                      myshape(3)*GL_SAT_DATA2D%mass(ix+1,iy+1,it) + &
                                      myshape(4)*GL_SAT_DATA2D%mass(ix  ,iy+1,it) 
             end if
             !
             if( (GL_SAT_DATA2D%mask(ix  ,iy  ,it).eq.GL_SAT_DATA2D%fill_value).or.  &      ! Check if some FillValue exists
                 (GL_SAT_DATA2D%mask(ix+1,iy  ,it).eq.GL_SAT_DATA2D%fill_value).or.  &
                 (GL_SAT_DATA2D%mask(ix+1,iy+1,it).eq.GL_SAT_DATA2D%fill_value).or.  &
                 (GL_SAT_DATA2D%mask(ix  ,iy+1,it).eq.GL_SAT_DATA2D%fill_value) ) then
                MY_OBS%mask(i,j,it) = 0.0_rp
             else
                mask = myshape(1)*GL_SAT_DATA2D%mask(ix  ,iy  ,it) + &
                       myshape(2)*GL_SAT_DATA2D%mask(ix+1,iy  ,it) + &
                       myshape(3)*GL_SAT_DATA2D%mask(ix+1,iy+1,it) + &
                       myshape(4)*GL_SAT_DATA2D%mask(ix  ,iy+1,it)
                if(mask.ge.0.5_rp) then
                   MY_OBS%mask(i,j,it) = 1.0_rp
                else
                   MY_OBS%mask(i,j,it) = 0.0_rp
                end if
             end if
         end do
         !
       end if
    end do
    end do
    !
    !*** Overwrite eventual messages from interpolation errors
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_interpolate_sat_obs'
    MY_ERR%message = ' '   
    !
    !*** Print to log file
    !
    my_npoin      = 0
    my_npoin_obs  = 0
    my_npoin_mask = 0
    do i = my_ips,my_ipe 
    do j = my_jps,my_jpe
       my_npoin = my_npoin + 1
       if(ANY(MY_OBS%mass(i,j,:).gt.0.0_rp)) my_npoin_obs  = my_npoin_obs  + 1
       if(ANY(MY_OBS%mask(i,j,:).gt.0.0_rp)) my_npoin_mask = my_npoin_mask + 1
    end do
    end do
    !
    gl_npoin     (1) = my_npoin
    gl_npoin_obs (1) = my_npoin_obs
    gl_npoin_mask(1) = my_npoin_mask
    call parallel_sum(gl_npoin,     COMM_MODEL)
    call parallel_sum(gl_npoin_obs, COMM_MODEL)
    call parallel_sum(gl_npoin_mask,COMM_MODEL)
    lulog = MY_FILES%lulog
    !
    if(master_model) write(lulog,10) gl_npoin, &
                                     gl_npoin_obs, 100.0_rp*gl_npoin_obs /gl_npoin, &
                                     gl_npoin_mask,100.0_rp*gl_npoin_mask/gl_npoin
10  format(&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '    INTERPOLATION OF SATELLITE OBSERVATIONS         ',/,   &
         '                                                    ',/,   &
         '----------------------------------------------------',/,   &
         '  Number of 2D FALL3D grid points         : ',i9          ,/,   &       
         '  Number of points with mass observations : ',i9,' (',f7.2,' %)',/, &
         '  Number of points with detection mask    : ',i9,' (',f7.2,' %)')
    !
    !*** Finally load the observed variable
    !
    select case(MY_OBS%type)
    case(OBS_TYPE_SATELLITE_DETECTION)
       MY_OBS%var(:,:,:) = MY_OBS%mask(:,:,:)
    case(OBS_TYPE_SATELLITE_RETRIEVAL)  
       MY_OBS%var(:,:,:) = MY_OBS%mass(:,:,:)
    end select
    !
    return
  end subroutine validation_interpolate_sat_obs
  !
  !---------------------------------------------
  !    subroutine validation_interpolate_dep_obs
  !---------------------------------------------
  !
  !>   @brief
  !>   Interpolates dep observations to the (local) model grid
  !
  subroutine validation_interpolate_dep_obs(MY_FILES,MY_OBS,MY_RES,GL_DEP_DATA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_OBS      list of observation parameters
    !>   @param MY_RES      list of model results parameters
    !>   @param GL_DEP_DATA deposit gridded data
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(IN   ) :: MY_RES
    type(DEP_DATA),       intent(IN   ) :: GL_DEP_DATA
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    type(Q1_GRID) :: OBS_MESH
    integer(ip)   :: i,j,ipoin,lulog
    integer(ip)   :: el_po(1),ielem,ix,iy,it
    integer(ip)   :: my_npoin,my_npoin_obs,my_npoin_mask
    integer(ip)   :: gl_npoin(1),gl_npoin_obs(1),gl_npoin_mask(1)
    real(rp)      :: s,t,st,myshape(4),mask
    real(dp)      :: lon(1),lat(1),s_po(1),t_po(1)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_interpolate_dep_obs'
    MY_ERR%message = ' '
    !
    MY_OBS%nt          = 1
    MY_OBS%tracer_code = GL_DEP_DATA%tracer_code
    !
    MY_OBS%start_year  = MY_RES%start_year
    MY_OBS%start_month = MY_RES%start_month
    MY_OBS%start_day   = MY_RES%start_day
    !
    allocate(MY_OBS%timesec(MY_OBS%nt))
    MY_OBS%timesec(:) = MY_RES%timesec(MY_RES%nt)
    !
    !*** Allocate memory
    !
    allocate(MY_OBS%mass(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt))
    allocate(MY_OBS%mask(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt))
    allocate(MY_OBS%var (my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt))
    !
    !*** Build the observations Q1 mesh
    !
    OBS_MESH%nx    =  GL_DEP_DATA%nx
    OBS_MESH%ny    =  GL_DEP_DATA%ny
    OBS_MESH%npoin =  GL_DEP_DATA%nx    *  GL_DEP_DATA%ny
    OBS_MESH%nelem = (GL_DEP_DATA%nx-1) * (GL_DEP_DATA%ny-1)
    !
    allocate(OBS_MESH%coord(2,OBS_MESH%npoin))
    ipoin = 0
    do j = 1,OBS_MESH%ny
    do i = 1,OBS_MESH%nx
       ipoin = ipoin + 1
       !
       !*** longitudes are transformed to the FALL3D range [-180,180) to avoid errors of "grid point not found"
       !
       if(GL_DEP_DATA%lon(i,j).ge.180.0_rp) then
          OBS_MESH%coord(1,ipoin) = GL_DEP_DATA%lon(i,j) - 360.0_rp
       else
          OBS_MESH%coord(1,ipoin) = GL_DEP_DATA%lon(i,j)
       end if
       OBS_MESH%coord(2,ipoin) = GL_DEP_DATA%lat(i,j)
       !
    end do
    end do
    !
    call maths_set_lnodsQ1(OBS_MESH,MY_ERR)
    !
    !*** Loop over all my_mass_points    
    !
    do i = my_ips,my_ipe 
    do j = my_jps,my_jpe
       !
       lon(1) = MY_RES%lon_p(i)
       lat(1) = MY_RES%lat_p(j)
       call maths_get_host_elemQ1(OBS_MESH,1_ip,lon,lat,el_po,s_po,t_po,MY_ERR)
       !
       if(MY_ERR%flag.ne.0) then
          MY_OBS%mass(i,j,1:MY_OBS%nt) = 0.0_rp          ! point not in OBS_MESH
          MY_OBS%mask(i,j,1:MY_OBS%nt) = 0.0_rp
          !
       else                                              !  point is in OBS_MESH
          !
          ielem = el_po(1)                               ! host element
          iy    = (ielem-1)/(OBS_MESH%nx-1) + 1
          ix    = ielem - (iy-1)*(OBS_MESH%nx-1)
          s     = s_po(1)
          t     = t_po(1)
          st    = s*t
          !
          myshape(1) = (1.0_rp-t-s+st)*0.25_rp                           !  4     3
          myshape(2) = (1.0_rp-t+s-st)*0.25_rp                           !
          myshape(3) = (1.0_rp+t+s+st)*0.25_rp                           !
          myshape(4) = (1.0_rp+t-s-st)*0.25_rp                           !  1     2
          !
          do it = 1,MY_OBS%nt
             !                                      
             if( (GL_DEP_DATA%mass(ix  ,iy  ).eq.GL_DEP_DATA%fill_value).or.  &      ! Check if some FillValue exists
                 (GL_DEP_DATA%mass(ix+1,iy  ).eq.GL_DEP_DATA%fill_value).or.  &
                 (GL_DEP_DATA%mass(ix+1,iy+1).eq.GL_DEP_DATA%fill_value).or.  &
                 (GL_DEP_DATA%mass(ix  ,iy+1).eq.GL_DEP_DATA%fill_value) ) then
                MY_OBS%mass(i,j,it) = 0.0_rp
             else
                MY_OBS%mass(i,j,it) = myshape(1)*GL_DEP_DATA%mass(ix  ,iy  ) + &
                                      myshape(2)*GL_DEP_DATA%mass(ix+1,iy  ) + &
                                      myshape(3)*GL_DEP_DATA%mass(ix+1,iy+1) + &
                                      myshape(4)*GL_DEP_DATA%mass(ix  ,iy+1) 
             end if
             !
             if( (GL_DEP_DATA%mask(ix  ,iy  ).eq.GL_DEP_DATA%fill_value).or.  &      ! Check if some FillValue exists
                 (GL_DEP_DATA%mask(ix+1,iy  ).eq.GL_DEP_DATA%fill_value).or.  &
                 (GL_DEP_DATA%mask(ix+1,iy+1).eq.GL_DEP_DATA%fill_value).or.  &
                 (GL_DEP_DATA%mask(ix  ,iy+1).eq.GL_DEP_DATA%fill_value) ) then
                MY_OBS%mask(i,j,it) = 0.0_rp
             else
                mask = myshape(1)*GL_DEP_DATA%mask(ix  ,iy  ) + &
                       myshape(2)*GL_DEP_DATA%mask(ix+1,iy  ) + &
                       myshape(3)*GL_DEP_DATA%mask(ix+1,iy+1) + &
                       myshape(4)*GL_DEP_DATA%mask(ix  ,iy+1)
                if(mask.ge.0.5_rp) then
                   MY_OBS%mask(i,j,it) = 1.0_rp
                else
                   MY_OBS%mask(i,j,it) = 0.0_rp
                end if
             end if
         end do
         !
       end if
    end do
    end do
    !
    !*** Overwrite eventual messages from interpolation errors
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_interpolate_dep_obs'
    MY_ERR%message = ' '   
    !
    !*** Print to log file
    !
    my_npoin      = 0
    my_npoin_obs  = 0
    my_npoin_mask = 0
    do i = my_ips,my_ipe 
    do j = my_jps,my_jpe
       my_npoin = my_npoin + 1
       if(ANY(MY_OBS%mass(i,j,:).gt.0.0_rp)) my_npoin_obs  = my_npoin_obs  + 1
       if(ANY(MY_OBS%mask(i,j,:).gt.0.0_rp)) my_npoin_mask = my_npoin_mask + 1
    end do
    end do
    !
    gl_npoin     (1) = my_npoin
    gl_npoin_obs (1) = my_npoin_obs
    gl_npoin_mask(1) = my_npoin_mask
    call parallel_sum(gl_npoin,     COMM_MODEL)
    call parallel_sum(gl_npoin_obs, COMM_MODEL)
    call parallel_sum(gl_npoin_mask,COMM_MODEL)
    lulog = MY_FILES%lulog
    !
    if(master_model) write(lulog,10) gl_npoin, &
                                     gl_npoin_obs, 100.0_rp*gl_npoin_obs /gl_npoin, &
                                     gl_npoin_mask,100.0_rp*gl_npoin_mask/gl_npoin
10  format(/,&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '      INTERPOLATION OF DEPOSIT OBSERVATIONS         ',/,   &
         '                                                    ',/,   &
         '----------------------------------------------------',/,   &
         '  Number of 2D FALL3D grid points         : ',i9          ,/,   &       
         '  Number of points with mass observations : ',i9,' (',f7.2,' %)',/, &
         '  Number of points with detection mask    : ',i9,' (',f7.2,' %)')
    !
    !*** Finally load the observed variable
    !
    select case(MY_OBS%type)
    case(OBS_TYPE_DEPOSIT_CONTOURS)
       MY_OBS%var(:,:,:) = MY_OBS%mask(:,:,:)
   ! case(OBS_TYPE_DEPOSIT)  
   !    MY_OBS%var(:,:,:) = MY_OBS%mass(:,:,:)
    end select
    !
    return
  end subroutine validation_interpolate_dep_obs
  !
  !---------------------------------------------
  !    subroutine validation_interpolate_dep_pts
  !---------------------------------------------
  !
  !>   @brief
  !>   Get interpolation factors for deposit points
  !
  subroutine validation_interpolate_dep_pts(MY_FILES,MY_OBS,MY_RES,GL_DEP_PTS,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_OBS      list of observation parameters
    !>   @param MY_RES      list of model results parameters
    !>   @param GL_DEP_PTS  deposit points data
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(IN   ) :: MY_RES
    type(DEP_PTS),        intent(IN   ) :: GL_DEP_PTS
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    type(Q1_GRID) :: OBS_MESH
    integer(ip)   :: ipts,ix,iy,my_npoin,gl_npoin(1),lulog
    real(rp)      :: xp,yp,s,t
    real(rp)      :: my_lonmin,my_lonmax,my_latmin,my_latmax,glonmin,glatmin
    real(rp)      :: dlon,dlat,inv_dlon,inv_dlat
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_interpolate_dep_pts'
    MY_ERR%message = ' '
    !
    MY_OBS%nt          = 1
    MY_OBS%tracer_code = GL_DEP_PTS%tracer_code
    !
    MY_OBS%start_year  = MY_RES%start_year
    MY_OBS%start_month = MY_RES%start_month
    MY_OBS%start_day   = MY_RES%start_day
    !
    allocate(MY_OBS%timesec(MY_OBS%nt))
    MY_OBS%timesec(:) = MY_RES%timesec(MY_RES%nt)
    !
    MY_OBS%npts = GL_DEP_PTS%npts
    allocate(MY_OBS%mass_pts(MY_OBS%npts,MY_OBS%nt))
    allocate(MY_OBS%var_pts (MY_OBS%npts,MY_OBS%nt))
    MY_OBS%mass_pts(:,1) = GL_DEP_PTS%mass(:)
    !
    !*** Allocate memory
    !
    allocate(MY_OBS%ipts(MY_OBS%npts))
    allocate(MY_OBS%jpts(MY_OBS%npts))
    allocate(MY_OBS%spts(MY_OBS%npts))
    allocate(MY_OBS%tpts(MY_OBS%npts))
    !  
    MY_OBS%ipts(:) = 0   ! not in my domain
    MY_OBS%jpts(:) = 0
    MY_OBS%spts(:) = 0.0_rp
    MY_OBS%tpts(:) = 0.0_rp
    !
    !*** Get grid and subdomain limits
    ! 
    my_lonmin = MY_RES%lon_c(my_ibs)
    my_lonmax = MY_RES%lon_c(my_ibe)
    my_latmin = MY_RES%lat_c(my_jbs)
    my_latmax = MY_RES%lat_c(my_jbe)
    !
    glonmin = MY_RES%lon_c(1)
    if(glonmin.ge.180.0_rp) glonmin = glonmin - 360.0_rp
    glatmin = MY_RES%lat_c(1)
    !
    inv_dlon = 1.0_rp / (MY_RES%lon_c(my_ibs+1)-MY_RES%lon_c(my_ibs))
    inv_dlat = 1.0_rp / (MY_RES%lat_c(my_jbs+1)-MY_RES%lat_c(my_jbs))
    !
    !*** Loop over points
    !
    compute_points: do ipts = 1,GL_DEP_PTS%npts
       !
       xp = GL_DEP_PTS%lon(ipts)
       yp = GL_DEP_PTS%lat(ipts)
       !
       ! Note that longitudes are in the range [-180,180) and so is xp
       if(xp.ge.180.0_rp) xp = xp - 360.0_rp
       !
       ! Check latitudes
       if(yp.lt.my_latmin .or. yp.ge.my_latmax) cycle compute_points
       !
       ! Check longitudes (all in [-180,180))
       if(my_lonmin.lt.my_lonmax) then
           if(xp.lt.my_lonmin .or. xp.ge.my_lonmax) cycle compute_points
       else
           if(xp.lt.my_lonmin .and. xp.ge.my_lonmax) cycle compute_points
       end if
       !
       !  compute indexes and interpolation factors
       !
       dlat = yp - glatmin
       dlon = xp - glonmin
       if(dlon.lt.0) dlon = dlon + 360.0_rp  ! meridian crossing
       ix = 1 + int(dlon*inv_dlon)           ! refer to cell boundaries
       iy = 1 + int(dlat*inv_dlat)
       !
       dlat = yp - MY_RES%lat_c(iy)
       dlon = xp - MY_RES%lon_c(ix)
       if(dlon.lt.0) dlon = dlon + 360.0_rp
       s = 2.0_rp * dlon*inv_dlon - 1.0_rp
       t = 2.0_rp * dlat*inv_dlat - 1.0_rp
       !
       !  Interpolation factor along x
       !
       MY_OBS%ipts(ipts) = ix
       MY_OBS%spts(ipts) = s
       !
       !  Interpolation factor along y
       !
       MY_OBS%jpts(ipts) = iy
       MY_OBS%tpts(ipts) = t
       !
    end do compute_points
    !
    !*** Print to log file
    !
    my_npoin      = 0
    do ipts = 1,MY_OBS%npts
       if(MY_OBS%ipts(ipts).gt.0) my_npoin = my_npoin + 1
    end do
    !
    gl_npoin (1) = my_npoin
    call parallel_sum(gl_npoin,COMM_MODEL)
    lulog = MY_FILES%lulog
    !
    if(master_model) write(lulog,10) gl_npoin, 100.0_rp*gl_npoin/GL_DEP_PTS%npts
 10 format(/,&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '      INTERPOLATION OF DEPOSIT POINTS               ',/,   &
         '                                                    ',/,   &
         '----------------------------------------------------',/,   &
         '  Number of points in the model domain    : ',i9,' (',f7.2,' %)')  
    !
    !*** Finally load the observed variable
    !
    select case(MY_OBS%type)
    case(OBS_TYPE_DEPOSIT_POINTS)
       MY_OBS%var_pts(:,:) = MY_OBS%mass_pts(:,:)
    end select
    ! 
    return
  end subroutine validation_interpolate_dep_pts
  !
  !-------------------------------------------
  !    subroutine validation_get_time_factors
  !-------------------------------------------
  !
  !>   @brief
  !>   Get time lag and time interpolation factors
  !
  subroutine validation_get_time_factors(MY_FILES,MY_OBS,MY_RES,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_OBS    list of observation parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    logical           :: found
    character(len=24) :: time_str_obs,time_str_mod1,time_str_mod2
    integer(ip)       :: obs_julian, res_julian,it_o,it_m
    integer(ip)       :: iyr,imo,idy,ihr,imi,ise,lulog
    real(rp)          :: time_lag
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_get_time_factors'
    MY_ERR%message = ' '
    !
    lulog = MY_FILES%lulog     
    !
    !*** Find factors
    !
    allocate(MY_OBS%it_m(MY_OBS%nt))
    allocate(MY_OBS%st_m(MY_OBS%nt))
    !
    do it_o = 1,MY_OBS%nt
       !
       call time_addtime(MY_RES%start_year,       &   ! already refered to model origin
                         MY_RES%start_month,      &
                         MY_RES%start_day,        &
                         0_ip,                    &
                         iyr,imo,idy,ihr,imi,ise, &
                         MY_OBS%timesec(it_o),    &
                         MY_ERR)
       call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str_obs, MY_ERR)

       if( (MY_OBS%timesec(it_o).lt.MY_RES%timesec(1)        ).or. &
           (MY_OBS%timesec(it_o).gt.MY_RES%timesec(MY_RES%nt)) ) then
          MY_OBS%it_m(it_o) = -1
          MY_OBS%st_m(it_o) = 0.0_rp
          !
          if(master_model) write(lulog,10) time_str_obs
  10      format(/,&
                 '  Observations at    :',a,/, &
                 '  Model results      : not found')
       else 
          found = .false.
          it_m  = 0
          do while(.not.found)
             it_m = it_m + 1
             if( (MY_OBS%timesec(it_o).ge.MY_RES%timesec(it_m  )).and. &
                 (MY_OBS%timesec(it_o).le.MY_RES%timesec(it_m+1)) ) then         
               found = .true.
               MY_OBS%it_m(it_o) = it_m
               MY_OBS%st_m(it_o) = (MY_OBS%timesec(it_o)-MY_RES%timesec(it_m))/ &
                                   (MY_RES%timesec(it_m+1)-MY_RES%timesec(it_m))
             end if
          end do
          !
          call time_addtime(MY_RES%start_year,       &
                            MY_RES%start_month,      &
                            MY_RES%start_day,        &
                            0_ip,                    &
                            iyr,imo,idy,ihr,imi,ise, &
                            MY_RES%timesec(it_m),    &
                            MY_ERR)
          call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str_mod1, MY_ERR)
          !
          call time_addtime(MY_RES%start_year,       &
                            MY_RES%start_month,      &
                            MY_RES%start_day,        &
                            0_ip,                    &
                            iyr,imo,idy,ihr,imi,ise, &
                            MY_RES%timesec(it_m+1),    &
                            MY_ERR)
          call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str_mod2, MY_ERR)
          !
          if(master_model) write(lulog,20) time_str_obs,time_str_mod1,time_str_mod2,MY_OBS%st_m(it_o)
  20      format(/,&
                 '  Observations at    :',a,/, &
                 '  Model results from :',a,/,&
                 '                  to :',a,' (s=',f6.3,')')
       end if
       !
    end do
    !
    return
  end subroutine validation_get_time_factors
  ! 
  !-----------------------------------------
  !    subroutine validation_type_grid
  !-----------------------------------------
  !
  !>   @brief
  !>   Performs validation with gridded data. It applies to both satellite and deposit observations
  !
  subroutine validation_type_grid(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_VAL    list of validation    parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=s_file)  :: file_inp,name_nc,var_name
    logical                :: var_exists
    integer(ip)            :: lulog,ispe,it,ith
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_type_grid'
    MY_ERR%message = ' '
    !
    lulog = MY_FILES%lulog
    !
    !*** Decide which model variable has to be read
    ! 
    select case(MY_OBS%type)
    case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
       var_name = TRIM(col_nc_name)
    case(OBS_TYPE_DEPOSIT_CONTOURS)
       var_name = TRIM(grn_nc_name)
    end select 
    !
    !*** Loop over species (currently limited to one option only)  
    !
    do ispe = 1,nspe_max
       if(MY_OBS%tracer_code.ne.ispe) cycle
       !
       if(master_model) write(lulog,10) SPE_TAG(ispe), TRIM(var_name)
10     format(&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '       VALIDATION FOR ',a,' WITH ',a                 ,/,   &
         '                                                    ',/,   &
         '----------------------------------------------------')
       !
       !*** Type of run (single/ensemble)
       !
       select case(MY_RES%type)
       case(RES_TYPE_SINGLE_RUN)
          !
          !*** 1. Metrics for deterministic col_mass/grn_load
          !
          select case(MY_OBS%type)
          case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
             MY_VAL%var_name         = TRIM(col_nc_name)
          case(OBS_TYPE_DEPOSIT_CONTOURS)
             MY_VAL%var_name         = TRIM(grn_nc_name)
          end select 
          MY_VAL%metric_histogram    = .false.
          MY_VAL%metric_categoric    = .true.
          MY_VAL%metric_brier        = .false.
          select case(MY_OBS%type)
          case(OBS_TYPE_SATELLITE_DETECTION)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_SATELLITE_RETRIEVAL)
             MY_VAL%metric_quantitative_grid = .true.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_DEPOSIT_CONTOURS)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          end select 
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)  
          call validation_get_my_results_grid(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,MY_RES%nth)
          !    
          if(var_exists) then
             !
             !  get contours
             !
             call validation_allocate      (MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             do ith = 1,MY_RES%nth
                do it  = 1,MY_OBS%nt 
                   if(MY_OBS%it_m(it).lt.0) cycle  
                   call validation_get_prob_con(MY_VAL%threshold(ith),MY_RES%var (:,:,it,ith),MY_RES%prob(:,:,it,ith))
                   select case(MY_OBS%type)
                   case(OBS_TYPE_SATELLITE_DETECTION) 
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   case(OBS_TYPE_SATELLITE_RETRIEVAL)
                      call validation_get_prob_con(MY_VAL%threshold(ith),MY_OBS%mass(:,:,it    ),MY_OBS%prob(:,:,it,ith))
                   case(OBS_TYPE_DEPOSIT_CONTOURS)
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   end select 
                end do
             end do
             ! 
             call validation_get_metrics(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR) ! Compute and output metrics
             !
          end if ! if(var_exists) 
          !
       case(RES_TYPE_ENSEMBLE_RUN)
          !
          !*** 1. Talagrand histogram
          !
          MY_VAL%var_name                 = 'ensemble_histogram'
          MY_VAL%metric_categoric         = .false.
          MY_VAL%metric_brier             = .false.
          MY_VAL%metric_quantitative_grid = .false.
          MY_VAL%metric_quantitative_pts  = .false.
          select case(MY_OBS%type)
          case(OBS_TYPE_SATELLITE_DETECTION)
             MY_VAL%metric_histogram    = .false.  
          case(OBS_TYPE_SATELLITE_RETRIEVAL)
             MY_VAL%metric_histogram    = .true.  
          case(OBS_TYPE_DEPOSIT_CONTOURS)
             MY_VAL%metric_histogram    = .false.  
          end select 
          MY_RES%nth      = MY_RES%nens
          MY_VAL%nens     = MY_RES%nens
          !
          if(MY_VAL%metric_histogram) then
             name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)
             call validation_get_my_results_grid(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,MY_RES%nth)
             !    
             if(var_exists) then
                call validation_get_histogram_grid(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR) ! Compute and output ensemble histogram
             end if
          end if
          !
          !*** 2. Metrics for ensemble_mean 
          !
          MY_VAL%var_name            = 'ensemble_mean'
          MY_VAL%metric_histogram    = .false.
          MY_VAL%metric_categoric    = .true.
          MY_VAL%metric_brier        = .false.
          select case(MY_OBS%type)
          case(OBS_TYPE_SATELLITE_DETECTION)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_SATELLITE_RETRIEVAL)
             MY_VAL%metric_quantitative_grid = .true.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_DEPOSIT_CONTOURS)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          end select 
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)//TRIM(ens_mean_nc_name)  
          call validation_get_my_results_grid(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,MY_RES%nth)
          !    
          if(var_exists) then
             !
             !  get contours
             !
             call validation_allocate(MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             do ith = 1,MY_RES%nth
                do it  = 1,MY_OBS%nt 
                   if(MY_OBS%it_m(it).lt.0) cycle  
                   call validation_get_prob_con(MY_VAL%threshold(ith),MY_RES%var (:,:,it,ith),MY_RES%prob(:,:,it,ith))
                   select case(MY_OBS%type)
                   case(OBS_TYPE_SATELLITE_DETECTION) 
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   case(OBS_TYPE_SATELLITE_RETRIEVAL)
                      call validation_get_prob_con(MY_VAL%threshold(ith),MY_OBS%mass(:,:,it    ),MY_OBS%prob(:,:,it,ith))
                   case(OBS_TYPE_DEPOSIT_CONTOURS)
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   end select 
                end do
             end do
             ! 
             call validation_get_metrics(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR) ! Compute and output metrics
             !
          end if ! if(var_exists) 
          !
          !*** 3. Metrics for ensemble_logmean 
          !
          MY_VAL%var_name            = 'ensemble_logmean'
          MY_VAL%metric_histogram    = .false.
          MY_VAL%metric_categoric    = .true.
          MY_VAL%metric_brier        = .false.
          select case(MY_OBS%type)
          case(OBS_TYPE_SATELLITE_DETECTION)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_SATELLITE_RETRIEVAL)
             MY_VAL%metric_quantitative_grid = .true.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_DEPOSIT_CONTOURS)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          end select 
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)//TRIM(ens_logmean_nc_name)  
          call validation_get_my_results_grid(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,MY_RES%nth)
          !    
          if(var_exists) then
             !
             !  get contours
             !
             call validation_allocate(MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             do ith = 1,MY_RES%nth
                do it  = 1,MY_OBS%nt 
                   if(MY_OBS%it_m(it).lt.0) cycle  
                   call validation_get_prob_con(MY_VAL%threshold(ith),MY_RES%var (:,:,it,ith),MY_RES%prob(:,:,it,ith))
                   select case(MY_OBS%type)
                   case(OBS_TYPE_SATELLITE_DETECTION) 
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   case(OBS_TYPE_SATELLITE_RETRIEVAL)
                      call validation_get_prob_con(MY_VAL%threshold(ith),MY_OBS%mass(:,:,it    ),MY_OBS%prob(:,:,it,ith))
                   case(OBS_TYPE_DEPOSIT_CONTOURS)
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   end select 
                end do
             end do
             ! 
             call validation_get_metrics(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR) ! Compute and output metrics
             !
          end if ! if(var_exists) 
          !
          !*** 4. Metrics for ensemble_median 
          !
          MY_VAL%var_name            = 'ensemble_median'
          MY_VAL%metric_histogram    = .false.
          MY_VAL%metric_categoric    = .true.
          MY_VAL%metric_brier        = .false.
          select case(MY_OBS%type)
          case(OBS_TYPE_SATELLITE_DETECTION)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_SATELLITE_RETRIEVAL)
             MY_VAL%metric_quantitative_grid = .true.
             MY_VAL%metric_quantitative_pts  = .false.
          case(OBS_TYPE_DEPOSIT_CONTOURS)
             MY_VAL%metric_quantitative_grid = .false.
             MY_VAL%metric_quantitative_pts  = .false.
          end select 
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)//TRIM(ens_median_nc_name)
          call validation_get_my_results_grid(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)
          !
          if(var_exists) then
             !
             !  get contours
             !
             call validation_allocate(MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             do ith = 1,MY_RES%nth
                do it  = 1,MY_OBS%nt 
                   if(MY_OBS%it_m(it).lt.0) cycle  
                   call validation_get_prob_con(MY_VAL%threshold(ith),MY_RES%var (:,:,it,ith),MY_RES%prob(:,:,it,ith))
                   select case(MY_OBS%type)
                   case(OBS_TYPE_SATELLITE_DETECTION) 
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   case(OBS_TYPE_SATELLITE_RETRIEVAL)
                      call validation_get_prob_con(MY_VAL%threshold(ith),MY_OBS%mass(:,:,it    ),MY_OBS%prob(:,:,it,ith))
                   case(OBS_TYPE_DEPOSIT_CONTOURS)
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   end select 
                end do
             end do
             ! 
             call validation_get_metrics(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR) ! Compute and output metrics metrics
             !
          end if ! if(var_exists) 
          !
          !*** 5. Metrics for probabilities 
          !
          MY_VAL%var_name                 = 'ensemble_prob'
          MY_VAL%metric_histogram         = .false.
          MY_VAL%metric_categoric         = .true.
          MY_VAL%metric_brier             = .true.
          MY_VAL%metric_quantitative_grid = .false.
          MY_VAL%metric_quantitative_pts  = .false.
          !
          select case(MY_OBS%type)
          case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
              if(MY_OBS%tracer_code.eq.SPE_SO2) then
                 MY_RES%nth = MY_RES%nth_col_mass_DU
                 MY_VAL%nth = MY_RES%nth_col_mass_DU
              else
                 MY_RES%nth = MY_RES%nth_col_mass
                 MY_VAL%nth = MY_RES%nth_col_mass
              end if
          case(OBS_TYPE_DEPOSIT_CONTOURS)
              MY_RES%nth = MY_RES%nth_grn_load
              MY_VAL%nth = MY_RES%nth_grn_load
          end select     
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)//TRIM(ens_prb_nc_name)
          call validation_get_my_results_grid(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists,.false.)
          !
          MY_RES%var = MY_RES%var * 1e-2_rp  ! from 100% to (0,1). Note that units are not converted when reading results
          !
          if(var_exists) then
             !
             !  get contours
             !
             call validation_allocate(MY_OBS,MY_RES,MY_VAL) 
             call validation_res_thresholds(MY_OBS,MY_RES,MY_VAL) 
             do ith = 1,MY_RES%nth
                do it  = 1,MY_OBS%nt 
                   if(MY_OBS%it_m(it).lt.0) cycle  
                   MY_RES%prob(:,:,it,ith) = MY_RES%var (:,:,it,ith) 
                   select case(MY_OBS%type)
                   case(OBS_TYPE_SATELLITE_DETECTION) 
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   case(OBS_TYPE_SATELLITE_RETRIEVAL)
                      call validation_get_prob_con(MY_VAL%threshold(ith),MY_OBS%mass(:,:,it    ),MY_OBS%prob(:,:,it,ith))
                   case(OBS_TYPE_DEPOSIT_CONTOURS)
                      MY_OBS%prob(:,:,it,ith) = MY_OBS%mask(:,:,it)
                   end select 
                end do
             end do
             ! 
             call validation_get_metrics(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR) ! Compute and output metrics metrics
             !
          end if ! if(var_exists) 
          !
       end select
       !
    end do  
    !
    return
  end subroutine validation_type_grid
  ! 
  !-----------------------------------------
  !    subroutine validation_type_pts
  !-----------------------------------------
  !
  !>   @brief
  !>   Performs validation with points data
  !
  subroutine validation_type_pts(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_OBS      list of observation   parameters
    !>   @param MY_RES      list of model results parameters
    !>   @param MY_VAL      list of validation    parameters
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=s_file)  :: file_inp,name_nc,var_name
    logical                :: var_exists
    integer(ip)            :: lulog,ispe
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_type_pts'
    MY_ERR%message = ' '
    !
    lulog = MY_FILES%lulog
    !
    var_name = TRIM(grn_nc_name) ! model variable to be read
    !
    !*** Loop over species (currently limited to one option only)  
    !
    do ispe = 1,nspe_max
       if(MY_OBS%tracer_code.ne.ispe) cycle
       !
       if(master_model) write(lulog,10) SPE_TAG(ispe), TRIM(var_name)
10     format(&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '       VALIDATION FOR ',a,' WITH ',a                 ,/,   &
         '                                                    ',/,   &
         '----------------------------------------------------')
       !
       !*** Type of run (single/ensemble)
       !
       select case(MY_RES%type)
       case(RES_TYPE_SINGLE_RUN)
          !
          !*** 1. Metrics for deterministic grn_load
          !
          MY_VAL%var_name                 = TRIM(grn_nc_name) 
          MY_VAL%metric_histogram         = .false.
          MY_VAL%metric_categoric         = .false.
          MY_VAL%metric_brier             = .false.
          MY_VAL%metric_quantitative_grid = .false.
          MY_VAL%metric_quantitative_pts  = .true.
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)  
          call validation_get_my_results_pts(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var_pts(MY_OBS%npts,MY_OBS%nt)
          !
          if(var_exists) then
             call validation_allocate      (MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             call validation_get_metrics   (MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)  ! Compute and output metrics
          end if
          !
       case(RES_TYPE_ENSEMBLE_RUN)
          !
          !*** 1. Talagrand histogram
          !
          MY_VAL%var_name                 = 'ensemble_histogram'
          MY_VAL%metric_histogram         = .true.
          MY_VAL%metric_categoric         = .false.
          MY_VAL%metric_brier             = .false.
          MY_VAL%metric_quantitative_grid = .false.
          MY_VAL%metric_quantitative_pts  = .false.
          MY_RES%nth  = MY_RES%nens
          MY_VAL%nens = MY_RES%nens
          !
          if(MY_VAL%metric_histogram) then
             name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)
             call validation_get_my_results_pts(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var_pts(MY_OBS%npts,MY_OBS%nt,MY_RES%nth)
             !    
             if(var_exists) then
                call validation_get_histogram_pts(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR) ! Compute and output ensemble histogram
             end if
          end if
          !
          !*** 2. Metrics for ensemble_mean 
          !
          MY_VAL%var_name                 = 'ensemble_mean'
          MY_VAL%metric_histogram         = .false.
          MY_VAL%metric_categoric         = .false.
          MY_VAL%metric_brier             = .false.
          MY_VAL%metric_quantitative_grid = .false.
          MY_VAL%metric_quantitative_pts  = .true.
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)//TRIM(ens_mean_nc_name)  
          call validation_get_my_results_pts(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var_pts(MY_OBS%npts,MY_OBS%nt,MY_RES%nth)
          !
          if(var_exists) then
             call validation_allocate      (MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             call validation_get_metrics   (MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)  ! Compute and output metrics
          end if
          !
          !*** 3. Metrics for ensemble_logmean 
          !
          MY_VAL%var_name                 = 'ensemble_logmean'
          MY_VAL%metric_histogram         = .false.
          MY_VAL%metric_categoric         = .false.
          MY_VAL%metric_brier             = .false.
          MY_VAL%metric_quantitative_grid = .false.
          MY_VAL%metric_quantitative_pts  = .true.
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)//TRIM(ens_logmean_nc_name)  
          call validation_get_my_results_pts(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var_pts(MY_OBS%npts,MY_OBS%nt)
          !
          if(var_exists) then
             call validation_allocate      (MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             call validation_get_metrics   (MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)  ! Compute and output metrics
          end if
          !
          !*** 4. Metrics for ensemble_median  
          !
          MY_VAL%var_name                 = 'ensemble_median'
          MY_VAL%metric_histogram         = .false.
          MY_VAL%metric_categoric         = .false.
          MY_VAL%metric_brier             = .false.
          MY_VAL%metric_quantitative_grid = .false.
          MY_VAL%metric_quantitative_pts  = .true.
          MY_RES%nth = 1
          MY_VAL%nth = 1
          !
          name_nc = TRIM(SPE_TAG(ispe))//TRIM(var_name)//TRIM(ens_median_nc_name)  
          call validation_get_my_results_pts(name_nc,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)   ! read results in MY_RES%var_pts(MY_OBS%npts,MY_OBS%nt)
          !
          if(var_exists) then
             call validation_allocate      (MY_OBS,MY_RES,MY_VAL)  
             call validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL) 
             call validation_get_metrics   (MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)  ! Compute and output metrics
          end if
          !
       end select
       !
    end do  
    !
    return
  end subroutine validation_type_pts
  !
  !
  !   PRIVATE ROUTINES
  ! 
  !-------------------------------------
  !    subroutine validation_get_metrics
  !-------------------------------------
  !
  !>   @brief
  !>   Compute and output categoric and quantitative metrics
  !
  subroutine validation_get_metrics(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_VAL    list of validation    parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(IN   ) :: MY_OBS
    type(RES_PARAMS),     intent(IN   ) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: file_name
    character(len=24)     :: time_str
    integer(ip)           :: lures,it,ith,ipoin
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_get_metrics'
    MY_ERR%message = ' '
    !
    lures = 90
    !
    select case(MY_OBS%type)
    case(OBS_TYPE_SATELLITE_DETECTION) 
         MY_VAL%obs_name ='sat_det'
    case(OBS_TYPE_SATELLITE_RETRIEVAL)
         MY_VAL%obs_name ='sat_ret'
    case(OBS_TYPE_VAA)
        MY_VAL%obs_name ='vaa'
    case(OBS_TYPE_DEPOSIT_POINTS)
        MY_VAL%obs_name ='dep_pts'
    case(OBS_TYPE_DEPOSIT_CONTOURS)
        MY_VAL%obs_name ='dep_con'
    end select
    !
    !*** Open file
    !
    file_name = TRIM(MY_FILES%commonpath)//'/'//TRIM(MY_FILES%problemname)//'.val.'// &
                TRIM(MY_VAL%obs_name)//'.vs.'//TRIM(MY_VAL%var_name)//'.res'
    if(master_model) then 
       call inpout_open_file(lures, file_name, MY_ERR)
       write(lures,10) TRIM(MY_VAL%var_name),SPE_TAG(MY_OBS%tracer_code),TRIM(MY_VAL%obs_name)
10     format('!                              '  ,/,&
              '! ',a,' metrics for ',a,' with ',a,/,&
              '!                              ')
    end if
    !
    !*** Computes metrics 
    !
    do ith = 1,MY_VAL%nth 
       !
       if(master_model) write(lures,20) MY_VAL%threshold(ith)
20     format('!                        '  ,/,&
              '! Threshold: ',e10.3,'     GFMS    GFAR    GPPV    GPOD    GCCM    BS     NRMSE ',/,&
              '! ------------------------------------------------------------------------------')
       !
       do it  = 1,MY_OBS%nt  
          if(MY_OBS%it_m(it).lt.0) cycle
          !
          !*** 1. Categoric metrics (only for contours)
          !
          select case(MY_VAL%metric_categoric)
          case(.true.)
             !
             !*** Generalized Figure Merit of Space (GFMS)
             !
             call validation_get_GFMS(MY_RES%prob(:,:,it,ith),MY_OBS%prob(:,:,it,ith),MY_RES%Hm1_p,MY_VAL%GFMS)
             !
             !*** Generalized False Alarm Ratio (GFAR) and Generalised Positive Predictive Value (GPPV= 1-GFAR)
             !
             call validation_get_GFAR(MY_RES%prob(:,:,it,ith),MY_OBS%prob(:,:,it,ith),MY_RES%Hm1_p,MY_VAL%GFAR)
             MY_VAL%GPPV = 1.0_rp - MY_VAL%GFAR
             !
             !*** Generalized Generalised Probability of Detection (GPOD)
             !
             call validation_get_GPOD(MY_RES%prob(:,:,it,ith),MY_OBS%prob(:,:,it,ith),MY_RES%Hm1_p,MY_VAL%GPOD)
             !
             !*** Generalised Composite Categorical Metric (GCCM)
             !
             MY_VAL%GCCM = (MY_VAL%GFMS + MY_VAL%GPPV + MY_VAL%GPOD)/3.0_rp
             !
          case(.false.)
             MY_VAL%GFMS = -1.0_rp
             MY_VAL%GPPV = -1.0_rp
             MY_VAL%GFAR = -1.0_rp
             MY_VAL%GPOD = -1.0_rp
             MY_VAL%GCCM = -1.0_rp
          end select
          !
          !*** 2. Brier score
          !
          select case(MY_VAL%metric_brier)
          case(.true.)
             !
             call validation_get_Brier_score(MY_RES%prob(:,:,it,ith),MY_OBS%prob(:,:,it,ith),MY_RES%Hm1_p,MY_VAL%BS)
             !
          case(.false.)         
             MY_VAL%BS   = -1.0_rp
          end select
          !
          !*** 3. Quantitative metrics (on a grid or points). Normalized Root Mean Square Error (NRMSE)
          !
          MY_VAL%NRMSE = -1.0_rp
          if(MY_VAL%metric_quantitative_grid) then
             !
             call validation_get_NRMSE_grid(MY_RES%var(:,:,it,ith),MY_OBS%mass(:,:,it),MY_VAL%NRMSE)  
             !
          end if
          if(MY_VAL%metric_quantitative_pts) then
             !
             call validation_get_NRMSE_pts(MY_OBS%npts,MY_RES%var_pts(:,it,ith),MY_OBS%var_pts(:,it),MY_VAL%NRMSE)  
             !
          end if
          !
          !*** Write results
          !
          call time_addtime(MY_OBS%start_year,       &
                            MY_OBS%start_month,      &
                            MY_OBS%start_day,        &
                            0_ip,                    &
                            iyr,imo,idy,ihr,imi,ise, &
                            MY_OBS%timesec(it),    &
                            MY_ERR)
          call time_dateformat(iyr,imo,idy,ihr,imi,ise,5_ip, time_str, MY_ERR)  ! 00mon-00:00
          !
          if(master_model) write(lures,50) time_str,MY_VAL%GFMS,MY_VAL%GFAR,MY_VAL%GPPV,MY_VAL%GPOD,MY_VAL%GCCM, &
                                                    MY_VAL%BS,  MY_VAL%NRMSE
50        format(a,6(2x,f6.3),2(2x,f6.3))
          !
          if(MY_VAL%metric_quantitative_pts) then
            do ipoin = 1,MY_OBS%npts
               if(master_model) write(lures,60) MY_OBS%var_pts(ipoin,it), MY_RES%var_pts(ipoin,it,ith)
            end do
 60         format(2(2x,f7.2))
          end if
          !
       end do  ! it  = 1,MY_OBS%nt  
    end do     ! ith = 1,MY_VAL%nth 
    !
    if(master_model) close(lures)
    return
  end subroutine validation_get_metrics
  ! 
  !--------------------------------------------
  !    subroutine validation_get_histogram_grid
  !--------------------------------------------
  !
  !>   @brief
  !>   Compute and output rank histogram
  !
  subroutine validation_get_histogram_grid(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_VAL    list of validation    parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(IN   ) :: MY_OBS
    type(RES_PARAMS),     intent(IN   ) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    logical               :: bin_found
    character(len=s_file) :: file_name
    integer(ip)           :: lures,nens,iens,it,ix,iy,jens,suma
    !
    integer(ip), allocatable :: index_o(:),index_e(:)
    real(rp),    allocatable :: rank_e(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_get_histogram_grid'
    MY_ERR%message = ' '
    !
    lures = 90
    nens  = MY_RES%nens
    !
    allocate(MY_VAL%histogram(nens))
    MY_VAL%histogram(:) = 0
    !
    allocate(index_o(nens))
    do iens = 1,nens
       index_o(iens) = iens
    end do
    !
    allocate(index_e(nens))
    allocate(rank_e (nens))
    !
    select case(MY_OBS%type)
    case(OBS_TYPE_SATELLITE_DETECTION) 
         MY_VAL%obs_name ='sat_det'
    case(OBS_TYPE_SATELLITE_RETRIEVAL)
         MY_VAL%obs_name ='sat_ret'
    case(OBS_TYPE_VAA)
         MY_VAL%obs_name ='vaa'
    case(OBS_TYPE_DEPOSIT_POINTS)
         MY_VAL%obs_name ='dep_pts'
    case(OBS_TYPE_DEPOSIT_CONTOURS)
         MY_VAL%obs_name ='dep_con'
    end select
    !
    !*** Open file
    !
    file_name = TRIM(MY_FILES%commonpath)//'/'//TRIM(MY_FILES%problemname)//'.val.'// &
                TRIM(MY_VAL%obs_name)//'.vs.'//TRIM(MY_VAL%var_name)//'.res'

    if(master_model) then 
       call inpout_open_file(lures, file_name, MY_ERR)
       write(lures,10) TRIM(MY_VAL%var_name),SPE_TAG(MY_OBS%tracer_code),TRIM(MY_VAL%obs_name)
10     format('!                              '  ,/,&
              '! ',a,' of observations for ',a,' with ',a,/,&
              '!                              ')
    end if
    !
    !*** Loop over observation time steps and points
    !
    do it  = 1,MY_OBS%nt  
       if(MY_OBS%it_m(it).le.0) cycle
       !
       do ix = my_ips,my_ipe
       do iy = my_jps,my_jpe
          !
          if(MY_OBS%mass(ix,iy,it).gt.EPSILON) then
             !
             !*** Sort ensemble member results by increasing order 
             !
             index_e(:) = index_o(:)
             rank_e (:) = MY_RES%var(ix,iy,it,:)
             call postp_rank_vector(nens, rank_e, index_e, EPSILON, MY_ERR)
             !
             !*** Build the nens bins (at the mid-point)
             !
             !do iens = 1,nens-1
             !   rank_e(iens) = 0.5_rp*(rank_e(iens)+rank_e(iens+1))
             !end do
             !
             !*** Find the observation bin
             !
             jens = nens
             bin_found = .false.
             do iens = 1,nens
                if( (MY_OBS%mass(ix,iy,it).le.rank_e(iens)).and.(jens.eq.nens) ) then
                     jens = iens
                     bin_found = .true.
                end if
             end do
             !
             if(bin_found) MY_val%histogram(jens) = MY_val%histogram(jens) + 1
             !
          end if
          !
      end do
      end do
      !
    end do
    !
    !*** Summ across processors
    !
    call parallel_sum(MY_val%histogram,COMM_WORLD)
    suma = sum(MY_val%histogram)
    !
    !*** Write the file
    !
    if(master_model) then
       do iens = 1,nens
           write(lures,20) iens,MY_val%histogram(iens),100.0_rp*MY_val%histogram(iens)/suma
 20        format(i4,1x,i8,1x,f7.2)
       end do
       close(lures)
    end if
    !
    return
  end subroutine validation_get_histogram_grid
  ! 
  !--------------------------------------------
  !    subroutine validation_get_histogram_pts
  !--------------------------------------------
  !
  !>   @brief
  !>   Compute and output rank histogram for points
  !
  subroutine validation_get_histogram_pts(MY_FILES,MY_OBS,MY_RES,MY_VAL,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_VAL    list of validation    parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(OBS_PARAMS),     intent(IN   ) :: MY_OBS
    type(RES_PARAMS),     intent(IN   ) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: file_name
    integer(ip)           :: lures,nens,iens,it,ipoin,jens,suma
    !
    integer(ip), allocatable :: index_o(:),index_e(:)
    real(rp),    allocatable :: rank_e(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_get_histogram_pts'
    MY_ERR%message = ' '
    !
    lures = 90
    nens  = MY_RES%nens
    !
    allocate(MY_VAL%histogram(nens))
    MY_VAL%histogram(:) = 0
    !
    allocate(index_o(nens))
    do iens = 1,nens
       index_o(iens) = iens
    end do
    !
    allocate(index_e(nens))
    allocate(rank_e (nens))
    !
    select case(MY_OBS%type)
    case(OBS_TYPE_SATELLITE_DETECTION) 
         MY_VAL%obs_name ='sat_det'
    case(OBS_TYPE_SATELLITE_RETRIEVAL)
         MY_VAL%obs_name ='sat_ret'
    case(OBS_TYPE_VAA)
        MY_VAL%obs_name ='vaa'
    case(OBS_TYPE_DEPOSIT_POINTS)
        MY_VAL%obs_name ='dep_pts'
    case(OBS_TYPE_DEPOSIT_CONTOURS)
        MY_VAL%obs_name ='dep_con'
    end select
    !
    !*** Open file
    !
    file_name = TRIM(MY_FILES%commonpath)//'/'//TRIM(MY_FILES%problemname)//'.val.'// &
                TRIM(MY_VAL%obs_name)//'.vs.'//TRIM(MY_VAL%var_name)//'.res'

    if(master_model) then 
       call inpout_open_file(lures, file_name, MY_ERR)
       write(lures,10) TRIM(MY_VAL%var_name),SPE_TAG(MY_OBS%tracer_code),TRIM(MY_VAL%obs_name)
10     format('!                              '  ,/,&
              '! ',a,' of observations for ',a,' with ',a,/,&
              '!                              ')
    end if
    !
    !*** Loop over observation time steps and points
    !
    do it  = 1,MY_OBS%nt  
       if(MY_OBS%it_m(it).le.0) cycle
       !
       do ipoin = 1,MY_OBS%npts
          !
          if(MY_OBS%mass_pts(ipoin,it).gt.0.0_rp) then
             !
             !*** Sort ensemble member results by increasing order 
             !
             index_e(:) = index_o(:)
             rank_e (:) = MY_RES%var_pts(ipoin,it,:)
             call postp_rank_vector(nens, rank_e, index_e, EPSILON, MY_ERR)
             !
             !*** Build the nens bins (at the mid-point)
             !
             do iens = 1,nens-1
                rank_e(iens) = 0.5_rp*(rank_e(iens)+rank_e(iens+1))
             end do
             !
             !*** Find the observation bin
             !
             jens = nens
             do iens = 1,nens
                if( (MY_OBS%mass_pts(ipoin,it).le.rank_e(iens)).and.(jens.eq.nens) ) jens = iens
             end do
             !iens = index_e(jens)
             MY_val%histogram(jens) = MY_val%histogram(jens) + 1
             !
          end if
          !
      end do
      !
    end do
    !
    !*** Write the file
    !
    suma = sum(MY_val%histogram)
    if(master_model) then
       do iens = 1,nens
           write(lures,20) iens,MY_val%histogram(iens),100.0_rp*MY_val%histogram(iens)/suma
 20        format(i4,1x,i8,1x,f7.2)
       end do
       close(lures)
    end if
    !
    return
  end subroutine validation_get_histogram_pts
  ! 
  !-----------------------------------------
  !    subroutine validation_allocate
  !-----------------------------------------
  !
  !>   @brief
  !>   Allocates memory for probablity contours
  !
  subroutine validation_allocate(MY_OBS,MY_RES,MY_VAL)
    implicit none
    !
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_VAL    list of validation    parameters
    !
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    !
    if(allocated(MY_OBS%prob)) deallocate(MY_OBS%prob)
    allocate(MY_OBS%prob(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,MY_RES%nth))
    MY_OBS%prob = 0.0_rp
    !
    if(allocated(MY_RES%prob)) deallocate(MY_RES%prob)
    allocate(MY_RES%prob(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,MY_RES%nth))
    MY_RES%prob = 0.0_rp
    !
    if(allocated(MY_VAL%threshold)) deallocate(MY_VAL%threshold)
    allocate(MY_VAL%threshold(MY_VAL%nth))
    MY_VAL%threshold = 0.0_rp
    !
    return
  end subroutine validation_allocate
  ! 
  !-----------------------------------------
  !    subroutine validation_obs_thresholds
  !-----------------------------------------
  !
  !>   @brief
  !>   Sets observation thresholds for validation 
  !
  subroutine validation_obs_thresholds(MY_OBS,MY_RES,MY_VAL)
    implicit none
    !
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_VAL    list of validation    parameters
    !
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    !
    integer(ip) :: ith
    !
    do ith = 1,MY_RES%nth
       select case(MY_OBS%type)
       case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
         select case(MY_OBS%tracer_code)  
         case(SPE_SO2)
            MY_VAL%threshold(ith) = MY_OBS%col_mass_obs_threshold_DU
         case default
            MY_VAL%threshold(ith) = MY_OBS%col_mass_obs_threshold
         end select
       case(OBS_TYPE_DEPOSIT_CONTOURS,OBS_TYPE_DEPOSIT_POINTS)
            MY_VAL%threshold(ith) = MY_OBS%grn_load_obs_threshold    
       end select 
    end do  
    !
    return
  end subroutine validation_obs_thresholds
  ! 
  !-----------------------------------------
  !    subroutine validation_res_thresholds
  !-----------------------------------------
  !
  !>   @brief
  !>   Sets results (model) thresholds for validation 
  !
  subroutine validation_res_thresholds(MY_OBS,MY_RES,MY_VAL)
    implicit none
    !
    !>   @param MY_OBS    list of observation   parameters
    !>   @param MY_RES    list of model results parameters
    !>   @param MY_VAL    list of validation    parameters
    !
    type(OBS_PARAMS),     intent(INOUT) :: MY_OBS
    type(RES_PARAMS),     intent(INOUT) :: MY_RES
    type(VAL_PARAMS),     intent(INOUT) :: MY_VAL
    !
    integer(ip) :: ith
    !
    do ith = 1,MY_RES%nth
       select case(MY_OBS%type)
       case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
         select case(MY_OBS%tracer_code)  
         case(SPE_SO2)
            MY_VAL%threshold(ith) = MY_RES%th_col_mass_DU(ith)
         case default
            MY_VAL%threshold(ith) = MY_RES%th_col_mass(ith)
         end select
       case(OBS_TYPE_DEPOSIT_CONTOURS,OBS_TYPE_DEPOSIT_POINTS)
            MY_VAL%threshold(ith) = MY_RES%th_grn_load(ith)
       end select 
    end do  
    !
    return
  end subroutine validation_res_thresholds
  !
  !---------------------------------------------
  !    subroutine validation_get_my_results_grid
  !----------------------------------------------
  !
  subroutine validation_get_my_results_grid(var_name,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists,convert_flag)
    implicit none
    !
    character(len=* ),  intent(IN   ) :: var_name
    type(FILE_LIST),    intent(IN   ) :: MY_FILES
    type(RES_PARAMS),   intent(INOUT) :: MY_RES
    type(OBS_PARAMS),   intent(IN   ) :: MY_OBS
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    logical,            intent(INOUT) :: var_exists
    logical,optional,   intent(IN   ) :: convert_flag
    !
    logical               :: convert_units
    character(len=s_file) :: file_res
    integer(ip)           :: lulog,istat,ncID,varID,ndims
    integer(ip)           :: it,ix,iy,it_m,ith
    integer(ip)           :: nbx,nby,nt,nth
    real(rp)              :: st_m,var1,var2
    !
    real(rp), allocatable :: gl_work2(:,:)
    real(rp), allocatable :: gl_work3(:,:,:)
    real(rp), allocatable :: gl_work4(:,:,:,:)
    real(rp), allocatable :: my_work4(:,:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_get_my_results_grid'
    MY_ERR%message = ' '
    !
    lulog      = MY_FILES%lulog
    file_res   = TRIM(MY_RES%file_name)
    var_exists = .false.
    if(present(convert_flag)) then
       convert_units = convert_flag
    else
       convert_units = .true.
    end if
    !
    nbx = MY_RES%nbx
    nby = MY_RES%nby
    nt  = MY_RES%nt
    nth = MY_RES%nth
    !
    !*** Allocates
    !
    if(allocated(MY_RES%var)) deallocate(MY_RES%var)
    allocate(MY_RES%var(my_ips:my_ipe,my_jps:my_jpe,MY_OBS%nt,nth))
    MY_RES%var(:,:,:,:) = 0.0_rp
    !
    !*** Check if the variable exists
    !
    if(master_model) then
       istat = nf90_open(file_res,NF90_NOWRITE, ncID)
       istat = nf90_inq_varid(ncID,var_name,varID)
       if(istat.eq.0) then
          var_exists = .true.
          istat = nf90_inquire_variable(ncID, varID, ndims=ndims)
       end if    
       istat = nf90_close(ncID)
       write(lulog,10) TRIM(var_name),var_exists
 10    format('  Reading results for variable : ',a,/,&
              '                variable found : ',l)
    end if
    call parallel_bcast(var_exists,1,0)
    if(.not.var_exists) return 
    !
    !*** Master reads the global results
    !
    if(master_model) then
       allocate(gl_work4(nbx,nby,nth,nt))
       if(ndims.eq.3) then
          allocate(gl_work3(nbx,nby,nt))
          call nc_IO_read_variable(file_res, var_name, gl_work3, nbx, nby, nt, MY_ERR)
          gl_work4(:,:,1,:) = gl_work3(:,:,:)
          deallocate(gl_work3)
      else if(ndims.eq.4) then
          call nc_IO_read_variable(file_res, var_name, gl_work4, nbx, nby, nth, nt, MY_ERR)
      end if
    end if
    !
    !*** Broadcast of results and interpolation 
    !
    if(master_model) then
      allocate(gl_work2(nbx,nby   ))
     else
      allocate(gl_work2(1,1))
    end if
    allocate(my_work4(my_ibs:my_ibe,my_jbs:my_jbe,nt,nth))
    !
    do ith = 1,nth
    do it  = 1,nt
       if(master_model) gl_work2(:,:) = gl_work4(:,:,ith,it)
       call domain_scatter_corner_points_0halo_2D(gl_work2,nbx,nby,my_work4(:,:,it,ith))
    end do
    end do
    !
    !*** Interpolate at mass points and observation time instants
    !
    do ith = 1,nth
    do it  = 1,MY_OBS%nt
       it_m = MY_OBS%it_m(it)
       st_m = MY_OBS%st_m(it)
       !
       if(it_m.gt.0) then
          do ix = my_ibs,my_ibe-1
          do iy = my_jbs,my_jbe-1
             var1 = 0.25_rp*(my_work4(ix  ,iy  ,it_m  ,ith) + &
                             my_work4(ix+1,iy  ,it_m  ,ith) + &
                             my_work4(ix+1,iy+1,it_m  ,ith) + &
                             my_work4(ix  ,iy+1,it_m  ,ith))
             var2 = 0.25_rp*(my_work4(ix  ,iy  ,it_m+1,ith) + &
                             my_work4(ix+1,iy  ,it_m+1,ith) + &
                             my_work4(ix+1,iy+1,it_m+1,ith) + &
                             my_work4(ix  ,iy+1,it_m+1,ith))
             MY_RES%var(ix,iy,it,ith) = (1.0_rp-st_m)*var1 + st_m*var2
         end do
         end do
       end if
       !
    end do
    end do
    !
    !*** Finally, convert model result units to refer always to IS
    !
    if(convert_units) then
       select case(MY_OBS%type)
       case(OBS_TYPE_SATELLITE_DETECTION, OBS_TYPE_SATELLITE_RETRIEVAL)
            select case(MY_OBS%tracer_code)  
            case(SPE_SO2)
               MY_RES%var = MY_RES%var * 64.0_rp / 2.238e3_rp / 1e3_rp ! DU --> g/m2 --> kg/m2  
            case default
               MY_RES%var = MY_RES%var * 1e-3_rp ! g/m2 --> kg/m2
            end select
       case(OBS_TYPE_DEPOSIT_CONTOURS)
           ! already in kg/m2
       end select 
    end if
    !
    return
  end subroutine validation_get_my_results_grid
  !
  !--------------------------------------------
  !    subroutine validation_get_my_results_pts
  !--------------------------------------------
  !
  subroutine validation_get_my_results_pts(var_name,MY_FILES,MY_RES,MY_OBS,MY_ERR,var_exists)
    implicit none
    !
    character(len=* ),  intent(IN   ) :: var_name
    type(FILE_LIST),    intent(IN   ) :: MY_FILES
    type(RES_PARAMS),   intent(INOUT) :: MY_RES
    type(OBS_PARAMS),   intent(IN   ) :: MY_OBS
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    logical,            intent(INOUT) :: var_exists
    !
    character(len=s_file) :: file_res
    integer(ip)           :: lulog,istat,ncID,varID,ndims
    integer(ip)           :: it,ix,iy,it_m,ith,ipoin
    integer(ip)           :: nbx,nby,nt,nth,npts
    real(rp)              :: st_m,s,t,st,my_shape(4),var1,var2
    !
    real(rp), allocatable :: gl_work2(:,:)
    real(rp), allocatable :: gl_work3(:,:,:)
    real(rp), allocatable :: gl_work4(:,:,:,:)
    real(rp), allocatable :: my_work4(:,:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'validation_get_my_results_pts'
    MY_ERR%message = ' '
    !
    lulog      = MY_FILES%lulog
    file_res   = TRIM(MY_RES%file_name)
    var_exists = .false.
    !
    nbx  = MY_RES%nbx
    nby  = MY_RES%nby
    nt   = MY_RES%nt
    nth  = MY_RES%nth
    npts = MY_OBS%npts
    !
    !*** Allocates
    !
    if(allocated(MY_RES%var_pts)) deallocate(MY_RES%var_pts)
    allocate(MY_RES%var_pts(MY_OBS%npts,MY_OBS%nt,nth))
    MY_RES%var_pts(:,:,:) = 0.0_rp
    !
    !*** Check if the variable exists
    !
    if(master_model) then
       istat = nf90_open(file_res,NF90_NOWRITE, ncID)
       istat = nf90_inq_varid(ncID,var_name,varID)
       if(istat.eq.0) then
          var_exists = .true.
          istat = nf90_inquire_variable(ncID, varID, ndims=ndims)
       end if    
       istat = nf90_close(ncID)
       write(lulog,10) TRIM(var_name),var_exists
 10    format('  Reading results for variable : ',a,/,&
              '                variable found : ',l)
    end if
    call parallel_bcast(var_exists,1,0)
    if(.not.var_exists) return 
    !
    !*** Master reads the global results
    !
    if(master_model) then
       allocate(gl_work4(nbx,nby,nth,nt))
       if(ndims.eq.3) then
          allocate(gl_work3(nbx,nby,nt))
          call nc_IO_read_variable(file_res, var_name, gl_work3, nbx, nby, nt, MY_ERR)
          gl_work4(:,:,1,:) = gl_work3(:,:,:)
          deallocate(gl_work3)
      else if(ndims.eq.4) then
          call nc_IO_read_variable(file_res, var_name, gl_work4, nbx, nby, nth, nt, MY_ERR)
      end if
    end if
    !
    !*** Broadcast of results and interpolation 
    !
    if(master_model) then
      allocate(gl_work2(nbx,nby   ))
     else
      allocate(gl_work2(1,1))
    end if
    allocate(my_work4(my_ibs:my_ibe,my_jbs:my_jbe,nt,nth))
    !
    do ith = 1,nth
    do it  = 1,nt
       if(master_model) gl_work2(:,:) = gl_work4(:,:,ith,it)
       call domain_scatter_corner_points_0halo_2D(gl_work2,nbx,nby,my_work4(:,:,it,ith))
    end do
    end do
    !
    !*** Interpolate at observation points and observation time instants
    !
    do ith = 1,nth
    do it  = 1,MY_OBS%nt
       it_m = MY_OBS%it_m(it)
       st_m = MY_OBS%st_m(it)
       !
       if(it_m.gt.0) then
          do ipoin = 1,npts
             if(MY_OBS%ipts(ipoin).gt.0) then  ! I host the point in my subdomain
                ix = MY_OBS%ipts(ipoin)
                iy = MY_OBS%jpts(ipoin)
                s  = MY_OBS%spts(ipoin)
                t  = MY_OBS%tpts(ipoin)
                st = s*t
                my_shape(1) = (1.0_rp-t-s+st)*0.25_rp                   !  4      3
                my_shape(2) = (1.0_rp-t+s-st)*0.25_rp                   !
                my_shape(3) = (1.0_rp+t+s+st)*0.25_rp                   !
                my_shape(4) = (1.0_rp+t-s-st)*0.25_rp                   !  1      2    
                !
                var1 = my_shape(1)*my_work4(ix  ,iy  ,it_m  ,ith) + &
                       my_shape(2)*my_work4(ix+1,iy  ,it_m  ,ith) + &
                       my_shape(3)*my_work4(ix+1,iy+1,it_m  ,ith) + &
                       my_shape(4)*my_work4(ix  ,iy+1,it_m  ,ith)
                var2 = my_shape(1)*my_work4(ix  ,iy  ,it_m+1,ith) + &
                       my_shape(2)*my_work4(ix+1,iy  ,it_m+1,ith) + &
                       my_shape(3)*my_work4(ix+1,iy+1,it_m+1,ith) + &
                       my_shape(4)*my_work4(ix  ,iy+1,it_m+1,ith)
                MY_RES%var_pts(ipoin,it,ith) = (1.0_rp-st_m)*var1 + st_m*var2
              end if
           end do
        end if
        !
     end do
     end do
     !
     !*** Finally, summ accross processors
     !
     call parallel_sum(MY_RES%var_pts,COMM_MODEL)
     !
     return
  end subroutine validation_get_my_results_pts
  ! 
  !-----------------------------------------
  !    subroutine validation_get_prob_con
  !-----------------------------------------
  !
  subroutine validation_get_prob_con(threshold,var,prob)
    implicit none
    !
    real(rp), intent(IN   ) :: threshold
    real(rp), intent(IN   ) :: var (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(INOUT) :: prob(my_ips:my_ipe,my_jps:my_jpe)
    !
    integer(ip) :: i,j
    !
    !*** Initializations
    !
    prob(:,:) = 0.0_rp
    !
    do j   = my_jps,my_jpe
    do i   = my_ips,my_ipe
       if(var(i,j).ge.threshold) prob(i,j) = 1.0_rp
    end do
    end do
    !
    return
  end subroutine validation_get_prob_con
  ! 
  !-----------------------------------------
  !    subroutine validation_get_GFMS
  !-----------------------------------------
  !
  subroutine validation_get_GFMS(P_m,P_o,Hm1,GFMS)
    implicit none
    !
    real(rp), intent(IN   ) :: P_m (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: P_o (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: Hm1 (:)    ! global index
    real(rp), intent(INOUT) :: GFMS
    !
    integer(ip) :: i,j
    real(rp)    :: my_num,my_den
    !
    !*** Initializations
    !
    my_num = 0.0_rp
    my_den = 0.0_rp
    !
    do j = my_jps,my_jpe  ! Loop over all my_mass_points   
    do i = my_ips,my_ipe   
       my_num = my_num + Hm1(j)*P_m(i,j)*P_o(i,j)
       my_den = my_den + Hm1(j)*P_m(i,j)*P_o(i,j)
       if(P_o(i,j).eq.0.0_rp) my_den = my_den + Hm1(j)*P_m(i,j)
       if(P_m(i,j).eq.0.0_rp) my_den = my_den + Hm1(j)*P_o(i,j)
    end do
    end do
    !
    !*** Summ across processors
    !
    call parallel_sum(my_num,COMM_WORLD)
    call parallel_sum(my_den,COMM_WORLD)
    !
    GFMS = 0.0_rp
    if(my_den.gt.0.0_rp) GFMS = my_num/my_den
    !
    return
  end subroutine validation_get_GFMS
  ! 
  !-----------------------------------------
  !    subroutine validation_get_GFAR
  !-----------------------------------------
  !
  subroutine validation_get_GFAR(P_m,P_o,Hm1,GFAR)
    implicit none
    !
    real(rp), intent(IN   ) :: P_m (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: P_o (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: Hm1 (:)    ! global index
    real(rp), intent(INOUT) :: GFAR
    !
    integer(ip) :: i,j
    real(rp)    :: my_num,my_den
    !
    !*** Initializations
    !
    my_num = 0.0_rp
    my_den = 0.0_rp
    !
    do j = my_jps,my_jpe  ! Loop over all my_mass_points   
    do i = my_ips,my_ipe
       if(P_o(i,j).eq.0.0_rp) then
          my_num = my_num + Hm1(j)*P_m(i,j)
          my_den = my_den + Hm1(j)*P_m(i,j)
       end if  
       my_den = my_den + Hm1(j)*P_m(i,j)*P_o(i,j)
    end do
    end do
    !
    !*** Summ across processors
    !
    call parallel_sum(my_num,COMM_WORLD)
    call parallel_sum(my_den,COMM_WORLD)
    !
    GFAR = 0.0_rp
    if(my_den.gt.0.0_rp) GFAR = my_num/my_den
    !
    return
  end subroutine validation_get_GFAR
  ! 
  !-----------------------------------------
  !    subroutine validation_get_GPOD
  !-----------------------------------------
  !
  subroutine validation_get_GPOD(P_m,P_o,Hm1,GPOD)
    implicit none
    !
    real(rp), intent(IN   ) :: P_m (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: P_o (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: Hm1 (:)    ! global index
    real(rp), intent(INOUT) :: GPOD
    !
    integer(ip) :: i,j
    real(rp)    :: my_num,my_den
    !
    !*** Initializations
    !
    my_num = 0.0_rp
    my_den = 0.0_rp
    !
    do j = my_jps,my_jpe  ! Loop over all my_mass_points   
    do i = my_ips,my_ipe   
       my_num = my_num + Hm1(j)*P_m(i,j)*P_o(i,j)
       my_den = my_den + Hm1(j)*P_m(i,j)*P_o(i,j)
       if(P_m(i,j).eq.0.0_rp) my_den = my_den + Hm1(j)*P_o(i,j)
    end do
    end do
    !
    !*** Summ across processors
    !
    call parallel_sum(my_num,COMM_WORLD)
    call parallel_sum(my_den,COMM_WORLD)
    !
    GPOD = 0.0_rp
    if(my_den.gt.0.0_rp) GPOD = my_num/my_den
    !
    return
  end subroutine validation_get_GPOD
  ! 
  !-----------------------------------------
  !    subroutine validation_get_Brier_score
  !-----------------------------------------
  !
  subroutine validation_get_Brier_score(P_m,P_o,Hm1,BS)
    implicit none
    !
    real(rp), intent(IN   ) :: P_m (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: P_o (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: Hm1 (:)    ! global index
    real(rp), intent(INOUT) :: BS
    !
    integer(ip) :: i,j
    real(rp)    :: my_num,my_den
    !
    !*** Initializations
    !
    my_num = 0.0_rp
    my_den = 0.0_rp
    !
    do j = my_jps,my_jpe  ! Loop over all my_mass_points   
    do i = my_ips,my_ipe
       if(P_o(i,j).gt.0.0_rp) then
          my_num = my_num + Hm1(j)*(P_m(i,j)-P_o(i,j))*(P_m(i,j)-P_o(i,j))
          my_den = my_den + Hm1(j)
       end if
    end do
    end do
    !
    !*** Summ across processors
    !
    call parallel_sum(my_num,COMM_WORLD)
    call parallel_sum(my_den,COMM_WORLD)
    !
    BS = 0.0_rp
    if(my_den.gt.0.0_rp) BS = my_num/my_den
    !
    return
  end subroutine validation_get_Brier_score
  ! 
  !-----------------------------------------
  !    subroutine validation_get_NRMSE_grid
  !-----------------------------------------
  !
  subroutine validation_get_NRMSE_grid(M,O,NRMSE)
    implicit none
    !
    real(rp), intent(IN   ) :: M (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: O (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(INOUT) :: NRMSE
    !
    integer(ip) :: i,j,my_n
    real(rp)    :: my_num,my_Omax,my_Omin,gl_Omin,gl_Omax
    !
    !*** Initializations
    !
    my_num  = 0.0_rp
    my_n    = 0
    my_Omax = maxval(O(:,:))
    my_Omin = minval(O(:,:))
    !
    do j = my_jps,my_jpe  ! Loop over all my_mass_points   
    do i = my_ips,my_ipe  
       if(O(i,j).gt.0.0_rp) then
          my_n   = my_n   + 1
          my_num = my_num + (M(i,j)-O(i,j))*(M(i,j)-O(i,j))
       end if
    end do
    end do
    !
    !*** Summ across processors
    !
    call parallel_sum(my_n  ,COMM_WORLD)
    call parallel_sum(my_num,COMM_WORLD)
    !
    call parallel_min(my_Omin,gl_Omin,COMM_WORLD)
    call parallel_max(my_Omax,gl_Omax,COMM_WORLD)
    !
    NRMSE = 0.0_rp
    if(my_n.gt.0) NRMSE = (sqrt(my_num/my_n))/(gl_Omax-gl_Omin)
    !
    return
  end subroutine validation_get_NRMSE_grid
  ! 
  !-----------------------------------------
  !    subroutine validation_get_NRMSE_pts
  !-----------------------------------------
  !
  subroutine validation_get_NRMSE_pts(npts,M,O,NRMSE)
    implicit none
    !
    integer(ip), intent(IN   ) :: npts
    real(rp),    intent(IN   ) :: M (npts)
    real(rp),    intent(IN   ) :: O (npts)
    real(rp),    intent(INOUT) :: NRMSE
    !
    integer(ip) :: i,gl_n
    real(rp)    :: gl_Omin,gl_Omax,gl_num
    !
    !*** Initializations
    !
    gl_num  = 0.0_rp
    gl_n    = 0
    gl_Omax = maxval(O(:))
    gl_Omin = minval(O(:))
    !
    do i = 1,npts
       if(O(i).gt.0.0_rp) then
          gl_n   = gl_n   + 1 
          gl_num = gl_num + (M(i)-O(i))*(M(i)-O(i))
       end if
    end do
    !
    NRMSE = 0.0_rp
    if(gl_n.gt.0) NRMSE = (sqrt(gl_num/gl_n))/(gl_Omax-gl_Omin)
    !
    return
  end subroutine validation_get_NRMSE_pts
  !
  !
  !
END MODULE Validation
