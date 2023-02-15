!***************************************************************
!>
!> Module for procedures related to SetDbs
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Dbs
  use KindType
  use InpOut
  use Parallel
  use netcdf
  use Domain
  use Maths
  use Time
  use Grid
  use Phys
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: dbs_read_inp_meteo
  PUBLIC :: dbs_bcast_inp_meteo
  PUBLIC :: dbs_read_metmodel_grid
  PUBLIC :: dbs_bcast_metmodel_grid
  PUBLIC :: dbs_read_metmodel_times
  PUBLIC :: dbs_bcast_metmodel_times
  PUBLIC :: dbs_read_metmodel_data
  PUBLIC :: dbs_set_interp2d
  PUBLIC :: dbs_set_profile
  PUBLIC :: dbs_out_profile
  PUBLIC :: dbs_interpola2d
  PUBLIC :: dbs_interpola3d
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: dbs_set_dictionary
  PRIVATE :: dbs_read_dictionary
  PRIVATE :: dbs_read_metmodel_var2d
  PRIVATE :: dbs_read_metmodel_var3d
  PRIVATE :: dbs_read_metmodel_var4d
  PRIVATE :: dbs_get_geopotential_hyb
  PRIVATE :: dbs_get_map_projection_info
  PRIVATE :: dbs_get_zo_from_24ldu
  !
  !    LIST OF PRIVATE VARIABLES
  !
  integer(ip), PRIVATE  :: ncID
  integer(ip), PRIVATE  :: dimID
  integer(ip), PRIVATE  :: varID
  !
  !    DICTIONARY DEFINITION
  !
  integer(ip), PRIVATE, parameter :: DIM_LON    = 1
  integer(ip), PRIVATE, parameter :: DIM_LAT    = 2
  integer(ip), PRIVATE, parameter :: DIM_ZLEV   = 3
  integer(ip), PRIVATE, parameter :: DIM_TIME   = 4
  integer(ip), PRIVATE, parameter :: DIM_XSTAGE = 5
  integer(ip), PRIVATE, parameter :: DIM_YSTAGE = 6
  integer(ip), PRIVATE, parameter :: DIM_ZSTAGE = 7
  integer(ip), PRIVATE, parameter :: DIM_SOILAY = 8
  !
  integer(ip), PRIVATE, parameter :: VAR_LON   = 10
  integer(ip), PRIVATE, parameter :: VAR_LAT   = 11
  integer(ip), PRIVATE, parameter :: VAR_ZLEV  = 12
  integer(ip), PRIVATE, parameter :: VAR_TOPO  = 13
  integer(ip), PRIVATE, parameter :: VAR_LMASK = 14
  integer(ip), PRIVATE, parameter :: VAR_LUSE  = 15
  integer(ip), PRIVATE, parameter :: VAR_Z0    = 16
  !
  integer(ip), PRIVATE, parameter :: VAR_TIME  = 20
  integer(ip), PRIVATE, parameter :: VAR_PBLH  = 21
  integer(ip), PRIVATE, parameter :: VAR_UST   = 22
  integer(ip), PRIVATE, parameter :: VAR_SMOI  = 23
  integer(ip), PRIVATE, parameter :: VAR_PREC  = 24
  integer(ip), PRIVATE, parameter :: VAR_ACCP  = 25
  integer(ip), PRIVATE, parameter :: VAR_U10   = 26
  integer(ip), PRIVATE, parameter :: VAR_V10   = 27
  integer(ip), PRIVATE, parameter :: VAR_T2    = 28
  integer(ip), PRIVATE, parameter :: VAR_MON   = 29
  integer(ip), PRIVATE, parameter :: VAR_PSFC  = 30
  !
  integer(ip), PRIVATE, parameter :: VAR_HGT   = 40
  integer(ip), PRIVATE, parameter :: VAR_P     = 41
  integer(ip), PRIVATE, parameter :: VAR_T     = 42
  integer(ip), PRIVATE, parameter :: VAR_TP    = 43
  integer(ip), PRIVATE, parameter :: VAR_TV    = 44
  integer(ip), PRIVATE, parameter :: VAR_U     = 45
  integer(ip), PRIVATE, parameter :: VAR_V     = 46
  integer(ip), PRIVATE, parameter :: VAR_W     = 47
  integer(ip), PRIVATE, parameter :: VAR_RH    = 48
  integer(ip), PRIVATE, parameter :: VAR_QV    = 49
  integer(ip), PRIVATE, parameter :: VAR_RHO   = 50
  !
  logical,               PRIVATE  :: EXISTS    (100)
  logical,               PRIVATE  :: COMPUTED  (100)
  character(len=s_name), PRIVATE  :: DICTIONARY(100)
  !
  integer(ip), parameter, private :: s_long  = 512    !  Generic long string lenght. Use '(a512)' to read
  integer(ip), parameter, private :: nwormax = 128
  integer(ip), parameter, private :: nparmax = 128
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine dbs_read_metmodel_grid
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads meteo model grid and terrain data nedded to generate MY_GRID (topography and lmask)
  !
  subroutine dbs_read_metmodel_grid(MY_FILES,MY_GRID,MY_MET,GL_METMODEL,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_GRID     grid configuration parameters
    !>   @param MY_MET      variables related to meteorology in MY_GRID
    !>   @param GL_METMODEL variables related to driving meteorological model
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(METEOROLOGY),   intent(IN   ) :: MY_MET
    type(METEO_MODEL),   intent(INOUT) :: GL_METMODEL
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: istat,i,j,iz
    real(rp), allocatable :: work1d(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_read_metmodel_grid'
    MY_ERR%message = ' '
    !
    !*** Define dictionary depending on each model
    !
    if(MY_FILES%file_tbl_met.eq.'-') then
       call dbs_set_dictionary(MY_MET%meteo_data_type, MY_ERR)
    else
       call dbs_read_dictionary(MY_FILES%file_tbl_met, MY_ERR)
    end if
    if(MY_ERR%flag.ne.0) return
    !
    !*** Open netCDF file and get ncID
    !
    istat = nf90_open(TRIM(MY_FILES%file_met),NF90_NOWRITE, ncID)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Unable to open '//TRIM(MY_FILES%file_met)
       return
    end if
    !
    !*** Read dimensions
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LON),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_METMODEL%nx)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LAT),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_METMODEL%ny)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_ZLEV),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_METMODEL%nz)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_TIME),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_METMODEL%nt)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    !*** Coordinate variables could be reversed (dafult value)
    !
    GL_METMODEL%xreversed = .false.
    GL_METMODEL%yreversed = .false.
    GL_METMODEL%zreversed = .false.
    !
    !*** Allocates
    !
    allocate(GL_METMODEL%lon  ( GL_METMODEL%nx, GL_METMODEL%ny))
    allocate(GL_METMODEL%lat  ( GL_METMODEL%nx, GL_METMODEL%ny))
    allocate(GL_METMODEL%pres ( GL_METMODEL%nz                ))
    allocate(GL_METMODEL%topg ( GL_METMODEL%nx, GL_METMODEL%ny))
    !
    !*** Reads coordinates and terrain data
    !
    ! Reads Lat Lon
    select case(MY_MET%meteo_data_type)
    case('WRF')
       !
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LON),varID)
       istat = nf90_get_var  (ncID,varID,GL_METMODEL%lon)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
       !
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LAT),varID)
       istat = nf90_get_var  (ncID,varID,GL_METMODEL%lat)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
       !
    case('GFS','GDAS','ERA5','GRIB2NC','ERA5ML')
       !
       allocate(work1d(GL_METMODEL%nx))
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LON),varID)
       istat = nf90_get_var  (ncID,varID,work1d)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
       do j = 1,GL_METMODEL%ny
          GL_METMODEL%lon(:,j) = work1d(:)
       end do
       deallocate(work1d)
       !
       allocate(work1d(GL_METMODEL%ny))
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LAT),varID)
       istat = nf90_get_var  (ncID,varID,work1d)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
       if (work1d(2).gt.work1d(1)) then
          do i = 1,GL_METMODEL%nx
             GL_METMODEL%lat(i,:) = work1d(:)
          end do
       else
          GL_METMODEL%yreversed = .true.
          do i = 1,GL_METMODEL%nx
             GL_METMODEL%lat(i,1:GL_METMODEL%ny) = work1d(GL_METMODEL%ny:1:-1)
          end do
       end if
       deallocate(work1d)
       !
    case default
       !
       !  DEFAULT
       !
       MY_ERR%flag    = 1
       MY_ERR%message = 'Meteo model not implemented '
       return
    end select
    !
    ! Reads pressure 1D when applicable
    select case(MY_MET%meteo_data_type)
       !
    case('GFS','GDAS','ERA5','GRIB2NC')
       !   pres
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_ZLEV),varID)
       istat = nf90_get_var  (ncID,varID,GL_METMODEL%pres)
       if(istat.ne.0) then
          MY_ERR%flag    = istat
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
       !
       !   NOTE: GFS/ERA5 pressure levels given from top to bottom (surface); i.e. need to be inverted
       if (GL_METMODEL%pres(1).lt.GL_METMODEL%pres(2)) then
          GL_METMODEL%zreversed = .true.
          allocate(work1d(GL_METMODEL%nz))
          work1d = GL_METMODEL%pres
          i = 0
          do iz = GL_METMODEL%nz,1,-1
             i = i + 1
             GL_METMODEL%pres(i) = work1d(iz)
          end do
          deallocate(work1d)
       end if
    case('ERA5ML')
       GL_METMODEL%zreversed = .true.
    end select
    !
    !*** Convert pressure units
    select case(MY_MET%meteo_data_type)
       !
    case('GFS','ERA5','GRIB2NC')
        GL_METMODEL%pres = GL_METMODEL%pres*100.0_rp  ! (converted from mb to Pa)
    end select
    !
    !*** Reads topography
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_TOPO),varID)
    if(istat.ne.0) then
       EXISTS(VAR_TOPO)   = .false.
       COMPUTED(VAR_TOPO) = .true.
       GL_METMODEL%topg   = 0.0_rp
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_TOPO))// &
            ' not found in met model file. Assuming 0 value')
    else
       call dbs_read_metmodel_var2d(varID, &
            GL_METMODEL%nx,GL_METMODEL%ny, &
            GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
            GL_METMODEL%topg, &
            MY_ERR)
       select case(MY_MET%meteo_data_type)
       case('ERA5','ERA5ML')
          GL_METMODEL%topg = GL_METMODEL%topg / 9.81_rp
       end select
    end if
    !
    !*** Prints to log file
    !
    write(MY_FILES%lulog,10) TRIM(MY_MET%meteo_data_type), &
         GL_METMODEL%lon(1,1), GL_METMODEL%lon(GL_METMODEL%nx,GL_METMODEL%ny), &
         GL_METMODEL%lat(1,1), GL_METMODEL%lat(GL_METMODEL%nx,GL_METMODEL%ny), &
         MY_GRID%lonmin,MY_GRID%lonmax, &
         MY_GRID%latmin,MY_GRID%latmax
10  format(/,2x,a,&
         ' meteo model spatial coverage: ',/,&
         '  Longitude range    : ',f9.1,1x,f9.1,/,&
         '  Latitude  range    : ',f9.1,1x,f9.1,/,&
         /,&
         '  FALL3D grid spatial coverage: ',/,&
         '  Longitude range    : ',f9.1,1x,f9.1,/,&
         '  Latitude  range    : ',f9.1,1x,f9.1)
    !
    return
  end subroutine dbs_read_metmodel_grid
  !
  !-----------------------------------------
  !    subroutine dbs_bcast_metmodel_grid
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts meteo model grid and terrain data nedded to generate MY_GRID (topography and lmask)
  !
  subroutine dbs_bcast_metmodel_grid(GL_METMODEL, MY_ERR)
    implicit none
    !
    !>   @param GL_METMODEL variables related to meteorological model
    !>   @param MY_ERR      error handler
    !
    type(METEO_MODEL), intent(INOUT) :: GL_METMODEL
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    integer(ip) :: npoin
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_bcast_metmodel_grid'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_METMODEL%nx,1,0)
    call parallel_bcast(GL_METMODEL%ny,1,0)
    call parallel_bcast(GL_METMODEL%nz,1,0)
    !
    call parallel_bcast(GL_METMODEL%xreversed,1,0)
    call parallel_bcast(GL_METMODEL%yreversed,1,0)
    call parallel_bcast(GL_METMODEL%zreversed,1,0)
    !
    call parallel_bcast(EXISTS  ,size(EXISTS)  ,0)
    call parallel_bcast(COMPUTED,size(COMPUTED),0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(GL_METMODEL%lon  ( GL_METMODEL%nx, GL_METMODEL%ny))
       allocate(GL_METMODEL%lat  ( GL_METMODEL%nx, GL_METMODEL%ny))
       allocate(GL_METMODEL%pres ( GL_METMODEL%nz                ))
       allocate(GL_METMODEL%topg ( GL_METMODEL%nx, GL_METMODEL%ny))
    end if
    !
    npoin = GL_METMODEL%nx * GL_METMODEL%ny
    call parallel_bcast(GL_METMODEL%lon  ,npoin,0)
    call parallel_bcast(GL_METMODEL%lat  ,npoin,0)
    call parallel_bcast(GL_METMODEL%topg ,npoin,0)
    npoin = GL_METMODEL%nz
    call parallel_bcast(GL_METMODEL%pres ,npoin,0)
    !
    return
  end subroutine dbs_bcast_metmodel_grid
  !
  !-----------------------------------------
  !    subroutine dbs_read_metmodel_times
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads meteorological model time coverage and perform checks
  !
  subroutine dbs_read_metmodel_times(MY_FILES,MY_MET,GL_METMODEL,MY_TIME,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_MET      variables related to meteorology in MY_GRID
    !>   @param GL_METMODEL variables related to driving meteorological model
    !>   @param MY_TIME     variables related to time
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(METEOROLOGY),   intent(INOUT) :: MY_MET
    type(METEO_MODEL),   intent(INOUT) :: GL_METMODEL
    type(RUN_TIME),      intent(INOUT) :: MY_TIME
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    logical        :: found
    integer(ip)    :: istat,it,i,julday1,julday2
    integer(ip)    :: iyr,imo,idy,ihr,imi,ise
    real(rp)       :: time_dbs_start_sec,time_dbs_end_sec
    type(DATETIME) :: time_dbs_start,time_dbs_end,time_driver_ref
    !
    real(rp)                       :: time_factor
    character(len=s_name)          :: timeunit_string
    real(dp),          allocatable :: driver_times(:)
    character(len=19), allocatable :: WRF_string(:)
    !
    !*** Initializations
    !
    select case(MY_MET%meteo_data_type)
    case('WRF','GFS','GRIB2NC','GDAS','ERA5','ERA5ML')
        MY_ERR%flag    = 0
        MY_ERR%source  = 'dbs_read_metmodel_times'
        MY_ERR%message = ' '
    case default
        MY_ERR%flag    = 1
        MY_ERR%source  = 'dbs_read_metmodel_times'
        MY_ERR%message = 'Meteo model not implemented '
        return
    end select
    !
    !*** Read dimensions
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_TIME),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_METMODEL%nt)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    !*** Allocates
    !
    allocate(GL_METMODEL%time   ( GL_METMODEL%nt ))
    allocate(GL_METMODEL%timesec( GL_METMODEL%nt ))
    allocate(driver_times(GL_METMODEL%nt))
    !
    !*** Read times from driver
    !
    select case(MY_MET%meteo_data_type)
    case('WRF')
        !
        !*** WRF model
        !
        allocate(WRF_string(GL_METMODEL%nt))  ! time instants in format YYYY-MM-DD_HH:MM:SS
        !
        istat = nf90_inq_varid(ncID,DICTIONARY(VAR_TIME),varID)
        istat = nf90_get_var(ncID,varID,WRF_string)
        if(istat.ne.nf90_noerr) then
            MY_ERR%flag    = istat
            MY_ERR%message = nf90_strerror(istat)
            return
        end if
        !
        !*** Define for the driver:
        !    time(nt)         in format YYYY-MM-DD HH:MM:SS
        !    reference time   in format YYYY-MM-DD 00:00:00
        !    driver_times(nt) in format seconds after the reference time
        !
        do it = 1,GL_METMODEL%nt
            iyr = stoi1(WRF_string(it)(1 :1 ))*1000 + &
                  stoi1(WRF_string(it)(2 :2 ))*100  + &
                  stoi1(WRF_string(it)(3 :3 ))*10   + &
                  stoi1(WRF_string(it)(4 :4 ))
            imo = stoi1(WRF_string(it)(6 :6 ))*10   + &
                  stoi1(WRF_string(it)(7 :7 ))
            idy = stoi1(WRF_string(it)(9 :9 ))*10   + &
                  stoi1(WRF_string(it)(10:10))
            ihr = stoi1(WRF_string(it)(12:12))*10   + &
                  stoi1(WRF_string(it)(13:13))
            imi = stoi1(WRF_string(it)(15:15))*10   + &
                  stoi1(WRF_string(it)(16:16))
            ise = stoi1(WRF_string(it)(18:18))*10   + &
                  stoi1(WRF_string(it)(19:19))
            GL_METMODEL%time(it) = DATETIME(iyr,imo,idy,ihr,imi,ise)
            if(it.eq.1) then
                time_driver_ref = DATETIME(iyr,imo,idy,0,0,0)
                call time_julian_date(iyr,imo,idy,julday1,MY_ERR)
            end if
            call time_julian_date(iyr,imo,idy,julday2,MY_ERR)
            driver_times(it) = (julday2-julday1)*86400.0_dp + &
                                ihr*3600.0_dp + imi*60.0_dp + &
                                ise*1.0_dp
        end do
    case default
        !
        !*** Assuming CF Conventions for input file
        !
        istat = nf90_inq_varid(ncID,DICTIONARY(VAR_TIME),varID)
        istat = nf90_get_var  (ncID,varID,driver_times)
        if(istat.ne.nf90_noerr) then
          MY_ERR%flag    = istat
          MY_ERR%source  = 'dbs_read_metmodel_times'
          MY_ERR%message = nf90_strerror(istat)
          return
        end if
        !
        istat = nf90_inquire_attribute(ncID, varID, 'units')
        if (istat.eq.nf90_noerr) then
          istat = nf90_get_att(ncID, varID, 'units', timeunit_string)
        end if
        !
        if(istat.ne.nf90_noerr) then
            MY_ERR%flag    = istat
            MY_ERR%source  = 'dbs_read_metmodel_times'
            MY_ERR%message = nf90_strerror(istat)
            return
        end if
        !
        call inpout_decode_timeunit(timeunit_string,time_factor,time_driver_ref,MY_ERR)
        if(MY_ERR%flag.ne.0) return
        !
        !*** Convert to seconds
        !
        driver_times = time_factor*driver_times
        !
        !*** Compute driver time seconds after YYYY-MM-DD 00:00:00
        !
        driver_times = driver_times                     + &
                       time_driver_ref%hour   * 3600_dp + &
                       time_driver_ref%minute * 60.0_dp + &
                       time_driver_ref%second * 1.0_dp
        !
        !time_addtime assumes a proleptic
        !Gregorian calendar.
        !If year<=1582 a difference of 2 days
        !exists between the Julian and
        !the proleptic Gregorian calendars.
        if(time_driver_ref%year.le.1582_ip) then
            driver_times = driver_times-48*3600_dp
            call task_wriwarn(MY_ERR, "Assuming a Julian calendar for the driver met model")
        end if
        !
        !*** Define for the driver:
        !    time(nt)         in format YYYY-MM-DD HH:MM:SS
        !    reference time   in format YYYY-MM-DD 00:00:00
        !    driver_times(nt) in format seconds after the reference time
        !
        do it = 1,GL_METMODEL%nt
            call time_addtime(time_driver_ref%year,    &
                              time_driver_ref%month,   &
                              time_driver_ref%day,     &
                              0,                       &
                              iyr,imo,idy,ihr,imi,ise, &
                              driver_times(it),        &
                              MY_ERR)
            GL_METMODEL%time(it) = DATETIME(iyr,imo,idy,ihr,imi,ise)
       end do
    end select
    !
    !*** Compute delta time
    !
    if(GL_METMODEL%nt.gt.1) then
        GL_METMODEL%dt = driver_times(2) - driver_times(1)
    else
        MY_ERR%flag    = 1
        MY_ERR%source  = 'dbs_read_metmodel_times'
        MY_ERR%message = 'Unable to interpolate: only one time step was found'
        return
    end if
    !
    !*** Store driver model start time
    !
    GL_METMODEL%start_year   = GL_METMODEL%time(1)%year
    GL_METMODEL%start_month  = GL_METMODEL%time(1)%month
    GL_METMODEL%start_day    = GL_METMODEL%time(1)%day
    GL_METMODEL%start_hour   = GL_METMODEL%time(1)%hour
    GL_METMODEL%start_minute = GL_METMODEL%time(1)%minute
    GL_METMODEL%start_second = GL_METMODEL%time(1)%second
    !
    !*** driver_times in seconds after YYYY-MM-DD 00:00:00 of start time
    !
    call time_julian_date(time_driver_ref%year,time_driver_ref%month,time_driver_ref%day,julday1,MY_ERR)
    call time_julian_date(GL_METMODEL%start_year,GL_METMODEL%start_month,GL_METMODEL%start_day,julday2,MY_ERR)
    !
    ! Compute this in double precision
    driver_times(:) = driver_times(:) - (julday2-julday1)*86400.0_dp
    GL_METMODEL%timesec(:) = driver_times(:)
    !
    !*** Calculates time lag (that is, the time in seconds between the met model origin
    !*** and the DBS origin). Both origins are referred to 0000UTC, but may belong to different days
    !
    call time_julian_date(GL_METMODEL%start_year, GL_METMODEL%start_month, GL_METMODEL%start_day, julday1, MY_ERR)
    call time_julian_date(MY_TIME%start_year,     MY_TIME%start_month,     MY_TIME%start_day,     julday2, MY_ERR)
    !
    MY_MET%time_lag = (julday2-julday1)*86400.0_rp
    !
    !*** Prints log file
    !
    call time_addtime(MY_TIME%start_year,      &
                      MY_TIME%start_month,     &
                      MY_TIME%start_day,       &
                      0,                       &
                      iyr,imo,idy,ihr,imi,ise, &
                      MY_TIME%dbs_start,       &
                      MY_ERR)

    time_dbs_start     = DATETIME(iyr,imo,idy,ihr,0,0)
    time_dbs_start_sec = MY_TIME%dbs_start
    call time_addtime(MY_TIME%start_year,      &
                      MY_TIME%start_month,     &
                      MY_TIME%start_day,       &
                      0,                       &
                      iyr,imo,idy,ihr,imi,ise, &
                      MY_TIME%dbs_end,         &
                      MY_ERR)
    time_dbs_end       = DATETIME(iyr,imo,idy,ihr,0,0)
    time_dbs_end_sec   = MY_TIME%dbs_end
    !
    write(MY_FILES%lulog,10) TRIM(MY_MET%meteo_data_type),     &
                             GL_METMODEL%time(1),              &
                             GL_METMODEL%time(GL_METMODEL%nt), &
                             time_dbs_start,                   &
                             time_dbs_end,                     &
                             MY_MET%time_lag/3600.0_rp
10  format(/,2x,a,&
         ' meteo model time coverage : ',/, &
         '  From               : ',I4,2('-',I2.2),1x,I2.2,2(':',I2.2),/, &
         '  To                 : ',I4,2('-',I2.2),1x,I2.2,2(':',I2.2),/, &
         '                       ',/,&
         '  FALL3D dbs time coverage : ',/, &
         '  From               : ',I4,2('-',I2.2),1x,I2.2,2(':',I2.2),/, &
         '  To                 : ',I4,2('-',I2.2),1x,I2.2,2(':',I2.2),/, &
         '  Time lag (h)       : ',f15.0)
    !
    !*** Checks time coverage and interpolation indexes
    !
    if((MY_MET%time_lag+time_dbs_start_sec).lt.GL_METMODEL%timesec(1)) then
       MY_ERR%flag    = 1
       MY_ERR%source  = 'dbs_read_metmodel_times'
       MY_ERR%message = 'Meteo model starting time larger than dbs starting time '
       return
    end if
    if((MY_MET%time_lag+time_dbs_end_sec).gt.GL_METMODEL%timesec(GL_METMODEL%nt)) then
       MY_ERR%flag    = 1
       MY_ERR%source  = 'dbs_read_metmodel_times'
       MY_ERR%message = 'Meteo model ending time smaller than dbs ending time '
       return
    end if
    !
    found =.false.
    it = 0
    do while(.not.found)
       it = it + 1
       if(it.eq.GL_METMODEL%nt) then
          MY_ERR%flag    = 1
          MY_ERR%source  = 'dbs_read_metmodel_times'
          MY_ERR%message = 'Unable to find interpolation interval for time_dbs_start_sec'
          return
       end if
       if( ((MY_MET%time_lag+time_dbs_start_sec).ge.GL_METMODEL%timesec(it  )).and. &
           ((MY_MET%time_lag+time_dbs_start_sec).le.GL_METMODEL%timesec(it+1)) ) then
          found = .true.
          if((MY_MET%time_lag+time_dbs_start_sec).eq.GL_METMODEL%timesec(it+1)) then
             MY_MET%its = it+1
          else
             MY_MET%its = it
          end if
       end if
    end do
    !
    found =.false.
    it = 0
    do while(.not.found)
       it = it + 1
       if(it.eq.GL_METMODEL%nt) then
          MY_ERR%flag    = 1
          MY_ERR%source  = 'dbs_read_metmodel_times'
          MY_ERR%message = 'Unable to find interpolation interval for time_dbs_end_sec'
          return
       end if
       if( ((MY_MET%time_lag+time_dbs_end_sec).ge.GL_METMODEL%timesec(it  )).and. &
           ((MY_MET%time_lag+time_dbs_end_sec).le.GL_METMODEL%timesec(it+1)) ) then
          found = .true.
          if((MY_MET%time_lag+time_dbs_end_sec).eq.GL_METMODEL%timesec(it)) then
             MY_MET%ite = it
          else
             MY_MET%ite = it+1
          end if
       end if
    end do
    MY_MET%nt  = MY_MET%ite - MY_MET%its + 1
    !
    allocate(MY_MET%time   (MY_MET%nt))
    allocate(MY_MET%timesec(MY_MET%nt))
    MY_MET%time   (1:MY_MET%nt) = GL_METMODEL%time(MY_MET%its:MY_MET%ite)
    i = 0
    do it = MY_MET%its,MY_MET%ite
       i = i +1
       MY_MET%timesec(i) = GL_METMODEL%timesec(it) - MY_MET%time_lag
    end do
    !
    !*** Writes to log file
    !
    write(MY_FILES%lulog,20) (MY_MET%time(it), MY_MET%its+it-1, it = 1,MY_MET%nt)
20  format( /,2x,'Data will be interpolated at times (YYYY-MM-DD HH:MM:SS): ',/, &
            *(2x,I4,2('-',I2.2),1x,I2.2,2(':',I2.2), &
            ' (step ',i3,' of the meteorological model)',/) )
    !
    return
  end subroutine dbs_read_metmodel_times
  !
  !-----------------------------------------
  !    subroutine dbs_bcast_metmodel_times
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts meteo model time related variables
  !
  subroutine dbs_bcast_metmodel_times(MY_MET,GL_METMODEL,MY_ERR)
    implicit none
    !
    !>   @param MY_MET      variables related to meteorology in MY_GRID
    !>   @param GL_METMODEL variables related to meteorological model
    !>   @param MY_ERR      error handler
    !
    type(METEOROLOGY), intent(INOUT) :: MY_MET
    type(METEO_MODEL), intent(INOUT) :: GL_METMODEL
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_bcast_metmodel_times'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_METMODEL%nt,1,0)
    call parallel_bcast(GL_METMODEL%dt,1,0)
    call parallel_bcast(GL_METMODEL%start_year,1,0)
    call parallel_bcast(GL_METMODEL%start_month,1,0)
    call parallel_bcast(GL_METMODEL%start_day,1,0)
    call parallel_bcast(GL_METMODEL%start_hour,1,0)
    call parallel_bcast(GL_METMODEL%start_minute,1,0)
    call parallel_bcast(GL_METMODEL%start_second,1,0)
    !
    call parallel_bcast(MY_MET%nt,1,0)
    call parallel_bcast(MY_MET%its,1,0)
    call parallel_bcast(MY_MET%ite,1,0)
    call parallel_bcast(MY_MET%time_lag,1,0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(GL_METMODEL%time    (GL_METMODEL%nt))
       allocate(GL_METMODEL%timesec (GL_METMODEL%nt))

       allocate(MY_MET%time    (MY_MET%nt))
       allocate(MY_MET%timesec (MY_MET%nt))
    end if
    !
    call parallel_bcast(GL_METMODEL%time%year  ,GL_METMODEL%nt,0)
    call parallel_bcast(GL_METMODEL%time%month ,GL_METMODEL%nt,0)
    call parallel_bcast(GL_METMODEL%time%day   ,GL_METMODEL%nt,0)
    call parallel_bcast(GL_METMODEL%time%hour  ,GL_METMODEL%nt,0)
    call parallel_bcast(GL_METMODEL%time%minute,GL_METMODEL%nt,0)
    call parallel_bcast(GL_METMODEL%time%second,GL_METMODEL%nt,0)
    call parallel_bcast(GL_METMODEL%timesec    ,GL_METMODEL%nt,0)

    call parallel_bcast(MY_MET%time%year       ,MY_MET%nt,0)
    call parallel_bcast(MY_MET%time%month      ,MY_MET%nt,0)
    call parallel_bcast(MY_MET%time%day        ,MY_MET%nt,0)
    call parallel_bcast(MY_MET%time%hour       ,MY_MET%nt,0)
    call parallel_bcast(MY_MET%time%minute     ,MY_MET%nt,0)
    call parallel_bcast(MY_MET%time%second     ,MY_MET%nt,0)
    call parallel_bcast(MY_MET%timesec         ,MY_MET%nt,0)
    !
    return
  end subroutine dbs_bcast_metmodel_times
  !
  !-----------------------------------------
  !    subroutine dbs_read_metmodel_data
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads and interpolates meteo variables within the dbs time slab
  !
  subroutine dbs_read_metmodel_data(MY_FILES,MY_MET,GL_METMODEL,GL_METPROFILES,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_MET      variables related to meteorology in MY_GRID
    !>   @param GL_METMODEL variables related to driving meteorological model
    !>   @param GL_METPROFILES  variables related to metrorological profiles
    !>   @param MY_GRID     grid configuration parameters
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(METEOROLOGY),   intent(INOUT) :: MY_MET
    type(METEO_MODEL),   intent(INOUT) :: GL_METMODEL
    type(METEO_PROFILE), intent(INOUT) :: GL_METPROFILES
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)             :: it,my_it,istat,ipoin,ielem,i,j,k,ix,iy,iz,ix_prof,iy_prof
    integer(ip)             :: nx,ny,nz,npoin,nx_stag,ny_stag,nz_stag
    real(rp)                :: s,t,st,myshape(4),z,rh,tc,e,es
    logical                 :: rotate_winds
    real(rp)                :: cone, cen_long, alpha, cos_alpha_prof, sin_alpha_prof, tmp_wind_prof
    real(rp),   allocatable :: zmodel   (:,:,:)
    real(rp),   allocatable :: work3d   (:,:,:)
    real(rp),   allocatable :: work3d2  (:,:,:)
    real(rp),   allocatable :: my_zmodel(:,:)
    integer(ip),allocatable :: my_iz    (:,:)
    real(rp),   allocatable :: my_sz    (:,:)
    real(rp),   allocatable :: work2d   (:,:)
    real(rp),   allocatable :: work2d2  (:,:)
    real(rp),   allocatable :: psfc     (:,:)
    real(rp),   allocatable :: cos_alpha(:,:)
    real(rp),   allocatable :: sin_alpha(:,:)
    real(rp),   allocatable :: tmp_wind (:,:)
    real(rp),   allocatable :: my_acc_prec_ref (:,:)
    real(rp),   allocatable :: a_coeff  (:)
    real(rp),   allocatable :: b_coeff  (:)
    real(rp),   allocatable :: zc       (:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_read_metmodel_data'
    MY_ERR%message = ' '
    !
    nx    = GL_METMODEL%nx
    ny    = GL_METMODEL%ny
    nz    = GL_METMODEL%nz
    !
    allocate(work2d(nx,ny))
    allocate(work3d(nx,ny,nz))
    !
    !*** Initilize other model specific variables
    !
    select case(MY_MET%meteo_data_type)
    case('WRF')
       nx_stag = nx + 1
       ny_stag = ny + 1
       nz_stag = nz + 1
    case('ERA5ML')
       allocate(a_coeff(0:nz))
       allocate(b_coeff(0:nz))
       if(master_model) call inpout_get_file_hyb(MY_FILES%file_hyb,     &
            nz,a_coeff,b_coeff,    &
            GL_METMODEL%zreversed, &
            MY_ERR)
       call parallel_bcast(MY_ERR%flag,1,0)
       if(MY_ERR%flag.ne.0) return
    case default
       continue
    end select
    !
    !*** Rotation matrices for wind
    !
    select case(MY_MET%meteo_data_type)
    case('WRF')
       if(master_model) then
          !Reads map projection parameters from WRF file
          !rotate_winds defines if wind rotation will be required
          !cone = cone*radians_per_degree
          call dbs_get_map_projection_info(cone,cen_long,rotate_winds,MY_ERR)
       end if
       call parallel_bcast(cone,1,0)
       call parallel_bcast(cen_long,1,0)
       call parallel_bcast(rotate_winds,1,0)
       !
       if(rotate_winds) then
          allocate(cos_alpha(my_ibs:my_ibe,my_jbs:my_jbe))
          allocate(sin_alpha(my_ibs:my_ibe,my_jbs:my_jbe))
          allocate(tmp_wind (my_ibs:my_ibe,my_jbs:my_jbe))
          !
          do j = my_jbs,my_jbe
             do i = my_ibs,my_ibe
                alpha = MY_GRID%lon_c(i) - cen_long
                if(alpha.gt.180.0_rp) then
                   alpha = alpha - 360.0_rp
                else if(alpha.lt.-180.0_rp) then
                   alpha = alpha + 360.0_rp
                end if
                if(MY_GRID%lat_c(j).lt.0) then
                   alpha = -alpha*cone
                else
                   alpha =  alpha*cone
                end if
                cos_alpha(i,j) = cos(alpha)
                sin_alpha(i,j) = sin(alpha)
             end do
          end do
          !
          !Rotation factor for wind profile
          alpha = GL_METPROFILES%lon - cen_long
          if(alpha.gt.180.0_rp) then
             alpha = alpha - 360.0_rp
          else if(alpha.lt.-180.0_rp) then
             alpha = alpha + 360.0_rp
          end if
          if(GL_METPROFILES%lat.lt.0) then
             alpha = -alpha*cone
          else
             alpha =  alpha*cone
          end if
          cos_alpha_prof = cos(alpha)
          sin_alpha_prof = sin(alpha)
       end if
    case default
       rotate_winds = .false.
    end select
    !
    !*** Time-independent 2D variables
    !
    !
    !    my_var(my_ibs:my_ibe ,my_jbs:my_jbe)  at my processor cell cell corners
    !
    allocate(MY_MET%my_lmaskc(my_ibs:my_ibe,my_jbs:my_jbe))   ! land mask
    allocate(MY_MET%my_lusec (my_ibs:my_ibe,my_jbs:my_jbe))   ! land use index
    allocate(MY_MET%my_z0c   (my_ibs:my_ibe,my_jbs:my_jbe))   ! roughness length (m)
    !
    !*** 1. Land Mask
    !
    if(master_model) then
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LMASK),varID)
    end if
    call parallel_bcast(istat,1,0)
    if(istat.ne.0) then
       EXISTS(VAR_LMASK)   = .false.
       COMPUTED(VAR_LMASK) = .true.
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_LMASK))// &
            ' not found in met model file')
       MY_MET%my_lmaskc(:,:) = 1.0_rp
    else
       if(master_model) then
          call dbs_read_metmodel_var2d(varID, &
               nx,ny, &
               GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
               work2d, &
               MY_ERR)
       end if
       npoin = nx*ny
       call parallel_bcast(work2d,npoin,0)
       call dbs_interpola2d(nx,ny,work2d, &
            MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
            MY_MET%my_lmaskc,MY_ERR,.true.)
       !
       select case(MY_MET%meteo_data_type)
       case('ERA5','ERA5ML')
          do j = my_jbs,my_jbe
             do i = my_ibs,my_ibe
                if( MY_MET%my_lmaskc(i,j).lt.0.5_rp ) then
                   MY_MET%my_lmaskc(i,j) = 0.0_rp
                else
                   MY_MET%my_lmaskc(i,j) = 1.0_rp
                end if
             end do
          end do
       end select
       !
    end if
    !
    !*** 2. Land Use
    !
    if(master_model) then
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LUSE),varID)
    end if
    call parallel_bcast(istat,1,0)
    if(istat.ne.0) then
       EXISTS(VAR_LUSE)    = .false.
       COMPUTED(VAR_LUSE)  = .true.
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_LUSE))// &
            ' not found in met model file')
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             if( MY_MET%my_lmaskc(i,j).eq.0.0_rp) then
                MY_MET%my_lusec(i,j) = 16.0_rp ! Water Bodies
             else
                MY_MET%my_lusec(i,j) = 6.0_rp  ! Cropland/Woodland Mosaic
             end if
          end do
       end do
    else
       if(master_model) then
          call dbs_read_metmodel_var2d(varID, &
               nx,ny, &
               GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
               work2d, &
               MY_ERR)
       end if
       npoin = nx*ny
       call parallel_bcast(work2d,npoin,0)
       call dbs_interpola2d(nx,ny,work2d, &
            MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
            MY_MET%my_lusec,MY_ERR,.true.)
    end if
    !
    !*** 3. Z0
    !
    if(master_model) then
       istat = nf90_inq_varid(ncID,DICTIONARY(VAR_Z0),varID)
    end if
    call parallel_bcast(istat,1,0)
    if(istat.ne.0) then
       EXISTS(VAR_Z0)    = .false.
       COMPUTED(VAR_Z0)  = .true.
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_Z0))// &
            ' not found in met model file')
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             call dbs_get_zo_from_24ldu(MY_MET%my_lusec(i,j),MY_MET%my_z0c(i,j))
          end do
       end do
    else
       if(master_model) then
          call dbs_read_metmodel_var2d(varID, &
               nx,ny, &
               GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
               work2d, &
               MY_ERR)
       end if
       npoin = nx*ny
       call parallel_bcast(work2d,npoin,0)
       call dbs_interpola2d(nx,ny,work2d, &
            MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
            MY_MET%my_z0c,MY_ERR)
    end if
    !
    !*** Time-dependent 3D variables
    !
    allocate(MY_MET%my_pblhc(my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !boundary layer height (m)
    allocate(MY_MET%my_ustc (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !friction velocity (m/s)
    allocate(MY_MET%my_smoic(my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !soil moisture (m3/m3)
    allocate(MY_MET%my_prec (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !precipitation rate (mm/h)
    allocate(MY_MET%my_u10  (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !10m u-velocity (m/s)
    allocate(MY_MET%my_v10  (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !10m v-velocity (m/s)
    allocate(MY_MET%my_t2   (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !2m temperature (K)
    allocate(MY_MET%my_monc (my_ibs:my_ibe,my_jbs:my_jbe,MY_MET%nt)) !Monin-Obukhov length (m)
    !
    !*** Time-dependent 4D variables
    !
    allocate (MY_MET%my_pc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !pressure (Pa)
    allocate (MY_MET%my_tc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !temperature (K)
    allocate (MY_MET%my_tpc (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !potential temperature (K)
    allocate (MY_MET%my_tvc (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !virtual temperature (K)
    allocate (MY_MET%my_uc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !u-velocity (m/s)
    allocate (MY_MET%my_vc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !v-velocity (m/s)
    allocate (MY_MET%my_wc  (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !w-velocity (m/s)
    allocate (MY_MET%my_qvc (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !specific humidity (kg/kg)
    allocate (MY_MET%my_rhoc(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe,MY_MET%nt)) !density (kg/m3)
    !
    allocate(GL_METPROFILES%zavl(GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%p   (GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%t   (GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%tp  (GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%tv  (GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%u   (GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%v   (GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%qv  (GL_METMODEL%nz,MY_MET%nt))
    allocate(GL_METPROFILES%rho (GL_METMODEL%nz,MY_MET%nt))
    !
    allocate(psfc     (nx,ny))
    allocate(zmodel   (nx,ny,nz))
    allocate(my_zmodel(MY_MET%npoin,nz   ))
    allocate(my_iz    (MY_MET%npoin,my_kbs:my_kbe))
    allocate(my_sz    (MY_MET%npoin,my_kbs:my_kbe))
    allocate(zc       (nz))
    !
    !  Loop over time steps
    !
    my_it = 0
    do it = MY_MET%its,MY_MET%ite
       my_it = my_it + 1
       !
       ! Reading 3D variables
       !
       ! my_pblhc (my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_PBLH),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_PBLH)    = .false.
          COMPUTED(VAR_PBLH)  = .true.
          ! default value computed below
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_pblhc(:,:,my_it),MY_ERR)
       end if
       !
       !my_ustc(my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_UST),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_UST)    = .false.
          COMPUTED(VAR_UST)  = .true.
          ! default value computed below
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_ustc(:,:,my_it),MY_ERR)
       end if
       !
       !my_smoic(my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_SMOI),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_SMOI)    = .false.
          COMPUTED(VAR_SMOI)  = .true.
          MY_MET%my_smoic(:,:,my_it) = 0.1_rp
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_smoic(:,:,my_it),MY_ERR)
       end if
       ! assigns smoi=1 over water
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             if( MY_MET%my_lmaskc(i,j).eq.0.0_rp) then
                MY_MET%my_smoic(i,j,my_it) = 1.0_rp
             end if
          end do
       end do
       !
       !my_prec(my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_PREC),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_PREC)    = .false.
          COMPUTED(VAR_PREC)  = .true.
          !computed below
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_prec(:,:,my_it),MY_ERR)
          !
          select case(MY_MET%meteo_data_type)
          case('ERA5','ERA5ML')
             MY_MET%my_prec(:,:,my_it) = 1e3_rp * MY_MET%my_prec(:,:,my_it)  ! meters/hour to mm/h
          case default
             MY_MET%my_prec(:,:,my_it) = 3600.0_rp * MY_MET%my_prec(:,:,my_it)  ! mm/s to mm/h
          end select
       end if
       !
       !my_u10(my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_U10),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_U10)    = .false.
          COMPUTED(VAR_U10)  = .true.
          MY_MET%my_u10(:,:,my_it) = 0.0_rp
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_u10(:,:,my_it),MY_ERR)
       end if
       !
       !my_v10(my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_V10),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_V10)    = .false.
          COMPUTED(VAR_V10)  = .true.
          MY_MET%my_v10(:,:,my_it) = 0.0_rp
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_v10(:,:,my_it),MY_ERR)
       end if
       !
       !Wind rotations
       if(rotate_winds) then
          tmp_wind(:,:)            =  MY_MET%my_v10(:,:,my_it)*sin_alpha(:,:) +  MY_MET%my_u10(:,:,my_it)*cos_alpha(:,:)
          MY_MET%my_v10(:,:,my_it) =  MY_MET%my_v10(:,:,my_it)*cos_alpha(:,:) -  MY_MET%my_u10(:,:,my_it)*sin_alpha(:,:)
          MY_MET%my_u10(:,:,my_it) = tmp_wind(:,:)
       end if
       !
       !my_t2(my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_T2),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_T2)    = .false.
          COMPUTED(VAR_T2)  = .true.
          MY_MET%my_t2(:,:,my_it) = 300.0_rp
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_t2(:,:,my_it),MY_ERR)
       end if
       !
       !my_monc(my_ibs:my_ibe, my_jbs:my_jbe, nt)
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_MON),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_MON)    = .false.
          COMPUTED(VAR_MON)  = .true.
          ! default value computed below
       else
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed,GL_METMODEL%yreversed, &
                  work2d, &
                  MY_ERR)
          end if
          npoin = nx*ny
          call parallel_bcast(work2d,npoin,0)
          call dbs_interpola2d(nx,ny,work2d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               MY_MET%my_monc(:,:,my_it),MY_ERR)
       end if
       !
       !psfc(nx,ny): no interpolated. Required to compute mandatory variables
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_PSFC),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.nf90_noerr) EXISTS(VAR_PSFC) = .false.
       select case(MY_MET%meteo_data_type)
       case('ERA5ML')
          if(master_model) then
             istat = nf90_inq_varid(ncID,'lnsp',varID)
          end if
          call parallel_bcast(istat,1,0)
          if(istat.eq.nf90_noerr) COMPUTED(VAR_PSFC) = .true.
       end select
       !
       if(EXISTS(VAR_PSFC).or.COMPUTED(VAR_PSFC)) then
          if(master_model) then
             call dbs_read_metmodel_var3d(varID, &
                  nx,ny,it, &
                  GL_METMODEL%xreversed, &
                  GL_METMODEL%yreversed, &
                  psfc, &
                  MY_ERR)
             if(COMPUTED(VAR_PSFC)) psfc = exp(psfc)  ! I have read lnsp; need to convert
          end if
       else
          select case(MY_MET%meteo_data_type)
          case('ERA5ML')
             MY_ERR%flag       = 1
             MY_ERR%message    = 'Unable to find variables '//TRIM(DICTIONARY(VAR_PSFC))//' and lnsp'
             return
          end select
       end if
       !
       ! Reading 4D variables
       !
       !  1. Read model heights (model/format) dependent
       !
       select case(MY_MET%meteo_data_type)
       case('WRF')
          if(master_model) then
             istat = nf90_inq_varid(ncID,DICTIONARY(VAR_HGT),varID)
          end if
          call parallel_bcast(istat,1,0)
          if(istat.ne.0) then
             EXISTS(VAR_HGT) = .false.
             MY_ERR%flag     = 1
             MY_ERR%message  = 'Unable to find variable '//TRIM(DICTIONARY(VAR_HGT))
             return
          else
             if(master_model) then
                allocate(work3d2(nx,ny,nz_stag))
                ! base-state geopotential PBH
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz_stag,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d2, &
                     MY_ERR)
                zmodel(:,:,:) = 0.5_rp*(work3d2(1:nx,1:ny,1:nz_stag-1)+work3d2(1:nx,1:ny,2:nz_stag))     ! z at mass point
                istat = nf90_inq_varid(ncID,'PH',varID)
                ! perturbation potential PH
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz_stag,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d2, &
                     MY_ERR)
                zmodel(:,:,:) = zmodel(:,:,:) + 0.5_rp*(work3d2(1:nx,1:ny,1:nz_stag-1)+work3d2(1:nx,1:ny,2:nz_stag))
                deallocate(work3d2)
             end if
          end if
       case('ERA5ML')
          EXISTS(VAR_HGT)   = .false.
          COMPUTED(VAR_HGT) = .true.
          !
          ! To compute geopotential from model levels the following variables are required: ZSFC,PSFC,Q,T,a_coeff,b_coeff
          !
          ! Use work3d to save Qv
          if(master_model) then
             istat = nf90_inq_varid(ncID,DICTIONARY(VAR_QV),varID)
          end if
          call parallel_bcast(istat,1,0)
          if(istat.ne.0) then
             EXISTS(VAR_QV)    = .false.
             COMPUTED(VAR_HGT) = .false.
             MY_ERR%flag       = 1
             MY_ERR%message    = 'Unable to find variable '//TRIM(DICTIONARY(VAR_QV))
             return
          else
             if(master_model) then
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d, &
                     MY_ERR)
             end if
          end if
          !
          ! Use work3d2 to save T
          if(master_model) then
             istat = nf90_inq_varid(ncID,DICTIONARY(VAR_T),varID)
          end if
          call parallel_bcast(istat,1,0)
          if(istat.ne.0) then
             EXISTS(VAR_T)     = .false.
             COMPUTED(VAR_HGT) = .false.
             MY_ERR%flag       = 1
             MY_ERR%message    = 'Unable to find variable '//TRIM(DICTIONARY(VAR_T))
             return
          else
             if(master_model) then
                allocate(work3d2(nx,ny,nz))
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d2, &
                     MY_ERR)
             end if
          end if
          !
          ! computes zmodel (geopotential at model levels)
          !
          if(master_model) then
             call dbs_get_geopotential_hyb(nx,ny,nz,         &
                  a_coeff,          &
                  b_coeff,          &
                  GL_METMODEL%topg, &
                  psfc,             &
                  work3d,           &
                  work3d2,          &
                  zmodel,           &
                  MY_ERR)
             deallocate(work3d2)
          end if
          !
       case default
          if(master_model) then
             istat = nf90_inq_varid(ncID,DICTIONARY(VAR_HGT),varID)
          end if
          call parallel_bcast(istat,1,0)
          if(istat.ne.0) then
             EXISTS(VAR_HGT) = .false.
             MY_ERR%flag     = 1
             MY_ERR%message  = 'Unable to find variable '//TRIM(DICTIONARY(VAR_HGT))
             return
          else
             if(master_model) then
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     zmodel, &
                     MY_ERR)
             end if
          end if
       end select
       !
       !
       npoin = nx*ny*nz
       call parallel_bcast(zmodel,npoin,0)

       select case(MY_MET%meteo_data_type)
       case('WRF','ERA5','ERA5ML')
          zmodel = zmodel/9.81_rp  ! convert geopotential to height
       end select

       do iz = 1,nz
          zmodel(1:nx,1:ny,iz) = zmodel(1:nx,1:ny,iz) - GL_METMODEL%topg(1:nx,1:ny)  ! above model terrain
       end do
       !
       !  Interpolate zmodel at my point coordinates:  my_zmodel(my_x,my_y,model_nz) = my_zmodel(my_npoin,model_nz)
       !
       ipoin = 0
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             ipoin = ipoin + 1
             !
             ix = MY_MET%el_indexes(1,ipoin)
             iy = MY_MET%el_indexes(2,ipoin)
             !
             myshape(1:4) = MY_MET%interp_factor(1:4,ipoin)
             !
             do k = 1,nz
                my_zmodel(ipoin,k) = myshape(1)*zmodel(ix  ,iy  ,k) + &
                                     myshape(2)*zmodel(ix+1,iy  ,k) + &
                                     myshape(3)*zmodel(ix+1,iy+1,k) + &
                                     myshape(4)*zmodel(ix  ,iy+1,k)
                !
             end do
          end do
       end do
       !
       ! Calculate interpolation factors my_iz(npoin,my_nz) my_s(npoin,my_nz) (same for all variables at current
       ! time step)
       !
       ipoin = 0
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             ipoin = ipoin + 1
             zc(1:nz) = my_zmodel(ipoin,1:nz)
             do k = my_kbs,my_kbe
                z = MY_GRID%z_c(i,j,k) - MY_GRID%h_c(i,j)   ! above terrain
                call grid_get_shapez (1, nz,zc, z, my_iz(ipoin,k), my_sz(ipoin,k))
             end do
          end do
       end do
       !
       !  Interpolate zmodel at profile coordinates
       !
       ielem = GL_METPROFILES%el_po  ! met model element
       iy_prof = (ielem-1)/(nx-1) + 1
       ix_prof = ielem - (iy_prof-1)*(nx-1)
       !
       s = GL_METPROFILES%s_po
       t = GL_METPROFILES%t_po
       st= s*t
       !
       myshape(1) = (1.0_rp-t-s+st)*0.25_rp                           !  4     3
       myshape(2) = (1.0_rp-t+s-st)*0.25_rp                           !
       myshape(3) = (1.0_rp+t+s+st)*0.25_rp                           !
       myshape(4) = (1.0_rp+t-s-st)*0.25_rp                           !  1     2
       !
       do k = 1,nz
          GL_METPROFILES%zavl(k,my_it) = myshape(1)*zmodel(ix_prof  ,iy_prof  ,k) + &
               myshape(2)*zmodel(ix_prof+1,iy_prof  ,k) + &
               myshape(3)*zmodel(ix_prof+1,iy_prof+1,k) + &
               myshape(4)*zmodel(ix_prof  ,iy_prof+1,k)
       end do
       !
       !  2. Pressure
       !
       select case(MY_MET%meteo_data_type)
       case('WRF')
          if(master_model) then
             istat = nf90_inq_varid(ncID,DICTIONARY(VAR_P),varID)
          end if
          call parallel_bcast(istat,1,0)
          if(istat.ne.0) then
             EXISTS(VAR_P)   = .false.
             MY_ERR%flag     = 1
             MY_ERR%message  = 'Unable to find variable '//TRIM(DICTIONARY(VAR_P))
             return
          else
             if(master_model) then
                allocate(work3d2(nx,ny,nz))
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d, &
                     MY_ERR)
                istat = nf90_inq_varid(ncID,'PB',varID)
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d2, &
                     MY_ERR)
                work3d = work3d + work3d2
                deallocate(work3d2)
             end if
          end if
       case('ERA5ML')
          if(master_model) then
             do iz = 1,nz
                work3d(:,:,iz) = 0.5_rp*(a_coeff(iz)+a_coeff(iz-1)) +  &
                     0.5_rp*(b_coeff(iz)+b_coeff(iz-1)) *  psfc(:,:)
             end do
          end if
       case('GRIB2NC','GFS','GDAS','ERA5')
          if(master_model) then
             do iz = 1,nz
                work3d(:,:,iz) = GL_METMODEL%pres(iz)
             end do
          end if
       end select
       !
       npoin = nx*ny*nz
       call parallel_bcast(work3d,npoin,0)
       call dbs_interpola3d(nx,ny,nz,work3d, &
            MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
            my_iz,my_sz,MY_MET%my_pc(:,:,:,my_it),MY_ERR)
       !
       !     profile
       do k = 1,nz
          GL_METPROFILES%p(k,my_it) = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
               myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
               myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
               myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
       end do
       !
       !  3. Potential temperature
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_TP),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_TP)   = .false.
          COMPUTED(VAR_TP) = .true.
       else
          if(master_model) then
             call dbs_read_metmodel_var4d(varID, &
                  nx,ny,nz,it, &
                  GL_METMODEL%xreversed, &
                  GL_METMODEL%yreversed, &
                  GL_METMODEL%zreversed, &
                  work3d, &
                  MY_ERR)
             select case(MY_MET%meteo_data_type)
             case('WRF')
                work3d = work3d + 300.0_rp ! add reference temperature
             end select
          end if
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_tpc(:,:,:,my_it),MY_ERR)

          !    store profile
          do k = 1,nz
             GL_METPROFILES%tp(k,my_it) = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
                  myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
                  myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
                  myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
          end do
       end if
       !
       !  4. Temperature
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_T),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_T)   = .false.
          COMPUTED(VAR_T) = .true.
       else
          if(master_model) then
             call dbs_read_metmodel_var4d(varID, &
                  nx,ny,nz,it, &
                  GL_METMODEL%xreversed, &
                  GL_METMODEL%yreversed, &
                  GL_METMODEL%zreversed, &
                  work3d, &
                  MY_ERR)
          end if
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_tc(:,:,:,my_it),MY_ERR)
          !     profile
          do k = 1,nz
             GL_METPROFILES%t (k,my_it) = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
                  myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
                  myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
                  myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
          end do
       end if
       !
       ! Check existence of T and TP (at least one is mandatory)
       !
       if (EXISTS(VAR_TP) .or. EXISTS(VAR_T)) then
          if(COMPUTED(VAR_TP)) then
             ! Tp = T*(1bar/P))**(R/cp)
             MY_MET%my_tpc(:,:,:,my_it) = MY_MET%my_tc(:,:,:,my_it)*((1.01e5_rp/MY_MET%my_pc(:,:,:,my_it))**(0.285_rp))
             !     profile
             do k = 1,nz
                GL_METPROFILES%tp(k,my_it) = GL_METPROFILES%t(k,my_it)*((1.01e5_rp/GL_METPROFILES%p(k,my_it))**(0.285_rp))
             end do
          end if
          if(COMPUTED(VAR_T)) then
             ! T = Tp*((p+pb)/pref_theta)**(R/cp)
             MY_MET%my_tc(:,:,:,my_it) = MY_MET%my_tpc(:,:,:,my_it)*((MY_MET%my_pc(:,:,:,my_it)/1e5_rp)**(0.285_rp))
             !     profile
             do k = 1,nz
                GL_METPROFILES%t (k,my_it) = GL_METPROFILES%tp(k,my_it)*((GL_METPROFILES%p(k,my_it)/1e5_rp)**(0.285_rp))
             end do
          end if
       else
          MY_ERR%flag     = 1
          MY_ERR%message  = 'Unable to find variables '//TRIM(DICTIONARY(VAR_T))//' and '//TRIM(DICTIONARY(VAR_TP))
          return
       end if
       !
       !  5. Specific humidity
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_QV),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_QV)   = .false.
          COMPUTED(VAR_QV) = .true.
       else
          if(master_model) then
             call dbs_read_metmodel_var4d(varID, &
                  nx,ny,nz,it, &
                  GL_METMODEL%xreversed, &
                  GL_METMODEL%yreversed, &
                  GL_METMODEL%zreversed, &
                  work3d, &
                  MY_ERR)
             !
             select case(MY_MET%meteo_data_type)
             case('WRF')
                ! WRF gives water vapor mixing ratio (e),
                ! so the conversion to specific humidity
                ! qv = e/1+e is needed
                work3d = work3d/(1.0_rp+work3d)
             end select
          end if
          !
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_qvc(:,:,:,my_it),MY_ERR)
          !     profile
          do k = 1,nz
             GL_METPROFILES%qv(k,my_it) = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
                  myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
                  myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
                  myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
          end do
       end if
       !
       !  6. Relative humidity
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_RH),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_RH)   = .false.
          COMPUTED(VAR_RH) = .true. ! I need to compute it
       end if
       !
       if(COMPUTED(VAR_QV) .and. EXISTS(VAR_RH)) then
          if(master_model) then
             call dbs_read_metmodel_var4d(varID, &
                  nx,ny,nz,it, &
                  GL_METMODEL%xreversed, &
                  GL_METMODEL%yreversed, &
                  GL_METMODEL%zreversed, &
                  work3d, &
                  MY_ERR)
          end if
          !
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_qvc(:,:,:,my_it),MY_ERR)
          !
          !  Convert rh to qv
          !  following Bolton (1980)
          !  es = 6.112 * exp((17.67 * T)/(T + 243.5));  saturation vapor pressure in mb, T in C
          !  e  = es * (RH/100.0);                       vapor pressure in mb
          !  q  = (0.622 * e)/(p - (0.378 * e))          specific humidity in kg/kg, p in mb
          !
          do k = my_kbs,my_kbe
             do j = my_jbs,my_jbe
                do i = my_ibs,my_ibe
                   rh = MY_MET%my_qvc(i,j,k,my_it)                ! Relative Humidity in percent
                   tc = MY_MET%my_tc (i,j,k,my_it)-273.15_rp      ! Temperature in celsius
                   es = 6.112_rp*exp( 17.67_rp*tc/(243.5_rp+tc) ) ! Saturation Pressure in mb
                   e  = 0.01_rp*es*rh                             ! Vapor pressure in mb
                   ! Specific humidity in kg/kg
                   MY_MET%my_qvc(i,j,k,my_it) = 0.622_rp*e/(0.01_rp*MY_MET%my_pc(i,j,k,my_it)-0.378_rp*e)
                end do
             end do
          end do
          !     profile
          do k = 1,nz
             rh = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
                  myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
                  myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
                  myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
             tc = GL_METPROFILES%t(k,my_it)-273.15_rp
             es = 6.112_rp*exp( (17.67_rp*tc)/(243.5_rp+tc) )
             e  = 0.01_rp*es*rh
             GL_METPROFILES%qv(k,my_it) = 0.622_rp*e/(0.01_rp*GL_METPROFILES%p(k,my_it)-0.378_rp*e)
          end do
       else if(COMPUTED(VAR_QV) .and. COMPUTED(VAR_RH)) then
          MY_ERR%flag = 1
          MY_ERR%message  = 'Unable to find variables '//TRIM(DICTIONARY(VAR_QV))//' and '//TRIM(DICTIONARY(VAR_RH))
          return
       end if
       !
       !  7. Virtual temperature   Tv = T*( 1+ 1.6077*e)/(1+e)
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_TV),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_TV)   = .false.
          COMPUTED(VAR_TV) = .true.
          !
          do k = my_kbs,my_kbe
             do j = my_jbs,my_jbe
                do i = my_ibs,my_ibe
                   ! Mixing ratio
                   e = MY_MET%my_qvc(i,j,k,my_it) / (1.0_rp - MY_MET%my_qvc(i,j,k,my_it))
                   MY_MET%my_tvc(i,j,k,my_it) = MY_MET%my_tc(i,j,k,my_it)*(1.0_rp + 1.6077_rp*e)/(1.0_rp+e)
                end do
             end do
          end do
          !     profile
          do k = 1,nz
             ! Mixing ratio e = qv/(1-qv)
             e = GL_METPROFILES%qv(k,my_it)/(1.0_rp - GL_METPROFILES%qv(k,my_it))
             GL_METPROFILES%tv(k,my_it) = GL_METPROFILES%t(k,my_it)*(1.0_rp + 1.6077_rp*e)/(1.0_rp+e)
          end do
       else
          if(master_model) then
             call dbs_read_metmodel_var4d(varID, &
                  nx,ny,nz,it, &
                  GL_METMODEL%xreversed, &
                  GL_METMODEL%yreversed, &
                  GL_METMODEL%zreversed, &
                  work3d, &
                  MY_ERR)
          end if
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_tvc(:,:,:,my_it),MY_ERR)
          !     profile
          do k = 1,nz
             GL_METPROFILES%tv(k,my_it) = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
                  myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
                  myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
                  myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
          end do
       end if
       !
       !  8. Density. Gas law using virtual temperature (account for water in air)
       !
       MY_MET%my_rhoc(:,:,:,my_it) = MY_MET%my_pc(:,:,:,my_it)/(287.06_rp*MY_MET%my_tvc(:,:,:,my_it))
       EXISTS  (VAR_RHO) = .false.
       COMPUTED(VAR_RHO) = .true.
       !
       !     profile
       do k = 1,nz
          GL_METPROFILES%rho(k,my_it) = GL_METPROFILES%p(k,my_it)/(287.06_rp*GL_METPROFILES%tv(k,my_it))
       end do
       !
       !  9. u-velocity
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_U),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_U)   = .false.
          MY_ERR%flag     = 1
          MY_ERR%message  = 'Unable to find variable '//TRIM(DICTIONARY(VAR_U))
          return
       else
          if(master_model) then
             select case(MY_MET%meteo_data_type)
             case('WRF')
                allocate(work3d2(nx_stag,ny,nz))
                call dbs_read_metmodel_var4d(varID, &
                     nx_stag,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d2, &
                     MY_ERR)
                work3d = 0.5_rp*(work3d2(1:nx_stag-1,1:ny,1:nz) + work3d2(2:nx_stag,1:ny,1:nz)) ! u box
                deallocate(work3d2)
             case default
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d, &
                     MY_ERR)
             end select
          end if
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_uc(:,:,:,my_it),MY_ERR)
          !
          !     profile
          do k = 1,nz
             GL_METPROFILES%u (k,my_it) = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
                  myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
                  myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
                  myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
          end do
       end if
       !
       !  10. v-velocity
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_V),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_V)   = .false.
          MY_ERR%flag     = 1
          MY_ERR%message  = 'Unable to find variable '//TRIM(DICTIONARY(VAR_V))
          return
       else
          if(master_model) then
             select case(MY_MET%meteo_data_type)
             case('WRF')
                allocate(work3d2(nx,ny_stag,nz))
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny_stag,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d2, &
                     MY_ERR)
                work3d = 0.5_rp*(work3d2(1:nx,1:ny_stag-1,1:nz) + work3d2(1:nx,2:ny_stag,1:nz)) ! v box
                deallocate(work3d2)
             case default
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d, &
                     MY_ERR)
             end select
          end if
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_vc(:,:,:,my_it),MY_ERR)
          !
          !     profile
          do k = 1,nz
             GL_METPROFILES%v (k,my_it) = myshape(1)*work3d(ix_prof  ,iy_prof  ,k) + &
                  myshape(2)*work3d(ix_prof+1,iy_prof  ,k) + &
                  myshape(3)*work3d(ix_prof+1,iy_prof+1,k) + &
                  myshape(4)*work3d(ix_prof  ,iy_prof+1,k)
          end do
       end if
       !
       !Wind rotations
       if(rotate_winds) then
          do k = my_kbs,my_kbe
             tmp_wind(:,:)             =  MY_MET%my_vc(:,:,k,my_it)*sin_alpha(:,:) +  MY_MET%my_uc(:,:,k,my_it)*cos_alpha(:,:)
             MY_MET%my_vc(:,:,k,my_it) =  MY_MET%my_vc(:,:,k,my_it)*cos_alpha(:,:) -  MY_MET%my_uc(:,:,k,my_it)*sin_alpha(:,:)
             MY_MET%my_uc(:,:,k,my_it) =  tmp_wind(:,:)
          end do
          !
          do k = 1,nz
             !Rotate profile winds
             tmp_wind_prof             = GL_METPROFILES%v(k,my_it)*sin_alpha_prof + GL_METPROFILES%u(k,my_it)*cos_alpha_prof
             GL_METPROFILES%v(k,my_it) = GL_METPROFILES%v(k,my_it)*cos_alpha_prof - GL_METPROFILES%u(k,my_it)*sin_alpha_prof
             GL_METPROFILES%u(k,my_it) = tmp_wind_prof
          end do
       end if
       !
       !  11. w-velocity
       !
       if(master_model) then
          istat = nf90_inq_varid(ncID,DICTIONARY(VAR_W),varID)
       end if
       call parallel_bcast(istat,1,0)
       if(istat.ne.0) then
          EXISTS(VAR_W)   = .false.
          COMPUTED(VAR_W) = .true.
          MY_MET%my_wc(:,:,:,my_it) = 0.0_rp
          call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_W))// &
               ' not found in met model file')
       else
          if(master_model) then
             select case(MY_MET%meteo_data_type)
             case('WRF')
                allocate(work3d2(nx,ny,nz_stag))
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz_stag,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d2, &
                     MY_ERR)
                work3d = 0.5_rp*(work3d2(1:nx,1:ny,1:nz_stag-1) + work3d2(1:nx,1:ny,2:nz_stag)) ! w box
                deallocate(work3d2)
             case default
                call dbs_read_metmodel_var4d(varID, &
                     nx,ny,nz,it, &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     GL_METMODEL%zreversed, &
                     work3d, &
                     MY_ERR)
             end select
          end if
          npoin = nx*ny*nz
          call parallel_bcast (work3d,npoin,0)
          call dbs_interpola3d(nx,ny,nz,work3d, &
               MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
               my_iz,my_sz,MY_MET%my_wc(:,:,:,my_it),MY_ERR)
          !
          select case(MY_MET%meteo_data_type)
          case('WRF')
             continue
          case default
             !
             !  Convert from Omega velocity to w = - omega/(rho*g) = - omega*R*Tv/(P*g)
             !
             MY_MET%my_wc(:,:,:,my_it) = -MY_MET%my_wc(:,:,:,my_it)/(MY_MET%my_rhoc(:,:,:,my_it)*9.81_rp)
          end select
       end if
       !
       ! 12. friction velocity ust
       !     Note: for variables below, computations have to be done by "ground" processors only. Values are extended
       !     to all processors using COMM_GRIDZ communicator (but after the time loop in order to reduce communications)
       !
       if(COMPUTED(VAR_UST)) then
          if(my_kbs.eq.1) then
             call phys_get_ust(my_ibs,my_ibe,my_jbs,my_jbe, &
                  MY_GRID%z_c(:,:,my_kbs+1)-MY_GRID%z_c(:,:,my_kbs), MY_MET%my_z0c,      &
                  MY_MET%my_tvc (:,:,my_kbs+1,my_it), MY_MET%my_tvc(:,:,my_kbs,  my_it), &
                  MY_MET%my_uc  (:,:,my_kbs+1,my_it), MY_MET%my_vc (:,:,my_kbs+1,my_it), &
                  MY_MET%my_ustc(:,:,my_it), MY_ERR)
          else
             MY_MET%my_ustc(:,:,my_it) = 0.0_rp
          end if
       end if
       !
       ! 13. Monin-Obukhov length
       !
       if(COMPUTED(VAR_MON)) then
          if(my_kbs.eq.1) then
             call phys_get_monin(my_ibs,my_ibe,my_jbs,my_jbe, &
                  MY_GRID%z_c(:,:,my_kbs+1)-MY_GRID%z_c(:,:,my_kbs), MY_MET%my_z0c,      &
                  MY_MET%my_tvc (:,:,my_kbs+1,my_it), MY_MET%my_tvc(:,:,my_kbs,  my_it), &
                  MY_MET%my_uc  (:,:,my_kbs+1,my_it), MY_MET%my_vc (:,:,my_kbs+1,my_it), &
                  MY_MET%my_ustc(:,:,my_it), MY_MET%my_monc(:,:,my_it), MY_ERR)
          else
             MY_MET%my_monc(:,:,my_it) = 0.0_rp
          end if
       end if
       !
       ! 14. PBLH
       !
       if(COMPUTED(VAR_PBLH)) then
          MY_MET%my_pblhc(:,:,my_it) = 1500.0_rp  ! for next version estimate phlb from (6)
       end if
       !
    end do  ! time loop

    if(COMPUTED(VAR_PREC)) then
       allocate(my_acc_prec_ref(my_ibs:my_ibe,my_jbs:my_jbe))
       my_it = -1
       do it = MY_MET%its-1,MY_MET%ite
          my_it = my_it + 1
          !
          if(it.lt.1) then
             my_acc_prec_ref = 0.0_rp
             CYCLE
          end if
          if(master_model) then
             istat = nf90_inq_varid(ncID,DICTIONARY(VAR_ACCP),varID)
          end if
          call parallel_bcast(istat,1,0)
          if(istat.ne.0) then
             EXISTS(VAR_ACCP)    = .false.
             COMPUTED(VAR_ACCP)  = .false.
          else
             if(master_model) then
                call dbs_read_metmodel_var3d(varID,                 &
                     nx,ny,it,              &
                     GL_METMODEL%xreversed, &
                     GL_METMODEL%yreversed, &
                     work2d,                &
                     MY_ERR)
                select case(MY_MET%meteo_data_type)
                case('WRF')
                   istat = nf90_inq_varid(ncID,'RAINC',varID)
                   if(istat.eq.nf90_noerr) then
                      allocate(work2d2(nx,ny))
                      call dbs_read_metmodel_var3d(varID,                 &
                           nx,ny,it,              &
                           GL_METMODEL%xreversed, &
                           GL_METMODEL%yreversed, &
                           work2d2,               &
                           MY_ERR)
                      work2d = work2d + work2d2
                      deallocate(work2d2)
                   end if
                end select
             end if
             npoin = nx*ny
             call parallel_bcast(work2d,npoin,0)
             if(my_it.gt.0) then
                !save accumulated precipitation in my_prec
                call dbs_interpola2d(nx,ny,work2d, &
                     MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
                     MY_MET%my_prec(:,:,my_it),MY_ERR)
             else
                call dbs_interpola2d(nx,ny,work2d, &
                     MY_MET%npoin,MY_MET%el_indexes,MY_MET%interp_factor, &
                     my_acc_prec_ref,MY_ERR)
             end if
          end if
          !
       end do
       !
       if (EXISTS(VAR_ACCP)) then
          !compute precipitation rates
          MY_MET%my_prec(:,:,2:MY_MET%nt) = 3600.0_rp * (MY_MET%my_prec(:,:,2:MY_MET%nt) - MY_MET%my_prec(:,:,1:MY_MET%nt-1)) / &
               GL_METMODEL%dt
          MY_MET%my_prec(:,:,1)          = 3600.0_rp * (MY_MET%my_prec(:,:,1) - my_acc_prec_ref(:,:)) / GL_METMODEL%dt
       else
          MY_MET%my_prec = 0.0_rp
       end if
       deallocate(my_acc_prec_ref)
       !
    end if
    !
    !
    !*** deallocate memory
    !
    deallocate(work2d)
    deallocate(work3d)
    deallocate(psfc)
    deallocate(zmodel)
    !
    select case(MY_MET%meteo_data_type)
    case('WRF')
       if (allocated(cos_alpha)) deallocate(cos_alpha)
       if (allocated(sin_alpha)) deallocate(sin_alpha)
       if (allocated(tmp_wind )) deallocate(tmp_wind )
    case('ERA5ML')
       if (allocated(a_coeff)) deallocate(a_coeff)
       if (allocated(b_coeff)) deallocate(b_coeff)
    end select
    !
    !*** Finally, extend ust and mon to all processors (if necessary)
    !
    if(COMPUTED(VAR_UST)) call parallel_sum(MY_MET%my_ustc,COMM_GRIDZ)  ! only along z
    if(COMPUTED(VAR_MON)) call parallel_sum(MY_MET%my_monc,COMM_GRIDZ)  ! only along z
    !
    !*** Print other warning messages
    !
    if(COMPUTED(VAR_PBLH)) then
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_PBLH))// &
            ' not found in met model file. Assumed constant')
    end if
    if(COMPUTED(VAR_SMOI)) then
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_SMOI))// &
            ' not found in met model file. Assumed constant')
    end if
    if(COMPUTED(VAR_PREC)) then
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_PREC))// &
            ' not found in met model file')
    end if
    if(COMPUTED(VAR_U10)) then
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_U10))// &
            ' not found in met model file. Assumed constant')
    end if
    if(COMPUTED(VAR_V10)) then
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_V10))// &
            ' not found in met model file. Assumed constant')
    end if
    if(COMPUTED(VAR_T2)) then
       call task_wriwarn(MY_ERR,'Variable '//TRIM(DICTIONARY(VAR_T2))// &
            ' not found in met model file. Assumed constant')
    end if
    !
    !*** Close file
    !
    if(master_model) then
       istat = nf90_close(ncID)
    end if
    !
    !*** Prints log file
    !
    if(master_model) then
       write(MY_FILES%lulog,10) TRIM(MY_FILES%file_met),TRIM(MY_MET%meteo_data_type)
10     format(2x,'List of meteorological variables found:',/,&
            2x,'File      : ',a,/,&
            2x,'Data type : ',a,/,/,&
            2x,'Variable      Found    Computed',/,&
            2x,'-------------------------------')
       !
       write(MY_FILES%lulog,20) &
            TRIM(DICTIONARY(VAR_TOPO )),EXISTS(VAR_TOPO ),COMPUTED(VAR_TOPO ), &
            TRIM(DICTIONARY(VAR_LMASK)),EXISTS(VAR_LMASK),COMPUTED(VAR_LMASK), &
            TRIM(DICTIONARY(VAR_LUSE )),EXISTS(VAR_LUSE ),COMPUTED(VAR_LUSE ), &
            TRIM(DICTIONARY(VAR_Z0   )),EXISTS(VAR_Z0   ),COMPUTED(VAR_Z0   ), &
            TRIM(DICTIONARY(VAR_PBLH )),EXISTS(VAR_PBLH ),COMPUTED(VAR_PBLH ), &
            TRIM(DICTIONARY(VAR_UST  )),EXISTS(VAR_UST  ),COMPUTED(VAR_UST  ), &
            TRIM(DICTIONARY(VAR_SMOI )),EXISTS(VAR_SMOI ),COMPUTED(VAR_SMOI ), &
            TRIM(DICTIONARY(VAR_PREC )),EXISTS(VAR_PREC ),COMPUTED(VAR_PREC ), &
            TRIM(DICTIONARY(VAR_U10  )),EXISTS(VAR_U10  ),COMPUTED(VAR_U10  ), &
            TRIM(DICTIONARY(VAR_V10  )),EXISTS(VAR_V10  ),COMPUTED(VAR_V10  ), &
            TRIM(DICTIONARY(VAR_T2   )),EXISTS(VAR_T2   ),COMPUTED(VAR_T2   ), &
            TRIM(DICTIONARY(VAR_MON  )),EXISTS(VAR_MON  ),COMPUTED(VAR_MON  ), &
            TRIM(DICTIONARY(VAR_PSFC )),EXISTS(VAR_PSFC ),COMPUTED(VAR_PSFC ), &
            TRIM(DICTIONARY(VAR_HGT  )),EXISTS(VAR_HGT  ),COMPUTED(VAR_HGT  ), &
            TRIM(DICTIONARY(VAR_P    )),EXISTS(VAR_P    ),COMPUTED(VAR_P    ), &
            TRIM(DICTIONARY(VAR_T    )),EXISTS(VAR_T    ),COMPUTED(VAR_T    ), &
            TRIM(DICTIONARY(VAR_TP   )),EXISTS(VAR_TP   ),COMPUTED(VAR_TP   ), &
            TRIM(DICTIONARY(VAR_TV   )),EXISTS(VAR_TV   ),COMPUTED(VAR_TV   ), &
            TRIM(DICTIONARY(VAR_U    )),EXISTS(VAR_U    ),COMPUTED(VAR_U    ), &
            TRIM(DICTIONARY(VAR_V    )),EXISTS(VAR_V    ),COMPUTED(VAR_V    ), &
            TRIM(DICTIONARY(VAR_W    )),EXISTS(VAR_W    ),COMPUTED(VAR_W    ), &
            TRIM(DICTIONARY(VAR_RH   )),EXISTS(VAR_RH   ),COMPUTED(VAR_RH   ), &
            TRIM(DICTIONARY(VAR_QV   )),EXISTS(VAR_QV   ),COMPUTED(VAR_QV   ), &
            TRIM(DICTIONARY(VAR_RHO  )),EXISTS(VAR_RHO  ),COMPUTED(VAR_RHO  )
       !
20     format(100(2x,a12,'  -  ',l1,'  -  ',l1,/))
    end if

    return
  end subroutine dbs_read_metmodel_data
  !
  !-----------------------------------------
  !    subroutine dbs_interpola2d
  !-----------------------------------------
  !
  !>   @brief
  !>   Interpolates a variable from the 2D Q1 meto model grid to my_grid
  !
  subroutine dbs_interpola2d(nx,ny,var,npoin,el_indexes,interp_factor,my_var,MY_ERR,closest)
    implicit none
    !
    !>   @param nx            number of meteo model grid points along x
    !>   @param ny            number of meteo model grid points along y
    !>   @param var           var(nx,ny) variable to interpolate
    !>   @param npoin         number of points (my number of cell corners)
    !>   @param el_indexes    el_indexes(2,npoin) native meteo model element
    !>   @param interp_factor interp_factor(4,npoin) interpolation factors to interpolate from native meteo model
    !>   @param my_var        my_var(my_ibs:my_ibe,my_jbs:my_jbe) interpolated variable
    !>   @param MY_ERR        error handler
    !>   @param               if true assigns closest point rsther than interpolation (optional)
    !
    integer(ip),        intent(IN   ) :: nx
    integer(ip),        intent(IN   ) :: ny
    real(rp),           intent(IN   ) :: var(nx,ny)
    integer(ip),        intent(IN   ) :: npoin
    integer(ip),        intent(IN   ) :: el_indexes(2,npoin)
    real(rp),           intent(IN   ) :: interp_factor(4,npoin)
    real(rp),           intent(INOUT) :: my_var(my_ibs:my_ibe,my_jbs:my_jbe)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    logical, optional , intent(IN   ) :: closest
    !
    logical     :: closestpoint
    integer(ip) :: i,j,ipoin,ix,iy,k,kk
    real(rp)    :: myshape(4),smax
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_interpola2d'
    MY_ERR%message = ' '
    !
    if(present(closest)) then
       closestpoint = closest
    else
       closestpoint = .false.   ! default value
    end if
    !
    !*** Loop over my_points
    !
    ipoin = 0
    do j = my_jbs,my_jbe
       do i = my_ibs,my_ibe
          ipoin = ipoin + 1
          !
          ix = el_indexes(1,ipoin)
          iy = el_indexes(2,ipoin)
          !
          myshape(1:4) = interp_factor(1:4,ipoin)
          !
          if(closestpoint) then
             !
             !  Closest point (max value of myshape)
             !
             k    = 1
             smax = 0.0_rp
             do kk = 1,4
                if(myshape(kk).gt.smax) then
                   smax = myshape(kk)
                   k = kk
                end if
             end do
             myshape(:) = 0.0_rp
             myshape(k) = 1.0_rp
          end if
          !
          my_var(i,j) = myshape(1)*var(ix  ,iy  ) + &
                        myshape(2)*var(ix+1,iy  ) + &
                        myshape(3)*var(ix+1,iy+1) + &
                        myshape(4)*var(ix  ,iy+1)
       end do
    end do
    !
    return
  end subroutine dbs_interpola2d
  !
  !-----------------------------------------
  !    subroutine dbs_interpola3d
  !-----------------------------------------
  !
  !>   @brief
  !>   Interpolates a variable from the 2D Q1 meto model grid and profile to my_grid
  !
  subroutine dbs_interpola3d(nx,ny,nz,var,npoin,el_indexes,interp_factor,my_iz,my_sz,my_var,MY_ERR)
    implicit none
    !
    !>   @param nx            number of meteo model grid points along x
    !>   @param ny            number of meteo model grid points along y
    !>   @param nz            number of meteo model grid points along z
    !>   @param var           var(nx,ny,nz) variable to interpolate
    !>   @param npoin         number of points (my number of 2D cell corners)
    !>   @param el_indexes    el_indexes(2,npoin) native meteo model element
    !>   @param interp_factor interp_factor(4,npoin) interpolation factors to interpolate from native meteo model
    !>   @param my_var        my_var(my_ibs:my_ibe,my_jbs:my_jbe) interpolated variable
    !>   @param MY_ERR        error handler
    !
    integer(ip),        intent(IN   ) :: nx
    integer(ip),        intent(IN   ) :: ny
    integer(ip),        intent(IN   ) :: nz
    real(rp),           intent(IN   ) :: var(nx,ny,nz)
    integer(ip),        intent(IN   ) :: npoin
    integer(ip),        intent(IN   ) :: el_indexes(2,npoin)
    real(rp),           intent(IN   ) :: interp_factor(4,npoin)
    integer(ip),        intent(IN   ) :: my_iz (npoin,my_kbs:my_kbe)
    real(rp),           intent(IN   ) :: my_sz (npoin,my_kbs:my_kbe)
    real(rp),           intent(INOUT) :: my_var(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k
    integer(ip) :: ix,iy,iz
    integer(ip) :: ipoin
    real(rp)    :: myshape(4),sz
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_interpola3d'
    MY_ERR%message = ' '
    !
    !*** Loop over my_points
    !
    ipoin = 0
    do j = my_jbs,my_jbe
       do i = my_ibs,my_ibe
          ipoin = ipoin + 1
          !
          ix = el_indexes(1,ipoin)
          iy = el_indexes(2,ipoin)
          !
          myshape(1:4) = interp_factor(1:4,ipoin)
          !
          do k = my_kbs,my_kbe
             sz = my_sz(ipoin,k)
             iz = my_iz(ipoin,k)
             !
             my_var(i,j,k) = (myshape(1)*var(ix  ,iy  ,iz  ) + &
                              myshape(2)*var(ix+1,iy  ,iz  ) + &
                              myshape(3)*var(ix+1,iy+1,iz  ) + &
                              myshape(4)*var(ix  ,iy+1,iz  ))*(1.0_rp-sz) + &
                             (myshape(1)*var(ix  ,iy  ,iz+1) + &
                              myshape(2)*var(ix+1,iy  ,iz+1) + &
                              myshape(3)*var(ix+1,iy+1,iz+1) + &
                              myshape(4)*var(ix  ,iy+1,iz+1))*sz
             !
          end do
       end do
    end do
    !
    return
  end subroutine dbs_interpola3d
  !
  !-----------------------------------------
  !    subroutine dbs_set_interp2d
  !-----------------------------------------
  !
  !>   @brief
  !>   Build met model 2D Q1 grid and set 2D interpolation factors
  !
  subroutine dbs_set_interp2d(MY_GRID,MY_MET,GL_METMODEL,MY_ERR)
    implicit none
    !
    !>   @param MY_GRID     grid configuration parameters
    !>   @param MY_MET      variables related to meteorology in MY_GRID
    !>   @param GL_METMODEL variables related to driving meteorological model
    !>   @param MY_ERR      error handler
    !
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(METEOROLOGY),   intent(INOUT) :: MY_MET
    type(METEO_MODEL),   intent(INOUT) :: GL_METMODEL
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)              :: i,j,ix,iy
    integer(ip)              :: ielem,ipoin
    integer(ip)              :: nx,ny
    real(rp)                 :: toler, s, t, st
    integer(ip), allocatable :: my_flag(:)
    real(dp),    allocatable :: lon_po(:),lat_po(:)
    integer(ip), allocatable :: el_po(:)
    real(dp),    allocatable :: s_po(:),t_po(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_set_interp2d'
    MY_ERR%message = ' '
    !
    !*** Build 2D meteo model grid (plane)
    !
    nx = GL_METMODEL%nx
    ny = GL_METMODEL%ny
    GL_METMODEL%GRID2D%nx    = nx
    GL_METMODEL%GRID2D%ny    = ny
    GL_METMODEL%GRID2D%npoin = nx * ny
    GL_METMODEL%GRID2D%nelem = (nx-1) * (ny-1)
    !
    call maths_set_lnodsQ1(GL_METMODEL%GRID2D,MY_ERR)
    !
    allocate(GL_METMODEL%GRID2D%coord(2,GL_METMODEL%GRID2D%npoin))
    ipoin = 0
    do j = 1,GL_METMODEL%ny
       do i = 1,GL_METMODEL%nx
          ipoin = ipoin + 1
          !*** longitudes are transformed to the FALL3D
          !*** range [-180,180) to avoid errors of
          !*** "grid point not found"
          if (GL_METMODEL%lon(i,j).ge.180.0_rp) then
             GL_METMODEL%GRID2D%coord(1,ipoin) = GL_METMODEL%lon(i,j) - 360.0_rp
          else
             GL_METMODEL%GRID2D%coord(1,ipoin) = GL_METMODEL%lon(i,j)
          end if
          GL_METMODEL%GRID2D%coord(2,ipoin) = GL_METMODEL%lat(i,j)
       end do
    end do
    !
    !*** Allocates
    !
    MY_MET%npoin = (my_ibe-my_ibs+1)*(my_jbe-my_jbs+1)
    !
    allocate(MY_MET%el_indexes(2,MY_MET%npoin))
    allocate(MY_MET%interp_factor(4,MY_MET%npoin))
    !
    allocate(el_po(MY_MET%npoin))
    allocate(s_po (MY_MET%npoin))
    allocate(t_po (MY_MET%npoin))
    !
    !*** List of cell corner points in my_domain
    !
    allocate(lon_po(MY_MET%npoin))
    allocate(lat_po(MY_MET%npoin))
    !
    toler = 0.01_rp  ! tolerance to prevent elsest failure at boundaries
    !
    ipoin = 0
    do j = my_jbs,my_jbe
       do i = my_ibs,my_ibe
          ipoin = ipoin + 1
          !
          lon_po(ipoin) = MY_GRID%lon_c(i)
          lat_po(ipoin) = MY_GRID%lat_c(j)
          !
          lon_po(ipoin) = max(lon_po(ipoin), -180.0_rp + toler)
          lon_po(ipoin) = min(lon_po(ipoin),  180.0_rp - toler)
          lat_po(ipoin) = max(lat_po(ipoin),  -90.0_rp + toler)
          lat_po(ipoin) = min(lat_po(ipoin),   90.0_rp - toler)
       end do
    end do
    !
    !*** Get interpolation factors (parallel interpolation)
    !
    call maths_get_host_elemQ1(GL_METMODEL%GRID2D, &
                               MY_MET%npoin,       &
                               lon_po,lat_po,      &
                               el_po,s_po,t_po,    &
                               MY_ERR)
    !
    !*** Error check across processors
    !
    allocate(my_flag(0:npes_model-1))
    my_flag(:)          = 0
    my_flag(mype_model) = MY_ERR%flag
    call parallel_sum(my_flag, COMM_MODEL)
    do i = 0,npes_model-1
       if(my_flag(i).ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'FALL3D grid point not found in meteorological model grid '
          return
       end if
    end do
    !
    !*** Compute interpolation factors
    !
    ipoin = 0
    do j = my_jbs,my_jbe
       do i = my_ibs,my_ibe
          ipoin = ipoin + 1
          !
          ielem = el_po(ipoin)  ! native met model element
          s     = s_po(ipoin)   ! shape functions
          t     = t_po(ipoin)
          st    = s*t
          !
          ! 4--3
          ! 1--2
          MY_MET%interp_factor(1,ipoin) = (1.0_rp-t-s+st)*0.25_rp
          MY_MET%interp_factor(2,ipoin) = (1.0_rp-t+s-st)*0.25_rp
          MY_MET%interp_factor(3,ipoin) = (1.0_rp+t+s+st)*0.25_rp
          MY_MET%interp_factor(4,ipoin) = (1.0_rp+t-s-st)*0.25_rp
          !
          iy = (ielem-1)/(nx-1) + 1
          ix = ielem - (iy-1)*(nx-1)
          !
          MY_MET%el_indexes(1,ipoin) = ix
          MY_MET%el_indexes(2,ipoin) = iy
          !
       end do
    end do
    !
    return
  end subroutine dbs_set_interp2d
  !
  !-----------------------------------------
  !    subroutine dbs_set_profile
  !-----------------------------------------
  !
  !>   @brief
  !>   Sets the vertical profiles
  !
  subroutine dbs_set_profile(GL_METMODEL,MY_MET,GL_METPROFILES,MY_ERR)
    implicit none
    !
    !>   @param GL_METMODEL     variables related to driving meteorological model
    !>   @param MY_MET          variables related to meteorology in MY_GRID
    !>   @param GL_METPROFILES  variables related to metrorological profiles
    !>   @param MY_ERR          error handler
    !
    type(METEO_MODEL),   intent(IN   ) :: GL_METMODEL
    type(METEOROLOGY),   intent(IN   ) :: MY_MET
    type(METEO_PROFILE), intent(INOUT) :: GL_METPROFILES
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: el_po(1),ielem,nx,ny,ix,iy
    real(rp)    :: s,t,st,myshape(4)
    real(dp)    :: lon(1),lat(1),s_po(1),t_po(1)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_set_profile'
    MY_ERR%message = ' '
    !
    GL_METPROFILES%nz = GL_METMODEL%nz
    GL_METPROFILES%nt = MY_MET%nt
    !
    allocate(GL_METPROFILES%time   (GL_METPROFILES%nt))
    allocate(GL_METPROFILES%timesec(GL_METPROFILES%nt))
    GL_METPROFILES%time    = MY_MET%time
    GL_METPROFILES%timesec = MY_MET%timesec
    !
    !*** Get interpolation factors at profle location (vent coordinates)
    !
    lon(1) = GL_METPROFILES%lon
    lat(1) = GL_METPROFILES%lat
    call maths_get_host_elemQ1(GL_METMODEL%GRID2D,1_ip,lon,lat,el_po,s_po,t_po,MY_ERR)
    !
    if(MY_ERR%flag.eq.0) then
       GL_METPROFILES%el_po = el_po(1)
       GL_METPROFILES%s_po  =  s_po(1)
       GL_METPROFILES%t_po  =  t_po(1)
    else
       call task_wriwarn(MY_ERR,'Vent coordinates outside the meteo domain, profile computed at node 1')
       GL_METPROFILES%el_po = 1
       GL_METPROFILES%s_po  = 1.0_rp
       GL_METPROFILES%t_po  = 0.0_rp
    end if
    !
    !*** Gets profile location (vent) elevation
    !
    nx = GL_METMODEL%nx
    ny = GL_METMODEL%ny
    !
    ielem = GL_METPROFILES%el_po  ! met model element
    iy    = (ielem-1)/(nx-1) + 1
    ix    = ielem - (iy-1)*(nx-1)
    !
    s  = GL_METPROFILES%s_po
    t  = GL_METPROFILES%t_po
    st = s*t
    !
    myshape(1) = (1.0_rp-t-s+st)*0.25_rp                           !  4     3
    myshape(2) = (1.0_rp-t+s-st)*0.25_rp                           !
    myshape(3) = (1.0_rp+t+s+st)*0.25_rp                           !
    myshape(4) = (1.0_rp+t-s-st)*0.25_rp                           !  1     2
    !
    GL_METPROFILES%zo = myshape(1)*GL_METMODEL%topg(ix  ,iy  ) + &
         myshape(2)*GL_METMODEL%topg(ix+1,iy  ) + &
         myshape(3)*GL_METMODEL%topg(ix+1,iy+1) + &
         myshape(4)*GL_METMODEL%topg(ix  ,iy+1)
    !
    return
  end subroutine dbs_set_profile
  !
  !-----------------------------------------
  !    subroutine dbs_out_profile
  !-----------------------------------------
  !
  !>   @brief
  !>   Writes the vertical profiles
  !
  subroutine dbs_out_profile(MY_FILES,GL_METPROFILES,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param GL_METPROFILES  variables related to metrorological profiles
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(METEO_PROFILE), intent(INOUT) :: GL_METPROFILES
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: it,iz
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_out_profile'
    MY_ERR%message = ' '
    !
    !*** Opens file
    !
    open(90,FILE=TRIM(MY_FILES%file_pro),status='unknown')
    write(90,1) GL_METPROFILES%lon,GL_METPROFILES%lat,GL_METPROFILES%zo
1   format(&
         '#            '     ,/,&
         '#  Atmospheric Profiles above the vent',/,&
         '#     lon  : ',f9.3,/,&
         '#     lat  : ',f9.3,/,&
         '#     zo   : ',f9.3,/,&
         '#            '     ,/,&
         '# Z(avl)   Density   Pressure   Temperature Specific-humidity  Wind-velocity       Wind-velocity' ,/,&
         '#  (km)    (kg/m^3)    (hPa)        (K)         (g/kg)       West->East(m/s)    North->South(m/s)',/,&
         '#----------------------------------------------------------------------------------------------')
    !
    do it = 1,GL_METPROFILES%nt
       write(90,2) GL_METPROFILES%time(it),GL_METPROFILES%timesec(it)
2      format('#',/,&
            '# time    : ',I4,5(1x,I2.2),/,&
            '# timesec : ',f16.0,   /,&
            '#')
       !
       do iz = 1,GL_METPROFILES%nz
          write(90,3) (GL_METPROFILES%zavl(iz,it))/1e3_rp, &   ! km above terrain
                       GL_METPROFILES%rho(iz,it),          &   ! kg/m3
                       GL_METPROFILES%p(iz,it)/1e2_rp,     &   ! hPa
                       GL_METPROFILES%t(iz,it),            &   ! K
                       GL_METPROFILES%qv(iz,it)*1e3_rp,    &   ! g/kg
                       GL_METPROFILES%u(iz,it),            &   ! m/s
                       GL_METPROFILES%v(iz,it)                 ! m/s
3         format(7(2x,f8.3))
       end do
    end do
    close(90)
    !
    return
  end subroutine dbs_out_profile
  !
  !-----------------------------------------
  !    subroutine dbs_read_inp_meteo
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads the meteo data block form the input file
  !
  subroutine dbs_read_inp_meteo(MY_FILES, MY_TIME, MY_MET, GL_METPROFILES, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_TIME     run time related parameters
    !>   @param MY_MET      variables related to meteorology in MY_GRID
    !>   @param GL_METPROFILES  variables related to metrorological profiles
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),    intent(INOUT) :: MY_FILES
    type(RUN_TIME),     intent(INOUT) :: MY_TIME
    type(METEOROLOGY),  intent(INOUT) :: MY_MET
    type(METEO_PROFILE),intent(INOUT) :: GL_METPROFILES
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    real(rp)              :: file_version
    character(len=s_file) :: file_inp, word
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_read_inp_meteo'
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
       MY_ERR%source  = 'dbs_read_inp_meteo'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version'
       return
    end if
    !
    !*** Reads METEO_DATA block
    !
    call inpout_get_cha (file_inp, 'METEO_DATA','METEO_DATA_FORMAT',MY_MET%meteo_data_type, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_cha (file_inp, 'METEO_DATA','METEO_DATA_DICTIONARY_FILE',word, 1, MY_ERR, .false.)
    if(MY_ERR%flag.ne.0) then
       MY_FILES%file_tbl_met = '-'
    else
       MY_FILES%file_tbl_met = word
    end if
    !
    call inpout_get_cha (file_inp, 'METEO_DATA','METEO_DATA_FILE',MY_FILES%file_met, 1, MY_ERR, .false.)
    if(MY_ERR%flag.ne.0) return
    !
    if(TRIM(MY_MET%meteo_data_type).eq.'ERA5ML') then
       call inpout_get_cha (file_inp, 'METEO_DATA','METEO_LEVELS_FILE',MY_FILES%file_hyb, 1, MY_ERR, .false.)
       if(MY_ERR%flag.ne.0) return
    end if
    !
    call inpout_get_rea (file_inp, 'METEO_DATA','DBS_BEGIN_METEO_DATA_(HOURS_AFTER_00)', MY_TIME%dbs_start, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TIME%dbs_start = MY_TIME%dbs_start*3600.0_rp  ! h -> s
    !
    call inpout_get_rea (file_inp, 'METEO_DATA','DBS_END_METEO_DATA_(HOURS_AFTER_00)', MY_TIME%dbs_end, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_TIME%dbs_end = MY_TIME%dbs_end*3600.0_rp  ! h -> s
    !
    call inpout_get_rea (file_inp, 'METEO_DATA','METEO_COUPLING_INTERVAL_(MIN)', MY_MET%meteo_coupling_interval, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    MY_MET%meteo_coupling_interval = MY_MET%meteo_coupling_interval*60  ! min --> s
    !
    !*** Reads info for profiles from SOURCE block
    !
    GL_METPROFILES%exists = .true.
    !
    call inpout_get_rea (file_inp, 'SOURCE','LON_VENT',GL_METPROFILES%lon, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    ! Input longitudes should be in the range (-180,180]
    if(GL_METPROFILES%lon.ge.180.0) GL_METPROFILES%lon = GL_METPROFILES%lon - 360.0
    !
    call inpout_get_rea (file_inp, 'SOURCE','LAT_VENT',GL_METPROFILES%lat, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    return
  end subroutine dbs_read_inp_meteo
  !
  !-----------------------------------------
  !    subroutine dbs_bcast_inp_meteo
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts meteo data block from input file
  !
  subroutine dbs_bcast_inp_meteo(MY_FILES, MY_TIME, MY_MET, GL_METPROFILES, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES    list of files
    !>   @param MY_TIME     run time related parameters
    !>   @param MY_MET      variables related to meteorology in MY_GRID
    !>   @param GL_METPROFILES  variables related to metrorological profiles
    !>   @param MY_ERR      error handler
    !
    type(FILE_LIST),    intent(INOUT) :: MY_FILES
    type(RUN_TIME),     intent(INOUT) :: MY_TIME
    type(METEOROLOGY),  intent(INOUT) :: MY_MET
    type(METEO_PROFILE),intent(INOUT) :: GL_METPROFILES
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_bcast_inp_meteo'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_FILES%file_met             ,1,0)
    call parallel_bcast(MY_FILES%file_hyb             ,1,0)
    call parallel_bcast(MY_FILES%file_tbl_met         ,1,0)
    !
    call parallel_bcast(MY_TIME%dbs_start             ,1,0)
    call parallel_bcast(MY_TIME%dbs_end               ,1,0)
    !
    call parallel_bcast(MY_MET%meteo_data_type        ,1,0)
    call parallel_bcast(MY_MET%meteo_coupling_interval,1,0)
    !
    call parallel_bcast(GL_METPROFILES%exists ,1,0)
    call parallel_bcast(GL_METPROFILES%lon    ,1,0)
    call parallel_bcast(GL_METPROFILES%lat    ,1,0)
    !
    return
  end subroutine dbs_bcast_inp_meteo
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  !
  !-----------------------------------------
  !    subroutine dbs_set_dictionary
  !-----------------------------------------
  !
  subroutine dbs_set_dictionary( meteo_data_type, MY_ERR )
    implicit none
    !
    character(len=s_name),intent(IN   ) :: meteo_data_type
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_set_dictionary'
    MY_ERR%message = ' '
    !
    EXISTS  (:) = .true.   ! default
    COMPUTED(:) = .false.
    !
    select case(meteo_data_type)
    case('WRF')
       !
       !  WRF model
       !
       DICTIONARY(DIM_LON   )  = 'west_east'
       DICTIONARY(DIM_LAT   )  = 'south_north'
       DICTIONARY(DIM_ZLEV  )  = 'bottom_top'
       DICTIONARY(DIM_TIME  )  = 'Time'
       DICTIONARY(DIM_XSTAGE)  = 'west_east_stag'
       DICTIONARY(DIM_YSTAGE)  = 'south_north_stag'
       DICTIONARY(DIM_ZSTAGE)  = 'bottom_top_stag'
       DICTIONARY(DIM_SOILAY)  = 'soil_layers_stag'
       !
       DICTIONARY(VAR_LON  ) = 'XLONG'
       DICTIONARY(VAR_LAT  ) = 'XLAT'
       DICTIONARY(VAR_ZLEV ) = '-'
       DICTIONARY(VAR_TOPO ) = 'HGT'
       DICTIONARY(VAR_LMASK) = 'LANDMASK'
       DICTIONARY(VAR_LUSE ) = 'LU_INDEX'
       DICTIONARY(VAR_Z0   ) = 'ZNT'
       !
       DICTIONARY(VAR_TIME ) = 'Times'
       DICTIONARY(VAR_PBLH ) = 'PBLH'
       DICTIONARY(VAR_UST  ) = 'UST'
       DICTIONARY(VAR_SMOI ) = 'SMOIS'
       DICTIONARY(VAR_PREC ) = 'PRECIP_RATE'
       DICTIONARY(VAR_ACCP ) = 'RAINNC'
       DICTIONARY(VAR_U10  ) = 'U10'
       DICTIONARY(VAR_V10  ) = 'V10'
       DICTIONARY(VAR_T2   ) = 'T2'
       DICTIONARY(VAR_MON  ) = 'mon'
       DICTIONARY(VAR_PSFC ) = 'PSFC'
       !
       DICTIONARY(VAR_HGT  ) = 'PHB'
       DICTIONARY(VAR_P    ) = 'P'
       DICTIONARY(VAR_T    ) = 'Temp'
       DICTIONARY(VAR_TP   ) = 'T'   ! WRF gives potential temperature
       DICTIONARY(VAR_TV   ) = 'TV'
       DICTIONARY(VAR_U    ) = 'U'
       DICTIONARY(VAR_V    ) = 'V'
       DICTIONARY(VAR_W    ) = 'W'
       DICTIONARY(VAR_RH   ) = 'RH'
       DICTIONARY(VAR_QV   ) = 'QVAPOR'
       DICTIONARY(VAR_RHO  ) = 'RHO'
       !
    case('GFS','GDAS')
       !
       !  GFS format
       !
       DICTIONARY(DIM_LON   )  = 'lon'
       DICTIONARY(DIM_LAT   )  = 'lat'
       DICTIONARY(DIM_ZLEV  )  = 'lev'
       DICTIONARY(DIM_TIME  )  = 'time'
       DICTIONARY(DIM_XSTAGE)  = '-'
       DICTIONARY(DIM_YSTAGE)  = '-'
       DICTIONARY(DIM_ZSTAGE)  = '-'
       DICTIONARY(DIM_SOILAY)  = '-'
       !
       DICTIONARY(VAR_LON  ) = 'lon'
       DICTIONARY(VAR_LAT  ) = 'lat'
       DICTIONARY(VAR_ZLEV ) = 'lev'
       DICTIONARY(VAR_TOPO ) = 'hgt'
       DICTIONARY(VAR_LMASK) = 'lnd'
       DICTIONARY(VAR_LUSE ) = 'luse'
       DICTIONARY(VAR_Z0   ) = 'z0'
       !
       DICTIONARY(VAR_TIME ) = 'time'
       DICTIONARY(VAR_PBLH ) = 'pblh'
       DICTIONARY(VAR_UST  ) = 'ust'
       DICTIONARY(VAR_SMOI ) = 'smoi'
       DICTIONARY(VAR_PREC ) = 'prate'
       DICTIONARY(VAR_ACCP ) = 'ACC_PRECIP'
       DICTIONARY(VAR_U10  ) = 'u10'
       DICTIONARY(VAR_V10  ) = 'v10'
       DICTIONARY(VAR_T2   ) = 't2'
       DICTIONARY(VAR_MON  ) = 'mon'
       DICTIONARY(VAR_PSFC ) = 'psfc'
       !
       DICTIONARY(VAR_HGT  ) = 'z'
       DICTIONARY(VAR_P    ) = 'p'
       DICTIONARY(VAR_T    ) = 't'
       DICTIONARY(VAR_TP   ) = 'tp'
       DICTIONARY(VAR_TV   ) = 'tv'
       DICTIONARY(VAR_U    ) = 'u'
       DICTIONARY(VAR_V    ) = 'v'
       DICTIONARY(VAR_W    ) = 'w'
       DICTIONARY(VAR_RH   ) = 'rh'
       DICTIONARY(VAR_QV   ) = 'qv'
       DICTIONARY(VAR_RHO  ) = 'rho'
       !
    case('GRIB2NC')
       !
       !  GRIB2NC format
       !  NOTE: conversion with wgrib2 assumed for variable names
       !
       DICTIONARY(DIM_LON   )  = 'longitude'
       DICTIONARY(DIM_LAT   )  = 'latitude'
       DICTIONARY(DIM_ZLEV  )  = 'lev'
       DICTIONARY(DIM_TIME  )  = 'time'
       DICTIONARY(DIM_XSTAGE)  = '-'
       DICTIONARY(DIM_YSTAGE)  = '-'
       DICTIONARY(DIM_ZSTAGE)  = '-'
       DICTIONARY(DIM_SOILAY)  = '-'
       !
       DICTIONARY(VAR_LON  ) = 'longitude'
       DICTIONARY(VAR_LAT  ) = 'latitude'
       DICTIONARY(VAR_ZLEV ) = 'lev'
       DICTIONARY(VAR_TOPO ) = 'HGT_surface'
       DICTIONARY(VAR_LMASK) = 'LAND_surface'
       DICTIONARY(VAR_LUSE ) = 'LUSE'
       DICTIONARY(VAR_Z0   ) = 'z0'
       !
       DICTIONARY(VAR_TIME ) = 'time'
       DICTIONARY(VAR_PBLH ) = 'HPBL_surface'
       DICTIONARY(VAR_UST  ) = 'UST'
       DICTIONARY(VAR_SMOI ) = 'SMOIS'
       DICTIONARY(VAR_PREC ) = 'PRATE_surface'
       DICTIONARY(VAR_ACCP ) = 'ACC_PRECIP'
       DICTIONARY(VAR_U10  ) = 'UGRD_10maboveground'
       DICTIONARY(VAR_V10  ) = 'VGRD_10maboveground'
       DICTIONARY(VAR_T2   ) = 'TMP_2maboveground'
       DICTIONARY(VAR_MON  ) = 'mon'
       DICTIONARY(VAR_PSFC ) = 'PRES_surface'
       !
       DICTIONARY(VAR_HGT  ) = 'HGT'
       DICTIONARY(VAR_P    ) = 'p'
       DICTIONARY(VAR_T    ) = 'TMP'
       DICTIONARY(VAR_TP   ) = 'tp'
       DICTIONARY(VAR_TV   ) = 'tv'
       DICTIONARY(VAR_U    ) = 'UGRD'
       DICTIONARY(VAR_V    ) = 'VGRD'
       DICTIONARY(VAR_W    ) = 'VVEL'
       DICTIONARY(VAR_RH   ) = 'RH'
       DICTIONARY(VAR_QV   ) = 'qv'
       DICTIONARY(VAR_RHO  ) = 'rho'
       !
    case('ERA5','ERA5ML')
       !
       !  ERA5 format
       !
       DICTIONARY(DIM_LON   )  = 'longitude'
       DICTIONARY(DIM_LAT   )  = 'latitude'
       DICTIONARY(DIM_ZLEV  )  = 'level'
       DICTIONARY(DIM_TIME  )  = 'time'
       DICTIONARY(DIM_XSTAGE)  = '-'
       DICTIONARY(DIM_YSTAGE)  = '-'
       DICTIONARY(DIM_ZSTAGE)  = '-'
       DICTIONARY(DIM_SOILAY)  = '-'
       !
       DICTIONARY(VAR_LON  )   = 'longitude'     !longitude (degrees_east)
       DICTIONARY(VAR_LAT  )   = 'latitude'      !latitude (degrees_north)
       DICTIONARY(VAR_ZLEV )   = 'level'         !pressure_level (millibars)
       DICTIONARY(VAR_TIME )   = 'time'          !time (hours since 1900-01-01 00:00:00.0)
       !
       DICTIONARY(VAR_TOPO )   = 'z_2'           !Geopotential (m2/s2)
       DICTIONARY(VAR_LMASK)   = 'lsm'           !Land-sea mask (0-1)
       DICTIONARY(VAR_LUSE )   = 'luse'          !
       DICTIONARY(VAR_Z0   )   = 'z0'            !
       !
       DICTIONARY(VAR_PBLH )   = 'blh'           !Boundary layer height (m)
       DICTIONARY(VAR_UST  )   = 'zust'          !Friction velocity (m/s)
       DICTIONARY(VAR_SMOI )   = 'swvl1'         !Volumetric soil water layer 1 (m3/m3)
       DICTIONARY(VAR_PREC )   = 'tp'            !Total precipitation (m)
       DICTIONARY(VAR_ACCP )   = 'accp'          !Accumulated precipitation (mm)
       DICTIONARY(VAR_U10  )   = 'u10'           !10 metre U wind component (m/s)
       DICTIONARY(VAR_V10  )   = 'v10'           !10 metre V wind component (m/s)
       DICTIONARY(VAR_T2   )   = 't2m'           !2 metre temperature (K)
       DICTIONARY(VAR_MON  )   = 'mon'           !
       DICTIONARY(VAR_PSFC )   = 'sp'            !Surface pressure (Pa)
       !
       !Nombre z usado dos veces!
       DICTIONARY(VAR_HGT  )   = 'z'             !Geopotential (m2/s2)
       DICTIONARY(VAR_P    )   = 'p'             !
       DICTIONARY(VAR_T    )   = 't'             !Temperature (K)
       DICTIONARY(VAR_TP   )   = 'ptemp'         !
       DICTIONARY(VAR_TV   )   = 'tv'            !
       DICTIONARY(VAR_U    )   = 'u'             !U component of wind (m/s)
       DICTIONARY(VAR_V    )   = 'v'             !V component of wind (m/s)
       DICTIONARY(VAR_W    )   = 'w'             !Vertical velocity (Pa/s)
       DICTIONARY(VAR_RH   )   = 'rh'            !
       DICTIONARY(VAR_QV   )   = 'q'             !Specific humidity (kg/kg)
       DICTIONARY(VAR_RHO  )   = 'rho'           !
       !
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Meteo model not implemented '
    end select
    !
    return
  end subroutine dbs_set_dictionary
  !
  !
  !-----------------------------------------
  !    subroutine dbs_read_dictionary
  !-----------------------------------------
  !
  !
  subroutine dbs_read_dictionary( file_tbl_met, MY_ERR )
    implicit none
    !
    character(len=s_file),intent(IN   ) :: file_tbl_met
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    integer(ip)           :: nword,npar
    real(rp)              :: param(nparmax)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_read_dictionary'
    MY_ERR%message = ' '
    !
    EXISTS  (:) = .true.   ! default
    COMPUTED(:) = .false.
    !
    !*** Opens the file
    !
    open(90,FILE=TRIM(file_tbl_met),STATUS='old',ERR=101)
    !
    !*** Scan file to the end
    !
    do while(0.eq.0)
       read(90,'(a512)',END=100) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(npar.eq.1) DICTIONARY(INT(param(1))) = TRIM(words(2))
    end do
    !
    !*** Successful end
    !
100 close(90)
    return
    !
    !*** List of errors
    !
101 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'error opening the input file '//TRIM(file_tbl_met)
    return
    !
  end subroutine dbs_read_dictionary
  !
  !-----------------------------------------
  !    subroutine dbs_read_metmodel_var2d
  !-----------------------------------------
  !
  subroutine dbs_read_metmodel_var2d(varID,nx,ny,xreversed,yreversed,varDATA,MY_ERR)
    implicit none

    integer(ip),        intent(IN)    :: varID,nx,ny
    logical,            intent(IN)    :: xreversed,yreversed
    real(rp),           intent(INOUT) :: varDATA(nx,ny)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR

    integer(ip)                       :: istat,ndims,ix,iy
    real(rp)                          :: FillValue, scale_factor, add_offset
    real(rp)                          :: swap
    logical                           :: use_scale_factor, use_fillvalue
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '

    istat = nf90_inquire_variable(ncID,varID,ndims=ndims)
    if(ndims.eq.2) then
       istat = nf90_get_var(ncID,varID,varDATA,start=(/1,1/),count=(/nx,ny/))
    else if(ndims.eq.3) then
       istat = nf90_get_var(ncID,varID,varDATA,start=(/1,1,1/),count=(/nx,ny,1/))
    end if

    istat = nf90_inquire_attribute(ncID, varID, "scale_factor")
    if (istat .eq. nf90_noerr) then
       use_scale_factor = .True.
       istat = NF90_GET_ATT(ncID,varID,'scale_factor',scale_factor)
       istat = NF90_GET_ATT(ncID,varID,'add_offset',add_offset)
    else
       use_scale_factor = .False.
    endif

    istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
    if (istat .eq. nf90_noerr) then
       use_fillvalue = .True.
    else
       use_fillvalue = .False.
    end if

    if(use_fillvalue .and. use_scale_factor) then
       !both fillvalue and scale present
       where(varDATA.eq.FillValue)
          varDATA = 0.0_rp
       elsewhere
          varDATA = varDATA*scale_factor + add_offset
       end where
    elseif(use_fillvalue) then
       !fillvalue without scale
       where(varDATA.eq.FillValue) varDATA=0.0_rp
    elseif(use_scale_factor) then
       !scale withour fill
       varDATA = varDATA*scale_factor + add_offset
    end if

    if (xreversed) then
       call task_wriwarn(MY_ERR,'Reverse order for longitudes is not supported')
    end if

    if (yreversed) then
       do iy=1,ny/2
          do ix=1,nx
             swap                = varDATA(ix,iy)
             varDATA(ix,iy)      = varDATA(ix,ny-iy+1)
             varDATA(ix,ny-iy+1) = swap
          end do
       end do
    end if

    return
  end subroutine dbs_read_metmodel_var2d
  !
  !
  !-----------------------------------------
  !    subroutine dbs_read_metmodel_var3d
  !-----------------------------------------
  !
  subroutine dbs_read_metmodel_var3d(varID,nx,ny,it,xreversed,yreversed,varDATA,MY_ERR)
    implicit none

    integer(ip),        intent(IN)    :: varID,nx,ny,it
    logical,            intent(IN)    :: xreversed,yreversed
    real(rp),           intent(INOUT) :: varDATA(nx,ny)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR

    integer(ip)                       :: istat,ndims,ix,iy
    real(rp)                          :: FillValue, scale_factor, add_offset
    real(rp)                          :: swap
    logical                           :: use_scale_factor, use_fillvalue
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '

    istat = nf90_inquire_variable(ncID,varID,ndims=ndims)
    if(ndims.eq.3) then
       istat = nf90_get_var(ncID,varID,varDATA,start=(/1,1,it/),count=(/nx,ny,1/))
    else if(ndims.eq.4) then
       istat = nf90_get_var(ncID,varID,varDATA,start=(/1,1,1,it/),count=(/nx,ny,1,1/))
    end if

    istat = nf90_inquire_attribute(ncID, varID, "scale_factor")
    if (istat .eq. nf90_noerr) then
       use_scale_factor = .True.
       istat = NF90_GET_ATT(ncID,varID,'scale_factor',scale_factor)
       istat = NF90_GET_ATT(ncID,varID,'add_offset',add_offset)
    else
       use_scale_factor = .False.
    endif

    istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
    if (istat .eq. nf90_noerr) then
       use_fillvalue = .True.
    else
       use_fillvalue = .False.
    end if

    if(use_fillvalue .and. use_scale_factor) then
       !both fillvalue and scale present
       where(varDATA.eq.FillValue)
          varDATA = 0.0_rp
       elsewhere
          varDATA = varDATA*scale_factor + add_offset
       end where
    elseif(use_fillvalue) then
       !fillvalue without scale
       where(varDATA.eq.FillValue) varDATA=0.0_rp
    elseif(use_scale_factor) then
       !scale withour fill
       varDATA = varDATA*scale_factor + add_offset
    end if

    if (xreversed) then
       call task_wriwarn(MY_ERR,'Reverse order for longitudes is not supported')
    end if

    if (yreversed) then
       do iy=1,ny/2
          do ix=1,nx
             swap                = varDATA(ix,iy)
             varDATA(ix,iy)      = varDATA(ix,ny-iy+1)
             varDATA(ix,ny-iy+1) = swap
          end do
       end do
    end if

    return
  end subroutine dbs_read_metmodel_var3d
  !
  !
  !-----------------------------------------
  !    subroutine dbs_read_metmodel_var4d
  !-----------------------------------------
  !
  subroutine dbs_read_metmodel_var4d(varID,nx,ny,nz,it,xreversed,yreversed,zreversed,varDATA,MY_ERR)
    implicit none

    integer(ip),        intent(IN)    :: varID,nx,ny,nz,it
    logical,            intent(IN)    :: xreversed,yreversed,zreversed
    real(rp),           intent(INOUT) :: varDATA(nx,ny,nz)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR

    integer(ip)                       :: istat,ndims,ix,iy,iz
    real(rp)                          :: FillValue, scale_factor, add_offset
    real(rp)                          :: swap
    logical                           :: use_scale_factor, use_fillvalue
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '

    istat = nf90_inquire_variable(ncID,varID,ndims=ndims)
    if(ndims.eq.4) then
       istat = nf90_get_var(ncID,varID,varDATA,start=(/1,1,1,it/),count=(/nx,ny,nz,1/))
    else
       call task_wriwarn(MY_ERR,'Dimension number should be 4')
    end if

    istat = nf90_inquire_attribute(ncID, varID, "scale_factor")
    if (istat .eq. nf90_noerr) then
       use_scale_factor = .True.
       istat = NF90_GET_ATT(ncID,varID,'scale_factor',scale_factor)
       istat = NF90_GET_ATT(ncID,varID,'add_offset',add_offset)
    else
       use_scale_factor = .False.
    endif

    istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
    if (istat .eq. nf90_noerr) then
       use_fillvalue = .True.
    else
       use_fillvalue = .False.
    end if

    if(use_fillvalue .and. use_scale_factor) then
       !both fillvalue and scale present
       where(varDATA.eq.FillValue)
          varDATA = 0.0_rp
       elsewhere
          varDATA = varDATA*scale_factor + add_offset
       end where
    elseif(use_fillvalue) then
       !fillvalue without scale
       where(varDATA.eq.FillValue) varDATA=0.0_rp
    elseif(use_scale_factor) then
       !scale withour fill
       varDATA = varDATA*scale_factor + add_offset
    end if

    if (xreversed) then
       call task_wriwarn(MY_ERR,'Reverse order for longitudes is not supported')
    end if

    if (yreversed) then
       do iz=1,nz
          do iy=1,ny/2
             do ix=1,nx
                swap                   = varDATA(ix,iy,iz)
                varDATA(ix,iy,iz)      = varDATA(ix,ny-iy+1,iz)
                varDATA(ix,ny-iy+1,iz) = swap
             end do
          end do
       end do
    end if

    if (zreversed) then
       do iz=1,nz/2
          do iy=1,ny
             do ix=1,nx
                swap                   = varDATA(ix,iy,iz)
                varDATA(ix,iy,iz)      = varDATA(ix,iy,nz-iz+1)
                varDATA(ix,iy,nz-iz+1) = swap
             end do
          end do
       end do
    end if

    return
  end subroutine dbs_read_metmodel_var4d
  !
  !
  !-----------------------------------------
  !    subroutine dbs_get_geopotential_hyb
  !-----------------------------------------
  !      Coefficients defining the model
  !      levels are read from an external
  !      file:
  !
  subroutine dbs_get_geopotential_hyb(nx,ny,nz,a,b,zsfc,psfc,q,t,z,MY_ERR)
    implicit none

    integer(ip),        intent(IN)    :: nx,ny,nz
    real(rp),           intent(IN)    :: a(0:nz)
    real(rp),           intent(IN)    :: b(0:nz)
    real(rp),           intent(IN)    :: zsfc(nx,ny)
    real(rp),           intent(IN)    :: psfc(nx,ny)
    real(rp),           intent(IN)    :: q(nx,ny,nz)
    real(rp),           intent(IN)    :: t(nx,ny,nz)
    real(rp),           intent(INOUT) :: z(nx,ny,nz)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    real(rp), allocatable :: p1(:,:),p2(:,:) !Pressure at half level
    real(rp), allocatable :: zprev(:,:)      !Record previous height
    integer(ip)           :: iz
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%message = ' '
    !
    allocate(p1(nx,ny))
    allocate(p2(nx,ny))
    allocate(zprev(nx,ny))
    !
    zprev = zsfc*9.81_rp ! Convert from height to geopotential
    do iz=1,nz
       p2 = 0.5_rp*(a(iz-1)+a(iz)) + psfc * 0.5_rp*(b(iz-1)+b(iz))
       if(iz.eq.1) then
          p1 = psfc
          z(:,:,iz) = zprev + 287.05_rp * t(:,:,iz) * (1.0_rp+0.61_rp*q(:,:,iz))*log(p1/p2)
       else
          p1 = 0.5_rp*(a(iz-1)+a(iz-2)) + psfc * 0.5_rp*(b(iz-1)+b(iz-2))
          z(:,:,iz) = zprev +                                             &
               287.05_rp * 0.5_rp*(t(:,:,iz)+t(:,:,iz-1)) *        &
               (1.0_rp + 0.61_rp*0.5_rp*(q(:,:,iz)+q(:,:,iz-1))) * &
               log(p1/p2)
       end if
       zprev = z(:,:,iz)
    end do
    !
    deallocate(p1)
    deallocate(p2)
    deallocate(zprev)
    !
    return
  end subroutine dbs_get_geopotential_hyb
  !
  !-----------------------------------------
  !    subroutine dbs_get_map_projection_info
  !-----------------------------------------
  !
  subroutine dbs_get_map_projection_info(cone,         &
       cen_long,     &
       rotate_winds, &
       MY_ERR)
    implicit none
    !tmp_wind
    real(rp),           intent(INOUT) :: cone
    real(rp),           intent(INOUT) :: cen_long
    logical,            intent(INOUT) :: rotate_winds
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip)                :: map_projection      !Map projection type
    real(rp)                   :: true_lat1,true_lat2 !Map projection parameters
    real(rp)                   :: rpd                 !Radians per degree
    integer(ip)                :: istat               !NetCDF status
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dbs_get_map_projection_info'
    MY_ERR%message = ' '
    !
    !Default values
    cone=1.0_rp
    cen_long=0.0_rp
    rotate_winds=.false.
    !
    rpd = PI/180.0_rp !radians per degree
    !
    !Get global attributes
    istat = nf90_get_att(ncID,NF90_GLOBAL,'MAP_PROJ',map_projection)
    if (istat .ne. nf90_noerr) then
       call task_wriwarn(MY_ERR,'Global attribute MAP_PROJ not found. No wind rotation will be applied')
       map_projection = 0
    end if
    !
    !Decide if wind rotation will be applied
    select case(map_projection)
    case(0,3,6)
       rotate_winds = .false.
    case(1,2)
       rotate_winds = .true.
       !
       !Get global attribute for cen_long
       istat = nf90_get_att(ncID,NF90_GLOBAL,'STAND_LON',cen_long)
       if (istat .ne. nf90_noerr) then
          istat = nf90_get_att(ncID,NF90_GLOBAL,'CEN_LON',cen_long)
          if (istat .ne. nf90_noerr) then
             call task_wriwarn(MY_ERR,'Global attributes STAND_LON and CEN_LON not found. No wind rotation will be applied')
             rotate_winds = .false.
          end if
       end if
       !
       if(map_projection.eq.1) then
          !Lambert Conformal
          !
          !Get global attribute for truelat1 and truelat2
          istat = nf90_get_att(ncID,NF90_GLOBAL,'TRUELAT1',true_lat1)
          if (istat .ne. nf90_noerr) then
             call task_wriwarn(MY_ERR,'Global attribute TRUELAT1 not found. No wind rotation will be applied')
             rotate_winds = .false.
          end if
          !
          istat = nf90_get_att(ncID,NF90_GLOBAL,'TRUELAT2',true_lat2)
          if (istat .ne. nf90_noerr) then
             call task_wriwarn(MY_ERR,'Global attribute TRUELAT2 not found. No wind rotation will be applied')
             rotate_winds = .false.
          end if
       end if
    end select
    !
    !Compute cone for Lambert projection
    if(map_projection.eq.1 .and. rotate_winds) then
       if( 90.0_rp - abs(true_lat1) .lt. 0.1_rp ) then
          call task_wriwarn(MY_ERR,'Check true_lat1. No wind rotation will be applied')
          rotate_winds = .false.
       elseif( 90.0_rp - abs(true_lat2) .lt. 0.1_rp ) then
          call task_wriwarn(MY_ERR,'Check true_lat2. No wind rotation will be applied')
          rotate_winds = .false.
       elseif( abs(true_lat1-true_lat2) .gt. 0.1_rp ) then
          cone = log(cos(true_lat1*rpd)) - log(cos(true_lat2*rpd))
          cone = cone/( log(tan((45.0_rp - abs(0.5_rp*true_lat1))*rpd)) - &
               log(tan((45.0_rp - abs(0.5_rp*true_lat2))*rpd)) )
       else
          cone = sin(abs(true_lat1)*rpd)
       end if
    end if
    !
    cone = cone*rpd
    !
  end subroutine dbs_get_map_projection_info
  !
  !-------------------------------------------------------
  !    subroutine dbs_get_zo_from_24ldu
  !
  !    Convert from 24 USGS land use categories to surface
  !    roughness length assuming the following equivalence
  !
  !               USGS                                    zo
  !    ------------------------------------------------------------
  !    1 'Urban and Built-Up Land'                         0.80
  !    2 'Dryland Cropland and Pasture'                    0.15
  !    3 'Irrigated Cropland and Pasture'                  0.10
  !    4 'Mixed Dryland/Irrigated Cropland and Pasture'    0.15
  !    5 'Cropland/Grassland Mosaic'                       0.14
  !    6 'Cropland/Woodland Mosaic'                        0.20
  !    7 'Grassland'                                       0.12
  !    8 'Shrubland'                                       0.05
  !    9 'Mixed Shrubland/Grassland'                       0.06
  !   10 'Savanna'                                         0.15
  !   11 'Deciduous Broadleaf Forest'                      0.50
  !   12 'Deciduous Needleleaf Forest'                     0.50
  !   13 'Evergreen Broadleaf Forest'                      0.50
  !   14 'Evergreen Needleleaf Forest'                     0.50
  !   15 'Mixed Forest'                                    0.50
  !   16 'Water Bodies'                                    0.0001
  !   17 'Herbaceous Wetland'                              0.20
  !   18 'Wooded Wetland'                                  0.40
  !   19 'Barren or Sparsely Vegetated'                    0.01
  !   20 'Herbaceous Tundra'                               0.10
  !   21 'Wooded Tundra'                                   0.30
  !   22 'Mixed Tundra'                                    0.15
  !   23 'Bare Ground Tundra'                              0.10
  !   24 'Snow or Ice'                                     0.001
  !
  !-----------------------------------------
  !
  subroutine dbs_get_zo_from_24ldu(ldu,zo)
    implicit none
    !
    real(rp), intent(IN   ) :: ldu
    real(rp), intent(INOUT) :: zo
    !
    if(ldu.le.1) then
       zo = 0.8_rp
    else if(ldu.le.2) then
       zo = 0.15_rp
    else if(ldu.le.3) then
       zo = 0.10_rp
    else if(ldu.le.4) then
       zo = 0.15_rp
    else if(ldu.le.5) then
       zo = 0.14_rp
    else if(ldu.le.6) then
       zo = 0.20_rp
    else if(ldu.le.7) then
       zo = 0.12_rp
    else if(ldu.le.8) then
       zo = 0.05_rp
    else if(ldu.le.9) then
       zo = 0.06_rp
    else if(ldu.le.10) then
       zo = 0.15_rp
    else if(ldu.le.11) then
       zo = 0.50_rp
    else if(ldu.le.12) then
       zo = 0.50_rp
    else if(ldu.le.13) then
       zo = 0.50_rp
    else if(ldu.le.14) then
       zo = 0.50_rp
    else if(ldu.le.15) then
       zo = 0.50_rp
    else if(ldu.le.16) then
       zo = 0.0001_rp
    else if(ldu.le.17) then
       zo = 0.20_rp
    else if(ldu.le.18) then
       zo = 0.40_rp
    else if(ldu.le.19) then
       zo = 0.01_rp
    else if(ldu.le.20) then
       zo = 0.10_rp
    else if(ldu.le.21) then
       zo = 0.30_rp
    else if(ldu.le.22) then
       zo = 0.15_rp
    else if(ldu.le.23) then
       zo = 0.10_rp
    else if(ldu.le.24) then
       zo = 0.001_rp
    else
       zo = 0.10_rp  ! default
    end if
    !
    return
  end subroutine dbs_get_zo_from_24ldu
  !
  !
  !
END MODULE Dbs
