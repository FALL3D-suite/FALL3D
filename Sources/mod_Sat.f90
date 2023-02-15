!***************************************************************
!>
!> Module for procedures related to Satelite retrievals I/O
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Sat
  use KindType
  use InpOut
  use Parallel
  use Domain
  use netcdf
  use Time
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: sat_set_initial_condition
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: sat_read_inp_sat
  PRIVATE :: sat_bcast_inp_sat
  PRIVATE :: sat_read_data
  PRIVATE :: sat_filter_data
  PRIVATE :: sat_bcast_sat_pts_data
  PRIVATE :: sat_set_dictionary
  PRIVATE :: sat_read_dictionary
  !
  !    LIST OF PRIVATE VARIABLES
  !
  integer(ip), PRIVATE :: ncID
  integer(ip), PRIVATE :: dimID
  integer(ip), PRIVATE :: varID
  !
  !    DICTIONARY DEFINITION
  !
  integer(ip), PRIVATE, parameter :: DIM_LON    = 1
  integer(ip), PRIVATE, parameter :: DIM_LAT    = 2
  integer(ip), PRIVATE, parameter :: DIM_TIME   = 3
  !
  integer(ip), PRIVATE, parameter :: VAR_LON   = 4
  integer(ip), PRIVATE, parameter :: VAR_LAT   = 5
  integer(ip), PRIVATE, parameter :: VAR_TIME  = 6
  integer(ip), PRIVATE, parameter :: VAR_MASS  = 7
  integer(ip), PRIVATE, parameter :: VAR_HTOP  = 8
  integer(ip), PRIVATE, parameter :: VAR_THICK = 9
  !
  integer(ip), PRIVATE, parameter :: ATR_T0     = 10
  integer(ip), PRIVATE, parameter :: ATR_SENSOR = 11
  integer(ip), PRIVATE, parameter :: ATR_PLATFO = 12
  integer(ip), PRIVATE, parameter :: ATR_RESOL  = 13
  integer(ip), PRIVATE, parameter :: ATR_TRACER = 14
  !
  logical,               PRIVATE  :: EXISTS    (20)
  character(len=s_name), PRIVATE  :: DICTIONARY(20)
  !
  integer(ip), parameter, private :: s_long  = 512    !  Generic long string lenght. Use '(a512)' to read
  integer(ip), parameter, private :: nwormax = 128
  integer(ip), parameter, private :: nparmax = 128
  !
  !    type SAT_DATA
  !
  type SAT_DATA
     !
     logical           :: read_all_slabs   !< flag on data read
     !
     character(s_name) :: sensor           !< type of data sensor
     character(s_name) :: platform         !< type of data platform
     character(s_name) :: resolution       !< sat data resolution
     character(s_name) :: tracer_type      !< type of inserted data
     !
     integer(ip) :: islab                  !< time slab read
     !
     integer(ip) :: nx                     !< number x points
     integer(ip) :: ny                     !< number y points
     integer(ip) :: nt                     !< number time slabs read
     integer(ip) :: nt_file                !< number time slabs in file
     integer(ip) :: start_year             !< start year
     integer(ip) :: start_month            !< start month
     integer(ip) :: start_day              !< start day
     integer(ip) :: start_hour             !< start hour
     integer(ip) :: start_min              !< start minute
     !
     real(rp)    :: fill_value               !< fill value for NaN data
     real(rp)    :: time_lag                 !< difference in data and model time origins
     real(rp)    :: d_cut_off                !< cut-off size
     real(rp), allocatable :: lon    (:,:)   !< x-coordinates
     real(rp), allocatable :: lat    (:,:)   !< y-coordinates of source point
     real(rp), allocatable :: time   (:)     !< time   (nt_file) slabs in format YYYYMMDDHHMMSS
     real(rp), allocatable :: timesec(:)     !< timesec(nt_file) slabs in sec after T0
     real(rp), allocatable :: mass   (:,:,:) !< total column mass loading
     real(rp), allocatable :: htop   (:,:,:) !< cloud-top height
     real(rp), allocatable :: thick  (:,:,:) !< cloud thickness
     !
  end type SAT_DATA
  !
  !    type SAT_DATA_POINTS
  !
  type SAT_DATA_POINTS
     !
     integer(ip) :: np                     !< number of points
     real(rp)    :: time                   !< time in format YYYYMMDDHHMMS
     real(rp)    :: timesec                !< time in sec after 0000UTC
     !
     real(rp), allocatable :: lon  (:)   !< cloud points x-coordinates
     real(rp), allocatable :: lat  (:)   !< cloud points y-coordinates of source point
     real(rp), allocatable :: mass (:)   !< cloud points total column mass loading
     real(rp), allocatable :: htop (:)   !< cloud points cloud-top height
     real(rp), allocatable :: thick(:)   !< cloud points cloud thickness
     real(rp), allocatable :: area (:)   !< cloud points associated area
     !
  end type SAT_DATA_POINTS
  !
  !
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine sat_set_initial_condition
  !-----------------------------------------
  !
  !>   @brief
  !>   Sets initial condition from a satelite rietrival
  !
  subroutine sat_set_initial_condition(MY_FILES,MY_TIME,MY_GRID,MY_TRA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(RUN_TIME),      intent(INOUT) :: MY_TIME
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    type(SAT_DATA)        :: GL_SAT
    type(SAT_DATA_POINTS) :: GL_SAT_PTS
    !
    logical     :: found,foundx,foundy,foundz
    integer(ip) :: is,ix,iy,iz,ibin
    real(rp)    :: xs,ys,zmin,zmax,z1,z2,mass,vol,cmean,fmass
    real(rp)    :: lonmin,lonmax,latmin,latmax
    !
    logical,  allocatable :: my_cell_count(:,:,:)
    real(rp), allocatable :: my_2dmass    (:,:)
    real(rp), allocatable :: my_2dthik    (:,:)
    real(rp), allocatable :: mass_local   (:)
    real(rp), allocatable :: fc           (:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_set_initial_condition'
    MY_ERR%message = ' '
    !
    !*** Master reads and broadcasts input file block
    !
    if(master) call sat_read_inp_sat(MY_FILES,GL_SAT,MY_ERR)
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    call sat_bcast_inp_sat(MY_FILES,GL_SAT, MY_ERR)
    !
    !*** Master reads satelite data (one time slab only)
    !
    if(master) call sat_read_data(MY_FILES,GL_SAT,MY_ERR,GL_SAT%islab)
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    !*** Master filters and broadcasts data (filter points with non-zero column mass) and checks
    !*** consistency between satelite and input data in time
    !
    if(master) call sat_filter_data(MY_FILES,MY_TIME,GL_SAT,GL_SAT_PTS,MY_ERR)
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    call sat_bcast_sat_pts_data(GL_SAT_PTS, MY_ERR)
    !
    !*** Interpolates data. Concentration is imposed conserving the total mass
    !
    allocate(my_cell_count(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe))
    allocate(my_2dmass    (my_ips:my_ipe,my_jps:my_jpe))
    allocate(my_2dthik    (my_ips:my_ipe,my_jps:my_jpe))
    my_cell_count(:,:,:) = .false.
    my_2dmass    (:,:)   = 0.0_rp
    my_2dthik    (:,:)   = 0.0_rp
    !
    lonmin = MY_GRID%lon_c(my_ibs)
    lonmax = MY_GRID%lon_c(my_ibe)
    latmin = MY_GRID%lat_c(my_jbs)
    latmax = MY_GRID%lat_c(my_jbe)
    !
    do is = 1,GL_SAT_PTS%np  ! loop over potential source points
       !
       xs = GL_SAT_PTS%lon(is)
       ys = GL_SAT_PTS%lat(is)
       !
       if((xs.ge.lonmin).and.(xs.le.lonmax).and. &
            (ys.ge.latmin).and.(ys.le.latmax)) then
          !
          ! I am a candidate point
          !
          found  = .false.
          foundx = .false.
          ix     = my_ibs
          do while(.not.found)
             if((xs.ge.MY_GRID%lon_c(ix)).and.(xs.lt.MY_GRID%lon_c(ix+1))) then
                found  = .true.
                foundx = .true.
             else
                ix = ix + 1
                if(ix.eq.my_ibe) found=.true.
             end if
          end do
          !
          found  = .false.
          foundy = .false.
          iy     = my_jbs
          do while(.not.found)
             if((ys.ge.MY_GRID%lat_c(iy)).and.(ys.lt.MY_GRID%lat_c(iy+1))) then
                found  = .true.
                foundy = .true.
             else
                iy = iy + 1
                if(iy.eq.my_jbe) found=.true.
             end if
          end do
          !
          if(foundx.and.foundy) then
             !
             foundz = .false.
             zmax   = GL_SAT_PTS%htop(is)
             zmin   = GL_SAT_PTS%htop(is)-GL_SAT_PTS%thick(is)
             !
             do iz = my_kbs,my_kbe-1
                z1 = MY_GRID%z_c(ix,iy,iz  )
                z2 = MY_GRID%z_c(ix,iy,iz+1)
                if( ((zmin.ge.z1).and.(zmin.le.z2)).or. &
                     ((zmax.ge.z1).and.(zmax.le.z2)) ) then
                   foundz = .true.
                   if(.not.my_cell_count(ix,iy,iz)) then
                      my_cell_count(ix,iy,iz) = .true.      ! one cell can host many points
                      my_2dthik(ix,iy) = my_2dthik(ix,iy) + (z2-z1)
                   end if
                end if
             end do
             !
             if(foundz) my_2dmass(ix,iy) = my_2dmass(ix,iy) + GL_SAT_PTS%mass(is)*GL_SAT_PTS%area(is)  ! mass in my column
          end if
          !
       end if
    end do  ! is = 1,GL_SAT_PTS%np
    !
    !*** Check spatial consistency of points (i.e. mass exists in the domain)
    !
    allocate(mass_local(0:nproc-1))
    mass_local(:)     = 0.0_rp
    mass_local(mpime) = sum(my_2dmass)
    call parallel_sum(mass_local, COMM_WORLD)
    mass = sum(mass_local)
    if(mass.le.0.0_rp) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'No mass found in the domain for insertion. Check domain limits'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    !*** Get thickness across z-processors
    !
    call parallel_sum(my_2dthik,COMM_GRIDZ)  ! only along z
    !
    !*** Determine the affected bins and relative mass fraction
    !
    select case(GL_SAT%tracer_type)
    case('TEPHRA')
       !
       allocate(fc(MY_TRA%nbins))
       fc    = 0.0_rp
       fmass = 0.0_rp
       do ibin = 1,MY_TRA%nbins
          if(MY_TRA%MY_BIN%bin_type(ibin).eq.'tephra'.and. &
               MY_TRA%MY_BIN%bin_diam(ibin).le.GL_SAT%d_cut_off) then
             fc(ibin) = MY_TRA%MY_BIN%bin_fc(ibin)
             fmass = fmass + fc(ibin)
          end if
       end do
       fc(:) = fc(:)/fmass
       !
    case('SO2')
       !
       allocate(fc(MY_TRA%nbins))
       fc    = 0.0_rp
       do ibin = 1,MY_TRA%nbins
          if(MY_TRA%MY_BIN%bin_type(ibin).eq.'SO2') then
             fc(ibin) = 1.0_rp
          end if
       end do
    case default
       !
       MY_ERR%flag    = 1
       MY_ERR%message = 'Insertion data not available for this tracer: '//TRIM(GL_SAT%tracer_type)
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
       !
    end select
    !
    !*** Set the (averaged) concentration. Note that concentration is already scaled (i.e. no
    !*** need to first divide and then multiply by map factors)
    !
    MY_TRA%my_c   (:,:,:,:) = 0.0_rp
    !
    do iy = my_jps,my_jpe
       do ix = my_ips,my_ipe
          if(my_2dmass(ix,iy).gt.0.0_rp) then
             vol   = MY_GRID%dX1_p(ix)*MY_GRID%dX2_p(iy)*my_2dthik(ix,iy)
             cmean = my_2dmass(ix,iy)/vol   ! already scaled
             !
             do iz = my_kps,my_kpe
                if(my_cell_count(ix,iy,iz)) then
                   do ibin = 1,MY_TRA%nbins
                      MY_TRA%my_c(ix,iy,iz,ibin) = fc(ibin)*cmean
                   end do
                end if
             end do
          end if
       end do
    end do
    !
    !*** Exchange halos
    !
    do ibin = 1,MY_TRA%nbins
       call domain_swap_mass_points_2halo_x ( MY_TRA%my_c(:,:,:,ibin) )
       call domain_swap_mass_points_2halo_y ( MY_TRA%my_c(:,:,:,ibin) )
       call domain_swap_mass_points_2halo_z ( MY_TRA%my_c(:,:,:,ibin) )
    end do
    !
    !*** Finally, set the run start time
    !
    MY_TIME%run_start = GL_SAT_PTS%timesec
    MY_TRA%gl_mass_in = mass
    !
    return
  end subroutine sat_set_initial_condition
  !
  !
  !    PRIVATE ROUTINES
  !
  !-----------------------------------------
  !    subroutine sat_read_inp_sat
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads the satelite data block form the input file
  !
  subroutine sat_read_inp_sat(MY_FILES,GL_SAT,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param GL_SAT    variables related to sat data
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(SAT_DATA),    intent(INOUT) :: GL_SAT
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    real(rp)              :: file_version
    character(len=s_file) :: file_inp
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_read_inp_sat'
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
    !*** Reads INSERTION_DATA block
    !
    call inpout_get_cha (file_inp, 'INSERTION_DATA','INSERTION_FILE',MY_FILES%file_sat, 1, MY_ERR, .false.)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Insertion data file not found in input file'
       return
    end if
    !
    call inpout_get_cha (file_inp, 'INSERTION_DATA','INSERTION_DICTIONARY_FILE',MY_FILES%file_tbl_sat, 1, MY_ERR, .false.)
    if(MY_ERR%flag.ne.0) then
       MY_FILES%file_tbl_sat = '-'
       MY_ERR%flag = 0
    end if
    !
    call inpout_get_int (file_inp, 'INSERTION_DATA','INSERTION_TIME_SLAB',GL_SAT%islab, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       GL_SAT%islab = 1
       MY_ERR%flag  = 0
    end if
    !
    call inpout_get_cha (file_inp, 'INSERTION_DATA','INSERTED_TRACER',GL_SAT%tracer_type, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) then
       MY_ERR%message = 'Type of inserted tracer not specifyed'
       return
    end if
    !
    call inpout_get_rea (file_inp, 'INSERTION_DATA','DIAMETER_CUT_OFF_(MIC)',GL_SAT%d_cut_off, 1, MY_ERR)
    if(MY_ERR%flag.eq.0) then
       GL_SAT%d_cut_off = 1e-6_rp*GL_SAT%d_cut_off  ! mic --> m
    else
       GL_SAT%d_cut_off = 1d9 ! no cut off
    end if
    !
    return
  end subroutine sat_read_inp_sat
  !
  !-----------------------------------------
  !    subroutine sat_bcast_inp_sat
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts the cloud of points structure
  !
  subroutine sat_bcast_inp_sat(MY_FILES,GL_SAT, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param GL_SAT    variables related to sat data
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(SAT_DATA),    intent(INOUT) :: GL_SAT
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_bcast_inp_sat'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_FILES%file_sat     ,1,0)
    call parallel_bcast(MY_FILES%file_tbl_sat ,1,0)
    !
    call parallel_bcast(GL_SAT%islab          ,1,0)
    call parallel_bcast(GL_SAT%tracer_type    ,1,0)
    call parallel_bcast(GL_SAT%d_cut_off      ,1,0)
    !
    return
  end subroutine sat_bcast_inp_sat
  !
  !---------------------------------
  !    subroutine sat_read_data
  !---------------------------------
  !
  !>   @brief
  !>   Master reads a satellite data file
  !
  subroutine sat_read_data(MY_FILES,GL_SAT,MY_ERR,islab)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_ERR        error handler
    !>   @param islab         (optional). Time slab to read. If not given, all slabs are read
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(SAT_DATA),        intent(INOUT) :: GL_SAT
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    integer(ip), optional ,intent(IN   ) :: islab
    !
    integer(ip)           :: istat,it,lulog
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    character(len=s_name) :: str
    character(len=24)     :: time_str
    real(rp)              :: FillValue
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_read_data'
    MY_ERR%message = ' '
    !
    if(present(islab)) then
       GL_SAT%read_all_slabs = .false.
       GL_SAT%islab = islab
    else
       GL_SAT%read_all_slabs = .true.
       GL_SAT%islab = 1
    end if
    !
    !*** Define dictionary depending on each model
    !
    if(MY_FILES%file_tbl_sat.eq.'-') then
       call sat_set_dictionary(MY_ERR)
       if(MY_ERR%flag.ne.0) return
    else
       call sat_read_dictionary(MY_FILES%file_tbl_sat, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    end if
    !
    !*** Open netCDF file and get ncID
    !
    istat = nf90_open(TRIM(MY_FILES%file_sat),NF90_NOWRITE, ncID)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = 'Unable to open '//TRIM(MY_FILES%file_sat)
       return
    end if
    !
    !*** Read dimensions
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LON),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_SAT%nx)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LAT),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_SAT%ny)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_TIME),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_SAT%nt_file)
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    if(GL_SAT%read_all_slabs) then
       GL_SAT%nt = GL_SAT%nt_file
    else
       GL_SAT%nt = 1
    end if
    !
    !*** Allocates
    !
    allocate(GL_SAT%time   (GL_SAT%nt_file))
    allocate(GL_SAT%timesec(GL_SAT%nt_file))
    !
    allocate(GL_SAT%lon    (GL_SAT%nx,GL_SAT%ny))
    allocate(GL_SAT%lat    (GL_SAT%nx,GL_SAT%ny))
    allocate(GL_SAT%mass   (GL_SAT%nx,GL_SAT%ny,GL_SAT%nt))
    allocate(GL_SAT%htop   (GL_SAT%nx,GL_SAT%ny,GL_SAT%nt))
    allocate(GL_SAT%thick  (GL_SAT%nx,GL_SAT%ny,GL_SAT%nt))
    !
    !*** Get timesec for all slabs in the file (referred to T0)
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_TIME),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT%timesec,start=(/1/),count=(/GL_SAT%nt_file/))
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_T0), str)  ! "YYYY-MM-DD HH:MM UTC"
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    GL_SAT%start_year  = stoi1(str(1 :1 ))*1000 + &
         stoi1(str(2 :2 ))*100  + &
         stoi1(str(3 :3 ))*10   + &
         stoi1(str(4 :4 ))
    GL_SAT%start_month = stoi1(str(6 :6 ))*10 + &
         stoi1(str(7 :7 ))
    GL_SAT%start_day   = stoi1(str(9 :9 ))*10 + &
         stoi1(str(10:10))
    GL_SAT%start_hour  = stoi1(str(12:12))*10 + &
         stoi1(str(13:13))
    GL_SAT%start_min   = stoi1(str(15:15))*10 + &
         stoi1(str(16:16))
    !
    !*** Compute time in format YYYYMMDDHHMMSS
    !
    GL_SAT%time(1) = 1e10_rp*GL_SAT%start_year    + &
         1e8_rp *GL_SAT%start_month   + &
         1e6_rp *GL_SAT%start_day     + &
         1e4_rp *GL_SAT%start_hour    + &
         1e2_rp *GL_SAT%start_min

    do it = 2,GL_SAT%nt_file
       call time_addtime(GL_SAT%start_year,GL_SAT%start_month, GL_SAT%start_day,GL_SAT%start_hour,  &
            iyr,imo,idy,ihr,imi,ise,GL_SAT%timesec(it),MY_ERR)
       GL_SAT%time(it) = 1e10_rp*iyr + 1e8_rp*imo + 1e6_rp*idy + 1e4_rp*ihr + 1e2_rp*imi + ise
    end do
    !
    !*** Read other variables
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LON),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT%lon,start=(/1,1/),count=(/GL_SAT%nx,GL_SAT%ny/))
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LAT),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT%lat,start=(/1,1/),count=(/GL_SAT%nx,GL_SAT%ny/))
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
    if (istat .eq. nf90_noerr) then
       GL_SAT%fill_value = FillValue
    else
       GL_SAT%fill_value = NF90_FILL_REAL
    end if
    !
    !   Mass loading
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_MASS),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT%mass,start=(/1,1,GL_SAT%islab/),count=(/GL_SAT%nx,GL_SAT%ny,GL_SAT%nt/))
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
    if (istat .ne. nf90_noerr) FillValue = GL_SAT%fill_value
    select case(GL_SAT%tracer_type)
    case('SO2')
       where(GL_SAT%mass.ne.FillValue)
           GL_SAT%mass = GL_SAT%mass * 64.0_rp / 2.238e3_rp / 1e3_rp ! DU --> g/m2 --> kg/m2.  Molecular mass SO2 = 64 gr/mol; Avogadro/1DU = 2.238e3_rp
       elsewhere
           GL_SAT%mass = GL_SAT%fill_value
       endwhere
    case default
       where(GL_SAT%mass.ne.FillValue)
           GL_SAT%mass = GL_SAT%mass * 1e-3_rp                       ! g/m2 --> kg/m2
       elsewhere
           GL_SAT%mass = GL_SAT%fill_value
       endwhere
    end select
    !
    !  Top height
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_HTOP),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT%htop,start=(/1,1,GL_SAT%islab/),count=(/GL_SAT%nx,GL_SAT%ny,GL_SAT%nt/))
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
    if (istat .ne. nf90_noerr) FillValue = GL_SAT%fill_value
    where(GL_SAT%htop.ne.FillValue)
        GL_SAT%htop = GL_SAT%htop * 1e3_rp  ! km --> m
    elsewhere
        GL_SAT%htop = GL_SAT%fill_value
    endwhere
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_THICK),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT%thick,start=(/1,1,GL_SAT%islab/),count=(/GL_SAT%nx,GL_SAT%ny,GL_SAT%nt/))
    if(istat.ne.0) then
       MY_ERR%flag    = istat
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
    if (istat .ne. nf90_noerr) FillValue = GL_SAT%fill_value
    where(GL_SAT%thick.ne.FillValue)
        GL_SAT%thick = GL_SAT%thick * 1e3_rp  ! km --> m
    elsewhere
        GL_SAT%thick = GL_SAT%fill_value
    endwhere
    !
    !*** Read other global attributes
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_SENSOR), GL_SAT%sensor)
    if(istat.ne.0) GL_SAT%sensor ='N/A'
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_PLATFO), GL_SAT%platform)
    if(istat.ne.0) GL_SAT%platform ='N/A'
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_RESOL), GL_SAT%resolution)
    if(istat.ne.0) GL_SAT%resolution ='N/A'
    !
    !*** Print to log file
    !
    lulog = MY_FILES%lulog
    !
    write(lulog,10)
10  format(&
         '----------------------------------------------------',/,   &
         '                                                    ',/,   &
         '           SAT DATA (INITIAL CONDITION)             ',/,   &
         '                                                    ',/,   &
         '----------------------------------------------------')
    !
    call time_addtime(GL_SAT%start_year,GL_SAT%start_month, GL_SAT%start_day, GL_SAT%start_hour,  &
         iyr,imo,idy,ihr,imi,ise,GL_SAT%timesec(1),MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,20) TRIM(time_str)
20  format(/,&
         'TIME RANGE OF SATELITE DATA',/, &
         '  Initial time       : ',a)
    !
    call time_addtime(GL_SAT%start_year,GL_SAT%start_month, GL_SAT%start_day, GL_SAT%start_hour,  &
         iyr,imo,idy,ihr,imi,ise,GL_SAT%timesec(GL_SAT%nt_file),MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,21) TRIM(time_str)
21  format('  Final   time       : ',a)
    !
    write(lulog,31) TRIM(GL_SAT%sensor), TRIM(GL_SAT%platform), &
         TRIM(GL_SAT%resolution), TRIM(GL_SAT%tracer_type), &
         minval(GL_SAT%lon),  minval(GL_SAT%lat),  &
         maxval(GL_SAT%lon),  maxval(GL_SAT%lat),  &
         GL_SAT%nx, GL_SAT%ny, GL_SAT%nt
31  format(/, &
         'TYPE AND COVERAGE OF SATELITE DATA',/, &
         '  Sensor             :  ',a               ,/,      &
         '  Platform           :  ',a               ,/,      &
         '  Resolution         :  ',a               ,/,      &
         '  Tracer type        :  ',a               ,/,      &
         '  Bottom-left corner : (',f9.4,2x,f9.4,')',/,      &
         '  Top-right   corner : (',f9.4,2x,f9.4,')',/,      &
         '  Number points x    : ',i9  ,/,                   &
         '  Number points y    : ',i9  ,/,                   &
         '  Number time slabs  : ',i9)
    !
    return
  end subroutine sat_read_data
  !
  !---------------------------------
  !    subroutine sat_filter_data
  !---------------------------------
  !
  !>   @brief
  !>   Filters data and sets the cloud of points structure.
  !>   @details
  !>   It also checks consistency between satelite and input data in time
  !
  subroutine sat_filter_data(MY_FILES,MY_TIME,GL_SAT,GL_SAT_PTS,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_TIME       run time related parameters
    !>   @param GL_SAT        variables related to original sat data
    !>   @param GL_SAT_PTS    variables related to points sat data (filtered points)
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(RUN_TIME),        intent(INOUT) :: MY_TIME
    type(SAT_DATA),        intent(INOUT) :: GL_SAT
    type(SAT_DATA_POINTS), intent(INOUT) :: GL_SAT_PTS
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: it,i,j,jp,lulog
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: julday1,julday2
    real(rp)              :: dlon,dlat,colat
    character(len=24)     :: time_str
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_filter_data'
    MY_ERR%message = ' '
    !
    if(GL_SAT%read_all_slabs) then
       it = GL_SAT%islab
    else
       it = 1
    end if
    !
    !*** Get the number of points
    !
    GL_SAT_PTS%np = 0
    do j = 1,GL_SAT%ny
       do i = 1,GL_SAT%nx
          if(GL_SAT%mass(i,j,it).gt.0.0_rp .and. GL_SAT%mass(i,j,it).ne.GL_SAT%fill_value) GL_SAT_PTS%np = GL_SAT_PTS%np + 1
       end do
    end do
    !
    if(GL_SAT_PTS%np.eq.0) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'No points with positive column mass detected'
       return
    end if
    !
    !*** Allocates
    !
    allocate(GL_SAT_PTS%lon  (GL_SAT_PTS%np))
    allocate(GL_SAT_PTS%lat  (GL_SAT_PTS%np))
    allocate(GL_SAT_PTS%mass (GL_SAT_PTS%np))
    allocate(GL_SAT_PTS%htop (GL_SAT_PTS%np))
    allocate(GL_SAT_PTS%thick(GL_SAT_PTS%np))
    allocate(GL_SAT_PTS%area (GL_SAT_PTS%np))
    !
    !*** Fills the structure
    !
    jp = 0
    do j = 1,GL_SAT%ny
       do i = 1,GL_SAT%nx
          if(GL_SAT%mass(i,j,it).gt.0.0_rp .and. GL_SAT%mass(i,j,it).ne.GL_SAT%fill_value) then
             jp = jp + 1
             GL_SAT_PTS%lon  (jp) = GL_SAT%lon  (i,j)
             GL_SAT_PTS%lat  (jp) = GL_SAT%lat  (i,j)
             GL_SAT_PTS%mass (jp) = GL_SAT%mass (i,j,it)
             GL_SAT_PTS%htop (jp) = GL_SAT%htop (i,j,it)
             GL_SAT_PTS%thick(jp) = GL_SAT%thick(i,j,it)
             !
             !*** Computes associated area
             !
             if(i.eq.1) then
                dlon = GL_SAT%lon(i+1,j)-GL_SAT%lon(i,j)
             else if(i.eq.GL_SAT%nx) then
                dlon = GL_SAT%lon(i,j)-GL_SAT%lon(i-1,j)
             else
                dlon = 0.5_rp*(GL_SAT%lon(i+1,j)-GL_SAT%lon(i-1,j))
             end if
             if(j.eq.1) then
                dlat = abs(GL_SAT%lat(i,j+1)-GL_SAT%lat(i,j))
             else if(j.eq.GL_SAT%ny) then
                dlat = abs(GL_SAT%lat(i,j)-GL_SAT%lat(i,j-1))
             else
                dlat = abs(0.5_rp*(GL_SAT%lat(i,j+1)-GL_SAT%lat(i,j-1)))
             end if
             dlon  = dlon*PI/180.0_rp
             dlat  = dlat*PI/180.0_rp
             colat = (90.0_rp- GL_SAT%lat(i,j))*PI/180.0_rp   ! colatitude in rad
             !
             GL_SAT_PTS%area(jp) = REARTH*REARTH*dlon*dlat*sin(colat)
          end if
       end do
    end do
    !
    !*** Print to log file
    !
    lulog = MY_FILES%lulog
    !
    call time_addtime(GL_SAT%start_year,GL_SAT%start_month, GL_SAT%start_day, GL_SAT%start_hour,  &
         iyr,imo,idy,ihr,imi,ise,GL_SAT%timesec(GL_SAT%islab),MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str, MY_ERR)
    write(lulog,20) GL_SAT%islab, TRIM(time_str), GL_SAT_PTS%np
20  format(/,&
         'DATA INSERTION',/, &
         '  Time slab          :  ',i9 ,/,      &
         '  Insertion time     :  ',a  ,/,      &
         '  Filtered points    :  ',i9)
    !
    !*** Calculates time lag (origins may belong to different days)
    !
    call time_julian_date(GL_SAT%start_year, GL_SAT%start_month,  GL_SAT%start_day, julday1, MY_ERR)
    call time_julian_date(MY_TIME%start_year,MY_TIME%start_month, MY_TIME%start_day,julday2, MY_ERR)
    !
    GL_SAT%time_lag = (julday2-julday1)*86400.0_rp - 3600*GL_SAT%start_hour - 60*GL_SAT%start_min
    !
    !*** Convert timesec to seconds after 00:00 UTC (not to seconds after HH:MM UTC referred to T0)
    !
    do it = 1,GL_SAT%nt_file
       GL_SAT%timesec(it) = GL_SAT%timesec(it) - GL_SAT%time_lag + GL_SAT%start_hour
    end do
    !
    GL_SAT_PTS%time    = GL_SAT%time   (GL_SAT%islab)
    GL_SAT_PTS%timesec = GL_SAT%timesec(GL_SAT%islab)
    !
    !*** Check insertion time consistency
    !
    if(GL_SAT%timesec(GL_SAT%islab).lt.MY_TIME%run_start) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Data insertion time before run start time'
       return
    end if
    !
    if(GL_SAT%timesec(GL_SAT%islab).gt.MY_TIME%run_end) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Data insertion time after run end time'
       return
    end if
    !
    return
  end subroutine sat_filter_data
  !
  !-----------------------------------------
  !    subroutine sat_bcast_sat_pts_data
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts the cloud of points structure
  !
  subroutine sat_bcast_sat_pts_data(GL_SAT_PTS, MY_ERR)
    implicit none
    !
    !>   @param GL_SAT_PTS    variables related to points sat data (filtered points)
    !>   @param MY_ERR        error handler
    !
    type(SAT_DATA_POINTS), intent(INOUT) :: GL_SAT_PTS
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip) :: npoin
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_bcast_sat_pts_data'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_SAT_PTS%np     ,1,0)
    call parallel_bcast(GL_SAT_PTS%time   ,1,0)
    call parallel_bcast(GL_SAT_PTS%timesec,1,0)
    !
    !*** Memory allocation
    !
    npoin = GL_SAT_PTS%np
    !
    if(.not.master) then
       allocate(GL_SAT_PTS%lon  (npoin))
       allocate(GL_SAT_PTS%lat  (npoin))
       allocate(GL_SAT_PTS%mass (npoin))
       allocate(GL_SAT_PTS%htop (npoin))
       allocate(GL_SAT_PTS%thick(npoin))
       allocate(GL_SAT_PTS%area (npoin))
    end if
    !
    call parallel_bcast(GL_SAT_PTS%lon  ,npoin,0)
    call parallel_bcast(GL_SAT_PTS%lat  ,npoin,0)
    call parallel_bcast(GL_SAT_PTS%mass ,npoin,0)
    call parallel_bcast(GL_SAT_PTS%htop ,npoin,0)
    call parallel_bcast(GL_SAT_PTS%thick,npoin,0)
    call parallel_bcast(GL_SAT_PTS%area ,npoin,0)
    !
    return
  end subroutine sat_bcast_sat_pts_data
  !
  !-----------------------------------------
  !    subroutine sat_set_dictionary
  !-----------------------------------------
  !
  subroutine sat_set_dictionary(  MY_ERR )
    implicit none
    !
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_set_dictionary'
    MY_ERR%message = ' '
    !
    EXISTS  (:) = .true.   ! default
    !
    select case(MY_ERR%flag)
    case(0)
       !
       !  Dafault model
       !
       DICTIONARY(DIM_LON   )  = 'lon'
       DICTIONARY(DIM_LAT   )  = 'lat'
       DICTIONARY(DIM_TIME  )  = 'time'
       !
       DICTIONARY(VAR_LON   )  = 'longitude'
       DICTIONARY(VAR_LAT   )  = 'latitude'
       DICTIONARY(VAR_TIME  )  = 'time'
       DICTIONARY(VAR_MASS  )  = 'mass_loading'
       DICTIONARY(VAR_HTOP  )  = 'cloud_top_height'
       DICTIONARY(VAR_THICK )  = 'cloud_thickness'
       !
       DICTIONARY(ATR_T0    )  = 'T0'
       DICTIONARY(ATR_SENSOR)  = 'unknown'
       DICTIONARY(ATR_PLATFO)  = 'unknown'
       DICTIONARY(ATR_RESOL )  = 'unknown'
       DICTIONARY(ATR_TRACER)  = 'unknown'
       !
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Type of data not implemented '
    end select
    !
    return
  end subroutine sat_set_dictionary
  !
  !
  !-----------------------------------------
  !    subroutine sat_read_dictionary
  !-----------------------------------------
  !
  !
  subroutine sat_read_dictionary( file_tbl_sat, MY_ERR )
    implicit none
    !
    character(len=s_file),intent(IN   ) :: file_tbl_sat
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
    MY_ERR%source  = 'sat_read_dictionary'
    MY_ERR%message = ' '
    !
    !*** Opens the file
    !
    open(90,FILE=TRIM(file_tbl_sat),STATUS='old',ERR=101)
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
    MY_ERR%message = 'error opening the input file '//TRIM(file_tbl_sat)
    return
    !
  end subroutine sat_read_dictionary
  !
  !
  !
END MODULE Sat
