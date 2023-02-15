!***************************************************************
!>
!> Module for procedures related to Satelite retrievals I/O
!> @author
!> Arnau Folch and Leonardo Mingari
!>
!***************************************************************
MODULE Sat
  use KindType
  use InpOut
  use Parallel
  use Domain
  use Time
  use Ensemble
  use netcdf
  !
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: sat_set_initial_condition
  PUBLIC :: sat_get_gl_info
  PUBLIC :: sat_get_my_data
  PUBLIC :: sat_get_gl_data2d
  PUBLIC :: sat_bcast_sat_data2d
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: sat_read_inp_insertion
  PRIVATE :: sat_bcast_info
  PRIVATE :: sat_bcast_data
  PRIVATE :: sat_set_dictionary
  PRIVATE :: sat_get_dictionary
  PRIVATE :: sat_get_info
  PRIVATE :: sat_get_data
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
  integer(ip), PRIVATE, parameter :: VAR_LON    = 10
  integer(ip), PRIVATE, parameter :: VAR_LAT    = 11
  integer(ip), PRIVATE, parameter :: VAR_TIME   = 12
  integer(ip), PRIVATE, parameter :: VAR_MASS   = 13
  integer(ip), PRIVATE, parameter :: VAR_HTOP   = 14
  integer(ip), PRIVATE, parameter :: VAR_THICK  = 15
  integer(ip), PRIVATE, parameter :: VAR_ERROR  = 16
  integer(ip), PRIVATE, parameter :: VAR_MASK   = 17
  !
  integer(ip), PRIVATE, parameter :: ATR_T0     = 20
  integer(ip), PRIVATE, parameter :: ATR_SENSOR = 21
  integer(ip), PRIVATE, parameter :: ATR_PLATFO = 22
  integer(ip), PRIVATE, parameter :: ATR_RESOL  = 23
  integer(ip), PRIVATE, parameter :: ATR_TRACER = 24
  !
  integer(ip), PRIVATE, parameter :: DICT_SIZE  = 30
  character(len=s_name), PRIVATE  :: DICTIONARY(DICT_SIZE)
  !
  integer(ip), parameter, private :: s_long  = 512    !  Generic long string lenght. Use '(a512)' to read
  integer(ip), parameter, private :: nwormax = 128
  integer(ip), parameter, private :: nparmax = 128
  !
  !    type SAT_INFO
  !
  type SAT_INFO
      !
      character(s_name) :: sensor                !< type of data sensor
      character(s_name) :: platform              !< type of data platform
      character(s_name) :: resolution            !< sat data resolution
      character(s_name) :: tracer_type           !< type of inserted data
      !
      integer(ip)       :: tracer_code           !< tracer code
      integer(ip)       :: islab                 !< time slab read
      real(rp)          :: d_cut_off             !< cut-off size
      !
      integer(ip)       :: nx                    !< number x points
      integer(ip)       :: ny                    !< number y points
      integer(ip)       :: nt                    !< number time slabs in file
      !
      logical           :: include_zeros         !< if zero mass points should be included
      logical           :: mandatory(DICT_SIZE)  !< mandatory variables in satellite file
      !
      real(rp),       allocatable :: timesec(:)  !< timesec(nt_file) slabs in sec after YYYY-MM-DD 00:00:00 UTC
      type(DATETIME), allocatable :: time(:)     !< time   (nt_file) slabs in datetime format
      !
  end type SAT_INFO
  !
  !    type SAT_DATA
  !
  type SAT_DATA
     !
     integer(ip)           :: np                 !< number of points
     real(rp)              :: timesec            !< time in sec after 0000UTC
     type(DATETIME)        :: time               !< time in datetime format
     !
     real(rp), allocatable :: lon  (:)           !< cloud points x-coordinates
     real(rp), allocatable :: lat  (:)           !< cloud points y-coordinates of source point
     real(rp), allocatable :: area (:)           !< cloud points area
     real(rp), allocatable :: mass (:)           !< cloud points mass
     real(rp), allocatable :: htop (:)           !< cloud points cloud-top height
     real(rp), allocatable :: thick(:)           !< cloud points cloud thickness
     real(rp), allocatable :: error(:)           !< cloud points error
     !
  end type SAT_DATA
  !
  !    type SAT_DATA2D
  !
  type SAT_DATA2D
     !
     real(rp)              :: fill_value
     !
     real(rp), allocatable :: lon  (:,:)         !< x-coordinates
     real(rp), allocatable :: lat  (:,:)         !< y-coordinates
     real(rp), allocatable :: mass (:,:,:)       !< total column mass loading
     real(rp), allocatable :: mask (:,:,:)       !< cloud mask
     !
  end type SAT_DATA2D
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
  !>   Set initial condition from satelite retrieval
  !
  subroutine sat_set_initial_condition(MY_FILES,MY_TIME,MY_GRID,MY_TRA,MY_ENS,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_TIME   run time related parameters
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_TRA    variables related to tracers
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(RUN_TIME),      intent(INOUT) :: MY_TIME
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(TRACERS),       intent(INOUT) :: MY_TRA
    type(ENS_PARAMS),    intent(IN   ) :: MY_ENS
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    type(SAT_INFO) :: GL_SAT_INFO
    type(SAT_DATA) :: MY_SAT_DATA
    !
    integer(ip) :: is,ix,iy,iz,ibin
    real(rp)    :: xs,ys
    real(rp)    :: ztop,zbottom,z1,z2
    real(rp)    :: mass,vol,fmass
    real(rp)    :: glonmin,glatmin
    real(rp)    :: dlon,dlat,inv_dlon,inv_dlat
    real(rp)    :: colmass,mass_fraction
    !
    real(rp), allocatable :: my_mass3d(:,:,:)
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
    if(master_model) call sat_read_inp_insertion(MY_FILES,GL_SAT_INFO,MY_ERR)
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    ! Read netcdf file, set and broadcast GL_SAT_INFO
    call sat_get_gl_info(MY_FILES,MY_TIME,GL_SAT_INFO,MY_ERR)
    GL_SAT_INFO%mandatory(VAR_ERROR) = .false. !Error is not required here
    !
    !*** Read filtered satelite data (one time slab only)
    !
    call sat_get_my_data(MY_FILES,MY_TIME,MY_GRID,GL_SAT_INFO,MY_SAT_DATA,MY_ERR)
    !
    !*** If necessary, perturbate initial conditions (cloud top and thickness) in ensemble runs
    !
    if(nens.gt.1) then
       MY_SAT_DATA%htop  = ensemble_perturbate_variable( ID_CLOUD_HEIGHT,    &
                                                         MY_SAT_DATA%htop,   &
                                                         MY_ENS )
       MY_SAT_DATA%thick = ensemble_perturbate_variable( ID_CLOUD_THICKNESS, &
                                                         MY_SAT_DATA%thick,  &
                                                         MY_ENS )
    end if 
    !
    !*** Interpolate data imposing mass conservation
    !
    allocate(my_mass3d(my_ips:my_ipe,my_jps:my_jpe,my_kps:my_kpe))
    my_mass3d(:,:,:) = 0.0_rp
    !
    glonmin = MY_GRID%lonmin
    glatmin = MY_GRID%latmin
    if(glonmin.ge.180.0_rp) glonmin = glonmin - 360.0_rp
    !
    inv_dlon = 1.0_rp/MY_GRID%dlon
    inv_dlat = 1.0_rp/MY_GRID%dlat
    !
    !*** Loop over potential source points
    !
    compute_mass: do is = 1,MY_SAT_DATA%np
       !
       xs = MY_SAT_DATA%lon(is)
       ys = MY_SAT_DATA%lat(is)
       !
       dlat = ys - glatmin
       dlon = xs - glonmin
       if(dlon.lt.0) dlon = dlon + 360.0_rp
       ix = 1 + int(dlon*inv_dlon)
       iy = 1 + int(dlat*inv_dlat)
       !
       !*** Total Mass in column
       !
       colmass = MY_SAT_DATA%mass(is)*MY_SAT_DATA%area(is)
       !
       ztop    = MY_SAT_DATA%htop(is)
       zbottom = MY_SAT_DATA%htop(is)-MY_SAT_DATA%thick(is)
       !
       do iz = my_kbs,my_kbe-1
         z1 = MY_GRID%z_c(ix,iy,iz  )
         z2 = MY_GRID%z_c(ix,iy,iz+1)
         !
         z1 = max(z1,zbottom)
         z2 = min(z2,ztop)
         if(z2.gt.z1) then
            mass_fraction = (z2-z1)/MY_SAT_DATA%thick(is)
            my_mass3d(ix,iy,iz) = my_mass3d(ix,iy,iz) + colmass * mass_fraction
         end if
       end do
       !
    end do compute_mass
    !
    !*** Check spatial consistency of points (i.e. mass exists in the domain)
    !
    allocate(mass_local(0:npes_model-1))
    mass_local(:)          = 0.0_rp
    mass_local(mype_model) = sum(my_mass3d)
    call parallel_sum(mass_local, COMM_MODEL)
    mass = sum(mass_local)
    if(mass.le.0.0_rp) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'No mass found in the domain for insertion. Check domain limits'
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    end if
    !
    !*** Determine the affected bins and relative mass fraction
    !
    select case(GL_SAT_INFO%tracer_code)
    case(SPE_TEPHRA)
       !
       allocate(fc(MY_TRA%nbins))
       fc    = 0.0_rp
       fmass = 0.0_rp
       do ibin = 1,MY_TRA%nbins
          if(MY_TRA%MY_BIN%bin_spe (ibin).eq.SPE_TEPHRA.and. &
             MY_TRA%MY_BIN%bin_diam(ibin).le.GL_SAT_INFO%d_cut_off) then
             fc(ibin) = MY_TRA%MY_BIN%bin_fc(ibin)
             fmass = fmass + fc(ibin)
          end if
       end do
       fc(:) = fc(:)/fmass
       !
    case(SPE_SO2)
       !
       allocate(fc(MY_TRA%nbins))
       fc    = 0.0_rp
       do ibin = 1,MY_TRA%nbins
          if(MY_TRA%MY_BIN%bin_spe(ibin).eq.SPE_SO2) then
             fc(ibin) = 1.0_rp
          end if
       end do
    case default
       !
       MY_ERR%flag    = 1
       MY_ERR%message = 'Insertion data not available for tracer: '//TRIM(GL_SAT_INFO%tracer_type)
       call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
       !
    end select
    !
    !*** Set the (averaged) concentration. Note that concentration is already scaled (i.e. no
    !*** need to first divide and then multiply by map factors)
    !
    MY_TRA%my_c(:,:,:,:) = 0.0_rp 
    !
    do iz = my_kps,my_kpe
    do iy = my_jps,my_jpe
    do ix = my_ips,my_ipe
      vol = MY_GRID%dX1_p(ix)*MY_GRID%dX2_p(iy)*MY_GRID%dX3_p(iz)
      do ibin = 1,MY_TRA%nbins
        MY_TRA%my_c(ix,iy,iz,ibin) = fc(ibin)*my_mass3d(ix,iy,iz)/vol
      end do
    end do
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
    MY_TIME%run_start = MY_SAT_DATA%timesec
    MY_TRA%gl_mass_in = mass
    !
    return
  end subroutine sat_set_initial_condition
  !
  !-----------------------------------------
  !    subroutine sat_get_gl_info
  !-----------------------------------------
  !
  !>   @brief
  !>   Get global information from satelite data
  !
  subroutine sat_get_gl_info(MY_FILES,MY_TIME,GL_SAT_INFO,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_TIME       run time related parameters
    !>   @param GL_SAT_INFO   attributes in satellite input file
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(RUN_TIME),      intent(IN   ) :: MY_TIME
    type(SAT_INFO),      intent(INOUT) :: GL_SAT_INFO
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_get_gl_info'
    MY_ERR%message = ' '
    !
    !*** Master reads and broadcasts information about the satellite input file
    !
    if(master_model) call sat_get_info(MY_FILES,MY_TIME,GL_SAT_INFO,MY_ERR)
    call parallel_bcast(MY_ERR%flag,1,0)
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    call sat_bcast_info(GL_SAT_INFO,MY_ERR)
    !
  end subroutine sat_get_gl_info
  !
  !-----------------------------------------
  !    subroutine sat_get_my_data
  !-----------------------------------------
  !
  !>   @brief
  !>   Gets observations in my grid
  !
  subroutine sat_get_my_data(MY_FILES,MY_TIME,MY_GRID,GL_SAT_INFO,MY_SAT_DATA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_TIME       run time related parameters
    !>   @param MY_GRID       grid configuration parameters
    !>   @param GL_SAT_INFO   attributes in satellite input file
    !>   @param MY_SAT_DATA   variables related to points sat data (filtered points)
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(RUN_TIME),        intent(IN   ) :: MY_TIME
    type(ARAKAWA_C_GRID),  intent(IN   ) :: MY_GRID
    type(SAT_INFO),        intent(IN   ) :: GL_SAT_INFO
    type(SAT_DATA),        intent(INOUT) :: MY_SAT_DATA
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)                          :: ipoin,is
    real(rp)                             :: xs,ys 
    real(rp)                             :: lonmin,lonmax,latmin,latmax
    type(SAT_DATA)                       :: GL_SAT_DATA
    logical, allocatable                 :: obs_valid(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_get_my_data'
    MY_ERR%message = ' '
    !
    !*** Master reads and filters satellite data (one time slab only)
    !
    if(master_model) call sat_get_data(MY_FILES,    &
                                       MY_TIME,     &
                                       MY_GRID,     &
                                       GL_SAT_INFO, &
                                       GL_SAT_DATA, &
                                       MY_ERR )
    !LAM: It will not end all tasks (only the filter ones)
    !when data is assimilated. CORRECT IT!
    call parallel_bcast(MY_ERR%flag,1,0)
    !The error could be related to no valid points detected. 
    !What to do in this case?
    if(MY_ERR%flag.ne.0) call task_runend(TASK_RUN_FALL3D, MY_FILES, MY_ERR)
    !
    ! Allocate data in GL_SAT_DATA for non-master PEs and broadcast
    call sat_bcast_data(GL_SAT_INFO,GL_SAT_DATA,MY_ERR)
    !
    MY_SAT_DATA%time    = GL_SAT_DATA%time 
    MY_SAT_DATA%timesec = GL_SAT_DATA%timesec 
    !
    allocate(obs_valid(GL_SAT_DATA%np))
    obs_valid(:) = .false.
    !
    lonmin = MY_GRID%lon_c(my_ibs)
    lonmax = MY_GRID%lon_c(my_ibe)
    latmin = MY_GRID%lat_c(my_jbs)
    latmax = MY_GRID%lat_c(my_jbe)
    !
    ipoin = 0
    !
    ! loop over potential source points
    count_my_obs: do is = 1,GL_SAT_DATA%np 
       !
       xs = GL_SAT_DATA%lon(is)
       ys = GL_SAT_DATA%lat(is)
       !
       !Check latitudes
       if(ys.lt.latmin .or. ys.ge.latmax) cycle count_my_obs 
       !
       !Check longitudes (all in [-180,180))
       if(lonmin.lt.lonmax) then
           if(xs.lt.lonmin .or. xs.ge.lonmax) cycle count_my_obs
       else
           if(xs.lt.lonmin .and. xs.ge.lonmax) cycle count_my_obs
       end if
       !
       obs_valid(is) = .True.
       ipoin = ipoin + 1
       !
    end do count_my_obs
    !
    !*** Deallocate if allocated
    !*** this routine can be called multiple times
    !
    if(allocated(MY_SAT_DATA%lon))   deallocate(MY_SAT_DATA%lon  )
    if(allocated(MY_SAT_DATA%lat))   deallocate(MY_SAT_DATA%lat  )
    if(allocated(MY_SAT_DATA%area))  deallocate(MY_SAT_DATA%area )
    if(allocated(MY_SAT_DATA%mass))  deallocate(MY_SAT_DATA%mass )
    if(allocated(MY_SAT_DATA%htop))  deallocate(MY_SAT_DATA%htop )
    if(allocated(MY_SAT_DATA%thick)) deallocate(MY_SAT_DATA%thick)
    if(allocated(MY_SAT_DATA%error)) deallocate(MY_SAT_DATA%error)
    !
    MY_SAT_DATA%np = ipoin
    !
    if(MY_SAT_DATA%np.gt.0) then
        !
        !*** Allocate
        !
        allocate(MY_SAT_DATA%lon  (MY_SAT_DATA%np))
        allocate(MY_SAT_DATA%lat  (MY_SAT_DATA%np))
        allocate(MY_SAT_DATA%area (MY_SAT_DATA%np))
        !
        ipoin = 0
        do is=1,GL_SAT_DATA%np
          if(obs_valid(is)) then
              ipoin = ipoin + 1
              MY_SAT_DATA%lon  (ipoin) = GL_SAT_DATA%lon  (is)
              MY_SAT_DATA%lat  (ipoin) = GL_SAT_DATA%lat  (is) 
              MY_SAT_DATA%area (ipoin) = GL_SAT_DATA%area (is)
          end if
        end do
        !
        if(GL_SAT_INFO%mandatory(VAR_MASS)) then
            allocate(MY_SAT_DATA%mass (MY_SAT_DATA%np))
            ipoin = 0
            do is=1,GL_SAT_DATA%np
              if(obs_valid(is)) then
                  ipoin = ipoin + 1
                  MY_SAT_DATA%mass (ipoin) = GL_SAT_DATA%mass (is)
              end if
            end do
        end if
        !
        if(GL_SAT_INFO%mandatory(VAR_HTOP)) then
            allocate(MY_SAT_DATA%htop (MY_SAT_DATA%np))
            ipoin = 0
            do is=1,GL_SAT_DATA%np
              if(obs_valid(is)) then
                  ipoin = ipoin + 1
                  MY_SAT_DATA%htop (ipoin) = GL_SAT_DATA%htop (is)
              end if
            end do
        end if
        !
        if(GL_SAT_INFO%mandatory(VAR_THICK)) then
            allocate(MY_SAT_DATA%thick(MY_SAT_DATA%np))
            ipoin = 0
            do is=1,GL_SAT_DATA%np
              if(obs_valid(is)) then
                  ipoin = ipoin + 1
                  MY_SAT_DATA%thick(ipoin) = GL_SAT_DATA%thick(is)
              end if
            end do
        end if
        !
        if(GL_SAT_INFO%mandatory(VAR_ERROR)) then
            allocate(MY_SAT_DATA%error(MY_SAT_DATA%np))
            ipoin = 0
            do is=1,GL_SAT_DATA%np
              if(obs_valid(is)) then
                  ipoin = ipoin + 1
                  MY_SAT_DATA%error(ipoin) = GL_SAT_DATA%error(is)
              end if
            end do
        end if
        !
    end if
    !
    return
    !
  end subroutine sat_get_my_data
  !
  !-----------------------------------------
  !    subroutine sat_get_gl_data2d
  !-----------------------------------------
  !
  !>   @brief
  !>   Read gridded data from satelite retrieval
  !
  subroutine sat_get_gl_data2d(MY_FILES,GL_SAT_INFO,GL_SAT_DATA2D,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES       list of files
    !>   @param GL_SAT_INFO    metadata of satellite input file
    !>   @param GL_SAT_DATA2D  satellite gridded data
    !>   @param MY_ERR         error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(SAT_INFO),      intent(IN   ) :: GL_SAT_INFO
    type(SAT_DATA2D),    intent(INOUT) :: GL_SAT_DATA2D
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: file_in
    integer(ip)           :: istat
    integer(ip)           :: nt,nx,ny
    real(rp)              :: FillValue
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_get_gl_data2d'
    MY_ERR%message = ' '
    !
    file_in = MY_FILES%file_sat
    !    
    nt = GL_SAT_INFO%nt
    nx = GL_SAT_INFO%nx
    ny = GL_SAT_INFO%ny
    !
    !*** Allocates
    !
    allocate(GL_SAT_DATA2D%lon (nx,ny))
    allocate(GL_SAT_DATA2D%lat (nx,ny))
    allocate(GL_SAT_DATA2D%mass(nx,ny,nt))
    allocate(GL_SAT_DATA2D%mask(nx,ny,nt))
    !
    !*** Open netCDF file and get ncID
    !
    istat = nf90_open(TRIM(file_in),NF90_NOWRITE,ncID)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_gl_data2d'
       MY_ERR%message = 'Unable to open '//TRIM(MY_FILES%file_sat)
       return
    end if
    !
    !*** Read variables
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LON),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT_DATA2D%lon, &
                           start=(/1,1/),                &
                           count=(/nx,ny/))
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_gl_data2d'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LAT),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT_DATA2D%lat, &
                           start=(/1,1/),                &
                           count=(/nx,ny/))
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_gl_data2d'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_MASS),varID)
    if(istat.eq.nf90_noerr) then
       istat = nf90_get_var  (ncID,varID,GL_SAT_DATA2D%mass, &
                              start=(/1,1,1/),               &
                              count=(/nx,ny,nt/))
       istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
       if (istat.ne.nf90_noerr) FillValue = NF90_FILL_REAL
       GL_SAT_DATA2D%fill_value = FillValue
    else
       GL_SAT_DATA2D%mass(:,:,:) = 0.0_rp
    end if
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_MASK),varID)
    if(istat.eq.nf90_noerr) then
       istat = nf90_get_var  (ncID,varID,GL_SAT_DATA2D%mask, &
                              start=(/1,1,1/),               &
                              count=(/nx,ny,nt/))
       istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
       if (istat.ne.nf90_noerr) FillValue = NF90_FILL_REAL
       GL_SAT_DATA2D%fill_value = FillValue
    else
       GL_SAT_DATA2D%mask(:,:,:) = 0.0_rp
    end if
    !
    select case(GL_SAT_INFO%tracer_code)
    case(SPE_SO2)
       where(GL_SAT_DATA2D%mass.ne.FillValue)
           ! DU --> g/m2 --> kg/m2.  
           ! Molecular mass SO2 = 64 gr/mol; 
           ! Avogadro/1DU = 2.238e3_rp
           GL_SAT_DATA2D%mass = GL_SAT_DATA2D%mass * 64.0_rp / 2.238e3_rp / 1e3_rp
       elsewhere
           GL_SAT_DATA2D%mass = GL_SAT_DATA2D%fill_value 
       endwhere
    case default
       where(GL_SAT_DATA2D%mass.ne.FillValue)
           ! g/m2 --> kg/m2
           GL_SAT_DATA2D%mass = GL_SAT_DATA2D%mass * 1e-3_rp
       elsewhere
           GL_SAT_DATA2D%mass = GL_SAT_DATA2D%fill_value
       endwhere
    end select
    !
    !*** Close the file
    !
    istat = nf90_close(ncID)
    !
  end subroutine sat_get_gl_data2d
  !
  !-----------------------------------------
  !    subroutine sat_bcast_sat_data2d
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts the satellite gridded data 
  !
  subroutine sat_bcast_sat_data2d(GL_SAT_INFO,GL_SAT_DATA2D,MY_ERR)
    implicit none
    !
    !>   @param GL_SAT_INFO    metadata of satellite input file
    !>   @param GL_SAT_DATA2D  satellite gridded data
    !>   @param MY_ERR         error handler
    !
    type(SAT_INFO),     intent(IN   ) :: GL_SAT_INFO
    type(SAT_DATA2D),   intent(INOUT) :: GL_SAT_DATA2D
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: nt,nx,ny
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_bcast_sat_data2d'
    MY_ERR%message = ' '
    !
    nt = GL_SAT_INFO%nt
    nx = GL_SAT_INFO%nx
    ny = GL_SAT_INFO%ny
    !
    call parallel_bcast(GL_SAT_DATA2D%fill_value,1,0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(GL_SAT_DATA2D%lon (nx,ny   ))
       allocate(GL_SAT_DATA2D%lat (nx,ny   ))
       allocate(GL_SAT_DATA2D%mass(nx,ny,nt))
       allocate(GL_SAT_DATA2D%mask(nx,ny,nt))
    end if
    !
    call parallel_bcast(GL_SAT_DATA2D%lon ,nx*ny,   0)
    call parallel_bcast(GL_SAT_DATA2D%lat ,nx*ny,   0)
    call parallel_bcast(GL_SAT_DATA2D%mass,nx*ny*nt,0)
    call parallel_bcast(GL_SAT_DATA2D%mask,nx*ny*nt,0)
    !
    return
  end subroutine sat_bcast_sat_data2d
  !
  !
  !    PRIVATE ROUTINES
  !
  !-----------------------------------------
  !    subroutine sat_read_inp_insertion
  !-----------------------------------------
  !
  !>   @brief
  !>   Read the INSERTION_DATA block from the input file
  !
  subroutine sat_read_inp_insertion(MY_FILES,GL_SAT_INFO,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES       list of files
    !>   @param GL_SAT_INFO    variables related to sat data
    !>   @param MY_ERR         error handler
    !
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(SAT_INFO),    intent(INOUT) :: GL_SAT_INFO
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    real(rp)              :: file_version
    character(len=s_file) :: file_inp
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_read_inp_insertion'
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
       MY_ERR%source  = 'sat_read_inp_insertion'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version'
       return
    end if
    !
    !*** Read INSERTION_DATA block
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
    call inpout_get_int (file_inp, 'INSERTION_DATA','INSERTION_TIME_SLAB',GL_SAT_INFO%islab, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       GL_SAT_INFO%islab = 1
       MY_ERR%flag  = 0
    end if
    !
    call inpout_get_rea (file_inp, 'INSERTION_DATA','DIAMETER_CUT_OFF_(MIC)',GL_SAT_INFO%d_cut_off, 1, MY_ERR)
    if(MY_ERR%flag.eq.0) then
       GL_SAT_INFO%d_cut_off = 1e-6_rp*GL_SAT_INFO%d_cut_off  ! mic --> m
    else
       GL_SAT_INFO%d_cut_off = 1d9 ! no cut off
    end if
    !
    return
  end subroutine sat_read_inp_insertion
  !
  !-----------------------------------------
  !    subroutine sat_bcast_info
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts info from the satellite file
  !
  subroutine sat_bcast_info(GL_SAT_INFO,MY_ERR)
    implicit none
    !
    !>   @param GL_SAT_INFO    variables related to sat data
    !>   @param MY_ERR         error handler
    !
    type(SAT_INFO),     intent(INOUT) :: GL_SAT_INFO
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_bcast_info'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_SAT_INFO%sensor              ,1,0)
    call parallel_bcast(GL_SAT_INFO%platform            ,1,0)
    call parallel_bcast(GL_SAT_INFO%resolution          ,1,0)
    call parallel_bcast(GL_SAT_INFO%tracer_type         ,1,0)
    !
    call parallel_bcast(GL_SAT_INFO%tracer_code         ,1,0)
    call parallel_bcast(GL_SAT_INFO%islab               ,1,0)
    call parallel_bcast(GL_SAT_INFO%d_cut_off           ,1,0)
    !
    call parallel_bcast(GL_SAT_INFO%nx                  ,1,0)
    call parallel_bcast(GL_SAT_INFO%ny                  ,1,0)
    call parallel_bcast(GL_SAT_INFO%nt                  ,1,0)
    !
    call parallel_bcast(GL_SAT_INFO%include_zeros       ,1,0)
    call parallel_bcast(GL_SAT_INFO%mandatory,DICT_SIZE   ,0)
    !
    !*** No broadcast performed for time
    !
    if(.not.master_model) then
        if(allocated(GL_SAT_INFO%timesec)) deallocate(GL_SAT_INFO%timesec)
        allocate(GL_SAT_INFO%timesec(GL_SAT_INFO%nt))
    end if
    call parallel_bcast(GL_SAT_INFO%timesec,GL_SAT_INFO%nt,0)
    !
    return
    !
  end subroutine sat_bcast_info
  !
  !-----------------------------------------
  !    subroutine sat_bcast_data
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts satellite data
  !
  subroutine sat_bcast_data(GL_SAT_INFO,GL_SAT_DATA,MY_ERR)
    implicit none
    !
    !>   @param GL_SAT_DATA   variables related to points sat data (filtered points)
    !>   @param MY_ERR        error handler
    !
    type(SAT_INFO),        intent(IN   ) :: GL_SAT_INFO
    type(SAT_DATA),        intent(INOUT) :: GL_SAT_DATA
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip) :: npoin
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_bcast_data'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_SAT_DATA%np,          1,0)
    call parallel_bcast(GL_SAT_DATA%timesec,     1,0)
    call parallel_bcast(GL_SAT_DATA%time%year,   1,0)
    call parallel_bcast(GL_SAT_DATA%time%month,  1,0)
    call parallel_bcast(GL_SAT_DATA%time%day,    1,0)
    call parallel_bcast(GL_SAT_DATA%time%hour,   1,0)
    call parallel_bcast(GL_SAT_DATA%time%minute, 1,0)
    call parallel_bcast(GL_SAT_DATA%time%second, 1,0)
    !
    !*** Memory allocation
    !
    npoin = GL_SAT_DATA%np
    !
    if(.not. allocated(GL_SAT_DATA%lon) ) allocate(GL_SAT_DATA%lon  (npoin))
    if(.not. allocated(GL_SAT_DATA%lat) ) allocate(GL_SAT_DATA%lat  (npoin))
    if(.not. allocated(GL_SAT_DATA%area)) allocate(GL_SAT_DATA%area (npoin))
    !
    call parallel_bcast(GL_SAT_DATA%lon,  npoin,0)
    call parallel_bcast(GL_SAT_DATA%lat,  npoin,0)
    call parallel_bcast(GL_SAT_DATA%area, npoin,0)
    !
    if(GL_SAT_INFO%mandatory(VAR_MASS)) then
        if(.not. allocated(GL_SAT_DATA%mass))  allocate(GL_SAT_DATA%mass  (npoin))
        call parallel_bcast(GL_SAT_DATA%mass,npoin,0)
    end if
    !
    if(GL_SAT_INFO%mandatory(VAR_HTOP)) then
        if(.not. allocated(GL_SAT_DATA%htop))  allocate(GL_SAT_DATA%htop  (npoin))
        call parallel_bcast(GL_SAT_DATA%htop,npoin,0)
    end if
    !
    if(GL_SAT_INFO%mandatory(VAR_THICK)) then
        if(.not. allocated(GL_SAT_DATA%thick)) allocate(GL_SAT_DATA%thick (npoin))
        call parallel_bcast(GL_SAT_DATA%thick,npoin,0)
    end if
    !
    if(GL_SAT_INFO%mandatory(VAR_ERROR)) then
        if(.not. allocated(GL_SAT_DATA%error)) allocate(GL_SAT_DATA%error (npoin))
        call parallel_bcast(GL_SAT_DATA%error,npoin,0)
    end if
    !
    return
  end subroutine sat_bcast_data
  !
  !-----------------------------------------
  !    subroutine sat_set_dictionary
  !-----------------------------------------
  !
  subroutine sat_set_dictionary(MY_ERR)
    implicit none
    !
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_set_dictionary'
    MY_ERR%message = ' '
    !
    !  Dafault data structure
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
    DICTIONARY(VAR_ERROR )  = 'mass_error'
    DICTIONARY(VAR_MASK  )  = 'ash_flag'
    !
    DICTIONARY(ATR_T0    )  = 'T0'
    DICTIONARY(ATR_SENSOR)  = 'unknown'
    DICTIONARY(ATR_PLATFO)  = 'unknown'
    DICTIONARY(ATR_RESOL )  = 'unknown'
    DICTIONARY(ATR_TRACER)  = 'unknown'
    !
    return
  end subroutine sat_set_dictionary
  !
  !-----------------------------------------
  !    subroutine sat_get_dictionary
  !-----------------------------------------
  !
  subroutine sat_get_dictionary( file_tbl_sat, MY_ERR )
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
    MY_ERR%source  = 'sat_get_dictionary'
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
  end subroutine sat_get_dictionary
  !
  !---------------------------------
  !    subroutine sat_get_info
  !---------------------------------
  !
  !>   @brief
  !>   Reads attributes from a satellite data file
  !
  subroutine sat_get_info(MY_FILES,MY_TIME,GL_SAT_INFO,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_TIME       run time related parameters
    !>   @param GL_SAT_INFO   attributes in satellite input file
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(RUN_TIME),        intent(IN   ) :: MY_TIME
    type(SAT_INFO),        intent(INOUT) :: GL_SAT_INFO
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: istat,it,lulog
    integer(ip)           :: ref_year,ref_month,ref_day,ref_hour,ref_min 
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: julday1,julday2
    real(rp)              :: time_lag
    character(len=s_name) :: str,timeunit_string
    character(len=24)     :: time_str1,time_str2
    real(rp)              :: time_factor
    type(DATETIME)        :: time_ref
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_get_info'
    MY_ERR%message = ' '
    !
    !*** Define dictionary
    !
    if(MY_FILES%file_tbl_sat.eq.'-') then
       call sat_set_dictionary(MY_ERR)
       if(MY_ERR%flag.ne.0) return
    else
       call sat_get_dictionary(MY_FILES%file_tbl_sat, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    end if
    !
    !*** Open netCDF file and get ncID
    !
    istat = nf90_open(TRIM(MY_FILES%file_sat),NF90_NOWRITE, ncID)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_info'
       MY_ERR%message = 'Unable to open '//TRIM(MY_FILES%file_sat)
       return
    end if
    !
    !*** Read dimensions
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LON),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_SAT_INFO%nx)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_info'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LAT),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_SAT_INFO%ny)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_info'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_TIME),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_SAT_INFO%nt)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_info'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    ! Set default values
    GL_SAT_INFO%include_zeros = .false.
    GL_SAT_INFO%mandatory(:)  = .true.
    !
    !*** Allocates
    !
    allocate(GL_SAT_INFO%time   (GL_SAT_INFO%nt))
    allocate(GL_SAT_INFO%timesec(GL_SAT_INFO%nt))
    !
    !*** Get timesec for all slabs in the file (referred to 00:00UTC of T0)
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_TIME),varID)
    istat = nf90_get_var  (ncID,varID,GL_SAT_INFO%timesec,start=(/1/),count=(/GL_SAT_INFO%nt/))
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_info'
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
        MY_ERR%source  = 'sat_get_info'
        MY_ERR%message = nf90_strerror(istat)
        return
    end if
    !
    call inpout_decode_timeunit(timeunit_string,time_factor,time_ref,MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    ! Convert to seconds since time_ref
    GL_SAT_INFO%timesec = GL_SAT_INFO%timesec * time_factor
    !
    ! Correct %timesec to refer it to 00:00 UTC of time_ref
    GL_SAT_INFO%timesec = GL_SAT_INFO%timesec         + &
                          time_ref%hour   * 3600.0_dp + &
                          time_ref%minute * 60.0_dp   + &
                          time_ref%second * 1.0_dp
    !
    ! Compute %time in datetime format
    do it = 1,GL_SAT_INFO%nt
       call time_addtime(time_ref%year,           &
                         time_ref%month,          &
                         time_ref%day,            &
                         0_ip,                    &
                         iyr,imo,idy,ihr,imi,ise, &
                         GL_SAT_INFO%timesec(it), &
                         MY_ERR )
       GL_SAT_INFO%time(it) = DATETIME(iyr,imo,idy,ihr,imi,ise)
    end do
    !
    ! Compute diference between FALL3D and netcdf reference times
    call time_julian_date(time_ref%year,time_ref%month,time_ref%day,julday1,MY_ERR)
    call time_julian_date(MY_TIME%start_year,MY_TIME%start_month,MY_TIME%start_day,julday2,MY_ERR)
    time_lag = (julday2-julday1)*86400.0_rp
    !
    !*** Convert timesec to seconds after 00:00 UTC
    !*** of FALL3D reference time (instead of time_ref)
    do it = 1,GL_SAT_INFO%nt
       GL_SAT_INFO%timesec(it) = GL_SAT_INFO%timesec(it) - time_lag 
    end do
    !
    !*** Read global attributes
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_SENSOR), GL_SAT_INFO%sensor)
    if(istat.ne.nf90_noerr) GL_SAT_INFO%sensor ='N/A'
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_PLATFO), GL_SAT_INFO%platform)
    if(istat.ne.nf90_noerr) GL_SAT_INFO%platform ='N/A'
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_RESOL), GL_SAT_INFO%resolution)
    if(istat.ne.nf90_noerr) GL_SAT_INFO%resolution ='N/A'
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_TRACER), GL_SAT_INFO%tracer_type)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_info'
       MY_ERR%message = nf90_strerror(istat)
       return
    else
       call upcase(GL_SAT_INFO%tracer_type)
       if(TRIM(GL_SAT_INFO%tracer_type).eq.'TEPHRA') then
         GL_SAT_INFO%tracer_code = SPE_TEPHRA
       else if(TRIM(GL_SAT_INFO%tracer_type).eq.'ASH') then
         GL_SAT_INFO%tracer_code = SPE_TEPHRA
       else if(TRIM(GL_SAT_INFO%tracer_type).eq.'DUST') then
         GL_SAT_INFO%tracer_code = SPE_DUST
       else if(TRIM(GL_SAT_INFO%tracer_type).eq.'SO2') then
         GL_SAT_INFO%tracer_code = SPE_SO2 
       else if(TRIM(GL_SAT_INFO%tracer_type).eq.'H2O') then
         GL_SAT_INFO%tracer_code = SPE_H2O
       else
         MY_ERR%flag    = 1
         MY_ERR%message = 'Type of inserted tracer not allowed'
         return
       end if
    end if
    !
    !*** Close the file
    !
    istat = nf90_close(ncID)
    !
    !*** Print to log file
    !
    lulog = MY_FILES%lulog
    !
    iyr = GL_SAT_INFO%time(1)%year
    imo = GL_SAT_INFO%time(1)%month
    idy = GL_SAT_INFO%time(1)%day
    ihr = GL_SAT_INFO%time(1)%hour
    imi = GL_SAT_INFO%time(1)%minute
    ise = GL_SAT_INFO%time(1)%second
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str1, MY_ERR)
    iyr = GL_SAT_INFO%time(GL_SAT_INFO%nt)%year
    imo = GL_SAT_INFO%time(GL_SAT_INFO%nt)%month
    idy = GL_SAT_INFO%time(GL_SAT_INFO%nt)%day
    ihr = GL_SAT_INFO%time(GL_SAT_INFO%nt)%hour
    imi = GL_SAT_INFO%time(GL_SAT_INFO%nt)%minute
    ise = GL_SAT_INFO%time(GL_SAT_INFO%nt)%second
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,3_ip, time_str2, MY_ERR)
    !
    write(lulog,10)
    write(lulog,20) time_str1,time_str2
    write(lulog,30) TRIM(GL_SAT_INFO%sensor),         &
                    TRIM(GL_SAT_INFO%platform),       &
                    TRIM(GL_SAT_INFO%resolution),     &
                    TRIM(GL_SAT_INFO%tracer_type),    &
                    GL_SAT_INFO%nx,  GL_SAT_INFO%ny, GL_SAT_INFO%nt
10  format(                                                       /, &
           '----------------------------------------------------',/, &
           '                                                    ',/, &
           '                    SATELLITE DATA                  ',/, &
           '                                                    ',/, &
           '----------------------------------------------------')
20  format(                                                       /, &
           'TIME RANGE OF SATELITE DATA'                         ,/, &
           '  Initial time       : ',a,/, &
           '  Final time         : ',a)
30  format(                                            /, &
           'TYPE AND COVERAGE OF SATELLITE DATA'      ,/, &
           '  Sensor             :  ',a               ,/, &
           '  Platform           :  ',a               ,/, &
           '  Resolution         :  ',a               ,/, &
           '  Tracer type        :  ',a               ,/, &
           '  Number points x    : ',i9               ,/, &
           '  Number points y    : ',i9               ,/, &
           '  Number time slabs  : ',i9                   )
    !
  end subroutine sat_get_info
  !
  !---------------------------------
  !    subroutine sat_get_data
  !---------------------------------
  !
  !>   @brief
  !>   Get observation data filtered
  !
  subroutine sat_get_data(MY_FILES,MY_TIME,MY_GRID,GL_SAT_INFO,GL_SAT_DATA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_TIME       run time related parameters
    !>   @param MY_GRID       grid configuration parameters
    !>   @param GL_SAT_INFO   attributes in satellite input file
    !>   @param GL_SAT_DATA   variables related to points sat data (filtered points)
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(RUN_TIME),        intent(IN   ) :: MY_TIME
    type(ARAKAWA_C_GRID),  intent(IN   ) :: MY_GRID
    type(SAT_INFO),        intent(IN   ) :: GL_SAT_INFO
    type(SAT_DATA),        intent(INOUT) :: GL_SAT_DATA
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)              :: istat,lulog
    integer(ip)              :: it,nx,ny,i,j,ipoin,ndims
    real(rp)                 :: dlon,dlat,colat
    real(rp)                 :: latmin,latmax,lonmin,lonmax
    real(rp)                 :: lon_west,lon_east
    real(rp)                 :: fill_value
    integer(ip), allocatable :: sat_mask  (:,:)
    real(rp),    allocatable :: sat_lat   (:,:)
    real(rp),    allocatable :: sat_lon   (:,:)
    real(rp),    allocatable :: sat_mass  (:,:)
    real(rp),    allocatable :: sat_htop  (:,:)
    real(rp),    allocatable :: sat_thick (:,:)
    real(rp),    allocatable :: sat_error (:,:)
    real(rp),    allocatable :: work1d(:)
    character(len=s_file)    :: file_in
    logical                  :: strong_filter
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'sat_get_data'
    MY_ERR%message = ' '
    !
    if(GL_SAT_INFO%include_zeros) then
        strong_filter = .False.
    else
        ! Include only positive values 
        strong_filter = .True.
    end if
    !
    file_in = MY_FILES%file_sat
    !    
    it = GL_SAT_INFO%islab
    nx = GL_SAT_INFO%nx
    ny = GL_SAT_INFO%ny
    !
    !*** Set Time
    !
    if(it.gt.GL_SAT_INFO%nt) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Maximum number of time slabs exceeded'
       return
    else
       GL_SAT_DATA%time    = GL_SAT_INFO%time(it)
       GL_SAT_DATA%timesec = GL_SAT_INFO%timesec(it)
    end if
    !
    !*** Check insertion time consistency
    !
    if(GL_SAT_DATA%timesec.lt.MY_TIME%run_start) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Satellite data time before run start time'
       return
    end if
    !
    if(GL_SAT_DATA%timesec.gt.MY_TIME%run_end) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'Satellite data time after run end time'
       return
    end if
    !
    !*** Open netCDF file and get ncID
    !
    istat = nf90_open(TRIM(file_in),NF90_NOWRITE, ncID)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_data'
       MY_ERR%message = 'Unable to open '//TRIM(MY_FILES%file_sat)
       return
    end if
    !
    ! Mask for NaN data
    allocate(sat_mask (nx,ny))
    sat_mask = 1_ip
    !
    ! Mass loading
    if(GL_SAT_INFO%mandatory(VAR_MASS)) then
        allocate(sat_mass (nx,ny))
        istat = nf90_inq_varid(ncID,DICTIONARY(VAR_MASS),varID)
        if(istat.ne.nf90_noerr) then
           MY_ERR%flag    = istat
           MY_ERR%source  = 'sat_get_data'
           MY_ERR%message = nf90_strerror(istat)
           return
        else
           istat = nf90_get_var(ncID,varID,sat_mass, &
                                start = (/1,1,it/),  &
                                count = (/nx,ny,1/)  )
        end if
        !
        !*** if only positive values are filtered
        !
        if(strong_filter) then
            where(sat_mass.gt.0.0_rp) sat_mask = 0_ip
        else
            where(sat_mass.ge.0.0_rp) sat_mask = 0_ip
        end if
        !
        istat = nf90_get_att(ncID,varID,'_FillValue',fill_value)
        if(istat.eq.nf90_noerr) then
            where(sat_mass.eq.fill_value) sat_mask = 1_ip
        end if
        !
        select case(GL_SAT_INFO%tracer_code)
        case(SPE_SO2)
            ! Molecular mass SO2 = 64 gr/mol 
            ! Avogadro/1DU = 2.238e3_rp
            ! DU --> g/m2 --> kg/m2
            sat_mass = sat_mass * 64.0_rp / 2.238e3_rp / 1e3_rp 
        case default
            ! g/m2 --> kg/m2
            sat_mass = sat_mass * 1E-3_rp
        end select
        !
    end if
    !
    ! Longitudes
    allocate(sat_lon(nx,ny))
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LON),varID)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_data'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inquire_variable(ncID,varID,ndims=ndims)
    if(ndims.eq.1) then
       allocate(work1d(nx))
       istat = nf90_get_var(ncID,varID,work1d,  &
                            start=(/1/),        &
                            count=(/nx/)        )
       do j = 1,ny
           sat_lon(:,j) = work1d
       end do
       deallocate(work1d)
    else if(ndims.eq.2) then
       istat = nf90_get_var(ncID,varID,sat_lon, &
                            start = (/1,1/),    &
                            count = (/nx,ny/)   )
    else
       MY_ERR%flag    = 1
       MY_ERR%source  = 'sat_get_data'
       MY_ERR%message = "Unable to read dimension size for longitude"
       return
    end if
    !
    istat = nf90_get_att(ncID,varID,'_FillValue',fill_value)
    if(istat.eq.nf90_noerr) then
        where(sat_lon.eq.fill_value) sat_mask = 1_ip
    end if
    !
    ! Use longitudes in the interval [-180,180)
    where(sat_lon.ge.180_rp) sat_lon = sat_lon - 360.0_rp
    !
    ! Latitudes
    allocate(sat_lat(nx,ny))
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LAT),varID)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'sat_get_data'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inquire_variable(ncID,varID,ndims=ndims)
    if(ndims.eq.1) then
       allocate(work1d(ny))
       istat = nf90_get_var(ncID,varID,work1d,  &
                            start=(/1/),        &
                            count=(/ny/)        )
       do i = 1,nx
           sat_lat(i,:) = work1d
       end do
       deallocate(work1d)
    else if(ndims.eq.2) then
       istat = nf90_get_var(ncID,varID,sat_lat, &
                            start = (/1,1/),    &
                            count = (/nx,ny/)   )
    else
       MY_ERR%flag    = 1
       MY_ERR%source  = 'sat_get_data'
       MY_ERR%message = "Unable to read dimension size for latitude"
       return
    end if
    !
    istat = nf90_get_att(ncID,varID,'_FillValue',fill_value)
    if(istat.eq.nf90_noerr) then
        where(sat_lat.eq.fill_value) sat_mask = 1_ip
    end if
    !  
    ! Top height
    if(GL_SAT_INFO%mandatory(VAR_HTOP)) then
        allocate(sat_htop(nx,ny))
        istat = nf90_inq_varid(ncID,DICTIONARY(VAR_HTOP),varID)
        if(istat.ne.0) then
           MY_ERR%flag    = istat
           MY_ERR%source  = 'sat_get_data'
           MY_ERR%message = nf90_strerror(istat)
           return
        else
           istat = nf90_get_var(ncID,varID,sat_htop, &
                                start = (/1,1,it/),  &
                                count = (/nx,ny,1/)  )
        end if
        !
        istat = nf90_get_att(ncID,varID,'_FillValue',fill_value)
        if(istat.eq.nf90_noerr) then
            where(sat_htop.eq.fill_value) sat_mask = 1_ip
        end if
        !
        ! km -> m
        sat_htop  = sat_htop  * 1E3_rp
        !
    end if
    !  
    ! Cloud thick
    if(GL_SAT_INFO%mandatory(VAR_THICK)) then
        allocate(sat_thick(nx,ny))
        istat = nf90_inq_varid(ncID,DICTIONARY(VAR_THICK),varID)
        if(istat.ne.nf90_noerr) then
           MY_ERR%flag    = istat
           MY_ERR%source  = 'sat_get_data'
           MY_ERR%message = nf90_strerror(istat)
           return
        else
           istat = nf90_get_var(ncID,varID,sat_thick, &
                                start = (/1,1,it/),   &
                                count = (/nx,ny,1/)   )
        end if
        !
        istat = nf90_get_att(ncID,varID,'_FillValue',fill_value)
        if(istat.eq.nf90_noerr) then
            where(sat_thick.eq.fill_value) sat_mask = 1_ip
        end if
        !
        ! km -> m
        sat_thick = sat_thick * 1E3_rp
        !
    end if
    !  
    ! Mass loading error
    if(GL_SAT_INFO%mandatory(VAR_ERROR)) then
        allocate(sat_error(nx,ny))
        istat = nf90_inq_varid(ncID,DICTIONARY(VAR_ERROR),varID)
        if(istat.ne.nf90_noerr) then
           MY_ERR%flag    = istat
           MY_ERR%source  = 'sat_get_data'
           MY_ERR%message = nf90_strerror(istat)
           return
        else
           istat = nf90_get_var(ncID,varID,sat_error, &
                                start = (/1,1,it/),   &
                                count = (/nx,ny,1/)   )
        end if
        !
        istat = nf90_get_att(ncID,varID,'_FillValue',fill_value)
        if(istat.eq.nf90_noerr) then
            where(sat_error.eq.fill_value) sat_mask = 1_ip
        end if
        !
        select case(GL_SAT_INFO%tracer_code)
        case(SPE_SO2)
            ! Molecular mass SO2 = 64 gr/mol 
            ! Avogadro/1DU = 2.238e3_rp
            ! DU --> g/m2 --> kg/m2
            sat_error = sat_error * 64.0_rp / 2.238e3_rp / 1e3_rp 
        case default
            ! g/m2 --> kg/m2
            sat_error = sat_error * 1E-3_rp
        end select
        !
    end if
    !
    !*** Set mask for points outside domain
    !
    latmin = MY_GRID%latmin
    latmax = MY_GRID%latmax
    lonmin = MY_GRID%lonmin 
    lonmax = MY_GRID%lonmax 
    !
    ! Longitudes in the interval [-180,180)
    if(lonmin.ge.180.0_rp) lonmin = lonmin - 360.0
    if(lonmax.ge.180.0_rp) lonmax = lonmax - 360.0
    !
    if(lonmin.lt.lonmax) then
        where(sat_lat.lt.latmin .or. sat_lat.ge.latmax .or. &
              sat_lon.lt.lonmin .or. sat_lon.ge.lonmax ) sat_mask=1_ip
    else
        where(sat_lat.lt.latmin .or.  sat_lat.ge.latmax .or. &
             (sat_lon.lt.lonmin .and. sat_lon.ge.lonmax) ) sat_mask=1_ip
    end if
    !
    !*** Number of valid points
    !
    GL_SAT_DATA%np = nx*ny - SUM(sat_mask)
    if(GL_SAT_DATA%np.eq.0) then
       MY_ERR%flag    = 1
       MY_ERR%source  = 'sat_get_data'
       MY_ERR%message = 'No valid points detected in satellite file'
       return
    end if
    !
    !*** Allocate
    !
    allocate(GL_SAT_DATA%lon  (GL_SAT_DATA%np))
    allocate(GL_SAT_DATA%lat  (GL_SAT_DATA%np))
    allocate(GL_SAT_DATA%area (GL_SAT_DATA%np))
    if(GL_SAT_INFO%mandatory(VAR_MASS) ) allocate(GL_SAT_DATA%mass (GL_SAT_DATA%np))
    if(GL_SAT_INFO%mandatory(VAR_HTOP) ) allocate(GL_SAT_DATA%htop (GL_SAT_DATA%np))
    if(GL_SAT_INFO%mandatory(VAR_THICK)) allocate(GL_SAT_DATA%thick(GL_SAT_DATA%np))
    if(GL_SAT_INFO%mandatory(VAR_ERROR)) allocate(GL_SAT_DATA%error(GL_SAT_DATA%np))
    !
    !*** Fill the structure
    !
    ipoin = 0
    do j = 1,ny
    do i = 1,nx
      if(sat_mask(i,j).eq.0) then
          ipoin = ipoin + 1
          !
          !*** Computes associated area
          !
          if(i.eq.1) then
             lon_west = sat_lon(i,j)
          else
             lon_west = sat_lon(i-1,j)
          endif
          !
          if(i.eq.nx) then
             lon_east = sat_lon(i,j)
          else
             lon_east = sat_lon(i+1,j)
          end if
          !
          dlon = lon_east-lon_west
          if(dlon.lt.0) dlon = dlon + 360.0_rp
          if(i.gt.1 .and. i.lt.nx) dlon=dlon*0.5_rp
          !
          if(j.eq.1) then
             dlat = sat_lat(i,j+1)-sat_lat(i,j)
          else if(j.eq.ny) then
             dlat = sat_lat(i,j)-sat_lat(i,j-1)
          else
             dlat = 0.5_rp*(sat_lat(i,j+1)-sat_lat(i,j-1))
          end if
          !
          dlon  = abs(dlon)*PI/180.0_rp
          dlat  = abs(dlat)*PI/180.0_rp
          ! colatitude in rad
          colat = (90.0_rp-sat_lat(i,j))*PI/180.0_rp
          !
          GL_SAT_DATA%lon  (ipoin) = sat_lon  (i,j)
          GL_SAT_DATA%lat  (ipoin) = sat_lat  (i,j)
          GL_SAT_DATA%area (ipoin) = REARTH*REARTH*dlon*dlat*sin(colat)
          !
      end if
    end do
    end do
    !
    if(GL_SAT_INFO%mandatory(VAR_MASS) ) then
        ipoin = 0
        do j = 1,ny
        do i = 1,nx
            if(sat_mask(i,j).eq.0) then
                ipoin = ipoin + 1
                GL_SAT_DATA%mass (ipoin) = sat_mass (i,j)
            end if
        end do
        end do
    end if
    !
    if(GL_SAT_INFO%mandatory(VAR_HTOP) ) then
        ipoin = 0
        do j = 1,ny
        do i = 1,nx
            if(sat_mask(i,j).eq.0) then
                ipoin = ipoin + 1
                GL_SAT_DATA%htop (ipoin) = sat_htop (i,j)
            end if
        end do
        end do
    end if
    !
    if(GL_SAT_INFO%mandatory(VAR_THICK) ) then
        ipoin = 0
        do j = 1,ny
        do i = 1,nx
            if(sat_mask(i,j).eq.0) then
                ipoin = ipoin + 1
                GL_SAT_DATA%thick(ipoin) = sat_thick(i,j)
            end if
        end do
        end do
    end if
    !
    if(GL_SAT_INFO%mandatory(VAR_ERROR) ) then
        ipoin = 0
        do j = 1,ny
        do i = 1,nx
            if(sat_mask(i,j).eq.0) then
                ipoin = ipoin + 1
                GL_SAT_DATA%error(ipoin) = sat_error(i,j)
            end if
        end do
        end do
    end if
    !
    !*** Close the file
    !
    istat = nf90_close(ncID)
    !
    !*** Print to log file
    !
    lulog = MY_FILES%lulog
    !
    write(lulog,10) GL_SAT_INFO%islab,GL_SAT_DATA%time,GL_SAT_DATA%np
10  format(                                                              /, &
           'SATELLITE DATA'                                             ,/, &
           '  Time slab          :  ',i9                                ,/, &
           '  Time data          :  ',I4,2('-',I2.2),1x,I2.2,2(':',I2.2),/, &
           '  Filtered points    :  ',i9 )  
    !
  end subroutine sat_get_data
  ! 
END MODULE Sat
