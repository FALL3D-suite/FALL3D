!***************************************************************
!>
!> Module for operations related to deposit I/O
!> @author
!> Arnau Folch and Leonardo Mingari
!>
!***************************************************************
MODULE Deposit
  use KindType
  use InpOut
  use Parallel
  use netcdf
  !
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: deposit_get_depdata
  PUBLIC :: deposit_bcast_depdata
  PUBLIC :: deposit_get_ptsdata
  PUBLIC :: deposit_bcast_ptsdata
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: dep_set_dictionary
  PRIVATE :: dep_get_dictionary
  !
  !    LIST OF PRIVATE VARIABLES
  !
  integer(ip), PRIVATE :: ncID
  integer(ip), PRIVATE :: dimID
  integer(ip), PRIVATE :: varID
  !
  !    DICTIONARY DEFINITION
  !
  integer(ip), PRIVATE, parameter :: DIM_LON   = 1
  integer(ip), PRIVATE, parameter :: DIM_LAT   = 2
  !
  integer(ip), PRIVATE, parameter :: VAR_LON   = 10
  integer(ip), PRIVATE, parameter :: VAR_LAT   = 11
  integer(ip), PRIVATE, parameter :: VAR_LOAD  = 12
  integer(ip), PRIVATE, parameter :: VAR_THICK = 13
  integer(ip), PRIVATE, parameter :: VAR_MASK  = 14
  !
  integer(ip), PRIVATE, parameter :: ATR_TRACER = 20
  integer(ip), PRIVATE, parameter :: ATR_LABEL  = 21
  !
  integer(ip), PRIVATE, parameter :: DICT_SIZE  = 30
  character(len=s_name), PRIVATE  :: DICTIONARY(DICT_SIZE)
  !
  integer(ip), parameter, private :: s_long  = 512    !  Generic long string lenght. Use '(a512)' to read
  integer(ip), parameter, private :: nwormax = 128
  integer(ip), parameter, private :: nparmax = 128
  !
  !    type DEP_DATA
  !
  type DEP_DATA
     !
     logical           :: EXISTS(DICT_SIZE)     !< variables in deposit file
     character(s_name) :: tracer_type           !< type of inserted data
     integer(ip)       :: tracer_code           !< tracer code
     integer(ip)       :: nx                    !< number x points
     integer(ip)       :: ny                    !< number y points
     !
     real(rp)              :: fill_value
     real(rp), allocatable :: lon  (:,:)         !< x-coordinates
     real(rp), allocatable :: lat  (:,:)         !< y-coordinates
     real(rp), allocatable :: mass (:,:)         !< total mass loading
     real(rp), allocatable :: thick(:,:)         !< total deposit thickness
     real(rp), allocatable :: mask (:,:)         !< deposit mask
     !
  end type DEP_DATA
  !
  !    type DEP_PTS
  !
  type DEP_PTS
     !
     logical           :: EXISTS(DICT_SIZE)       !< variables in deposit file
     character(s_name) :: tracer_type             !< type of inserted data
     integer(ip)       :: tracer_code             !< tracer code
     integer(ip)       :: npts                    !< number of points
     !
     character(s_name), allocatable :: label(:)   !< point label
     real(rp),          allocatable :: lon  (:)   !< x-coordinates
     real(rp),          allocatable :: lat  (:)   !< y-coordinates
     real(rp),          allocatable :: mass (:)   !< total mass loading
     real(rp),          allocatable :: thick(:)   !< total deposit thickness
     !
  end type DEP_PTS
  !
  !
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine deposit_get_depdata
  !-----------------------------------------
  !
  !>   @brief
  !>   Read gridded data from a ground deposit
  !
  subroutine deposit_get_depdata(MY_FILES,GL_DEP_DATA,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES       list of files
    !>   @param GL_DEP_DATA    deposit gridded data
    !>   @param MY_ERR         error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(DEP_DATA),      intent(INOUT) :: GL_DEP_DATA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: istat,i,j,lulog
    !
    real(rp)              :: FillValue
    real(rp), allocatable :: work1d(:),work3d(:,:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'deposit_get_depdata'
    MY_ERR%message = ' '
    !
    !*** Define dictionary
    !
    if(MY_FILES%file_tbl_dep.eq.'-') then
       call dep_set_dictionary(MY_ERR)
       if(MY_ERR%flag.ne.0) return
    else
       call dep_get_dictionary(MY_FILES%file_tbl_dep, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    end if
    !
    !*** Open netCDF file and get ncID
    !
    istat = nf90_open(TRIM(MY_FILES%file_dep),NF90_NOWRITE, ncID)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'deposit_get_depdata'
       MY_ERR%message = 'Unable to open '//TRIM(MY_FILES%file_dep)
       return
    end if
    !
    !*** Read dimensions
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LON),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_DEP_DATA%nx)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'deposit_get_depdata'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    istat = nf90_inq_dimid        (ncID,DICTIONARY(DIM_LAT),dimID)
    istat = nf90_inquire_dimension(ncID, dimID, len = GL_DEP_DATA%ny)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'deposit_get_depdata'
       MY_ERR%message = nf90_strerror(istat)
       return
    end if
    !
    !*** Read global attributes
    !
    istat = nf90_get_att(ncID, NF90_GLOBAL, DICTIONARY(ATR_TRACER), GL_DEP_DATA%tracer_type)
    if(istat.ne.nf90_noerr) then
       MY_ERR%flag    = istat
       MY_ERR%source  = 'deposit_get_depdata'
       MY_ERR%message = nf90_strerror(istat)
       return
    else
       call upcase(GL_DEP_DATA%tracer_type)
       if(TRIM(GL_DEP_DATA%tracer_type).eq.'TEPHRA') then
         GL_DEP_DATA%tracer_code = SPE_TEPHRA
       else if(TRIM(GL_DEP_DATA%tracer_type).eq.'ASH') then
         GL_DEP_DATA%tracer_code = SPE_TEPHRA
       else if(TRIM(GL_DEP_DATA%tracer_type).eq.'DUST') then
         GL_DEP_DATA%tracer_code = SPE_DUST
       else if(TRIM(GL_DEP_DATA%tracer_type).eq.'SO2') then
         GL_DEP_DATA%tracer_code = SPE_SO2
       else if(TRIM(GL_DEP_DATA%tracer_type).eq.'H2O') then
         GL_DEP_DATA%tracer_code = SPE_H2O
       else
         MY_ERR%flag    = 1
         MY_ERR%message = 'Type of inserted tracer not allowed'
         return
       end if
    end if
    !
    !*** Allocate
    !
    allocate(GL_DEP_DATA%lon (GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    allocate(GL_DEP_DATA%lat (GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    allocate(GL_DEP_DATA%mass(GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    allocate(GL_DEP_DATA%mask(GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    !
    !*** Read longitude (from dimension or variable)
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LON),varID)
    if(istat.eq.nf90_noerr) then
       istat = nf90_get_var(ncID,varID,GL_DEP_DATA%lon, &
                            start=(/1,1/),count=(/GL_DEP_DATA%nx,GL_DEP_DATA%ny/))
    else
       allocate(work1d(GL_DEP_DATA%nx))
       istat = nf90_inq_varid(ncID,DICTIONARY(DIM_LON),varID)
       if(istat.ne.nf90_noerr) then
          MY_ERR%flag    = istat
          MY_ERR%source  = 'deposit_get_depdata'
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
       istat = nf90_get_var(ncID,varID,work1d,start=(/1/),count=(/GL_DEP_DATA%nx/))
       do j = 1,GL_DEP_DATA%ny
           GL_DEP_DATA%lon(:,j) = work1d(:)
       end do
       deallocate(work1d)
    end if
    !
    !*** Read latitude (from dimension or variable)
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LAT),varID)
    if(istat.eq.nf90_noerr) then
       istat = nf90_get_var(ncID,varID,GL_DEP_DATA%lat, &
                            start=(/1,1/),count=(/GL_DEP_DATA%nx,GL_DEP_DATA%ny/))
    else
       allocate(work1d(GL_DEP_DATA%ny))
       istat = nf90_inq_varid(ncID,DICTIONARY(DIM_LAT),varID)
       if(istat.ne.nf90_noerr) then
          MY_ERR%flag    = istat
          MY_ERR%source  = 'deposit_get_depdata'
          MY_ERR%message = nf90_strerror(istat)
          return
       end if
       istat = nf90_get_var(ncID,varID,work1d,start=(/1/),count=(/GL_DEP_DATA%ny/))
       do i = 1,GL_DEP_DATA%nx
           GL_DEP_DATA%lat(i,:) = work1d(:)
       end do
       deallocate(work1d)
    end if
    !
    !*** Read mass load
    !
    allocate(work3d(GL_DEP_DATA%nx,GL_DEP_DATA%ny,1)) ! contours may have trhesholds (ignore)

    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_LOAD),varID)
    if(istat.eq.nf90_noerr) then
       GL_DEP_DATA%EXISTS(VAR_LOAD) = .true.
       istat = nf90_get_var(ncID,varID,work3d, &
                            start=(/1,1,1/),count=(/GL_DEP_DATA%nx,GL_DEP_DATA%ny,1/))
       GL_DEP_DATA%mass(:,:) = work3d(:,:,1)
       istat = nf90_get_att(ncID,varID,'_FillValue',FillValue)
       if (istat.ne.nf90_noerr) FillValue = NF90_FILL_REAL
       GL_DEP_DATA%fill_value = FillValue
    else
       GL_DEP_DATA%EXISTS(VAR_LOAD) = .false.
       GL_DEP_DATA%fill_value       = NF90_FILL_REAL
       GL_DEP_DATA%mass             = 0.0_rp
    end if
    !
    !*** Read mask
    !
    istat = nf90_inq_varid(ncID,DICTIONARY(VAR_MASK),varID)
    if(istat.eq.nf90_noerr) then
       GL_DEP_DATA%EXISTS(VAR_MASK) = .true.
       istat = nf90_get_var(ncID,varID,work3d, &
                            start=(/1,1,1/),count=(/GL_DEP_DATA%nx,GL_DEP_DATA%ny,1/))
       GL_DEP_DATA%mask(:,:) = work3d(:,:,1)
    else
       GL_DEP_DATA%EXISTS(VAR_MASK) = .false.
       GL_DEP_DATA%mask = 0.0_rp
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
    write(lulog,10)
    write(lulog,20) TRIM(GL_DEP_DATA%tracer_type),    &
                    GL_DEP_DATA%nx,  GL_DEP_DATA%ny, 1
10  format(                                                       /, &
           '----------------------------------------------------',/, &
           '                                                    ',/, &
           '              DEPOSIT GRIDDED DATA                  ',/, &
           '                                                    ',/, &
           '----------------------------------------------------')
20  format(                                            /, &
           'TYPE AND COVERAGE OF DEPOSIT DATA'        ,/, &
           '  Tracer type        :  ',a               ,/, &
           '  Number points x    : ',i9               ,/, &
           '  Number points y    : ',i9               ,/, &
           '  Number time slabs  : ',i9                   )
    !
    return
  end subroutine deposit_get_depdata
  !
  !-----------------------------------------
  !    subroutine deposit_bcast_depdata
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts deposit data
  !
  subroutine deposit_bcast_depdata(GL_DEP_DATA,MY_ERR)
    implicit none
    !
    !>   @param GL_DEP_DATA    deposit gridded data
    !>   @param MY_ERR         error handler
    !
    type(DEP_DATA),      intent(INOUT) :: GL_DEP_DATA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: npoin
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'deposit_bcast_depdata'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_DEP_DATA%EXISTS,DICT_SIZE,0)
    call parallel_bcast(GL_DEP_DATA%tracer_type, 1,0)
    call parallel_bcast(GL_DEP_DATA%tracer_code, 1,0)
    call parallel_bcast(GL_DEP_DATA%nx,          1,0)
    call parallel_bcast(GL_DEP_DATA%ny,          1,0)
    call parallel_bcast(GL_DEP_DATA%fill_value,  1,0)
    !
    !*** Memory allocation
    !
    if(.not. allocated(GL_DEP_DATA%lon)  ) allocate(GL_DEP_DATA%lon  (GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    if(.not. allocated(GL_DEP_DATA%lat)  ) allocate(GL_DEP_DATA%lat  (GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    if(.not. allocated(GL_DEP_DATA%mass) ) allocate(GL_DEP_DATA%mass (GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    if(.not. allocated(GL_DEP_DATA%mask) ) allocate(GL_DEP_DATA%mask (GL_DEP_DATA%nx,GL_DEP_DATA%ny))
    !
    npoin = GL_DEP_DATA%nx * GL_DEP_DATA%ny
    call parallel_bcast(GL_DEP_DATA%lon,  npoin,0)
    call parallel_bcast(GL_DEP_DATA%lat,  npoin,0)
    call parallel_bcast(GL_DEP_DATA%mass, npoin,0)
    call parallel_bcast(GL_DEP_DATA%mask, npoin,0)
    !
    return
  end subroutine deposit_bcast_depdata
  !
  !-----------------------------------------
  !    subroutine deposit_get_ptsdata
  !-----------------------------------------
  !
  !>   @brief
  !>   Read scatered points from a ground deposit
  !
  subroutine deposit_get_ptsdata(MY_FILES,GL_DEP_PTS,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES       list of files
    !>   @param GL_DEP_PTS     deposit points data
    !>   @param MY_ERR         error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(DEP_PTS),       intent(INOUT) :: GL_DEP_PTS
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=s_long) :: card
    character(len=s_long) :: words(nwormax)
    integer(ip)           :: nword,npar
    real(rp)              :: param(nparmax)
    !
    logical               :: label_is_string, proceed, cm_to_mm
    character(len=s_file) :: fname
    integer(ip)           :: CODE(DICT_SIZE) = 0
    integer(ip)           :: icode,ncol,ipts,lulog
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'deposit_get_ptsdata'
    MY_ERR%message = ' '
    !
    GL_DEP_PTS%EXISTS(:) = .false.
    cm_to_mm             = .false.
    proceed              = .true.
    !
    GL_DEP_PTS%tracer_type = 'TEPHRA'
    GL_DEP_PTS%tracer_code = SPE_TEPHRA
    !
    !*** First decode the dictionary table
    !
    fname = MY_FILES%file_tbl_dep
    open(90,FILE=TRIM(fname),STATUS='old',ERR=101)
    !
    ncol = 0           ! number of expected columns
    do
       read(90,'(a512)',END=100) card
       call inpout_sdecode(card,words,param,nword,npar)
       if(npar.gt.0) then
          icode       = INT(param(1))
          CODE(icode) = INT(param(2))
          if(CODE(icode).ne.0) then
             ncol = ncol + 1
             GL_DEP_PTS%EXISTS(icode) = .true.
             !
             ! unit conversion
             if(TRIM(words(3)).eq.'cm') cm_to_mm = .true.
          end if
       end if
    end do
100 close(90)
    !
    !*** Read the pts file
    !
    fname = MY_FILES%file_dep
    !
    !*** Get number of points and allocate
    !
    GL_DEP_PTS%npts = 0
    call inpout_get_file_nrows(fname,GL_DEP_PTS%npts,MY_ERR)
    if(MY_ERR%flag    .ne.0) return
    if(GL_DEP_PTS%npts.eq.0) then
       MY_ERR%flag    = -1
       MY_ERR%source  = 'deposit_get_ptsdata'
       MY_ERR%message = 'deposit file has no points'
       return
    end if
    !
    allocate(GL_DEP_PTS%label(GL_DEP_PTS%npts))
    allocate(GL_DEP_PTS%lon  (GL_DEP_PTS%npts))
    allocate(GL_DEP_PTS%lat  (GL_DEP_PTS%npts))
    allocate(GL_DEP_PTS%mass (GL_DEP_PTS%npts))
    allocate(GL_DEP_PTS%thick(GL_DEP_PTS%npts))
    !
    ipts = 0
    open(90,FILE=TRIM(fname),STATUS='old',ERR=101)
    do
       read(90,'(a512)',END=200) card
       call inpout_sdecode(card,words,param,nword,npar)
       !
       if(npar.gt.0) then         ! decode this line
          proceed = .true.
          if(nword.eq.1) then
             label_is_string = .true.
             if(npar.ne.ncol-1) proceed = .false.
          else
             label_is_string = .false.
             if(npar.ne.ncol) proceed = .false.
          end if
          if(.not.proceed) then
             MY_ERR%flag    = -1
             MY_ERR%source  = 'deposit_get_ptsdata'
             MY_ERR%message = 'inconsistency between columns in deposit file and file tbl'
             return
          end if
          !
          ipts = ipts + 1
          select case(label_is_string)
          case(.true.)
             if(GL_DEP_PTS%EXISTS(ATR_LABEL)) GL_DEP_PTS%label(ipts) = TRIM(words(1))
             if(GL_DEP_PTS%EXISTS(VAR_LON  )) GL_DEP_PTS%lon  (ipts) = param(CODE(VAR_LON  )-1)
             if(GL_DEP_PTS%EXISTS(VAR_LAT  )) GL_DEP_PTS%lat  (ipts) = param(CODE(VAR_LAT  )-1)
             if(GL_DEP_PTS%EXISTS(VAR_LOAD )) GL_DEP_PTS%mass (ipts) = param(CODE(VAR_LOAD )-1)
             if(GL_DEP_PTS%EXISTS(VAR_THICK)) GL_DEP_PTS%thick(ipts) = param(CODE(VAR_THICK)-1)
          case(.false.)
             if(GL_DEP_PTS%EXISTS(ATR_LABEL)) write(GL_DEP_PTS%label(ipts),'(i4.4)') INT(param(CODE(ATR_LABEL)))
             if(GL_DEP_PTS%EXISTS(VAR_LON  )) GL_DEP_PTS%lon  (ipts) = param(CODE(VAR_LON  ))
             if(GL_DEP_PTS%EXISTS(VAR_LAT  )) GL_DEP_PTS%lat  (ipts) = param(CODE(VAR_LAT  ))
             if(GL_DEP_PTS%EXISTS(VAR_LOAD )) GL_DEP_PTS%mass (ipts) = param(CODE(VAR_LOAD ))
             if(GL_DEP_PTS%EXISTS(VAR_THICK)) GL_DEP_PTS%thick(ipts) = param(CODE(VAR_THICK))
          end select
          !
          !*** convert units if necessary
          !
          if(cm_to_mm) GL_DEP_PTS%thick(ipts) = 10.0_rp*GL_DEP_PTS%thick(ipts)  ! thick in mm
          !
       end if
    end do
200 close(90)
    !
    !*** Print to log file
    !
    lulog = MY_FILES%lulog
    !
    write(lulog,10)
    write(lulog,20) TRIM(GL_DEP_PTS%tracer_type), GL_DEP_PTS%npts
10  format(                                                       /, &
           '----------------------------------------------------',/, &
           '                                                    ',/, &
           '              DEPOSIT POINTS DATA                   ',/, &
           '                                                    ',/, &
           '----------------------------------------------------')
20  format(                                            /, &
           'TYPE AND COVERAGE OF DATA POINTS '        ,/, &
           '  Tracer type        :  ',a               ,/, &
           '  Number points      : ',i9               )
    !
    !*** Finally, convert thickness to load (only if load does not exist)
    !
    if(GL_DEP_PTS%EXISTS(VAR_LOAD )) return  ! I am done
    !
    if(.not.GL_DEP_PTS%EXISTS(VAR_THICK)) then
        MY_ERR%flag    = -1
        MY_ERR%source  = 'deposit_get_ptsdata'
        MY_ERR%message = 'neither load nor thinckess exist in the pts file'
        return
    end if
    !
    do ipts = 1,GL_DEP_PTS%npts
       GL_DEP_PTS%mass(ipts) = GL_DEP_PTS%thick(ipts)  ! deposit density of 1000 kg/m3 assumed
    end do
    !
    return
    !
    !*** List of errors
    !
101 MY_ERR%flag = -1
    close(90)
    MY_ERR%message = 'error opening the input file '//TRIM(fname)
    return
    !
  end subroutine deposit_get_ptsdata
  !
  !-----------------------------------------
  !    subroutine deposit_bcast_ptsdata
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts deposit data
  !
  subroutine deposit_bcast_ptsdata(GL_DEP_PTS,MY_ERR)
    implicit none
    !
    !>   @param GL_DEP_PTS   deposit points data
    !>   @param MY_ERR       error handler
    !
    type(DEP_PTS),      intent(INOUT) :: GL_DEP_PTS
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: npoin,i
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'deposit_bcast_ptsdata'
    MY_ERR%message = ' '
    !
    call parallel_bcast(GL_DEP_PTS%EXISTS,DICT_SIZE,0)
    call parallel_bcast(GL_DEP_PTS%tracer_type, 1,0)
    call parallel_bcast(GL_DEP_PTS%tracer_code, 1,0)
    call parallel_bcast(GL_DEP_PTS%npts,        1,0)
    !
    !*** Memory allocation
    !
    npoin = GL_DEP_PTS%npts
    if(.not. allocated(GL_DEP_PTS%label)) allocate(GL_DEP_PTS%label(npoin))
    if(.not. allocated(GL_DEP_PTS%lon  )) allocate(GL_DEP_PTS%lon  (npoin))
    if(.not. allocated(GL_DEP_PTS%lat  )) allocate(GL_DEP_PTS%lat  (npoin))
    if(.not. allocated(GL_DEP_PTS%mass )) allocate(GL_DEP_PTS%mass (npoin))
    if(.not. allocated(GL_DEP_PTS%thick)) allocate(GL_DEP_PTS%thick(npoin))
    !
    do i = 1,npoin
       call parallel_bcast(GL_DEP_PTS%label(i),1,0)
    end do
    call parallel_bcast(GL_DEP_PTS%lon,  npoin,0)
    call parallel_bcast(GL_DEP_PTS%lat,  npoin,0)
    call parallel_bcast(GL_DEP_PTS%mass, npoin,0)
    call parallel_bcast(GL_DEP_PTS%thick,npoin,0)
    !
    return
  end subroutine deposit_bcast_ptsdata
  !
  !
  !
  !        MODULE PRIVATE ROUTINES
  !
  !
  !
  !-----------------------------------------
  !    subroutine dep_set_dictionary
  !-----------------------------------------
  !
  subroutine dep_set_dictionary(MY_ERR)
    implicit none
    !
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'dep_set_dictionary'
    MY_ERR%message = ' '
    !
    !  Dafault data structure
    !
    DICTIONARY(DIM_LON   )  = 'lon'
    DICTIONARY(DIM_LAT   )  = 'lat'
    !
    DICTIONARY(VAR_LON   )  = 'longitude'
    DICTIONARY(VAR_LAT   )  = 'latitude'
    DICTIONARY(VAR_LOAD  )  = 'mass_loading'
    DICTIONARY(VAR_THICK )  = 'thickness'
    DICTIONARY(VAR_MASK  )  = 'loading_flag'
    !
    DICTIONARY(ATR_TRACER)  = 'unknown'
    !
    return
  end subroutine dep_set_dictionary
  !
  !-----------------------------------------
  !    subroutine dep_get_dictionary
  !-----------------------------------------
  !
  subroutine dep_get_dictionary( file_tbl_dep, MY_ERR )
    implicit none
    !
    character(len=s_file),intent(IN   ) :: file_tbl_dep
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
    MY_ERR%source  = 'dep_get_dictionary'
    MY_ERR%message = ' '
    !
    !*** Opens the file
    !
    open(90,FILE=TRIM(file_tbl_dep),STATUS='old',ERR=101)
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
    MY_ERR%message = 'error opening the input file '//TRIM(file_tbl_dep)
    return
    !
  end subroutine dep_get_dictionary
    !
    !
    !
END MODULE Deposit
