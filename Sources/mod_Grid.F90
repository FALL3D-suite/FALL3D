!***********************************************************************
!>
!> Module for procedures related to grid operations
!> @author
!> Arnau Folch
!>
!**********************************************************************
MODULE Grid
  use KindType
  use InpOut
  use Domain
  implicit none
  save
  !
  !    LIST OF PUBLIC VARIABLES
  !
  integer(ip), parameter :: MAP_H_CARTESIAN = 0
  integer(ip), parameter :: MAP_H_SPHERICAL = 1
  integer(ip), parameter :: MAP_H_POLAR     = 2
  integer(ip), parameter :: MAP_H_MERCATOR  = 3
  !
  integer(ip), parameter :: MAP_V_CARTESIAN               = 0
  integer(ip), parameter :: MAP_V_SIGMA_NO_DECAY          = 1
  integer(ip), parameter :: MAP_V_SIGMA_LINEAR_DECAY      = 2
  integer(ip), parameter :: MAP_V_SIGMA_EXPONENTIAL_DECAY = 3
  !
  logical               :: periodic(3) = .false.
  integer(ip)           :: np(3)
  integer(ip)           :: nb(3)
  real(rp), allocatable :: gl_sigma(:)
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: grid_build_arakawa_c
  PUBLIC :: grid_get_stagered_velocity
  PUBLIC :: grid_get_scaled_velocity
  PUBLIC :: grid_get_scaled_variables
  PUBLIC :: grid_get_time_step
  PUBLIC :: grid_p2c
  PUBLIC :: grid_p2c_2D
  PUBLIC :: grid_c2p
  PUBLIC :: grid_c2p_2D
  PUBLIC :: grid_get_shapez
  PUBLIC :: grid_get_mass_volume
  PUBLIC :: grid_get_mass_sink
  PUBLIC :: grid_get_mass_boundaries
  PUBLIC :: grid_read_inp_grid
  PUBLIC :: grid_bcast_inp_grid
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine grid_read_inp_grid
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads the grid block form the input file
  !
  subroutine grid_read_inp_grid(MY_FILES,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),     intent(INOUT) :: MY_FILES
    type(ARAKAWA_C_GRID),intent(INOUT) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: gl_nbz,nsig,k
    real(rp)              :: file_version
    real(rp)              :: resolution,lon_span,ds
    character(len=s_file) :: file_inp, word
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_read_inp_grid'
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
       MY_ERR%source  = 'grid_read_inp_grid'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version'
       return
    end if
    !
    !*** Reads GRID block
    !
    call inpout_get_cha (file_inp, 'GRID','HORIZONTAL_MAPPING', word, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    select case (word)
    case('CARTESIAN')
       MY_GRID%map_h = MAP_H_CARTESIAN
    case('SPHERICAL')
       MY_GRID%map_h = MAP_H_SPHERICAL
    case('POLAR')
       MY_GRID%map_h = MAP_H_POLAR
    case('MERCATOR')
       MY_GRID%map_h = MAP_H_MERCATOR
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Incorrect horizontal mapping. Options: CARTESIAN / SPHERICAL / POLAR / MERCATOR'
       return
    end select
    !
    call inpout_get_cha (file_inp, 'GRID','VERTICAL_MAPPING', word, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    select case (word)
    case('CARTESIAN')
       MY_GRID%map_v = MAP_V_CARTESIAN
    case('SIGMA_NO_DECAY')
       MY_GRID%map_v = MAP_V_SIGMA_NO_DECAY
    case('SIGMA_LINEAR_DECAY')
       MY_GRID%map_v = MAP_V_SIGMA_LINEAR_DECAY
    case('SIGMA_EXPONENTIAL_DECAY')
       MY_GRID%map_v = MAP_V_SIGMA_EXPONENTIAL_DECAY
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = &
            'Incorrect vertical mapping. Options: CARTESIAN / SIGMA_NO_DECAY / SIGMA_LINEAR_DECAY / SIGMA_EXPONENTIAL_DECAY '
       return
    end select
    !
    call inpout_get_rea (file_inp, 'GRID','LONMIN', MY_GRID%lonmin, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, 'GRID','LONMAX', MY_GRID%lonmax, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, 'GRID','LATMIN', MY_GRID%latmin, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, 'GRID','LATMAX', MY_GRID%latmax, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    call inpout_get_rea (file_inp, 'GRID','ZMAX_(M)', MY_GRID%X3max, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    word= ''
    call inpout_get_cha (file_inp, 'GRID','NX', word, 1, MY_ERR, .true.)
    if(word.eq.'RESOLUTION') then
       call inpout_get_rea (file_inp, 'GRID','NX', resolution, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       !
       !  handle all possible cases
       !
       if(MY_GRID%lonmin.lt.MY_GRID%lonmax) then
          lon_span = MY_GRID%lonmax-MY_GRID%lonmin
       else
          lon_span = 360.0_rp + MY_GRID%lonmax-MY_GRID%lonmin
       end if
       np(1) = nint(lon_span/resolution)
    else
       call inpout_get_int (file_inp, 'GRID','NX', np(1), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    end if
    !
    word= ''
    call inpout_get_cha (file_inp, 'GRID','NY', word, 1, MY_ERR, .true.)
    if(word.eq.'RESOLUTION') then
       call inpout_get_rea (file_inp, 'GRID','NY', resolution, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
       np(2) = nint((MY_GRID%latmax-MY_GRID%latmin)/resolution)
    else
       call inpout_get_int (file_inp, 'GRID','NY', np(2), 1, MY_ERR)
       if(MY_ERR%flag.ne.0) return
    end if
    !
    call inpout_get_int (file_inp, 'GRID','NZ', np(3), 1, MY_ERR)
    if(MY_ERR%flag.ne.0) return
    !
    !*** Number of global boundaries and global sigma values
    !
    nb(1) = np(1) + 1
    nb(2) = np(2) + 1
    nb(3) = np(3) + 1
    !
    gl_nbz = nb(3)
    allocate(gl_sigma(gl_nbz))
    !
    call inpout_get_npar(file_inp, 'GRID','SIGMA_VALUES',nsig,MY_ERR)
    !
    if((MY_ERR%flag.ne.0).or.(nsig.eq.0)) then
       MY_ERR%flag = 0
       !
       ! sigma values not given; homogeneous distribution assumed
       !
       ds = 1.0_rp /(gl_nbz-1)
       do k = 1,gl_nbz
          gl_sigma(k) = (k-1)*ds
       end do
       !
    else
       !
       ! (some) global sigma values given
       !
       call inpout_get_rea (file_inp, 'GRID','SIGMA_VALUES', gl_sigma, nsig, MY_ERR)
       !
       if(nsig.gt.gl_nbz) then
          MY_ERR%flag    = -1
          MY_ERR%message = 'Number of SIGMA_VALUES larger than number of grid cells (boundaries)'
          return
       else
          if(gl_sigma(1).ne.0.0_rp) then
             gl_sigma(1) = 0.0_rp
             call task_wriwarn(MY_ERR,'First SIGMA_VALUE automatically set to zero')
          end if
          !
          ds = (1.0_rp - gl_sigma(nsig))/(gl_nbz-nsig)
          do k = nsig+1,gl_nbz
             gl_sigma(k) = gl_sigma(k-1) + ds
          end do
       end if
    end if
    !
    !*** Additional computations
    !
    if( (MY_GRID%lonmin.eq.-180.0_rp).and. &
         (MY_GRID%lonmax.eq. 180.0_rp) ) periodic(1) = .true.
    !
    return
  end subroutine grid_read_inp_grid
  !
  !-----------------------------------------
  !    subroutine grid_bcast_inp_grid
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts grid block from input file
  !
  subroutine grid_bcast_inp_grid (MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param MY_GRID   grid configuration parameters
    !>   @param MY_ERR    error handler
    !
    type(ARAKAWA_C_GRID),intent(INOUT) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_bcast_inp_grid'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_GRID%map_h    ,1,0)
    call parallel_bcast(MY_GRID%map_v    ,1,0)
    call parallel_bcast(MY_GRID%map_h    ,1,0)
    call parallel_bcast(MY_GRID%lonmin   ,1,0)
    call parallel_bcast(MY_GRID%lonmax   ,1,0)
    call parallel_bcast(MY_GRID%latmin   ,1,0)
    call parallel_bcast(MY_GRID%latmax   ,1,0)
    call parallel_bcast(MY_GRID%X3max    ,1,0)
    call parallel_bcast(np               ,3,0)
    call parallel_bcast(nb               ,3,0)
    call parallel_bcast(periodic         ,3,0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       allocate(gl_sigma(nb(3)))
    end if
    !
    call parallel_bcast(gl_sigma,nb(3),0)
    !
    return
  end subroutine grid_bcast_inp_grid
  !
  !-----------------------------------
  !    subroutine grid_build_arakawa_c
  !-----------------------------------
  !
  !>   @brief
  !>   Builds an Arakawa-C grid.
  !>   @details
  !>   The call is mandatory and done in 2 steps before and after reading topography
  !
  subroutine grid_build_arakawa_c(itask,map_h,map_v,lonmin,lonmax,latmin,latmax,X3max,gl_sigma,my_hc,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param itask    task flag: if 0 computes variables not needing topography in MY_GRID; if 1 computes variables needing topography in MY_GRID
    !>   @param map_h    horizonatal mapping flag: CARTESIAN = 0, SPHERICAL = 1, POLAR = 2
    !>   @param map_v    vertical mapping flag: SIGMA_NO_DECAY = 0, SIGMA_LINEAR_DECAY = 1, SIGMA_EXPONENTIAL_DECAY = 2
    !>   @param lonmin   longitude of the grid W side in the range (-180,180)
    !>   @param lonmax   longitude of the grid E side in the range (-180,180)
    !>   @param latmin   latitude  of the grid S side in the range ( -90,90 )
    !>   @param latmax   latitude  of the grid N side in the range ( -90,90 )
    !>   @param X3max    top of the computational domain in m. Spans in vertical form (0,X3max)
    !>   @param gl_sigma global values of sigma coordinate (0,1) where sigma = X3/X3max
    !>   @param my_hc    values of topography at my processor cell corners
    !>   @param MY_GRID  grid structure to be filled in this subroutine
    !>   @param MY_ERR   error handler
    !
    integer(ip),         intent(IN)    :: itask
    integer(ip),         intent(IN)    :: map_h
    integer(ip),         intent(IN)    :: map_v
    real(rp),            intent(IN)    :: lonmin
    real(rp),            intent(IN)    :: lonmax
    real(rp),            intent(IN)    :: latmin
    real(rp),            intent(IN)    :: latmax
    real(rp),            intent(IN)    :: X3max
    real(rp),            intent(IN)    :: gl_sigma(1:gl_nbz)
    real(rp),            intent(IN)    :: my_hc   (my_ibs:my_ibe,my_jbs:my_jbe)
    type(ARAKAWA_C_GRID),intent(INOUT) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k
    real(rp)    :: dlon,dlat,lon_c,lat_c,X3_c,colat
    real(rp)    :: h_p,h, dX3_b, dX3_p, X3_p, X3_p1
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_build_arakawa_c'
    MY_ERR%message = ' '
    !
    !*** Task selection 0
    !
    select case(itask)
    case(0_ip)
       !
       !*** Global grid limits and grid resolution along each direction
       !
       select case(map_h)
       case(MAP_H_SPHERICAL)
          !
          !  handle cases passing through meridian day
          !
          if(lonmax.gt.lonmin) then
             dlon = (lonmax-lonmin)/(gl_nbx-1)
          else
             dlon = (360.0_rp+lonmax-lonmin)/(gl_nbx-1)
          end if
       case default
          dlon = (lonmax-lonmin)/(gl_nbx-1)
       end select
       !
       dlat = (latmax-latmin)/(gl_nby-1)
       !
       MY_GRID%map_h  = map_h
       MY_GRID%map_v  = map_v
       MY_GRID%lonmin = lonmin
       MY_GRID%lonmax = lonmax
       MY_GRID%latmin = latmin
       MY_GRID%latmax = latmax
       MY_GRID%X3max  = X3max
       MY_GRID%dlon   = dlon
       MY_GRID%dlat   = dlat
       !
       allocate(MY_GRID%gl_sigma(gl_nbz))
       MY_GRID%gl_sigma(:) = gl_sigma(:)
       !
       !*** Coordinates of cell corners MY_GRID%lon_c MY_GRID%lat_c MY_GRID%X3_c (no halo)
       !
       allocate(MY_GRID%lon_c(my_ibs:my_ibe))
       do i = 1,gl_nbx   ! global span
          lon_c = lonmin + (i-1)*dlon
          if((i.ge.my_ibs).and.(i.le.my_ibe)) MY_GRID%lon_c(i) = lon_c
       end do
       !
       allocate(MY_GRID%lat_c(my_jbs:my_jbe))
       do j = 1,gl_nby   ! global span
          lat_c = latmin + (j-1)*dlat
          if((j.ge.my_jbs).and.(j.le.my_jbe)) MY_GRID%lat_c(j) = lat_c
       end do
       !
       allocate(MY_GRID%X3_c(my_kbs:my_kbe))
       do k = 1,gl_nbz   ! global span
          X3_c = gl_sigma(k)*MY_GRID%X3max
          if((k.ge.my_kbs).and.(k.le.my_kbe)) MY_GRID%X3_c(k) = X3_c
       end do
       !
       !*** Coordinates of mass points MY_GRID%lon_p MY_GRID%lat_p  MY_GRID%X3_p (no halo)
       !
       allocate(MY_GRID%lon_p(my_ips:my_ipe))
       do i = my_ips,my_ipe
          MY_GRID%lon_p(i) = 0.5_rp*(MY_GRID%lon_c(i)+MY_GRID%lon_c(i+1))
       end do
       !
       allocate(MY_GRID%lat_p(my_jps:my_jpe))
       do j = my_jps,my_jpe
          MY_GRID%lat_p(j) = 0.5_rp*(MY_GRID%lat_c(j)+MY_GRID%lat_c(j+1))
       end do
       !
       allocate(MY_GRID%X3_p(my_kps:my_kpe))
       do k = my_kps,my_kpe
          MY_GRID%X3_p(k) = 0.5_rp*(MY_GRID%X3_c(k)+MY_GRID%X3_c(k+1))
       end do
       !
       !*** Scale factor at mass points MY_GRID%Hm1_p (no halo)
       !
       allocate(MY_GRID%Hm1_p(my_jps:my_jpe))
       select case(map_h)
       case(MAP_H_CARTESIAN)
          MY_GRID%Hm1_p(:) = 1.0_rp
          !
       case(MAP_H_SPHERICAL)
          do j = my_jps,my_jpe
             colat            = (90.0_rp- MY_GRID%lat_p(j))*PI/180.0_rp   ! colatitude in rad
             MY_GRID%Hm1_p(j) = sin(colat)
          end do
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case(MAP_H_MERCATOR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          return
          !
       end select
       !
       !*** Scale factor at corner points MY_GRID%Hm1_c (no halo)
       !
       allocate(MY_GRID%Hm1_c(my_jbs:my_jbe))
       select case(map_h)
       case(MAP_H_CARTESIAN)
          MY_GRID%Hm1_c(:) = 1.0_rp
          !
       case(MAP_H_SPHERICAL)
          do j = my_jbs,my_jbe
             colat            = (90.0_rp- MY_GRID%lat_c(j))*PI/180.0_rp   ! colatitude in rad
             MY_GRID%Hm1_c(j) = sin(colat)
          end do
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case(MAP_H_MERCATOR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          return
          !
       end select
       !
       !*** Scale factor at mass points MY_GRID%Hm2_p (2 halo)
       !
       allocate(MY_GRID%Hm2_p(my_jps_2h:my_jpe_2h))
       select case(map_h)
       case(MAP_H_CARTESIAN)
          MY_GRID%Hm2_p(:) = 1.0_rp
          !
       case(MAP_H_SPHERICAL)
          MY_GRID%Hm2_p(:) = 1.0_rp
          !
       case(MAP_H_POLAR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case(MAP_H_MERCATOR)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          return
          !
       end select
       !
       !*** dX at mass and boundary points MY_GRID%dX1_p MY_GRID%dX1_b  (with halo and no scaling factor)
       !
       allocate(MY_GRID%dX1_p(my_ips_2h:my_ipe_2h))
       allocate(MY_GRID%dX1_b(my_ibs_1h:my_ibe_1h))
       !
       select case(map_h)
       case(MAP_H_CARTESIAN)
          MY_GRID%dX1_p(:) = MY_GRID%dlon
          MY_GRID%dX1_b(:) = MY_GRID%dlon
          !
       case(MAP_H_SPHERICAL)
          MY_GRID%dX1_p(:) = REARTH*MY_GRID%dlon*PI/180.0_rp
          MY_GRID%dX1_b(:) = REARTH*MY_GRID%dlon*PI/180.0_rp
          !
       case(MAP_H_POLAR)
          MY_GRID%dX1_p(:) = MY_GRID%dlon
          MY_GRID%dX1_b(:) = MY_GRID%dlon
          !
       case(MAP_H_MERCATOR)
          MY_GRID%dX1_p(:) = MY_GRID%dlon
          MY_GRID%dX1_b(:) = MY_GRID%dlon
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          return
          !
       end select
       !
       !*** dY at mass and boundary points MY_GRID%dX2_p MY_GRID%dX2_b  (with halo and no scaling factor)
       !
       allocate(MY_GRID%dX2_p(my_jps_2h:my_jpe_2h))
       allocate(MY_GRID%dX2_b(my_jbs_1h:my_jbe_1h))
       !
       select case(map_h)
       case(MAP_H_CARTESIAN)
          MY_GRID%dX2_p(:) = MY_GRID%dlat
          MY_GRID%dX2_b(:) = MY_GRID%dlat
          !
       case(MAP_H_SPHERICAL)
          MY_GRID%dX2_p(:) = REARTH*MY_GRID%dlat*PI/180.0_rp
          MY_GRID%dX2_b(:) = REARTH*MY_GRID%dlat*PI/180.0_rp
          !
       case(MAP_H_POLAR)
          MY_GRID%dX2_p(:) = MY_GRID%dlat
          MY_GRID%dX2_b(:) = MY_GRID%dlat
          !
       case(MAP_H_MERCATOR)
          MY_GRID%dX2_p(:) = MY_GRID%dlat
          MY_GRID%dX2_b(:) = MY_GRID%dlat
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect horizontal mapping'
          return
          !
       end select
       !
       !*** dX3 at mass points and at boundary points (i.e. cell size) MY_GRID%dX3_p MY_GRID%dX3_b  (with halo and no scaling factor)
       !
       allocate(MY_GRID%dX3_b(my_kbs_1h:my_kbe_1h))
       allocate(MY_GRID%dX3_p(my_kps_2h:my_kpe_2h))
       !
       do k = 1,gl_nbz-1   ! global span
          dX3_b = (gl_sigma(k+1)-gl_sigma(k))*MY_GRID%X3max
          if((k.ge.my_kbs_1h).and.(k.le.my_kbe_1h)) MY_GRID%dX3_b(k) = dX3_b
       end do
       if(my_kbs_1h.lt.1     ) MY_GRID%dX3_b(my_kbs_1h          ) = MY_GRID%dX3_b(my_kbs  )
       if(my_kbe_1h.gt.gl_nbz) MY_GRID%dX3_b(my_kbe   :my_kbe_1h) = MY_GRID%dX3_b(my_kbe-1)
       !
       do k = 1,gl_nbz-2   ! global span
          X3_p  = 0.5_rp*(gl_sigma(k+1)+gl_sigma(k  ))*MY_GRID%X3max
          X3_p1 = 0.5_rp*(gl_sigma(k+2)+gl_sigma(k+1))*MY_GRID%X3max
          dX3_p = X3_p1 - X3_p
          if((k.ge.my_kps_2h).and.(k.le.my_kpe_2h)) MY_GRID%dX3_p(k) = dX3_p
       end do
       if(my_kps_2h.lt.1     ) MY_GRID%dX3_p(my_kps_2h:my_kps-1 ) = MY_GRID%dX3_p(my_kps  )
       if(my_kpe_2h.gt.gl_nbz) MY_GRID%dX3_p(my_kpe   :my_kpe_2h) = MY_GRID%dX3_p(my_kpe-1)
       !
       !*** If necessary, correct longitudes to be in the range (-180,180)
       !
       select case(map_h)
       case(MAP_H_SPHERICAL)
          do i = my_ibs,my_ibe
             if(MY_GRID%lon_c(i).gt.180.0_rp) MY_GRID%lon_c(i) = MY_GRID%lon_c(i) - 360.0_rp
          end do
          do i = my_ips,my_ipe
             if(MY_GRID%lon_p(i).gt.180.0_rp) MY_GRID%lon_p(i) = MY_GRID%lon_p(i) - 360.0_rp
          end do
       case default
          continue
       end select
       !
       !*** Task selection 1
       !
    case(1_ip)
       !
       !*** Topography at cell corners MY_GRID%h_c(my_ibs:my_ibe,my_jbs:my_jbe) (no halo)
       !
       allocate(MY_GRID%h_c(my_ibs:my_ibe,my_jbs:my_jbe))
       MY_GRID%h_c(my_ibs:my_ibe,my_jbs:my_jbe) = my_hc(my_ibs:my_ibe,my_jbs:my_jbe)
       !
       !*** Topography gradient at mass points MY_GRID%dhdx_p MY_GRID%dhdy_p (no halo)
       !
       allocate(MY_GRID%dhdx_p(my_ips:my_ipe,my_jps:my_jpe))
       allocate(MY_GRID%dhdy_p(my_ips:my_ipe,my_jps:my_jpe))
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             MY_GRID%dhdx_p(i,j) = 0.5_rp*( my_hc(i+1,j)+my_hc(i+1,j+1)-my_hc(i,j)-my_hc(i,j+1))/MY_GRID%dX1_b(i)
             MY_GRID%dhdy_p(i,j) = 0.5_rp*(-my_hc(i+1,j)+my_hc(i+1,j+1)-my_hc(i,j)+my_hc(i,j+1))/MY_GRID%dX2_b(j)
          end do
       end do
       !
       !*** Scale factor at mass points MY_GRID%Hm3_p (no halo)
       !
       allocate(MY_GRID%Hm3_p(my_ips:my_ipe,my_jps:my_jpe))
       select case(map_v)
       case(MAP_V_CARTESIAN)
          MY_GRID%Hm3_p(:,:) = 1.0_rp
          !
       case(MAP_V_SIGMA_NO_DECAY)
          MY_GRID%Hm3_p(:,:) = 1.0_rp
          !
       case(MAP_V_SIGMA_LINEAR_DECAY)
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                h_p = 0.25_rp*(my_hc(i,j)+my_hc(i+1,j)+my_hc(i+1,j+1)+my_hc(i,j+1))  ! topo at mass points
                MY_GRID%Hm3_p(i,j) = (X3max-h_p)/X3max
             end do
          end do
          !
       case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect vertical mapping'
          return
          !
       end select
       !
       !***  z-coordinate (a.s.l) at cell corners MY_GRID%z_c (no halo)
       !
       allocate(MY_GRID%z_c(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe))
       select case(map_v)
       case(MAP_V_CARTESIAN)
          do j = my_jbs,my_jbe
             do i = my_ibs,my_ibe
                do k = my_kbs,my_kbe
                   MY_GRID%z_c(i,j,k) = MY_GRID%X3_c(k)
                end do
             end do
          end do
          !
       case(MAP_V_SIGMA_NO_DECAY)
          do j = my_jbs,my_jbe
             do i = my_ibs,my_ibe
                h = MY_GRID%h_c(i,j)
                do k = my_kbs,my_kbe
                   MY_GRID%z_c(i,j,k) = MY_GRID%X3_c(k) + h
                end do
             end do
          end do
          !
       case(MAP_V_SIGMA_LINEAR_DECAY)
          do j = my_jbs,my_jbe
             do i = my_ibs,my_ibe
                h = MY_GRID%h_c(i,j)
                do k = my_kbs,my_kbe
                   MY_GRID%z_c(i,j,k) = MY_GRID%X3_c(k)*(X3max-h)/X3max + h
                end do
             end do
          end do
          !
       case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
          MY_ERR%flag    = 1
          MY_ERR%message = 'Mapping not implemented yet'
          return
          !
       case default
          MY_ERR%flag    = 1
          MY_ERR%message = 'Incorrect vertical mapping'
          return
          !
       end select
       !
    end select
    !
    return
  end subroutine grid_build_arakawa_c
  !
  !------------------------------------------
  !    subroutine grid_get_stagered_velocity
  !------------------------------------------
  !
  !>   @brief
  !>   Computes Arakawa-C (staggered) velocity at boundaries from values at cell corners
  !
  subroutine grid_get_stagered_velocity(my_uc, my_vc, my_wc, my_u, my_v, my_w, MY_ERR)
    implicit none
    !
    !>   @param my_uc   u-wind velocity at my processor cell cell corners
    !>   @param my_vc   v-wind velocity at my processor cell cell corners
    !>   @param my_wc   w-wind velocity at my processor cell cell corners
    !>   @param my_u    u-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param my_v    v-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param my_w    w-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param MY_ERR  error handler
    !
    real(rp),            intent(IN   ) :: my_uc   (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),            intent(IN   ) :: my_vc   (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),            intent(IN   ) :: my_wc   (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),            intent(INOUT) :: my_u    (my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(INOUT) :: my_v    (my_ips   :my_ipe   ,my_jbs_1h:my_jbe_1h,my_kps   :my_kpe   )
    real(rp),            intent(INOUT) :: my_w    (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h)
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_get_staggered_variables'
    MY_ERR%message = ' '
    !
    !*** Compute u-component at cell boundaries (staggered) from values at corners
    !
    do k = my_kps,my_kpe
       do j = my_jps,my_jpe
          do i = my_ibs,my_ibe
             my_u(i,j,k) = 0.25_rp*( my_uc(i,j,k) + my_uc(i,j+1,k) + my_uc(i,j,k+1) + my_uc(i,j+1,k+1) )
          end do
       end do
    end do
    my_u(my_ibs_1h,:,:) = my_u(my_ibs,:,:)
    my_u(my_ibe_1h,:,:) = my_u(my_ibe,:,:)
    call domain_swap_velo_points_1halo_x( my_u )
    !
    !*** Compute v-component at cell boundaries (staggered) from values at corners
    !
    do k = my_kps,my_kpe
       do i = my_ips,my_ipe
          do j = my_jbs,my_jbe
             my_v(i,j,k) = 0.25_rp*( my_vc(i,j,k) + my_vc(i+1,j,k) + my_vc(i,j,k+1) + my_vc(i+1,j,k+1) )
          end do
       end do
    end do
    my_v(:,my_jbs_1h,:) = my_v(:,my_jbs,:)
    my_v(:,my_jbe_1h,:) = my_v(:,my_jbe,:)
    call domain_swap_velo_points_1halo_y( my_v )
    !
    !*** Compute w-component at cell boundaries (staggered) from values at corners
    !
    do j = my_jps,my_jpe
       do i = my_ips,my_ipe
          do k = my_kbs,my_kbe
             my_w(i,j,k) = 0.25_rp*( my_wc(i,j,k) + my_wc(i+1,j,k) + my_wc(i,j+1,k) + my_wc(i+1,j+1,k) )
          end do
       end do
    end do
    my_w(:,:,my_kbs_1h) = my_w(:,:,my_kbs)
    my_w(:,:,my_kbe_1h) = my_w(:,:,my_kbe)
    call domain_swap_velo_points_1halo_z( my_w )
    !
    return
  end subroutine grid_get_stagered_velocity
  !
  !------------------------------------------
  !    subroutine grid_get_scaled_velocity
  !------------------------------------------
  !
  !>   @brief
  !>   Computes Arakawa-C (staggered) scaled velocity
  !
  subroutine grid_get_scaled_velocity(my_uc,my_vc,my_u,my_v,my_w,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param my_uc  u-wind velocity at my processor cell cell corners
    !>   @param my_vc  v-wind velocity at my processor cell cell corners
    !>   @param my_u   u-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param my_v   v-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param my_w   w-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param MY_GRID grid structure already filled by subroutine grid_build_arakawa_c
    !>   @param MY_ERR  error handler
    !
    real(rp),            intent(IN   ) :: my_uc (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),            intent(IN   ) :: my_vc (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),            intent(INOUT) :: my_u  (my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(INOUT) :: my_v  (my_ips   :my_ipe   ,my_jbs_1h:my_jbe_1h,my_kps   :my_kpe   )
    real(rp),            intent(INOUT) :: my_w  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h)
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k
    real(rp)    :: Hm1,Hm2,Hm3,hx,hy,hz,uw,vw
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_get_scaled_velocity'
    MY_ERR%message = ' '
    !
    !*** Scale u-component
    !
    do k = my_kps,my_kpe
       do j = my_jps,my_jpe
          Hm1 = MY_GRID%Hm1_p(j)
          do i = my_ibs_1h,my_ibe_1h
             my_u(i,j,k) = my_u(i,j,k)/Hm1
          end do
       end do
    end do
    !
    !*** Scale v-component
    !
    do k = my_kps,my_kpe
       do j = my_jbs_1h,my_jbe_1h
          Hm2 = 0.5_rp*(MY_GRID%Hm2_p(j)+MY_GRID%Hm2_p(j-1))  ! Hm2 at v point
          do i = my_ips,my_ipe
             my_v(i,j,k) = my_v(i,j,k)/Hm2
          end do
       end do
    end do
    !
    !*** Scale w-component (topography correction)
    !
    select case(MY_GRID%map_v)
    case(MAP_V_CARTESIAN)
       !
       continue  ! no need to scale
       !
    case(MAP_V_SIGMA_NO_DECAY)
       !
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             Hm3 = MY_GRID%Hm3_p (i,j)
             hx  = MY_GRID%dhdx_p(i,j)
             hy  = MY_GRID%dhdy_p(i,j)
             do k = my_kbs,my_kbe
                uw  = 0.25_rp*( my_uc(i,j,k) + my_uc(i+1,j,k) + my_uc(i,j+1,k) + my_uc(i+1,j+1,k)  )   ! u at w point
                vw  = 0.25_rp*( my_vc(i,j,k) + my_vc(i+1,j,k) + my_vc(i,j+1,k) + my_vc(i+1,j+1,k)  )   ! v at w point
                !
                my_w(i,j,k) = (my_w(i,j,k) - hx*uw - hy*vw)/Hm3
             end do
          end do
       end do
       my_w(:,:,my_kbs_1h) = my_w(:,:,my_kbs)
       my_w(:,:,my_kbe_1h) = my_w(:,:,my_kbe)
       call domain_swap_velo_points_1halo_z( my_w )
       !
    case(MAP_V_SIGMA_LINEAR_DECAY)
       !
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             Hm3 = MY_GRID%Hm3_p (i,j)
             do k = my_kbs,my_kbe
                hz = (1.0_rp - MY_GRID%X3_c(k)/MY_GRID%X3max)
                hx  = MY_GRID%dhdx_p(i,j) * hz
                hy  = MY_GRID%dhdy_p(i,j) * hz
                uw  = 0.25_rp*( my_uc(i,j,k) + my_uc(i+1,j,k) + my_uc(i,j+1,k) + my_uc(i+1,j+1,k)  )   ! u at w point
                vw  = 0.25_rp*( my_vc(i,j,k) + my_vc(i+1,j,k) + my_vc(i,j+1,k) + my_vc(i+1,j+1,k)  )   ! v at w point
                !
                my_w(i,j,k) = (my_w(i,j,k) - hx*uw - hy*vw)/Hm3
             end do
          end do
       end do
       my_w(:,:,my_kbs_1h) = my_w(:,:,my_kbs)
       my_w(:,:,my_kbe_1h) = my_w(:,:,my_kbe)
       call domain_swap_velo_points_1halo_z( my_w )
       !
    case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
       MY_ERR%flag    = 1
       MY_ERR%message = 'Mapping not implemented yet'
       return
       !
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Incorrect vertical mapping'
       return
       !
    end select
    !
    return
  end subroutine grid_get_scaled_velocity
  !
  !------------------------------------------
  !    subroutine grid_get_scaled_variables
  !------------------------------------------
  !
  !>   @brief
  !>   Computes Arakawa-C (staggered) scaled variables (different from wind velocity)
  !
  subroutine grid_get_scaled_variables(my_k1,my_k2,my_k3,my_rho,my_vs,nbins,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param my_k1   x-diffusion at mass points
    !>   @param my_k2   y-diffusion at mass points
    !>   @param my_k3   z-diffusion at mass points
    !>   @param my_rho  air density at mass points
    !>   @param my_vs   settling velocity at w-boundaries
    !>   @param nbins   number of bins
    !>   @param MY_GRID grid structure already filled by subroutine grid_build_arakawa_c
    !>   @param MY_ERR  error handler
    !
    integer(ip),         intent(IN   ) :: nbins
    real(rp),            intent(INOUT) :: my_k1 (my_ips_2h:my_ipe_2h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(INOUT) :: my_k2 (my_ips   :my_ipe   ,my_jps_2h:my_jpe_2h,my_kps   :my_kpe   )
    real(rp),            intent(INOUT) :: my_k3 (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps_2h:my_kpe_2h)
    real(rp),            intent(INOUT) :: my_rho(my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(INOUT) :: my_vs (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h, 1:nbins)
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k,ibin,kk
    real(rp)    :: Hm1,Hm2,Hm3,hx,hy,hz
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_get_scaled_variables'
    MY_ERR%message = ' '
    !
    !*** Scale k3 (mass points)
    !
    select case(MY_GRID%map_v)
    case(MAP_V_CARTESIAN)
       !
       continue  ! no need to scale
       !
    case(MAP_V_SIGMA_NO_DECAY)
       !
       do k = my_kps,my_kpe
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                Hm3 = MY_GRID%Hm3_p (i,j)
                hx  = MY_GRID%dhdx_p(i,j)
                hy  = MY_GRID%dhdy_p(i,j)
                !
                my_k3(i,j,k) = (my_k3(i,j,k) + hx*hx*my_k1(i,j,k) + hy*hy*my_k2(i,j,k))/(Hm3*Hm3)
             end do
          end do
       end do
       !
    case(MAP_V_SIGMA_LINEAR_DECAY)
       !
       do k = my_kps,my_kpe
          hz = (1.0_rp - MY_GRID%X3_p(k)/MY_GRID%X3max)
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                Hm3 = MY_GRID%Hm3_p (i,j)
                hx  = MY_GRID%dhdx_p(i,j)* hz
                hy  = MY_GRID%dhdy_p(i,j)* hz
                !
                my_k3(i,j,k) = (my_k3(i,j,k) + hx*hx*my_k1(i,j,k) + hy*hy*my_k2(i,j,k))/(Hm3*Hm3)
             end do
          end do
       end do
       !
    case(MAP_V_SIGMA_EXPONENTIAL_DECAY)
       MY_ERR%flag    = 1
       MY_ERR%message = 'Mapping not implemented yet'
       return
       !
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Incorrect vertical mapping'
       return
       !
    end select
    !
    my_k3(:,:,my_kps_2h  ) = my_k3(:,:,my_kps)
    my_k3(:,:,my_kps_2h+1) = my_k3(:,:,my_kps)
    my_k3(:,:,my_kpe_2h  ) = my_k3(:,:,my_kpe)
    my_k3(:,:,my_kpe_2h-1) = my_k3(:,:,my_kpe)
    !
    do j = my_jps,my_jpe
       do i = my_ips,my_ipe
          call domain_swap_mass_points_2halo_1dz(my_k3(i,j,:))
       end do
    end do
    !
    !*** Scale k1 (mass points)
    !
    do k = my_kps,my_kpe
       do j = my_jps,my_jpe
          Hm1 = MY_GRID%Hm1_p(j)
          do i = my_ips_2h,my_ipe_2h
             my_k1(i,j,k) = my_k1(i,j,k)/(Hm1*Hm1)
          end do
       end do
    end do
    !
    !*** Scale k2 (mass points)
    !
    do k = my_kps,my_kpe
       do j = my_jps_2h,my_jpe_2h
          Hm2 = MY_GRID%Hm2_p(j)
          do i = my_ips,my_ipe
             my_k2(i,j,k) = my_k2(i,j,k)/(Hm2*Hm2)
          end do
       end do
    end do
    !
    !*** Scale rho
    !
    do k = my_kps,my_kpe
       do j = my_jps,my_jpe
          Hm1 = MY_GRID%Hm1_p(j)
          Hm2 = MY_GRID%Hm2_p(j)
          do i = my_ips,my_ipe
             Hm3 = MY_GRID%Hm3_p(i,j)
             my_rho(i,j,k) = my_rho(i,j,k)*(Hm1*Hm2*Hm3)
          end do
       end do
    end do
    !
    !*** Scale settling velocity
    !
    do ibin = 1,nbins
       do k = my_kbs_1h,my_kbe_1h
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                Hm3 = MY_GRID%Hm3_p(i,j)
                my_vs(i,j,k,ibin) = my_vs(i,j,k,ibin)/Hm3
             end do
          end do
       end do
    end do
    !
    return
  end subroutine grid_get_scaled_variables
  !
  !---------------------------------
  !    subroutine grid_get_time_step
  !---------------------------------
  !
  !>   @brief
  !>   Gets the critical time integration step
  !
  subroutine grid_get_time_step(my_dt,dt,my_u,my_v,my_w,my_k1,my_k2,my_k3,MY_GRID,MY_MOD,MY_ERR)
    implicit none
    !
    !>   @param my_dt   my processor (local) time step
    !>   @param dt      global critical time step (minimum among all processors)
    !>   @param my_u    u-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param my_v    v-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param my_w    w-wind velocity at my processor cell cell boundaries (with 1 halo)
    !>   @param my_k1   k1 (horizontal) diffusivity at my processor mass points (with 2 halo)
    !>   @param my_k2   k2 (horizontal) diffusivity at my processor mass points (with 2 halo)
    !>   @param my_k3   k3 (vertical)   diffusivity at my processor mass points (with 2 halo)
    !>   @param MY_GRID grid structure already filled by subroutine grid_build_arakawa_c
    !>   @param MY_MOD  model physics related parameters
    !>   @param MY_ERR  error handler
    !
    real(rp),            intent(INOUT) :: my_dt
    real(rp),            intent(INOUT) :: dt
    real(rp),            intent(IN)    :: my_u  (my_ibs_1h:my_ibe_1h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(IN)    :: my_v  (my_ips   :my_ipe   ,my_jbs_1h:my_jbe_1h,my_kps   :my_kpe   )
    real(rp),            intent(IN)    :: my_w  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h)
    real(rp),            intent(IN)    :: my_k1 (my_ips_2h:my_ipe_2h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),            intent(IN)    :: my_k2 (my_ips   :my_ipe   ,my_jps_2h:my_jpe_2h,my_kps   :my_kpe   )
    real(rp),            intent(IN)    :: my_k3 (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps_2h:my_kpe_2h)
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(MODEL_PHYS),    intent(IN   ) :: MY_MOD
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: i,j,k
    real(rp)              :: dtinv,dt1,dt2,dt3,dx,dy,dz
    real(rp), allocatable :: dt_local(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_get_time_step'
    MY_ERR%message = ' '
    !
    select case(MY_MOD%CFL_criterion)
    case(1)
       !
       !  MY_MOD%CFL_criterion =  1  (minimum of all dimensions)
       !
       dt1 = 0.0_rp
       do i = my_ips,my_ipe
          dx = MY_GRID%dX1_p(i)
          do k = my_kps,my_kpe
             do j = my_jps,my_jpe
                dtinv = 2.0_rp*my_k1(i,j,k)/(dx*dx)+abs(my_u(i,j,k))/dx
                dt1   = max(dt1,dtinv)
             end do
          end do
       end do
       !
       dt2 = 0.0_rp
       do j = my_jps,my_jpe
          dy = MY_GRID%dX2_p(j)
          do k = my_kps,my_kpe
             do i = my_ips,my_ipe
                dtinv = 2.0_rp*my_k2(i,j,k)/(dy*dy)+abs(my_v(i,j,k))/dy
                dt2   = max(dt2,dtinv)
             end do
          end do
       end do
       !
       dt3 = 0.0_rp
       do k = my_kps,my_kpe
          dz = MY_GRID%dX3_p(k)
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                dtinv = 2.0_rp*my_k3(i,j,k)/(dz*dz)+abs(my_w(i,j,k))/dz
                dt3   = max(dt3,dtinv)
             end do
          end do
       end do
       !
    case(2)
       !
       !  MY_MOD%CFL_criterion =  2  (all dimensions at the same time)
       !
       dt1 = 0.0_rp
       dt2 = 0.0_rp
       dt3 = 0.0_rp
       !
       do k = my_kps,my_kpe
          dz = MY_GRID%dX3_p(k)
          do j = my_jps,my_jpe
             dy = MY_GRID%dX2_p(j)
             do i = my_ips,my_ipe
                dx = MY_GRID%dX1_p(i)
                !
                dtinv = 2.0_rp*my_k1(i,j,k)/(dx*dx)+abs(my_u(i,j,k))/dx + &
                     2.0_rp*my_k2(i,j,k)/(dy*dy)+abs(my_v(i,j,k))/dy + &
                     2.0_rp*my_k3(i,j,k)/(dz*dz)+abs(my_w(i,j,k))/dz
                dt1   = max(dt1,dtinv)
             end do
          end do
       end do
       !
    end select
    !
    !*** Local time step
    !
    my_dt = max(dt1,dt2,dt3)
    if(my_dt.gt.0.0_rp) my_dt = 1.0_rp/my_dt
    my_dt = MY_MOD%CFL_safety_factor*my_dt
    !
    !*** Golbal time step
    !
    allocate(dt_local(0:npes_model-1))
    dt_local(:)          = 0.0_rp
    dt_local(mype_model) = my_dt
    call parallel_sum(dt_local, COMM_MODEL)
    dt = minval(dt_local(:))
    !
    return
  end subroutine grid_get_time_step
  !
  !-----------------------------
  !    subroutine grid_p2c
  !-----------------------------
  !
  !>   @brief
  !>   Converts an array from mass points to corner points (used only for postprocess purposes)
  !
  subroutine grid_p2c(my_c,my_cc)
    implicit none
    !
    !>   @param my_c  values at my processor mass points (with 2 halo)
    !>   @param my_cc values at my processor corner points
    !
    real(rp), intent(INOUT) :: my_c (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h)
    real(rp), intent(INOUT) :: my_cc(my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    !
    integer(ip) :: i,j,k
    !
    !*** First, ensure proper swap of halos, including corners
    !
    call domain_swap_mass_points_2halo_x (my_c)
    call domain_swap_mass_points_2halo_y (my_c)
    call domain_swap_mass_points_2halo_z (my_c)
    !
    call domain_swap_mass_points_2halo_corners_x (my_c)
    call domain_swap_mass_points_2halo_corners_y (my_c)
    call domain_swap_mass_points_2halo_corners_z (my_c)
    !
    do k = my_kbs,my_kbe
       do j = my_jbs,my_jbe
          do i = my_ibs,my_ibe
             my_cc(i,j,k) = (my_c(i,j,k  ) + my_c(i-1,j,k  ) + my_c(i,j-1,k  ) + my_c(i-1,j-1,k  ) + &
                  my_c(i,j,k-1) + my_c(i-1,j,k-1) + my_c(i,j-1,k-1) + my_c(i-1,j-1,k-1))/8.0_rp
          end do
       end do
    end do
    !
    return
  end subroutine grid_p2c
  !
  !-----------------------------
  !    subroutine grid_p2c_2D
  !-----------------------------
  !
  !>   @brief
  !>   Converts an array from mass points to corner points (used only for postprocess purposes)
  !
  subroutine grid_p2c_2D(my_c,my_cc)
    implicit none
    !
    !>   @param my_c  values at my processor mass points (with 2 halo)
    !>   @param my_cc values at my processor corner points
    !
    real(rp), intent(INOUT) :: my_c (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h)
    real(rp), intent(INOUT) :: my_cc(my_ibs   :my_ibe   ,my_jbs   :my_jbe   )
    !
    integer(ip) :: i,j
    !
    !*** First, ensure proper swap of halos, including boundaries
    !
    my_c(my_ips-1,:) = my_c(my_ips,:)
    my_c(my_ipe+1,:) = my_c(my_ipe,:)
    my_c(:,my_jps-1) = my_c(:,my_jps)
    my_c(:,my_jpe+1) = my_c(:,my_jpe)
    !
    call domain_swap_mass_points_2halo_2Dx (my_c)
    call domain_swap_mass_points_2halo_2Dy (my_c)
    !
    ! LAM: needed swapping corners?? -> my_c(i-1,j-1)
    !
    do j = my_jbs,my_jbe
       do i = my_ibs,my_ibe
          my_cc(i,j) = (my_c(i,j) + my_c(i-1,j) + my_c(i,j-1) + my_c(i-1,j-1))/4.0_rp
       end do
    end do
    !
    return
  end subroutine grid_p2c_2D
  !
  !-----------------------------
  !    subroutine grid_c2p
  !-----------------------------
  !
  !>   @brief
  !>   Converts an array from corner points to mass points
  !
  subroutine grid_c2p(my_c,my_cc)
    implicit none
    !
    !>   @param my_c  values at my processor mass points (with 0 halo)
    !>   @param my_cc values at my processor corner points
    !
    real(rp), intent(INOUT) :: my_c (my_ips:my_ipe,my_jps:my_jpe,my_kps:my_kpe)
    real(rp), intent(IN   ) :: my_cc(my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe)
    !
    integer(ip) :: i,j,k
    !
    do k = my_kps,my_kpe
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             my_c(i,j,k) = (my_cc(i,j,k  ) + my_cc(i+1,j,k  ) + my_cc(i,j+1,k  ) + my_cc(i+1,j+1,k  ) + &
                  my_cc(i,j,k+1) + my_cc(i+1,j,k+1) + my_cc(i,j+1,k+1) + my_cc(i+1,j+1,k+1))/8.0_rp
          end do
       end do
    end do
    !
    return
  end subroutine grid_c2p
  !
  !-----------------------------
  !    subroutine grid_c2p_2D
  !-----------------------------
  !
  !>   @brief
  !>   Converts an array from corner points to mass points
  !
  subroutine grid_c2p_2D(my_c,my_cc)
    implicit none
    !
    !>   @param my_c  values at my processor mass points (with 0 halo)
    !>   @param my_cc values at my processor corner points
    !
    real(rp), intent(INOUT) :: my_c (my_ips:my_ipe,my_jps:my_jpe)
    real(rp), intent(IN   ) :: my_cc(my_ibs:my_ibe,my_jbs:my_jbe)
    !
    integer(ip) :: i,j
    !
    do j = my_jps,my_jpe
       do i = my_ips,my_ipe
          my_c(i,j) = (my_cc(i,j) + my_cc(i+1,j) + my_cc(i,j+1) + my_cc(i+1,j+1))/4.0_rp
       end do
    end do
    !
    return
  end subroutine grid_c2p_2D
  !
  !-----------------------------------
  !    subroutine grid_get_shapez
  !-----------------------------------
  !
  !>   @brief
  !>   Gets 1D interpolation factor and host element along z (if any)
  !>
  !
  subroutine grid_get_shapez(kbs,kbe,zc,z,ik,shapez)
    implicit none
    !
    !>   @param kbs    start array counter
    !>   @param kbe    end   array counter
    !>   @param zc     values of z at my processor corner points
    !>   @param z      value  of z at which interpolate
    !>   @param ik     returned host element (0 means none)
    !>   @param shapez retruned shape factor
    !
    integer(ip), intent(IN)    :: kbs
    integer(ip), intent(IN)    :: kbe
    real(rp),    intent(IN)    :: zc(kbs:kbe)
    real(rp),    intent(IN)    :: z
    integer(ip), intent(INOUT) :: ik
    real(rp),    intent(INOUT) :: shapez
    !
    logical     :: found
    integer(ip) :: k
    !
    if(z.lt.zc(kbs)) then
       ik = kbs
       shapez = 0.0_rp
       return            ! not in my domain
    else if(z.gt.zc(kbe)) then
       ik = kbe-1
       shapez = 1.0_rp
       return            ! not in my domain
    else
       found = .false.
       k = kbs-1
       do while(.not.found)
          k = k+1
          if((z.ge.zc(k)).and.(z.le.zc(k+1))) then
             found = .true.
             ik = k
             shapez = (z-zc(k))/(zc(k+1)-zc(k))
          end if
       end do
    end if
    !
    return
  end subroutine grid_get_shapez
  !
  !------------------------------------
  !    subroutine  grid_get_mass_volume
  !------------------------------------
  !
  !>   @brief
  !>   Computes mass in computational volume (in the local and global domains) from concentration at mass points.
  !>   @details
  !>   Using the scaled concentration c* dXdYdZ = c Hm1*Hm2*Hm3 dlon*dlat*dz
  !
  subroutine grid_get_mass_volume(gl_mass,MY_TRA,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param gl_mass mass in the computational domain
    !>   @param MY_TRA  tracers structure
    !>   @param MY_GRID grid structure already filled by subroutine grid_build_arakawa_c
    !>   @param MY_ERR  error handler
    !
    real(rp),            intent(INOUT) :: gl_mass
    type(TRACERS),       intent(IN   ) :: MY_TRA
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: i,j,k,ibin
    real(rp)              :: dX,dY,dZ,my_mass
    real(rp), allocatable :: mass(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_get_mass_volume'
    MY_ERR%message = ' '
    !
    my_mass = 0.0_rp
    do k = my_kps,my_kpe
       dZ = MY_GRID%dX3_p(k)
       do j = my_jps,my_jpe
          dY = MY_GRID%dX2_p(j)
          do i = my_ips,my_ipe
             dX = MY_GRID%dX1_p(i)
             do ibin = 1,MY_TRA%nbins
                my_mass = my_mass + MY_TRA%my_c(i,j,k,ibin) *dX*dY*dZ
             end do
          end do
       end do
    end do
    !
    allocate(mass(0:npes_model-1))
    mass(:)          = 0.0_rp
    mass(mype_model) = my_mass
    call parallel_sum(mass, COMM_MODEL)
    !
    gl_mass = 0.0_rp
    do i = 0,npes_model-1
       gl_mass = gl_mass + mass(i)
    end do
    deallocate(mass)
    !
    return
  end subroutine grid_get_mass_volume
  !
  !------------------------------------
  !    subroutine  grid_get_mass_sink
  !------------------------------------
  !
  !>   @brief
  !>   Computes mass removed by sink terms
  !>   @details
  !>   Using the scaled concentration c* dXdYdZ = c Hm1*Hm2*Hm3 dlon*dlat*dz
  !
  subroutine grid_get_mass_sink(gl_mass,MY_TRA,MY_GRID,MY_ERR)
    implicit none
    !
    !>   @param gl_mass mass lost by sink terms
    !>   @param MY_TRA  tracers structure
    !>   @param MY_GRID grid structure already filled by subroutine grid_build_arakawa_c
    !>   @param MY_ERR  error handler
    !
    real(rp),            intent(INOUT) :: gl_mass
    type(TRACERS),       intent(IN   ) :: MY_TRA
    type(ARAKAWA_C_GRID),intent(IN   ) :: MY_GRID
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: i,j,ibin
    real(rp)              :: dX,dY,my_mass,awet
    real(rp), allocatable :: mass(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_get_mass_sink'
    MY_ERR%message = ' '
    !
    !*** wet deposition
    !
    my_mass = 0.0_rp
    do j = my_jps,my_jpe
       dY = MY_GRID%dX2_p(j)
       do i = my_ips,my_ipe
          dX   = MY_GRID%dX1_p(i)
          awet = 0.0_rp
          do ibin = 1,MY_TRA%nbins
             awet = awet + MY_TRA%my_awet(i,j,ibin)
          end do
          my_mass = my_mass + awet*dX*dY
       end do
    end do
    !
    allocate(mass(0:npes_model-1))
    mass(:)          = 0.0_rp
    mass(mype_model) = my_mass
    call parallel_sum(mass, COMM_MODEL)
    !
    gl_mass = 0.0_rp
    do i = 0,npes_model-1
       gl_mass = gl_mass + mass(i)
    end do
    deallocate(mass)
    !
    return
  end subroutine grid_get_mass_sink
  !
  !----------------------------------------
  !    subroutine  grid_get_mass_boundaries
  !----------------------------------------
  !
  !>   @brief
  !>   Computes mass lost through the bundaries in the global domain
  !>   @details
  !>   Using the scaled concentration c* dXdYdZ = c Hm1*Hm2*Hm3 dlon*dlat*dz (already done by ADS)
  !
  subroutine grid_get_mass_boundaries(gl_mass_ground,gl_mass_lateral,MY_TRA,MY_ERR)
    implicit none
    !
    !>   @param gl_mass_ground  total mass accumulated at ground
    !>   @param gl_mass_lateral total mass leaving/entering lateral and top boundaries
    !>   @param MY_TRA          tracers structure
    !>   @param MY_ERR          error handler
    !
    real(rp),            intent(INOUT) :: gl_mass_ground
    real(rp),            intent(INOUT) :: gl_mass_lateral
    type(TRACERS),       intent(IN   ) :: MY_TRA
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: i
    real(rp), allocatable :: mass(:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'grid_get_mass_boundaries'
    MY_ERR%message = ' '
    !
    allocate(mass(0:npes_model-1))
    !
    !*** Mass at ground
    !
    mass(:)          = 0.0_rp
    mass(mype_model) = abs(MY_TRA%my_D_flux)
    call parallel_sum(mass, COMM_MODEL)
    !
    gl_mass_ground = 0.0_rp
    do i = 0,npes_model-1
       gl_mass_ground = gl_mass_ground + mass(i)
    end do
    !
    !*** Mass at lateral and top
    !
    mass(:)          = 0.0_rp
    mass(mype_model) = abs(MY_TRA%my_W_flux) + abs(MY_TRA%my_E_flux) + abs(MY_TRA%my_S_flux) + &
         abs(MY_TRA%my_N_flux) + abs(MY_TRA%my_U_flux)
    call parallel_sum(mass, COMM_MODEL)
    !
    gl_mass_lateral = 0.0_rp
    do i = 0,npes_model-1
       gl_mass_lateral = gl_mass_lateral + mass(i)
    end do
    !
    deallocate(mass)
    !
    return
  end subroutine grid_get_mass_boundaries
  !
  !
END MODULE Grid
