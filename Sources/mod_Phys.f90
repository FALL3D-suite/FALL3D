!***************************************************************
!>
!> Module for point-level atmospheric physics related operations
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE Phys
  use KindType
  use InpOut
  use Parallel
  use Domain
  use Grid
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: phys_read_inp_model
  PUBLIC :: phys_bcast_inp_model
  PUBLIC :: phys_get_kdiffu
  PUBLIC :: phys_get_vset
  PUBLIC :: phys_get_vset_point
  PUBLIC :: phys_get_psi
  PUBLIC :: phys_get_ust
  PUBLIC :: phys_get_monin
  PUBLIC :: phys_radionuclides
  PUBLIC :: phys_wet_deposition
  PUBLIC :: phys_dry_deposition
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: phys_get_Rib
  PRIVATE :: phys_get_Fih
  PRIVATE :: phys_get_Fc
  PRIVATE :: phys_get_Gm
  PRIVATE :: phys_get_Gh
  PRIVATE :: phys_get_gama
  !
  !    LIST OF PUBLIC VARIABLES
  !
  ! Settling velocity models
  integer(ip), parameter :: MOD_ARASTOPOUR    = 1
  integer(ip), parameter :: MOD_GANSER        = 2
  integer(ip), parameter :: MOD_WILSON        = 3
  integer(ip), parameter :: MOD_DELLINO       = 4
  integer(ip), parameter :: MOD_PFEIFFER      = 5
  integer(ip), parameter :: MOD_DIOGUARDI2017 = 6
  integer(ip), parameter :: MOD_DIOGUARDI2018 = 7
  !
  ! Turbulence models
  integer(ip), parameter :: MOD_CONSTANT      = 1
  integer(ip), parameter :: MOD_CMAQ          = 2
  integer(ip), parameter :: MOD_SIMILARITY    = 3
  !
  ! Limiters
  integer(ip), parameter :: LIMITER_MINMOD   = 1
  integer(ip), parameter :: LIMITER_SUPERBEE = 2
  integer(ip), parameter :: LIMITER_OSPRE    = 3
  !
  ! Air properties
  integer(ip), parameter :: TIME_MARCHING_EULER = 1
  integer(ip), parameter :: TIME_MARCHING_RK    = 2
  !
  real(rp), parameter    :: visa0 = 1.827e-5_rp     !< reference air viscosity
  real(rp), parameter    :: Ta0   = 291.15_rp       !< reference air temperature
  !
  ! Life time (t_1/2) and dacay rate (kn) for radionuclides (kn = log(2)/t_1/2)
  ! 134Cs t_1/2 = 2.065  years = 6.51E7 seconds -> kn= 1.06E-8  (1/seconds)
  ! 137Cs t_1/2 = 30.17  years = 9.51E8 seconds -> kn= 9.29E-10
  ! 131I  t_1/2 = 8.0197 days  = 6.93E5 seconds -> kn= 1.00E-6
  ! 90Sr  t_1/2 = 28.79  years = 9.08E8 seconds -> kn= 7.63E-10
  ! 90Y   t_1/2 = 2.69   days  = 2.33E5 seconds -> kn= 2.98E-6
  ! Decay factors are: exp(-kn*dt)
  !
  ! Decay rates of radionuclides
  real(rp), parameter :: kn_CS134 = 1.06e-8_rp    !< Decay rate of 134Cs (1/s)
  real(rp), parameter :: kn_CS137 = 9.29e-10_rp   !< Decay rate of 137Cs (1/s)
  real(rp), parameter :: kn_I131  = 1.00e-6_rp    !< Decay rate of 131I  (1/s)
  real(rp), parameter :: kn_Sr90  = 7.63e-10_rp   !< Decay rate of 90Sr  (1/s)
  real(rp), parameter :: kn_Y90   = 2.98e-6_rp    !< Decay rate of 90Y   (1/s)

CONTAINS
  !
  !-----------------------------------------
  !    subroutine phys_read_inp_model
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads the MODEL_PHYSICS block form the input file
  !
  subroutine phys_read_inp_model(MY_FILES,MY_MOD,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),   intent(INOUT) :: MY_FILES
    type(MODEL_PHYS),  intent(INOUT) :: MY_MOD
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    real(rp)              :: file_version
    real(rp)              :: rvoid
    character(len=s_file) :: file_inp, word
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_read_inp_model'
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
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version '
       return
    end if
    !
    !*** Reads MODEL_PHYSICS block
    !
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','LIMITER',word, 1, MY_ERR, .true.)
    select case(word)
    case('MINMOD')
       MY_MOD%limiter = LIMITER_MINMOD
    case('SUPERBEE')
       MY_MOD%limiter = LIMITER_SUPERBEE
    case('OSPRE')
       MY_MOD%limiter = LIMITER_OSPRE
    case default
       MY_MOD%limiter = LIMITER_SUPERBEE  ! default value
    end select
    !
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','TIME_MARCHING',word, 1, MY_ERR, .true.)
    select case(word)
    case('EULER')
       MY_MOD%time_marching = TIME_MARCHING_EULER
    case('RUNGE-KUTTA')
       MY_MOD%time_marching = TIME_MARCHING_RK
    case default
       MY_MOD%time_marching = TIME_MARCHING_RK  ! default value
    end select
    !
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','CFL_CRITERION',word, 1, MY_ERR, .true.)
    select case(word)
    case('ONE_DIMENSIONAL')
       MY_MOD%CFL_criterion = 1  ! one dimension
    case('ALL_DIMENSIONS')
       MY_MOD%CFL_criterion = 2  ! all dimensions
    case default
       MY_MOD%CFL_criterion = 2  ! default value
    end select
    !
    call inpout_get_rea (file_inp,'MODEL_PHYSICS','CFL_SAFETY_FACTOR',MY_MOD%CFL_safety_factor, 1, MY_ERR)
    if(MY_ERR%flag.ne.0) then
       MY_MOD%CFL_safety_factor = 0.9_rp  ! default value
    end if
    !
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','TERMINAL_VELOCITY_MODEL',word, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    select case(word)
    case('ARASTOOPOUR')
       MY_MOD%modv = MOD_ARASTOPOUR
    case('GANSER')
       MY_MOD%modv = MOD_GANSER
    case('WILSON')
       MY_MOD%modv = MOD_WILSON
    case('DELLINO')
       MY_MOD%modv = MOD_DELLINO
    case('PFEIFFER')
       MY_MOD%modv = MOD_PFEIFFER
    case('DIOGUARDI2017')
       MY_MOD%modv = MOD_DIOGUARDI2017
    case('DIOGUARDI2018')
       MY_MOD%modv = MOD_DIOGUARDI2018
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'incorrect terminal velocity model '
       return
    end select
    !
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','HORIZONTAL_TURBULENCE_MODEL',word, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    select case(word)
    case('CONSTANT')
       MY_MOD%modkh = MOD_CONSTANT
       call inpout_get_rea (file_inp,'MODEL_PHYSICS','HORIZONTAL_TURBULENCE_MODEL',MY_MOD%kh0,1,MY_ERR)
       if(MY_ERR%flag.ne.0) return
    case('CMAQ')
       MY_MOD%modkh = MOD_CMAQ
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'incorrect horizontal turbulence model '
       return
    end select
    !
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','VERTICAL_TURBULENCE_MODEL',word, 1, MY_ERR, .true.)
    if(MY_ERR%flag.ne.0) return
    select case(word)
    case('CONSTANT')
       MY_MOD%modkv = MOD_CONSTANT
       call inpout_get_rea (file_inp,'MODEL_PHYSICS','VERTICAL_TURBULENCE_MODEL',MY_MOD%kv0,1,MY_ERR)
       if(MY_ERR%flag.ne.0) return
    case('SIMILARITY')
       MY_MOD%modkv = MOD_SIMILARITY
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'incorrect vertical turbulence model '
       return
    end select
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','WET_DEPOSITION',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_MOD%wet_deposition = .true.
    else
       MY_MOD%wet_deposition = .false.  ! default value
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','DRY_DEPOSITION',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_MOD%dry_deposition = .true.
    else
       MY_MOD%dry_deposition = .false.  ! default value
    end if
    !
    word = ''
    call inpout_get_cha (file_inp, 'MODEL_PHYSICS','GRAVITY_CURRENT',word, 1, MY_ERR, .true.)
    if(TRIM(word).eq.'YES') then
       MY_MOD%gravity_current = .true.
    else
       MY_MOD%gravity_current = .false.  ! default value
    end if
    !
    if(MY_MOD%gravity_current) then
       !
       call inpout_get_rea (file_inp,'IF_GRAVITY_CURRENT','C_FLOW_RATE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_MOD%MY_GC%c_flow_rate = 0.43e3_rp
       else
          MY_MOD%MY_GC%c_flow_rate = rvoid
       end if
       !
       call inpout_get_rea (file_inp,'IF_GRAVITY_CURRENT','LAMBDA_GRAV',rvoid,1,MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_MOD%MY_GC%lambda = 0.2_rp
       else
          MY_MOD%MY_GC%lambda = rvoid
       end if
       !
       call inpout_get_rea (file_inp,'IF_GRAVITY_CURRENT','K_ENTRAIN',rvoid,1,MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_MOD%MY_GC%k_entrain = 0.1_rp
       else
          MY_MOD%MY_GC%k_entrain = rvoid
       end if
       !
       call inpout_get_rea (file_inp,'IF_GRAVITY_CURRENT','BRUNT_VAISALA',rvoid,1,MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_MOD%MY_GC%brunt_vaisala = 0.02_rp
       else
          MY_MOD%MY_GC%brunt_vaisala = rvoid
       end if
       !
       call inpout_get_rea (file_inp,'IF_GRAVITY_CURRENT','GC_START_(HOURS_AFTER_00)',rvoid,1,MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'GC start time not given '
          return
       else
          MY_MOD%MY_GC%start_time = rvoid*3600.0_rp  ! h --> s
       end if
       !
       call inpout_get_rea (file_inp,'IF_GRAVITY_CURRENT','GC_END_(HOURS_AFTER_00)',rvoid,1,MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'GC end time not given '
          return
       else
          MY_MOD%MY_GC%end_time = rvoid*3600.0_rp  ! h --> s
       end if
       !
       call inpout_get_rea (file_inp, 'SOURCE','LON_VENT',MY_MOD%MY_GC%lon, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Source (GC) longitude not given '
          return
       end if
       !
       call inpout_get_rea (file_inp, 'SOURCE','LAT_VENT',MY_MOD%MY_GC%lat, 1, MY_ERR)
       if(MY_ERR%flag.ne.0) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Source (GC) latitude not given '
          return
       end if
       !
    end if
    !
    return
  end subroutine phys_read_inp_model
  !
  !-----------------------------------------
  !    subroutine phys_bcast_inp_model
  !-----------------------------------------
  !
  !>   @brief
  !>   Master broadcasts MODEL_PHYSICS block from input file
  !
  subroutine phys_bcast_inp_model(MY_MOD,MY_ERR)
    implicit none
    !
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_ERR    error handler
    !
    type(MODEL_PHYS),  intent(INOUT) :: MY_MOD
    type(ERROR_STATUS),intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_bcast_inp_model'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_MOD%wet_deposition  ,1,0)
    call parallel_bcast(MY_MOD%dry_deposition  ,1,0)
    call parallel_bcast(MY_MOD%gravity_current ,1,0)
    !
    call parallel_bcast(MY_MOD%modv              ,1,0)
    call parallel_bcast(MY_MOD%limiter           ,1,0)
    call parallel_bcast(MY_MOD%time_marching     ,1,0)
    call parallel_bcast(MY_MOD%CFL_criterion     ,1,0)
    call parallel_bcast(MY_MOD%modkv             ,1,0)
    call parallel_bcast(MY_MOD%modkh             ,1,0)
    !
    call parallel_bcast(MY_MOD%CFL_safety_factor ,1,0)
    call parallel_bcast(MY_MOD%kh0               ,1,0)
    call parallel_bcast(MY_MOD%kv0               ,1,0)
    !
    if(MY_MOD%gravity_current) then
       call parallel_bcast(MY_MOD%MY_GC%c_flow_rate   ,1,0)
       call parallel_bcast(MY_MOD%MY_GC%lambda        ,1,0)
       call parallel_bcast(MY_MOD%MY_GC%k_entrain     ,1,0)
       call parallel_bcast(MY_MOD%MY_GC%brunt_vaisala ,1,0)
       call parallel_bcast(MY_MOD%MY_GC%start_time    ,1,0)
       call parallel_bcast(MY_MOD%MY_GC%end_time      ,1,0)
       call parallel_bcast(MY_MOD%MY_GC%lon           ,1,0)
       call parallel_bcast(MY_MOD%MY_GC%lat           ,1,0)
    end if
    !
    return
  end subroutine phys_bcast_inp_model
  !
  !-----------------------------------------
  !    subroutine phys_get_kdiffu
  !-----------------------------------------
  !
  !>   @brief
  !>   Computes vertical and horizontal diffusion coefficients
  !
  subroutine phys_get_kdiffu(MY_MOD,MY_GRID,my_uc,my_vc,my_tvc,my_pblh,my_mon,my_ust,my_k1,my_k2,my_k3,MY_ERR)
    implicit none
    !
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_GRID   ARAKAWA_C_GRID structure
    !>   @param my_uc     u-wind velocity at my processor cell corners
    !>   @param my_vc     v-wind velocity at my processor cell corners
    !>   @param my_tv     air virtual temperature at mass points
    !>   @param my_pblh   boundary layer height at mass points
    !>   @param my_mon    Monin-Obukhov lenght  at mass points
    !>   @param my_ust    friction velocity u*  at mass points
    !>   @param my_k1     x-diffusion        at mass points
    !>   @param my_k2     y-diffusion        at mass points
    !>   @param my_k3     z-diffusion        at mass points
    !>   @param MY_ERR    error handler
    !
    type(MODEL_PHYS),     intent(IN   ) :: MY_MOD
    type(ARAKAWA_C_GRID), intent(IN   ) :: MY_GRID
    real(rp),             intent(IN   ) :: my_uc  (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),             intent(IN   ) :: my_vc  (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),             intent(IN   ) :: my_tvc (my_ibs   :my_ibe   ,my_jbs   :my_jbe   ,my_kbs   :my_kbe   )
    real(rp),             intent(IN   ) :: my_pblh(my_ips   :my_ipe   ,my_jps   :my_jpe)
    real(rp),             intent(IN   ) :: my_mon (my_ips   :my_ipe   ,my_jps   :my_jpe)
    real(rp),             intent(IN   ) :: my_ust (my_ips   :my_ipe   ,my_jps   :my_jpe)
    real(rp),             intent(INOUT) :: my_k1  (my_ips_2h:my_ipe_2h,my_jps   :my_jpe   ,my_kps   :my_kpe   )
    real(rp),             intent(INOUT) :: my_k2  (my_ips   :my_ipe   ,my_jps_2h:my_jpe_2h,my_kps   :my_kpe   )
    real(rp),             intent(INOUT) :: my_k3  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps_2h:my_kpe_2h)
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k
    real(rp)    :: dx,dy,dudx,dvdx,dudy,dvdy
    real(rp)    :: alfa,kht,khn,khf,khn0,rkhmin,landac
    real(rp)    :: ust,mon,pblh,z,fih,fc,Ri,lc
    real(rp)    :: z1,z2,u1,u2,v1,v2,umod1,umod2,umod,Tv1,Tv2
    !
    real(rp), allocatable :: my_zp(:,:,:)
    real(rp), allocatable :: my_hp(:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_get_kdiffu'
    MY_ERR%message = ' '
    !
    alfa   = 0.28_rp            ! Smagorinsky parameter in L.E. Kh formula
    Khf    = 8000.0_rp          ! eddy diffusivity at a fixed resolution (CMAQ)
    rkhmin = 100.0_rp           ! Minimum K_h value
    landac = 30.0_rp            ! asymptotic length scale
    !
    select case(MY_GRID%map_h)
    case(MAP_H_CARTESIAN)
       Khn0 = 4000.0_rp
    case(MAP_H_SPHERICAL)
       Khn0 = 0.036_rp
    case(MAP_H_POLAR)
       MY_ERR%flag    = 1
       MY_ERR%message = 'Mapping not implemented yet'
       return
    case(MAP_H_MERCATOR)
       MY_ERR%flag    = 1
       MY_ERR%message = 'Mapping not implemented yet'
       return
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Incorrect horizontal mapping'
       return
    end select
    !
    !*** Horizontal Component(s)
    !
    select case(MY_MOD%modkh)
    case(MOD_CONSTANT)
       !
       my_k1(:,:,:) = MY_MOD%kh0
       my_k2(:,:,:) = MY_MOD%kh0
       !
    case(MOD_CMAQ)
       !
       do k = my_kps,my_kpe
          do j = my_jps,my_jpe
             do i = my_ips,my_ipe
                !
                dx = MY_GRID%dX1_p(i)*MY_GRID%Hm1_p(j)
                dy = MY_GRID%dX2_p(j)*MY_GRID%Hm2_p(j)
                !
                dudx = 0.5_rp*(my_uc(i+1,j  ,k) - my_uc(i,j,k) + my_uc(i+1,j+1,k) - my_uc(i  ,j+1,k))/dx
                dvdx = 0.5_rp*(my_vc(i+1,j  ,k) - my_vc(i,j,k) + my_vc(i+1,j+1,k) - my_vc(i  ,j+1,k))/dx
                dudy = 0.5_rp*(my_uc(i  ,j+1,k) - my_uc(i,j,k) + my_uc(i+1,j+1,k) - my_uc(i+1,j  ,k))/dy
                dvdy = 0.5_rp*(my_vc(i  ,j+1,k) - my_vc(i,j,k) + my_vc(i+1,j+1,k) - my_vc(i+1,j  ,k))/dy
                !
                kht = alfa*alfa*dx*dy*sqrt( (dudx-dvdy)**2 + (dvdx+dudy)**2 )
                khn = khf*(Khn0/(MY_GRID%Hm1_p(j)*MY_GRID%dlon))*(Khn0/(MY_GRID%Hm2_p(j)*MY_GRID%dlat))
                !
                my_k1(i,j,k) = (1.0_rp/Kht) + (1.0_rp/Khn)
                my_k1(i,j,k) = max(1.0_rp/my_k1(i,j,k),rkhmin)
                my_k2(i,j,k) = my_k1(i,j,k)
                !
             end do
          end do
       end do
       !
       my_k1(my_ips_2h  ,:,:) = my_k1(my_ips,:,:)
       my_k1(my_ips_2h+1,:,:) = my_k1(my_ips,:,:)
       my_k1(my_ipe_2h  ,:,:) = my_k1(my_ipe,:,:)
       my_k1(my_ipe_2h-1,:,:) = my_k1(my_ipe,:,:)
       !
       do k = my_kps,my_kpe
          do j = my_jps,my_jpe
             call domain_swap_mass_points_2halo_1dx(my_k1(:,j,k))
          end do
       end do
       !
       my_k2(:,my_jps_2h  ,:) = my_k2(:,my_jps,:)
       my_k2(:,my_jps_2h+1,:) = my_k2(:,my_jps,:)
       my_k2(:,my_jpe_2h  ,:) = my_k2(:,my_jpe,:)
       my_k2(:,my_jpe_2h-1,:) = my_k2(:,my_jpe,:)
       !
       do k = my_kps,my_kpe
          do i = my_ips,my_ipe
             call domain_swap_mass_points_2halo_1dy(my_k2(i,:,k))
          end do
       end do
       !
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Horizontal diffusion parameterization not implemented '
       return
    end select
    !
    !*** Vertical Component
    !
    select case(MY_MOD%modkv)
    case(MOD_CONSTANT)
       !
       my_k3(:,:,:) = MY_MOD%kv0
       !
    case(MOD_SIMILARITY)
       !
       allocate(my_zp(my_ips:my_ipe,my_jps:my_jpe,my_kps:my_kpe))
       allocate(my_hp(my_ips:my_ipe,my_jps:my_jpe))
       !
       call grid_c2p   (my_zp,MY_GRID%z_c)  ! z    at mass points
       call grid_c2p_2D(my_hp,MY_GRID%h_c)  ! topo at mass points
       !
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             !
             pblh = my_pblh(i,j)
             mon  = my_mon (i,j)
             ust  = my_ust (i,j)
             !
             do k = my_kps,my_kpe
                z = max(0.0_rp,my_zp(i,j,k)-my_hp(i,j))  ! height above terrain
                !
                if(z.lt.pblh) then  ! z<h
                   call phys_get_Fih(z,mon,fih)
                   my_k3(i,j,k) = KARMAN*z*ust*(1.0_rp-z/pblh)*(1.0_rp-z/pblh)/fih
                   my_k3(i,j,k) = max(0.0_rp,my_k3(i,j,k))
                else                ! z>h
                   lc = 1.0_rp/(KARMAN*z) + 1.0_rp/landac
                   lc = 1.0_rp/lc
                   !
                   z1 = 0.25_rp*(MY_GRID%z_c(i,j,k)+MY_GRID%z_c(i+1,j,k)+MY_GRID%z_c(i,j+1,k)+MY_GRID%z_c(i+1,j+1,k))          ! z at face 1
                   z1 = z1 - my_hp(i,j)
                   z2 = 0.25_rp*(MY_GRID%z_c(i,j,k+1)+MY_GRID%z_c(i+1,j,k+1)+MY_GRID%z_c(i,j+1,k+1)+MY_GRID%z_c(i+1,j+1,k+1))  ! z at face 2
                   z2 = z2 - my_hp(i,j)
                   !
                   u1 = 0.25_rp*(my_uc(i,j,k)  +my_uc(i+1,j,k)  +my_uc(i,j+1,k)  +my_uc(i+1,j+1,k))     ! u at face 1
                   v1 = 0.25_rp*(my_vc(i,j,k)  +my_vc(i+1,j,k)  +my_vc(i,j+1,k)  +my_vc(i+1,j+1,k))     ! v at face 1
                   u2 = 0.25_rp*(my_uc(i,j,k+1)+my_uc(i+1,j,k+1)+my_uc(i,j+1,k+1)+my_uc(i+1,j+1,k+1))   ! u at face 2
                   v2 = 0.25_rp*(my_vc(i,j,k+1)+my_vc(i+1,j,k+1)+my_vc(i,j+1,k+1)+my_vc(i+1,j+1,k+1))   ! v at face 2
                   !
                   umod1 = sqrt(u1*u1+v1*v1)
                   umod2 = sqrt(u2*u2+v2*v2)
                   umod  = 0.5_rp*(umod1+umod2)
                   !
                   Tv1 = 0.25_rp*(my_tvc(i,j,k)  +my_tvc(i+1,j,k)  +my_tvc(i,j+1,k)  +my_tvc(i+1,j+1,k))     ! tv at face 1
                   Tv2 = 0.25_rp*(my_tvc(i,j,k+1)+my_tvc(i+1,j,k+1)+my_tvc(i,j+1,k+1)+my_tvc(i+1,j+1,k+1))   ! tv at face 2
                   !
                   call phys_get_Rib(Tv2,Tv1,umod,z2,z1,Ri)
                   call phys_get_Fc (Ri,fc)
                   !
                   my_k3(i,j,k) = lc*lc*abs((umod2-umod1)/(z2-z1))*fc
                   my_k3(i,j,k) = max(0.0_rp,my_k3(i,j,k))
                end if
             end do
             !
          end do
       end do
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
    case default
       MY_ERR%flag    = 1
       MY_ERR%message = 'Horizontal diffusion parameterization not implemented '
       return
    end select
    !
    return
  end subroutine phys_get_kdiffu
  !
  !-----------------------------------------
  !    subroutine phys_get_vset
  !-----------------------------------------
  !
  !>   @brief
  !>   Get particle terminal fall velocity
  !
  subroutine phys_get_vset(MY_MOD, MY_AGR, my_rhoc, my_tc, MY_BIN, my_vs, MY_ERR)
    implicit none
    !
    !>   @param MY_MOD    model physics related parameters
    !>   @param MY_AGR    list of parameters defining an aggregation model
    !>   @param my_rhoc   air density     at corner points
    !>   @param my_tc     air temperature at corner points
    !>   @param MY_BIN    list of parameters defining bin granulometric properties
    !>   @param my_vs     settling velocity at w-boundaries
    !>   @param MY_ERR    error handler
    !
    type(MODEL_PHYS),     intent(IN   ) :: MY_MOD
    type(AGR_PARAMS),     intent(IN   ) :: MY_AGR
    real(rp),             intent(IN   ) :: my_rhoc(my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    real(rp),             intent(IN   ) :: my_tc  (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    type(BIN_PARAMS),     intent(IN   ) :: MY_BIN
    real(rp),             intent(INOUT) :: my_vs  (my_ips:my_ipe, my_jps:my_jpe, my_kbs_1h:my_kbe_1h,1:MY_BIN%nbins)
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k,ibin
    real(rp)    :: rhoa,ta,visa
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_get_vset'
    MY_ERR%message = ' '
    !
    !*** Set setling velocity of aerosol bins to zero
    !
    my_vs(:,:,:,:) = 0.0_rp
    !
    !*** Settling velocity of particle bins
    !
    do k = my_kbs,my_kbe
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             !
             rhoa = 0.25_rp*(my_rhoc(i,j,k)+my_rhoc(i+1,j,k)+my_rhoc(i+1,j+1,k)+my_rhoc(i,j+1,k))  ! air density     at mass point (w-boundary)
             ta   = 0.25_rp*(my_tc  (i,j,k)+my_tc  (i+1,j,k)+my_tc  (i+1,j+1,k)+my_tc  (i,j+1,k))  ! air temperature at mass point (w-boundary)
             visa = visa0*((ta0+120.0_rp)/(ta+120.0_rp))*((ta/ta0)**1.5_rp)                        ! Sutherland's law
             !
             do ibin = 1,MY_BIN%nbins
                if(MY_BIN%bin_cat(ibin).ne.CAT_AEROSOL) &
                     call phys_get_vset_point(MY_BIN%bin_diam(ibin),MY_BIN%bin_rho(ibin),MY_BIN%bin_psi(ibin), &
                     rhoa,visa,my_vs(i,j,k,ibin),MY_MOD%modv,MY_ERR)
             end do
             !
          end do
       end do
    end do
    !
    my_vs(:,:,my_kbs_1h,1:MY_BIN%nbins) = my_vs(:,:,my_kbs,1:MY_BIN%nbins)
    my_vs(:,:,my_kbe_1h,1:MY_BIN%nbins) = my_vs(:,:,my_kbe,1:MY_BIN%nbins)
    do ibin = 1,MY_BIN%nbins
       call domain_swap_velo_points_1halo_z(my_vs(:,:,:,ibin))
    end do
    !
    !*** Correct settling velocities of aggregates
    !
    do ibin = 1,MY_BIN%nbins
       if(MY_BIN%bin_type(ibin).eq.'aggregate') then
          my_vs(:,:,:,ibin) = MY_AGR%vset_fac * my_vs(:,:,:,ibin)
       end if
    end do
    !
    return
  end subroutine phys_get_vset
  !
  !-----------------------------------------
  !    subroutine phys_get_vset_point
  !-----------------------------------------
  !
  !>   @brief
  !>   Get particle terminal fall velocity for a given air density and viscosity.
  !
  !      modv = 1   ARASTOPOUR.    psi = 1 (not used)
  !      modv = 2   GANSER         psi = sphericity
  !      modv = 3   WILSON         psi = (b+c)/2a    a>b>c semi-axes
  !      modv = 4   DELLINO        psi = sphericity/circularity
  !      modv = 5   PFEIFFER       psi = (b+c)/2a    a>b>c semi-axes
  !      modv = 6   DIOGUARDI2017  psi = sphericity
  !      modv = 7   DIOGUARDI2018  psi = sphericity
  !
  subroutine phys_get_vset_point(diam,rhop,psi,rhoa,visc,vset,model,MY_ERR,reynolds,cdrag)
    implicit none
    !
    !>   @param  diam      particle diameter
    !>   @param  rhop      particle density
    !>   @param  psi       particle shape factor (computed by phys_get_psi)
    !>   @param  rhoa      air      density
    !>   @param  visc      air      viscosity
    !>   @param  vset      output terminal velocity
    !>   @param  model     terminal settling velocity model
    !>   @param  MY_ERR    error handler
    !>   @param  reynolds  output Reynolds number
    !>   @param  cdrag     output drag coefficient
    !
    real(rp),           intent(IN   ) :: diam
    real(rp),           intent(IN   ) :: rhop
    real(rp),           intent(IN   ) :: psi
    real(rp),           intent(IN   ) :: rhoa
    real(rp),           intent(IN   ) :: visc
    real(rp),           intent(  OUT) :: vset
    integer(ip),        intent(IN   ) :: model
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    real(rp), optional, intent(  OUT) :: reynolds
    real(rp), optional, intent(  OUT) :: cdrag
    !
    integer(ip)  :: it,maxit
    real(rp)     :: atol,rtol,cd
    real(rp)     :: rey,rk1,rk2,a,b,vold,cd100,cd1000
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_get_vset_point'
    MY_ERR%message = ' '
    !
    atol  = 1e-12_rp            ! Absolute tolerance
    rtol  = 1e-6_rp             ! Relative tolerance
    maxit = 1000                ! Maximum number of iterations
    cd    = 1.0_rp              ! Guess value
    !
    !*** Model choice
    !
    select case(model)
    case(MOD_ARASTOPOUR)           ! Arastopour
       !
       vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
       vold=vset
       do it=1,maxit
          rey=rhoa*vset*diam/visc
          if(rey <= 988.947_rp) then   ! This is the actual transition point
             cd=24.0_rp/rey*(1.0_rp+0.15_rp*rey**0.687_rp)
          else
             cd=0.44_rp
          endif
          vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
          if(abs(vset-vold) <= atol+rtol*abs(vset)) then
             if(present(reynolds)) reynolds=rey
             if(present(cdrag))    cdrag=cd
             return
          end if
          vold=vset
       end do
       !
    case(MOD_GANSER)     ! Ganser
       !
       vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
       vold=vset
       do it=1,maxit
          rey=rhoa*vset*diam/visc
          rk1=3.0_rp/(1.0_rp+2.0_rp/sqrt(psi))  ! Stokes' shape factor
          rk2=10.0_rp**(1.8148_rp*(-log10(psi))**0.5743_rp) !Newton's shape fact
          cd=24.0_rp/(rey*rk1)*(1.0_rp+0.1118_rp*(rey*rk1*rk2)**0.6567_rp) + &
               0.4305_rp*rk2/(1.0_rp+3305.0_rp/(rey*rk1*rk2))
          vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
          if(abs(vset-vold) <= atol+rtol*abs(vset)) then
             if(present(reynolds)) reynolds=rey
             if(present(cdrag))    cdrag=cd
             return
          end if
          vold=vset
       end do
       !
    case(MOD_WILSON)     ! Wilson
       !
       vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
       vold=vset
       do it=1,maxit
          rey=rhoa*vset*diam/visc
          cd=24.0_rp/rey*psi**(-0.828_rp)+2.0_rp*sqrt(1.07_rp-psi)
          vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
          if(abs(vset-vold) <= atol+rtol*abs(vset)) then
             if(present(reynolds)) reynolds=rey
             if(present(cdrag))    cdrag=cd
             return
          end if
          vold=vset
       enddo
       !
    case(MOD_DELLINO)      ! Dellino
       !
       vset = ((diam**3*GI*rhop*rhoa*(psi**1.6_rp))/(visc**2))**0.5206_rp
       vset = (1.2065_rp*visc*vset)/(diam*rhoa)
       if(present(reynolds)) reynolds=0.0_rp ! @@@@ TODO
       if(present(cdrag))    cdrag=0.0_rp    ! @@@@ TODO
       return
       !
    case(MOD_PFEIFFER)     ! Pfeiffer
       !
       vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
       vold=vset
       cd100=0.24_rp*psi**(-0.828_rp)+2.0_rp*sqrt(1.07_rp-psi) ! cd at rey=100
       cd1000=1.0_rp            ! cd at rey>=1000 (from Pfeiffer et al., 2005)
       !cd1000=1d0-0.56d0*psi
       a=(cd1000-cd100)/900.0_rp                            ! interpolation
       b=cd1000-1000.0_rp*a
       do it=1,maxit
          rey=rhoa*vset*diam/visc
          if(rey <= 100.0_rp) then
             cd=24.0_rp/rey*psi**(-0.828_rp)+2.0_rp*sqrt(1.07_rp-psi)
          elseif(rey > 100.0_rp .and. rey < 1000.0_rp) then
             cd=a*rey+b
          else
             cd=cd1000
          endif
          vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
          if(abs(vset-vold) <= atol+rtol*abs(vset)) then
             if(present(reynolds)) reynolds=rey
             if(present(cdrag))    cdrag=cd
             return
          end if
          vold=vset
       end do
       !
    case(MOD_DIOGUARDI2017)
       !
       vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
       vold=vset
       do it=1,maxit
          rey=rhoa*vset*diam/visc
          ! Evaluate cd_sphere
          cd = 24.0_rp*(1.0_rp+0.15_rp*rey**0.687_rp)/rey + &
               0.42_rp/(1.0_rp+42500.0_rp/rey**1.16_rp)
          ! Note: the Reynolds number is limited to 10^-2 in the exponent
          ! to avoid the strong divergence near Re=0. This limit is compatible
          ! with the range of Reynolds numbers investigated in the experiments
          ! of Dioguardi et al., 2017.  psi is the sphericity
          cd = 0.74533_rp*rey**0.146012_rp*cd / &
               psi**(0.5134_rp/max(rey,0.01_rp)**0.2_rp)
          vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
          if(abs(vset-vold) <= atol+rtol*abs(vset)) then
             if(present(reynolds)) reynolds=rey
             if(present(cdrag))    cdrag=cd
             return
          end if
          vold=vset
       end do
       !
    case(MOD_DIOGUARDI2018)
       !
       vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
       vold=vset
       do it=1,maxit
          rey=rhoa*vset*diam/visc
          cd = 24.0_rp/rey*(((1.0_rp-psi)/rey+1.0_rp)**0.25_rp + &
               (0.1806_rp*rey**0.6459_rp)*psi**(-rey**0.08_rp)) + &
               0.4251_rp/(1.0_rp+6880.95_rp/rey*psi**5.03_rp)
          vset=sqrt(4.0_rp*GI*diam*rhop/(3.0_rp*cd*rhoa))
          if(abs(vset-vold) <= atol+rtol*abs(vset)) then
             if(present(reynolds)) reynolds=rey
             if(present(cdrag))    cdrag=cd
             return
          end if
          vold=vset
       end do
       !
    case default
       !
       ! *** Model not set (should not reach this line)
       !
       MY_ERR%flag = 1
       MY_ERR%message = 'setling velocity model not found '
       !
       if(present(reynolds)) reynolds=0.0_rp
       if(present(cdrag))    cdrag=0.0_rp
       !
       return
    end select
    !
    !*** No convergence
    !
    MY_ERR%flag = 2
    MY_ERR%message = 'no convergence in vset '
    !
    if(present(reynolds)) reynolds=0.0_rp
    if(present(cdrag))    cdrag=0.0_rp
    !
    return
  end subroutine phys_get_vset_point
  !
  !-----------------------------------------
  !    subroutine phys_get_psi
  !-----------------------------------------
  !
  !>   @brief
  !>   Calculates the particle shape factor psi from the sphericity depending on the velocity model
  !
  !      modv = 1   ARASTOPOUR.    psi = 1 (not used)
  !      modv = 2   GANSER         psi = sphericity
  !      modv = 3   WILSON         psi = (b+c)/2a    a>b>c semi-axes
  !      modv = 4   DELLINO        psi = sphericity/circularity
  !      modv = 5   PFEIFFER       psi = (b+c)/2a    a>b>c semi-axes
  !      modv = 6   DIOGUARDI2017  psi = sphericity
  !      modv = 7   DIOGUARDI2018  psi = sphericity
  !
  subroutine phys_get_psi(psi,sphe,diam,modv,nc,MY_ERR)
    implicit none
    !
    !>   @param  psi (nc)  particle shape factor (model dependent)
    !>   @param  sphe(nc)  particle shphericity
    !>   @param  diam(nc)  particle diameter
    !>   @param  modv      settling velocity model
    !>   @param  nc        number of bins
    !>   @param MY_ERR     error handler
    !
    integer(ip),        intent(IN   ) :: nc
    integer(ip),        intent(IN   ) :: modv
    real(rp),           intent(INOUT) :: psi(nc)
    real(rp),           intent(IN   ) :: sphe(nc)
    real(rp),           intent(IN   ) :: diam(nc)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: ic
    real   (rp) :: gama,circula
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_get_psi'
    MY_ERR%message = ' '
    !
    !*** Computes psi
    !
    select case(modv)
    case(MOD_ARASTOPOUR)           ! Arastopour
       !
       psi(1:nc) = 1.0_rp
       !
    case(MOD_GANSER)     ! Ganser
       !
       psi(1:nc) = sphe(1:nc)
       !
    case(MOD_WILSON)     ! Wilson
       !
       do ic = 1,nc
          call phys_get_gama(diam(ic),gama,MY_ERR%flag)  ! Get a/c
          if(MY_ERR%flag /= 0) then
             MY_ERR%message = 'No convergence in phys_get_gama'
             return
          end if
          !
          if(gama >= 1.0_rp) then                ! oblate
             psi(ic) = 0.5_rp*(1.0_rp+1.0_rp/gama)
          else                                ! prolate
             psi(ic) = gama
          end if
       end do
       !
    case(MOD_DELLINO)      ! Dellino
       !
       do ic = 1,nc
          call phys_get_gama(diam(ic),gama,MY_ERR%flag)         ! Get a/c
          if(MY_ERR%flag /= 0) then
             MY_ERR%message = 'No convergence in phys_get_gama'
             return
          end if
          !
          if(gama >= 1.0_rp) then                       ! oblate
             circula = 1.0_rp
             psi(ic) = sphe(ic)/circula
          else                                    ! prolate
             circula = sqrt(gama)                 ! Riley 1941
             psi(ic) = sphe(ic)/circula
          end if
       end do
       !
    case(MOD_PFEIFFER)     ! Pfeiffer
       !
       do ic = 1,nc
          call phys_get_gama(diam(ic),gama,MY_ERR%flag)  ! Get a/c
          if(MY_ERR%flag /= 0) then
             MY_ERR%message = 'No convergence in phys_get_gama'
             return
          end if
          !
          if(gama >= 1.0_rp) then                ! oblate
             psi(ic) = 0.5_rp*(1.0_rp+1.0_rp/gama)
          else                                ! prolate
             psi(ic) = gama
          end if
       end do
       !
    case(MOD_DIOGUARDI2017,MOD_DIOGUARDI2018)     ! Dioguardi 2017, Dioguardi 2018
       !
       psi(1:nc) = sphe(1:nc)
       !
    case default
       !
       MY_ERR%flag = 1
       MY_ERR%message = 'setling velocity model not found '
    end select
    !
    return
  end subroutine phys_get_psi
  !
  !
  !-----------------------------------------
  !    subroutine phys_get_ust
  !-----------------------------------------
  !
  !>   @brief
  !>   Gets friction velocity u*
  !
  subroutine phys_get_ust(is,ie,js,je,z1,z0,Tv1,Tv0,u1,v1,ust,MY_ERR)
    implicit none
    !
    !>   @param is         starting index along x
    !>   @param ie         end      index along x
    !>   @param js         starting index along y
    !>   @param je         end      index along y
    !>   @param z1         z1 (is:ie,js:je)  reference layer height (first layer thickness)
    !>   @param z0         z0 (is:ie,js:je)  base      layer height (roughness length)
    !>   @param tv1        Tv1(is:ie,js:je)  virtual potential temperature at z1
    !>   @param tv0        Tv0(is:ie,js:je)  virtual potential temperature at z0
    !>   @param u1         u1 (is:ie,js:je)  u-velocity at z1
    !>   @param v1         v1 (is:ie,js:je)  v-velocity at z1
    !>   @param ust        ust(is:ie,js:je)  output friction velocity
    !>   @param MY_ERR     error handler
    !
    integer(ip),        intent(IN   ) :: is
    integer(ip),        intent(IN   ) :: ie
    integer(ip),        intent(IN   ) :: js
    integer(ip),        intent(IN   ) :: je
    real(rp),           intent(IN   ) :: z1 (is:ie,js:je)
    real(rp),           intent(IN   ) :: z0 (is:ie,js:je)
    real(rp),           intent(IN   ) :: Tv1(is:ie,js:je)
    real(rp),           intent(IN   ) :: Tv0(is:ie,js:je)
    real(rp),           intent(IN   ) :: u1 (is:ie,js:je)
    real(rp),           intent(IN   ) :: v1 (is:ie,js:je)
    real(rp),           intent(INOUT) :: ust(is:ie,js:je)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j
    real(rp)    :: umod,Rib,Gm
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_get_ust'
    MY_ERR%message = ' '
    !
    do j = js,je
       do i = is,ie
          !
          umod = max(sqrt(u1(i,j)*u1(i,j)+v1(i,j)*v1(i,j)),1e-4_rp)      ! Velocity
          !
          call phys_get_Rib(Tv1(i,j),Tv0(i,j),umod,z1(i,j),z0(i,j),Rib)  ! Bulk Richardson number
          !
          call phys_get_Gm(Rib,z1(i,j),z0(i,j),Gm)
          !
          ust(i,j) = KARMAN*umod*sqrt(Gm)/log(z1(i,j)/z0(i,j))
          !
       end do
    end do
    !
    return
  end subroutine phys_get_ust
  !
  !-----------------------------------------
  !    subroutine phys_get_monin
  !-----------------------------------------
  !
  !>   @brief
  !>   Gets the Monin-Obukhov length
  !
  subroutine phys_get_monin(is,ie,js,je,z1,z0,Tv1,Tv0,u1,v1,ust,mon,MY_ERR)
    implicit none
    !
    !>   @param is         starting index along x
    !>   @param ie         end      index along x
    !>   @param js         starting index along y
    !>   @param je         end      index along y
    !>   @param z1         z1 (is:ie,js:je)  reference layer height (first layer thickness)
    !>   @param z0         z0 (is:ie,js:je)  base      layer height (roughness length)
    !>   @param tv1        Tv1(is:ie,js:je)  virtual potential temperature at z1
    !>   @param tv0        Tv0(is:ie,js:je)  virtual potential temperature at z0
    !>   @param u1         u1 (is:ie,js:je)  u-velocity at z1
    !>   @param v1         v1 (is:ie,js:je)  v-velocity at z1
    !>   @param ust        ust(is:ie,js:je)  friction velocity
    !>   @param mon        mon(is:ie,js:je)  output Monin-Obukhov length
    !>   @param MY_ERR     error handler
    !
    integer(ip),        intent(IN   ) :: is
    integer(ip),        intent(IN   ) :: ie
    integer(ip),        intent(IN   ) :: js
    integer(ip),        intent(IN   ) :: je
    real(rp),           intent(IN   ) :: z1 (is:ie,js:je)
    real(rp),           intent(IN   ) :: z0 (is:ie,js:je)
    real(rp),           intent(IN   ) :: Tv1(is:ie,js:je)
    real(rp),           intent(IN   ) :: Tv0(is:ie,js:je)
    real(rp),           intent(IN   ) :: u1 (is:ie,js:je)
    real(rp),           intent(IN   ) :: v1 (is:ie,js:je)
    real(rp),           intent(IN   ) :: ust(is:ie,js:je)
    real(rp),           intent(INOUT) :: mon(is:ie,js:je)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j
    real(rp)    :: umod,Rib,Gh,thstar
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_get_monin'
    MY_ERR%message = ' '
    !
    do j = js,je
       do i = is,ie
          !
          umod = max(sqrt(u1(i,j)*u1(i,j)+v1(i,j)*v1(i,j)),1e-4_rp)      ! Velocity
          !
          call phys_get_Rib(Tv1(i,j),Tv0(i,j),umod,z1(i,j),z0(i,j),Rib)  ! Bulk Richardson number
          !
          call phys_get_Gh(Rib,z1(i,j),z0(i,j),Gh)
          !
          !*** thstar (Pr=1 assumed)
          !
          thstar = log(z1(i,j)/z0(i,j))
          thstar = KARMAN*KARMAN*umod*(Tv1(i,j)-Tv0(i,j))*sqrt(Gh)/(ust(i,j)*thstar*thstar)
          !
          mon(i,j) = ust(i,j)*ust(i,j)*0.5_rp*(Tv0(i,j)+Tv1(i,j))/(KARMAN*GI*thstar)
          !
          mon(i,j) = min(mon(i,j), 1e3_rp)
          mon(i,j) = max(mon(i,j),-1e3_rp)
          !
          if(abs(mon(i,j)).lt.10) mon(i,j) = sign(10.0_rp,mon(i,j))
       end do
    end do
    !
    return
  end subroutine phys_get_monin
  !
  !-----------------------------------------
  !    subroutine phys_radionuclides
  !-----------------------------------------
  !
  !>   @brief Computes the decay of radionuclides
  !>   @details
  !>   Allowed radionuclides: Cs135, Cs137, I131, Sr90 and Y90.
  !>   @author A.Folch, A.Costa, G.Macedonio
  !
  subroutine phys_radionuclides(MY_TRA,MY_SPE,MY_TIME,MY_ERR)
    implicit none
    !
    !>   @param MY_TRA    Tracers
    !>   @param MY_SPE    Species parameters
    !>   @param MY_TIME   Time specifications
    !>   @param MY_ERR    error handler
    !
    type(TRACERS),        intent(INOUT) :: MY_TRA
    type(SPECIES_PARAMS), intent(IN   ) :: MY_SPE
    type(RUN_TIME),       intent(IN   ) :: MY_TIME
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    real(rp)    :: radio_fact(MY_TRA%MY_BIN%nbins) ! Decay factors
    real(rp)    :: dt             ! Time step
    integer(ip) :: n_sr90_classes ! Number of SR90 classes
    integer(ip) :: n_y90_classes  ! Number of Y90 classes
    integer(ip) :: sr90_y90_shift ! Shift ibin(y90)-ibin(sr90)
    integer(ip) :: ibin,i,j,k
    !
    !*** Initializations
    !
    dt = MY_TIME%gl_dt  ! Time step
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_radionuclides'
    MY_ERR%message = ' '

    !
    ! Y90 is a child of Sr90.
    ! In this version of Fall3d each Sr90 class bin decays in the corresponding
    ! bin of Y90 with a one-to-one mapping and in the same order.
    ! This implies that the number of classes of Sr90 must be the same of the
    ! classes of Y90. Here we only check that the number of both classes is the same.
    ! It is responsability of the user to provide physically consistent classes.
    ! 

    ! Count the number of Y90 and SR90 classes and evaluate the shift between
    ! the indices (ibin) of Sr90 and Y90 classes.
    ! The shift is such that ibin(Sr90) = ibin(Y90) + sr90_y90_shift
    n_y90_classes  = 0
    n_sr90_classes = 0
    sr90_y90_shift = 0
    do ibin = 1,MY_TRA%MY_BIN%nbins
       if(MY_TRA%MY_BIN%bin_cat(ibin) == CAT_RADIONUCLIDE .and. &
            MY_TRA%MY_BIN%bin_spe(ibin) == SPE_Y90) then
          n_y90_classes = n_y90_classes + 1
          if(n_y90_classes == 1) sr90_y90_shift = sr90_y90_shift - ibin  ! child (-)
       end if
       if(MY_TRA%MY_BIN%bin_cat(ibin) == CAT_RADIONUCLIDE .and. &
            MY_TRA%MY_BIN%bin_spe(ibin) == SPE_SR90) then
          n_sr90_classes = n_sr90_classes + 1
          if(n_sr90_classes == 1) sr90_y90_shift = sr90_y90_shift + ibin ! Father (+)
       end if
    end do

    ! Check
    if(n_y90_classes /= n_sr90_classes) then
       MY_ERR%flag    = 1
       MY_ERR%message = 'The number of SR90 and Y90 classes must be equal.'
       return
    end if

    !
    !
    !*** Computes radionuclides decay for all types
    !
    do ibin = 1,MY_TRA%MY_BIN%nbins
       if(MY_TRA%MY_BIN%bin_cat(ibin) == CAT_RADIONUCLIDE) then
          select case (MY_TRA%MY_BIN%bin_spe(ibin))
          case (SPE_CS134)
             radio_fact(ibin) = exp(-kn_CS134*dt)
          case (SPE_CS137)
             radio_fact(ibin) = exp(-kn_CS137*dt)
          case (SPE_I131)
             radio_fact(ibin) = exp(-kn_I131*dt)
          case (SPE_SR90)
             radio_fact(ibin) = exp(-kn_SR90*dt)
          case (SPE_Y90)
             radio_fact(ibin) = exp(-kn_Y90*dt)
          case default
             MY_ERR%flag    = 1
             MY_ERR%message = 'Invalid radionuclide type: '// &
                  trim(MY_SPE%name(MY_TRA%MY_BIN%bin_spe(ibin)))
             return
          end select
       end if
    end do

    ! Loop on all the local cells (no need to exchange halos)
    do ibin = 1,MY_TRA%MY_BIN%nbins
       if(MY_TRA%MY_BIN%bin_cat(ibin) == CAT_RADIONUCLIDE) then
          do i = my_ips_2h,my_ipe_2h
             do j = my_jps_2h,my_jpe_2h
                do k = my_kps_2h,my_kpe_2h
                   MY_TRA%my_c(i,j,k,ibin) = MY_TRA%my_c(i,j,k,ibin)*radio_fact(ibin)
                   ! Y90 also receives from Sr90
                   if(MY_TRA%MY_BIN%bin_spe(ibin) == SPE_Y90) then
                      MY_TRA%my_c(i,j,k,ibin)  = MY_TRA%my_c(i,j,k,ibin+sr90_y90_shift) * &
                      (1.0_rp - radio_fact(ibin+sr90_y90_shift))
                   end if
                end do
             end do
          end do
       end if
    end do
    !
    !*** Does not need to exchange halos
    !
    return
  end subroutine phys_radionuclides
  !
  !-----------------------------------------
  !    subroutine phys_wet_deposition
  !-----------------------------------------
  !
  !>   @brief
  !>   Computes wet deposition mechanisms according to Jung and Shao (2006)
  !>   @details
  !>   Wet deposition is assumed below the PBL only. This is done because there is no
  !>   informtion about the height of the precipitation (only the total rate is known).
  !>       dC/dt = -L*C = a*(P**b)*C   P in mm/h
  !>       a = 8.4d-5
  !>       b = 0.79
  !>       P precipitation rate in mmh-1
  !
  subroutine phys_wet_deposition(MY_MOD,dt,my_pre,my_pblh,my_zc,dX3_p,my_awet,my_c,MY_ERR)
    implicit none
    !
    !>   @param MY_MOD    model physics related parameters
    !>   @param dt        integration time step
    !>   @param my_pre    precipitation rate          at mass points
    !>   @param my_pblh   boundary layer height       at mass points
    !>   @param my_zc     z-coordinate at my processor cell corners
    !>   @param dX3_p     dX3                         at mass points
    !>   @param my_awet   accumulated wet deposition  at mass points
    !>   @param my_c      scaled bin concentration    at mass points
    !>   @param MY_ERR    error handler
    !
    type(MODEL_PHYS),     intent(IN   ) :: MY_MOD
    real(rp),             intent(IN   ) :: dt
    real(rp),             intent(IN   ) :: my_pre (my_ips:my_ipe,      my_jps:my_jpe)
    real(rp),             intent(IN   ) :: my_pblh(my_ips:my_ipe,      my_jps:my_jpe)
    real(rp),             intent(IN   ) :: my_zc  (my_ibs:my_ibe,      my_jbs:my_jbe,      my_kbs:my_kbe)
    real(rp),             intent(IN   ) :: dX3_p  (my_kps_2h:my_kpe_2h)
    real(rp),             intent(INOUT) :: my_awet(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h)
    real(rp),             intent(INOUT) :: my_c   (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h)
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i,j,k
    real(rp)    :: a,b,lambda,zp,pblh
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_wet_deposition'
    MY_ERR%message = ' '
    !
    a = MY_MOD%wet_deposition_a
    b = MY_MOD%wet_deposition_b
    !
    !*** Computes wet deposition
    !
    do j = my_jps,my_jpe
       do i = my_ips,my_ipe
          !
          lambda = min(1.0_rp, dt*a*(my_pre(i,j)**b))
          pblh   = my_pblh(i,j)
          !
          do k = my_kps,my_kpe
             zp = 0.5_rp*(my_zc(i,j,k)+my_zc(i,j,k+1))    ! z-coordinate at mass point
             if(zp.le.pblh) then
                my_awet(i,j) = my_awet(i,j) + dX3_p(k)*lambda*my_c(i,j,k)
                my_c(i,j,k)  = my_c(i,j,k)*(1.0_rp-lambda)
                !
             end if
          end do
       end do
    end do
    !
    !*** Exchange halos
    !
    call domain_swap_mass_points_2halo_x ( my_c )
    call domain_swap_mass_points_2halo_y ( my_c )
    call domain_swap_mass_points_2halo_z ( my_c )
    !
    return
  end subroutine phys_wet_deposition
  !
  !-----------------------------------------
  !    subroutine phys_dry_deposition
  !-----------------------------------------
  !
  !>   @brief
  !>   Computes the dry deposition velocity according to Feng
  !>   @details
  !>   J.Feng, A size-resolved model and a four-mode parameterization
  !>   of dry deposition of atmospheric aerosols, JGR, v113, 2008.
  !
  !    Computation is done only for particles smaller than 100 mic
  !    (the limit of the aerosol Giant mode). The Feng model discriminates
  !    between 6 land-use categories:
  !    1 'Urban'
  !    2 'Remote continental'
  !    3 'Desert'
  !    4 'Polar'
  !    5 'Marine'
  !    6 'Rural'
  !
  !    whereas the DBS is based on the USGS 24-categories. We assume
  !    the following simplifications:
  !
  !               USGS                                   Equivalent
  !    ------------------------------------------------------------
  !    1 'Urban and Built-Up Land'                         1
  !    2 'Dryland Cropland and Pasture'                    6
  !    3 'Irrigated Cropland and Pasture'                  6
  !    4 'Mixed Dryland/Irrigated Cropland and Pasture'    6
  !    5 'Cropland/Grassland Mosaic'                       6
  !    6 'Cropland/Woodland Mosaic'                        6
  !    7 'Grassland'                                       6
  !    8 'Shrubland'                                       6
  !    9 'Mixed Shrubland/Grassland'                       6
  !   10 'Savanna'                                         3
  !   11 'Deciduous Broadleaf Forest'                      6
  !   12 'Deciduous Needleleaf Forest'                     6
  !   13 'Evergreen Broadleaf Forest'                      6
  !   14 'Evergreen Needleleaf Forest'                     6
  !   15 'Mixed Forest'                                    6
  !   16 'Water Bodies'                                    5
  !   17 'Herbaceous Wetland'                              6
  !   18 'Wooded Wetland'                                  6
  !   19 'Barren or Sparsely Vegetated'                    3
  !   20 'Herbaceous Tundra'                               2
  !   21 'Wooded Tundra'                                   2
  !   22 'Mixed Tundra'                                    2
  !   23 'Bare Ground Tundra'                              2
  !   24 'Snow or Ice'                                     4
  !
  subroutine phys_dry_deposition(MY_MOD, my_rhoc, my_tc, my_zc, my_ustc, my_lusec, MY_BIN, my_vs, MY_ERR)
    implicit none
    !
    !>   @param MY_MOD    model physics related parameters
    !>   @param my_rhoc   air density     at corner points
    !>   @param my_tc     air temperature at corner points
    !>   @param my_zc     z-coordinate at my processor cell corners
    !>   @param my_ustc   friction veloc  at corner points
    !>   @param my_lusec  land use catergories (24 USGS bins assumed)
    !>   @param MY_BIN    list of parameters defining bin granulometric properties
    !>   @param my_vs     settling velocity at w-boundaries
    !>   @param MY_ERR    error handler
    !
    type(MODEL_PHYS),     intent(INOUT) :: MY_MOD
    real(rp),             intent(IN   ) :: my_rhoc (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    real(rp),             intent(IN   ) :: my_tc   (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    real(rp),             intent(IN   ) :: my_zc   (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe)
    real(rp),             intent(IN   ) :: my_ustc (my_ibs:my_ibe, my_jbs:my_jbe)
    real(rp),             intent(IN   ) :: my_lusec(my_ibs:my_ibe, my_jbs:my_jbe)
    type(BIN_PARAMS),     intent(IN   ) :: MY_BIN
    real(rp),             intent(INOUT) :: my_vs  (my_ips:my_ipe, my_jps:my_jpe, my_kbs_1h:my_kbe_1h,1:MY_BIN%nbins)
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    integer(ip), save              :: ipass = 0
    integer(ip), save, allocatable :: my_iland(:,:)
    !
    integer(ip) :: ibin,i,j,iland,imode
    real(rp)    :: ust,ra,T,visc,rho,z1,z2,Re,Vd1,Vd,a,b
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'phys_dry_deposition'
    MY_ERR%message = ' '
    !
    !*** This is only for ground processors
    !
    if(my_kbs.ne.1) return
    !
    !*** First time initialize values of constants a/b and aerosol mode
    !
    if(ipass == 0) then
       ipass = 1
       !
       !***  a(iland,imode) and b(iland,imode)
       !
       MY_MOD%dry_deposition_a(1,1) = 0.0048_rp      ! Mode 1
       MY_MOD%dry_deposition_a(2,1) = 0.0037_rp
       MY_MOD%dry_deposition_a(3,1) = 0.0042_rp
       MY_MOD%dry_deposition_a(4,1) = 0.0032_rp
       MY_MOD%dry_deposition_a(5,1) = 0.0043_rp
       MY_MOD%dry_deposition_a(6,1) = 0.0045_rp
       MY_MOD%dry_deposition_b(1,1) = 1.0_rp
       MY_MOD%dry_deposition_b(2,1) = 1.0_rp
       MY_MOD%dry_deposition_b(3,1) = 1.0_rp
       MY_MOD%dry_deposition_b(4,1) = 1.0_rp
       MY_MOD%dry_deposition_b(5,1) = 1.0_rp
       MY_MOD%dry_deposition_b(6,1) = 1.0_rp
       !
       MY_MOD%dry_deposition_a(1,2) = 0.0315_rp     ! Mode 2
       MY_MOD%dry_deposition_a(2,2) = 0.0120_rp
       MY_MOD%dry_deposition_a(3,2) = 0.2928_rp
       MY_MOD%dry_deposition_a(4,2) = 0.1201_rp
       MY_MOD%dry_deposition_a(5,2) = 0.1337_rp
       MY_MOD%dry_deposition_a(6,2) = 0.0925_rp
       MY_MOD%dry_deposition_b(1,2) = 2.7925_rp
       MY_MOD%dry_deposition_b(2,2) = 2.2413_rp
       MY_MOD%dry_deposition_b(3,2) = 3.8581_rp
       MY_MOD%dry_deposition_b(4,2) = 3.4407_rp
       MY_MOD%dry_deposition_b(5,2) = 3.5456_rp
       MY_MOD%dry_deposition_b(6,2) = 3.2920_rp
       !
       MY_MOD%dry_deposition_a(1,3) = 1.2891_rp     ! Mode 3
       MY_MOD%dry_deposition_a(2,3) = 1.3977_rp
       MY_MOD%dry_deposition_a(3,3) = 1.3970_rp
       MY_MOD%dry_deposition_a(4,3) = 1.1838_rp
       MY_MOD%dry_deposition_a(5,3) = 1.2834_rp
       MY_MOD%dry_deposition_a(6,3) = 1.2654_rp
       MY_MOD%dry_deposition_b(1,3) = 2.6878_rp
       MY_MOD%dry_deposition_b(2,3) = 2.5838_rp
       MY_MOD%dry_deposition_b(3,3) = 2.5580_rp
       MY_MOD%dry_deposition_b(4,3) = 2.8033_rp
       MY_MOD%dry_deposition_b(5,3) = 2.7157_rp
       MY_MOD%dry_deposition_b(6,3) = 2.7227_rp
       !
       MY_MOD%dry_deposition_a(1,4) = 1.0338_rp     ! Mode 4
       MY_MOD%dry_deposition_a(2,4) = 1.0707_rp
       MY_MOD%dry_deposition_a(3,4) = 0.9155_rp
       MY_MOD%dry_deposition_a(4,4) = 1.0096_rp
       MY_MOD%dry_deposition_a(5,4) = 1.1595_rp
       MY_MOD%dry_deposition_a(6,4) = 1.0891_rp
       MY_MOD%dry_deposition_b(1,4) = 1.2644_rp
       MY_MOD%dry_deposition_b(2,4) = 1.3247_rp
       MY_MOD%dry_deposition_b(3,4) = 1.0364_rp
       MY_MOD%dry_deposition_b(4,4) = 1.2069_rp
       MY_MOD%dry_deposition_b(5,4) = 1.4863_rp
       MY_MOD%dry_deposition_b(6,4) = 1.4240_rp
       !
       !  Loop over bins to averiguate the mode (including particles)
       !
       allocate(MY_MOD%dry_deposition_mode(MY_BIN%nbins))
       do ibin = 1,MY_BIN%nbins
          if(MY_BIN%bin_diam(ibin) > 100e-6_rp) then        ! > 100 min not considered
             MY_MOD%dry_deposition_mode(ibin) = 0
          else if(MY_BIN%bin_diam(ibin) > 10e-6_rp) then    ! > 10  mic   giant mode
             MY_MOD%dry_deposition_mode(ibin) = 1
          else if(MY_BIN%bin_diam(ibin) > 2.5e-6_rp) then   ! > 2.5 mic   coarse mode
             MY_MOD%dry_deposition_mode(ibin) = 2
          else if(MY_BIN%bin_diam(ibin) > 0.1e-6_rp) then   ! > 0.1 mic   accumulation mode
             MY_MOD%dry_deposition_mode(ibin) = 3
          else                                              ! nucleii mode
             MY_MOD%dry_deposition_mode(ibin) = 4
          end if
       end do
       !
       !  Get the land coefficient
       !
       allocate(my_iland(my_ips:my_ipe, my_jps:my_jpe))
       do j = my_jps,my_jpe
          do i = my_ips,my_ipe
             !
             select case(INT(my_lusec(i,j)))   ! NOTE: nearest point; it is actually on boundaries
             case(1)
                my_iland(i,j) = 1
             case(2,3,4,5,6,7,8,9,11,12,13,14,15,17,18)
                my_iland(i,j) = 6
             case(10,19)
                my_iland(i,j) = 3
             case(16)
                my_iland(i,j) = 5
             case(20,21,22,23)
                my_iland(i,j) = 2
             case(24)
                my_iland(i,j) = 4
             case default
                my_iland(i,j) = 6
             end select
          end do
       end do
       !
    end if   ! ipass = 0
    !
    !
    do j = my_jps,my_jpe
       do i = my_ips,my_ipe
          !
          !*** Compute aerodynamic resistance. Since this is computed at
          !*** z=z0 we make no disctintion between stable, neutral and unstable
          !
          ust = 0.25_rp*(my_ustc(i,j) + my_ustc(i+1,j) + my_ustc(i+1,j+1) + my_ustc(i,j+1))
          if(ust > 0.0_rp) then
             ra = 1.0_rp/(KARMAN*ust)
          else
             ra = 1e20_rp   ! prevent overflow
          end if
          !
          !*** Air viscosity (Sutherland's law)
          !
          T    = 0.25_rp*(my_tc(i,j,1) + my_tc(i+1,j,1) + my_tc(i+1,j+1,1) + my_tc(i,j+1,1))
          visc = visa0*((Ta0+120.0_rp)/(T+120.0_rp))*((T/Ta0)**1.5_rp)
          !
          !*** Re*
          !
          rho = 0.25_rp*(my_rhoc(i,j,1) + my_rhoc(i+1,j,1) + my_rhoc(i+1,j+1,1) + my_rhoc(i,j+1,1))
          z1  = 0.25_rp*(my_zc  (i,j,1) + my_zc  (i+1,j,1) + my_zc  (i+1,j+1,1) + my_zc  (i,j+1,1))
          z2  = 0.25_rp*(my_zc  (i,j,2) + my_zc  (i+1,j,2) + my_zc  (i+1,j+1,2) + my_zc  (i,j+1,2))
          Re  = ust*(z2-z1)*0.5_rp*rho/visc
          !
          !*** Fist term of deposition velocity
          !
          Vd1 = -0.5_rp*((Re-40300.0_rp)/15330.0_rp)*((Re-40300.0_rp)/15330.0_rp)
          Vd1 = 0.0226_rp*ust*exp(Vd1)
          !
          !*** Loop over bins
          !
          do ibin = 1,MY_BIN%nbins
             if(MY_MOD%dry_deposition_mode(ibin) > 0) then
                imode = MY_MOD%dry_deposition_mode(ibin)
                iland = my_iland(i,j)
                a     = MY_MOD%dry_deposition_a(iland,imode)
                b     = MY_MOD%dry_deposition_b(iland,imode)
                !
                Vd = Vd1 + a*(ust**b)
                Vd = 1.0_rp/(ra + 1.0_rp/Vd)    ! deposition velocity
                !
                my_vs(i,j, my_kbs_1h:my_kbs,ibin) = my_vs(i,j, my_kbs_1h:my_kbs,ibin) + Vd
                !
             end if
          end do
          !
       end do
    end do
    !
    return
  end subroutine phys_dry_deposition
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  !-----------------------------------------
  !    subroutine phys_get_Rib
  !    Gets the Richardson bulk number
  !-----------------------------------------
  !
  subroutine phys_get_Rib(Tv1,Tv0,umod,z1,z0,Rib)
    implicit none
    !
    real(rp), intent(IN   ) :: Tv1,Tv0,umod,z1,z0
    real(rp), intent(INOUT) :: Rib
    !
    Rib = GI*(z1-z0)*(Tv1-Tv0)/(umod*umod*0.5_rp*(Tv1+Tv0))   ! Richardson bulk
    !
    return
  end subroutine phys_get_Rib
  !
  !-----------------------------------------
  !    subroutine phys_get_Fih
  !-----------------------------------------
  !
  subroutine phys_get_Fih(z,L,fih)
    implicit none
    !
    real(rp), intent(IN   ) :: z,L
    real(rp), intent(INOUT) :: fih
    !
    real(rp), parameter :: gama = 15.0_rp
    real(rp), parameter :: beta =  5.0_rp
    real(rp)            :: zl
    !
    zl = z/L
    !
    if(zl.lt.0.0_rp) then                  ! unstable
       fih = 1.0_rp/sqrt(1.0_rp-gama*zl)
    else                                   ! stable & neutral
       fih = 1.0_rp + beta*zl
    end if
    !
    return
  end subroutine phys_get_Fih
  !
  !-----------------------------------------
  !    subroutine phys_get_Fc
  !-----------------------------------------
  !
  subroutine phys_get_Fc(Ri,fc)
    implicit none
    !
    real(rp), intent(IN   ) :: Ri
    real(rp), intent(INOUT) :: fc
    !
    if(Ri.gt.0.0_rp) then                  ! stable
       fc = 1.0_rp + 10.0_rp*Ri*(1.0_rp+8.0_rp*Ri)
       fc = 1.0_rp/fc
    else                                   ! unstable
       fc = sqrt(1.0_rp-18.0_rp*Ri)
    end if
    !
    return
  end subroutine phys_get_Fc
  !
  !-----------------------------------------
  !    subroutine phys_get_Gm
  !-----------------------------------------
  !
  subroutine phys_get_Gm(Rib,z1,z0,Gm)
    implicit none
    !
    real(rp), intent(IN   ) :: Rib,z1,z0
    real(rp), intent(INOUT) :: Gm
    !
    !*** Stable/Unstable ABL
    !
    if(Rib.ge.0.0_rp) then
       !                             Stable
       Gm = 1.0_rp + 4.7_rp*Rib
       Gm = 1.0_rp/(Gm*Gm)
    else
       !                             Unstable
       Gm = log(z1/z0)
       Gm = 1.0_rp + (70.0_rp*KARMAN*KARMAN*sqrt(abs(Rib)*z1/z0))/(Gm*Gm)
       Gm = 1.0_rp - ((9.4_rp*Rib)/Gm)
    end if
    !
    return
  end subroutine phys_get_Gm
  !
  !-----------------------------------------
  !    subroutine phys_get_Gh
  !-----------------------------------------
  !
  subroutine phys_get_Gh(Rib,z1,z0,Gh)
    implicit none
    !
    real(rp), intent(IN   ) :: Rib,z1,z0
    real(rp), intent(INOUT) :: Gh
    !
    !*** Stable/Unstable ABL
    !
    if(Rib.ge.0.0_rp) then
       !                             Stable
       Gh = 1.0_rp + 4.7_rp*Rib
       Gh = 1.0_rp/(Gh*Gh)
    else
       !                             Unstable
       Gh = log(z1/z0)
       Gh = 1.0_rp + (50.0_rp*KARMAN*KARMAN*sqrt(abs(Rib)*z1/z0))/(Gh*Gh)
       Gh = 1.0_rp - ((9.4_rp*Rib)/Gh)
    end if
    !
    return
  end subroutine phys_get_Gh
  !
  !-----------------------------------------
  !    subroutine phys_get_gama
  !
  !     Gets gama = a/c
  !
  !     Prolate spheroid: a=minor semi-axis, c=major semi-axis
  !
  !     NOTE: In all cases it is assumed that particles are prolate ellipsoids
  !
  !     a = b < c  prolate   (gama < 1)
  !
  !     The inversion of the area of the ellipsoid is done numerically.
  !     Area given by:
  !
  !     A = 2*pi*(a**2 + c**2*e/tan(e) )   e = acos(gama)
  !     d = 2*c*gama**(2/3)               (prolate)
  !
  !     NOTE: particle diameter is multiplied by a factor. It does not affect
  !           results (a/c) and is done to facilitate convergence and prevent
  !           propagation of rounding errors (e.g. for micron size particles
  !           diam of the order 1d-6 rised to 2 or 3)
  !
  !     ierr - Return status (0=OK, 1=No convergence)
  !-----------------------------------------
  !
  subroutine phys_get_gama(diam,gama,ierr)
    implicit none
    !
    real(rp),    intent(in)  :: diam
    real(rp),    intent(out) :: gama
    integer(ip), intent(out) :: ierr
    !
    integer(ip) :: iiter,niter
    real   (rp) :: d,gmin,gmax,Ao,toler,e
    real   (rp) :: Ap
    !
    !*** Initializations
    !
    ierr  = 0
    d     = diam*1e3_rp         ! see NOTE
    niter = 1000
    toler = 1e-8_rp
    gmin  = 1e-3_rp
    gmax  = 1.0_rp
    !
    !*** Surface area
    !
    Ap = 4.0_rp*PI*(0.5_rp*d)**2 ! d=diameter of the sphere with same volume
    !
    !*** Iterates
    !
    do iiter = 1,niter
       gama = 0.5_rp*(gmin+gmax)
       e    = acos(gama)
       Ao   = 0.5_rp*PI*d*d*(gama**(-4.0_rp/3.0_rp))*(gama*gama + (e/tan(e)))
       if(Ao < Ap) then
          gmax = gama
       else
          gmin = gama
       end if
       if((iiter > 1).and.(abs(Ao-Ap) < toler)) return  ! Convergence
    end do
    !
    !*** No convergence
    !
    ierr = 1
    return
    !
  end subroutine phys_get_gama
  !
  !
  !
END MODULE Phys
