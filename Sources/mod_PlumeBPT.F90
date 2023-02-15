!***************************************************************
!>
!> Solves the 1-D radially-averaged equations for a plume
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE PlumeBPT
  use KindType
  use StdAtm
  use Phys
  use Time
  implicit none
  save
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: plumeBPT_initialize_plume
  PUBLIC :: plumeBPT_initialize_wind
  PUBLIC :: plumeBPT_solve_plume
  PUBLIC :: plumeBPT_write_plumeprop
  !
  PUBLIC :: dfds
  PUBLIC :: jac
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: set_latent_heat_flag
  PRIVATE :: enthalpy_particles
  PRIVATE :: enthalpy_air
  PRIVATE :: enthalpy_vapour
  PRIVATE :: enthalpy_liquid
  PRIVATE :: enthalpy_ice
  PRIVATE :: density_air
  PRIVATE :: density_vapour
  PRIVATE :: get_gas_molar_fractions
  PRIVATE :: saturation_pressure_over_liquid
  PRIVATE :: saturation_pressure_over_ice
  PRIVATE :: temperature_mixture
  PRIVATE :: enthalpy_mixture
  PRIVATE :: Ta
  PRIVATE :: Pa
  PRIVATE :: Va
  PRIVATE :: mua
  PRIVATE :: Da
  PRIVATE :: sh_a
  PRIVATE :: dVadz
  PRIVATE :: is_collapse
  PRIVATE :: getplume
  PRIVATE :: setplume
  PRIVATE :: splum
  PRIVATE :: get_entrainment_coef
  PRIVATE :: costa
  !
  interface rhoa
     module procedure rhoa_z
     module procedure rhoa_z_T
  end interface rhoa
  !
  !    LIST OF TYPES IN THE MODULE
  !
  type volcanic_plume
     !
     logical               :: aggregation
     logical               :: moist_air
     logical               :: wind_coupling
     logical               :: reentrainment
     logical               :: latent_heat
     !
     character(len=s_name) :: type_aggr
     character(len=s_name) :: solve_for
     character(len=s_name) :: type_as    ! CONSTANT (value jet, value plume) / KAMINSKI-R / KAMINSKI-C / OLD
     character(len=s_name) :: type_av    ! CONSTANT (value) / TATE
     character(len=s_name) :: umbrella_model  ! SPARKS1986
     !
     integer(ip)           :: status     ! Status code
     integer(ip)           :: neq        ! number of equations for lsode
     integer(ip)           :: ns         ! number of plume source points (plume+umbrella)
     integer(ip)           :: np         ! number of plume source points (plume)
     integer(ip)           :: nc         ! number of particle classes
     integer(ip)           :: modv       ! terminal velocity model
     !
     real(rp)              :: xv_UTM     ! Vent UTM-X coordinate
     real(rp)              :: yv_UTM     ! Vent UTM-Y coordinate
     real(rp)              :: zv         ! vent altitude (m)
     real(rp)              :: n_MFR(2)   ! MER search range
     real(rp)              :: MER        ! MER (in kg/s) during this phase
     real(rp)              :: mass       ! Total erupted mass (in kg)
     real(rp)              :: xi         ! Factor (Bursik 2001).
     real(rp)              :: zmin_wind  ! Ignore wind entrainment below this zvalue (low jet region)
     real(rp)              :: c_umbrella ! Thickness of umbrella relative to Hb (>1)
     real(rp)              :: a_s_jet    ! Default (constant) value in jet   region
     real(rp)              :: a_s_plume  ! Default (constant) value in plume region
     real(rp)              :: a_v        ! Default (constant) value
     !
     real(rp), allocatable :: x   (:)    ! lon(ns)
     real(rp), allocatable :: y   (:)    ! lat(ns)
     real(rp), allocatable :: z   (:)    ! z(ns) (elevation in m above terrain)
     real(rp), allocatable :: Q   (:)    ! Bulk mass flow rate (ns)
     real(rp), allocatable :: En  (:)    ! Total energy flow rate (ns)
     real(rp), allocatable :: M   (:,:)  ! Particle mass flow rate (nc,ns)
     real(rp), allocatable :: Mf  (:,:)  ! Mass flow rate of particles that fall from the eruption column (nc,ns)
     real(rp), allocatable :: Magr(:,:)  ! Mass flow rate of particles that aggregate (nc,ns)
     real(rp), allocatable :: Mair(:)    ! Mass flow rate of air (ns)
     real(rp), allocatable :: Mw  (:)    ! Mass flow rate of volatiles (vapor + liquid + ice) (ns)
     real(rp), allocatable :: L   (:)    ! Coordinate s (along the plume centerline)
     real(rp), allocatable :: H   (:)    ! Theta angle
     real(rp), allocatable :: U   (:)    ! Bulk velocity
     real(rp), allocatable :: T   (:)    ! Bulk temperature
     real(rp), allocatable :: D   (:)    ! Bulk density
     real(rp), allocatable :: D_rhoa(:)  ! Bulk density/air density
     real(rp), allocatable :: R   (:)    ! Plume radius
     real(rp), allocatable :: xv  (:)    ! water vapor  mass fraction
     real(rp), allocatable :: xl  (:)    ! liquid water mass fraction
     real(rp), allocatable :: xs  (:)    ! ice (solid)  mass fraction
     real(rp), allocatable :: as  (:)    ! a_shear
     real(rp), allocatable :: av  (:)    ! a_vortex
     !
  end type volcanic_plume
  !
  type particle_properties
     !
     integer(ip)           :: iaggr = 0
     real(rp)              :: vset_aggr
     real(rp)              :: Dfo
     real(rp)              :: diam_aggr
     !
     real(rp), allocatable :: fc    (:)                ! fc  (nc)
     real(rp), allocatable :: rhop  (:)                ! rhop(nc)
     real(rp), allocatable :: diam  (:)                ! diam(nc)
     real(rp), allocatable :: psi   (:)                ! psi (nc)
     real(rp), allocatable :: vlimit(:)                ! vlimit(nc)
     !
  end type particle_properties
  !
  type meteo_data
     !
     logical               :: standard_atmosphere = .false.
     integer(ip)           :: nz
     real(rp), allocatable :: z(:)       ! Height (a.s.l.)
     real(rp), allocatable :: rho(:)     ! Air density
     real(rp), allocatable :: p(:)       ! Pressure
     real(rp), allocatable :: T(:)       ! Temperature
     real(rp), allocatable :: sh(:)      ! Specific humidity
     real(rp), allocatable :: u(:)       ! Wind speed
     real(rp), allocatable :: PSIa(:)    ! Wind direction
     !
  end type meteo_data
  !
  !    LIST OF PRIVATE VARIABLES  IN THE MODULE
  !
  type(volcanic_plume)     , private :: plume
  type(particle_properties), private :: part
  type(meteo_data),          private :: profile
  !
  real(rp), private, allocatable :: A_p(:)  ! aggregation coefficients
  real(rp), private, allocatable :: A_m(:)  ! aggregation coefficients
  real(rp), private, allocatable :: fo (:)  ! work arrays for BC at inlet and solutions
  real(rp), private, allocatable :: f  (:)  ! work arrays for BC at inlet and solutions
  real(rp), private :: s_old
  real(rp), private :: r_old
  real(rp), private :: R0
  real(rp), private :: u0
  real(rp), private :: w0
  real(rp), private :: P0
  real(rp), private :: rho0
  real(rp), private :: rhopart0
  real(rp), private :: Enthalp0
  !
  !*** Constants
  !
  real(rp), private :: Cw                    ! water (generic) used if latent_heat=.false.
  real(rp), private :: Cp
  real(rp), private :: Ca
  real(rp), private :: Cv
  real(rp), private :: Cl
  real(rp), private :: Cs
  !
  real(rp), private :: rhol = 1000.0_rp      ! liquid water density
  real(rp), private :: rhos =  920.0_rp      ! ice density
  !
  real(rp), private, parameter :: runiv   = 8.314472_rp    ! Universal gas constant (J/K mole)
  real(rp), private, parameter :: pmolw   = 0.018_rp       ! Molecular weight of water (kg/mole)
  real(rp), private, parameter :: rvapour = runiv/pmolw    ! Gas constant of water vapour
  real(rp), private, parameter :: pmola   = 0.029_rp       ! Molecular weight of air (kg/mole)
  real(rp), private, parameter :: rair    = runiv/pmola    ! Gas constant of air
  real(rp), private, parameter :: Tref    = 273.16_rp      ! Reference temperature (Triple point)
  real(rp), private, parameter :: Pref    = 611.22_rp      ! Reference pressure    (Triple point)
  real(rp), private, parameter :: kb      = 1.38e-23_rp    ! Boltzmann constant
  !
  real(rp), private, parameter :: L_sl = 3.337e5_rp     ! Latent heat of ice->liquid
  real(rp), private, parameter :: L_lv = 2.501e6_rp     ! Latent heat of liquid->vapour
  real(rp), private, parameter :: hs0  = Tref*CS0       ! Enthalpy of ice at T=Tref (it is set such that enthalpy_ice(0)=0)
  real(rp), private            :: hl0  = hs0+L_sl       ! Enthalpy of liquid at T=Tref. Default value for latent_heat=.true.
  real(rp), private            :: hv0  = hs0+L_sl+L_lv  ! Enthalpy of vapour at T=Tref. Default value for latent_heat=.true.
  !
  integer(ip), parameter :: STATUS_OK       = 0
  integer(ip), parameter :: STATUS_COLLAPSE = 1  ! Column collapse
  integer(ip), parameter :: STATUS_ERROR    = 2  ! Generic error
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine plumeBPT_initialize_plume
  !-----------------------------------------
  !
  !>   @brief
  !>   Initialize the structures of the module and allocates memory
  !
  subroutine plumeBPT_initialize_plume(&
       nc,                   &
       fc,                   &
       diam,                 &
       rhop,                 &
       psi,                  &
       vset_aggr,            &
       diam_aggr,            &
       Dfo,                  &
                                !
       Cww,                  &
       Cpp,                  &
       Caa,                  &
                                !
       plume_ns,             &
       plume_np,             &
       plume_modv,           &
       plume_xv_UTM,         &
       plume_yv_UTM,         &
       plume_zv,             &
       plume_n_MFR,          &
       plume_xi,             &
       plume_zmin_wind,      &
       plume_c_umbrella,     &
       plume_a_s_jet,        &
       plume_a_s_plume,      &
       plume_a_v,            &
       plume_aggregation,    &
       plume_moist_air,      &
       plume_wind_coupling,  &
       plume_reentrainment,  &
       plume_latent_heat,    &
       plume_type_aggr,      &
       plume_solve_for,      &
       plume_type_as,        &
       plume_type_av,        &
       plume_umbrella_model, &
       MY_ERR )
    implicit none
    !
    !>   @param nc                   number of particle classes
    !>   @param fc                   particle class mass fraction
    !>   @param diam                 particle class diameter
    !>   @param rhop                 particle class density
    !>   @param psi                  particle shape factor (depending on velocity model)
    !>   @param vset_aggr            factor multiplying setling velocity of aggregates
    !>   @param diam_aggr            aggregate diameter
    !>   @param Dfo                  fractal exponent for aggregation
    !>   @param Cww                  specific heat of water      at constant pressure
    !>   @param Cpp                  specific heat of pyroclasts at constant pressure
    !>   @param Caa                  specific heat of air        at constant pressure
    !>   @param plume_ns             number of plume sources (plume+umbrella)
    !>   @param plume_np             number of plume sources (with no umbrella)
    !>   @param modv                 terminal velocity model
    !>   @param plume_xv_UTM         vent UTM-x coordinate
    !>   @param plume_yv_UTM         vent UTM-y coordinate
    !>   @param plume_zv             vent altitude (m)
    !>   @param plume_n_MFR          MER search range
    !>   @param xi                   factor (Bursik 2001).
    !>   @param plume_zmin_wind      ignore wind entrainment below this zvalue (low jet region)
    !>   @param plume_c_umbrella     thickness of umbrella relative to Hb (>1)
    !>   @param plume_a_s_jet        default (constant) value in jet   region
    !>   @param plume_a_s_plume      default (constant) value in plume region
    !>   @param plume_a_v            default (constant) value
    !>   @param plume_aggregation    plume_aggregation   flag
    !>   @param plume_moist_air      plume_moist_air     flag
    !>   @param plume_wind_coupling  plume_wind_coupling flag
    !>   @param plume_reentrainment  plume_reentrainment flag
    !>   @param plume_latent_heat    plume_latent_heat   flag
    !>   @param plume_type_aggr      aggregation model: NONE / PERCENTAGE / CORNELL / COSTA
    !>   @param plume_solve_for      plume solving strategy
    !>   @param plume_type_as        default parameterization type for as
    !>   @param plume_type_av        default parameterization type for av
    !>   @param plume_umbrella_model default umbrella model
    !>   @param MY_ERR               error handler
    !
    integer(ip)          , intent(IN   ) :: nc
    real(rp)             , intent(IN   ) :: fc  (nc)
    real(rp)             , intent(IN   ) :: diam(nc)
    real(rp)             , intent(IN   ) :: rhop(nc)
    real(rp)             , intent(IN   ) :: psi (nc)
    real(rp)             , intent(IN   ) :: vset_aggr
    real(rp)             , intent(IN   ) :: diam_aggr
    real(rp)             , intent(IN   ) :: Dfo
    !
    real(rp)             , intent(IN   ) :: Cww
    real(rp)             , intent(IN   ) :: Cpp
    real(rp)             , intent(IN   ) :: Caa
    !
    integer(ip)          , intent(IN   ) :: plume_ns
    integer(ip)          , intent(IN   ) :: plume_np
    integer(ip)          , intent(IN   ) :: plume_modv
    real(rp)             , intent(IN   ) :: plume_xv_UTM
    real(rp)             , intent(IN   ) :: plume_yv_UTM
    real(rp)             , intent(IN   ) :: plume_zv
    real(rp)             , intent(IN   ) :: plume_n_MFR(2)
    real(rp)             , intent(IN   ) :: plume_xi
    real(rp)             , intent(IN   ) :: plume_zmin_wind
    real(rp)             , intent(IN   ) :: plume_c_umbrella
    real(rp)             , intent(IN   ) :: plume_a_s_jet
    real(rp)             , intent(IN   ) :: plume_a_s_plume
    real(rp)             , intent(IN   ) :: plume_a_v
    logical              , intent(IN   ) :: plume_aggregation
    logical              , intent(IN   ) :: plume_moist_air
    logical              , intent(IN   ) :: plume_wind_coupling
    logical              , intent(IN   ) :: plume_reentrainment
    logical              , intent(IN   ) :: plume_latent_heat
    character(len=s_name), intent(IN   ) :: plume_type_aggr
    character(len=s_name), intent(IN   ) :: plume_solve_for
    character(len=s_name), intent(IN   ) :: plume_type_as
    character(len=s_name), intent(IN   ) :: plume_type_av
    character(len=s_name), intent(IN   ) :: plume_umbrella_model
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip), save :: ipass = 0
    integer(ip)       :: ic
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'plumeBPT_initialize_plume'
    MY_ERR%message = ' '
    !
    !*** If necessary deallocates memory. This is done in order to call plumeBPT
    !*** with different granulometries or solving strategies
    !
    if(ipass == 0) then
       ipass = 1
    else
       deallocate(part%fc)
       deallocate(part%diam)
       deallocate(part%rhop)
       deallocate(part%psi)
       deallocate(part%vlimit)
       deallocate(A_p)
       deallocate(A_m)
       !
       deallocate(plume%x)
       deallocate(plume%y)
       deallocate(plume%z)
       deallocate(plume%L)
       deallocate(plume%H)
       deallocate(plume%U)
       deallocate(plume%T)
       deallocate(plume%D)
       deallocate(plume%D_rhoa)
       deallocate(plume%R)
       deallocate(plume%Q)
       deallocate(plume%En)
       deallocate(plume%Mw)
       deallocate(plume%Mair)
       deallocate(plume%xv)
       deallocate(plume%xl)
       deallocate(plume%xs)
       deallocate(plume%as)
       deallocate(plume%av)
       deallocate(plume%M)
       deallocate(plume%Mf)
       deallocate(plume%Magr)
       !
       deallocate(fo)
       deallocate(f )
    end if
    !
    !*** Initializations and memory allocation
    !
    plume%nc = nc
    plume%ns = plume_ns          ! number of plume sources (plume+umbrella)
    plume%np = plume_np          ! number of plume sources (up to NBL)
    !
    !*** Number of equations for lsode. Note that for the COSTA aggragation model plume%nc additional
    !*** equations are necessary to determine the partition between fallen mass and aggregating mass
    !
    if(TRIM(plume_type_aggr).eq.'COSTA') then
       plume%neq = 9+2*plume%nc
    else
       plume%neq = 9+plume%nc
    end if
    !
    allocate(part%fc    (plume%nc))
    allocate(part%diam  (plume%nc))
    allocate(part%rhop  (plume%nc))
    allocate(part%psi   (plume%nc))
    allocate(part%vlimit(plume%nc))
    allocate(A_p        (plume%nc))
    allocate(A_m        (plume%nc))
    A_p(:) = 0.0_rp
    A_m(:) = 0.0_rp
    !
    allocate(plume%x     (plume%ns))
    allocate(plume%y     (plume%ns))
    allocate(plume%z     (plume%ns))
    allocate(plume%L     (plume%ns))
    allocate(plume%H     (plume%ns))
    allocate(plume%U     (plume%ns))
    allocate(plume%T     (plume%ns))
    allocate(plume%D     (plume%ns))
    allocate(plume%D_rhoa(plume%ns))
    allocate(plume%R     (plume%ns))
    allocate(plume%Q     (plume%ns))
    allocate(plume%En    (plume%ns))
    allocate(plume%Mw    (plume%ns))
    allocate(plume%Mair  (plume%ns))
    allocate(plume%xv    (plume%ns))
    allocate(plume%xl    (plume%ns))
    allocate(plume%xs    (plume%ns))
    allocate(plume%as    (plume%ns))
    allocate(plume%av    (plume%ns))
    allocate(plume%M     (plume%nc,plume%ns))
    allocate(plume%Mf    (plume%nc,plume%ns))
    allocate(plume%Magr  (plume%nc,plume%ns))
    !
    allocate(fo(plume%neq))
    allocate(f (plume%neq))
    !
    !*** Initializations (fill the plume structure)
    !
    plume%aggregation   = plume_aggregation
    plume%moist_air     = plume_moist_air
    plume%wind_coupling = plume_wind_coupling
    plume%reentrainment = plume_reentrainment
    plume%latent_heat   = plume_latent_heat
    if(.not. plume%latent_heat) call set_latent_heat_flag(.false.)
    !
    plume%modv            = plume_modv
    plume%xv_UTM          = plume_xv_UTM
    plume%yv_UTM          = plume_yv_UTM
    plume%zv              = plume_zv
    plume%solve_for       = plume_solve_for
    plume%n_MFR(1:2)      = plume_n_MFR(1:2)
    plume%xi              = plume_xi
    plume%zmin_wind       = plume_zmin_wind
    plume%c_umbrella      = plume_c_umbrella
    plume%a_s_jet         = plume_a_s_jet
    plume%a_s_plume       = plume_a_s_plume
    plume%a_v             = plume_a_v
    !
    plume%type_aggr       = plume_type_aggr
    plume%type_as         = plume_type_as
    plume%type_av         = plume_type_av
    plume%umbrella_model = plume_umbrella_model
    !
    !*** Initializations (fill the part structure)
    !
    part%fc    (1:plume%nc) = fc  (1:plume%nc)
    part%diam  (1:plume%nc) = diam(1:plume%nc)
    part%rhop  (1:plume%nc) = rhop(1:plume%nc)
    part%psi   (1:plume%nc) = psi (1:plume%nc)
    part%vlimit(1:plume%nc) = 0.0_rp
    !
    part%vset_aggr     = vset_aggr
    part%diam_aggr     = diam_aggr
    part%Dfo           = Dfo
    if(TRIM(plume%type_aggr).eq.'COSTA') then
       part%iaggr = 0
       do ic = 1,plume%nc-1
          if(part%diam_aggr.gt.part%diam(ic)) then
             if(part%iaggr.eq.0) part%iaggr = ic    ! index of the first aggregating class
          end if
       end do
       if(part%iaggr == 0) part%iaggr = plume%nc-1  ! ensure at least 1 aggregating class
    end if
    !
    !*** Initializations (constants)
    !
    Cw = Cww
    Cp = Cpp
    Ca = Caa
    !
    Cv = CV0  ! Set the default values
    Cl = CL0
    Cs = CS0
    !
    return
  end subroutine plumeBPT_initialize_plume
  !
  !-----------------------------------------
  !    subroutine plumeBPT_initialize_wind
  !-----------------------------------------
  !
  !>   @brief
  !>   Initialize the wind profile structure and allocates memory
  !
  subroutine plumeBPT_initialize_wind(&
       profile_nz,           &
       profile_z,            &
       profile_rho,          &
       profile_p,            &
       profile_T,            &
       profile_sh,           &
       profile_u,            &
       profile_PSIa,         &
       MY_ERR )
    !
    !>   @param profile_nz    number of z-layers
    !>   @param profile_z     height of layers
    !>   @param profile_rho   air density
    !>   @param profile_p     air pressure
    !>   @param profile_T     air temperature
    !>   @param profile_sh    specific humidity
    !>   @param profile_u     wind speed
    !>   @param profile_PSIa  wind direction in rad (0,2pi)
    !>   @param MY_ERR        error handler
    !
    implicit none
    integer(ip),        intent(IN   ) :: profile_nz
    real   (rp),        intent(IN   ) :: profile_z   (profile_nz)
    real   (rp),        intent(IN   ) :: profile_rho (profile_nz)
    real   (rp),        intent(IN   ) :: profile_p   (profile_nz)
    real   (rp),        intent(IN   ) :: profile_T   (profile_nz)
    real   (rp),        intent(IN   ) :: profile_sh  (profile_nz)
    real   (rp),        intent(IN   ) :: profile_u   (profile_nz)
    real   (rp),        intent(IN   ) :: profile_PSIa(profile_nz)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip), save :: ipass = 0
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'plumeBPT_initialize_wind'
    MY_ERR%message = ' '
    !
    !*** If necessary deallocates memory. This is done in order to call fplumeBPT
    !*** with different wind profiles
    !
    if(ipass.eq.0) then
       ipass = 1
    else
       deallocate(profile%z   )
       deallocate(profile%rho )
       deallocate(profile%p   )
       deallocate(profile%T   )
       deallocate(profile%sh  )
       deallocate(profile%u   )
       deallocate(profile%PSIa)
    end if
    !
    !*** Allocates and fills the structure
    !
    profile%nz = profile_nz
    allocate(profile%z   (profile%nz))
    allocate(profile%rho (profile%nz))
    allocate(profile%p   (profile%nz))
    allocate(profile%T   (profile%nz))
    allocate(profile%sh  (profile%nz))
    allocate(profile%u   (profile%nz))
    allocate(profile%PSIa(profile%nz))
    !
    profile%z   (1:profile_nz) = profile_z   (1:profile_nz)
    profile%rho (1:profile_nz) = profile_rho (1:profile_nz)
    profile%p   (1:profile_nz) = profile_p   (1:profile_nz)
    profile%T   (1:profile_nz) = profile_T   (1:profile_nz)
    profile%sh  (1:profile_nz) = profile_sh  (1:profile_nz)
    profile%u   (1:profile_nz) = profile_u   (1:profile_nz)
    profile%PSIa(1:profile_nz) = profile_PSIa(1:profile_nz)
    !
    return
  end subroutine plumeBPT_initialize_wind
  !
  !-----------------------------------------
  !    subroutine plumeBPT_solve_plume
  !-----------------------------------------
  !
  !>   @brief
  !>   Solves the 1-D radially-averaged equations for a plume.
  !
  !
  !      np      Number of plume points for output. Points are equally spaced
  !              along the s direction (i.e. not in the vertical z due
  !              to plume bent-over by wind). The points start at s=0
  !              and finish at the neutral bouyancy level.
  !      ns      Total number of points for output (Plume+Umbrella). Mass
  !              in the umbrella region is distributed following a Gaussian.
  !      nc      Number of classes (including aggregates).
  !              Plume equations are solved for the particles only.
  !
  subroutine plumeBPT_solve_plume(&
                                !                    Input variables
       HPlume,          &
       M0,              &
       plume_u0,        &
       T0,              &
       Tv0,             &
       Tl0,             &
       Ts0,             &
       xv0,             &
       xl0,             &
       xs0,             &
       plume_ns,        &
                                !                     Output variables
       plume_status,       &
       plume_radius,       &
       plume_MER,          &
       plume_x,            &
       plume_y,            &
       plume_z,            &
       plume_Q,            &
       plume_En,           &
       plume_M,            &
       plume_Mf,           &
       plume_Magr,         &
       plume_Mair,         &
       plume_Mw,           &
       plume_L,            &
       plume_H,            &
       plume_U,            &
       plume_T,            &
       plume_D,            &
       plume_D_rhoa,       &
       plume_R,            &
       plume_xv,           &
       plume_xl,           &
       plume_xs,           &
       plume_as,           &
       plume_av,           &
       MY_ERR)
    !
    !>   @param HPlume        Plume height
    !>   @param M0            MER                  at the vent
    !>   @param plume_u0      Velocity             at the vent
    !>   @param T0            Mixture temperature  at the vent
    !>   @param Tv0           Vapor   temperature  at the vent
    !>   @param Tl0           Liquid  temperature  at the vent
    !>   @param Ts0           Solid   temperature  at the vent
    !>   @param xv0           Vapor  mass fraction at the vent
    !>   @param xl0           Liquid mass fraction at the vent
    !>   @param xs0           Solid  mass fraction at the vent
    !>   @param plume_ns      number of plume sources (plume+umbrella)
    !>   @param plume_status  Exit status
    !>   @param plume_radius  Vent radius
    !>   @param plume_MER     MER
    !>   @param plume_x       x-coord(ns)
    !>   @param plume_y       y-coord(ns)
    !>   @param plume_z       z-coord(ns) (elevation in m above terrain)
    !>   @param plume_Q       Bulk mass flow rate (ns)
    !>   @param plume_En      Total energy flow rate (ns)
    !>   @param plume_M       Particle mass flow rate (nc,ns)
    !>   @param plume_Mf      Mass flow rate of particles that fall from the eruption column (nc,ns)
    !>   @param plume_Magr    Mass flow rate of particles that aggregate (nc,ns)
    !>   @param plume_Mair    Mass flow rate of air (ns)
    !>   @param plume_Mw      Mass flow rate of volatiles (vapor + liquid + ice) (ns)
    !>   @param plume_L       Coordinate s (along the plume centerline)
    !>   @param plume_H       Theta angle
    !>   @param plume_U       Bulk velocity
    !>   @param plume_T       Bulk temperature
    !>   @param plume_D       Bulk density
    !>   @param plume_D_rhoa  Bulk density/air density
    !>   @param plume_R       Plume radius
    !>   @param plume_xv      water vapor  mass fraction
    !>   @param plume_xl      liquid water mass fraction
    !>   @param plume_xs      ice (solid)  mass fraction
    !>   @param plume_as      a_shear
    !>   @param plume_av      a_vortex
    !>   @param MY_ERR        error handler
    !
    implicit none
    real(rp),    intent(in)    :: HPlume
    real(rp),    intent(inout) :: M0
    real(rp),    intent(in)    :: plume_u0
    real(rp),    intent(in)    :: T0
    real(rp),    intent(in)    :: Tv0
    real(rp),    intent(in)    :: Tl0
    real(rp),    intent(in)    :: Ts0
    real(rp),    intent(in)    :: xv0
    real(rp),    intent(in)    :: xl0
    real(rp),    intent(in)    :: xs0
    integer(ip), intent(inout) :: plume_ns
    !
    integer(ip), intent(out) :: plume_status
    real   (rp), intent(out) :: plume_radius
    real   (rp), intent(out) :: plume_MER
    real   (rp), intent(out) :: plume_x     (plume%ns)
    real   (rp), intent(out) :: plume_y     (plume%ns)
    real   (rp), intent(out) :: plume_z     (plume%ns)
    real   (rp), intent(out) :: plume_Q     (plume%ns)
    real   (rp), intent(out) :: plume_En    (plume%ns)
    real   (rp), intent(out) :: plume_M     (plume%nc,plume%ns)
    real   (rp), intent(out) :: plume_Mf    (plume%nc,plume%ns)
    real   (rp), intent(out) :: plume_Magr  (plume%nc,plume%ns)
    real   (rp), intent(out) :: plume_Mair  (plume%ns)
    real   (rp), intent(out) :: plume_Mw    (plume%ns)
    real   (rp), intent(out) :: plume_L     (plume%ns)
    real   (rp), intent(out) :: plume_H     (plume%ns)
    real   (rp), intent(out) :: plume_U     (plume%ns)
    real   (rp), intent(out) :: plume_T     (plume%ns)
    real   (rp), intent(out) :: plume_D     (plume%ns)
    real   (rp), intent(out) :: plume_D_rhoa(plume%ns)
    real   (rp), intent(out) :: plume_R     (plume%ns)
    real   (rp), intent(out) :: plume_xv    (plume%ns)
    real   (rp), intent(out) :: plume_xl    (plume%ns)
    real   (rp), intent(out) :: plume_xs    (plume%ns)
    real   (rp), intent(out) :: plume_as    (plume%ns)
    real   (rp), intent(out) :: plume_av    (plume%ns)
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    logical     :: lexit,go_on_s,go_on_MFR,jet_zone
    integer(ip) :: ind,indc
    integer(ip) :: MFR_iter,ic,istate,is
    real(rp)    :: n_MFR_min,n_MFR_max,s,so,ds,sb,sc,rhorhoa,rhorhoamin
    real(rp)    :: x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xa,xw,xv,xl,xs,xp
    real(rp)    :: rhoair,rhoaT,rhopart,Vaair,a_shear,a_vortex
    real(rp)    :: Hb,Hc,dz
    real(rp)    :: xnbl,ynbl,Tnbl,Qnbl,Ennbl,znbl,zpp,dxdz,dydz,dxnbl,dynbl,dznbl,time_u
    real(rp)    :: dMp,dEn,enthalpy,Pair,Pvap
    real(rp)    :: err
    real(rp)    :: geff    ! Effective gravity
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'plumeBPT_solve_plume'
    MY_ERR%message = ' '
    !
    plume%Magr(:,:) = 0.0_rp
    plume%Mf  (:,:) = 0.0_rp
    plume%M   (:,:) = 0.0_rp
    !
    !*** Loop over MFR. It depends on the solving strategy (solve for MFR or height)
    !
    go_on_MFR = .true.
    MFR_iter  = 1
    indc      = 1
    n_MFR_min = plume%n_MFR(1)   ! min MFR (e.g. 10**2 . From input file)
    n_MFR_max = plume%n_MFR(2)   ! max MFR (e.g. 10**10. From input file)
    !
    do while(go_on_MFR)
       !
       plume%status = STATUS_OK     ! Clear status flag
       !
       select case(plume%solve_for)
       case('HEIGHT')
          continue                      ! MFR given, a single iteration with MFR=M0 is sufficient
       case('MFR')
          ! Method of bisection
          if(MFR_iter == 1) then
             M0 = 10.0_rp**n_MFR_min
          else if(MFR_iter == 2) then
             M0 = 10.0_rp**n_MFR_max
          else
             M0 = 10.0_rp**(0.5_rp*(n_MFR_min+n_MFR_max))
          end if
       end select
       !
       ! *** Set the initial conditions at the vent
       !
       w0 = xv0 + xl0 + xs0     ! Water mass fraction
       !
       u0 = plume_u0            ! Velocity at the vent (store for later use)
       !
       P0 = Pa(plume%zv)        ! Pressure at the vent
       !
       Enthalp0 = (1.0_rp-w0)*enthalpy_particles(T0)+xv0*enthalpy_vapour(Tv0)+ &      ! Enthalpy (per unit mass) at the vent
            xl0*enthalpy_liquid(Tl0)+xs0*enthalpy_ice(Ts0)
       !
       ! Average density of the particles at the vent
       if(plume%nc > 0) then
          rhopart0 = 1.0_rp/sum(part%fc(1:plume%nc)/part%rhop(1:plume%nc))
       else
          rhopart0 = 1.0_rp  ! Arbitrary, not used if nc=0
       end if
       !
       ! Density of mixture at the vent (particles + vapour + liquid + ice)
       rho0=1.0_rp/((1.0_rp-w0)/rhopart0+xv0/density_vapour(Tv0,P0)+xl0/rhol+xs0/rhos)
       !
       R0 = sqrt(M0/(pi*rho0*U0))    ! Vent radius
       !
       !*** Load boundary conditions for this MFR iteration
       !
       fo(1)  = M0                                   !  Eq(1).  TOTAL Mass flow rate          Q = pi*r*r*rho*u
       fo(2)  = M0*u0                                !  Eq(2).  Axial momentum                P = pi*r*r*rho*u*u
       fo(3)  = 0.5_rp*pi                            !  Eq(3).  Radial momentum               Theta
       fo(4)  = M0*(Enthalp0+GI*plume%zv+0.5_rp*u0*u0) !  Eq(4).  Energy                        J = pi*r*r*rho*u*E
       fo(5)  = 0.0_rp                                  !  Eq(5).  Mass flow rate of air         Ma
       fo(6)  = M0*w0                                !  Eq(6).  Mass flow rate of volatiles   Mw = Q*xw
       fo(7)  = plume%xv_UTM                         !  Eq(7).  Trajectory X in UTM
       fo(8)  = plume%yv_UTM                         !  Eq(8).  Trajectory y in UTM
       fo(9)  = plume%zv                             !  Eq(9).  Z  in m a.s.l.
       do ic = 1,plume%nc
          fo(9+ic) = M0*(1.0_rp-w0)*part%fc(ic)         !  Eq(9+ic). Mass flow rate of particles Mi
       end do
       !
       if(plume%type_aggr.eq.'COSTA') then
          do ic = 1,plume%nc
             fo(9+plume%nc+ic) = 0.0_rp              !  Eq(9+plume%nc+ic). Mass flow rate of particles that aggregate
          end do
       end if
       !
       !***  Loop over s to find Sb, the Neutral Buoyancy Level in s coordinate
       !
       go_on_s = .true.
       sc      = 1
       s       =  0.0_rp
       so      =  0.0_rp
       ds      = 10.0_rp   ! In meters: arbitrary choice (change with caution)
       f(:)    = fo(:)     ! Set initial conditions
       !
       rhorhoamin = 1d9
       jet_zone   = .true.
       !
       s_old   = so
       r_old   = 0.0_rp
       !
       ind = 0
       do while(go_on_s)
          s  = s + ds
          call splum(so,f,s,plume%neq,istate)      ! integration from so to s
          ind = ind + 1                            ! Number of iteration
          !
          if(istate == 2) then
             !
             if(plume%status /= STATUS_OK) then
                ! Plume collapse/errors detected
                go_on_s = .false.
                lexit   = .false.
                sb      = s
             else
                ! No errors detected
                call getplume(f,x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xv,xl,xs,xp)
                rhoair     = rhoa(z)
                rhorhoa    = rho/rhoair
                rhorhoamin = min(rhorhoamin,rhorhoa)
                !
                sb = s   ! Save reached plume length
                ! Check jet/convective transition
                if(rhorhoa <= 1.0_rp .and. jet_zone) jet_zone = .false.
                ! Check if we have reached the NBL
                if(rhorhoa > 1.0_rp .and.(.not.jet_zone)) then
                   go_on_s = .false.
                   lexit   = .true.
                end if
                !
                !*** Update iteration values
                s_old   = s
                r_old   = r
             end if
             !
          else
             !
             !*** Wrong termination (istate /= 2)
             go_on_s = .false.
             lexit   = .false.
             sb      = s
          end if
          !
          !*** Handle collapse (for any return status)
          !
          if(is_collapse(plume)) then
             go_on_s = .false.
             lexit   = .false.
             indc = ind-1  ! Save index before collapse
             sc = s - ds   ! Save plume length before collapse
          end if
          !
       end do ! go_on_s
       !
       select case(plume%solve_for)
       case('HEIGHT')
          go_on_MFR = .false.   ! Do not need other iterations
          if(.not.lexit) then
             if(is_collapse(plume)) then
                call task_wriwarn(MY_ERR,'plumeBPT_solve_plume: plume collapse detected')
             else
                MY_ERR%flag    = 1
                MY_ERR%message = 'solveplume: wrong termination'
                return
             end if
          end if
          !
       case('MFR')
          !
          if(lexit) then
             !
             !***  Checks if the plume height is close to the required value.
             !***  Note that z is the NBL in m a.s.l. and HPlume is the total
             !***  required height (including umbrella) in m a.v.
             !
             err = Hplume - plume%c_umbrella*(z - plume%zv + 8.0_rp*R0)
             !
             if( abs(err) <= ds) then
                go_on_MFR = .false.   ! Done
             else if( err < 0.0_rp .or. is_collapse(plume)) then
                ! n_MFR_max = 0.5_rp*(n_MFR_min+n_MFR_max)
                n_MFR_max = Log10(M0)
             else
                ! n_MFR_min = 0.5_rp*(n_MFR_min+n_MFR_max)
                n_MFR_min = Log10(M0)
             end if
             !
          else
             ! Go here if column collapse or wrong termination of splum
             ! n_MFR_max = 0.5_rp*(n_MFR_min+n_MFR_max)
             ! n_MFR_max = Log10(M0)
             !
             ! Modify n_MFR_min and n_MFR_max by a small, different, amount
             n_MFR_min = min(n_MFR_max,n_MFR_min+0.005)
             n_MFR_max = max(n_MFR_min,n_MFR_max-0.001)
          end if
          !
          MFR_iter = MFR_iter + 1
          !
          if(MFR_iter == 100) then
             MY_ERR%flag    = 1
             MY_ERR%message = 'MFR iterations = Itermax. Convergence not achieved'
             return
          end if
          !
       end select
       !
    end do              ! end do while(go_on_MFR)
    !
    !*** Integrates again up to the NBL (sb) and store the variables
    !*** in the plume%np points
    !
    s  = 0.0_rp
    so = 0.0_rp
    ds = sb/(plume%np-1)  ! np-1 points (reserve space for z=0)
    f(:) = fo(:)          ! Set initial conditions
    plume%MER = M0        ! Store mass eruption rate
    !
    s_old   = so
    r_old   = 0.0_rp
    !
    ! COLLAPSING COLUMN: Set maximum path along the column
    !
    if(is_collapse(plume)) then
       plume%np = min(indc,plume%ns)-1 ! Use maximum memory space
       ds = sc/(plume%np+1)        ! Add 1 to compensate numerical errors
    end if
    !
    ! Store initial conditions
    call getplume(f,x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xv,xl,xs,xp)
    if(plume%wind_coupling) then
       Vaair  = Va(z)
       rhoair = rhoa(z)
    else
       Vaair  = 0.0_rp
       rhoair = rhoa(z)
    end if
    call get_entrainment_coef(z-plume%zv,R0,U0,rhoair,rho,r,u,Vaair,th,a_shear,a_vortex)
    call setplume(1,f,x,y,z,s,th,u,T,Q,En,r,rho,Ma,Mw,xv,xl,xs,a_shear,a_vortex)
    !
    ! Integrate along the column
    do is = 2,plume%np
       s  = s + ds
       ! Integrate one step
       call splum(so,f,s,plume%neq,istate)
       if(istate /= 2) then
          MY_ERR%flag    = 1
          MY_ERR%message = 'Error in the second loop?'
          return
       end if
       call getplume(f,x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xv,xl,xs,xp)
       !
       if(plume%wind_coupling) then
          Vaair  = Va(z)
          rhoair = rhoa(z)
       else
          Vaair  = 0.0_rp
          rhoair = rhoa(z)
       end if
       !
       !*** call get_entrainment_coef for storing a_shear and a_vortex
       !
       call get_entrainment_coef(z-plume%zv,R0,U0,rhoair,rho,r,u,Vaair,th,a_shear,a_vortex)
       !
       !*** Update iteration values
       !
       s_old   = s
       r_old   = r
       !
       !*** Store values to the plume structure
       !
       call setplume(is,f,x,y,z,s,th,u,T,Q,En,r,rho,Ma,Mw,xv,xl,xs,a_shear,a_vortex)
       !
    end do
    !
    ! COLLAPSING COLUMN: return
    !
    if(is_collapse(plume)) goto 1000
    !
    !*** UMBRELLA REGION (empirical model)
    !
    !*** Variables stored from M(ic,np) to M(ic,ns), i.e. on ns-np+1 points.
    !*** The total column height Hc is assumed to be Hc=c_umbrella*(Hb+8R0)
    !*** Gaussian profile with radius e^(-2) ar z=Hc
    !
    Hb = plume%z(plume%np)-plume%zv   ! NBL   height (above vent)

    if(plume%umbrella_model == 'SPARKS1986') then
       ! Warning: the estimate of the total height based on Sparks1986 could
       ! generate an inconsistent with energy balance
       Hc = plume%c_umbrella*(Hb+8.0_rp*R0) ! Total height (above vent)
       !
       ! OTHER OPTIONS TO BE ADDED HERE
       !    else if(plume%umbrella_model == 'MORTON1956') then
       !       ! The dependence with R0 is removed
       !       Hc = plume%c_umbrella*Hb  ! Total height (above vent)
       !
    else
       ! Invalid umbrella height model
       MY_ERR%flag    = 1
       MY_ERR%message = 'Invalid UMBRELLA_HEIGHT_MODEL'
       return
    end if
    !
    ! Effective gravity introduced for energy balance
    ! geff = 2.0_rp*(Hc-Hb)/plume%u(plume%np)**2
    !
    geff = 9.81_rp

    dz = (Hc-Hb)/(plume%ns-plume%np)  ! ns-np+1 points

    !
    xnbl = plume%x(plume%np)
    ynbl = plume%y(plume%np)
    znbl = plume%z(plume%np)
    Qnbl = plume%Q(plume%np)
    Tnbl = plume%T(plume%np)
    Ennbl= plume%En(plume%np)   ! Energy flow rate at NBL
    dxnbl=(plume%x(plume%np)-plume%x(plume%np-1)) ! DX at NBL
    dynbl=(plume%y(plume%np)-plume%y(plume%np-1)) ! DY at NBL
    dznbl=(plume%z(plume%np)-plume%z(plume%np-1)) ! DZ at NBL
    dxdz= dxnbl/dznbl                             ! dx/dz at NBL
    dydz= dynbl/dznbl                             ! dy/dz at NBL
    !
    do is = 1,plume%ns-plume%np
       !
       !*** Coordinates: linear extrapolation
       !
       plume%x(plume%np+is) = xnbl + dxdz*is*dz
       plume%y(plume%np+is) = ynbl + dydz*is*dz
       plume%z(plume%np+is) = plume%z(plume%np)+(is)*dz
       ! Normalized height above NBL (zpp=0 at Hb, zpp=1 at Hc)
       zpp = real(is)/real(plume%ns-plume%np)
       !
       !*** Length. L = L + dL where dL^2=dx^2+dy^2+dz^2
       !
       plume%L(plume%np+is) = plume%L(plume%np+is-1) + dz*sqrt( dxdz**2 + dydz**2 + 1.0_rp)
       !
       !*** Radius. Decreases with zpp as a Gaussian with sigma=1/2
       !
       plume%R(plume%np+is) = plume%R(plume%np)*exp(-2.0_rp*zpp**2)
       !
       !*** Theta = theta(NBL)
       !
       plume%H(plume%np+is) = plume%H(plume%np)
       !
       !*** Velocity: U^2 = Unbl^2 - k*z
       !
       plume%u(plume%np+is) = plume%u(plume%np)*sqrt(1.0_rp-zpp)
       !
       !*** Mair and volatiles (constant)
       !
       plume%Mair(plume%np+is) = plume%Mair(plume%np)
       plume%Mw  (plume%np+is) = plume%Mw  (plume%np)
       !
       !*** Particle vertical mass flow rate (decrease exponentially)
       !
       do ic = 1,plume%nc
          plume%M(ic,plume%np+is) = plume%M(ic,plume%np)*exp(-2.0_rp*zpp**2)
       end do
       !
       Mp = sum(plume%M(1:plume%nc,plume%np+is))
       Ma = plume%Mair(plume%np+is)
       Mw = plume%Mw(plume%np+is)
       plume%Q(plume%np+is) = Mp+Mw+Ma                   ! Total mass flow rate
       xp = Mp/(Mp+Mw+Ma)              ! particle  mass fraction
       xw = Mw/(Mp+Mw+Ma)              ! volatiles mass fraction
       xa = Ma/(Mp+Mw+Ma)              ! air mass fraction
       dMp = Mp - sum(plume%M(1:plume%nc,plume%np+is-1)) ! Loss of particles
       dEn = dMp*(enthalpy_particles(plume%T(plume%np+is-1)) + &
            0.5_rp*plume%u(plume%np+is-1)**2 + GI*znbl + &
            geff*(plume%z(plume%np+is-1)-znbl))
       plume%En(plume%np+is) = plume%En(plume%np+is-1) + dEn
       ! Enthalpy per unit mass
       enthalpy = plume%En(plume%np+is)/(Mp+Mw+Ma) - &
            GI*znbl - geff*(plume%z(plume%np+is)-znbl) - &
            0.5_rp*plume%u(plume%np+is)**2
       !
       call temperature_mixture(xa,xp,xw,plume%T(plume%np+is), &
            Pa(plume%u(plume%np+is)),plume%xv(plume%np+is), &
            plume%xl(plume%np+is),plume%xs(plume%np+is),Pair,Pvap,enthalpy)
       ! Check for negative temperature
       if(plume%T(plume%np+is) < 0.0_rp) then
          call task_wriwarn(MY_ERR,'PlumeBPT: found negative temperature')
          ! THIS IS AN ABNORMAL EXIT (TO BE HANDLED IN THE FUTURE)
          plume%ns = plume%np+is -1  ! WARNING: RESET THE NUMBER OF STEPS
          plume_ns = plume%ns        ! TO BE CHANGED IN THE FUTURE
          exit  ! EXIT FROM THE LOOP
       end if
       !
       !*** Density
       !
       ! Density of air at (z,T_plume)
       rhoaT   = rhoa(plume%z(plume%np+is),plume%T(plume%np+is))
       !
       ! Average particle density
       rhopart = Mp/sum(plume%M(1:plume%nc,plume%np+is)/part%rhop(1:plume%nc))
       !
       ! Bulk density
       plume%D(plume%np+is)=1.0_rp/(xp/rhopart+xl/rhol+xs/rhos+(1.0_rp-xp-xl-xs)/rhoaT)

       plume%D_rhoa(plume%np+is) = plume%D(plume%np+is)/rhoa(plume%z(plume%np+is))
       !
       !*** Entrainment
       !
       plume%as(plume%np+is) = plume%as(plume%np)
       plume%av(plume%np+is) = plume%av(plume%np)
       !
    end do
    !
    !*** If necessary, modify mass of particles to account for aggregation in the umbrella region
    !
    if(TRIM(plume%type_aggr).eq.'COSTA') then
       !
       time_u = 2.0_rp  ! time factor for the umbrella region (up and down)
       !
       do is = 1,plume%ns-plume%np
          !
          f(1:plume%nc) = plume%M(1:plume%nc,plume%np+is)                                             ! Particle MFR
          Q = sum(plume%M(1:plume%nc,plume%np+is)) + plume%Mair(plume%np+is) + plume%Mw(plume%np+is)  ! Total MFR
          !
          call costa(f, plume%z (plume%np+is),plume%u (plume%np+is),plume%R(plume%np+is), &
               plume%T (plume%np+is),plume%D (plume%np+is),Q, &
               plume%xl(plume%np+is),plume%xs(plume%np+is))
          !
          do ic = part%iaggr,plume%nc
             !
             if(ic.lt.plume%nc) then
                Mp = time_u*(A_p(ic)-A_m(ic))*dz
                Mp = min(Mp,plume%M(ic,plume%np+is))  ! limit the amount of aggregates
             else
                Mp = sum(plume%Magr(1:plume%nc-1,plume%np+is))
                Mp = -Mp
             end if
             !
             plume%Magr(ic,plume%np+is) = Mp
             plume%M   (ic,plume%np+is) = plume%M(ic,plume%np+is) + Mp
          end do
          !
       end do
       !
    end if
    !
    !*** Finally, computes the mass that FALLS from the column as:
    !*** Mf = DM/Ds - A(+) + A(-)
    !
    do ic = 1,plume%nc
       plume%Mf(ic,plume%ns) = plume%M(ic,plume%ns-1)
    end do
    !
    do ic = 1,plume%nc
       !
       ! Umbrella region (Magr already substracted)
       !
       do is = plume%ns-1,plume%np+1,-1
          Mp =  plume%M(ic,is-1) - plume%M(ic,is)
          plume%Mf(ic,is) = max(Mp,0.0_rp)
       end do
       !
       ! plume region. Note that plume%Magr(ic,is-1)-plume%Magr(ic,is) approaches the aggragated
       ! mass in the slab only. Small mass imbalance (<1%) can occurr in case of aggregation because of this...
       !
       do is = plume%np,2,-1
          Mp =  plume%M(ic,is-1) - plume%M(ic,is) - (plume%Magr(ic,is-1)-plume%Magr(ic,is))
          plume%Mf(ic,is) = max(Mp,0.0_rp)
       end do
    end do
    !
    do ic = 1,plume%nc
       Mp = M0*(1.0_rp-w0)*part%fc(ic) - plume%M(ic,1) + plume%Magr(ic,1)
       plume%Mf(ic,1)= max(Mp,0.0_rp)
    end do
    !
    !*** Finally, load values to return
    !
1000 continue   ! JUMP HERE FOR RETURN !!!
    !
    plume_status    = plume%status
    plume_radius    = R0
    plume_MER       = plume%MER
    !
    plume_x(:)      = plume%x(:)
    plume_y(:)      = plume%y(:)
    plume_z(:)      = plume%z(:)
    plume_Q(:)      = plume%Q(:)
    plume_En(:)     = plume%En(:)
    plume_M(:,:)    = plume%M(:,:)
    plume_Mf(:,:)   = plume%Mf(:,:)
    plume_Magr(:,:) = plume%Magr(:,:)
    plume_Mair(:)   = plume%Mair(:)
    plume_Mw(:)     = plume%Mw(:)
    plume_L(:)      = plume%L(:)
    plume_H(:)      = plume%H(:)
    plume_U(:)      = plume%U(:)
    plume_T(:)      = plume%T(:)
    plume_D(:)      = plume%D(:)
    plume_D_rhoa(:) = plume%D_rhoa(:)
    plume_R(:)      = plume%R(:)
    plume_xv(:)     = plume%xv(:)
    plume_xl(:)     = plume%xl(:)
    plume_xs(:)     = plume%xs(:)
    plume_as(:)     = plume%as(:)
    plume_av(:)     = plume%av(:)
    !
    return
  end subroutine plumeBPT_solve_plume
  !
  !-----------------------------------------
  !    subroutine plumeBPT_write_plumeprop
  !-----------------------------------------
  !
  !>   @brief
  !>   Master writes plume porperties
  !
  subroutine plumeBPT_write_plumeprop(MY_FILES,MY_TIME,MY_PLUME,MY_ESP,MY_GRN,MY_AGR,MY_MOD,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES      list of files
    !>   @param MY_TIME       RUN_TIME       structure already filled
    !>   @param MY_PLUME      list of parameters defining the Plume Source Parameters
    !>   @param MY_ESP        list of parameters defining Eruption Source Parameters
    !>   @param MY_GRN        list of parameters defining granulometry
    !>   @param MY_AGR        list of parameters defining an aggregation model
    !>   @param MY_MOD        model physics related parameters
    !>   @param MY_ERR        error handler
    !
    type(FILE_LIST),     intent(IN   ) :: MY_FILES
    type(RUN_TIME),      intent(IN   ) :: MY_TIME
    type(PLUME_PARAMS),  intent(IN   ) :: MY_PLUME
    type(ESP_PARAMS),    intent(IN   ) :: MY_ESP
    type(BIN_PARAMS),    intent(IN   ) :: MY_GRN
    type(AGR_PARAMS),    intent(IN   ) :: MY_AGR
    type(MODEL_PHYS),    intent(IN   ) :: MY_MOD
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    character(len=s_file) :: fname
    character(len=24)     :: str
    character(len=16)     :: str1,str2
    integer(ip), save     :: ipass = 0
    integer(ip)           :: lures,ic,is
    integer(ip)           :: iyr,imo,idy,ihr,imi,ise
    integer(ip)           :: nbins_par,nbins_aggr,nbins_tgsd,ibin_aggr
    real(rp)              :: Maggr,Mtot,Mfines,xaggr
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'plumeBPT_write_plumeprop'
    MY_ERR%message = ' '
    !
    fname = TRIM(MY_FILES%file_plu)//'.res'
    lures = MY_FILES%luplu_res
    !
    !*** First time open file and write header
    !
    if(ipass.eq.0) then
       ipass = 1
       open(lures,FILE=TRIM(fname),status='unknown',err=100)
       !
       write(lures,1)
1      format(/, &
            '----------------------------------------------------',/, &
            '                                                    ',/, &
            '           CHARACTERISTICS OF THE PLUME             ',/, &
            '                                                    ',/, &
            '----------------------------------------------------',/)
       !
       select case(MY_MOD%modv)
       case(MOD_ARASTOPOUR)
          str = 'Arastoopour et al., 1982'
       case(MOD_GANSER)
          str = 'Ganser, 1993'
       case(MOD_WILSON)
          str = 'Wilson and Huang 1979'
       case(MOD_DELLINO)
          str = 'Dellino et al., 2005'
       case(MOD_PFEIFFER)
          str = 'Pfeiffer et al., 2005'
       case(MOD_DIOGUARDI2017)
          str = 'Dioguardi et al., 2017'
       case(MOD_DIOGUARDI2018)
          str = 'Dioguardi et al., 2018'
       end select
       !
       write(lures,10) MY_ESP%lon,MY_ESP%lat,&
            MY_PLUME%xv_UTM,MY_PLUME%yv_UTM,MY_PLUME%zv/1d3,str,MY_GRN%nbins
10     format(/, &
            'CHARACTERISTICS OF THE COLUMN         '      ,/, &
            '  Longitude of the vent             : ',f12.7,/, &
            '  Latitude  of the vent             : ',f12.7,/, &
            '  X-coordinate of the vent  (UTM  ) : ',f11.0,/, &
            '  Y--coordinate of the vent (UTM  ) : ',f11.0,/, &
            '  Altitude of the vent      (km   ) : ',f11.4,/, &
            '  Terminal velocity model           : ',a    ,/, &
            '                                      '      ,/, &
            'GRANULOMETRIC DISTRIBUTION AT VENT    '      ,/, &
            '  Number of bins                    : ',i4)
       !
       write(lures,11) (MY_GRN%bin_diam(ic)*1d3,ic=1,MY_GRN%nbins)
       write(lures,12) (1d2*MY_GRN%bin_fc(ic)  ,ic=1,MY_GRN%nbins)
       write(lures,14) (MY_GRN%bin_psi (ic)    ,ic=1,MY_GRN%nbins)
11     format( &
            '  Diameter                  (mm   ) : ',50(f7.4,1x))
12     format( &
            '  Percentage                (in % ) : ',50(f7.1,1x))
14     format( &
            '  Shape factor              (-    ) : ',50(f7.2,1x))
       !
    end if
    !
    !*** Writes time step
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month,MY_TIME%start_day,0, &
         iyr,imo,idy,ihr,imi,ise,1.0_rp*MY_PLUME%time1,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,2_ip,str1,MY_ERR)
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month,MY_TIME%start_day,0, &
         iyr,imo,idy,ihr,imi,ise,1.0_rp*MY_PLUME%time2,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,2_ip,str2,MY_ERR)
    !
    write(lures,20) str1,str2
20  format(/, &
         '--------------------  ',/, &
         'SIMULATION INTERVAL : ',a,'    to  ',a,/, &
         '--------------------  ')
    !
    write(lures,21) plume%MER, &
         plume%z(plume%np)-plume%zv,plume%z(plume%np), &
         plume%z(plume%ns)-plume%zv,plume%z(plume%ns)

21  format(/, &
         'SUMMARY', /,&
         '  Mass Flow Rate              : ',e12.4,/,&
         '  Neutral Bouyancy Level      : ',f6.0,' (',f6.0,' a.s.l.)',/, &
         '  Total column height         : ',f6.0,' (',f6.0,' a.s.l.)')
    !
    !*** Writes granulometry (including aggregates)
    !
    write(lures,40) TRIM(plume%type_aggr)
40  format(/,&
         'AGGREGATION',/,&
         '  Aggregation model           : ',a)
    !
    nbins_par  = MY_GRN%nbins_par
    nbins_aggr = MY_AGR%nbins_aggr
    nbins_tgsd = nbins_par

    if(plume%aggregation) then
       Maggr  = 0.0_rp
       Mtot   = 0.0_rp
       Mfines = 0.0_rp
       !
       nbins_par  = MY_GRN%nbins_par
       nbins_aggr = MY_AGR%nbins_aggr
       nbins_tgsd = nbins_par - nbins_aggr
       ibin_aggr  = MY_GRN%is_aggr

       do is = 1,plume%ns
          do ic = 1,nbins_par
             Mtot = Mtot + plume%M(ic,is)   ! Total mass
          end do
          !
          do ic = 1,nbins_aggr
             Maggr = Maggr + plume%M(nbins_tgsd+ic,is)   ! Mass of aggregates
          end do
          !
          do ic = ibin_aggr,nbins_par
             Mfines = Mfines + plume%M(ic,is)   ! Mass of fines
          end do
       end do
       !
       write(lures,50) 1d2*Maggr/Mtot,1d2*Maggr/Mfines, &
            -log(MY_GRN%bin_diam(ibin_aggr)*1d3)/log(2.0_rp), &
            1d6*MY_GRN%bin_diam(ibin_aggr)

50     format(&
            '  Mass fraction aggregates                       (%) : ',f7.3,/,&
            '  Mass fraction aggregates with respect to fines (%) : ',f7.3,/,&
            '  Maximum aggregated class at fi                     : ',f7.2,/,&
            '  Maximum aggregated class at diameter           (um): ',f7.2)
       !
    end if
    !
    !*** Writes results along the plume (one file per time step)
    !
    call time_addtime(MY_TIME%start_year,MY_TIME%start_month,MY_TIME%start_day,0, &
         iyr,imo,idy,ihr,imi,ise,1.0_rp*MY_PLUME%time1,MY_ERR)
    call time_dateformat(iyr,imo,idy,ihr,imi,ise,1_ip,str1,MY_ERR)
    !
    fname = TRIM(MY_FILES%file_plu)//'.'//TRIM(str1)//'.res'

    write(lures,101) TRIM(fname)
101 format(/, &
         'RESULTS  ',/, &
         '  See plume profile in file   : ',a)
    !
    open(90,FILE=TRIM(fname),status='unknown')
    write(90,150) TRIM(str1),MY_PLUME%time1
150 format( &
         ' Date                  : ',a ,/, &
         ' Time (sec after 00UTC): ',i8,/, &
         '---------------------------------------------------------------------------------------------------------',/,&
         '    z       u       T    rho/rho_a  Angle   Q Total     Qa        Qw       xv       xl      xs     xaggr ',/,&
         '(km asl)  (m/s)    (C)      (-)     (deg)   (ks/s)    (kg/s)     (kg/s)    (%)      (%)     (%)     (%)  ',/,&
         '---------------------------------------------------------------------------------------------------------')
    !
    Maggr  = 0.0_rp
    !
    do is = 1,plume%ns
       !
       xaggr  = 0.0_rp
       if(plume%aggregation) then
          Mtot   = 0.0_rp
          do ic = 1,nbins_par
             Mtot = Mtot + plume%M(ic,is)   ! Total mass
          end do
          !
          Maggr  = 0.0_rp
          do ic = 1,nbins_aggr
             Maggr = Maggr + plume%M(nbins_tgsd+ic,is)   ! Mass of aggregates
          end do
          !
          xaggr = 1e2_rp*Maggr/Mtot
          !
       end if
       !
       write(90,160) &
            plume%z(is)/1e3_rp, &
            plume%u(is), &
            plume%T(is)-273.15, &
            plume%D_rhoa(is), &
            plume%h(is)*180.0/PI, &
            plume%Q(is),&
            plume%Mair(is),&
            plume%Mw(is),&
            plume%xv(is)*1e2_rp,&
            plume%xl(is)*1e2_rp,&
            plume%xs(is)*1e2_rp, &
            xaggr
       !
160    format(f7.3,1x,f7.2,1x,f7.1,1x,e10.3,1x,f5.1,3(1x,e10.3),3(1x,f7.3),1x,f6.1)
    end do
    close(90)
    !
    return
    !
100 MY_ERR%flag = -1
    MY_ERR%message = 'error opening file '//TRIM(fname)
    return
    !
  end subroutine plumeBPT_write_plumeprop
  !
  !-----------------------------------------------------------------------------------------
  !
  !                                   PRIVATE ROUTINES
  !
  !-----------------------------------------------------------------------------------------
  !
  !
  subroutine splum(so,f,s,neq,istate)
    !************************************************************************
    !*
    !*    Returns the value of f(neq,s ) (at point s ) given the
    !*    initial conditions   f(neq,so) (at point so)
    !*
    !************************************************************************
    use odepack
    implicit none
    !
    integer, intent(in)    ::  neq
    integer, intent(inout) ::  istate
    real(rp),intent(inout) ::  f(neq)
    real(rp),intent(inout) ::  so,s
    !
    integer  ::  iwork(20)
    real(rp), allocatable :: rwork(:)
    integer  ::  nneq(1),itol,itask,iopt,lrw,liw,mf
    real(rp) ::  rtol(1),atol(1)
    !
    !***  Defines lsode parameters
    !
    nneq(1) = neq
    itol   = 1
    itask  = 1              ! Normal computation (overshooting)
    istate = 1
    iopt   = 0
    lrw    = 20+16*neq      ! Dimension of rwork array
    liw    = 20
    mf     = 10             ! Jacobian is not supplied
    rtol(1) = 1e-5_rp       ! Relative error
    atol(1) = 1e-5_rp       ! Absolute error
    !
    ! Allocate work array (Allocation/deallocation should be moved out of splum)
    !
    allocate(rwork(lrw))
    if(itask==2) rwork(1)=s  ! Set TCRIT = TOUT (no overshooting)
    !
    call lsode(dfds,nneq,f,so,s,itol,rtol,atol,itask, &
         istate,iopt,rwork,lrw,iwork,liw,jac,mf)
    !
    ! Deallocate work array
    !
    deallocate(rwork)
    !
    return
  end subroutine splum
  !
  !
  !
  subroutine dfds(neq,s,f,E)
    !*************************************************************************
    !*
    !*    Defines the system of equations for lsode loading E(i)
    !*
    !*    df
    !*    -- = E(s,f)    i = 1:neq
    !*    ds
    !*
    !*   f(1)  = Total mass flow rate
    !*   f(2)  = Axial momentum flow rate
    !*   f(3)  = Radial momentum flow rate (theta angle)
    !*   f(4)  = Total energy (thermal + potential + kinetic)
    !*   f(5)  = Mass flow rate of entrained air
    !*   f(6)  = Mass flow rate of volatiles
    !*   f(7)  = X
    !*   f(8)  = Y
    !*   f(9)  = Z
    !*   f(9+1:9+nc)       = Mass flow rate for each particle class
    !*   f(9+nc+1:9+nc+nc) = Mass flow rate of aggregating particles
    !
    !*************************************************************************
    implicit none
    !
    integer(ip) :: neq
    real(rp)    :: s,f(neq),E(neq)
    !
    integer(ip) :: ic
    real(rp)    :: x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xv,xl,xs,xp    ! Derivated variables
    real(rp)    :: Taair,rhoair,muair,rhoaT,wair,Vaair,PSIair,hair,ue,dvaadz
    real(rp)    :: Paair,ppa,ppv
    real(rp)    :: a_shear,a_vortex
    real(rp)    :: dsdr,Po,Fo,fre,RHS,xw
    real(rp)    :: arhop  ! Density of particles with buoyancy
    !
    type(ERROR_STATUS) :: ERR
    !
    !*** Initializations
    !
    ppv = 0.0_rp
    !
    !*** Current values of derived variables
    !
    call getplume(f,x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xv,xl,xs,xp)
    !
    ! Check for collapsing column
    if(f(2) < 0.0_rp .or. f(3) < 0.0_rp) then
       plume%status = STATUS_COLLAPSE
       E = 0.0_rp   ! Set all derivatives to 0 and return
       return
    end if
    ! Check for errors
    if(T < 0.0_rp) then
       plume%status = STATUS_ERROR
       E = 0.0_rp   ! Set derivatives to 0 and return
       return
    end if
    !
    Taair  = Ta   (z)                   !  Temperature of the entrained air at z
    Paair  = Pa   (z)                   !  Pressure of the entrained air at z
    rhoair = rhoa (z)                   !  Density     of the entrained air at z
    muair  = mua  (T)                   !  Air viscosity (at bulk temperature)
    rhoaT  = rhoa (z,T)                 !  Air density   (at bulk temperature)
    !
    !*** Compute the settling velocity (using air density and viscosity at bulk temperature)
    !
    do ic = 1,plume%nc
       if(part%diam(ic) > 1e-10) then
          arhop = part%rhop(ic)-rhoaT  ! Accounts for buoyancy force
          call phys_get_vset_point(part%diam(ic),arhop,part%psi(ic),rhoaT,muair,part%vlimit(ic),plume%modv,ERR)
          if(ERR%flag==1) then
             part%vlimit(ic) = 0.0_rp ! No convergence in vsettl routine
          else if(ERR%flag==2) then
             part%vlimit(ic) = 0.0_rp ! No convergence in vsettl routine
          end if
       else
          part%vlimit(ic) = 0.0_rp
       end if
    end do
    if(plume%aggregation) part%vlimit(plume%nc) = part%vset_aggr*part%vlimit(plume%nc)  ! modify settling velocity of aggregating class
    !
    !***  Relative humidity of entrained air at z (kg/kg)
    !
    if(plume%moist_air) then
       wair = sh_a (z)
    else
       wair = 0.0_rp
    end if
    !
    if(plume%wind_coupling) then
       Vaair  = Va   (z)
       PSIair = Da   (z)
       dVaadz = dVadz(z)
    else
       Vaair  = 0.0_rp
       PSIair = 0.0_rp
       dVaadz = 0.0_rp
    end if
    !
    !*** Computes aggregation coefficients A_p(nc) and A_m(nc)
    !*** Note that this is necessary only for the Costa model because in the
    !*** other aggregation models aggregates are "formed" at the vent
    !
    if(TRIM(plume%type_aggr).eq.'COSTA') call costa(f(9+1),z,u,r,T,rho,Q,xl,xs)
    !
    !*** Computes the entrainment velocity
    !
    call get_entrainment_coef(z-plume%zv,R0,U0,rhoair,rho,r,u,Vaair,th,a_shear,a_vortex)
    !
    if((z-plume%zv).lt.plume%zmin_wind) then
       ue = 0.0_rp                      ! Ignore entrainment in the low jet zone
    else
       ue = a_shear*abs(u-Vaair*cos(th)) + a_vortex*abs(Vaair*sin(th))
    end if
    !
    !*** Defines the equations (note that the order in the array E needs to
    !*** be changed to account for dependencies)
    !
    !  E(9+1   :9+nc   ): Mass flux for each particle class
    !  E(9+nc+1:9+nc+nc): Mass flux of aggregating particles
    !
    ! These parameters are used with reentrainment

    Po  = pi*R0*R0*u0*u0
    Fo  = pi*R0*R0*u0*Enthalp0   ! Enthalp0 = Cbo*T0
    !
    do ic = 1,plume%nc
       !
       if(plume%reentrainment) then
          ! @@@@@@@@@@@ QUESTO IF CREA PROBLEMI (JUMP) @@@@@@@
          ! write(*,*) (s-s_old),(r-r_old),(s-s_old)/(r-r_old)
          if( abs((r-r_old)).gt.1d-6) then
             dsdr = (s-s_old)/(r-r_old)
          else
             dsdr = 0.0_rp
          end if
          fre = 0.43_rp/(1.0_rp+(0.78_rp*part%vlimit(ic)*(Po**0.25_rp)/sqrt(Fo))**6)
       else
          fre  = 0.0_rp
          dsdr = 0.0_rp
       end if
       !
       ! RHS = min(fre*ue*dsdr-vlimit(ic),0.0_rp)  ! Limit particle reentrainment
       ! E(9+ic) = xi*RHS*f(9+ic)/(u*r) + A_p(ic) - A_m(ic)
       !
       if(part%vlimit(ic) > 0.0_rp) then
          RHS = 1.0_rp/(1.0_rp+fre*ue*dsdr/part%vlimit(ic)) ! Modified reentraimment factor
       else
          RHS = 1.0_rp
       end if
       E(9+ic) = -plume%xi*f(9+ic)*part%vlimit(ic)*RHS/(u*r) + A_p(ic) - A_m(ic)
       ! Used only for fallen mass later on
       if(TRIM(plume%type_aggr).eq.'COSTA') E(9+plume%nc+ic) = A_p(ic) - A_m(ic)
    end do
    !
    ! E(1): Total mass flow rate
    !
    E(1) = 2.0_rp*pi*r*rhoair*ue + sum(E(9+1:9+plume%nc))
    !
    ! E(2): Axial momentum flow rate
    !
    ! E(2)=pi*r*r*(rhoair-rho)*g*sin(th)+Vaair*cos(th)*E(1)+u*sum(E(9+1:9+plume%nc))
    E(2)=pi*r*r*(rhoair-rho)*GI*sin(th)+Vaair*cos(th)*2.0_rp*pi*r*rhoair*ue+ &
         u*sum(E(9+1:9+plume%nc))
    !
    ! E(3): Radial momentum flow rate (theta)
    !
    ! E(3) = (pi*r*r*(rhoair-rho)*g*cos(th)-Vaair*sin(th)*E(1))/(pi*r*r*rho*u*u)
    E(3) = (pi*r*r*(rhoair-rho)*GI*cos(th)-Vaair*sin(th)*2.0_rp*pi*r*rhoair*ue)/(pi*r*r*rho*u*u)
    !
    ! E(5): Mass flow rate of entrained (dry) air
    !
    E(5) = 2.0_rp*pi*r*rhoair*ue*(1.0_rp-wair)
    !
    ! E(6): Mass flow rate of volatiles
    !
    E(6) = 2.0_rp*pi*r*rhoair*ue*wair
    !
    ! E(7) : X
    !
    E(7) = cos(th)*cos(PSIair)
    !
    ! E(8) : Y
    !
    E(8) = cos(th)*sin(PSIair)
    !
    ! E(9) : Z
    !
    E(9) = sin(th)
    !
    xw = Mw/(Mp+Mw+Ma)                  ! volatiles mass fraction
    !
    !  E(4) : Total energy (thermal + potential + kinetic)
    !
    ! hair = enthalpy of entrained air
    call enthalpy_mixture(1.0_rp-wair,0.0_rp,wair,Taair,Paair,xv,xl,xs,ppa,ppv,hair)
    !
    E(4) = 2.0_rp*pi*r*rhoair*ue*(hair+GI*z+0.5_rp*ue*ue)+Cp*T*sum(E(9+1:9+plume%nc))
    !
    return
  end subroutine dfds
  !
  !
  !
  subroutine get_entrainment_coef(z,R0,U0,rhoair,rho,r,u,Vaair,theta,a_shear,a_vortex)
    !****************************************************************
    !*
    !*   Computes the entrainment coefficients
    !*
    !****************************************************************
    use KindType
    implicit none
    !
    real(rp) :: z,R0,U0,rhoair,rho,r,u,Vaair,theta,a_shear,a_vortex
    real(rp) :: zs,A,dlnAdz,Ri,c0,c1,c2,c3,c4,h
    !
    zs = max(z/(2.0_rp*R0),0.0_rp)                           ! zstar = z/Dvent
    Ri = max(GI*(rhoair-rho)*r/(rhoair*u*u+1e-6_rp),1e-6_rp) ! Ri
    c0 = 0.0_rp
    c1 = 0.0_rp
    c2 = 0.0_rp
    c3 = 0.0_rp
    c4 = 0.0_rp
    !
    !*** a_vortex
    !
    SELECT CASE(plume%type_av)
    CASE('CONSTANT')
       !
       a_vortex = plume%a_v
       !
    CASE('TATE')
       !
       a_vortex= 0.34_rp*( Vaair*(sqrt(2.0_rp*abs(Ri))/U0) )**(-0.125_rp)
       a_vortex = min(a_vortex,1.0_rp)
       a_vortex = max(a_vortex,0.0_rp)
       !
    END SELECT
    !
    !*** a_shear
    !
    SELECT CASE(plume%type_as)
    CASE('CONSTANT')
       !
       if(rhoair.lt.rho) then  ! jet
          a_shear = plume%a_s_jet
       else                    ! plume
          a_shear = plume%a_s_plume
       end if
       return  ! we are done
       !
    CASE('KAMINSKI-R')
       !
       if(rhoair.lt.rho) then  ! jet
          c0 = 1.92003_rp
          c1 = 3737.26_rp
          c2 = 4825.98_rp
          c3 = 2.0_rp*(c2-c1)
          c4 = 0.00235_rp
       else                    ! plume
          c0 = 1.61717_rp
          c1 = 478.374_rp
          c2 = 738.348_rp
          c3 = 2.0_rp*(c2-c1)
          c4 = -0.00145_rp
       end if
       !
    CASE('KAMINSKI-C')
       !
       if(rhoair.lt.rho) then  ! jet
          c0 = 1.92003_rp
          c1 = 3737.26_rp
          c2 = 4825.98_rp
          c3 = 2.0_rp*(c2-c1)
          c4 = 0.00235_rp
       else                    ! plume
          c0 = 1.55_rp
          c1 = 329.0_rp
          c2 = 504.5_rp
          c3 = 2.0_rp*(c2-c1)
          c4 = -0.00145_rp
       end if
       !
    CASE('OLD')
       !
       if(rhoair.lt.rho) then  ! jet
          c0 = 1.6_rp
          c1 = 1657.0_rp
          c2 = 2411.0_rp
          c3 = 2.0_rp*(c2-c1)
          c4 = 0.0_rp
       else                    ! plume
          c0 = 1.6_rp
          c1 = 1657.0_rp
          c2 = 2411.0_rp
          c3 = 2.0_rp*(c2-c1)
          c4 = 0.0_rp
       end if
       !
    END SELECT
    !
    A      = c0*(zs*zs+c1)/(zs*zs+c2)
    dlnAdz = c3*zs/((zs*zs+c1)*(zs*zs+c2))
    h      = 1.0_rp/(1.0_rp+c4*exp(-5.0_rp*(zs/10_rp-1.0_rp)))
    A      = A*h
    !
    a_shear = 0.0675_rp + (1.0_rp-(1.0_rp/A))*Ri*sin(theta) + (0.5_rp*r*dlnAdz)/(2.0_rp*R0)
    a_shear  = min(a_shear,0.17_rp)
    a_shear  = max(a_shear,0.0_rp)
    !
    return
  end subroutine get_entrainment_coef
  !
  !
  !
  subroutine costa(f,z,u,r,T,rho,Q,xl,xs)
    !************************************************************
    !*
    !*  Computes aggregation coefficients according to Costa et al. (2010)
    !*
    !************************************************************
    implicit none
    !
    real(rp), intent(in) :: f(plume%nc)               ! Class mass flow rate
    real(rp), intent(in) :: z,u,r,T,rho,Q,xl,xs
    !
    integer(ip), save              :: ipass = 0
    real   (rp), save, allocatable :: Ni(:)          ! Number of particles of size i in an aggregate
    real   (rp), save, allocatable :: Df(:)          ! Fractal exponent  (size dependent)
    real   (rp), save, allocatable :: ka(:)          ! Fractal prefactor (size dependent)
    real   (rp), save, allocatable :: psi3j(:)       ! Diameter to volume fractal relationship **3
    real   (rp), save, allocatable :: psi4j(:)       ! Diameter to volume fractal relationship **4
    !
    real   (rp), save              :: sumNi,psi3,psi4
    !
    character(len=6)  :: wphase
    integer(ip)       :: ic,jc
    real   (rp)       :: muair,rhoaT,rhomean
    real   (rp)       :: alfa_mean,sumfc,sumff,w,Stij,viscl,alfa,Stcr,qq,conc
    real   (rp)       :: Ab,As,Ad,At,gammas,epsilon,dudr,ntot,dntot,vij,fi
    real   (rp)       :: expb,exps,expd,factor_t,factor_c,Dmin,dstar
    real   (rp)       :: deltaphi,dja,djb,work
    real   (rp)       :: arhop  ! Density of particles with buoyancy
    !
    type(ERROR_STATUS) :: ERR
    !
    !***  Allocates memory and stores some variables (first time only)
    !
    if(ipass == 0) then
       !
       ipass = 1
       !
       !*** Fractal exponent D_f(d) and fractal prefactor ka(d)
       !
       allocate(Df(plume%nc))
       allocate(ka(plume%nc))
       Df(:) = 0.0
       ka(:) = 0.0
       !
       Dmin  = 1.6_rp
       dstar = 2e-6_rp
       do ic = part%iaggr,plume%nc-1
          Df(ic) = part%Dfo - (1.36788*(part%Dfo-Dmin))/(1+exp((part%diam(ic)-dstar)/dstar))
          !
          ka(ic) = (sqrt(1.56-((1.728-Df(ic)/2.)**2.0)) - 0.228)**Df(ic)
          ka(ic) = ka(ic)*( ((2.0+Df(ic))/Df(ic))**(Df(ic)/2.0) )
          !
          !Df(ic) = Dfo  ! uncomment this to have the old version with constant factors
          !ka(ic) = 1.0
       end do
       !
       !*** Diameter to volume fractal relationship (note that this is actually size dependent)
       !
       allocate(psi3j(plume%nc))
       allocate(psi4j(plume%nc))
       psi3j(:) = 0.0
       psi4j(:) = 0.0
       do ic = part%iaggr,plume%nc-1
          work = part%diam(ic)*((pi*part%diam(ic)*part%diam(ic)*part%diam(ic)/6.0)**(-1.0_rp/Df(ic)))
          psi3j(ic) = work*work*work
          psi4j(ic) = psi3j(ic)*work
       end do
       !
       !*** However, because kernels are integrated we use by now for simplicity constant values
       psi3 = 6.0_rp/pi
       psi4 =  psi3*((6.0_rp/pi)**(1.0/3.0))
       !
       !*** Number of particles in an aggregate
       !
       allocate(Ni(plume%nc))
       Ni(:) = 0.0_rp
       !
       sumNi = 0.0_rp
       do ic = part%iaggr,plume%nc-1
          Ni(ic) = ka(ic)*( (part%diam(plume%nc)/part%diam(ic))**Df(ic) )
          sumNi  = sumNi + Ni(ic)
       end do
       !
    end if
    !
    !*** Initializations (necessary to ensure zero aggregation in vapour)
    !
    A_p(:) = 0.0_rp
    A_m(:) = 0.0_rp
    !
    !*** Select water phase (and do nothing for pure vapor phase)
    !*** Note that we assume that liquid and ice do not coexist
    !
    if( (xl.eq.0.0_rp).and.(xs.eq.0.0_rp) ) then
       wphase = 'vapour'
       return
    else if(xs.eq.0.0_rp) then
       wphase = 'liquid'
    else
       wphase = 'ice'
    end if
    !
    muair  = mua (T)      !  Air viscosity (at bulk temperature)
    rhoaT  = rhoa(z,T)    !  Air density   (at bulk temperature)
    !
    !*** Calculates the mean density of the aggregating particles
    !
    rhomean = 0.0_rp
    sumfc   = 0.0_rp
    do ic = part%iaggr,plume%nc-1
       rhomean = rhomean + part%fc(ic)*part%rhop(ic)
       sumfc   = sumfc   + part%fc(ic)
    end do
    rhomean = rhomean/sumfc
    !
    !*** Collision frequency kernel for Brownian motion at each point
    !
    Ab = -4.0_rp*kb*T/(3.0_rp*muair)
    !
    !*** Collision frequency kernel for laminar and turbulent fluid shear
    !
    dudr    = sqrt(2*pi)*u/r                 ! laminar
    epsilon = 0.0724*(u*u*u)/r               ! Smagorinsky-Lilly, factor = 2*sqrt(2)*cs*cs with cs = 0.16
    gammas  = sqrt(epsilon/(muair/rhoaT))    ! turbulent
    gammas  = max(gammas,dudr)
    !
    As = -2.0_rp*gammas*psi3/3.0_rp
    !
    !*** Collision frequency kernel for differential settling velocity at each point
    !
    Ad   = -pi*(rhomean-rho)*GI*psi4/(48.0_rp*muair)
    !
    !*** Collision frequency kernel for turbulent inertial kernel at each point
    !
    At = 1.82*(epsilon**(0.75))/(GI*(muair/rhoaT)**(0.25))*Ad
    !
    !*** Check the order of magnitude of the kernels
    !
    do ic = part%iaggr,plume%nc-1
       !     write(*,'(f10.3,2x,a2,f8.1,2x,4(3x,a3,e13.6))') diam(ic)*1d6,'z=',z,'Ab=',Ab,'As=',As,'Ad=',Ad,'At=',At
    end do
    !
    !*** Compute the settling velocity (using air density and viscosity at bulk temperature)
    !
    do ic = 1,plume%nc
       if(part%diam(ic) > 1e-10) then
          arhop = part%rhop(ic)-rhoaT  ! Accounts for buoyancy force
          call phys_get_vset_point(part%diam(ic),arhop,part%psi(ic),rhoaT,muair,part%vlimit(ic),plume%modv,ERR)
          if(ERR%flag==1) then
             part%vlimit(ic) = 0.0_rp ! Invalid model in vsettl routine
          else if(ERR%flag==2) then
             part%vlimit(ic) = 0.0_rp ! Invalid model in vsettl routine
          end if
       else
          part%vlimit(ic) = 0.0_rp
       end if
    end do
    if(plume%aggregation) part%vlimit(plume%nc) = part%vset_aggr*part%vlimit(plume%nc)
    !
    !*** Computes the number of available particles per unit volume and
    !*** the solid volume fraction fi
    !
    ntot  = 0.0_rp
    fi    = 0.0_rp
    do ic = part%iaggr,plume%nc-1
       if(ic == part%iaggr) then
          dja = part%diam(ic)
          djb = part%diam(ic)*(part%diam(ic)/part%diam(ic+1))         ! Assume same interval
          deltaphi = log(djb/dja)/log(2.0_rp)
       else
          dja  = part%diam(ic)
          djb  = part%diam(ic-1)
          deltaphi = log(djb/dja)/log(2.0_rp)
       end if
       conc = rho*f(ic)/Q
       ntot = ntot + 6.0*conc*(1.0_rp/dja**3-1.0_rp/djb**3)/(pi*part%rhop(ic)*deltaphi)
       fi = fi + conc/part%rhop(ic)
    end do
    ntot = ntot/(3.0_rp*log(2.0_rp))
    !
    !*** Computes the class-averaged sticking efficiency
    !
    alfa_mean = 0.0_rp
    !
    SELECT CASE(wphase)
    case('ice')
       !
       alfa_mean = 0.09_rp
       !
    case('liquid')
       !
       sumff = 0.0_rp
       do ic = part%iaggr,plume%nc-1
          do jc = part%iaggr,plume%nc-1
             sumff = sumff + part%fc(ic)*part%fc(jc)
          end do
       end do
       !
       viscl = 2.414e-5_rp*(10.0_rp**(247.7_rp/(T-140.0_rp)))  ! water viscosity at bulk temperature
       Stcr  = 1.3_rp                                       ! Critical Stokes number
       qq    = 0.8_rp
       !
       alfa_mean = 0.0_rp
       do ic = part%iaggr,plume%nc-1
          do jc = part%iaggr,plume%nc-1
             vij = abs(part%vlimit(ic)-part%vlimit(jc)) + (8.0_rp*kb*T)/ &
                  (3.0_rp*pi*muair*part%diam(ic)*part%diam(jc)) + &
                  2.0_rp*gammas*(part%diam(ic)+part%diam(jc))/(3.0_rp*pi)
             w = part%fc(ic)*part%fc(jc)/sumff
             Stij = 8.0_rp*rho*vij*part%diam(ic)*part%diam(jc)/ &
                  (9.0_rp*viscl*(part%diam(ic)+part%diam(jc)))
             alfa = 1.0_rp+((Stij/Stcr)**qq)
             alfa = 1.0_rp/alfa
             alfa_mean = alfa_mean + w*alfa
          end do
       end do
       !
    END SELECT
    !
    !*** Computes total particle decay per unit volume and time
    !
    expb = 2.0_rp                      ! Brownian     ntot exponent
    exps = 2.0_rp-(3.0_rp/part%Dfo)    ! Shear        ntot exponent
    expd = 2.0_rp-(4.0_rp/part%Dfo)    ! Differential ntot exponent
    !
    dntot = Ab*(ntot**expb) + As*(ntot**exps)*(fi**(3.0_rp/part%Dfo)) + &
         (Ad+At)*(ntot**expd)*(fi**(4.0_rp/part%Dfo))
    dntot = abs(alfa_mean*dntot)  ! positive sign in coefficent A
    !
    !** factor_t for estimating time that single particles spend in a control volume (lagrangian vs eulerian time)
    !
    factor_c = 2.0_rp  ! correction from local formulation to hop-hat  for ^2 terms
    factor_t = 2.0_rp  ! 2-7  also from top-hat to Gaussian
    !
    dntot = factor_c*factor_t*dntot
    !
    !*** Computes aggregation coefficients
    !
    do ic = part%iaggr,plume%nc-1
       if(f(ic).le.0.0) then
          A_m(ic) = 0.0
       else
          !        old version
          !        A_m(ic) = (dntot*Ni(ic)/sumNi)*(rhop(ic)*pi*diam(ic)**3/6.0_rp)*pi*r*r

          !        new version (is exactly the same no?)
          A_m(ic) = (dntot*Ni(ic)/sumNi)*(part%rhop(ic)*pi*part%diam(ic)**3/6.0_rp)*pi*r*r
       end if
    end do
    A_p(plume%nc) = sum(A_m(:))
    !
    return
  end subroutine costa
  !
  !
  !
  subroutine jac
    ! This is a dummy routine needed by lsode
    implicit none
  end subroutine jac
  !
  !
  !
  subroutine getplume(f,x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xv,xl,xs,xp)
    !********************************************************************
    !*
    !*    Extracts physical variables from values in the ODE's at
    !*    a certain value of the arc parameter s.
    !*
    !*    OUTPUTS : f,x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,xv,xl,xs,xp
    !*
    !********************************************************************
    use KindType
    implicit none
    !
    real(rp), intent(inout) :: f(plume%neq)
    real(rp), intent(out)   :: x,y,z,th,u,T,r,rho,Ma,Mw,Mp,Q,En,xv,xl,xs,xp
    !
    integer(ip) :: ic
    real(rp)    :: P,Pv,Pair,xw,xa,rhopart,rhoaT,enthalpy
    !
    !*** Limit mass of particles (in case of aggregation a sink term appears and
    !*** mass of aggregating classes could become negative if not limited)
    !
    if(TRIM(plume%type_aggr).eq.'COSTA') then
       do ic = part%iaggr,plume%nc-1
          f(9+ic) = max(f(9+ic),0.0_rp)
       end do
    end if
    !
    !***  Extract values
    !
    Q  = f(1)                     ! Total mass flow rate
    u  = f(2)/f(1)                ! Bulk velocity
    th = f(3)                     ! Plume bent-over angle (in rad).
    En = f(4)                     ! Energy flow rate
    Ma = f(5)                     ! Mass flux of entrained air
    Mw = f(6)                     ! Mass flux of volatiles (vapor+liquid+ice)
    x  = f(7)                     ! Coordinate-X
    y  = f(8)                     ! Coordinate-Y
    z  = f(9)                     ! Height above sea level
    Mp = sum(f(9+1:9+plume%nc)) ! Mass flux of particles
    !
    xp = Mp/(Mp+Mw+Ma)            ! particle  mass fraction
    xw = Mw/(Mp+Mw+Ma)            ! volatiles mass fraction
    xa = Ma/(Mp+Mw+Ma)            ! air mass fraction
    !
    enthalpy = f(4)/f(1)-GI*z-0.5_rp*u*u ! Enthalpy per unit mass
    !
    ! Total pressure (P of external air)
    P  = Pa(z)
    ! Evaluate bulk temperature and volatile fractions
    call temperature_mixture(xa,xp,xw,T,P,xv,xl,xs,Pair,Pv,enthalpy)
    !
    !*** Air density at P(z),T
    !
    rhoaT = rhoa(z,T)
    !
    !*** Bulk density and plume radius
    !
    ! Average particle density
    if(plume%nc > 0 .and. xp > 0.0_rp) then
       rhopart = Mp/sum(f(9+1:9+plume%nc)/part%rhop(1:plume%nc))
    else
       rhopart = 1.0_rp   ! Arbitrary: not relevant if xp=0
    end if
    !
    ! Bulk density
    rho = 1.0_rp/(xp/rhopart + xl/rhol + xs/rhos + xa/rhoaT + xv/density_vapour(T,P))
    !
    r  = sqrt(Q/(pi*rho*u))   ! Plume radius
    !
    return
  end subroutine getplume
  !
  !
  !
  subroutine setplume(is,f,x,y,z,s,th,u,T,Q,En,r,rho,Ma,Mw,xv,xl,xs,a_shear,a_vortex)
    !
    !*** Store values in the plume structure
    !
    implicit none
    real(rp),    intent(in) :: x,y,z,s,th,u,T,Q,En,r,rho,Ma,Mw,xv,xl,xs,a_shear,a_vortex
    real(rp),    intent(in) :: f(plume%neq)
    integer(ip), intent(in) :: is
    !
    plume%x     (is) = x
    plume%y     (is) = y
    plume%z     (is) = z
    plume%L     (is) = s
    plume%H     (is) = th
    plume%u     (is) = u
    plume%T     (is) = T
    plume%Q     (is) = Q
    plume%En    (is) = En
    plume%R     (is) = r
    plume%D     (is) = rho
    plume%D_rhoa(is) = rho/rhoa(z)
    plume%Mair  (is) = Ma
    plume%Mw    (is) = Mw
    plume%xv    (is) = xv
    plume%xl    (is) = xl
    plume%xs    (is) = xs
    plume%as    (is) = a_shear
    plume%av    (is) = a_vortex
    plume%M(1:plume%nc,is) = f(9+1:9+plume%nc)
    if(plume%type_aggr.eq.'COSTA') plume%Magr(1:plume%nc,is) = f(9+plume%nc+1:9+plume%nc+plume%nc)
    !
    return
  end subroutine setplume
  !
  !
  subroutine set_latent_heat_flag(lh)
    ! This subroutine sets the logical value of the variable latent_heat
    implicit none
    logical, intent(IN   ) :: lh
    plume%latent_heat = lh
    if(plume%latent_heat .eqv. .true.) then
       hl0=hs0+L_sl
       hv0=hl0+L_lv
       Cv = Cv0
       Cl = Cl0
       Cs = Cs0
    else
       ! No latent heat
       hl0= Cw*Tref
       hv0= Cw*Tref
       Cv = Cw
       Cl = Cw
       Cs = Cw
    end if
  end subroutine set_latent_heat_flag


  real(rp) function enthalpy_particles(temp)
    ! Enthalpy per unit mass of solid particles
    implicit none
    real(rp), intent(IN   )  :: temp  ! Temperature
    enthalpy_particles = Cp*temp
  end function enthalpy_particles

  real(rp) function enthalpy_air(temp)
    ! Enthalpy per unit mass of air
    implicit none
    real(rp), intent(IN   )  :: temp  ! Temperature
    enthalpy_air = Ca*temp
  end function enthalpy_air

  real(rp) function enthalpy_vapour(temp)
    ! Enthalpy per unit mass of water vapor
    implicit none
    real(rp), intent(IN   )  :: temp  ! Temperature
    enthalpy_vapour = hv0 + Cv*(temp-Tref)
  end function enthalpy_vapour

  real(rp) function enthalpy_liquid(temp)
    ! Enthalpy per unit mass of liquid water
    implicit none
    real(rp), intent(IN   )  :: temp  ! Temperature
    enthalpy_liquid = hl0 + Cl*(temp-Tref)
  end function enthalpy_liquid

  real(rp) function enthalpy_ice(temp)
    ! Enthalpy per unit mass of ice
    implicit none
    real(rp), intent(IN   )  :: temp  ! Temperature
    enthalpy_ice = hs0 + Cs*(temp-Tref)
  end function enthalpy_ice

  real(rp) function density_air(temp,pres)
    ! Density of air
    implicit none
    real(rp), intent(IN   ) :: temp,pres
    density_air = pres/(rair*temp)
  end function density_air

  real(rp) function density_vapour(temp,pres)
    ! Density of vapour
    implicit none
    real(rp), intent(IN   ) :: temp,pres
    density_vapour = pres/(rvapour*temp)
  end function density_vapour

  subroutine get_gas_molar_fractions(xa,xv,na,nv)
    ! Get molar fractions of vapour and air in the gas phase (na+nv=1)
    ! Safe result for xa=xv=0
    implicit none
    real(rp), intent(IN   )  :: xa,xv
    real(rp), intent(out) :: na,nv
    real(rp) :: ntot
    ntot = xa/pmola+xv/pmolw
    if(ntot /= 0.0_rp) then
       na = xa/(pmola*ntot)
       nv = xv/(pmolw*ntot)
    else
       na = 0.0_rp
       nv = 0.0_rp
    end if
    return
  end subroutine get_gas_molar_fractions

  subroutine saturation_pressure_over_liquid(T,psat)
    ! Vapor saturation pressure over liquid in Pa (T > Tref)
    implicit none
    real(rp), intent(IN   )  :: T
    real(rp), intent(out) :: psat
    psat = 611.22_rp*exp(17.67_rp*(T-273.16_rp)/(T-29.65_rp))
  end subroutine saturation_pressure_over_liquid

  subroutine saturation_pressure_over_ice(T,psat)
    ! Vapor saturation pressure over solid in Pa (T < Tref)
    implicit none
    real(rp), intent(IN   )  :: T
    real(rp), intent(out) :: psat
    psat = -9.09718_rp*(273.16_rp/T-1.0_rp)-3.56654_rp*Log10(273.16_rp/T)   &
         +0.87679_rp*(1.0_rp-T/273.16_rp)
    ! psat = 610.71_rp*(10.0_rp**psat)  ! Original
    psat = 611.22_rp*(10.0_rp**psat)    ! Modified by G.Macedonio
  end subroutine saturation_pressure_over_ice

  subroutine temperature_mixture(xa,xp,xw,temp,pres,xv,xl,xs,pa,pv,enthalpy)
    ! Returns the temperature of air+particles+water mixture with known
    ! total enthalpy. It also evaluates xv,xl,xs
    ! The sum xw+xl+xp must be 1 (no check)
    implicit none
    real(rp), parameter :: tol=1e-7_rp  ! Tolerance in temperature
    real(rp), parameter :: tmin=0.0_rp, tmax=3273.16_rp ! Search interval
    real(rp), intent(in)  :: xa,xp,xw  ! Air, particles and water fractions
    real(rp), intent(out) :: temp      ! Temperature
    real(rp), intent(in)  :: pres      ! Total pressure
    real(rp), intent(out) :: xv,xl,xs  ! Mass fractions of vapour,liquid,ice
    real(rp), intent(out) :: pa,pv     ! Partial pressures of air and vapour
    real(rp), intent(in)  :: enthalpy  ! Enthalpy of the mixture
    real(rp) :: ttry,tleft,tright
    real(rp) :: htry
    !
    ttry = tref
    tright = tmax
    tleft  = tmin
    ! Bisection
    do
       ttry = 0.5_rp*(tleft+tright)
       call enthalpy_mixture(xa,xp,xw,ttry,pres,xv,xl,xs,pa,pv,htry)
       if(htry == enthalpy) then
          temp = ttry
          return
       end if
       if(htry > enthalpy) then
          tright = ttry
       else
          tleft = ttry
       end if
       if(abs(tleft-tright) < tol) exit   ! Exit from loop
    end do
    !
    temp = 0.5_rp*(tleft+tright)
  end subroutine temperature_mixture

  subroutine enthalpy_mixture(xa,xp,xw,temp,pres,xv,xl,xs,pa,pv,enthalpy)
    ! Returns the enthalpy of air+particles+water mixture at equilibrium
    ! at given pressure and temperature.
    ! The sum xa+xp+xw must be equal to  1 (no check)
    implicit none
    real(rp), parameter :: tol=1e-7_rp
    real(rp), intent(in)  :: xa,xp,xw  ! Air, particles and water fractions
    real(rp), intent(in)  :: temp      ! Temperature
    real(rp), intent(in)  :: pres      ! Total pressure
    real(rp), intent(out) :: xv,xl,xs  ! Mass fractions of vapour,liquid,ice
    real(rp), intent(out) :: pa,pv     ! Partial pressures of air and vapour
    real(rp), intent(out) :: enthalpy  ! Enthalpy of the mixture
    real(rp) :: psat
    real(rp) :: na,nv                  ! Molar fractions of air and vapour
    real(rp) :: hp,ha,hl,hv,hs

    ha = enthalpy_air(temp)
    hp = enthalpy_particles(temp)

    if(xw == 0.0_rp) then
       ! Air and/or particles (no water)
       xl = 0.0_rp
       xs = 0.0_rp
       xv = 0.0_rp
       pv = 0.0_rp
       pa = pres
       enthalpy = xp*hp + xa*ha
       return
    end if

    ! Air + water + particles
    ! Guess vapour pressure (initially assume all water is vapour)
    call get_gas_molar_fractions(xa,xw,na,nv)
    hv = enthalpy_vapour(temp)    ! Enthalpy of vapour
    pv = nv*pres
    if(temp < Tref) then
       call saturation_pressure_over_ice(temp,psat)
       xl = 0.0_rp
       if(pv < psat) then
          ! Only vapour
          xv = xw
          xs = 0.0_rp
          pa = na*pres
          enthalpy = xp*hp + xa*ha + xv*hv
          ! write(*,*) '# P1 only vapour'
       else
          ! Ice + vapour
          pv = psat     ! Re-evaluate pv
          nv = pv/pres
          na = 1.0_rp-nv
          if(xa > tol) then
             xv = nv*pmolw*xa/(pmola*na)
          else
             xv = 0.0_rp
          end if
          hs = enthalpy_ice(temp)       ! Enthalpy of ice
          xs = xw-xv
          pa = na*pres
          enthalpy = xp*hp + xa*ha + xv*hv + xs*hs
          ! write(*,*) '# P2 Ice+vapour'
       end if
    else
       call saturation_pressure_over_liquid(temp,psat)
       if(pv < psat) then
          ! Only vapour
          xv = xw
          xl = 0.0_rp
          xs = 0.0_rp
          pa = na*pres
          enthalpy = xp*hp + xa*ha + xv*hv
          ! write(*,*) '# P3 only vapour (T>Tref)'
       else
          ! Liquid + vapour
          pv = psat     ! Re-evaluate pv
          nv = pv/pres
          na = 1.0_rp-nv
          pa = na*pres
          if(xa > tol) then
             xv = nv*pmolw*xa/(pmola*na)
          else
             xv = 0.0_rp
          end if
          hl = enthalpy_liquid(temp)    ! Enthalpy of liquid water
          xl = xw-xv
          xs = 0.0_rp
          enthalpy = xp*hp + xa*ha + xv*hv + xl*hl
          ! write(*,*) '# P4 liquid + vapour'
       end if
    end if
  end subroutine enthalpy_mixture

  real(rp) function Ta(z)
    !****************************************************************
    !*
    !*   Gets air temperature (K) at z (in m a.s.l.)
    !*
    !*   Standard atmosphere from: http://www.pdas.com/atmosdef.html
    !*
    !****************************************************************
    implicit none
    real(rp) :: z
    !
    logical     :: go_on
    integer(ip) :: iz1,iz2
    real(rp)    :: f1,f2
    !
    if(profile%standard_atmosphere) then
       !*** Standard atmosphere
       Ta = stdatm_ta(z)
       !
    else
       !
       !*** Values interpolated from a profile
       !
       if(z.lt.profile%z(1)) then
          Ta = profile%T(1)
          return
       end if
       !
       if(z.gt.profile%z(profile%nz)) then
          Ta = profile%T(profile%nz)
          return
       end if
       !
       !*** Gets the position indexes iz1,iz2  and the weights
       !
       iz1 = 0
       go_on = .true.
       do while(go_on)
          iz1 = iz1 + 1
          iz2 = iz1 + 1
          if(z.ge.profile%z(iz1).and.z.lt.profile%z(iz2)) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
          else if(iz2.eq.profile%nz) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
             !call task_wriwarn(MY_ERR,'Source position not found in function Ta')
          end if
       end do
       !
       Ta = f1*profile%T(iz1)+f2*profile%T(iz2)
       !
    end if
  end function Ta
  !
  !
  !
  real(rp) function Pa(z)
    !**************************************************************
    !*
    !*   Gets air pressure (in Pa) at z (in m a.s.l.)
    !*
    !**************************************************************
    implicit none
    real(rp) :: z
    !
    logical     :: go_on
    integer(ip) :: iz1,iz2
    real(rp)    :: f1,f2
    !
    if(profile%standard_atmosphere) then
       !
       !*** Standard atmosphere
       !
       Pa = stdatm_pa(z)
       !
    else
       !
       !*** Values interpolated from a profile
       !
       if(z.lt.profile%z(1)) then
          Pa = profile%P(1)
          return
       end if
       !
       if(z.gt.profile%z(profile%nz)) then
          Pa = profile%P(profile%nz)
          return
       end if
       !
       !*** Gets the position indexes iz1,iz2  and the weights
       !
       iz1 = 0
       go_on = .true.
       do while(go_on)
          iz1 = iz1 + 1
          iz2 = iz1 + 1
          if(z.ge.profile%z(iz1).and.z.lt.profile%z(iz2)) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
          else if(iz2.eq.profile%nz) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
             !call task_wriwarn(MY_ERR,'Source position not found in function Pa')
          end if
       end do
       !
       Pa = f1*profile%P(iz1)+f2*profile%P(iz2)
       !
    end if
    return
  end function Pa
  !
  !
  !
  real(rp) function rhoa_z(z)
    !**************************************************************
    !*
    !*    Interpolates air density at z (in m a.s.l.)
    !*
    !**************************************************************
    implicit none
    real(rp) :: z
    !
    logical     :: go_on
    integer(ip) :: iz1,iz2
    real(rp)    :: f1,f2
    !
    if(profile%standard_atmosphere) then
       !
       !*** Standard atmosphere
       !
       rhoa_z = stdatm_rhoa(z)
       !
    else
       !
       !*** Values interpolated
       !
       if(z.lt.profile%z(1)) then
          rhoa_z = profile%rho(1)
          return
       end if
       !
       if(z.gt.profile%z(profile%nz)) then
          rhoa_z = profile%rho(profile%nz)
          return
       end if
       !
       !*** Gets the position indexes iz1,iz2 and the weights
       !
       iz1 = 0
       go_on = .true.
       do while(go_on)
          iz1 = iz1 + 1
          iz2 = iz1 + 1
          if(z.ge.profile%z(iz1).and.z.lt.profile%z(iz2)) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
          else if(iz2.eq.profile%nz) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
             !call task_wriwarn(MY_ERR,'Source position not found in function rhoa')
          end if
       end do
       !
       rhoa_z = f1*profile%rho(iz1)+f2*profile%rho(iz2)
       !
    end if
    return
  end function rhoa_z
  !
  ! Air density (function of z and T)
  !
  real(rp) function rhoa_z_T(z,T)
    implicit none
    real(rp), intent(in) :: z,T
    rhoa_z_T = rhoa(z)*Ta(z)/T
  end function rhoa_z_T
  !
  !
  !
  real(rp) function Va(z)
    !**************************************************************
    !*
    !*    Interpolates air velocity (m/s) at z (in m a.s.l.)
    !*    from Vair
    !*
    !**************************************************************
    implicit none
    real(rp) :: z
    !
    logical     :: go_on
    integer(ip) :: iz1,iz2
    real(rp)    :: f1,f2
    !
    if(profile%standard_atmosphere) then
       !
       !*** Standard atmosphere
       !
       Va  = 0.0
       !
    else
       !
       !*** Values interpolated from a profile
       !
       if(z.lt.profile%z(1)) then
          Va = profile%u(1)
          return
       end if
       !
       if(z.gt.profile%z(profile%nz)) then
          Va = profile%u(profile%nz)
          return
       end if
       !
       !*** Gets the position indexes iz1,iz2  and the weights
       !
       iz1 = 0
       go_on = .true.
       do while(go_on)
          iz1 = iz1 + 1
          iz2 = iz1 + 1
          if(z.ge.profile%z(iz1).and.z.lt.profile%z(iz2)) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
          else if(iz2.eq.profile%nz) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
             !call task_wriwarn(MY_ERR,'Source position not found in function Va')
          end if
       end do
       !
       Va = f1*profile%u(iz1)+f2*profile%u(iz2)
       !
    end if
    return
  end function Va
  !
  !
  !
  real(rp) function Da(z)
    !**************************************************************
    !*
    !*    Interpolates air direction
    !*
    !**************************************************************
    implicit none
    real(rp) :: z
    !
    logical     :: go_on
    integer(ip) :: iz1,iz2
    real(rp)    :: f1,f2
    !
    if(profile%standard_atmosphere) then
       !
       !*** Standard atmosphere
       !
       Da  = 0.0
       !
    else
       !
       !*** Values interpolated from a profile
       !
       if(z.lt.profile%z(1)) then
          Da = profile%PSIa(1)
          return
       end if
       !
       if(z.gt.profile%z(profile%nz)) then
          Da = profile%PSIa(profile%nz)
          return
       end if
       !
       !*** Gets the position indexes iz1,iz2  and the weights
       !
       iz1 = 0
       go_on = .true.
       do while(go_on)
          iz1 = iz1 + 1
          iz2 = iz1 + 1
          if(z.ge.profile%z(iz1).and.z.lt.profile%z(iz2)) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
          else if(iz2.eq.profile%nz) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
             !call task_wriwarn(MY_ERR,'Source position not found in function Da')
          end if
       end do
       !
       Da = f1*profile%PSIa(iz1)+f2*profile%PSIa(iz2)
       !
    end if
    return
  end function Da
  !
  !
  !
  real(rp) function sh_a(z)
    !**************************************************************
    !*
    !*    Interpolates air specific humidity at z (in m a.s.l.)
    !*
    !**************************************************************
    implicit none
    real(rp) :: z
    !
    logical     :: go_on
    integer(ip) :: iz1,iz2
    real(rp)    :: f1,f2
    !
    if(profile%standard_atmosphere) then
       !
       !*** Standard atmosphere (dry)
       !
       sh_a = 0.0
       !
    else
       !
       !*** Values interpolated
       !
       if(z.lt.profile%z(1)) then
          sh_a = profile%sh(1)
          return
       end if
       !
       if(z.gt.profile%z(profile%nz)) then
          sh_a = profile%sh(profile%nz)
          return
       end if
       !
       !*** Gets the position indexes iz1,iz2 and the weights
       !
       iz1 = 0
       go_on = .true.
       do while(go_on)
          iz1 = iz1 + 1
          iz2 = iz1 + 1
          if(z.ge.profile%z(iz1).and.z.lt.profile%z(iz2)) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
          else if(iz2.eq.profile%nz) then
             go_on = .false.
             f1 = 1.0-((z-profile%z(iz1))/(profile%z(iz2)-profile%z(iz1)))
             f2 = 1.0-f1
             !call task_wriwarn(MY_ERR,'Source position not found in function sha')
          end if
       end do
       !
       sh_a = f1*profile%sh(iz1)+f2*profile%sh(iz2)
       !
    end if
    return
  end function sh_a
  !
  !
  !
  real(rp) function dVadz(z)
    !**************************************************************
    !*
    !*    Interpolates air velocity gradient
    !*    from Vair
    !*
    !**************************************************************
    implicit none
    real(rp) :: z
    !
    logical     :: go_on
    integer(ip) :: iz1,iz2
    !
    if(profile%standard_atmosphere) then
       !
       !*** Standard atmosphere
       !
       dVadz  = 0.0
       !
    else
       !
       !*** Values interpolated
       !
       if(z.lt.profile%z(1)) then
          dVadz = profile%u(1)/profile%z(1)
          return
       end if
       !
       if(z.gt.profile%z(profile%nz)) then
          dVadz = (profile%u(profile%nz)- profile%u(profile%nz-1))/ &
               (profile%z(profile%nz)- profile%z(profile%nz-1))
          return
       end if
       !
       !*** Gets the position indexes iz1,iz2  and the weights
       !
       iz1 = 0
       go_on = .true.
       do while(go_on)
          iz1 = iz1 + 1
          iz2 = iz1 + 1
          if(z.ge.profile%z(iz1).and.z.lt.profile%z(iz2)) then
             go_on = .false.
             dVadz = (profile%u(iz2)- profile%u(iz1))/ &
                  (profile%z(iz2)- profile%z(iz1))
          else if(iz2.eq.profile%nz) then
             go_on = .false.
             dVadz = (profile%u(iz2)- profile%u(iz1))/ &
                  (profile%z(iz2)- profile%z(iz1))
             !call task_wriwarn(MY_ERR,'Source position not found in function dVadz')
          end if
       end do
       !
    end if
    return
  end function dVadz
  !
  !
  !
  real(rp) function mua(T)
    !***********************************************************
    !*
    !*    Computes air viscosity at temperature T
    !*
    !************************************************************
    implicit none
    real(rp) :: T,mua0,Tair0
    !
    mua0  = 1.827e-5_rp  ! reference viscosity
    Tair0 = 291.15_rp    ! reference temperature
    !
    !*** Sutherland's law
    !
    mua = mua0*((Tair0+120.0)/(T+120.0))*((T/Tair0)**1.5)
    !
    return
  end function mua
  !
  !
  !
  logical function is_collapse(plume)
    ! Check if column is collapsed (actually checks status flag)
    implicit none
    type(volcanic_plume), intent(in) :: plume
    is_collapse = .false.
    if(iand(plume%status,STATUS_COLLAPSE) /= 0) is_collapse = .true.
  end function is_collapse





  !
  !
  !
END MODULE PlumeBPT
