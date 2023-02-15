!***************************************************************
!>
!> Module for definition of global parameters and types
!> @author
!> Arnau Folch
!>
!***************************************************************
MODULE KindType
  use Config
  implicit none
  save
  !
  !    LIST OF PUBLIC VARIABLES
  !
  integer,     parameter :: ip = 4                          !< Integer precision
#if defined WITH_R4
  integer,     parameter :: rp = 4                          !< Real    precision
#else
  integer,     parameter :: rp = 8                          !< Real    precision
#endif
  integer(ip), parameter :: s_name  = 64                    !< Generic string lenght
  integer(ip), parameter :: s_mess  = 512                   !< Message string lenght
  integer(ip), parameter :: s_file  = 512                   !< File path string lenght
  !
  integer(ip)            :: TASK_FLAG(0:4)  = (/0,0,0,0,0/) !< task         flags
  integer(ip), parameter :: TASK_RUN_ALL    = 0             !< All     task flag
  integer(ip), parameter :: TASK_SET_TGSD   = 1             !< SetTgsd task flag
  integer(ip), parameter :: TASK_SET_DBS    = 2             !< SetDbs  task flag
  integer(ip), parameter :: TASK_SET_SRC    = 3             !< SetSrc  task flag
  integer(ip), parameter :: TASK_RUN_FALL3D = 4             !< Fall3d  task flag
  !
  integer(ip), parameter :: LOG_LEVEL_NONE   = 0             !< log file level flag for none
  integer(ip), parameter :: LOG_LEVEL_NORMAL = 1             !< log file level flag for normal
  integer(ip), parameter :: LOG_LEVEL_FULL   = 2             !< log file level flag for full
  !
  real(rp), parameter    :: EPSILON = 1e-12_rp               !< Value of epsilon (numerical zero)
  real(rp), parameter    :: PI      = 4.0_rp*atan(1.0_rp)    !< Value of pi
  real(rp), parameter    :: REARTH  = 6356000.0_rp           !< Radius of the Earth in m
  real(rp), parameter    :: GI      = 9.81_rp                !< Gravity
  real(rp), parameter    :: KARMAN  = 0.4_rp                 !< VonKarman constant
  real(rp), parameter    :: CA0     = 998.0_rp               !< Specific heat capacity at constant pressure of dry air               (J kg^-1 K^-1)
  real(rp), parameter    :: CV0     = 1996.0_rp              !< Specific heat of water vapour
  real(rp), parameter    :: CP0     = 1250.0_rp              !< Specific heat capacity at constant pressure of particles             (J kg^-1 K^-1)
  real(rp), parameter    :: CL0     = 4187.0_rp              !< Specific heat capacity at constant pressure of water liquid          (J kg^-1 K^-1)
  real(rp), parameter    :: CS0     = 2108.0_rp              !< Specific heat capacity at constant pressure of water solid (ice)     (J kg^-1 K^-1)
  real(rp), parameter    :: CW0     = 2000.0_rp              !< Specific heat capacity at constant water (generic T)                 (J kg^-1 K^-1)
  !
  integer(ip), parameter :: CAT_PARTICLE     = 1
  integer(ip), parameter :: CAT_AEROSOL      = 2
  integer(ip), parameter :: CAT_RADIONUCLIDE = 3
  !
  integer(ip), parameter :: SPE_TEPHRA = 1
  integer(ip), parameter :: SPE_DUST   = 2
  integer(ip), parameter :: SPE_H2O    = 3
  integer(ip), parameter :: SPE_SO2    = 4
  integer(ip), parameter :: SPE_CS134  = 5
  integer(ip), parameter :: SPE_CS137  = 6
  integer(ip), parameter :: SPE_I131   = 7
  integer(ip), parameter :: SPE_SR90   = 8
  integer(ip), parameter :: SPE_Y90    = 9
  !
  integer(ip), parameter :: nspe_max = 9
  character(len=12), dimension(nspe_max) :: SPE_BLOCK = (/ &
                                                    'TEPHRA_TGSD ', &
                                                    'DUST_TGSD   ', &
                                                    'NONE        ', &
                                                    'NONE        ', &
                                                    'CS134_TGSD  ', &
                                                    'CS137_TGSD  ', &
                                                    'I131_TGSD   ', &
                                                    'SR90_TGSD   ', &
                                                    'Y90_TGSD    '/)
  !
  !    DEFINITION OF TYPES
  !
  !
  !>   type ERROR_STATUS: error handler and descriptor
  !
  type ERROR_STATUS
     integer(ip)       :: flag            !< Error flag: NO_ERROR = 0
     character(s_mess) :: source          !< Name of the subroutine/module that has generated the error
     character(s_mess) :: message         !< Detailed error message
     !
     integer(ip)       :: nwarn = 0       !< Number of warning messages
     character(s_mess) :: warning(100)    !< Detailed warning  messages
     real(rp)          :: cpu_start_time  !< CPU start time
     real(rp)          :: cpu_end_time    !< CPU end   time
  end type ERROR_STATUS
  !
  !>   type FILE_LIST: list of file logic units and names
  !
  type FILE_LIST
     integer(ip)       :: lulog     = 10         !< log       file logical unit
     integer(ip)       :: lusrc     = 11         !< src       file logical unit
     integer(ip)       :: luplu_res = 12         !< plume.res file logical unit
     integer(ip)       :: lugc_res  = 13         !< gc.res    file logical unit
     !
     character(s_file) :: problempath  = ' '      !< problem path
     character(s_file) :: problemname  = ' '      !< problem name
     character(s_file) :: file_log     = ' '      !< Name of the log   file
     character(s_file) :: file_inp     = ' '      !< Name of the input file
     character(s_file) :: file_tgsd    = '-'      !< Name of the tgsd  file
     character(s_file) :: file_dbs     = '-'      !< Name of the dbs   file
     character(s_file) :: file_met     = '-'      !< Name of the meto  file
     character(s_file) :: file_pro     = '-'      !< Name of the meteo profile  file
     character(s_file) :: file_grn     = '-'      !< Name of the grn   file
     character(s_file) :: file_src     = '-'      !< Name of the src   file
     character(s_file) :: file_plu     = '-'      !< Name of the plume file
     character(s_file) :: file_gc      = '-'      !< Name of the gravity current file
     character(s_file) :: file_pts     = '-'      !< Name of the pts   file
     character(s_file) :: file_res     = '-'      !< Name of the res   file
     character(s_file) :: file_rst     = '-'      !< Name of the rst   file
     character(s_file) :: file_sat     = '-'      !< Name of the sat   file
     character(s_file) :: file_hyb     = '-'      !< Name of the hyb   file (hybrid meteo models lookup table)
     character(s_file) :: file_tbl_met = '-'      !< Name of the tbl   file (meteo model diccionary table)
     character(s_file) :: file_tbl_sat = '-'      !< Name of the tbl   file (sat data    diccionary table)
     !
  end type FILE_LIST
  !
  !>   type SPECIES_PARAMS: list of parameters defining categories and species
  !
  type SPECIES_PARAMS
     !
     logical                  :: exists_tephra       = .false.
     logical                  :: exists_dust         = .false.
     logical                  :: exists_radionuclide = .false.
     logical                  :: exists_aerosol      = .false.
     !
     integer(ip)              :: nspe = 0      !<  number of species
     integer(ip), allocatable :: code    (:)   !< code    (nspe) specie   type code
     integer(ip), allocatable :: category(:)   !< category(nspe) category type code
     real(rp),    allocatable :: mf      (:)   !< mf      (nspe) specie   mass fractionç
     !
     character(s_name), allocatable :: name(:) !< name(nspe) specie name (tag)
     !
  end type SPECIES_PARAMS
  !
  !>   type TGSD_PARAMS: list of parameters defining a TGSD
  !
  type TGSD_PARAMS
     !
     integer(ip)       :: nbins             !<  number of bins
     integer(ip)       :: ng                !<  number blended distributions
     !
     real(rp)          :: fimean (2)        !<  mean value of fi for GAUSSIAN/BIGAUSSIAN distributions
     real(rp)          :: fidisp (2)        !<  disp value of fi for GAUSSIAN/BIGAUSSIAN distributions
     real(rp)          :: pweight(2)        !<  weights of the phi-distributions
     real(rp)          :: fimin             !<  minimum value of fi in distribution
     real(rp)          :: fimax             !<  maximum value of fi in distribution
     real(rp)          :: rhomin            !<  minimum value of particle density in distribution
     real(rp)          :: rhomax            !<  maximum value of particle density in distribution
     real(rp)          :: sphemin           !<  minimum value of particle sphericity in distribution
     real(rp)          :: sphemax           !<  maximum value of particle sphericity in distribution
     real(rp)          :: m63               !<  mass fraction of particles less than 64um (fi=4)
     real(rp)          :: visco             !<  magma viscosity (needed only to estimate BIGAUSSIAN distributions)
     !
     character(s_name) :: type_dist         !< type of distribution. Options: GAUSSIAN/BIGAUSSIAN/WEIBULL/BIWEIBULL/CUSTOM/ESTIMATE
     !
     real(rp), allocatable :: fc  (:)       !< fc  (nbins)  particle mass fraction
     real(rp), allocatable :: rhop(:)       !< rhop(nbins)  particle density
     real(rp), allocatable :: diam(:)       !< diam(nbins)  particle diameter
     real(rp), allocatable :: fi  (:)       !< fi  (nbins)  particle fi number
     real(rp), allocatable :: sphe(:)       !< shpe(nbins)  particle fi number
     real(rp), allocatable :: psi (:)       !< psi (nbins)  particle shape factor
     !
  end type TGSD_PARAMS
  !
  !>   type BIN_PARAMS: list of parameters defining bin granulometric properties
  !
  type BIN_PARAMS
     !
     integer(ip) :: nbins     = 0                   !< total number of bins
     integer(ip) :: nbins_par = 0                   !< total number of particle bins (tephra, dust, radionuclides)
     integer(ip) :: nbins_gas = 0                   !< total number of aerosol  bins (gas)
     integer(ip) :: is_aggr   = 0                   !< starting bin of aggregating classes
     !
     logical,           allocatable :: bin_effe(:)  !< bin_effe(nbins)  effective bin flag
     character(s_name), allocatable :: bin_type(:)  !< bin_type(nbins)  type of bin
     character(s_name), allocatable :: bin_name(:)  !< bin_name(nbins)  name of bin
     integer(ip),       allocatable :: bin_cat (:)  !< bin_cat (nbins)  bin category code
     integer(ip),       allocatable :: bin_spe (:)  !< bin_spe (nbins)  bin specie   code
     !
     real(rp), allocatable :: bin_fc  (:)           !< bin_fc  (nbins)  bin mass fraction
     real(rp), allocatable :: bin_rho (:)           !< bin_rhop(nbins)  bin density
     real(rp), allocatable :: bin_diam(:)           !< bin_diam(nbins)  bin diameter
     real(rp), allocatable :: bin_sphe(:)           !< bin_sphe(nbins)  bin shpericity
     real(rp), allocatable :: bin_psi (:)           !< bin_psi (nbins)  bin shape factor (velocity model dependent)
     !
  end type BIN_PARAMS
  !
  !>   type AGR_PARAMS: list of parameters defining an aggregation model
  !
  type AGR_PARAMS
     !
     logical     :: aggregation = .false.           !< aggregation flag
     integer(ip) :: nbins_aggr  = 0                 !< total number of aggregate bins
     !
     character(len=s_name) :: aggregation_model     !< aggregation model: NONE / PERCENTAGE / CORNELL / COSTA
     !
     real(rp)    :: diam_aggr                       !< aggregate diameter (1 class)
     real(rp)    :: vset_fac                        !< aggregate setling velocity correction factor
     real(rp)    :: Dfo                             !< aggregate setling velocity correction factor
     !
     real(rp), allocatable :: diam      (:)         !< diam      (nbins_aggr) diameter   of aggregates
     real(rp), allocatable :: rho       (:)         !< rho       (nbins_aggr) density    of aggregates
     real(rp), allocatable :: percentage(:)         !< percentage(nbins_aggr) percentage of aggregates
     !
  end type AGR_PARAMS
  !
  !>   type ESP_PARAMS: list of parameters defining a source term (e.g. Eruption Source Parameters)
  !
  type ESP_PARAMS
     !
     logical :: meteo_coupling                      !< source meteo coupling flag
     !
     character(s_name) :: source_type               !< type of source term: POINT / SUZUKI / TOP-HAT / PLUME / RESUSPENSION
     character(s_name) :: MER_vs_h                  !< derive MER from h: NONE / ESTIMATE-MASTIN / ESTIMATE-WOODHOUSE
     !
     integer(ip) :: ndt                             !< number of steps (e.g. eruption phases)
     integer(ip) :: meteo_coupling_interval         !< source meteo coupling time interval (in s)
     integer(ip) :: start_year                      !< run start year
     integer(ip) :: start_month                     !< run start month
     integer(ip) :: start_day                       !< run start day
     !
     integer(ip), allocatable :: start_time(:)      !< start_time(ndt) start time of each source phase (in s after 00:00 UTC)
     integer(ip), allocatable :: end_time  (:)      !< end_time  (ndt) end   time of each source phase (in s after 00:00 UTC)
     !
     real(rp)  :: lon                               !< source vent longitude
     real(rp)  :: lat                               !< source vent latitude
     real(rp)  :: zo                                !< source vent altitude
     real(rp)  :: profile_time_lag                  !< meteorological profile time lag (computed if necessary)
     real(rp)  :: alfa_plume                        !< radial entrainment coefficient
     real(rp)  :: beta_plume                        !< wind entrainment coefficient
     !
     real(rp), allocatable    :: h_dt (:)           !< h_dt (ndt)  plume height        of each source phase (a.v.l.)
     real(rp), allocatable    :: M0_dt(:)           !< M0_dt(ndt)  MER                 of each source phase
     real(rp), allocatable    :: w0_dt(:)           !< w0_dt(ndt)  water fraction      of each source phase
     real(rp), allocatable    :: T0_dt(:)           !< T0_dt(ndt)  mixture temperature of each source phase
     real(rp), allocatable    :: As_dt(:)           !< As_dt(ndt)  A-Suzuki            of each source phase
     real(rp), allocatable    :: Ls_dt(:)           !< Ls_dt(ndt)  L-Suzuki            of each source phase
     real(rp), allocatable    :: Th_dt(:)           !< Th_dt(ndt)  Hat thickness       of each source phase
     !
  end type ESP_PARAMS
  !
  !>   type PLUME_PARAMS: list of parameters needed by the BPT plume model
  !
  type PLUME_PARAMS
     !
     logical :: moist_air                           !< consider entrainment of moist from ambient air
     logical :: wind_coupling                       !< consider wind coupling
     logical :: reentrainment                       !< consider particle reentrainment
     logical :: latent_heat                         !< consider the effect of latent heat
     !
     character(len=s_name) :: solve_plume_for       !< Plume solving strategy
     character(len=s_name) :: type_as               !< Default parameterization type
     character(len=s_name) :: type_av               !< Default parameterization type
     character(len=s_name) :: umbrella_model        !< Default umbrella model
     character(len=3     ) :: zone_UTM              !< UTM zone
     !
     integer(ip) :: ndt                             !< number of steps (e.g. eruption phases)
     integer(ip) :: ns                              !< number of plume sources (plume+umbrella)
     integer(ip) :: np                              !< number of plume sources (with no umbrella)
     integer(ip) :: time1                           !< time1 interval
     integer(ip) :: time2                           !< time2 interval
     !
     real(rp) :: xv_UTM                             !< Vent UTM-X coordinate
     real(rp) :: yv_UTM                             !< Vent UTM-Y coordinate
     real(rp) :: zv                                 !< Vent altitude (m)
     real(rp) :: n_MFR(2)                           !< MER search range
     real(rp) :: xi                                 !< Bursik factor
     real(rp) :: zmin_wind                          !< Ignore wind entrainment below this zvalue (low jet region)
     real(rp) :: c_umbrella                         !< Thichness of umbrella relative to Hb (>1)
     real(rp) :: a_s_jet                            !< Default (constant) value in jet   region
     real(rp) :: a_s_plume                          !< Default (constant) value in plume region
     real(rp) :: a_v                                !< Default (constant) value
     !
     real(rp), allocatable    :: u0_dt(:)           !< u0_dt(ndt)  exit velocity            of each source phase
     real(rp), allocatable    :: Tv_dt(:)           !< Tv_dt(ndt)  gas    water temperature of each source phase
     real(rp), allocatable    :: Tl_dt(:)           !< Tl_dt(ndt)  liquid water temperature of each source phase
     real(rp), allocatable    :: Ts_dt(:)           !< Ts_dt(ndt)  solid  water temperature of each source phase
     real(rp), allocatable    :: wv_dt(:)           !< wv_dt(ndt)  vapor  water fraction    of each source phase
     real(rp), allocatable    :: wl_dt(:)           !< wl_dt(ndt)  liquid water fraction    of each source phase
     real(rp), allocatable    :: ws_dt(:)           !< ws_dt(ndt)  solid  water fraction    of each source phase
     !
  end type PLUME_PARAMS
  !
  !>   type SRC_PARAMS: variables defining a source term
  !
  type SRC_PARAMS
     !
     character(s_name) :: source_type      !< type of source term: POINT / SUZUKI / TOP-HAT / PLUME / RESUSPENSION
     !
     integer(ip) :: np                     !< number of point sources
     integer(ip) :: nbins                  !< number of effective bins
     integer(ip) :: start_time             !< source start time (in s after 00:00 UTC)
     integer(ip) :: end_time               !< source end   time (in s after 00:00 UTC)
     real   (rp) :: total_MFR              !< total MFR (all bins)
     !
     real(rp), allocatable :: x(:)         !< x(np) x-coordinate of source point
     real(rp), allocatable :: y(:)         !< y(np) y-coordinate of source point
     real(rp), allocatable :: z(:)         !< z(np) z-coordinate of source point (a.s.l.)
     real(rp), allocatable :: M(:,:)       !< M(nbins,np) mass flow rate for each point and bin
     !
  end type SRC_PARAMS
  !
  !>   type TRACERS: definition of all variables related to tracers and bin characterisitcs
  !
  type TRACERS
     !
     integer(ip) :: nbins     = 0        !< total number of bins
     integer(ip) :: nbins_par = 0        !< total number of particle bins (tephra, dust, radionuclides)
     integer(ip) :: nbins_gas = 0        !< total number of aerosol  bins (gas)
     !
     type(BIN_PARAMS) :: MY_BIN          !< list of parameters defining bin granulometric properties
     !
     real(rp)    :: gl_mass_rate = 0.0_rp     !< global mass flow rate
     real(rp)    :: gl_mass_in   = 0.0_rp     !< global mass injected in the global domain (accumulated)

     real(rp)    :: rst_mass_ground  = 0.0_rp !< restart global mass at ground
     real(rp)    :: rst_mass_lateral = 0.0_rp !< restart global mass at laterals
     real(rp)    :: rst_mass_sink    = 0.0_rp !< restart global mass from sink terms
     !
     real(rp)    :: my_N_flux    = 0.0_rp   !< total mass flux along my N boundary  (all bins)
     real(rp)    :: my_S_flux    = 0.0_rp   !< total mass flux along my S boundary  (all bins)
     real(rp)    :: my_E_flux    = 0.0_rp   !< total mass flux along my E boundary  (all bins)
     real(rp)    :: my_W_flux    = 0.0_rp   !< total mass flux along my W boundary  (all bins)
     real(rp)    :: my_U_flux    = 0.0_rp   !< total mass flux along my U boundary  (all bins)
     real(rp)    :: my_D_flux    = 0.0_rp   !< total mass flux along my D boundary  (all bins)
     !
     real(rp), allocatable :: my_c   (:,:,:,:) !< my_c   (my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h,my_kps_2h:my_kpe_2h,1:nbins)  scaled tracer concentration at mass points
     real(rp), allocatable :: my_s   (:,:,:,:) !< my_s   (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   ,1:nbins)  scaled tracer source        at mass points
     real(rp), allocatable :: my_vs  (:,:,:,:) !< my_vs  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kbs_1h:my_kbe_1h,1:nbins)  settling velocity           at w-boundaries
     real(rp), allocatable :: my_acum(:,:,:  ) !< my_acum(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h                    ,1:nbins)  ground accumulation         at mass points
     real(rp), allocatable :: my_awet(:,:    ) !< my_awet(my_ips_2h:my_ipe_2h,my_jps_2h:my_jpe_2h                            )  accumulated wet deposition  at mass points
     !
  end type TRACERS
  !
  !>   type GRAVITY_CURRENT: definition of all variables related to GC model physics and parameterizations
  !
  type GRAVITY_CURRENT
     !
     real(rp)    :: c_flow_rate                  !< c      constant (0.43d3 for tropical eruptions; 0.87d3 for mid-latitude and polar)
     real(rp)    :: lambda                       !< lambda constant
     real(rp)    :: k_entrain                    !< entrainment coefficinet
     real(rp)    :: brunt_vaisala                !< Brunt-Vaisala frequency
     real(rp)    :: start_time                   !< GC starting time
     real(rp)    :: end_time                     !< GC end      time
     real(rp)    :: lon                          !< longitude of the GC center
     real(rp)    :: lat                          !< latitude  of the GC center
     real(rp)    :: mass_flow_rate               !< current total MER
     real(rp)    :: h_tot                        !< current column heigh (top)
     real(rp)    :: vol_flow_rate                !< current volumetric flow rate
     real(rp)    :: max_radius                   !< current maximum radius
     real(rp)    :: radius_grav                  !< current radius
     real(rp)    :: th_grav                      !< current thickness
     !
  end type GRAVITY_CURRENT
  !
  !>   type MODEL_PHYSICS: definition of all variables related to model physics and parameterizations
  !
  type MODEL_PHYS
     !
     logical     :: wet_deposition       !< Compute wet deposition of particles
     logical     :: dry_deposition       !< Compute dry deposition mechanisms
     logical     :: gravity_current      !< Compute dry deposition mechanisms
     !
     integer(ip) :: modv                 !< Particle terminal fall velocity model flag
     integer(ip) :: modkh                !< horizontal diffusion parameterization flag
     integer(ip) :: modkv                !< vertical   diffusion parameterization flag
     integer(ip) :: limiter              !< limiter flag
     integer(ip) :: time_marching        !< time integration scheme
     integer(ip) :: CFL_criterion        !< CFL calculation criterion flag
     !
     real(rp)    :: CFL_safety_factor            !< Courant-Friedrichs-Lewy (i.e. safety factor value <1)
     real(rp)    :: kh0                          !< horizontal diffusion default (constant) value
     real(rp)    :: kv0                          !< vertical   diffusion default (constant) value
     real(rp)    :: wet_deposition_a = 8.4e-5_rp !< wet deposition a (default from Jung and Shao 2006)
     real(rp)    :: wet_deposition_b = 0.79_rp   !< wet deposition b (default from Jung and Shao 2006)
     real(rp)    :: dry_deposition_a(6,4)        !< dry deposition a (default from Feng et al.   2008)
     real(rp)    :: dry_deposition_b(6,4)        !< dry deposition b (default from Feng et al.   2008)
     !
     integer(ip), allocatable :: dry_deposition_mode(:) !< dry deposition aerosol mode
     !
     type(GRAVITY_CURRENT) :: MY_GC
     !
  end type MODEL_PHYS
  !
  !>   type Q1_GRID: 2D Q1 grid defnition (structured)
  !
  type Q1_GRID
     integer(ip)   :: nx                       !< Number of grid points along x
     integer(ip)   :: ny                       !< Number of grid points along y
     integer(ip)   :: npoin                    !< Number of grid points
     integer(ip)   :: nelem                    !< Number of grid elements
     !
     integer(ip), allocatable :: lnods(:,:)    !< lnods(4,nelem) nodal conectivities
     real(rp),    allocatable :: coord(:,:)    !< coord(2,npoin) coordinates
     !
  end type Q1_GRID
  !
  !>   type ARAKAWA_C_GRID: Arakawa C type grid definition
  !
  type ARAKAWA_C_GRID
     integer(ip)           :: map_h       !< Horizonatal mapping flag: CARTESIAN = 0, SPHERICAL = 1, POLAR = 2
     integer(ip)           :: map_v       !< Vertical mapping flag: SIGMA_NO_DECAY = 0, SIGMA_LINEAR_DECAY = 1, SIGMA_EXPONENTIAL_DECAY = 2
     !
     real(rp)              :: lonmin      !< Longitude of the grid W side in the range (-180,180)
     real(rp)              :: lonmax      !< Longitude of the grid E side in the range (-180,180)
     real(rp)              :: latmin      !< Latitude  of the grid S side in the range ( -90,90 )
     real(rp)              :: latmax      !< Latitude  of the grid N side in the range ( -90,90 )
     real(rp)              :: X3max       !< Top of the computational domain in m. Spans in vertical form (0,X3max)
     real(rp)              :: dlon        !< Longitude resolution in deg (grid cell size)
     real(rp)              :: dlat        !< Latitude  resolution in deg (grid cell size)
     real(rp), allocatable :: gl_sigma(:) !< gl_sigma(1:gl_nbz) global values of sigma coordinate (0,1): sigma = X3/X3max
     !
     real(rp), allocatable :: lon_c(:)    !< lon_c (my_ibs:my_ibe) longitude values at my processor cell corners
     real(rp), allocatable :: lon_p(:)    !< lon_p (my_ips:my_ipe) longitude values at my processor mass points
     real(rp), allocatable :: lat_c(:)    !< lat_c (my_jbs:my_jbe) latitude  values at my processor cell corners
     real(rp), allocatable :: lat_p(:)    !< lat_p (my_jps:my_jpe) latitude  values at my processor mass points
     real(rp), allocatable :: X3_c (:)    !< X3_c  (my_kbs:my_kbe) sigma coordinate values at my processor cell corners
     real(rp), allocatable :: X3_p (:)    !< X3_p  (my_kps:my_kpe) sigma coordinate values at my processor mass points
     !
     real(rp), allocatable :: Hm1_p(:)    !< Hm1_p (my_jps   :my_jpe   )               X1 scale factor values at my processor mass points
     real(rp), allocatable :: Hm1_c(:)    !< Hm1_c (my_jbs   :my_jbe   )               X1 scale factor values at my processor boundaries
     real(rp), allocatable :: Hm2_p(:)    !< Hm2_p (my_jps_2h:my_jpe_2h)               X2 scale factor values at my processor mass points (2 halo)
     real(rp), allocatable :: Hm3_p(:,:)  !< Hm3_p (my_ips   :my_ipe,my_jps:my_jpe)    X3 scale factor values at my processor mass points
     !
     real(rp), allocatable :: dX1_p (:)   !< dX1_p (my_ips_2h:my_ipe_2h) dx at my processor mass     points (with no scaling factor)
     real(rp), allocatable :: dX1_b (:)   !< dX1_b (my_ibs_1h:my_ibe_1h) dx at my processor boundary points (with no scaling factor)
     real(rp), allocatable :: dX2_p (:)   !< dX2_p (my_jps_2h:my_jpe_2h) dy at my processor mass     points (with no scaling factor)
     real(rp), allocatable :: dX2_b (:)   !< dX2_b (my_jbs_1h:my_jbe_1h) dy at my processor boundary points (with no scaling factor)
     real(rp), allocatable :: dX3_p (:)   !< dX3_p (my_kps_2h:my_kpe_2h) dz at my processor mass     points (with no scaling factor)
     real(rp), allocatable :: dX3_b (:)   !< dX3_b (my_kbs_1h:my_kbe_1h) dz at my processor boundary points (with no scaling factor)
     !
     real(rp), allocatable :: h_c   (:,:)   !< h_c    (my_ibs:my_ibe,my_jbs:my_jbe)               topography values at my processor cell corners
     real(rp), allocatable :: dhdx_p(:,:)   !< dhdx_p (my_ips:my_ipe,my_jps:my_jpe)               topography x-gradient values at my processor mass points
     real(rp), allocatable :: dhdy_p(:,:)   !< dhdy_p (my_ips:my_ipe,my_jps:my_jpe)               topography y-gradient values at my processor mass points
     real(rp), allocatable :: z_c   (:,:,:) !< z_c    (my_ibs:my_ibe,my_jbs:my_jbe,my_kbs:my_kbe) z-coordinate at my processor cell corners
     !
  end type ARAKAWA_C_GRID
  !
  !>   type METEOROLOGY: definition of all model grid variables related to meteorology
  !
  type METEOROLOGY
     !
     character(len=s_name)    :: meteo_data_type  !< type of meteo model data. Options: WRF / GRIB2NC / GFS
     character(len=s_name)    :: meteo_data_model !< meteo model name
     !
     integer(ip)              :: nt                       !< number of stored meteo time steps
     integer(ip)              :: npoin                    !< number of points (my number of cell corners)
     integer(ip)              :: its                      !< start time interpolation index
     integer(ip)              :: ite                      !< end   time interpolation index
     real(rp)                 :: time_lag                 !< difference in met model and dbs time origins
     real(rp)                 :: meteo_coupling_interval  !< meteo coupling time interval (in s)
     !
     real(rp), allocatable    :: time   (:)       !< time   (nt) met model time steps in format YYYYMMDDHHMMSS
     real(rp), allocatable    :: timesec(:)       !< timesec(nt) met model time steps sec after 0000UTC
     !
     integer(ip), allocatable :: el_po(:)         !< el_po(npoin) meteo model point hosting element: el_po(ipoin) = ielem
     real(rp),    allocatable :: s_po (:)         !< s_po (npoin) meteo model point interpolation factor: s_po(ipoin)  = s
     real(rp),    allocatable :: t_po (:)         !< t_po (npoin) meteo model point interpolation factor: t_po(ipoin)  = t
     !
     real(rp), allocatable :: my_lmaskc(:,:)      !< my_lmaskc (my_ibs:my_ibe, my_jbs:my_jbe    )  land mask value  at my processor cell cell corners
     real(rp), allocatable :: my_lusec (:,:)      !< my_lusec  (my_ibs:my_ibe, my_jbs:my_jbe    )  land use index   at my processor cell cell corners
     real(rp), allocatable :: my_z0c   (:,:)      !< my_z0c    (my_ibs:my_ibe, my_jbs:my_jbe    )  roughness length at my processor cell cell corners
     !
     real(rp), allocatable :: my_pblhc (:,:,:)    !< my_pblhc  (my_ibs:my_ibe, my_jbs:my_jbe, nt)  boundary layer height at my processor cell cell corners
     real(rp), allocatable :: my_ustc  (:,:,:)    !< my_ustc   (my_ibs:my_ibe, my_jbs:my_jbe, nt)  friction velocity u*  at my processor cell cell corners
     real(rp), allocatable :: my_smoic (:,:,:)    !< my_smoic  (my_ibs:my_ibe, my_jbs:my_jbe, nt)  soil moisture         at my processor cell cell corners
     real(rp), allocatable :: my_prec  (:,:,:)    !< my_prec   (my_ibs:my_ibe, my_jbs:my_jbe, nt)  precipitation rate    at my processor cell cell corners
     real(rp), allocatable :: my_u10   (:,:,:)    !< my_u10    (my_ibs:my_ibe, my_jbs:my_jbe, nt)  10m u-velocity        at my processor cell cell corners
     real(rp), allocatable :: my_v10   (:,:,:)    !< my_v10    (my_ibs:my_ibe, my_jbs:my_jbe, nt)  10m v-velocity        at my processor cell cell corners
     real(rp), allocatable :: my_t2    (:,:,:)    !< my_t2     (my_ibs:my_ibe, my_jbs:my_jbe, nt)  2m temperature        at my processor cell cell corners
     real(rp), allocatable :: my_monc  (:,:,:)    !< my_monc   (my_ibs:my_ibe, my_jbs:my_jbe, nt)  Monin-Obukhov lenght  at my processor cell cell corners
     !
     real(rp), allocatable :: my_uc   (:,:,:,:)   !< my_uc   (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  u-wind velocity           at my processor cell cell corners
     real(rp), allocatable :: my_vc   (:,:,:,:)   !< my_vc   (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  v-wind velocity           at my processor cell cell corners
     real(rp) ,allocatable :: my_wc   (:,:,:,:)   !< my_wc   (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  w-wind velocity           at my processor cell cell corners
     real(rp), allocatable :: my_rhoc (:,:,:,:)   !< my_rhoc (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  air density               at my processor cell cell corners
     real(rp), allocatable :: my_tc   (:,:,:,:)   !< my_tc   (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  air temperature           at my processor cell cell corners
     real(rp), allocatable :: my_tpc  (:,:,:,:)   !< my_tpc  (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  air potential temperature at my processor cell cell corners
     real(rp), allocatable :: my_tvc  (:,:,:,:)   !< my_tpc  (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  air virtual   temperature at my processor cell cell corners
     real(rp), allocatable :: my_pc   (:,:,:,:)   !< my_pc   (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  air pressure              at my processor cell cell corners
     real(rp), allocatable :: my_qvc  (:,:,:,:)   !< my_qvc  (my_ibs:my_ibe, my_jbs:my_jbe, my_kbs:my_kbe, nt )  specific humidity         at my processor cell cell corners
     !
     real(rp), allocatable :: my_u  (:,:,:,:)     !< my_u    (my_ibs_1h:my_ibe_1h,my_jps:my_jpe,my_kps:my_kpe,2)  x-component of scaled wind velocity at cell boundaries
     real(rp), allocatable :: my_v  (:,:,:,:)     !< my_v    (my_jbs_1h:my_jbe_1h,my_ips:my_ipe,my_kps:my_kpe,2)  y-component of scaled wind velocity at cell boundaries
     real(rp), allocatable :: my_w  (:,:,:,:)     !< my_w    (my_kbs_1h:my_kbe_1h,my_ips:my_ipe,my_jps:my_jpe,2)  z-component of scaled wind velocity at cell boundaries
     !
     real(rp), allocatable :: my_pre (:,:)        !< my_pre  (my_ips   :my_ipe   ,my_jps   :my_jpe   )                      precipitation rate    at mass points
     real(rp), allocatable :: my_pblh(:,:)        !< my_pblh (my_ips   :my_ipe   ,my_jps   :my_jpe   )                      boundary layer height at mass points
     real(rp), allocatable :: my_mon (:,:)        !< my_mon  (my_ips   :my_ipe   ,my_jps   :my_jpe   )                      Monin-Obukhov lenght  at mass points
     real(rp), allocatable :: my_ust (:,:)        !< my_ust  (my_ips   :my_ipe   ,my_jps   :my_jpe   )                      friction velocity u*  at mass points
     !
     real(rp), allocatable :: my_rho (:,:,:)      !< my_rho  (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   )  air density             at mass points
     real(rp), allocatable :: my_t   (:,:,:)      !< my_t    (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   )  air temperature         at mass points
     real(rp), allocatable :: my_tv  (:,:,:)      !< my_tv   (my_ips   :my_ipe   ,my_jps   :my_jpe   ,my_kps   :my_kpe   )  air virtual temperature at mass points
     real(rp), allocatable :: my_k1  (:,:,:)      !< my_k1   (my_ips_2h:my_ipe_2h,my_jps   :my_jpe   ,my_kps   :my_kpe   )  x-diffusion             at mass points
     real(rp), allocatable :: my_k2  (:,:,:)      !< my_k2   (my_jps_2h:my_jpe_2h,my_ips   :my_ipe   ,my_kps   :my_kpe   )  y-diffusion             at mass points
     real(rp), allocatable :: my_k3  (:,:,:)      !< my_k3   (my_kps_2h:my_kpe_2h,my_ips   :my_ipe   ,my_jps   :my_jpe   )  z-diffusion             at mass points
     !
  end type METEOROLOGY
  !
  !>   type METEO_MODEL: definition of all variables related to the driving meteorological model
  !
  type METEO_MODEL
     !
     type(Q1_GRID) :: GRID2D             !< 2D meteo model grid (plane)
     !
     integer(ip) :: nx            !< number of meteo model grid points along x
     integer(ip) :: ny            !< number of meteo model grid points along y
     integer(ip) :: nz            !< number of meteo model grid points along z
     integer(ip) :: nt            !< number of stored meteo time steps
     !
     integer(ip) :: start_year    !< met model start year
     integer(ip) :: start_month   !< met model start month
     integer(ip) :: start_day     !< met model start day
     integer(ip) :: start_hour    !< met model start hour
     integer(ip) :: start_minute  !< met model start minute
     integer(ip) :: start_second  !< met model start second
     real(rp)    :: dt            !< met model time increment (in s)
     !
     real(rp), allocatable :: lon  (:,:)     !< lon  (nx,ny) met model grid longitude
     real(rp), allocatable :: lat  (:,:)     !< lat  (nx,ny) met model grid latitude
     real(rp), allocatable :: pres (:  )     !< pres (nz)    met model pressure levels
     real(rp), allocatable :: topg (:,:)     !< topg (nx,ny) met model topography
     !
     logical :: xreversed                    !< xreversed if longitudes are reversed
     logical :: yreversed                    !< yreversed if latitudes are reversed
     logical :: zreversed                    !< zreversed if vertical levels are reversed
     !
     real(rp), allocatable :: time   (:)     !< time   (nt) met model time steps in format YYYYMMDDHHMMSS
     real(rp), allocatable :: timesec(:)     !< timesec(nt) met model time steps sec after 0000UTC
     !
  end type METEO_MODEL
  !
  !>   type METEO_PROFILE: definition of all variables related to nt vertical profiles of meteo variables
  !
  type METEO_PROFILE
     !
     logical     :: exists = .false.
     !
     integer(ip) :: nz                     !< number of profile points (i.e. along z)
     integer(ip) :: nt                     !< number of stored meteo time steps
     integer(ip) :: el_po                  !< meteo model hosting element
     real(rp)    :: s_po                   !< meteo model point interpolation factor
     real(rp)    :: t_po                   !< meteo model point interpolation factor
     real(rp)    :: lon                    !< profile longitude
     real(rp)    :: lat                    !< profile latitude
     real(rp)    :: zo                     !< terrain elevation at profile point (lon.lat)
     !
     real(rp), allocatable :: time(  :)    !< time(   nt)  met model time steps in format YYYYMMDDHHMMSS
     real(rp), allocatable :: timesec(:)   !< timesec(nt)  met model time steps sec after 0000UTC
     real(rp), allocatable :: zavl(:,:)    !< zavl(nz,nt)  height level (above terrain or vent level)
     real(rp), allocatable :: zasl(:,:)    !< zasl(nz,nt)  height level (above sea level)
     real(rp), allocatable :: p   (:,:)    !< p   (nz,nt)  air pressure (Pa)
     real(rp), allocatable :: t   (:,:)    !< t   (nz,nt)  air temperature (K)
     real(rp), allocatable :: tp  (:,:)    !< tp  (nz,nt)  air potential temperature (K)
     real(rp), allocatable :: tv  (:,:)    !< tp  (nz,nt)  air virtual potential temperature (K)
     real(rp), allocatable :: u   (:,:)    !< u   (nz,nt)  x-wind speed (m/s)
     real(rp), allocatable :: v   (:,:)    !< v   (nz,nt)  y-wind speed (m/s)
     real(rp), allocatable :: qv  (:,:)    !< qv  (nz,nt)  specific humidity (kg/kg)
     real(rp), allocatable :: rho (:,:)    !< rho (nz,nt)  air density  (kg/m3)
     !
     real(rp), allocatable :: Vair(:,:)    !< Vair(nz,nt)  wind speed (m/s)
     real(rp), allocatable :: Aair(:,:)    !< Aair(nz,nt)  wind direction (Rad)
     real(rp), allocatable :: Nair(:,:)    !< Nair(nz,nt)  N2 buoyancy frequency
     !
  end type METEO_PROFILE
  !
  !>   type CUTS definition of postproccess grid cuts (including FLs)
  !
  type CUTS
     !
     integer(ip)              :: ncutx = 0       !< number of postprocces cuts along x planes
     real(rp),    allocatable :: x_cut  (:)      !< x_cut  (ncutx) x-coordiante of x cut planes
     integer(ip), allocatable :: index_x(:)      !< index_x(ncutx) index for cut interpolation along x
     real(rp),    allocatable :: shape_x(:)      !< shape_x(ncutx) shape for cut interpolation along x
     !
     integer(ip)              :: ncuty = 0       !< number of postprocces cuts along y planes
     real(rp),    allocatable :: y_cut  (:)      !< y_cut  (ncuty) y-coordiante of y cut planes
     integer(ip), allocatable :: index_y(:)      !< index_y(ncuty) index for cut interpolation along y
     real(rp),    allocatable :: shape_y(:)      !< shape_y(ncuty) shape for cut interpolation along y
     !
     integer(ip)              :: ncutz = 0       !< number of postprocces cuts along z planes
     real(rp),    allocatable :: z_cut  (:)      !< z_cut  (ncutz) z-coordiante of z cut planes
     integer(ip), allocatable :: index_z(:)      !< index_z(ncutz) index for cut interpolation along z
     real(rp),    allocatable :: shape_z(:)      !< shape_z(ncutz) shape for cut interpolation along z
     !
     integer(ip)              :: nfl = 0         !< number of postprocces cuts along z planes (FLs)
     real(rp),    allocatable :: zfl_cut(:)      !< zfl_cut(nfl)   z-coordiante of FL planes
     real(rp),    allocatable :: fl_cut (:)      !< fl_cut (nfl)   FL values
     integer(ip), allocatable :: index_zfl(:)    !< index_zfl(nfl) index for cut interpolation along z
     real(rp),    allocatable :: shape_zfl(:)    !< shape_zfl(nfl) shape for cut interpolation along z
     !
  end type CUTS
  !
  !>   type TRACKING_POINTS: definition of all variables related totracking points list
  !
  type TRACKING_POINTS
     !
     integer(ip) :: npts = 0                              !< Number of points
     !
     character(len=s_name), allocatable :: name_pts(:)    !< tracked point name
     integer(ip),           allocatable :: mproc(:)       !< Processor hosting the point
     integer(ip),           allocatable :: ipts(:)        !< i-index of tracked points
     integer(ip),           allocatable :: jpts(:)        !< j-index of tracked points
     integer(ip),           allocatable :: kpts(:)        !< k-index of tracked points
     real(rp),              allocatable :: xpts(:)        !< x-ccordinate of tracked points
     real(rp),              allocatable :: ypts(:)        !< y-ccordinate of tracked points
     real(rp),              allocatable :: zpts(:)        !< z-ccordinate of tracked points
     real(rp),              allocatable :: spts(:)        !< interpolation factor along x-ccordinate
     real(rp),              allocatable :: tpts(:)        !< interpolation factor along y-ccordinate
     real(rp),              allocatable :: wpts(:)        !< interpolation factor along z-ccordinate
     !
  end type TRACKING_POINTS
  !
  !>   type MODEL_OUTPUT: definition of all variables related to model output and postprocess
  !
  type MODEL_OUTPUT
     !
     logical  :: parallel_IO               !< Parallel I/O flag
     logical  :: out_dbs_file  = .true.    !< if .true. outputs dbs     file
     logical  :: out_rst       = .false.   !< if .true. outputs restart file
     logical  :: out_con_total = .false.   !< if .true. outputs total concentration on sigma planes (sum over all bins of a given substance)
     logical  :: out_con_bins  = .false.   !< if .true. outputs bin   concentration on sigma planes (         all bins of a given substance)
     logical  :: out_col_load  = .false.   !< if .true. outputs column  mass load                   (sum over all bins of a given substance)
     logical  :: out_cloud_top = .false.   !< if .true. outputs cloud top height
     logical  :: out_grn_total = .false.   !< if .true. outputs total deposit mass load             (sum over all bins of a given substance)
     logical  :: out_grn_bins  = .false.   !< if .true. outputs bin   deposit mass load             (         all bins of a given substance)
     logical  :: out_wet_total = .false.   !< if .true. outputs total wet deposition                (sum over all bins of a given substance)
     logical  :: track_points  = .false.   !< if .true. tracks a list of points
     !
     integer(ip) :: log_level              !< level of log files
     !
     real(rp)    :: out_start              !< output start in seconds after 00UTC of current day
     real(rp)    :: dt                     !< output time step
     real(rp)    :: rst                    !< restart time step
     !
     type(CUTS)            :: MY_CUTS
     type(TRACKING_POINTS) :: MY_PTS
     !
  end type MODEL_OUTPUT
  !
  !>   type RUN_TIME: definition of variables related to run time
  !
  type RUN_TIME
     !
     logical     :: go_on          !< run     flag
     logical     :: restart        !< restart flag
     logical     :: insertion      !< restart flag
     !
     integer(ip) :: start_year     !< run start year
     integer(ip) :: start_month    !< run start month
     integer(ip) :: start_day      !< run start day
     !
     real(rp)    :: run_start      !< run start in seconds after 00UTC of current day
     real(rp)    :: run_end        !< run end   in seconds after 00UTC of current day
     real(rp)    :: dbs_start      !< dbs start in seconds after 00UTC of current day
     real(rp)    :: dbs_end        !< dbs end   in seconds after 00UTC of current day
     !
     ! run time
     !
     integer(ip) :: iiter          !< time iteration number
     real(rp)    :: my_dt          !< my     critical time integration step
     real(rp)    :: gl_dt          !< global critical time integration step
     real(rp)    :: time           !< current time (in seconds after 00:00 UTC of start day)
     real(rp)    :: meteo_time     !< time to update meteo  data
     real(rp)    :: source_time    !< time to update source data
     !
  end type RUN_TIME
  !
END MODULE KindType
