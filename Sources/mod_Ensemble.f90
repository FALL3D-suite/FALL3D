!***********************************************************************
!>
!> Module for operations related to ensemble generation
!> @author
!> Leonardo Mingari and Arnau Folch
!>
!**********************************************************************
MODULE Ensemble
  use KindType
  use InpOut
  use Parallel
  use Shared, only: nens
  implicit none
  save
  !
  !    LIST OF PUBLIC VARIABLES
  !
  integer(ip), parameter :: nper = 14   !< number of possible perturbated parameters 
  !
  integer(ip), parameter :: ID_COLUMN_HEIGHT       = 1
  integer(ip), parameter :: ID_MASS_FLOW_RATE      = 2
  integer(ip), parameter :: ID_SOURCE_START        = 3
  integer(ip), parameter :: ID_SOURCE_DURATION     = 4
  integer(ip), parameter :: ID_TOP_HAT_THICKNESS   = 5
  integer(ip), parameter :: ID_SUZUKI_A            = 6 
  integer(ip), parameter :: ID_SUZUKI_L            = 7
  integer(ip), parameter :: ID_U_WIND              = 8
  integer(ip), parameter :: ID_V_WIND              = 9
  integer(ip), parameter :: ID_CLOUD_HEIGHT        = 10
  integer(ip), parameter :: ID_CLOUD_THICKNESS     = 11
  integer(ip), parameter :: ID_FI_MEAN             = 12
  integer(ip), parameter :: ID_DIAMETER_AGGREGATES = 13
  integer(ip), parameter :: ID_DENSITY_AGGREGATES  = 14
  !
  integer(ip), parameter :: PERTURBATION_TYPE_NONE     = 0
  integer(ip), parameter :: PERTURBATION_TYPE_RELATIVE = 1
  integer(ip), parameter :: PERTURBATION_TYPE_ABSOLUTE = 2
  !
  integer(ip), parameter :: PERTURBATION_PDF_UNIFORM  = 0
  integer(ip), parameter :: PERTURBATION_PDF_GAUSSIAN = 1
  !
  !*** Parameter for a truncated normal PDF in the range [-1,1]
  !
  real(rp), parameter    :: NORM_MU      = 0.0_rp  ! Mean
  real(rp), parameter    :: NORM_STD_INV = 2.5_rp  ! 1/Standard deviation
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: ensemble_read_inp 
  PUBLIC :: ensemble_bcast_params
  PUBLIC :: ensemble_init_random
  PUBLIC :: ensemble_write_random
  PUBLIC :: ensemble_read_random
  PUBLIC :: ensemble_perturbate_variable
  !
  !    LIST OF PRIVATE ROUTINES IN THE MODULE
  !
  PRIVATE :: ensemble_lhs
  PRIVATE :: ensemble_normal_intervals
  PRIVATE :: ensemble_perm_uniform
  PRIVATE :: ensemble_random_int
  PRIVATE :: ensemble_cumm_tnorm
  PRIVATE :: ensemble_cumm_norm
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine ensemble_read_inp
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads ENSEMBLE block from input file
  !
  subroutine ensemble_read_inp(MY_FILES, MY_ENS, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
    type(ENS_PARAMS),     intent(INOUT) :: MY_ENS
    type(ERROR_STATUS),   intent(INOUT) :: MY_ERR
    !
    real(rp)               :: file_version
    real(rp)               :: rvoid
    character(len=s_file)  :: file_inp, word
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'ensemble_read_inp'
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
       MY_ERR%source  = 'ensemble_read_inp'
       MY_ERR%message = 'Input file version deprecated. Please use 8.x file version '
       return
    end if
    !
    !*** Start reading
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','RANDOM_NUMBERS_FROM_FILE',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%read_random_from_file = .true.
    end if
    !
    !*** ID_COLUMN_HEIGHT
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_COLUMN_HEIGHT',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_COLUMN_HEIGHT) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_COLUMN_HEIGHT) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_COLUMN_HEIGHT) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_COLUMN_HEIGHT) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_COLUMN_HEIGHT).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_COLUMN_HEIGHT','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_COLUMN_HEIGHT) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for COLUMN_HEIGT. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_COLUMN_HEIGHT','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_COLUMN_HEIGHT) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_COLUMN_HEIGHT) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_COLUMN_HEIGHT) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_MASS_FLOW_RATE
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_MASS_FLOW_RATE',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_MASS_FLOW_RATE) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_MASS_FLOW_RATE) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_MASS_FLOW_RATE) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_MASS_FLOW_RATE) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_MASS_FLOW_RATE).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_MASS_FLOW_RATE','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_MASS_FLOW_RATE) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for MASS_FLOW_RATE. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_MASS_FLOW_RATE','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_MASS_FLOW_RATE) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_MASS_FLOW_RATE) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_MASS_FLOW_RATE) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_SOURCE_START
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_SOURCE_START',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_SOURCE_START) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_SOURCE_START) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_SOURCE_START) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_SOURCE_START) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_SOURCE_START).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_SOURCE_START','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_SOURCE_START) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for SOURCE_START. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_SOURCE_START','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_SOURCE_START) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_SOURCE_START) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_SOURCE_START) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_SOURCE_DURATION
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_SOURCE_DURATION',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_SOURCE_DURATION) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_SOURCE_DURATION) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_SOURCE_DURATION) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_SOURCE_DURATION) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_SOURCE_DURATION).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_SOURCE_DURATION','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_SOURCE_DURATION) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for SOURCE_DURATION. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_SOURCE_DURATION','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_SOURCE_DURATION) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_SOURCE_DURATION) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_SOURCE_DURATION) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_TOP_HAT_THICKNESS
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_TOP-HAT_THICKNESS',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_TOP_HAT_THICKNESS) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_TOP_HAT_THICKNESS) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_TOP_HAT_THICKNESS) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_TOP_HAT_THICKNESS) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_TOP_HAT_THICKNESS).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_TOP-HAT_THICKNESS','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_TOP_HAT_THICKNESS) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for TOP-HAT_THICKNESS. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_TOP-HAT_THICKNESS','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_TOP_HAT_THICKNESS) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_TOP_HAT_THICKNESS) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_TOP_HAT_THICKNESS) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_SUZUKI_A
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_SUZUKI_A',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_SUZUKI_A) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_SUZUKI_A) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_SUZUKI_A) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_SUZUKI_A) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_SUZUKI_A).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_SUZUKI_A','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_SUZUKI_A) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for SUZUKI_A. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_SUZUKI_A','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_SUZUKI_A) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_SUZUKI_A) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_SUZUKI_A) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_SUZUKI_L
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_SUZUKI_L',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_SUZUKI_L) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_SUZUKI_L) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_SUZUKI_L) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_SUZUKI_L) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_SUZUKI_L).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_SUZUKI_L','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_SUZUKI_L) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for SUZUKI_L. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_SUZUKI_L','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_SUZUKI_L) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_SUZUKI_L) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_SUZUKI_L) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_WIND
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_WIND',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_U_WIND) = PERTURBATION_TYPE_NONE 
        MY_ENS%perturbation_type(ID_V_WIND) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_U_WIND) = PERTURBATION_TYPE_RELATIVE
        MY_ENS%perturbation_type(ID_V_WIND) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_U_WIND) = PERTURBATION_TYPE_ABSOLUTE
        MY_ENS%perturbation_type(ID_V_WIND) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_U_WIND) = PERTURBATION_TYPE_NONE 
        MY_ENS%perturbation_type(ID_V_WIND) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_U_WIND).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_WIND','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_U_WIND) = rvoid
          MY_ENS%perturbation_range(ID_V_WIND) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for WIND. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_WIND','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_U_WIND) = PERTURBATION_PDF_UNIFORM
          MY_ENS%perturbation_pdf(ID_V_WIND) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_U_WIND) = PERTURBATION_PDF_GAUSSIAN
          MY_ENS%perturbation_pdf(ID_V_WIND) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_U_WIND) = PERTURBATION_PDF_UNIFORM
          MY_ENS%perturbation_pdf(ID_V_WIND) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_CLOUD_HEIGHT
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_DATA_INSERTION_CLOUD_HEIGHT',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_CLOUD_HEIGHT) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_CLOUD_HEIGHT) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_CLOUD_HEIGHT) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_CLOUD_HEIGHT) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_CLOUD_HEIGHT).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_DATA_INSERTION_CLOUD_HEIGHT','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_CLOUD_HEIGHT) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for CLOUD_HEIGHT. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_DATA_INSERTION_CLOUD_HEIGHT','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_CLOUD_HEIGHT) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_CLOUD_HEIGHT) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_CLOUD_HEIGHT) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_CLOUD_THICKNESS
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_DATA_INSERTION_CLOUD_THICKNESS',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_CLOUD_THICKNESS) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_CLOUD_THICKNESS) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_CLOUD_THICKNESS) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_CLOUD_THICKNESS) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_CLOUD_THICKNESS).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_DATA_INSERTION_CLOUD_THICKNESS','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_CLOUD_THICKNESS) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for CLOUD_THICKNESS. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_DATA_INSERTION_CLOUD_THICKNESS','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_CLOUD_THICKNESS) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_CLOUD_THICKNESS) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_CLOUD_THICKNESS) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_FI_MEAN
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_FI_MEAN',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_FI_MEAN) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_FI_MEAN) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_FI_MEAN) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_FI_MEAN) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_FI_MEAN).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_FI_MEAN','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_FI_MEAN) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for FI_MEAN. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_FI_MEAN','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_FI_MEAN) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_FI_MEAN) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_FI_MEAN) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_DIAMETER_AGGREGATES
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_DIAMETER_AGGREGATES_(MIC)',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_DIAMETER_AGGREGATES) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_DIAMETER_AGGREGATES) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_DIAMETER_AGGREGATES) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_DIAMETER_AGGREGATES) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_DIAMETER_AGGREGATES).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_DIAMETER_AGGREGATES_(MIC)','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_DIAMETER_AGGREGATES) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for DIAMETER_AGGREGATES. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_DIAMETER_AGGREGATES_(MIC)','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_DIAMETER_AGGREGATES) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_DIAMETER_AGGREGATES) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_DIAMETER_AGGREGATES) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ID_DENSITY_AGGREGATES
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_DENSITY_AGGREGATES',word,1,MY_ERR,.true.)
    select case(TRIM(word))
    case('NO')
        MY_ENS%perturbation_type(ID_DENSITY_AGGREGATES) = PERTURBATION_TYPE_NONE 
    case('RELATIVE')
        MY_ENS%perturbation_type(ID_DENSITY_AGGREGATES) = PERTURBATION_TYPE_RELATIVE
    case('ABSOLUTE')
        MY_ENS%perturbation_type(ID_DENSITY_AGGREGATES) = PERTURBATION_TYPE_ABSOLUTE
    case default
        MY_ENS%perturbation_type(ID_DENSITY_AGGREGATES) = PERTURBATION_TYPE_NONE 
    end select
    !
    if( MY_ENS%perturbation_type(ID_DENSITY_AGGREGATES).ne.PERTURBATION_TYPE_NONE ) then
       !
       call inpout_get_rea (file_inp,'IF_PERTURBATE_DENSITY_AGGREGATES','PERTURBATION_RANGE',rvoid,1,MY_ERR)
       if(MY_ERR%flag.eq.0) then
          MY_ENS%perturbation_range(ID_DENSITY_AGGREGATES) = rvoid
       else
          call task_wriwarn(MY_ERR,'PERTURBATION_RANGE not found for DENSITY_AGGREGATES. Assuming default value')
       end if
       !
       call inpout_get_cha (file_inp,'IF_PERTURBATE_DENSITY_AGGREGATES','PDF',word,1,MY_ERR,.true.)
       select case(TRIM(word))
       case('UNIFORM')
          MY_ENS%perturbation_pdf(ID_DENSITY_AGGREGATES) = PERTURBATION_PDF_UNIFORM
       case('GAUSSIAN')
          MY_ENS%perturbation_pdf(ID_DENSITY_AGGREGATES) = PERTURBATION_PDF_GAUSSIAN
       case default
          MY_ENS%perturbation_pdf(ID_DENSITY_AGGREGATES) = PERTURBATION_PDF_UNIFORM
       end select
       !
    end if
    !
    !*** ENSEMBLE_POSTPROCESS
    !
    call inpout_get_cha (file_inp,'ENSEMBLE_POSTPROCESS','POSTPROCESS_MEMBERS',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%postprocess_members = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE_POSTPROCESS','POSTPROCESS_MEAN',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%postprocess_mean = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE_POSTPROCESS','POSTPROCESS_LOGMEAN',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%postprocess_logmean = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE_POSTPROCESS','POSTPROCESS_MEDIAN',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%postprocess_median = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE_POSTPROCESS','POSTPROCESS_STANDARD_DEV',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%postprocess_sandard_dev = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE_POSTPROCESS','POSTPROCESS_PROBABILITY',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%postprocess_probability = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE_POSTPROCESS','POSTPROCESS_PERCENTILES',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        MY_ENS%postprocess_percentiles = .true.
    end if
    !
    call inpout_get_npar(file_inp,'ENSEMBLE_POSTPROCESS','CONCENTRATION_THRESHOLDS_(MG/M3)',MY_ENS%nth_con, MY_ERR)
    if(MY_ERR%flag.eq.0) then
       allocate(MY_ENS%th_con(MY_ENS%nth_con))
       call inpout_get_rea (file_inp,'ENSEMBLE_POSTPROCESS','CONCENTRATION_THRESHOLDS_(MG/M3)',&
                            MY_ENS%th_con,MY_ENS%nth_con, MY_ERR)
       MY_ENS%th_con(:) = MY_ENS%th_con(:) * 1e-3_rp  !  mg/m3 --> g/m3
    else
       MY_ENS%nth_con = 0
    end if
    !
    call inpout_get_npar(file_inp,'ENSEMBLE_POSTPROCESS','COLUMN_MASS_THRESHOLDS_(G/M2)',MY_ENS%nth_col_mass, MY_ERR)
    if(MY_ERR%flag.eq.0) then
       allocate(MY_ENS%th_col_mass(MY_ENS%nth_col_mass))
       call inpout_get_rea (file_inp,'ENSEMBLE_POSTPROCESS','COLUMN_MASS_THRESHOLDS_(G/M2)', &
                            MY_ENS%th_col_mass,MY_ENS%nth_col_mass, MY_ERR)
    else
       MY_ENS%nth_col_mass = 0
    end if
    !
    call inpout_get_npar(file_inp,'ENSEMBLE_POSTPROCESS','COLUMN_MASS_THRESHOLDS_(DU)',MY_ENS%nth_col_mass_DU, MY_ERR)
    if(MY_ERR%flag.eq.0) then
       allocate(MY_ENS%th_col_mass_DU(MY_ENS%nth_col_mass_DU))
       call inpout_get_rea (file_inp,'ENSEMBLE_POSTPROCESS','COLUMN_MASS_THRESHOLDS_(DU)', &
                            MY_ENS%th_col_mass_DU,MY_ENS%nth_col_mass_DU, MY_ERR)
    else
       MY_ENS%nth_col_mass_DU = 0
    end if
    !
    call inpout_get_npar(file_inp,'ENSEMBLE_POSTPROCESS','GROUND_LOAD_THRESHOLDS_(KG/M2)',MY_ENS%nth_grn_load, MY_ERR)
    if(MY_ERR%flag.eq.0) then
       allocate(MY_ENS%th_grn_load(MY_ENS%nth_grn_load))
       call inpout_get_rea (file_inp,'ENSEMBLE_POSTPROCESS','GROUND_LOAD_THRESHOLDS_(KG/M2)', &
                            MY_ENS%th_grn_load,MY_ENS%nth_grn_load, MY_ERR)
    else
       MY_ENS%nth_grn_load = 0
    end if
    !
    call inpout_get_npar(file_inp,'ENSEMBLE_POSTPROCESS','PERCENTILE_VALUES_(%)',MY_ENS%nval_per, MY_ERR)
    if(MY_ERR%flag.eq.0) then
       allocate(MY_ENS%val_per(MY_ENS%nval_per))
       call inpout_get_rea (file_inp,'ENSEMBLE_POSTPROCESS','PERCENTILE_VALUES_(%)', &
                            MY_ENS%val_per,MY_ENS%nval_per, MY_ERR)
    else
       MY_ENS%nval_per = 0
    end if
    !
    return
  end subroutine ensemble_read_inp
  !
  !-----------------------------------------
  !    subroutine ensemble_bcast_params
  !-----------------------------------------
  !
  !>   @brief
  !>   Broadcasts ENSEMBLE block parameters
  !
  subroutine ensemble_bcast_params(MY_ENS,MY_ERR)
    implicit none
    !
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(ENS_PARAMS),    intent(INOUT) :: MY_ENS
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'ensemble_bcast_params'
    MY_ERR%message = ' '
    !
    call parallel_bcast(MY_ENS%read_random_from_file,   1,0)
    call parallel_bcast(MY_ENS%postprocess_members,     1,0)
    call parallel_bcast(MY_ENS%postprocess_mean,        1,0)
    call parallel_bcast(MY_ENS%postprocess_logmean,     1,0)
    call parallel_bcast(MY_ENS%postprocess_median,      1,0)
    call parallel_bcast(MY_ENS%postprocess_sandard_dev, 1,0)
    call parallel_bcast(MY_ENS%postprocess_probability, 1,0)
    call parallel_bcast(MY_ENS%postprocess_percentiles, 1,0)
    !
    call parallel_bcast(MY_ENS%nth_con,        1,0)
    call parallel_bcast(MY_ENS%nth_col_mass,   1,0)
    call parallel_bcast(MY_ENS%nth_col_mass_DU,1,0)
    call parallel_bcast(MY_ENS%nth_grn_load,   1,0)
    call parallel_bcast(MY_ENS%nval_per,       1,0)
    !
    call parallel_bcast(MY_ENS%perturbation_type,    nper, 0)
    call parallel_bcast(MY_ENS%perturbation_pdf,     nper, 0)
    call parallel_bcast(MY_ENS%perturbation_range,   nper, 0)
    !
    !*** Memory allocation
    !
    if(.not.master_model) then
       if(MY_ENS%nth_con        .gt.0) allocate(MY_ENS%th_con        (MY_ENS%nth_con        ))
       if(MY_ENS%nth_col_mass   .gt.0) allocate(MY_ENS%th_col_mass   (MY_ENS%nth_col_mass   ))
       if(MY_ENS%nth_col_mass_DU.gt.0) allocate(MY_ENS%th_col_mass_DU(MY_ENS%nth_col_mass_DU))
       if(MY_ENS%nth_grn_load   .gt.0) allocate(MY_ENS%th_grn_load   (MY_ENS%nth_grn_load   ))
       if(MY_ENS%nval_per       .gt.0) allocate(MY_ENS%val_per       (MY_ENS%nval_per       ))
    end if
    !
    if(MY_ENS%nth_con        .gt.0) call parallel_bcast(MY_ENS%th_con,        MY_ENS%nth_con,        0)
    if(MY_ENS%nth_col_mass   .gt.0) call parallel_bcast(MY_ENS%th_col_mass,   MY_ENS%nth_col_mass,   0)
    if(MY_ENS%nth_col_mass_DU.gt.0) call parallel_bcast(MY_ENS%th_col_mass_DU,MY_ENS%nth_col_mass_DU,0)
    if(MY_ENS%nth_grn_load   .gt.0) call parallel_bcast(MY_ENS%th_grn_load,   MY_ENS%nth_grn_load,   0)
    if(MY_ENS%nval_per       .gt.0) call parallel_bcast(MY_ENS%val_per,       MY_ENS%nval_per,       0)
    !   
    return
  end subroutine ensemble_bcast_params
  !
  !-----------------------------------------
  !    subroutine ensemble_init_random
  !-----------------------------------------
  !
  !>   @brief
  !>   Set the perturbation vector of random numbers in the range (-1,1)
  !
  subroutine ensemble_init_random(MY_ENS,MY_ERR)
    implicit none
    !
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(ENS_PARAMS),   intent(INOUT) :: MY_ENS
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: iper
    integer(ip) :: npoin,nsam
    real(rp), allocatable :: rand_arr (:,:)
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'ensemble_init_random'
    MY_ERR%message = ' '
    !
    nsam = nens - 1
    allocate(rand_arr(nsam,nper))
    !
    if(mype_couple.eq.0) then
        call random_seed()
        call random_number(rand_arr)
        !
        !*** Perform a Latin hypercube sampling and
        !*** return a random number array in [-1,1]
        !
        call ensemble_lhs(nsam,nper,MY_ENS%perturbation_pdf,rand_arr)
    end if
    npoin = nsam*nper
    call parallel_bcast(rand_arr, npoin, 0, COMM_COUPLE)
    !
    !*** Loop over perturbated parameters 
    !*** (except for master_world; i.e. for 1st ensemble member)
    !
    do iper = 1,nper
       if(task_id.eq.1) then
          MY_ENS%perturbation_random(iper) = 0.0_rp
!       elseif(MY_ENS%perturbation_type(iper).eq.PERTURBATION_TYPE_NONE) then
!          MY_ENS%perturbation_random(iper) = 0.0_rp
       else
          MY_ENS%perturbation_random(iper) = rand_arr(task_id-1,iper)   ! Fill all numbers
       end if
    end do
    !
    return
  end subroutine ensemble_init_random
  !
  !--------------------------------------------
  !    subroutine ensemble_write_random
  !--------------------------------------------
  !
  !>   @brief
  !>   Writes a vector of random numbers required to compute perturbations
  !
  subroutine ensemble_write_random(MY_FILES,MY_ENS,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(ENS_PARAMS),      intent(IN   ) :: MY_ENS
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: myunit, i
    character(len=s_file) :: output_file
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'ensemble_write_random'
    MY_ERR%message = ' '
    !
    output_file = TRIM(MY_FILES%file_ens)
    !
    !*** writes the file
    !
    open(newunit=myunit,file=output_file,status='unknown',err=100)
    !
    do i=1,nper
        write(myunit,'(a15,1x,f8.5)') TRIM(MY_ENS%perturbation_name(i)),MY_ENS%perturbation_random(i)
    end do
    !
    close(myunit)
    !
    return
    !
100 MY_ERR%flag = 1
    MY_ERR%message ='Error saving file '//trim(output_file)
    !
    return
  end subroutine ensemble_write_random
  !
  !--------------------------------------------
  !    subroutine ensemble_read_random
  !--------------------------------------------
  !
  !>   @brief
  !>   Read a vector of random numbers required to compute perturbations
  !
  subroutine ensemble_read_random(MY_FILES,MY_ENS,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_ENS    list of ensemble parameters
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(ENS_PARAMS),      intent(INOUT) :: MY_ENS
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: myunit, i
    character(len=s_file) :: input_file,cvoid
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'ensemble_read_random'
    MY_ERR%message = ' '
    !
    input_file = TRIM(MY_FILES%file_ens)
    !
    !*** writes the file
    !
    open(newunit=myunit,file=input_file,status='old',err=100)
    !
    do i=1,nper
        read(myunit,*) cvoid, MY_ENS%perturbation_random(i)
    end do
    !
    close(myunit)
    !
    return
    !
100 MY_ERR%flag = 1
    MY_ERR%message ='Error reading file '//trim(input_file)
    !
    return
  end subroutine ensemble_read_random
  !
  !-----------------------------------------
  !    function ensemble_perturbate_variable
  !-----------------------------------------
  !
  !>   @brief
  !>   Computes perturbation of a variable 
  !
  elemental real(rp) function ensemble_perturbate_variable(var_ID,var,MY_ENS)
    implicit none
    !
    !>   @param var_ID  variable ID
    !>   @param var     variable to perturbate
    !>   @param MY_ENS  list of ensemble parameters
    !
    integer(ip),      intent(IN   ) :: var_ID
    real(rp),         intent(IN   ) :: var
    type(ENS_PARAMS), intent(IN   ) :: MY_ENS
    !
    real(rp) :: factor
    !
    select case(MY_ENS%perturbation_type(var_ID))
    case(PERTURBATION_TYPE_NONE)
       ensemble_perturbate_variable = var
    case(PERTURBATION_TYPE_RELATIVE)
       factor = MY_ENS%perturbation_random(var_ID)* &
                MY_ENS%perturbation_range (var_ID)/100.0_rp
       ensemble_perturbate_variable = var*(1.0_rp + factor)
    case(PERTURBATION_TYPE_ABSOLUTE)
        factor = MY_ENS%perturbation_random(var_ID)* &
                 MY_ENS%perturbation_range (var_ID)
       ensemble_perturbate_variable = var + factor
    end select
    !
    !*** Variable-dependent corrections
    !
    select case(var_ID)
    case(ID_COLUMN_HEIGHT)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 0.0_rp
    case(ID_MASS_FLOW_RATE)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 0.0_rp
    case(ID_SOURCE_DURATION)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 0.0_rp
    case(ID_TOP_HAT_THICKNESS)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 0.0_rp
    case(ID_SUZUKI_A)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 0.0_rp
    case(ID_SUZUKI_L)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 0.0_rp
    case(ID_DIAMETER_AGGREGATES)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 1.0_rp
    case(ID_DENSITY_AGGREGATES)
       if(ensemble_perturbate_variable.lt.0.0_rp) ensemble_perturbate_variable = 1.0_rp
    end select 
    !
  end function
  !
  !
  !    PRIVATE ROUTINES
  !
  !
  !--------------------------------------------
  !    subroutine ensemble_lhs
  !--------------------------------------------
  !
  !>   @brief
  !>   Implementation of the hypercube sampling algorithm
  !>
  !>   N Points are selected in a NDIM dimensional Latin hypercube.
  !>   Each of the NDIM coordinate dimensions is discretized to 
  !>   the values 1 through N. The points are to be chosen in such 
  !>   a way that no two points have any coordinate value in common.
  !
  subroutine ensemble_lhs(N,NDIM,PDF,X)
      implicit none
      !
      integer(ip), intent(IN   ) :: N
      integer(ip), intent(IN   ) :: NDIM
      integer(ip), intent(IN   ) :: PDF(NDIM)
      real(rp),    intent(INOUT) :: X(N,NDIM)
      !
      integer(ip) :: i,j,ii
      integer(ip) :: perm(N)
      real(rp)    :: BIN         (N+1)
      real(rp)    :: BIN_UNIFORM (N+1)
      real(rp)    :: BIN_GAUSSIAN(N+1)
      !
      !*** Define bin boundaries for a uniform PDF
      !*** in range [-1,1]
      !
      do i=1,N+1
        BIN_UNIFORM(i) = real(i-1, kind=rp) &
                       / real(N,   kind=rp)
      end do
      BIN_UNIFORM = 2.0_rp*BIN_UNIFORM - 1.0_rp
      !
      !*** Define bin boundaries for a truncated
      !*** normal distribution in range [-1,1]
      !
      call ensemble_normal_intervals(N,BIN_GAUSSIAN)
      !
      do j = 1,NDIM
        !
        !*** Build a random permutation of N objects
        !
        call ensemble_perm_uniform ( N, perm )
        !
        !*** Select PDF
        !
        select case(PDF(j))
        case(PERTURBATION_PDF_UNIFORM)
            BIN(:) = BIN_UNIFORM(:)
        case(PERTURBATION_PDF_GAUSSIAN)
            BIN(:) = BIN_GAUSSIAN(:)
        end select
        !
        !*** Force the corresponding i-th components of 
        !*** X to lie in the PERM(I)-th interval 
        !
        do i = 1,N
          ! Input  X in [0,1]
          ! Output X in [-1,1]
          ii     = perm(i)
          X(i,j) = BIN(ii) + X(i,j)*(BIN(ii+1)-BIN(ii))
        end do
      end do
      !
      return
  end subroutine ensemble_lhs
  !
  !--------------------------------------------
  !    subroutine ensemble_normal_intervals
  !--------------------------------------------
  !
  !>   @brief
  !>   Define bin boundaries for a truncated normal PDF
  !>   in range [-1,1]
  !
  subroutine ensemble_normal_intervals(N,X)
      implicit none
      !
      integer(ip), intent(IN   ) :: N
      real(rp),    intent(  OUT) :: X(N+1)
      !
      integer(ip) :: i
      real(rp)    :: error_x
      real(rp)    :: proba,r
      real(rp)    :: l_x,l_p !left   x, left   proba
      real(rp)    :: r_x,r_p !right  x, right  proba
      real(rp)    :: c_x,c_p !center x, center proba
      !
      error_x = 1.0_rp/(N*100.0_rp)
      !
      !*** Range limits [-1,1]
      !
      X(1)   = -1.0_rp
      X(N+1) =  1.0_rp
      !
      !*** Perform a binary search to found inner intervals
      !
      do i=2,N
        l_x = X(i-1) 
        r_x = X(N+1)
        l_p = ensemble_cumm_tnorm(l_x)
        r_p = 1.0_rp
        !
        !*** Cummulative probability for the i-th interval
        !
        proba = real(i-1,kind=rp)/real(N,kind=rp)
        !
        search_boundary: do
          !
          !*** Compute probability Pr[X<L] at center (cx)
          !*** for a truncated normal distribution
          !*** X~Norm[mu,std] in range [-1,1]
          !
          c_x = (l_x+r_x)*0.5_rp
          c_p = ensemble_cumm_tnorm(c_x)
          !
          if(c_p.ge.proba) then
              r_x = c_x
              r_p = c_p
          else
              l_x = c_x
              l_p = c_p
          end if
          !
          if(r_x-l_x.lt.error_x) exit search_boundary
          !
        end do search_boundary
        !
        r    = (proba-l_p)/(r_p-l_p)
        X(i) = l_x*(1.0_rp-r) + r*r_x
      end do
      !
      return
  end subroutine ensemble_normal_intervals
  !
  !--------------------------------------------
  !    subroutine ensemble_perm_uniform
  !--------------------------------------------
  !
  !>   @brief
  !>   Selects a random permutation of (1,2,...,N)
  !
  subroutine ensemble_perm_uniform ( N, perm )
    implicit none
    !
    integer(ip), intent(IN   ) :: N
    integer(ip), intent(  OUT) :: perm(N)
    !
    integer(ip) :: i,j,tmp
    !
    do i=1,N
      perm(i) = i
    end do
    !
    do i = 1,N-1
        j       = ensemble_random_int (i,N)
        tmp     = perm(i)
        perm(i) = perm(j)
        perm(j) = tmp
    end do
    !
    return  
  end subroutine ensemble_perm_uniform 
  !
  !--------------------------------------------
  !    function ensemble_random_int
  !--------------------------------------------
  !
  !>   @brief
  !>   Returns a scaled pseudorandom integer between a and b
  !
  function ensemble_random_int(a,b) result(x)
      implicit none
      !
      integer(ip), intent(IN) :: a
      integer(ip), intent(IN) :: b
      integer(ip)             :: x
      !
      integer(ip)             :: limits(2)
      real(rp)                :: r
      !
      if(a.le.b) then
          limits(1) = a
          limits(2) = b
      else
          limits(1) = b
          limits(2) = a
      end if
      !
      call random_number(r)
      !
      !*** Scale r to lie between min-0.5 and max+0.5
      !
      r = ( real(limits(1), kind=rp) - 0.5_rp ) * (1.0_rp-r) & 
        + ( real(limits(2), kind=rp) + 0.5_rp ) * r
      !
      !*** Use rounding to convert R to an 
      !*** integer between A and B
      !
      x = nint(r, kind=ip)
      x = max(x,limits(1))
      x = min(x,limits(2))
      !
      return
  end function ensemble_random_int
  !
  !--------------------------------------------
  !    function ensemble_cumm_tnorm
  !--------------------------------------------
  !
  !>   @brief
  !>   Returns cummulative probability Pr[X<L] 
  !>   for a truncated normal PDF in range [-1,1]
  !
  function ensemble_cumm_tnorm(x) result(y)
      implicit none
      !
      real(rp), intent(IN) :: x
      real(rp)             :: y
      !
      ! Initialized variables are automatically saved
      real(rp) :: coeff = -1.0_rp
      !
      real(rp), save :: cumm_a
      real(rp), save :: cumm_b
      !
      if(coeff.lt.0) then
          coeff  = NORM_STD_INV/sqrt(2.0_rp)
          cumm_a = ensemble_cumm_norm(-1.0_rp)
          cumm_b = ensemble_cumm_norm( 1.0_rp)
      end if
      !
      y = 0.5_rp * ( 1.0_rp + erf((x-NORM_MU)*coeff) )
      y = (y-cumm_a)/(cumm_b-cumm_a)
      !
      return
  end function ensemble_cumm_tnorm
  !
  !--------------------------------------------
  !    function ensemble_cumm_norm
  !--------------------------------------------
  !
  !>   @brief
  !>   Returns cummulative probability Pr[x<L]
  !>   for a normal PDF in range [-1,1]
  !
  function ensemble_cumm_norm(x) result(y)
      implicit none
      !
      real(rp), intent(IN) :: x
      real(rp)             :: y
      !
      ! Initialized variables are automatically saved
      real(rp) :: coeff = -1.0_rp 
      !
      if(coeff.lt.0) then
          coeff  = NORM_STD_INV/sqrt(2.0_rp)
      end if
      !
      y = 0.5_rp * ( 1.0_rp + erf((x-NORM_MU)*coeff) )
      !
      return
  end function ensemble_cumm_norm
  !
END MODULE Ensemble
