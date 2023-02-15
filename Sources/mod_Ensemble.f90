!***********************************************************************
!>
!> Module for operations related to ensemble generation 
!> @author
!> Leonardo Mingari
!>
!**********************************************************************
MODULE Ensemble
  use KindType
  use InpOut
  use Parallel
  implicit none
  save
  !
  !    LIST OF PUBLIC VARIABLES
  !
  integer(ip), parameter :: NPERT               = 3
  integer(ip), parameter :: ID_COLUMN_HEIGHT    = 1
  integer(ip), parameter :: ID_U_WIND           = 2
  integer(ip), parameter :: ID_V_WIND           = 3
  !
  logical                :: perturbate(NPERT)             = .false.
  real(rp)               :: perturbation_factor(NPERT)    = 1.0_rp
  real(rp)               :: perturbation_random(NPERT)    = 0.0_rp
  real(rp)               :: variance_factor(NPERT)        = 0.1_rp
  !
  logical                :: read_random_from_file         = .false.
  !
  !    LIST OF PUBLIC ROUTINES IN THE MODULE
  !
  PUBLIC :: ensemble_init_random
  PUBLIC :: ensemble_bcast_params
  PUBLIC :: ensemble_read_inp
  PUBLIC :: ensemble_write_random
  PUBLIC :: ensemble_read_random
  !
CONTAINS
  !
  !-----------------------------------------
  !    subroutine ensemble_init_random
  !-----------------------------------------
  !
  !>   @brief
  !>   Set the perturbation vector of random numbers in the range (-1,1)
  !
  subroutine ensemble_init_random(MY_ERR)
    implicit none
    !
    !>   @param MY_ERR    error handler
    !
    type(ERROR_STATUS), intent(INOUT) :: MY_ERR
    !
    integer(ip) :: i
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'ensemble_init_random'
    MY_ERR%message = ' '
    !
    call random_seed()
    call random_number( perturbation_random(1:NPERT) )
    perturbation_random = 2.0*perturbation_random-1.0
    !
    return
  end subroutine ensemble_init_random
  !
  !-----------------------------------------
  !    subroutine ensemble_read_inp
  !-----------------------------------------
  !
  !>   @brief
  !>   Reads ENSEMBLE block from input file
  !
  subroutine ensemble_read_inp(MY_FILES, MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),      intent(IN   ) :: MY_FILES
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
    !*** Starts reading
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','RANDOM_NUMBERS_FROM_FILE',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        read_random_from_file = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_COLUMN_HEIGHT',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        perturbate(ID_COLUMN_HEIGHT) = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_U_WIND',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        perturbate(ID_U_WIND) = .true.
    end if
    !
    call inpout_get_cha (file_inp,'ENSEMBLE','PERTURBATE_V_WIND',word,1,MY_ERR,.true.)
    if(TRIM(word).eq.'YES') then
        perturbate(ID_V_WIND) = .true.
    end if
    !
    call inpout_get_rea (file_inp,'ENSEMBLE','VARIANCE_FACTOR_COLUMN_HEIGHT',rvoid,1,MY_ERR)
    if(MY_ERR%flag.eq.0) then
        variance_factor(ID_COLUMN_HEIGHT) = rvoid
    else
      if(perturbate(ID_COLUMN_HEIGHT)) then
          call task_wriwarn(MY_ERR,'VARIANCE_FACTOR_COLUMN_HEIGHT not found. Assuming default value')
      end if
    end if
    !
    call inpout_get_rea (file_inp,'ENSEMBLE','VARIANCE_FACTOR_U_WIND',rvoid,1,MY_ERR)
    if(MY_ERR%flag.eq.0) then
        variance_factor(ID_U_WIND) = rvoid
    else
      if(perturbate(ID_U_WIND)) then
          call task_wriwarn(MY_ERR,'VARIANCE_FACTOR_U_WIND not found. Assuming default value')
      end if
    end if
    !
    call inpout_get_rea (file_inp,'ENSEMBLE','VARIANCE_FACTOR_V_WIND',rvoid,1,MY_ERR)
    if(MY_ERR%flag.eq.0) then
        variance_factor(ID_V_WIND) = rvoid
    else
      if(perturbate(ID_V_WIND)) then
          call task_wriwarn(MY_ERR,'VARIANCE_FACTOR_V_WIND not found. Assuming default value')
      end if
    end if
    !
    MY_ERR%flag = 0
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
  subroutine ensemble_bcast_params(MY_ERR)
    implicit none
    !
    !>   @param MY_ERR    error handler
    !
    type(ERROR_STATUS),  intent(INOUT) :: MY_ERR
    !
    !*** Initializations
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'ensemble_bcast_params'
    MY_ERR%message = ' '
    !
    call parallel_bcast(read_random_from_file, 1,     0, COMM_WORLD)
    call parallel_bcast(perturbate,            NPERT, 0, COMM_WORLD)
    call parallel_bcast(variance_factor,       NPERT, 0, COMM_WORLD)
    !
    return
  end subroutine ensemble_bcast_params
  !
  !--------------------------------------------
  !    subroutine ensemble_write_random
  !--------------------------------------------
  !
  !>   @brief
  !>   Writes a vector of random numbers required to compute perturbations
  !
  subroutine ensemble_write_random(MY_FILES,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
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
    do i=1,NPERT
        write(myunit,'(f19.15)') perturbation_random(i)
    end do
    !
    close(myunit)
    !
    return
    !
100 MY_ERR%flag = 1
    MY_ERR%message ='Error saving file '//output_file
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
  subroutine ensemble_read_random(MY_FILES,MY_ERR)
    implicit none
    !
    !>   @param MY_FILES  list of files
    !>   @param MY_ERR    error handler
    !
    type(FILE_LIST),       intent(IN   ) :: MY_FILES
    type(ERROR_STATUS),    intent(INOUT) :: MY_ERR
    !
    integer(ip)           :: myunit, i
    character(len=s_file) :: input_file
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
    do i=1,NPERT
        read(myunit,*) perturbation_random(i)
    end do
    !
    close(myunit)
    !
    return
    !
100 MY_ERR%flag = 1
    MY_ERR%message ='Error reading file '//input_file
    !
    return
  end subroutine ensemble_read_random
  !
END MODULE Ensemble
