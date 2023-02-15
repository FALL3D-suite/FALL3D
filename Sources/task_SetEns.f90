  subroutine task_SetEns
  !
  !------------------------------------
  !    subroutine task_SetEns
  !------------------------------------
  !
  !>   @brief
  !>   Task for generation of ensemble runs in FALL3D
  !>   @author
  !>   Leonardo Mingari
  !
  use KindType
  use Shared,   only: MY_FILES, MY_ERR, MY_ENS, nens
  use Parallel 
  use InpOut
  use Ensemble
  implicit none
  !
  integer(ip), allocatable :: ierror(:)
  real(rp),    allocatable :: random(:,:)
  !
  integer(ip)      :: iper,iens
  !
  !*** Initializations
  !
  MY_ERR%flag    = 0
  MY_ERR%source  = 'task_SetEns'
  MY_ERR%message = ' '
  MY_ERR%nwarn   = 0
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  !*** First, redefine the problem path for each ensemble run
  !
  write(MY_FILES%problempath, '(a,i4.4)') TRIM(MY_FILES%problempath)//'/',task_id
  !
  call inpout_get_filenames(TASK_SET_ENS,MY_FILES,MY_ERR)
  !
  !*** Master opens log file
  !
  if(master_world) call inpout_open_log_file(TASK_SET_ENS, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_ENS, MY_FILES, MY_ERR)
  !
  !*** If necessary create folders for all ensemble members
  ! 
  if(master_model) call EXECUTE_COMMAND_LINE('mkdir -p '//MY_FILES%problempath, EXITSTAT=MY_ERR%flag)
  !
  allocate(ierror(0:npes_world))
  ierror(:)          = 0_ip
  ierror(mype_world) = MY_ERR%flag
  call parallel_sum(ierror, COMM_WORLD)
  if(maxval(ierror).ne.0) call task_runend(TASK_SET_ENS, MY_FILES, MY_ERR)
  deallocate(ierror)
  !
  !*** Allocates memory and initializes
  !
  allocate(MY_ENS%perturbation_type(nper))
  MY_ENS%perturbation_type(:) = PERTURBATION_TYPE_NONE
  !
  allocate(MY_ENS%perturbation_pdf(nper))
  MY_ENS%perturbation_pdf(:)  = PERTURBATION_PDF_UNIFORM
  !
  allocate(MY_ENS%perturbation_range(nper))
  MY_ENS%perturbation_range(:)  = 0.0_rp
  !
  allocate(MY_ENS%perturbation_random(nper))
  MY_ENS%perturbation_random(:) = 0.0_rp
  !
  allocate(MY_ENS%perturbation_name(nper))
  MY_ENS%perturbation_name(ID_COLUMN_HEIGHT      ) = 'COLUMN_HEIGHT'
  MY_ENS%perturbation_name(ID_MASS_FLOW_RATE     ) = 'M_FLOW_RATE  '
  MY_ENS%perturbation_name(ID_SOURCE_START       ) = 'SOURCE_START '
  MY_ENS%perturbation_name(ID_SOURCE_DURATION    ) = 'SOURCE_DURAT '
  MY_ENS%perturbation_name(ID_TOP_HAT_THICKNESS  ) = 'TOP_HAT_THICK'
  MY_ENS%perturbation_name(ID_SUZUKI_A           ) = 'SUZUKI_A     '
  MY_ENS%perturbation_name(ID_SUZUKI_L           ) = 'SUZUKI_L     '
  MY_ENS%perturbation_name(ID_U_WIND             ) = 'WIND_U       '
  MY_ENS%perturbation_name(ID_V_WIND             ) = 'WIND_V       '
  MY_ENS%perturbation_name(ID_CLOUD_HEIGHT       ) = 'CLOUD_HEIGHT '
  MY_ENS%perturbation_name(ID_CLOUD_THICKNESS    ) = 'CLOUD_THICK  '
  MY_ENS%perturbation_name(ID_FI_MEAN            ) = 'FI_MEAN      '
  MY_ENS%perturbation_name(ID_DIAMETER_AGGREGATES) = 'DIAMETER_AGGR'
  MY_ENS%perturbation_name(ID_DENSITY_AGGREGATES ) = 'DENSITY_AGGR '
  !
  !*** Master reads and broadcasts ENSEMBLE block from input file
  !
  if(master_model) call ensemble_read_inp(MY_FILES,MY_ENS,MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0)
  if(MY_ERR%flag.eq.0) then
     call ensemble_bcast_params(MY_ENS,MY_ERR)
  else
     call task_runend(TASK_SET_ENS, MY_FILES, MY_ERR)
  end if
  !
  !*** Fill perturbation_random with random numbers in the range (-1,1)
  !
  if(master_model) then
      if(MY_ENS%read_random_from_file) then
          call ensemble_read_random(MY_FILES,MY_ENS,MY_ERR)
          if(MY_ERR%flag.ne.0) MY_ENS%read_random_from_file = .false.  ! force subsequent file creation
      end if
      if(.not.MY_ENS%read_random_from_file) then
          call ensemble_init_random (MY_ENS,  MY_ERR)
          call ensemble_write_random(MY_FILES,MY_ENS,MY_ERR)
      end if
  end if
  call parallel_bcast(MY_ENS%perturbation_random,nper,0)
  !
  !*** Retrieve all members information and write the log file
  !
  allocate(random(nens,nper))
  random(:,:) = 0.0_rp
  do iper = 1,nper
     if(master_model) random(task_id,iper) = MY_ENS%perturbation_random(iper)
  end do
  call parallel_sum(random, COMM_WORLD)
  !
  if(master_world) then
      write(MY_FILES%lulog,20)  MY_ENS%read_random_from_file, &
                               (MY_ENS%perturbation_name(iper),iper=1,nper)
      do iens = 1,nens
         write(MY_FILES%lulog,30) iens,(random(iens,iper),iper=1,nper)
      end do
  end if
  !      
20 format(/, &
          '  RANDOM_NUMBERS_FROM_FILE:   ',L2,/,/, &
          '  ENSEMBLE_MEMBER  ',*(a16),/, &
          '  -----------------')
30 format('      ',i4.4,'     ',*(4x,f8.5,4x))
  !
  !*** Normal end
  !
  if(master_world) call inpout_close_log_file(TASK_SET_ENS, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1,0,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_ENS, MY_FILES, MY_ERR)
  !
  return
  end subroutine task_SetEns
