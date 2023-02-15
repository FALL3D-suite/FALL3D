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
  use KindType, only: TASK_SET_ENS
  use Shared,   only: MY_FILES, MY_ERR
  use Parallel 
  use InpOut
  use Ensemble
  implicit none
  !
  !*** Initializations
  !
  MY_ERR%nwarn = 0
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  !*** Redefine the problem path for each ensemble run
  !    (required for all tasks)
  !
  WRITE(MY_FILES%problempath, '(A,I4.4)') TRIM(MY_FILES%problempath)//'/',task_id
  !
  call inpout_get_filenames(TASK_SET_ENS,MY_FILES,MY_ERR)
  !
  !*** Master opens log file
  !
  if(master_world) call inpout_open_log_file(TASK_SET_ENS, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_ENS, MY_FILES, MY_ERR)
  !
  !********
  !LAM: chequear portabilidad
  ! Check if directory exists
  if(master_model) then
      call EXECUTE_COMMAND_LINE('mkdir -p '//MY_FILES%problempath, &
          EXITSTAT=MY_ERR%flag)
  end if
  call parallel_bcast(MY_ERR%flag,1,0)
  !
  if(master_world) then
      call ensemble_read_inp(MY_FILES,MY_ERR)
  end if
  call parallel_bcast(MY_ERR%flag,1,0,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_ENS, MY_FILES, MY_ERR)
  !
  !*** Broadcast input parameters
  !
  call ensemble_bcast_params(MY_ERR)
  !
  !*** Writes log file
  !
  if(master_world) then
      write(MY_FILES%lulog,20) read_random_from_file,  &
                               perturbate
      write(MY_FILES%lulog,30) variance_factor
  end if
  !      
20 format(                                          /, &
          '  Input data: ',                         /, &
          '  RANDOM_NUMBERS_FROM_FILE: ',L2,1x,     /, &
          '  PERTURBATE:               ',*(L2,1x)    )
30 format('  VARIANCE_FACTOR:          ',*(f7.4,1x),/, &
          '  ---------------------------------------')
  !
  !*** Fill perturbation_random with random numbers
  !
  if(master_model) then
      if(read_random_from_file) then
          call ensemble_read_random(MY_FILES,MY_ERR)
          if(MY_ERR%flag.ne.0) read_random_from_file = .False.
          !Communicate warning to master_world and write it in log file
          if(MY_ERR%flag.ne.0) write(*,*) TRIM(MY_ERR%message)
      end if
      if(.not.read_random_from_file) then
          call ensemble_init_random(MY_ERR)
          call ensemble_write_random(MY_FILES,MY_ERR)
      end if
  end if
  call parallel_bcast(perturbation_random,NPERT,0)
  !
  !*** Compute the perturbation factor
  !
  perturbation_factor = 1.0_rp + &
                        perturbation_random * &
                        variance_factor
  !
  !*** Normal end
  !
  if(master_world) call inpout_close_log_file(TASK_SET_ENS, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip,COMM_WORLD)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_ENS, MY_FILES, MY_ERR)
  !
  return
  end subroutine task_SetEns
