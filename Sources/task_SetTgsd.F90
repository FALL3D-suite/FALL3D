  subroutine task_SetTgsd
  !
  !------------------------------------
  !    subroutine task_SetTgsd
  !------------------------------------
  !
  !>   @brief
  !>   Task for (serial) generation of a TGSD file
  !>   @author
  !>   Arnau Folch
  !
  use KindType
  use Shared
  use Parallel
  use InpOut
  use Tgsd
  use Grn
  use Ensemble
  implicit none
  !
  integer(ip)       :: ispe
  type(TGSD_PARAMS) :: MY_TGSD
  !
  !*** Initializations
  !
  MY_ERR%nwarn = 0
  call CPU_TIME(MY_ERR%cpu_start_time)
  !
  call inpout_get_filenames(TASK_SET_TGSD,MY_FILES,MY_ERR)
  !
  !*** Master opens log file
  !
  if(master_model) call inpout_open_log_file(TASK_SET_TGSD, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_TGSD, MY_FILES, MY_ERR)
  !
  !*** Master reads and broadcasts SPECIES block from input file
  !
  if(master_model) call grn_read_inp_species(MY_FILES, MY_SPE, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.eq.0) then
     call grn_bcast_species(MY_SPE,MY_ERR)
  else
     call task_runend(TASK_SET_TGSD, MY_FILES, MY_ERR)
  end if
  !
  !*** Loop over species
  !
  do ispe = 1,MY_SPE%nspe
     !
     if(MY_SPE%category(ispe).eq.CAT_AEROSOL) cycle  ! nothing to be done for CAT_AEROSOL
     !
     !*** Master reads and broadcasts specie_TGSD block from input file (stored in MY_TGSD)
     !
     if(master_model) call tgsd_read_inp_granulometry(MY_FILES, MY_TGSD, MY_SPE%code(ispe),MY_ENS,MY_ERR)
     call parallel_bcast(MY_ERR%flag,1,0)
     if(MY_ERR%flag.eq.0) then
        call tgsd_bcast_inp_granulometry(MY_TGSD,MY_ERR)
     else
        call task_runend(TASK_SET_TGSD, MY_FILES, MY_ERR)
     end if
     !
     select case(MY_TGSD%type_dist)
     case('CUSTOM')
        !
        !*** Nothing needs to be done; TGSD already furnished by the user
        !
     case default
        !
        !*** Compute MY_TGSD
        !
        call tgsd_setfrac(MY_TGSD,MY_ERR)
        if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_TGSD, MY_FILES, MY_ERR)
        !
        !*** Master writes output file
        !
        if(master_model) call tgsd_write_tgsd_granulometry(MY_FILES,MY_TGSD, MY_SPE%name(ispe),MY_ERR)
        call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
        if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_TGSD, MY_FILES, MY_ERR)
        !
        !*** Deallocate MY_TGSD
        !
        call tgsd_deallocate_TGSD(MY_TGSD,MY_ERR)
        !
     end select
     !
  end do
  !
  !*** Normal end
  !
  if(master_model) call inpout_close_log_file(TASK_SET_TGSD, MY_FILES, MY_ERR)
  call parallel_bcast(MY_ERR%flag,1_ip,0_ip)
  if(MY_ERR%flag.ne.0) call task_runend(TASK_SET_TGSD, MY_FILES, MY_ERR)
  !
  return
  end subroutine task_SetTgsd
