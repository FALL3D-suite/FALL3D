!
!------------------------------------
!    subroutine task_runend
!------------------------------------
!
!>   @brief
!>   Task to end a run prematurely
!>   @author
!>   Arnau Folch
!
subroutine task_runend(task, MY_FILES, MY_ERR)
  use KindType
  use Parallel
  use InpOut
  implicit none
  !
  !>   @param task      task flag
  !>   @param MY_FILES  list of files
  !>   @param MY_ERR    error handler
  !
  integer(ip),       intent(IN   ) :: task
  type(FILE_LIST),   intent(IN   ) :: MY_FILES
  type(ERROR_STATUS),intent(INOUT) :: MY_ERR
  !
  select case(task)
  case(TASK_SET_ENS,TASK_POS_ENS,TASK_POS_VAL)
     if(master_world) call inpout_close_log_file(task, MY_FILES, MY_ERR)
  case default
      if(master_model) call inpout_close_log_file(task, MY_FILES, MY_ERR)
  end select
  call parallel_hangup(MY_ERR)
  !
  return
end subroutine task_runend
!
!------------------------------------
!    subroutine task_wriwarn
!------------------------------------
!
!>   @brief
!>   Task to register a warning message
!>   @author
!>   Arnau Folch
!
subroutine task_wriwarn(MY_ERR, message)
  use KindType
  implicit none
  !
  !>   @param MY_ERR    error handler
  !>   @param MY_FILES  list of files
  !
  type(ERROR_STATUS),intent(INOUT) :: MY_ERR
  character(len=*),  intent(IN   ) :: message
  !
  MY_ERR%nwarn = MY_ERR%nwarn + 1
  if(MY_ERR%nwarn.lt.100) MY_ERR%warning(MY_ERR%nwarn) = TRIM(message)
  !
  return
end subroutine task_wriwarn
