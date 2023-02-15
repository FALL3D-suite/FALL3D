!***********************************************************************
!>
!> Module used as an interface between the F90 code and MPI
!> @details
!> The code is compiled in parallel if the macro WITH_MPI is defined and
!> in serial otherwise
!> @note
!> Module public variables are defined only after calling parallel_startup (mandatory)
!> @author
!> Arnau Folch
!>
!**********************************************************************
MODULE Parallel
  use KindType
  implicit none
#if defined WITH_MPI
  include 'mpif.h'
#endif
  save
  !
  !   LIST OF PUBLIC VARIABLES
  !
  logical     :: master          !< true/false (master for i/o)
  logical     :: PARALLEL_RUN    !< true/flase
  integer(ip) :: COMM_WORLD      !< MPI_COMM_WORLD handler
  integer(ip) :: MPI_PRECISION   !< MPI data type
  integer(ip) :: nproc           !< total number of processors
  integer(ip) :: mpime           !< my processor number (my task id)
  !
  !   LIST OF PUBLIC ROUTINES
  !
  PUBLIC  :: parallel_startup
  PUBLIC  :: parallel_hangup
  PUBLIC  :: parallel_wait
  PUBLIC  :: parallel_barrier
  PUBLIC  :: parallel_bcast
  PUBLIC  :: parallel_sum
  PUBLIC  :: parallel_max
  PUBLIC  :: parallel_min
  PUBLIC  :: parallel_isend
  PUBLIC  :: parallel_irecv
  !
  !>   @brief
  !>   Parallel_bcast ( array, n, who_sends ) processor who_sends broadcasts an array of lenght n (interfaced procedure)
  !
  INTERFACE parallel_bcast
     MODULE PROCEDURE bcast_real,bcast_real_1d,bcast_real_2d,bcast_real_3d, &
          bcast_real_4d,bcast_integer,bcast_integer_1d,bcast_integer_2d, &
          bcast_character,bcast_logical,bcast_logical_1d
  END INTERFACE parallel_bcast
  PRIVATE :: bcast_real,bcast_real_1d,bcast_real_2d,bcast_real_3d, &
       bcast_real_4d, bcast_integer,bcast_integer_1d,bcast_integer_2d, &
       bcast_character,bcast_logical,bcast_logical_1d
  !
  !>   @brief
  !>   Parallel_sum ( array, group )  array reduction among processors of group (interfaced procedure)
  !
  INTERFACE parallel_sum
     MODULE PROCEDURE parallel_sum_r1d, parallel_sum_r2d,parallel_sum_r3d, &
          parallel_sum_r4d,parallel_sum_i1d, parallel_sum_i2d, &
          parallel_sum_i3d, parallel_sum_i4d
  END INTERFACE parallel_sum
  PRIVATE :: parallel_sum_r1d, parallel_sum_r2d,parallel_sum_r3d, &
       parallel_sum_r4d,parallel_sum_i1d, parallel_sum_i2d,parallel_sum_i3d, &
       parallel_sum_i4d
  PRIVATE :: parallel_sum_real
  PRIVATE :: parallel_sum_integer
  !
  !>   @brief
  !>   Parallel_max ( my_max, gl_max, group )  maximum value across processors
  !
  INTERFACE parallel_max
     MODULE PROCEDURE parallel_max_real, parallel_max_integer
  END INTERFACE parallel_max
  PRIVATE :: parallel_max_real, parallel_max_integer
  !
  !>   @brief
  !>   Parallel_min ( my_min, gl_min, group )  minimum value across processors
  !
  INTERFACE parallel_min
     MODULE PROCEDURE parallel_min_real, parallel_min_integer
  END INTERFACE parallel_min
  PRIVATE :: parallel_min_real, parallel_min_integer
  !
  !>   @brief
  !>   Parallel_isend ( array(i), n, idest, itag, ihand, group)  I send n elements to processor idest starting from array(i)  (interfaced procedure)
  !
  INTERFACE parallel_isend
     MODULE PROCEDURE isend_real,isend_integer
  END INTERFACE parallel_isend
  PRIVATE :: isend_real,isend_integer
  !
  !>   @brief
  !>   Parallel_irecv ( array(i), n, idest, itag, ihand, group) I receive n elements from processor idest stored from array(i)  (interfaced procedure)
  !
  INTERFACE parallel_irecv
     MODULE PROCEDURE irecv_real,irecv_integer
  END INTERFACE parallel_irecv
  PRIVATE :: irecv_real,irecv_integer
  !
  !
  !
CONTAINS
  !
  !------------------------------
  !   subroutine parallel_startup
  !------------------------------
  !
  !>   @brief
  !>   Starts MPI and sets module public variables (mandatory)
  !
  subroutine parallel_startup (MY_ERR)
    implicit none
    !
    !>  @param MY_ERR  error handler
    !
    type(ERROR_STATUS) :: MY_ERR
    !
    MY_ERR%flag    = 0
    MY_ERR%source  = 'parallel_startup'
    MY_ERR%message = ' '
    !
#if defined WITH_MPI
    if(rp.eq.4) then
       MPI_PRECISION = MPI_FLOAT
    else
       MPI_PRECISION = MPI_DOUBLE_PRECISION
    end if
#endif
    !
#if defined WITH_MPI
    call MPI_INIT     (                     MY_ERR%flag)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,MY_ERR%flag)
    call MPI_COMM_RANK(MPI_COMM_WORLD,mpime,MY_ERR%flag)
    PARALLEL_RUN = .true.
    COMM_WORLD   = MPI_COMM_WORLD
    if(mpime == 0) then
       master = .true.
    else
       master = .false.
    end if
#else
    nproc  = 1
    mpime  = 0
    PARALLEL_RUN = .false.
    COMM_WORLD   = 0
    master = .true.
#endif
    !
    return
  end subroutine parallel_startup
  !
  !------------------------------
  !   subroutine parallel_hangup
  !------------------------------
  !
  !>   @brief
  !>   Closes MPI (mandatory)
  !
  subroutine parallel_hangup(MY_ERR)
    implicit none
    !
    !>  @param MY_ERR  error handler
    !
    type(ERROR_STATUS) :: MY_ERR
    !
#if defined WITH_MPI
    call MPI_FINALIZE(MY_ERR%flag)
#endif
    call exit(MY_ERR%flag)
    !
    return
  end subroutine parallel_hangup
  !
  !------------------------------
  !   subroutine parallel_wait
  !------------------------------
  !
  !>   @brief
  !>   Waits for ihand request to be completed
  !
  SUBROUTINE parallel_wait( ihand )
    IMPLICIT NONE
    !
    !>  @param ihand  request for MPI_WAIT
    !
    INTEGER(ip)  :: ihand
#if defined WITH_MPI
    INTEGER(ip) :: ierr
    INTEGER(ip) :: istatus( MPI_STATUS_SIZE )
    CALL MPI_WAIT( ihand, istatus, ierr )
#endif
    RETURN
  END SUBROUTINE parallel_wait
  !
  !------------------------------
  !   subroutine parallel_barrier
  !------------------------------
  !
  !>   @brief
  !>   Puts a barrier for a group of processors
  !
  SUBROUTINE parallel_barrier( what_group )
    IMPLICIT NONE
    !
    !>  @param what_group  group of processors for MPI_BARRIER
    !
    INTEGER(ip)  :: what_group
#if defined WITH_MPI
    INTEGER(ip) :: ierr
    CALL MPI_BARRIER( what_group, ierr )
#endif
    RETURN
  END SUBROUTINE parallel_barrier
  !
  !
  !
  !***********************************************************************
  !*
  !*    LIST OF ROUTINES INTERFACED (module private)
  !*
  !***********************************************************************
  !
  SUBROUTINE BCAST_REAL( ARRAY, N, who )
    IMPLICIT NONE
    REAL(rp)    ::  array
    INTEGER(ip) :: n, who
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_BCAST( array, n, MPI_PRECISION, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_REAL
  !
  !
  !
  SUBROUTINE BCAST_REAL_1d( ARRAY, N, who )
    IMPLICIT NONE
    REAL(rp)    ::  array(*)
    INTEGER(ip) :: n, who
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_BCAST( array, n, MPI_PRECISION, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_REAL_1d
  !
  !
  !
  SUBROUTINE BCAST_REAL_2d( ARRAY, N, who )
    IMPLICIT NONE
    REAL(rp)    ::  array(:,:)
    INTEGER(ip) :: n, who
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_BCAST( array, n, MPI_PRECISION, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_REAL_2d
  !
  !
  !
  SUBROUTINE BCAST_REAL_3d( ARRAY, N, who )
    IMPLICIT NONE
    REAL(rp)    ::  array(:,:,:)
    INTEGER(ip) :: n, who
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_BCAST( array, n, MPI_PRECISION, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_REAL_3d
  !
  !
  !
  SUBROUTINE BCAST_REAL_4d( ARRAY, N, who )
    IMPLICIT NONE
    REAL(rp)    ::  array(:,:,:,:)
    INTEGER(ip) :: n, who
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_BCAST( array, n, MPI_PRECISION, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_REAL_4d
  !
  !
  !
  SUBROUTINE BCAST_INTEGER(ARRAY,N,who)
    IMPLICIT NONE
    INTEGER(ip) :: ARRAY
    INTEGER(ip) :: N,who
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_BCAST( array, n, MPI_INTEGER, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_INTEGER
  !
  !
  !
  SUBROUTINE BCAST_INTEGER_1D(ARRAY,N,who)
    IMPLICIT NONE
    INTEGER(ip) :: ARRAY(*)
    INTEGER(ip) :: N,who
#if defined WITH_MPI
    INTEGER(ip) ::  err
    CALL MPI_BCAST( array, n, MPI_INTEGER, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_INTEGER_1D
  !
  !
  !
  SUBROUTINE BCAST_INTEGER_2D(ARRAY,N,who)
    IMPLICIT NONE
    INTEGER(ip) :: ARRAY(:,:)
    INTEGER(ip) :: N,who
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_BCAST( array, n, MPI_INTEGER, who, MPI_COMM_WORLD, err)
#endif
    RETURN
  END SUBROUTINE BCAST_INTEGER_2D
  !
  !
  !
  SUBROUTINE BCAST_CHARACTER( ARRAY, N, who )
    IMPLICIT NONE
    CHARACTER(LEN=*) :: ARRAY
    INTEGER(ip)      :: N, who
#if defined WITH_MPI
    INTEGER(ip) :: I, nn
    INTEGER(ip), ALLOCATABLE :: IARRAY(:)
    nn = LEN( array )
    n = n
    ALLOCATE( iarray( nn ) )
    DO I=1,nn
       IARRAY(I) = ICHAR( array( i:i ) )
    END DO
    CALL bcast_integer_1d( iarray, nn, who )
    DO I=1,nn
       ARRAY(i:i) = CHAR( iarray( i ) )
    END DO
    DEALLOCATE( iarray )
#endif
    RETURN
  END  SUBROUTINE BCAST_CHARACTER
  !
  !
  !
  SUBROUTINE BCAST_LOGICAL(ARRAY,N,who)
    IMPLICIT NONE
    LOGICAL ARRAY
    INTEGER(ip) :: N
    INTEGER(ip) :: who
#if defined WITH_MPI
    INTEGER(ip) :: I
    INTEGER(ip), ALLOCATABLE :: IARRAY(:)
    ALLOCATE( iarray( n ) )
    DO I=1,N
       IF(ARRAY) THEN
          IARRAY(I) = 1
       ELSE
          IARRAY(I) = 0
       END IF
    END DO
    CALL bcast_integer_1d( iarray, n, who )
    DO I=1,N
       IF(IARRAY(I).EQ.1) THEN
          ARRAY = .TRUE.
       ELSE
          ARRAY = .FALSE.
       END IF
    END DO
    DEALLOCATE( iarray )
#endif
    RETURN
  END SUBROUTINE BCAST_LOGICAL
  !
  !
  !
  SUBROUTINE BCAST_LOGICAL_1D(ARRAY,N,who)
    IMPLICIT NONE
    LOGICAL ARRAY(:)
    INTEGER(ip) :: N
    INTEGER(ip) :: who
#if defined WITH_MPI
    INTEGER(ip) :: I
    INTEGER(ip), ALLOCATABLE :: IARRAY(:)
    ALLOCATE( iarray( n ) )
    DO I=1,N
       IF(ARRAY(I)) THEN
          IARRAY(I) = 1
       ELSE
          IARRAY(I) = 0
       END IF
    END DO
    CALL bcast_integer_1d( iarray, n, who )
    DO I=1,N
       IF(IARRAY(I).EQ.1) THEN
          ARRAY(I) = .TRUE.
       ELSE
          ARRAY(I) = .FALSE.
       END IF
    END DO
    DEALLOCATE( iarray )
#endif
    RETURN
  END SUBROUTINE BCAST_LOGICAL_1D
  !
  !
  !
  SUBROUTINE PARALLEL_MAX_REAL( my_max, gl_max, what_grp )
    IMPLICIT NONE
    INTEGER(ip) :: what_grp
    REAL(rp)    :: my_max, gl_max
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_ALLREDUCE( my_max, gl_max, 1, MPI_PRECISION, MPI_MAX,  what_grp, ERR )
#else
    gl_max = my_max
#endif
    RETURN
  END SUBROUTINE PARALLEL_MAX_REAL
  !
  !
  !
  SUBROUTINE PARALLEL_MAX_INTEGER( my_max, gl_max, what_grp )
    IMPLICIT NONE
    INTEGER(ip) :: what_grp
    INTEGER(ip) :: my_max, gl_max
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_ALLREDUCE( my_max, gl_max, 1, MPI_INTEGER, MPI_MAX,  what_grp, ERR )
#else
    gl_max = my_max
#endif
    RETURN
  END SUBROUTINE PARALLEL_MAX_INTEGER
  !
  !
  !
  SUBROUTINE PARALLEL_MIN_REAL( my_min, gl_min, what_grp )
    IMPLICIT NONE
    INTEGER(ip) :: what_grp
    REAL(rp)    :: my_min, gl_min
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_ALLREDUCE( my_min, gl_min, 1, MPI_PRECISION, MPI_MIN,  what_grp, ERR )
#else
    gl_min = my_min
#endif
    RETURN
  END SUBROUTINE PARALLEL_MIN_REAL
  !
  !
  !
  SUBROUTINE PARALLEL_MIN_INTEGER( my_min, gl_min, what_grp )
    IMPLICIT NONE
    INTEGER(ip) :: what_grp
    INTEGER(ip) :: my_min, gl_min
#if defined WITH_MPI
    INTEGER(ip) :: err
    CALL MPI_ALLREDUCE( my_min, gl_min, 1, MPI_INTEGER, MPI_MIN,  what_grp, ERR )
#else
    gl_min = my_min
#endif
    RETURN
  END SUBROUTINE PARALLEL_MIN_INTEGER
  !
  !
  !
  SUBROUTINE PARALLEL_SUM_REAL( ARRAY, N, what_grp )
    IMPLICIT NONE
    INTEGER(ip) :: N, what_grp
    REAL(rp)    :: ARRAY(N)
#if defined WITH_MPI
    INTEGER(ip) :: err
    REAL(rp), ALLOCATABLE :: ARRAY_TMP(:)
    ALLOCATE( ARRAY_TMP( n ) )
    CALL MPI_ALLREDUCE( ARRAY, ARRAY_TMP, N, MPI_PRECISION, MPI_SUM,  what_grp, ERR )
    ARRAY(1:n) = ARRAY_TMP(1:n)
    DEALLOCATE( ARRAY_TMP )
#endif
    RETURN
  END SUBROUTINE PARALLEL_SUM_REAL
  !
  !
  !
  SUBROUTINE PARALLEL_SUM_INTEGER(ARRAY, N, what_grp)
    IMPLICIT NONE
    INTEGER N, what_grp
    INTEGER ARRAY(N)
#if defined WITH_MPI
    INTEGER(ip) :: err
    INTEGER(ip), ALLOCATABLE :: ARRAY_TMP(:)
    ALLOCATE( ARRAY_TMP( n ) )
    CALL MPI_ALLREDUCE(ARRAY, ARRAY_TMP, N, MPI_INTEGER, MPI_SUM, what_grp, ERR)
    ARRAY(1:n) = ARRAY_TMP(1:n)
    DEALLOCATE( ARRAY_TMP )
#endif
    RETURN
  END SUBROUTINE PARALLEL_SUM_INTEGER
  !
  !
  !
  SUBROUTINE parallel_sum_r1d( array, what_grp )
    REAL   (rp) :: array( : )
    INTEGER(ip) :: what_grp
    CALL parallel_sum_real( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_r1d
  !
  !
  !
  SUBROUTINE parallel_sum_r2d( array, what_grp )
    REAL   (rp) :: array( :, : )
    INTEGER(ip) :: what_grp
    CALL parallel_sum_real( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_r2d
  !
  !
  !
  SUBROUTINE parallel_sum_r3d( array, what_grp )
    REAL   (rp) :: array( :, :, : )
    INTEGER(ip) :: what_grp
    CALL parallel_sum_real( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_r3d
  !
  !
  !
  SUBROUTINE parallel_sum_r4d( array, what_grp )
    REAL   (rp) :: array( :, :, :, : )
    INTEGER(ip) :: what_grp
    CALL parallel_sum_real( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_r4d
  !
  !
  !
  SUBROUTINE parallel_sum_i1d( array, what_grp )
    INTEGER(ip) :: array( : )
    INTEGER(ip) :: what_grp
    CALL parallel_sum_integer( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_i1d
  !
  !
  !
  SUBROUTINE parallel_sum_i2d( array, what_grp )
    INTEGER(ip) :: array( :, : )
    INTEGER(ip) :: what_grp
    CALL parallel_sum_integer( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_i2d
  !
  !
  !
  SUBROUTINE parallel_sum_i3d( array, what_grp )
    INTEGER(ip) :: array( :, :, : )
    INTEGER(ip) :: what_grp
    CALL parallel_sum_integer( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_i3d
  !
  !
  !
  SUBROUTINE parallel_sum_i4d( array, what_grp )
    INTEGER(ip) :: array( :, : ,:, :)
    INTEGER(ip) :: what_grp
    CALL parallel_sum_integer( array, SIZE( array ), what_grp )
    RETURN
  END SUBROUTINE parallel_sum_i4d
  !
  !
  !
  SUBROUTINE ISEND_REAL( array, n, idest, itag, ihand, what_group )
    IMPLICIT NONE
    REAL(rp)    :: array !array(*)
    INTEGER(ip) :: n, idest, itag, ihand, what_group
#if defined WITH_MPI
    INTEGER(ip) :: ierr
    CALL MPI_ISEND( array, n, MPI_PRECISION, idest, itag, what_group, ihand, ierr )
#endif
    RETURN
  END SUBROUTINE ISEND_REAL
  !
  !
  !
  SUBROUTINE ISEND_INTEGER( array, n, idest, itag, ihand, what_group )
    IMPLICIT NONE
    INTEGER(ip) :: array !array(*)
    INTEGER(ip) :: n, idest, itag, ihand, what_group
#if defined WITH_MPI
    INTEGER(ip) :: ierr
    CALL MPI_ISEND( array, n, MPI_INTEGER, idest, itag, what_group, ihand, ierr )
#endif
    RETURN
  END SUBROUTINE ISEND_INTEGER
  !
  !
  !
  SUBROUTINE IRECV_REAL( array, n, isour, itag, ihand, what_group )
    IMPLICIT NONE
    REAL(rp)    :: array !array(*)
    INTEGER(ip) :: n, isour, itag, ihand, what_group
#if defined WITH_MPI
    INTEGER(ip) :: ierr
    CALL MPI_IRECV( array, n, MPI_PRECISION, isour, itag, what_group, ihand, ierr )
#endif
    RETURN
  END SUBROUTINE IRECV_REAL
  !
  !
  !
  SUBROUTINE IRECV_INTEGER( array, n, isour, itag, ihand, what_group )
    IMPLICIT NONE
    INTEGER(ip) :: array  !array(*)
    INTEGER(ip) :: n, isour, itag, ihand, what_group
#if defined WITH_MPI
    INTEGER(ip) :: ierr
    CALL MPI_IRECV( array, n, MPI_INTEGER, isour, itag, what_group, ihand, ierr )
#endif
    RETURN
  END SUBROUTINE IRECV_INTEGER
  !
  !
  !
END MODULE Parallel
