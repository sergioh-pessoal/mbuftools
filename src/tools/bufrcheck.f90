program bufrcheck

!******************************************************************************
!*                                  BUFRCHECK                                 *
!*                                                                            *
!*              PROGRAM FOR CHECKING THE INTEGRITY OF BUFR FILES              *.
!*                                                                            *
!*                   MCT-INPE-Sao Jose dos Campos, Brasil  (2024)             *
!*                                                                            *
!*                                                                            *
!*----------------------------------------------------------------------------*
!* Original version: sergio.ferreira@inpe.br  (SHSF)
!* Dependeces : MBUFR-ADT and STRIGFLIB
!******************************************************************************
! HYSTORY:
!   2024-08-01 - SHSF - INITIAL VERSION

USE MBUFR
USE STRINGFLIB
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                          stdout=>output_unit, &
                                          stderr=>error_unit
implicit none

character(len=1024)                  ::bufrfile
integer                              ::s       ! Number of subsets
integer                              ::m       ! Number of messages
integer, parameter                   ::nmmax=100000
integer(kind=8),dimension(nmmax)     ::Pos
type(sec1type),dimension(nmmax)      ::sec1
integer,dimension(nmmax)             ::nsubsets
integer,dimension(nmmax)             ::nbytes
integer                              ::err
integer                              ::i
    call welcome(bufrfile)

    Call OPEN_MBUFR(1, bufrfile)
    call FIND_MESSAGES_MBUFR(1,0,m ,pos,sec1,nsubsets,nbytes,errors=err)

    ! Total os subsets
    s=0
    do i=1,m
      s=s+nsubsets(i)
    end do

    write(stdout,'(i10,1x,i10,1x,i10)')s,m,err

      Close (1)
    stop
contains

!-----------------------------------------------------------------------------+
!  software welcome: Returns input parameters                                 |
!-----------------------------------------------------------------------------+
!
!
!-----------------------------------------------------------------------------+
  subroutine welcome(bufrfile)
  !*** Interface declaration
    character(len=*),intent(out)::bufrfile

  !***  local variables declaration
    character(len=256),dimension(300)   ::arg      ! Input arguments
    integer                             ::narg     ! Number of input arguments
    character(len=1),dimension(300)     ::namearg  ! Argument names
    integer                             ::iargc    !
    integer(kind=2)                     ::argc
    integer ::i,x0

  !*** start
    x0=0
    call getarg2(namearg,arg,narg)
    if (narg>0) then
        do i=1,narg
            if(namearg(i)=="i") then
                bufrfile=arg(i)
                x0=1
            end if
        end do
    end if
    if (x0==0) then
    print *,"+--------------------------------------------------------+"
    print *,"| MCTI-INPE BUFRCHECK : Checking BUFR file               |"
    print *,"| Include MBUFR-ADT module ",MBUFR_VERSION,"     |"
    print *,"+--------------------------------------------------------+"
    print *,"| USE: bufrcheck -i input_filename                       |"
    print *,"+                                                        |"
    print *,"| to check a BUFR file and returns: s,m,e                |"
    print *,"| where: s = Number of subsets                           |"
    print *,"|        m = Number of messages                          |"
    print *,"|        e = Number of messages with errors              |"
    print *,"+--------------------------------------------------------+"
    print *,""
    stop
    endif
  end subroutine
end program
