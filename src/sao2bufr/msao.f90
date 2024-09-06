!-----------------------------------------
!  Module to read SAO data (jusr for test)
!-----------------------------------------
module msao
 implicit none
 private
 public read_sao
 public data_file_index
 public geophysical_constants
 public system_descriptors
 public time_stamp_sounder_settings

 integer,dimension(80)           ::data_file_index
 real,dimension(16)              ::geophysical_constants
 character(len=120)              ::system_descriptors
 character(len=120)              ::time_stamp_sounder_settings
 integer::current_rg
 contains

 !-------------------------------
 ! subroutine to read a SAO file
 !--------------------------------
  subroutine read_sao(filename)
    character(len=*),intent(in)::filename
    character(len=1024)::line
    print *,trim(filename)
    open(1,file=filename, STATUS='unknown',FORM='UNFORMATTED',access='DIRECT',recl=1)
    !open(1,file=filename, STATUS='unknown', RECORDTYPE='STREAM_LF') < In case of intel fortran only
    current_rg=0

    call readline(2,line)   ! Read 2 lines
    read(line,100) data_file_index(1:80)

    call readline(1,line) ! read 1 line
    read(line,101)geophysical_constants

    call readline(1,system_descriptors)

    call readline(1,time_stamp_sounder_settings)

    print *,"---------------"
    print *,"data_file_index"
    print *,data_file_index

    print *,"---------------"
    print *,"geophysical_constants"
    print *,geophysical_constants

    print *,"---------------"
    print *,"system_descriptors"
    print *,trim(system_descriptors)

    print *,time_stamp_sounder_settings

    close(1)

 100   format(2(40I3))
 101   format(16F7.3)
  end subroutine


  !---------------------------------------------------------------------------
  ! Subroutine to read a text line with  "carriage return" and
  ! "line feed /new line" (ichar=10) at end of each line
  !
  ! Concatenates nl lines
  !------------------------------------------------------------------------
  subroutine readline(nl,line)
   integer,          intent(in)::nl ! Number of lines to be concatenated
   character(len=*),intent(out)::line
   character(len=1)::a
   integer:: i,j
   line=""
   i=0
   do j=1,nl
33    current_rg=current_rg+1
      i=i+1
      read (1,REC= current_rg)a
      if ((ichar(a)/=13).and.(ichar(a)/=10)) line(i:i)=a
      if (ichar(a)/=10) goto 33
   end do
   end subroutine

end module
