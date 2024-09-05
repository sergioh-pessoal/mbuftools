!-----------------------------------------------------------------------------+
!                         System Command module                               |
!                                                                             | 
!                      ( Cat - Concatenate Files )                            |
!-----------------------------------------------------------------------------+
! program example
!  use msystem
!  character(len=1024),dimension(10):: filelist
!  character(len=1024)              ::outfile
!  integer::err
!  outfile="teste.txt"
! 
!  call cat_msystem("*.f90",outfile,300,err)
!   if (err==0) print *,"Ok"
! end program
!------------------------------------------------------------------------------
 module msystem
 
  use iso_fortran_env
  implicit none
  
  private
  public cat_msystem
 
  
  integer,parameter::verbosity=3
  interface cat_msystem
	module procedure cat1
	module procedure cat2
  end interface 
	
 contains
!-----------------------------------------------------------------------------+
!                       ( Cat1 - Concatenate Files )                           |
!-----------------------------------------------------------------------------+
subroutine cat1(filelist,outfile,maxnf,err)
	character(len=*),dimension(:),intent(in)::filelist
	character(len=*),             intent(in)::outfile
	integer,                      intent(in)::maxnf
	integer,                     intent(out)::err
	integer::lsize
	integer::i,ios
	if (maxnf>0) then 
		call execute_command_line('cat '//trim(filelist(1))//' > '//trim(outfile), wait=.TRUE., exitstat=ios)
		if (verbosity>0) print *,"cat "//trim(filelist(1))//" > "//trim(outfile)
		do i=2,maxnf
			call execute_command_line('cat '//trim(filelist(i))//' >> '//trim(outfile), wait=.TRUE., exitstat=ios)
			if (verbosity>0) print *,"cat "//trim(filelist(i))//" >> "//trim(outfile)
			if (ios==0) call execute_command_line('rm '//trim(filelist(i)), wait=.FALSE.)
		end do 	
		err=ios
	end if 
end subroutine 
!-----------------------------------------------------------------------------+
!                       ( Cat2 - Concatenate Files )                          |
!-----------------------------------------------------------------------------+
subroutine cat2(basename,outfile,maxnf,err)

    character(len=*),intent(in)::basename ! File base name 
    character(len=*),intent(in)::outfile  ! Output file name
    integer,         intent(in)::maxnf    ! Maximum number of files
    integer,        intent(out)::err      ! Error code (0 = ok)

    character(len=1024),dimension(maxnf)::filelist
    character(len=*),         parameter :: ls_file = '/tmp/my_ls.tmp'
    integer                             :: u, ios
    character(len=30)                   :: filename
    integer::i,imax
    err=0
    i=0
    if (verbosity>0) print *," ls -1 "//trim(basename)//" > "//ls_file 
    call execute_command_line("ls -1 "//trim(basename)//" > "//ls_file, wait=.TRUE., exitstat=ios)
    if (ios /= 0) stop "Unable to get listing"
    
    
    open(newunit=u, file=ls_file, iostat=ios, status="old", action="read")
   
    err=ios
    if ( ios == 0 ) then 
    
	do 
		read(u, *, iostat=ios) filename
		if (is_iostat_end(ios)) exit
		i=i+1
		
		if (i<maxnf) then 
			filelist(i)=filename
			imax=i
			if (verbosity>0) print *,i,filename
		else
			err=1
			exit
		end if    
	end do
    end if 
    	
    close(u)
    
    if (err==0) then 	
	call execute_command_line('rm '//ls_file, wait=.FALSE.)
	if (imax>0) then 
		call cat1(filelist,outfile,imax,err)
	else
		err=3
	end if 
    end if 
    	
end subroutine 
end module
