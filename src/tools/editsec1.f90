program editsec1

! SHSF 20211105 - Initial version 

use mbufr
use stringflib

implicit none

!{ DECLARATION OF VARIABLES USED BY READ_MBUFR
  type(octtype)                   ::bufrmessage
  type(sec1type)                  ::sec1
  integer                         ::BUFR_ED
  integer                         ::err
  character(len=40)               ::header !Telecommunications header (40 bytes)
  
!}

!{ AUXILIARY VARIABLES OF MAIN PROGRAM
  character(len=1),dimension(300)  ::argname
  character(len=255),dimension(300)::arg
  integer                         ::narg
  character(len=255)              ::infile,outfile
  integer                         ::X1,X2,i
  integer                         ::nm 
  integer                         ::rr 
!}


       X1=0; X2=0
       nm=0
       call getarg2(argname,arg,narg)
       
      do i=1,narg
        if (argname(i)=="o") then 
          outfile=arg(i)
          x2=1
        elseif (argname(i)=="i") then
	  infile=arg(i)
	  x1=1
	end if 
      end do

	print *,"+-----------------------------------------------------------------+"
	print *,"|                   EDITSEC1   (2021-11-04)                       |"
	print *,"|          Include MBUFR-ADT module ",MBUFR_VERSION,"     |"
	print *,"+-----------------------------------------------------------------+"
	
	if (x1*x2==0) then 
              
		print *,"|                                                                 |"
		print *,"| use:   editsec1  -i <infile> -o <outfile>                      |"
		print *,"|                                                                 |"
		print *,"+-----------------------------------------------------------------"
		stop
	endif
  !}
  
	!----------------------------
	!Open input and output files 
	!---------------------------
	
	Call OPEN_MBUFR(1, infile)

	open(2,file=outfile,STATUS='unknown',FORM='UNFORMATTED',access='DIRECT',recl=1)

	rr=0
	
	
10	CONTINUE
	nm=nm+1
	!---------------------------------
	!Read next message from imput file 
	!----------------------------------
	
	Call READBIN_MBUFR(1,bufrmessage, bUFR_ED, sec1,err, header)
	 

 20	If ((bufrmessage%nocts > 0).and.(IOERR(1)==0)) Then

		print *,"Edition/center/MasterTable/LocalTable/Nocsts=",bufr_ED,sec1%center,sec1%VerMasterTable,sec1%VerLocalTable

		!---------------------------------------------------------------
		! If  Local Table Version Number = 0  replace by 2  (in octet 20)  
		!----------------------------------------------------------------
		if (sec1%VerLocalTable==0) then 
			bufrmessage%oct(20)=char(2)
		end if
		
		!---------------------------------------------------------------
		! Write the mensage in output file   
		!----------------------------------------------------------------
		 
		do i=1,bufrmessage%nocts
			rr=rr+1
			write (2,rec=rr) bufrmessage%oct(i)                                	
		end do
               
		deallocate(bufrmessage%oct)
		goto 10
      end if
  !}
  
 call Close_mbufr (1)
 close (2)
 print *,trim(color_text(":editsec1: Done",32,.true.)) 
 stop
End



