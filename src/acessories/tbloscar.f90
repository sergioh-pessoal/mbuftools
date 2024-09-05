!******************************************************************************
!******************************************************************************
!* CPTEC/INPE
!* Sergio Henique Soares Ferreira (SHSF) CPTEC/INPE
!* --------------------------------------------------------------------------
!* HISTORY

program tbloscar
  use stringflib, only:split,sep_words,sep_natnum,replace,val,isval,getarg2,ucases,rights,init_stringflib
  implicit none
  
  character (len=1024)               ::infile      !-Input file name
  character (len=1024)               ::outfile     !-Output file name
  character (len=2048)               ::line        !-A text line
  character (len=1024),dimension(100)::cols        !-columns
  integer                            ::ncols       !-Number of columns 
  integer,dimension(4)               ::Nat
  integer                            ::nNat 
  character (len=128),dimension(5)   ::wcols       !Columns for processing wigos id
  integer                            ::nwcols      !Number of columns win wcols  
  character(len=255)                 ::mbufr_tables!-MBUFR_TABLES directory
  character(len=128)                  ::auxc
  integer                            ::i,x1,x2
  character(len=255),dimension(10)   ::arg         !-An argument. 
  integer                            ::narg        !-Number of Arguments 
  character(len=1),dimension(10)     ::argname     !-Argument name.
  integer,parameter                  ::nsmax=10000   ! Maximum number of weather station
  integer                            ::ns            ! Number of weather station 
  integer,dimension(1:nsmax)         ::regionId      !1 RegionId
                                                      !2 RegionName
  					             !3 CountryArea
                                                     !4 CountryCode
  integer*1,dimension(1:nsmax)        ::wigos1        !5 StationId
  integer*2,dimension(1:nsmax)        ::wigos2
  integer*2,dimension(1:nsmax)        ::wigos3
  character(len=16),dimension(1:nsmax)::wigos4
  
  integer,dimension(1:nsmax)          ::IndexNbr     !6 IndexNbr
                                                     !7 IndexSubNbr
  character(len=60),dimension(1:nsmax)::StationName !8 StationName
  Real,dimension(1:nsmax)             ::Lat         !9 Latitude
  real,dimension(1:nsmax)             ::Lon         !10 Longitude
  logical::flag
  flag=.false.
!-----------------  
! **  Welcome **
!-----------------
!{

  x1=0
  x2=0
  outfile='VolA_Legacy.txt'
  call getarg2(argname,arg,narg)
   do i=1,narg
      if (argname(i)=="i") then 
          infile=arg(i)
          x1=1
      elseif (argname(i)=="o") then
          outfile=arg(i)
	  x2=1
      end if
    end do
    
    if (x1==0) then
      print *,"---------------------------------------------------------"
      print *," CPTEC/INPE tbloscar: Converts OSCAR tables               "
      print *,"---------------------------------------------------------"
      print *,"use:"
      print *," tbloscar -i infile -o outfile"
      print *,""
      print *,"--------------------------------------------------------"
      stop
     else
      print *,"---------------------------------------------------------"
      print *," CPTEC/INPE tbloscar: Converts OSCAR tables               "
      print *,"---------------------------------------------------------"
     endif
!}
 !----------------
 ! initialization
 !----------------
 !{
   call getenv('MBUFR_TABLES',mbufr_tables)
   
   !--------------------------------------------------------------------------
   ! Adds slash at the end of the path, if necessary.
   ! In this process, check if the path contains "/" or "\" (Windows or Linux) 
   !--------------------------------------------------------------------------
   !{
    i=len_trim(mbufr_tables)
    if ((mbufr_tables(i:i)/=char(92)).and.(mbufr_tables(i:i)/="/")) then 
        if (index(mbufr_tables,char(92))>0) then 
           mbufr_tables=trim(mbufr_tables)//char(92)
        else
           mbufr_tables=trim(mbufr_tables)//"/"
        end if
    end if
   outfile=trim(mbufr_tables)//trim(outfile)
   open(2,file=outfile,status='unknown')
   !}
  !} 
  !------------------------------
  ! Get confirmation before start
  !------------------------------
  !{ 
   print *,"Input table file  =",trim(infile)
   print *,"Output table file =",trim(outfile)
   print *,"Continue (S/N)?"
   read(*,*) auxc
   IF (ucases(auxc(1:1))/="S") stop 
   open (2,file=outfile,status='unknown')
  !}

      open (1,file=infile,status='old')
      read(1,'(a)',end=99)line
      ns=0
      
10    read(1,'(a)',end=99)line
        call split(line,char(9),cols,ncols)
        !if (index(line,"16020")>0) then 
	!  print *,trim(line)
	!  do i=1,ncols
	!   print *,i,"[",trim(cols(i)),"]"
	!  end do
	!   read(*,*) auxc
	! end if 
        ns=ns+1
        regionid(ns) = val(cols(1))
	
       !{ WIGOS ID Processing 
           auxc=trim(cols(5))
	   call split(auxc,"-",wcols,nwcols)   
	   wigos1(ns)=val(wcols(1))
	   wigos2(ns)=val(wcols(2))
	   wigos3(ns)=val(wcols(3))
	   wigos4(ns)=trim(wcols(4))
	   
	    auxc=wcols(4)
	    do x1=1,len_trim(auxc)
	     if (ichar(auxc(x1:x1))<32) then 
	        print *,"Unknown error: ichar=",ichar(auxc(x1:x1))
	        auxc(x1:x1)="."
		print *,"[",trim(auxc),"]"
	        wigos4(ns)=trim(auxc)
	    end if
	   end do  
       !}
        IndexNbr(ns) = val(cols(6))
        StationName(ns)=cols(8)
       !{ Convert latitude 
          auxc=cols(9)
	  call sep_natnum(auxc,Nat,nNat)
	  lat(ns)=real(Nat(1))+real(Nat(2))/60.0+real(nat(3))/60.0/60.0
	  if (index(auxc,"S")>0) lat(ns)=lat(ns)*(-1.0)
       !}
       !{ Convert longitude
          auxc=cols(10)
	  call sep_natnum(auxc,Nat,nNat)
	  lon(ns)=real(Nat(1))+real(Nat(2))/60.0+real(nat(3))/60.0/60.0
	  if (index(auxc,"W")>0) lon(ns)=lon(ns)*(-1.0)
       !}
       
      write(2,200)wigos1(ns),wigos2(ns),wigos3(ns),wigos4(ns),IndexNbr(ns),lat(ns),lon(ns),StationName(ns) 
      !write(*,200)wigos1(ns),wigos2(ns),wigos3(ns),wigos4(ns),IndexNbr(ns),lat(ns),lon(ns) 
      goto 10
 99 continue
   close(1)

 200 format (1x,I1,2('-',I5.5),'-',a16,1x,i5.5,2(1x,f11.6),1x,a)   
end program
