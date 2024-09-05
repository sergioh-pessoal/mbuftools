program bufrsplit
!>------------------------------------------------------------------------------|
!!BUFRSPLIT |SEPARA AS MENSAGENS BUFR DE UM ARQUIVO EM MULTIPLOS ARQUIVOS | SHSF|
!!------------------------------------------------------------------------------|
!!                                                                              |
!! THIS PROGRAM SPLIT  BUFR MESSAGE FROM A FILE AND SAVE IT IN MANY DIFERENT    |
!! FILES                                                                        |
!!                                                                              |
!<------------------------------------------------------------------------------|
!DEPENDENCIAS: MBUFR-ADT                                                        |
!-------------------------------------------------------------------------------|
! SHSF 20170606 -Corrigido bug no dimensionamento do array rg(:,:)
! SHSF 20180626 -Included date and time indentification by telecommunicatioin header
! SHSF 20180706 -Included initial date parameter 
! SHSF 20190222 -Selection by telecommunition header was included. 
!               - Fix a but in the selectio of message. The ending header was not included
! SHSF 20190322 - Duplicate header removal was included
! SHSF 20200401 -Messages when telecomunication headers are not informed were checked
! SHSF 20210308 -Include the option K to do not include Tel.Header in the output files
! SHSF 20230512 -The option n=4 has been included
USE mbufr
use stringflib
use datelib
!USE msflib  ! FOR USE WITH MICROSOFT POWER STATION/Para compilacao em Windows ( Microsoft Power Station )

implicit none

!{ DECLARATION OF VARIABLES USED BY READ_MBUFR/Declaracao das variaveis utilizadas em read_mbufr
  type(octtype)                   ::bufrmessage
  type(sec1type)                  ::sec1
  integer                         ::BUFR_ED
  integer                         ::err
  character(len=40)               ::header,header2 !Telecommunications header (40 bytes)
  character(len=40),dimension(10000)::header_list
  integer                          ::nheader_list
  integer,parameter                ::nargmax=600
!}

!{ AUXILIARY VARIABLES OF MAIN PROGRAM/variaveis auxiliares do progrma principal
  integer,dimension(0:1000,0:1000,-100:10) ::rg    ! Rg position (centers,Bufr Types, timesteps)
  integer,dimension(0:999)                   ::lcenter          !List of generating centres
  integer                                    ::ncenter          !Number of elements in lcenter
  integer,dimension(0:999)                   ::rcenter          !Region identification of a center in lcenter
  character(len=2)                           ::Region_Id
  character(len=1),dimension(nargmax)  ::argname
  character(len=255),dimension(nargmax)::arg
  integer                         :: narg
  integer                         :: i,rr,X1,X2,I1,I2,I3,j,l            
  integer                         :: NM ! Number of message
  integer                         :: NP ! Number of prepbufr file 
  integer                         :: PTYPE ! Previous BUFR type
  character(len=255)              :: infile,outfile,basefile,out1,basefile0

  real*8                          ::cdate
  real*8                          ::date_min   !It is the date menus 1 timestep 
  real*8                          ::date_max
  real*8                          ::rdate1     ! data de referencia (procedente do header e/ou do sistema)	
  real*8                          ::obs_date

  character(len=8)                ::header4    ! = hvalues(4)  (Dia e hora do header de telecomunicacoes) 
  character(len=10)               ::hdate0     ! Data fornecida ou do sistema para verificar o header
  character(len=10)               ::hdate0_min ! hdate0-1dia 
  character(len=10)               ::hdate1     ! <hdate0> Interception <header4>
  character(len=8),dimension(10)  ::hvalues    !Vector with header telecommunications values 
  integer                         ::nhvalues   !Number of elements in header
  CHARACTER(len=255)              ::local_tables 


  character(len=10)               ::cdate_min  !Combine provided date (1) with Date from telecomunication header (2) 
  character(len=10)               ::ccdate,cccdate !Data e hora do arquivo
  character(len=10)               ::cobs_date
  integer                         ::hsin     ! Synoptic time 
  integer                         ::hsin_inc ! Synoptic time_step 
  integer                         ::itime    ! Time index - Index relative to hsin and hsin_inc
  integer                         ::itime_max! The Last itime of the day
  integer                         ::itime_min! The first itime (previous time_step = -1)
  integer                         ::days_before  ! num of days before = 0
  integer                         ::nmax     ! Maximum number of message
  integer                         ::opsplit
  logical                         ::dup      ! .true. if header is duplicated
  logical                         ::rdup     ! .true. remove duplication function is ON
  logical                         ::rh       ! .true. remove telecomunication header in the output file 
  logical                         ::ox
  integer                         ::fx
  logical                         ::append
  logical                         ::ext
  integer                         ::msgct1=0
   character(len=255),dimension(nargmax)  ::flist              !Lista com nome dos arquivos 
   integer                            ::nf,xf
   character(len=255)                 ::txt
   !character(10) :: stime
   !character(5)  :: zone
   integer,dimension(8) :: sdate
   ! using keyword arguments
    
!}



 ! PROGRAM START/Inicio do programa
 !{ CAT THE INPUT ARGUMENTS: DATE, INPUT FILE NAME AND OUTPUT FILE NAME/ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida
       X1=0; X2=0
       msgct1=0
       append=.false.
       nf=0
       nmax=0
       date_min=0
       opsplit=0 
       hdate0=""
       itime_min=-1
       days_before=0
	rdup=.false.
	hsin_inc=6
	rh=.false.
       call getarg2(argname,arg,narg)
       	call getenv("MBUFR_TABLES",local_tables)
	print *,narg
	if (narg>nargmax) then 
	  print *,":BUFRSPLIT: Error: number of arguments grater than ",nargmax
	  stop
	end if 
	print *,"MBUFR_TABLES=",trim(local_tables)

       print *,narg
      do i=1,narg
        if (argname(i)=="o") then 
          basefile0=arg(i)
          x2=1
        elseif (argname(i)=="n") then 
          if (trim(arg(i))=="1") opsplit=1  
	  if (trim(arg(i))=="2") opsplit=2
	  if (trim(arg(i))=="3") opsplit=3
          if (trim(arg(i))=="4") opsplit=4
	  if (trim(arg(i))=="5") opsplit=5
	elseif (argname(i)=="h") then 
	  if (len_trim(arg(i))==8) then 

	     hdate0=trim(arg(i))
	     
	  end if
         elseif (argname(i)=="r") then 
		rdup=.true.
	 elseif (argname(i)=="w") then 
		hsin_inc=val(arg(i))
	elseif (argname(i)=="k") then 
		rh=.true.
	elseif (argname(i)=="x") then
		nmax=val(arg(i))
	elseif (argname(i)=="a") then
		append=.true.
	elseif (argname(i)=="p") then
		days_before=val(arg(i))
		
	 elseif(argname(i)=="?") then 
			nf=nf+1
			if (nf > 300) then 
				print *,trim(color_text("Warning! The maximum number of provide files is 300. Other files will be ignored", 33, .false.))
				nf=300
				exit
			else 
				flist(nf)=arg(i)
			end if 
			x1=1
         end if
      end do

    !In case of hdate0 not provided, than use system date
    !{      
     if (len_trim(hdate0)==0) then 
        call date_and_time(VALUES=sdate)
	write(hdate0,'(i4.4,2i2.2)')sdate(1),sdate(2),sdate(3)
     end if
    !}
    !{ set date_min and cdate_min
	cdate_min=hdate0(1:8)
	date_min=fjulian(cdate_min)-(real(hsin_inc)/24.0)
	write(hdate0_min,'(i4.4,3i2.2)')year(date_min),month(date_min),day(date_min),hour(date_min)	
    !}

           	print *,"+-----------------------------------------------------------------+"
		print *,"|                   BUFRSPLIT  (2023-10-10)                       |"
                print *,"|        Splits the BUFR messages into different files            |"
		print *,"|          Include MBUFR-ADT module ",MBUFR_VERSION,"     |"
		print *,"+-----------------------------------------------------------------+"
         if (x1*x2==0) then 
              
                print *,"|                                                                 |"
                print *,"| use:   bufrsplit  -o <output>  {-options}  <file_list>          |"
		print *,"|                                                                 |"
		print *,"| output= 'output directory name' or/and 'file output prefix'     |"
		print *,"|                                                                 |"
		print *,"| options:                                                        |"
		print *,"|                                                                 |"
		print *,"| <-n>   = 0 only by times                                        |"
		print *,"|        = 1 by type and times                                    |"
                print *,"|        = 2 by center, type and time                             |"
		print *,"|        = 3 by header or by message                              |"
		print *,"|        = 4 by PREPBUFR file (initialize with category 11 )      |"
		print *,"|        = 5 (=1) + Filter by center in regional_center_codes.txt |"
                print *,"|<-h> Combine date (yyyymmdd) with time in header if necessary    |"
	        print *,"|<-r> Remove duplicated header                                    |"
		print *,"|<-p> Previous Time period (Default = 1 day)                      |"
		print *,"|<-w> time window (Default = 6 hours)                             |"
		print *,"|<-k> Do not inclued the header in the output files               |"
		print *,"|<-x> $ maximum number of message to be processed                 |"
		print *,"|<-a> allow appending data to preexisting file                    |" 
		print *,"+-----------------------------------------------------------------+"
                
                stop
           else 
		print *," :BUFRSPLIT: Initial date set to ",hdate0_min
                print *," :BUFRSPLIT: option= ",opsplit
           endif
  !}


if (opsplit==5) call read_regional_center_codes
	

!{ PROCESS THE DATA READING FOR EACH NF INPUT FILES/Processa a leitura dos dados para cada um dos nf arquivos fornecidos
  rg(:,:,:)=0
  rr=0
  NM=0
  NP=0
  PTYPE=-9
  i1=0
  nheader_list=0
  itime_max=24/hsin_inc  
  itime_min=-1-(days_before*24/hsin_inc)

 
 do xf=1,nf
 	infile=flist(xf)
	write( *,'(1x," :BUFRSPLIT:Infile  (",i3,"/",i3,") ",a)') xf,nf,trim(infile)
	write( *,'(1x," :BUFRSPLIT:Outfile (basename) ",a)') trim(basefile0)
        Call OPEN_MBUFR(1, infile)
! --------------------------------------------------------------------
! READ MESSAGES FROM EACH OPENED FILE
! (Processa a leitura de cada uma das mensagens do arquivo abertor)
!-------------------------------------------------------------------
!{
	
       !open(3,file=trim(basefile)//".lst",status="unknown")
	header2=""
10	CONTINUE

	nm=nm+1
	
	header=""
	
	Call READBIN_MBUFR(1,bufrmessage, bUFR_ED, sec1,err, header)

!
!       In the case of category 11, it counts the number of PREPBUFR files
!
	if (opsplit==4) then 
	if ((ptype/=11).and.(sec1%btype==11)) then 
		NP=NP+1
		rg(1,1,1)=0  
	end if
	end if
	ptype=sec1%btype
!
!     Cheking for duplicated header 
!
	IF (ERR>0) goto 20 
	if (rdup) then 
		dup=.false.
		if (len_trim(header)>0) then
			do l=1,nheader_list
				if (trim(header)==trim(header_list(l))) then 
					dup=.true.
					exit
				end if
			end do
			if (.not.dup) then
				if (nheader_list<size(header_list,1)) then
					nheader_list=nheader_list+1
					header_list(nheader_list)=trim(header)
				else	
				
					print *,trim(color_text(" :BUFRSPLIT: Warning! The number of message excedded the limit",33,.false.)),nheader_list
					
				endif 
			endif 	
		end if

		if ((dup).and.(IOERR(1)==0)) then
			print *,":BUFRSPLIT: header = ",l,trim(header), " <- Duplication was Eliminated"
			close(2)
			goto 10 
		end if        
	end if
	
	
!}

	! Provisorio: assumir header anterior caso nao venha o header)
	if (len_trim(header)>0) then
 		header2=header
	elseif (len_trim(header2)>0) then
		header=header2
	        !print *,":BUFRSPLIT:Warning! More then 1 BUFR message per header =",trim(header)
	end if
     	


       ! Processa a intercessao entre header de comunicacao e hdate0 t
       ! Se o Header existir. Tambem obtem:rdate1 e hdate1
       !{
	
	call split(header,".",hvalues,nhvalues)
	 
	if ((nhvalues>4).and.(nhvalues<10)) then 
		header4=hvalues(4)
		if (header4(1:2)==hdate0(7:8)) then 
			hdate1=hdate0(1:8)//header4(3:6)
 		elseif (header4(1:2)==hdate0_min(7:8)) then
			hdate1=hdate0_min(1:8)//header4(3:6)
 		else
			hdate1="0000000000"
			txt=" :BUFRSPLIT: Warning! date in ["//trim(header)//"] is different than "//trim(hdate0)
			print *,trim(color_text(txt, 33, .false.))	
 		end if 
	else
		hdate1=hdate0
	end if
	rdate1=fjulian(hdate1)

        !}
        

 20	If ((bufrmessage%nocts > 0).and.(IOERR(1)==0)) Then
         !  
         ! Obtendo CDATE (Data e hora sinotica da secao 1 
         !
	 !{  
         
		!int(real(sec1%hour)/real(hsin_inc)+0.5)*hsin_inc
		hsin=sec1%hour+int(real(sec1%minute)/60.0+0.5)                    ! Arredondamento para horas inteiras 
		hsin=get_synoptic_time(hsin)                                      ! Arredondamento para hora sinotica central
		cdate=fjulian(sec1%year,sec1%month,sec1%day,0,0,0)+real(hsin)/24.0!Acrescentando hora sinotica central em decimos de dias
		obs_date=fjulian(sec1%year,sec1%month,sec1%day,sec1%hour,sec1%minute,0) ! Hora da observacao 
	
		
		
         !}
         !  
         ! Trabalhando com data do header (rdate1) 
         ! Comparando CDATE -data da secao1 com RDATE - data do header para obter  CCDATE  a data do arquivo  
         ! 
	 !{  
	 if(rdate1>0) then
 		!IF section1 date is zero than  use header date 
		if (cdate==0) then 
                   ccdate=hdate1
		   cdate=fjulian(ccdate)
                end if 
		
		write(ccdate,'(i4,3i2.2)')year(cdate),month(cdate),day(cdate),hour(cdate)

	elseif (cdate>0) then 
		!If no date in header, but there is date in section 1 than use section1 date
		write(ccdate,'(i4,3i2.2)')year(cdate),month(cdate),day(cdate),hour(cdate)
	else
		!Else there are no way what the date. 
		ccdate="0000000000"
		cdate=0
	end if
       

        !{ Obtendo itime 
	itime=get_timestep(cdate)
	
	!Only for test
        !if (hour(cdate)==12) then
	! write(cobs_date,'(i4,3i2.2)')year(obs_date),month(obs_date),day(obs_date),hour(obs_date)  
	! print *,"cdate,date_min,obs_date,itime,opsplit=",ccdate," ",cdate_min," ",cobs_date,itime,opsplit 
	!end if     	
	      			
	 if ((itime<itime_min).or.(itime>itime_max)) then 
	        txt=" :BUFRSPLIT: Warning! The date = "//trim(ccdate)//" is out of time window. Initial date="//trim(hdate0_min)
		if (msgct1<3) then 
			print *,trim(color_text(txt, 33, .false.))
			msgct1=msgct1+1
		end if
                !ccdate="0000000000"
                !itime=-24
		goto 777
         end if
         !}
                         
	  ! Obtendo nome do arquivo de saida
	  if (index(basefile0,"%")>0) then 
		basefile=basefile0  
 		basefile=replace(basefile,"%y4",ccdate(1:4))
	 	basefile=replace(basefile,"%m2",ccdate(5:6))
	 	basefile=replace(basefile,"%d2",ccdate(7:8))
	 	basefile=replace(basefile,"%h2",ccdate(9:10)) 
	  else
		basefile=basefile0
	  end if 
                      if (opsplit==0) then 
                         write(out1,'("T",a10)')ccdate
                         I1=1
                         I2=1
                         I3=itime
			 !print *,I1,I2,I3,rg(i1,i2,i3),sec1%hour,sec1%minute
                      elseif(opsplit==1) then 
        		write(out1,'("B",i3.3,"_T",a10)')sec1%btype,ccdate
                        i1=1
			I2=sec1%btype
                        I3=itime
		       elseif(opsplit==5) then 
        		i1=check_regional_center(sec1%center)
			I2=sec1%btype
                        I3=itime
			write(out1,'("R",i1,"_B",i3.3,"_T",a10)')i1,sec1%btype,ccdate
		      elseif(opsplit==2) then 
		      	write(out1,'("C",i3.3,"B",i3.3,"_T",a10)')sec1%center,sec1%btype,ccdate
                        I1=sec1%center
                        I2=sec1%btype
                        I3=itime
		      elseif (opsplit==3)  then 
		        write(out1,'("M",i5.5)')NM
			I1=1
		      	I2=1
			I3=1
			rg(i1,i2,i3)=0  ! Sempre reinicia o regiostro porque sera sempre um novo arquivo (sem agrupamento) 
			j=index(header,"...")+3
			if (j>=8) then
				out1=trim(out1)//"_"//header(j:j+17) 
			end if 
			out1=trim(out1)//"_"//trim(ccdate)
		     elseif (opsplit==4)  then 
		        write(out1,'("_M",i5.5)')NP
			I1=1
		      	I2=1
			I3=1
			out1=trim(out1)
                      end if

			outfile=trim(basefile)//trim(out1)//".bufr"
			inquire(file=outfile,EXIST=ox,SIZE=fx)
			if (fx > 0) then 
				if (rg(i1,i2,i3)==0) then
					if(append) then
						txt=":BUFRSPLIT: Warning! Appending data to "//trim(outfile)
						print *,trim(color_text(txt, 33, .true.))
						rg(i1,i2,i3)=fx
					else 
						open(2, file=outfile,status='old')
						close(2,status='delete')
						txt=":BUFRSPLIT: Warning! Rewrinting file "//trim(outfile)
						print *,trim(color_text(txt, 33, .true.))
					end if 
					!print *,"OX=",ox,"FLEN=",fx,trim(outfile)
					!print *,rg(i1,i2,i3)
			
				end if 
			else
				if (rg(i1,i2,i3)==0) then
					txt=":BUFRSPLIT: Opening "//trim(outfile)
					print *,trim(color_text(txt, 32, .true.))
				end if 
				
			end if
					
			open(2,file=outfile,STATUS='unknown',FORM='UNFORMATTED',access='DIRECT',recl=1) 
                       
                        rr=rg(I1,I2,I3)
			
			!if (sec1%center==43) then 
			!if ((rr==0).or.(sec1%center==43)) then
			!	print *," :BUFRSPLIT: -> Outfile=",trim(outfile),sec1%center !," Rg=",rr,"i1=",i1
			!end if
			
                        !Write telecommunication header
                        !{
                         if ((nhvalues>4).and.(.not.rh))  then
                           call write_header(2,rr,hvalues)
                           rg(i1,i2,i3)=rr
                         end if 
                        ! Write BUFR messages
                        !{
			do i=1,bufrmessage%nocts
                                rr=rr+1
				if (rr<=currentRgmax) then
					write (2,rec=rr) bufrmessage%oct(i)                                
					rg(I1,I2,I3)=rr
				else
					print *,"Error! rg > rgmax", rr,">",currentRGmax,"in Outfile=",trim(outfile)
					exit
				end if 
			end do
                        !}
 777			deallocate(bufrmessage%oct)
			close(2)
		      
			if (nmax>0) then
				if (nm<=nmax) then 
					if (rr<=currentrgmax) goto 10 
				endif
			else
				if (rr<=currentrgmax) goto 10 
			end if
		end if
  !}

 call Close_mbufr (1)
 end do
 close (2)
 close (3)
 print *,trim(color_text(":BUFRSPLIT: Done",32,.true.)) 
 stop
!}
 contains
 function get_timestep(cdate); integer ::get_timestep
	real*8,intent(in)::cdate
	get_timestep=int((cdate-date_min)*24.0/real(hsin_inc))+1
 end function
 
 function get_synoptic_time(hour); integer::get_synoptic_time 
	integer,intent(in)::hour
	get_synoptic_time=int(real(hour)/real(hsin_inc)+0.5)*hsin_inc                  ! Arredondamento para hora sinotica central
 end function 
 
 !-----------------------------------------------------------------------------|
 !read_regional_center_codes |                                                 |
 !-----------------------------------------------------------------------------|
 !-----------------------------------------------------------------------------|
 !Associated routines:
 ! 1) check_regional_center
 !
 
 subroutine read_regional_center_codes
     character(len=1024)::infile
     character(len=256)::line
     integer::i,center
     
     character(len=10),dimension(10)::cols
     integer::ncols
     
     infile=trim(local_tables)//"/regional_center_codes.txt"
     inquire(file=infile, exist=ext)
     if (ext) then 
	open (1,file=infile,status="unknown")
	print *,">>>>",trim(infile)
     else
	print *,":BUFRSPLIT: Error reading 'regional_center_codes.txt'"
	print *,":BUFRSPLIT: File=",trim(infile)
	print *,":BUFRSPLIT: File not fount"
	stop
     end if 
     i=0
     
 500 read(1,'(a)',end=501)line
	i=i+1
	if (i==1) then 
	   Region_id=trim(line)
	else
	   call split(line,",",cols,ncols)
	   if (ncols>=2) then 
	      center=val(cols(2))
	      if (center>0) then
	         ncenter=ncenter+1
	         lcenter(ncenter)=center
		 rcenter(ncenter)=val(cols(1))
	        ! print *,ncenter, lcenter(ncenter),"R",rcenter(ncenter)
	       end if
	   end if
	end if
	
        goto 500	
     
 501 close(1)
 
     print *,trim(infile)
 end subroutine
 !-----------------------------------------------------------------------------|
 !check_regional_center |                                                      |
 !-----------------------------------------------------------------------------|
 !Returns region identification from originating center code                   |
 !-----------------------------------------------------------------------------|
 !Associated routines:
 ! 1) read_regional_center_codes
 !
 function check_regional_center(center_code); integer::check_regional_center
   integer, intent(in)::center_code   
   integer            ::i
   
	check_regional_center=0
	Do i=1,ncenter
	   if (lcenter(i)==center_code) then
	      check_regional_center=rcenter(i)
	      exit
	   end if
	end do
   end function
 
End



