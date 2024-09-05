!*******************************************************************************
!*                                 BUFRQC					*
!*																			   *
!* Decodifica dados meteorologicos em BUFR e lista os dados decodificador      *
!* junto com os indice de confiabilidade 									   *
!*                                                                             *                                                                             *
!*                                                                        	   *
!*      Copyright (C) 2005 	Sergio Henrique S. Ferreira                        *
!* 																			   *
!*      MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                              *
!*                                                                             *
!*-----------------------------------------------------------------------------*
!* DEPENDENCIAS: MBUFR-ADT, GetArgs.                                           *  
!*******************************************************************************


program bufrqc


 USE MBUFR
 !USE MSFLIB  ! Para compilacao em Windows ( Microsoft Power Station )
 USE MFORMATS

 implicit none

!{ *  Declaracao das variaveis utilizadas em read_mbufr 
	integer :: nss
	type(sec1type)::sec1
	type(sec3type)::sec3
	type(sec4type)::sec4
	integer :: NBYTES,BUFR_ED 
	integer :: err
	Real,parameter	:: Null=-340282300      !valor nulo 
!} 

!{ *  Declaracao das variaveis utilizadas em format_qc
	
	type(sec4qctype)::sec4qc
!}




!{ variaveis auxiliares do progrma principal 
  integer ::i,f,J,nsubsets
  integer*2 ::argc
  integer :: iargc
  character(len=255)::infile,outfile,auxc
  integer :: nmm ! Numero maximo de mesagens  
  integer :: nm  ! Numero de mensagens bufr
  integer::icod
  character(len=50)::ncod
  character(len=50),dimension(0:99999)::tabncod
  character(len=50),dimension(0:255)::tabA,tabCC1
  character(len=255)::mbufr_tables,mbufr_tableA,mbufr_tableB,mbufr_commonTableC1
  character(len=255)::TXT
!}

 !{ Inicio do programa 
  !{ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida
	argc =  iargc()	
	
	
	if ((argc==4)) then
	  print *,"+------------------------------------+"
	  print *,"|               BUFRQC               |"
  	  print *,"+------------------------------------+"
	  print *,""
           i=1;call GetArg(i,infile)
	   i=2;call GetArg(i,outfile)
	   i=3;call GetArg(i,auxc)
	   read(auxc,*)nmm
	   i=4;call GetArg(i,auxc)
	   read(auxc,*)nss  
 
          print *,"Reading... ",infile
	else
	    print *,"+--------------------------------------------------------+"
	    print *,"| bufrqc - show n subsets of m messages                  |"
	    print *,"| USE:      bufrqc infile outfile nmessages nsubsets     |"
	    print *,"+--------------------------------------------------------+"
	    print *,"|    infile = Bufr input file name                       |"
	    print *,"|    outfile= text output filename                       |"
	    print *,"|    nmessages = Maximum number of messagens to dump     |"
	    print *,"|    nsubsets = Maximum number of subsets per messages   |"
	    print *,"+--------------------------------------------------------+"
		 stop
	endif
  !}
 
!{ * Ler nome dos descritores da tabela A

	call getenv('MBUFR_TABLES',mbufr_tables)
	if ((mbufr_tables(i:i)/=char(92)).and.(mbufr_tables(i:i)/="/")) then 
		if (index(mbufr_tables,char(92))>0) then 
			mbufr_tables=trim(mbufr_tables)//char(92)
		else
			mbufr_tables=trim(mbufr_tables)//"/"
		end if
	end if

	mbufr_tableA=trim(mbufr_tables)//"BufrTableA.txt"
	open(2,file=mbufr_tableA,status="unknown")
	 
551	 read(2,'(i3,1x,a50)',end=661)icod,ncod
	 tabA(icod)=ncod
	 goto 551
	
661 continue
    close(2)

!}
!{ Leitura do nome dos descritores da tabela B

	mbufr_tableB=trim(mbufr_tables)//"B0000461200.txt"
    open(2,file=mbufr_tableB,status="unknown")
	tabncod(:)=""
555	read(2,'(1x,i6,1x,a50)',end=666)icod,ncod
	 tabncod(icod)=ncod
	 goto 555
	
666 continue
	close(2)

!{ Leitura do nome dos descritores da tabela comum C1

	mbufr_commonTableC1=trim(mbufr_tables)//"CommonTableC1.txt"
	open(2,file=mbufr_commonTableC1,status="unknown")
	 tabCC1(:)=""
553	 read(2,'(i3,1x,a50)',end=663)icod,ncod
	 tabCC1(icod)=ncod
	 goto 553
	
663 continue
    close(2)

!}
 
!{ Leitura dos dados para cada um dos nf arquivos fornecidos


      

    NBYTES = 0
    
    Call OPEN_MBUFR(1, infile)  
	open(3,file=outfile,status="unknown")
	nm=0
  
  !{ Leitura de cada uma das mensagens do arquivo abertor >
   
 
10    CONTINUE        
      nm=nm+1
     
      
      Call READ_MBUFR(1,sec1,sec3,sec4, bUFR_ED, NBYTES,err)
	
	
	
	  	   
    If ((NBYTES > 0).and.(IOERR(1)==0)) Then
	   	nsubsets=sec3%nsubsets
	    call format_qc(sec4,nsubsets,sec4qc)

		write(3,'(1X,"MBUFR Error code",i4)')err
		write(3,'(1X,"CENTER: ",i4," - ",a50)')sec1%center,tabcc1(sec1%center)
		write(3,'(1X,"BUFR TYPE: ",i4," - ",a50)')sec1%bType,tabA(sec1%btype)
		write(3,'(1X,"BUFR SUBTYPE ",i4)')sec1%bsubtype
		write(3,'(1X,"DATE ",i4,"-",i2.2,"-",i2.2,2X,i2,":",i2.2)')sec1%year,sec1%month,sec1%day,sec1%hour,sec1%minute
	
		
		
		if(err>=0) then    

			if (nsubsets>nss) nsubsets=nss
			
			do j=1,nsubsets

				write(3,'(1x,"-----------------------------------------------------------------------------")')
				write(3,'(1x,"SUBSET = ",i4)')j
				write(3,'(1x,"BUFRCOD                         NAME                            VALUE      QC")')
				write(3,'(1x,"------ ------------------------------------------------------ ----------  ---")')
				do i=1,sec4qc%obs%nvars
					
					if (sec4qc%obs%d(i,j)/=null) then 
						if (sec4qc%obs%d(i,j)<99999) then 
						   txt=tabncod(sec4qc%obs%d(i,j))
						else
						   txt=""
						end if
						   
						if((sec4qc%obs%r(i,j)/=null).and.(sec4qc%qc(i,j)/=null)) then 
							write(3,'(1x,i6.6,1X,a50,F15.4,1x,i3)')sec4qc%obs%d(i,j),txt,sec4qc%obs%r(i,j),sec4qc%qc(i,j)
						elseif(sec4qc%obs%r(i,j)/=null) then 
							write(3,'(1x,i6.6,1X,a50,F15.4)')sec4qc%obs%d(i,j),txt,sec4qc%obs%r(i,j)
						elseif (sec4%d(i,j)<99999) then  
							write(3,'(1x,i6.6,1X,a50,a15)')sec4%d(i,j),txt,"Null"
						else 
						    write(3,'(1x,i6.6)')sec4%d(i,j)
						end if 
					end if
	    
				end do
			end do
			
		end if
		 !deallocate(sec3%d,sec4%r,sec4%d,sec4%c)
		if (nm<nmm) goTo 10
	
	end if
		
  !}

 Close (1)
 close (2)
 close (3)
!}


End 
