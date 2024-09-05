program bufrascii
!------------------------------------------------------------------------------!
!BUFR2CSV| DECODIFICA DADOS EM FORMATO BUFR                             | CPTEC|
!------------------------------------------------------------------------------|
!                                                                              !
! THIS PROGRAM READ BUFR FILES OF OBSERVATION DATA                             !
! AND WRITES THE DATA AND DESCRIPTORS IN AN EXCEL COMPATIBLE ASCII FILE        !
! Este programa le um arquivo bufr de dados observacionais                     !
! e lista os descritores e dados correspondentes em uma tabela                 !
!                                                                              |
! Lucas Moreira de Araujo Gonçalves (LMAG)                                     !
! Sergio H. S. Ferreira (SHSF)                                                                             !
!------------------------------------------------------------------------------!
!DEPENDENCIAS: MBUFR-ADT                                                       !
!------------------------------------------------------------------------------!
!Historico 
!  25/08/2015 SHSF. Atualizado comando de escrita para compatibilizacao com novas
!                   vercoes do fortran. Utilizando get_name_mbufr para obter nome
!                   das variaveis BUFR
!                     
!  2019  -          This program was converted in a program for deconde in .cvs 
!                   format 
 USE mbufr
 use stringflib
 !USE msflib  ! FOR USE WITH MICROSOFT POWER STATION/Para compilacao em Windows ( Microsoft Power Station )
implicit none

!{ Declaracao das variaveis utilizadas em read_mbufr 
  integer :: nss
  type(sec1type):: sec1
  type(sec3type):: sec3 
  type(sec4type):: sec4
  integer       :: NBYTES,BUFR_ED 
  integer       :: err
  
!}

!{ AUXILIARY VARIABLES OF MAIN PROGRAM/variaveis auxiliares do progrma principal
  integer                             ::i,f,J,nsubsets
  integer*2                           ::argc
  integer                             :: iargc
  character(len=255)::infile,outfile,outfile1,outfile2,auxc

  integer                             ::nmm !MAXIMUM NUMBER OF MESSAGES/Numero maximo de mesagens
  integer                             ::nm  !NUMBER OF BUFR MESSAGES/Numero de mensagens bufr
  integer*2                           ::numchar
  integer                             ::icod
  integer                             ::father_code ! BUFR code after the reaplication factor
  integer                             ::first_son_code
  integer                             ::replication_factor
  integer                             ::replication_index
  character(len=50)                   ::ncod
  character(len=50),dimension(0:99999)::tabncod
  character(len=50),dimension(0:255)::tabA,tabCC1
  character(len=255)::mbufr_tables,mbufr_tableA,mbufr_tableB,mbufr_commonTableC1
  character(len=255)::TXT,TXT2
  character(len=258)::AUXTXT
  character(len=50),dimension(1:11) :: Mat
  integer :: icenter
  integer :: imaster_table
  integer :: ilocal_table
  logical :: exists
  real                                ::null  ! Valor indefinido 
!}

 null=undef()

 ! PROGRAM START/Inicio do programa
 !{ CAT THE INPUT ARGUMENTS: DATE, INPUT FILE NAME AND OUTPUT FILE NAME/ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida
    
    
	AUXTXT=""
	argc =  iargc()

        if ((argc>=3).and.(argc<7)) then
		print *,"----------------------------------------"
		print *," INPE BUFR2CSV : Decode FM94 BUFR files "
		print *,"----------------------------------------"

		i=1;call GetArg(i,infile)
		i=2;call GetArg(i,auxc)
		read(auxc,*)nmm
		i=3;call GetArg(i,auxc)
		read(auxc,*)nss
		print *," Input filename: ",trim(infile)
		print *," Max number of mensagens: ",nmm
		print *," Max number of subsets: ",nss
		  
		  
		if (argc>=4) then 
			call GetArg(4,auxc)
			read(auxc,*)icenter
		end if


		  if (argc>=5) then  
			call GetArg(5,auxc)
			read(auxc,*)imaster_table
		end if

		if (argc==6) then  
			call GetArg(6,auxc)
			read(auxc,*)ilocal_table
		end if

        else
		print *,"----------------------------------------------"
		print *," INPE BUFR2CSV: Decode  FM94 BUFR files "
		print *," Include MBUFR-ADT module ",MBUFR_VERSION
		print *,"----------------------------------------------"
		print *,"use:"
		print *,"-------------------------------------------------------------------------"
		PRINT *," bufr2csv infile nmessages nsubsets {center} {master_table} {local_table}"
		print *,"           infile = Bufr input file name "
		print *,"           nmessages = Maximum number of messagens "
		print *,"           nsubsets = Maximum number of subsets per messages   "
			print *,"           center = Identification of Original/Generate Center [",icenter,"]"
			print *,"           master_table = Master Table Version  [",imaster_table,"]"
			print *,"           local_table = Local table version  [",ilocal_table,"]"
            print *,"--------------------------------------------------------------------------"



                 stop
        endif
	!}
	!{ READ BUFR OBSERVATION TYPES FROM TABLE A/Ler os tipos de observaçoes BUFR da tabela A
	tabA(:)=""
	call getenv('MBUFR_TABLES',mbufr_tables)
  
	!{ ADD "\" OR "/" AT THE END OF PATH NAME, IF IT WAS NECESSARY/Acrescenta barra no final do diretorio local_tables, caso seja necessario
	! VERIFY THE SYSTEM AND CHOOSE "\" IF SYSTEM IS THE WINDOWS AND CHOOSE "/" IF SYSTEM IS THE LINUX/Verifica se o diretorio contem barras do windows ou barra do linux 

	i=len_trim(mbufr_tables)
	if ((mbufr_tables(i:i)/="\").and.(mbufr_tables(i:i)/="/")) then 
		if (index(mbufr_tables,"\")>0) then 
			mbufr_tables=trim(mbufr_tables)//"\"
		else
			mbufr_tables=trim(mbufr_tables)//"/"
		end if
	end if
	!}
	
	mbufr_tableA=trim(mbufr_tables)//"BufrTableA.txt"
	open(2,file=mbufr_tableA,status="unknown")

551      read(2,'(i3,1x,a50)',end=661)icod,ncod
         tabA(icod)=ncod
         goto 551

661 continue
    close(2)

!}

!{ READ DESCRITOR'S NAME OF TABLE B/Ler o nome dos descritores da tabela B
	write(mbufr_tableB,'("B000",I3.3,I2.2,I2.2,".txt")')icenter,imaster_table,ilocal_table
        mbufr_tableB=trim(mbufr_tables)//trim(mbufr_tableB)
        open(2,file=mbufr_tableB,status="unknown")
        tabncod(:)=""
555     read(2,'(1x,i6,1x,a50)',end=666)icod,ncod
        tabncod(icod)=ncod
        goto 555

666     continue
        close(2)
!}
!{ READ DESCRITOR'S NAME OF COMMON TABLE C1/Ler nome dos descritores da tabela comum C1

        mbufr_commonTableC1=trim(mbufr_tables)//"CommonTableC1.txt"
        open(2,file=mbufr_commonTableC1,status="unknown")
        tabCC1(:)=""
553     read(2,'(i3,1x,a)',end=663)icod,ncod
        tabCC1(icod)=ncod
        goto 553

663      continue
        close(2)
!}

!{ PROCESS THE DATA READING FOR EACH NF INPUT FILES/Processa a leitura dos dados para cada um dos nf arquivos fornecidos

        NBYTES = 0
        Call OPEN_MBUFR(1, infile)
        !open(3,file=outfile,status="unknown")
        nm=0
	father_code=0
	first_son_code=0
	replication_index=0

!{ READ MESSAGES FROM EACH OPENED FILE/Processa a leitura de cada uma das mensagens do arquivo abertor


10    CONTINUE
	father_code=0
	replication_index=0
        nm=nm+1
        Call READ_MBUFR(1,sec1,sec3,sec4, bUFR_ED, NBYTES,err)

        If ((NBYTES > 0).and.(IOERR(1)==0)) Then

                write(outfile1,'("bufr_",I3.3,I2.2,2I3.3)')sec1%center,sec1%bType,sec1%bsubtype,sec1%VerMasterTable
                write(outfile2,'(I3.3,I4.4,4I2.2,".csv")')sec1%VerLocalTable,sec1%year,sec1%month,sec1%day,sec1%hour,sec1%minute
		outfile=trim(outfile1)//trim(outfile2)
		
		INQUIRE (FILE = outfile, EXIST = exists)
		open(3,file=outfile,ACCESS="append",status="unknown")

		nsubsets=sec3%nsubsets
		
		if(err>=0) then
			if ((nsubsets>nss).and.(nss>0)) nsubsets=nss
				! #### FIND REPLICATIONS
				do i=1,sec4%nvars
					if(sec4%d(i,1)==31001) then
						father_code=sec4%d(i+1,1)
						exit 
					end if
				end do	
		
				!#############WRITE DESCRIPTOR'S NAME/IMPRESSAO DO NOME DOS DESCRITORES##############
				if (.not. exists) then 
					replication_index=0
					do i=1,sec4%nvars
				
                			if (sec4%d(i,1)/=null) then
						
                        			if ((sec4%d(i,1)<99999).and.(sec4%d(i,1)>0)) then
                                			txt=get_name_mbufr(sec4%d(i,1))!tabncod(sec4%d(i,1))
                                		else
                                			txt=""
                                		end if
					end if
			
					if ((sec4%C(i,1)==0).or.(sec4%C(i,1)==1)) then	
						if (sec4%d(i,1)==father_code) then
							if (replication_index==0) then 
								write(3,'(a)')"" 
								write(3,'(a,",")',advance='no')trim(txt)
								replication_index=replication_index+1
							end if
						elseif (i/=sec4%nvars) then
							write(3,'(a,",")',advance='no')trim(txt)
						else
							write(3,'(a)')trim(txt)
						end if
					end if
				end do
	


       
      				!#############WRITE DESCRIPTOR/IMPRESSAO DOS DESCRITORES######################
				replication_index=0	
				do i=1,sec4%nvars
				
					if ((sec4%C(i,1)==0).or.(sec4%C(i,1)==1)) then
						if (sec4%d(i,1)==father_code) then
							if (replication_index==0) then 
								write(3,'(a)')"" 
								replication_index=replication_index+1
								write(3,'(i6.6,",")',advance='no')sec4%d(i,1)
							end if
						elseif (i/=sec4%nvars) then 
							write(3,'(i6.6,",")',advance='no')sec4%d(i,1)
						else
							write(3,'(i6.6,",")')sec4%d(i,1)
						end if				
					end if
				end do
				
			 end if

				!##############WRITE VALUES/IMPRESSAO DOS VALORES#######################
                       numchar=0
		       do j=1,nsubsets
                              do i=1,sec4%nvars
					

					if (sec4%C(i,j)>0) numchar=numchar+1
55					if (numchar>0) then
                                                        if (sec4%c(i,j)==numchar) then
                                                                IF (numchar>255) numchar=255
                                                                auxtxt(numchar+1:numchar+1)=char(int(sec4%r(i,j)))
                                                                txt2=txt
                                                        else

                                                                if (LEN_TRIM(AUXTXT)>0) then
                                                                write(3,'(a,",")',advance='no')trim(auxtxt)//(" ")
								else
								write(3,'("@@@@@@@;")',advance='no')
                                                                end if
                                                                numchar=sec4%c(i,j)
                                                                auxtxt=""
                                                                goto 55
                                                         end if




					else 

                                                if(sec4%r(i,j)/=null) then
							if (i==sec4%nvars) then
								write(3,'(F10.2)')sec4%r(i,j)
							else
								write(3,'(F10.2,",")',advance='no')sec4%r(i,j)
							end if

                                                elseif (sec4%d(i,j)<99999) then

							if (i==sec4%nvars) then
								write(3,'(a6)')"Null"
							else
								write(3,'(a6,",")',advance='no')"Null"
							end if

                                                else
							if (i==sec4%nvars) then
						    		write(3,'(a6)')"Null"
							else
								write(3,'(a6,",")',advance='no')"Null"
							end if
                                                end if

                                        end if

                                end do
                        end do
                end if

        deallocate(sec3%d,sec4%r,sec4%d,sec4%c)
                if ((nm<nmm).or.(nmm<=0)) goTo 10

        end if


  !}

 call Close_mbufr (1)
 close (2)
 close (3)
!}

End


