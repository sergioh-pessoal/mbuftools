program tbldump
!*******************************************************************************
!*                                 TBLDUMP                                     *
!*                                                                             *
!* Descarrega tabelas BUFR de um arquivo BUFR e salva em formato texto         *
!* (Dump BUFRTables from a BUFR file to a text format file)                    * 
!*                                                                             * 
!*                                                                             *
!*      Copyright (C) 2009  Sergio Henrique S. Ferreira  - SHSF                *
!*                          Waldenio Gambi de Almeida                          *
!*                                                                             *
!*      MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                              *
!*                                                                             *
!*-----------------------------------------------------------------------------*
!* DEPENDENCIAS: MBUFR-ADT, GetArgs.                                           *  
!*******************************************************************************
! Nota:
!  Esta ferramenta foi desenvolvida para permitir a extracao das tabelas 
!  BUFR embutidas nos  arquivos prepBUFR do NCEP e posterior decodificacao 
!  dos dados..
!------------------------------------------------------------------------------- 
! HISTORICO:
!  shsf 20150427 - Feito uma revisao do programa  e acrescentado a condicao 
!                  tbltype(1)%center=any para chamada do modulo MBUFR.  Esse 
!                  parametro foi acrescentado nas versos do MBUFR mais recente
!                  e nao estava sendo utilizada por esse programa, causando 
!                  erro na selecao dos tipos de BUFR que contem tabelas 
!                  

 USE mbufr
 !USE msflib  ! Para compilacao em Windows ( Microsoft Power Station )
 use stringflib 
 use mwritetxt

implicit none

!{ Declaracao das variaveis utilizadas em read_mbufr 
  integer                      ::nss
  type(sec1type)               ::sec1
  type(sec3type)               ::sec3 
  type(sec4type)               ::sec4
  type(selecttype),dimension(1)::tbltype
  integer                      ::NBYTES,BUFR_ED 
  integer                      ::err
  character(1)                 ::SN
!} 
!{ variaveis auxiliares do programa principal 
  integer               ::i
  integer*2             ::argc
  integer               ::iargc
  character(len=255)    ::infile,outfile, cver,ccode,lver,clocal
  integer               ::icode,iver,ilocal
  character(len=255)    ::btable          !Nome e caminho do arquivo da tabela B (B0000000000.txt)
  character(len=255)    ::atable          !Nome e caminho da tabela auxiliar   A (A0000000000.txt)
  character(len=255)    ::dtable          !Nome e caminho do arquivo da tabela D (D0000000000.txt)
  integer               ::nmm             !Numero maximo de mesagens  
  integer               ::nm              !Numero de mensagens bufr
  integer               ::icod
  logical               ::perr            !Se false Nao imprime secao 4 quando  houver erro de decodificacao
  integer,parameter     ::verbose=1
 !}
 !{ Identificacao das tabelas BUFR que serao editadas
  integer               ::edittab_center  ! Centro gerador
  integer               ::edittab_master  ! Versao da tabela mestre
  integer               ::edittab_local   ! Versao da tabela local
  character(len=255)    ::edittabb,edittabd
 

!}

 ! Inicio do programa 
 !{ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida
    
 	argc =  iargc()	
	perr=.false.

	call getenv('MBUFR_TABLES',mbufr_tables)
        !{ Acrescenta barra no final do diretorio local_tables, caso seja necessario
	! Nesse processo veirifica se o diretorio contem barras do windows ou barra do linux 

	i=len_trim(mbufr_tables)
	if ((mbufr_tables(i:i)/=char(92)).and.(mbufr_tables(i:i)/="/")) then 
		if (index(mbufr_tables,char(92))>0) then 
			mbufr_tables=trim(mbufr_tables)//char(92)
		else
			mbufr_tables=trim(mbufr_tables)//"/"
		end if
	end if
	nmm=0
	nss=0
	tbltype(1)%btype=11; tbltype(1)%bsubtype=any;tbltype(1)%center=any
	dtable=trim(mbufr_tables)//"D0000000000.txt"
	btable=trim(mbufr_tables)//"B0000000000.txt"
	atable=trim(mbufr_tables)//"A0000000000.txt"
	outfile=trim(mbufr_tables)//"X0000000000.txt"

        print *,"-------------------------------------------------------"
        print *," INPE-TBLDUMP : DUMP BUFR tables from a BUFR file" 
        print *," Version 1.3 "
        print *," Include MBUFR-ADT module ",MBUFR_VERSION
        print *,"--------------------------------------------------------"
	
       if ((argc>=1).and.(argc<=3)) then
         i=1;call GetArg(i,infile)
         if (argc>1) then 
             i=2;call GetArg(i,ccode)
         else 
             ccode="007"
         end if
         if (argc>2) then 
             i=3;call GetArg(i,cver)
         else
             cver="13"
        end if 
         if (argc>3) then 
             i=4;call GetArg(i,lver)
         else
             lver="1"
        end if 

        
         print *," Input filename: ",trim(infile)
         print *," Output filename: ",trim(outfile)
         print *," Max number of mensagens: ",nmm
         print *," Max number of subsets: ",nss
      else 
		print *,"use:"
		print *,""
		PRINT *," tblbufr infile {centre} {version} {localVersion}"
		print *,""
		print *,"          infile = Bufr input file name "
                print *,"          centre = Generate center code (default=007)"
                print *,"          version = Version of BUFR table (default=13)"
		stop
      endif
	!}
  
!---------------
! Processamento
!--------------
   	call init_mwritetxt
	call INIT_MBUFR(verbose,.false.)
	NBYTES = 0
	Call OPEN_MBUFR(1, infile,46,31,0)  
	open(3,file=outfile,status="unknown")
	open(30,file=atable,status="unknown")
	open(40,file=btable,status="unknown")
	open(50,file=dtable,status="unknown")
	nm=0



!{ Processa a leitura de cada uma das mensagens do arquivo aberto.

 
10    CONTINUE

	nm=nm+1

	Call READ_MBUFR(1, sec1,sec3,sec4, bUFR_ED, NBYTES,err,tbltype)
	If ((NBYTES > 0).and.(IOERR(1)==0)) Then
            
		if((err==0).or.(err>20)) then 
			
			write(3,'(1X,a,i2)')":BUFR: # EDITION =",BUFR_ED
			write(3,'(1X,I4," # MBUFR Error code")')err
			call write_sec1txt(3,sec1)

			!if ((sec3%nsubsets>nss).and.(nss>0))  sec3%nsubsets=nss
			call write_sec3txt(3,sec3)

		end if

		if((err==0).and.(sec1%btype==tbltype(1)%btype)) then    
		     ! print *,"Btype=",sec1%btype  	
	              call write_sec4txt(3,sec4,sec3%nsubsets)
                      write(3,'(1x,a)')":7777:"
               end if
      
           !deallocate(sec3%d,sec4%r,sec4%d,sec4%c)
           if (((nm<nmm).or.(nmm<=0)).and.(sec1%btype==11)) goTo 10
	
	end if
	
	  icode =sec1%center
	  iver=sec1%vermastertable
	  ilocal=sec1%verlocaltable

  !}

 call Close_mbufr (1)
 close (2)
 close (3)
 close (30)
 close (40)
 close (50)
!}

! Atualiza tabela B


print *,":TBLDUMP: The follow tables will be updated"
!sec1%center=val(ccode)
!sec1%vermastertable=val(cver)
!sec1%verlocaltable=0
sec1%center=icode
sec1%vermastertable=iver
sec1%verlocaltable=ilocal

write(*,'(1x,":TBLDUMP:Center=",i3," Master table=",i3," Local table=",i3)')sec1%center,sec1%vermastertable,sec1%verlocaltable
print *,"--------------------------------------------"
!print *,"Deseja prosseguir com a concatenacao ? [S/N]"
!read(*,*)SN

!if (UCASES(SN)=="S")  then 
  call merge_tabb2(btable, sec1%center,sec1%vermastertable,sec1%verlocaltable)
  call merge_tabd(dtable, sec1%center,sec1%vermastertable,sec1%verlocaltable)
!end if
print *,"tbldump: Processing completed"

End 
