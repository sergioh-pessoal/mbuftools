!>                                 BUFRDUMP                                                                                                               
!!
!! Descarrega dados e descritores de um arquivo BUFR para um arquivo texto.    
!! (Dump data and descriptors from a BUFR file to a text file)                  
!!                                                                              
!!                                                                             
!!      Copyright (C) 2005  Sergio Henrique S. Ferreira  - SHSF                
!!                          Waldenio Gambi de Almeida                          
!!                                                                             
!!      MCT-INPE-Brasil                               
!<                                                                             
!*-----------------------------------------------------------------------------
!* DEPENDENCIAS: MBUFR-ADT, GetArgs.                                             
!******************************************************************************
program bufrdump
!* DEPENDENCIAS: MBUFR-ADT, GetArgs.                                           *  
!*******************************************************************************
! HISTORICO:
! 2005    SHSF - Original version
! 2007-02 SHSF - Update of the MBUFR-ADT V 1.5 module for compilation in 
!                Windows (Microsoft Power Station)
! 2008-12 SHSF - Update of the MBUFR-ADT V 4.0 module and other necessary 
!                modifications for BUFR EDITION 4
! 2010-05-29 SHSF - odified in (* 1) to allow printing of BUFR with error
! 2018-05-23 SHSF - Added filter for variable characters with code between 31 to 127
! 2020-09-21 SHSF - Added option to print telecommunications header 
 USE mbufr
 USE mcodesflags
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
  integer       :: err2
  character(len=40)::header
!}

!{ variaveis auxiliares do programa principal 
  integer                             ::i,J,nsubsets,ii,w
  integer                             ::l4 ! Contador de linhas da secao 4
  character(len=255)                  ::infile,outfile,auxc
  integer                             ::narg
  character(len=1),dimension(10)      ::argname
  character(len=255),dimension(10)    ::arg
  integer                             ::nmm   !Numero maximo de mesagens  
  integer                             ::nm    !Numero de mensagens bufr
  integer*2                           ::numchar,p_numchar
  integer                             ::icod
  character(len=50)                   ::ncod
  character(len=50),dimension(0:99999)::tabncod
  logical,          dimension(0:99999)::codflag
  character(len=255)                  ::path2tables
 
  character(len=255)                  ::TXT,TXT2,txt3
  character(len=258)                  ::AUXTXT
  logical                             ::perr  ! Se false  Nao imprime secao 4 quando  houver erro de decodificacao
  integer                             ::X1,X2 ! Test if all necessary parameters was provided 
  type(selecttype),dimension(1)       ::select! Select data type acording table BUFR A
  logical                             ::selectopt
  logical                             ::prt_header
  integer                             ::wsi  ! if true WIGOS STATION IDENTIFIER IS PRESENT
  logical                             ::wsi_only
  integer                             ::rmk    
  character(len=1)                    ::son
  integer                             ::vrb !Verbosidade
  real                                ::null  ! Valor indefinido 
  logical                             ::exists
  integer                             ::VerMasterTable
!}

 perr=.false.
 null=undef()
 ! Inicio do programa 
 !{ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida
   call getarg2(argname,arg,narg)
     err2=0
     err=0
     nmm=0
     nss=0
     vrb=0
     prt_header=.false.
     wsi_only=.false.
     select(1)%btype=any
     select(1)%bsubtype=any
     select(1)%center=any
     selectopt=.false.
     x1=0
     x2=0
     path2tables=""
     rmk=1
    if ((argname(1)=="?").and.(narg>=4)) then 
      infile=arg(1)
      outfile=arg(2)
      nmm=val(arg(3))
      nss=val(arg(4))
      x1=1
    elseif((len_trim(argname(1))>0).and.(narg>1)) then
     
      do i=1,narg
        if (argname(i)=="i") then !,,,,,,,.....
          infile=arg(i)
          x1=1
        elseif (argname(i)=="o") then !................ Arquivo de saida
          outfile=arg(i)
          x2=1
        elseif (argname(i)=="b") then !...............Tipo de dado de saida
          select(1)%btype=val(arg(i))
          selectopt=.true.
        elseif (argname(i)=="s") then 
          select(1)%bsubtype=val(arg(i))
          selectopt=.true.
        elseif (argname(i)=="c") then 
          select(1)%center=val(arg(i))
          selectopt=.true.
        elseif (argname(i)=="m") then 
          nmm=val(arg(i))
        elseif (argname(i)=="g") then 
          nss=val(arg(i))
        elseif (argname(i)=="p") then !...............Caminho para as tabelas BUFR
          PATH2TABLES=arg(i)
        elseif (argname(i)=="r") then 
          rmk=0
        elseif (argname(i)=="v") then 
          vrb=val(arg(i))
        elseif (argname(i)=="e") then 
          perr=.true.
	elseif (argname(i)=="h") then 
          prt_header=.true.
	elseif (argname(i)=="w") then 
          wsi_only=.true.
        end if
      end do
      x1=x1*x2
    end if

    if (x1==0) then 
      call init_mbufr(vrb,.true.,VerMasterTable)
      print *,"+--------------------------------------------------------+"
      print *,"| INPE BUFRDUMP : Decode FM94 BUFR files                 |"
      print *,"| Version 20.04.2023                                     |"
      print *,"| Include MBUFR-ADT module ",MBUFR_VERSION,"     |"
   write(*,'(" | Version of Master Table :",i3,"                           |")')VerMasterTable
      print *,"+--------------------------------------------------------+"
      print *,"| use:       bufrdump -i infile -o outfile {options}     |"
      print *,"+--------------------------------------------------------+"
      print *,"|            infile = Bufr input file name               |"
      print *,"|            outfile= text output filename               |"
      print *,"+--------------------------------------------------------+"
      print *,"| options:                                               |"
      print *,"|            -b : Select the data type                   |"
      print *,"|            -s : Select the data subtype                |"
      print *,"|            -c : Select the generator center            |"
      print *,"|            -m : Maximum number of messagens            |"
      print *,"|            -g : Maximum number of subsets per messages |"
      print *,"|            -p : path to bufrtables directory           |"
      print *,"|            -r : Don't print comments                   |"
      print *,"|            -e : Print values even with errors          |"
      print *,"|            -h : Print telecommunication header         |"
      print *,"|            -v : (0,1,2,3) Verbose                      |"
      print *,"+--------------------------------------------------------+"
      stop
    else 
      print *,"----------------------------------------------"
      print *," INPE BUFRDUMP : Decode FM94 BUFR files "
      print *,"----------------------------------------------"
      print *,":BUFRDUMP: Input filename: ",trim(infile)
      print *,":BUFRDUMP: Output filename: ",trim(outfile)
      print *,":BUFRDUMP: Max number of mensagens: ",nmm
      print *,":BUFRDUMP: Max number of subsets: ",nss
    endif
   !}
  
 !{  initializes the mcodesflags module and loads the table-A and the table-C1
    call init_mcodesflags(path2tables)

 !}

 
!{ Processa a leitura dos dados para cada um dos "nf" arquivos fornecidos.

    NBYTES = 0
    call INIT_MBUFR(vrb,.true.)

	inquire(FILE = infile, EXIST = exists)
	if (.not. exists) then 
		print *, color_text("BUFRDUMP: Error: File not found -> "//infile, 31, .true.)
		stop
	end if
	
    if(len_trim(path2tables)==0) then  
      Call OPEN_MBUFR(1, infile)  
    else
      call open_mbufr(1,infile,path2tables)
    end if

    open(3,file=outfile,status="unknown")
    nm=0




!{ Processa a leitura de cada uma das mensagens do arquivo aberto.

 
10    CONTINUE
         

!obs(:,:)=null 
! Abaixo sao realizados os seguintes passoes:
!
! c)Dentro de cada mensagem processa cada um dos subsets de dados
! d)Os subsets sao organizados em linhas de dados (nrows)
! e)Conforme o caso uma observacao pode ter uma ou mais linhas
! f) KS contem um numero sequencial que indentifica a observacao
!{
   
    if (.not.selectopt) then
      Call READ_MBUFR(1,sec1,sec3,sec4, bUFR_ED, NBYTES,err,header=header)
    else
      Call READ_MBUFR(1,sec1,sec3,sec4, bUFR_ED, NBYTES,err,select,header=header)
      !-----------------------------------------------------------------------------------
      ! THE SEC3%NDESC=0 INDICATE THAT THIS MESSAGE WAS NOT SELECTED. JUMP TO THE NEXT MESSAGE
      !-----------------------------------------------------------------------------------
      !{
        if ((err==0).AND.(SEC3%NDESC==0)) then 
         deallocate(sec3%d,sec4%r,sec4%d,sec4%c)
         goto 10
       end if
      !}
    end if
    
    if (wsi_only) then 
    wsi=0
    do w=1,sec3%ndesc
	if (sec3%d(w)==307092) wsi=1
	if (sec3%d(w)==307103) wsi=1
	if (sec3%d(w)==308018) wsi=1
	if (sec3%d(w)==309056) wsi=1
	if (sec3%d(w)==309057) wsi=1
	if (sec3%d(w)==311012) wsi=1
	if (wsi==1)  then 
		print *,"*** WSI was found ----> ",sec3%d(w),w
		exit 
	end if 
    end do
    
    if ( wsi==0) then
	deallocate(sec3%d,sec4%r,sec4%d,sec4%c)
         goto 10
    end if
    end if
    
    !----------------------
    ! Print a BUFR MESSAGE
    !----------------------
    !{
    If ((NBYTES > 0).and.(IOERR(1)==0)) Then
      nm=nm+1
      if (err>0) err2=err2+1 
      if((err==0).or.(err>20)) then 
	if (prt_header) then
		ii=index(header,"BUFR")-1
		if (ii==0) ii=len_trim(header)
		write(3,'(1x,a)')"["//header(1:ii)//"]"
	end if 
        write(3,'(1X,a,i2)')":BUFR: # EDITION =",BUFR_ED
        write(3,'(1X,I4," # MBUFR Error code")')err
        write(3,'(1X,a)')":SEC1:"
        write(3,'(1x,I4," # BUFR MASTER TABLE")')sec1%NumMasterTable
        write(3,'(1X,I4," # ORIGINATING CENTER: ",a50)')sec1%center,tabcc1(sec1%center)
        write(3,'(1X,I4," # ORIGINATING SUBCENTER")')sec1%subcenter
        write(3,'(1X,I4," # UPDATE SEQUENCE NUMBER")')sec1%update
	if (sec1%sec2present) then  
	 write(3,'(1X,I4," # OPTIONAL SECTION (PRESENT BUT NOT PRINT)")')1
	else
	write(3,'(1X,I4," # NO OPTIONAL SECTION")')0
	end if
        write(3,'(1X,I4," # DATA CATEGORY: ",a50)')sec1%bType,tabA(sec1%btype)
        write(3,'(1X,I4," # DATA SUBCATEGORY: ",a50)')sec1%intbsubtype,tabCC13(sec1%btype,sec1%intbsubtype)
        write(3,'(1X,I4," # LOCAL DATA SUBCATEGORY ")')sec1%bsubtype
        write(3,'(1X,I4," # BUFR MASTER TABLE VERSION NUMBER")') sec1%VerMasterTable
        write(3,'(1X,I4," # LOCAL TABLE VERSION NUMBER")') sec1%VerLocalTable
        write(3,'(1X,I4," # YEAR ")')sec1%year
        write(3,'(1X,I4," # MONTH ")')sec1%month
        write(3,'(1X,I4," # DAY ")')sec1%day
        write(3,'(1X,I4," # HOUR ")')sec1%hour
        write(3,'(1X,I4," # MINUTE ")')sec1%minute
      !**********
      ! SECAO 3 *
      !**********
      !{ 
        write(3,'(1X,a)')":SEC3:"
        if ((sec3%nsubsets>nss).and.(nss>0))  sec3%nsubsets=nss
        write(3,'(1X,i5," # Num.subsets")')sec3%nsubsets
        write(3,'(1X,i5," # Num.descriptors")')sec3%ndesc
        write(3,'(1x,i5," # Flag for Compressed data (1=compressed 0=uncompressed)")')sec3%is_cpk
    !    write(3,'(1x,i5," # Flag for Data converted from a TAC message")')sec3%is_tac
        
        nsubsets=sec3%nsubsets
        auxtxt=""
        numchar=0
         
        do i=1,sec3%ndesc
          write(3,'(6x,i6.6)')sec3%d(i)
        end do
        end if
       
     !}
     !*************
     !* SECAO 4  *
     !************
     !{
        if((err==0).or.(perr)) then    
          
          write(3,'(1X,":SEC4:")')
          if ((nsubsets>nss).and.(nss>0)) nsubsets=nss
          write(3,'(1x,i5," # N. VARIABLES !!!")')sec4%nvars
         
          do j=1,nsubsets

            write(3,'(5x,":SUBSET ",I5.5,":")')j
            l4=0
            
            do i=1,sec4%nvars
             
              if (sec4%d(i,j)==null) goto 66
              
              if ((sec4%d(i,j)<1000000).and.(sec4%d(i,j)>0)) then
                   txt=ucases(signification_mcodesflags(sec4%d(i,j),sec4%r(i,j),sec4%a(i,j)) )
		
              else
                txt=""
              end if
             
              if (sec4%C(i,j)>0) numchar=numchar+1
               
                55 if (numchar>0) then
                  
                  !{ Se for variavel corrente ou a anterior for caracter entao processa essa parte
                  
                  if (sec4%c(i,j)==numchar) then !...(Se Variavel corrente acumulla os caracteres)
		    p_numchar=numchar	
                    IF (p_numchar>255) p_numchar=255 
		    if ((sec4%r(i,j)>31).and.(sec4%r(i,j)<127))then
                       auxtxt(p_numchar+1:p_numchar+1)=char(int(sec4%r(i,j)))
		    else 
		       auxtxt(p_numchar+1:p_numchar+1)=" "
		    end if
                    txt2=txt  ! Texto anterior
                  else  !......................(Se Variavel  anterior, entao imprime a variavel)
                    auxtxt(1:1)='"'
                    auxtxt(p_numchar+2:p_numchar+2)='"'
                    if(rmk==0) txt2=""
                      
                    if (sec4%k(i-1,j)) then; son="*"; else; son=" "; end if
                          
                    if (LEN_TRIM(AUXTXT)<=22) then
                      l4=l4+1
                      txt3=""
                      
                      write(txt3,'(1x,a22,1x,"# ",i5,") ",a1,i6.6,"-"a91)')trim(auxtxt),l4,son,sec4%d(i-1,j),txt2
                      write(3,'(a)')trim(txt3)
                    else
                      l4=l4+1
                      write(3,*) trim(auxtxt)," #",l4,")",son,sec4%d(i-1,j),trim(txt2)
                    end if
                    numchar=sec4%c(i,j)
                    auxtxt=""
                    goto 55
                  end if
                  !}
                else
                  !{ Se for variavel do tipo numerica entao processa essa parte
                    if (rmk==0) txt=""
                    if (sec4%k(i,j)) then; son="*"; else; son=" "; end if
                    if(sec4%r(i,j)/=null) then 
                      l4=l4+1
                      txt3=""
	
                      write(txt3,'(1x,F22.5," # ",i5,") ",a1,i6.6,"-",a91)')sec4%r(i,j),l4,son,sec4%d(i,j),ucases(txt)
                      write(3,'(a)')trim(txt3)
                    elseif (sec4%d(i,j)<=99999) then  
                      l4=l4+1
                      txt3=""
                      write(txt3,'(1x,a22," # ",i5,") ",a1,i6.6,"-",a91,1x)')"Null",l4,son,sec4%d(i,j),ucases(txt)
                      write(3,'(a)')trim(txt3)
                    else 
                      l4=l4+1
                      txt3=""
                      write(txt3,'(1x,a22," # ",i5,") ",a1,i6.6)')"Null",l4,son,sec4%d(i,j)
                      write(3,'(a)')trim(ucases(txt3))
                    end if
                  !}
                end if 
            66 continue
          end do !nvars
        end do ! nsubsets

        write(3,'(1x,a)')":7777:"
      end if
      !}
     
    !  deallocate(sec3%d,sec4%r,sec4%d,sec4%c)
      if ((nm<nmm).or.(nmm<=0)) goTo 10
        
    end if
	
	
  !}

 call Close_mbufr (1)
 close (2)
 close (3)
   
  print *,":BUFRDUMP: Total number of decoded messages........",nm
  if (err2>0) print *,":BUFRDUMP: Number of messages with error...",err2
  print *,":BUFRDUMP: End"
  print *,""
!}


End 
