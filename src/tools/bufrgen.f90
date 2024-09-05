	program bufrgen

!******************************************************************************
!*                                BUFRGEN                                     *
!*                                                                            *
!* Ler um arquivo texto (com Formato do BUFRDUMP) e gera um arquivo BUFR      *
!* (Read a text file (BUFRDUMP format) and generate a BUFR file)              *
!*                                                                            *
!*                                                                            *
!*      Copyright (C) 2005  Sergio Henrique S. Ferreira                       *
!*                          Waldenio Gambi de Almeida                         *
!*                                                                            *
!*      MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                             *
!*                                                                            *
!*                                                                            *
!******************************************************************************
! HISTORICO
! 2005      SHSF - Versao Original 
! 2007-02   SHSF - Atualizacao modulo MBUFR-ADT V 1.5 
! 2008-06-20 SHSF - Atualizacao do modulo MBUFR-ADT V4.0. Novos elementos da
!                   secao 4 ainda nao estao sendo considerados sendo seus valores
!                   definidos como zero. Sao estes sec1%second, sec1%local_bsubtype
!                   e sec1%update
! 2009-03-11 SHSF - Atualizacao do module MBUFR-ADT v4.0.3: Inclusao da funcao de
!                   auto geracao de de mensagem bufr 
! 20180416  SHSF  - Acrescentado verbosidade e incluido rotina init_mbufr           

  use mbufr
  use stringflib

  implicit none


  character(len=255):: infile       !Arquivo de entrada
  character(len=255):: outfile      !Arquivo de saida
  character(len=255):: line         !Uma linha do arquivo de entrada
  logical           :: newmessage   ! 
  integer           :: messages  
  integer           :: erro
  integer           :: i,s,i2
  integer*2         :: argc
  integer           :: iargc,v
  integer           ::narg,x1,x2,DDesc
  character(len=1),dimension(10)      ::argname
  character(len=255),dimension(10)    ::arg
  integer                             ::vrb !Verbosidade
  integer                             :: optional_section
  logical                             ::template_only
!}
!{ Declaracao de variaveis para MBUFR-ADT
   type(sec1type)::sec1
   type(sec3type)::sec3
   type(sec4type)::sec4
   integer       ::err
!}
!{ Iniciando variaveis
   newmessage=.false.
   messages=0
   vrb=3
   template_only=.false.
 !}

 !{ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida
    argc =  iargc()	

    call getarg2(argname,arg,narg)
     x1=0
     x2=0
     ddesc=0
    if ((argname(1)=="?").and.(narg>=2)) then 
      infile=arg(1)
      outfile=arg(2)
      x1=1
    elseif((len_trim(argname(1))>0).and.(narg>1)) then
     
      do i=1,narg
        if (argname(i)=="i") then !,,,,,,,.....
          infile=arg(i)
          x1=1
        elseif (argname(i)=="o") then !................ Arquivo de saida
          outfile=arg(i)
          x2=1
        elseif (argname(i)=="x") then !.................Formato de saida
          Ddesc=val(arg(i))
          if (Ddesc>=300000) X1=1
       elseif (argname(i)=="v") then 
          vrb=val(arg(i))
       elseif (argname(i)=="n") then 
        template_only=.true.
        end if
      end do
      x1=x1*x2
    end if

    if (x1==0) then 
      print *,"+--------------------------------------------------------------+"
      print *,"| CPTEC/INPE BUFRGEN : Encode  FM94 BUFR files                 |"
      print *,"| Include MBUFR-ADT module ",MBUFR_VERSION,"           |"
      print *,"+--------------------------------------------------------------+"
      print *,"|                  To Encode a BUFR message                    |"
      print *,"+-------------------+                    +---------------------+"
      print *,"|  bufrgen infile outfile                                      |"
      print *,"|  or                                                          |" 
      print *,"|  bufrgen  -i infile  -o outfile  {-n}                        |"
      print *,"|                                                              |"
      print *,"|  infile = Input file name                                    |"
      print *,"|  outfile = BUFR file name                                    |"
      print *,"|  -n = Do not write data in section 4 (template only )        |"
      print *,"+--------------------------------------------------------------+"
      print *,"|   To generate a BUFR example based on a table D descriptor   |"
      print *,"+-------------------+                    +---------------------+"
      print *,"|   bufrgen -x ddesc  -o outfile                               |"
      print *,"|                                                              |"
      print *,"|   ddesc = a Table D Descriptor                               |"
      print *,"|   outfile = BUFR file name                                   |"
      print *,"+--------------------------------------------------------------+"
      print *
      stop
    endif
   !}
  
   call INIT_MBUFR(vrb,.true.)
 if (Ddesc==0) then 
	open(1,file=infile,status="old")  ! Abre arquivo texto p/ leitura
	call open_mbufr(2,outfile) ! Abre arquivo BUFR p/ gravacao 
 
10	read(1,'(a)',end=999) line 
      
11	call CutString(line,"#")

	!{ Identificando nova mensagem e bandeira de erro 

	if (index(line,":BUFR:")>0) then
		read(1,*,end=999) erro
		if (erro==0) then 
			newmessage=.true.
			messages=messages+1
		end if
	end if
	
	if (newmessage) then 
		if(index(line,":SEC1:")>0) then
			read(1,*,end=999)          sec1%NumMasterTable
			read(1,*,end=999)          sec1%center
			read(1,*,end=999)          sec1%subcenter
			read(1,*,end=999)          sec1%update
			read(1,*,end=999)          optional_section
			read(1,*,end=999)          sec1%btype 
			read(1,'(a)',end=999)line; sec1%Intbsubtype = val(line)
			read(1,'(a)',end=999)line; sec1%bsubtype = val(line)
			read(1,*,end=999)          sec1%VerMasterTable 
			read(1,*,end=999)          sec1%VerLocalTable 
			read(1,*,end=999)          sec1%year 
			read(1,*,end=999)          sec1%month 
			read(1,*,end=999)          sec1%day  
			read(1,*,end=999)          sec1%hour 
			read(1,*,end=999)          sec1%minute 
			sec1%second=0

			if (sec1%bsubtype<0) sec1%bsubtype=0
			if (sec1%Intbsubtype<0) sec1%Intbsubtype=0
      
		elseif(index(line,":SEC3:")>0) then 
			read(1,*,end=999) sec3%nsubsets
			read(1,*,end=999) sec3%ndesc
			read(1,*,end=999) sec3%is_cpk
			!read(1,*,end=999) sec3%is_tac
			allocate(sec3%d(sec3%ndesc),STAT=ERR)

			do i=1,sec3%ndesc
				read(1,'(a)',end=999)line
				call Cutstring(line,"#")
				if (index(line,":")>0)  then 
					print *,"Error! Unspected end of section 3 reading descriptor ",i
					print *,"       ",trim(line)
					print *
					stop
				else
				sec3%d(i)=val(line)
				end if
			end do
			if (template_only) sec3%nsubsets=0
		
		elseif((index(line,":SEC4:")>0).and.(sec3%nsubsets>0)) then

			read(1,*,end=999)sec4%nvars
			allocate(sec4%r(sec4%nvars,sec3%nsubsets),STAT=ERR)
			sec4%r(:,:)=0
			s=0

55			read(1,'(a)',end=999)line
			call CutString(line,"#")

			if (index(line,":SUBSET")>0)  then 
				s=s+1  
				i=0
				goto 55
			end if

			if ((index(line,":SEC")>0).OR.(INDEX(LINE,":7777:")>0)) goto 11

			if (index(line,'"')>0) then ! Variavel caracter
				line=between_invdcommas(line)
				do i2=2,len_trim(line)-1
					i=i+1
					sec4%r(i,s)=ichar(line(i2:i2))
				end do
			else !Variavel Numerica ou nula 
				i=i+1
				sec4%r(i,s)=VAL(line)
			end if
			if (i<=sec4%nvars) goto 55
			if (s<sec3%nsubsets) goto 55
		
		elseif(index(line,":7777:")>0) then
			!> Coidifica a mensagem BUFR, apos encontrado
			!  o indicativo e fim da mensagem 7777
			call write_mbufr(2,sec1,sec3,sec4)
			newmessage=.false.
		end if
	end if    
    goto 10
 else 

    sec1%NumMasterTable = 0 
    sec1%center         = 46
    sec1%subcenter      = 0 
    sec1%update         = 0 
    sec1%btype          = 0 
    sec1%Intbsubtype    = 0
    sec1%bsubtype       = 0
    sec1%VerMasterTable = 14
    sec1%VerLocalTable  = 0 
    sec1%year           = 2008
    sec1%month          = 01
    sec1%day            = 01
    sec1%hour           = 0
    sec1%minute         = 0
    sec1%second         = 0

    sec3%nsubsets=0
    sec3%ndesc=1
    allocate(sec3%d(sec3%ndesc))
    sec3%is_cpk=0
    sec3%is_tac=0
    sec3%d(1)=Ddesc
    call open_mbufr(2,outfile) ! Abre arquivo BUFR p/ gravacao 
    call write_mbufr(2,sec1,sec3,sec4)
end if


999 continue

   close(1)

	call close_mbufr(2)

	end 



