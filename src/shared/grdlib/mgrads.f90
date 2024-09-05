!*******************************************************************************
!*                                 MGRADS                                      *
!*                                                                             *
!* Modulo de funcoes e subrotinas uteis para gravacao e leitura de dados       *
!*                    meteorologicos no formato do GRADS                       *
!*                                                                             *
!*                                                                             *
!*                      Sergio Henrique S. Ferreira                            *
!*                                                                             *
!*               MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                     *
!*                                                                             *
!*                                  2002                                       *
!*-----------------------------------------------------------------------------*
!*Autores:                                                                     *
!*   SHSF : Sergio Henrique S. Ferreira <sergio.ferreira@cptec.inpe.br>        * 
!*                                                                             *
!*******************************************************************************
!* DEPENDENCIAS                                                                * 
!* MODULOS DATELIB E STRINGFLIB                                                * 
!*******************************************************************************
! HISTORICO / ATUALIZACOES
!  DEZ 2002 -SHSF-   Versao Inicial ( Prototipo )
!  MAR 2006 -SHSF-   Inclusao/revisao das rotinas para ler e gravar em formato 
!                    de ponto de grades (Load/save)(bin/ctl)_linear. 
!                    Adaptacao de funcoes de interpolacao (1998 - MAER-CTA-IAR)
!  2009      -SHSF-   Criacao do tipo ctldef para tratar a leitura da definicao do
!                    ctl em uma estrutura separada a da matriz de dados observacionais
!  2009/10/02 SHSF-  Acrescido funcao writectl. Modificado openr, e acrescido funcionalidade
!                    para tratamento adequado de TDEF, com conversao para o calendario juliano
!  2010/01/14 SHSF - Revisao das rotinas openw_mgrads e criancao de uma interface para estas rotinas
!  2010/03/06 SHSF - Tratamendo de dados que estao acima do valor missing 
!  2010/04/14 SHSF - Revisado writectl : YDEF
!  2011/09/02 SHSF - Introduzido possibilidade de eixto y invertido 
!  2011/10/12 SHSF - Introduzido funcao timedate
!  2020/08/18 SHSF - subroutine init was included to set logical unit number in case of use with MPI
MODULE MGRADS


 USE DATELIB
 USE stringflib 
 implicit none

 PRIVATE




!{ Tipo definicoes de arquivos binarios do grades
type bindef
   integer                   ::un        !I/O buffer number
   character(len=8),POINTER  ::code(:)   !Codigo/sigla da variavel 
   character(len=255),POINTER::name(:)   !Nome da variavel 
   character(len=1024)       ::ctlname   !Nome do arquivo descritor (CTL) 
   character(len=1024)       ::binname   !Nome do arquivo de dados binarios (BIN)
   character(len=80)         ::tlable           
   integer,pointer           ::varlevs(:)! Numero de niveis verticais de cada variavel
   integer                   ::imax      !Numero de pontos de grade na direcao x
   integer                   ::jmax      !Numero de pontos de grade na direcao y
   integer                   ::kmax      !Numero de niveis verticais (maximo)  
   integer                   ::tmax      !Numero de passos de tempo 
   integer                   ::nvars     !Numero de variaveis 
   integer                   ::nvarsmax  !Numero maximo de variaveis (para alocacao) 
   real                      ::dlat      !Tamanho do elemento de grade na direcao das latitudes
   real                      ::dlon      !Tamanho do elemento de grade na direcao das longitude
   real,pointer              ::lev(:)    !Valores dos Niveis verticais 
   real,pointer              ::lat(:)    !Valores de latitude
   real,pointer              ::lon(:)    !Valores de longitude
   real*8                    ::time_init !Data juliana inicial em dias e decimos de dias 
   real*8                    ::time_step !Passo de tempo em dias e decimos de dias 
   real                      ::undef     !Valor indefinido
   integer                   ::nregtime  !Numero de registros por passo de tempo 
   integer                   ::NREG      !Numero total de registros
   integer                   ::recsize   !Tamanho dos resgistros (bytes)
   integer                   ::lof        !Tamanho do arquivo (bytes)
   integer                   ::horizontal ! Identificacao da tipo de coordenada  horizontal
                                          ! 1 = Lat e lon em graus 2 = Distancia em metros
   integer                   ::vertical   !Identificacao da variavel da coordenada  vertical
   integer                   ::mtype         !Tipo de modelo: 0=Modelo global 1 = modelo regional

   logical                   ::vcoords_from_ctl    !Se .true. = Coordenadas verticais provenientes do ctl 
   logical                   ::xrev                !Indica se o eixo x esta em ordem reversa
   logical                   ::yrev                !Indica se o eixo y esta em ordem reversa	
   logical                   ::optemplate          !Se .true. indica template 
end type 
!}

!{ Subrotinas PUBLICAS
   PUBLIC init_mgrads     ! set Logical Unit Number (default = 8) 
   PUBLIC OPENR_mgrads    ! Abertura logica do arquivo para leitura
   PUBLIC READBIN_mgrads  ! Leitura do arquivo aberto por OPENR 
   PUBLIC writectl_mgrads ! Grava arquivo CTL
   PUBLIC ADDVAR_mgrads   ! Adciona uma variavel a um arquivo aberto
   PUBLIC WRITEBIN_mgrads ! Grava uma variavel 
   PUBLIC CLOSE_mgrads    ! Fecha arquivo aberto
   PUBLIC varlevs_mgrads  ! Retorna o numero de niveis de uma variavel
   PUBLIC varindex        ! Retorna a posicao de uma variavel no ctl 
   PUBLIC BINDEF
   PUBLIC undefval
   public datetime_mgrads !Retorna a data relativa a um passo de tempo 
   public timedate_mgrads !Retoena o passo de tempo relativo a uma data
   PUBLIC overflow_err
   PUBLIC conv_undef_mgrads
   PUBLIC newgrid_mgrads

   PUBLIC OPENW_mgrads    ! Abertura logica do arquivo para gravacao
   interface OPENW_mgrads
     module procedure  OPENW_mgrads1 !call openw_mgrads(bf,outfile,imax,jmax,kmax,nvarmax)
     module procedure  OPENW_mgrads2 
     module procedure  OPENW_mgrads3 !call OPENw_mgrads(bf,outfile,bi,nvarmax,levs,nlevmax)
   end interface
!}

   real,parameter   ::undefval=-8388607e31
   integer,parameter::record_size_unit = 4 ! Unidade de tamanho do registro [ 4 = bytes ou 1 = Word ] conforme compilador 
   integer*4        ::blk !  =0 -> unbloked   >0 : blocked 
   integer          ::un=8  !Logical Unit Number 
                            !Data is transferred between the program and devices or files through a Fortran logical unit. 
		            !Logical units are identified in an I/O statement by a logical unit number,
		            ! a nonnegative integer from 0 to the maximum 4-byte integer value (2,147,483,647).
			    !In mgrads un=8 by defaut, but can be reset using Logical_Unit_number_mgrads subroutine
 CONTAINS
 


!==============================================================================!
! openr_mgrads   | Le dados do arquivo ctl                                |SHSF!
!==============================================================================!
! Exemplo de uso
!   character(len=256)::ctlname
!   type(bindef)::bf
!
!   call openr_mgrads(bf,ctlname)
!
 SUBROUTINE OPENR_mgrads(bf,ctlname)

 !{ Variaveis de interface
    character(len=*),intent(in)::ctlname
    type(bindef),intent(out)::bf
 !}

!{ Variaveis Locais
   character(len=255)::linha,linha0     ! Uma linha de texto
   character(len=255),dimension(200)::w ! Palavras de uma linha de texto
   character(len=5)::tstp               ! time step
   integer::nw                          ! Numero de palavras em w
   integer*2::i,II,jj
   integer::nlevs
   integer::err
   REAL::AUX
   integer::blkbytes
!}

   BF%YREV=.false.
   BF%XREV=.false.
   bf%optemplate=.false.
   close(8)
   open (8,file=(ctlname),status='old')
  ! print *,":MGRADS:OPENR:",trim(ctlname)
10 read(8,'(a)',end=999) linha
      linha0=linha
      linha=ucases(linha) 
      IF (index(linha,"YREV")>0) then
        bf%YREV=.true.
   !     print *,":MGRADS:YREV=" ,bf%YREV
      end if
      if (index(linha,"XREV")>0)  then
        bf%XREV=.true.
    !    print *,":MGRADS:XREV=" ,bf%XREV
      end if
      if ((index(linha,"TEMPLATE")>0).and.(index(linha,"OPTIONS")>0))  then
        bf%optemplate=.true.
     !   print *,":MGRADS:TEMPLATE=" ,bf%optemplate
      end if
     !{Separa nome do arquivo
      if (index(linha,"DSET")>0)then
       call sep_words(linha0,w,nw)
       bf%binname=w(nw)
      end if
      !}
      if (index(linha,"UNDEF")>0)then
       call sep_words(linha,w,nw)
       read(w(nw),*)bf%undef
      end if  

     !{XDEF Definicao do eixo X
      if (index(linha,"XDEF")>0)then
        call sep_words(linha,w,nw)
        bf%imax=val(w(2))
        !write(*,' (":MGRADS: Allocating lon[",i4.4,"]")')bf%imax
        allocate(bf%lon(bf%Imax),STAT=ERR)
        if (err>0) then 
          print *,":MGRADS: Error allocating lon"
          stop 
        end if 
        IF (INDEX(W(3),"LEVELS")>0) THEN
          CALL GETLEVELS(W,NW,bf%imax,bf%LON)
          bf%DLON=0
        ELSE
          AUX=VAL(W(5))
          bf%LON(1)=VAL(W(4))
          bf%DLON=val(W(5))
          DO II=2,bf%IMAX
            bf%LON(II)=bf%LON(II-1)+AUX
          END DO
        END IF
      end if
     !}
     !{ YDEF Definicao do eixo Y
      if (index(linha,"YDEF")>0)then 
       call sep_words(linha,w,nw)
       bf%jmax=val(w(2))
       !write(*,' (":MGRADS: Allocating lat[",i4.4,"]")')bf%jmax
       !if (associated(bf%lat)) deallocate(bf%lat)
       allocate(bf%lat(bf%jmax),STAT=ERR)
       if (err>0)then 
         print *,":MGRADS: Error allocating lat"
         stop 
       end if 
        IF (INDEX(W(3),"LEVELS")>0) then 
          CALL GETLEVELS(W,NW,bf%jmax,bf%LAT)
          bf%DLAT=0
          else
          AUX=VAL(W(5))
          bf%LAT(1)=VAL(W(4))
          bf%DLAT=val(W(5))
          DO II=2,bf%JMAX
           bf%LAT(II)=bf%LAT(II-1)+AUX
          END DO
       END IF
     end if
     !}

    !{ ZDEF Definicao do exio Z
    if (index(linha,"ZDEF")>0)then 
      call sep_words(linha,w,nw)
      bf%kmax=val(w(2))
      !{ Obtendo niveis isobaricos 
       allocate(bf%lev(bf%kmax),STAT=ERR)
        if (err>0) then 
          print *,":MGRADS: Error allocating lev"
          stop 
        end if
        IF (INDEX(W(3),"LEVELS")>0) CALL GETLEVELS(W,NW,bf%Kmax,bf%LEV)
      !}
    end if
   !}
   !{ Tempo  
    if (index(linha,"TDEF")>0)then
        call tdefconvert(linha,bf%time_init,bf%time_step,bf%tmax)
        IF (bf%tmax<1) then
            print *,":MGRADS: Error close TDEF in", trim(bf%ctlname)
            stop
        end if 
        bf%tlable=linha
    end if
    !}

      !{ Separa nome do arquivo 
        if (index(linha,"VARS")>0)then 
				call sep_words(linha,w,nw)
				bf%nvars=val(w(2))
				!if (associated(bf%varlevs)) deallocate(bf%varlevs)
				allocate(bf%varlevs(bf%nvars),STAT=ERR)
				!write(*,' (":MGRADS: Allocating bf%code[",i4.4,"]")')bf%nvars
				allocate(bf%code(bf%nvars),bf%name(bf%nvars),STAT=ERR)
				if (ERR>0) then 
				    print *,":MGRADS: Error allocating bf%code"
				    stop 
				end if
				do i=1,bf%nvars
						read(8,'(a)') linha
						call sep_words(linha,w,nw)
						bf%varlevs(i)=val(w(2))
						bf%code(i)=w(1)
						bf%name(i)=""
						do ii=3,nw
							bf%name(i)=trim(bf%name(i))//" "//trim(w(ii))
						end do
					end do
					goto 999
				end if
			
		  !}
	  !{ 
	

		 goto 10
   999  continue 
		!{Aloca as estruturas xy e bf
		!allocate(xy%code(NVar_Sup),STAT=ERR)
		!allocate(xy%name(NVar_Sup))
		
		

      close(8) 
     !{Get the PATh of binmame from ctlname)
	  !if (bf%binname(1:1)=="^") then
	  !  i=len_trim(bf%binname)
	  !  bf%binname=trim(getpath(ctlname))//bf%binname(2:i)
	  !end if

	 bf%binname=replace(bf%binname,"^",getpath(ctlname))
	
	  print *,":MGRADS: BINFILE=",trim(bf%binname)


    !{ Determinacao do tamanho do registro 
	blk=0 ! 0 = unblocked, 1 = blocked
	bf%recsize=(bf%imax*bf%jmax+blk)*4 !record_size_unit
    !}
 	!{ Obter tamanho de um registro de tempo
	 
	bf%nregtime=0 
	DO II=1,bf%nvars
		IF (bf%VARLEVS(II)==0) THEN
			bf%nregtime=bf%nregtime+1
		ELSE
			bf%nregtime=bf%nregtime+bf%VARLEVS(II)
		END IF
	END DO
	bf%NREG=bf%nregtime*bf%tmax
	bf%LOF=bf%recsize*bf%NREG
	bf%ctlname=ctlname
	end subroutine
!==============================================================================!
! varlevs_mgrads | Fornece o numero de niveis de uma variavel              |SHSF!
!==============================================================================!
 function varlevs_mgrads(bf,varcode);integer::varlevs_mgrads

      
   !{  vaiaveis da interface 
	type(bindef),intent(inout)::bf
	character(len=*),intent(in)::varcode    
	!}
	
	 !{- variaveis locais	!
	  integer:: v                    ! Indice da variavel a ser lida (1 a xyz%nvars)
  
!{ Verificando existencia da variavel e tambem se instante de tempo e valido

	v=varindex(bf,varcode)

	if (v==0) then 
	    print *,":MGRADS: Error: '",trim(varcode),"' not found in ",trim(bf%ctlname);
            varlevs_mgrads=-1
	    return
	end if

	varlevs_mgrads=bf%varlevs(v)

	end function
	!} 

  
!==============================================================================!
! ReadBin | Carrega campos no formato do grads                     |SHSF!
!==============================================================================!
!     Valores missing = -9.99e33                                               !
! Esta subrotina carrega campos de modelos no formato binario do grads         !
! Os campos sao fornecidos em duas estruturas de dados:                        !
!  a) Com os campos de superficie (xy)                                         !
!  b) Com os campos de altitude (xyz)                                          !
!                                                                              !  
!  O programa principal deve conter as declaracoes                             !:
!                                                                              !
!  use MGRADS                                                                  !
!  use datelib                                                                 !
!  type(xygrad):: xy         != Dados de superficie (x,y,nvar)                 !
!  type(xyzgrad) :: xyz     != Dados de altitude (x,y,z,nvars)                 !
!                                                                              !
!==============================================================================!
! HISTORICO                                                                    !
!      DEZ 2002 -  S�gio H.S. Ferreira - prototipo                             !
!      MAR 2006 -  Sergio H.S. Ferreira - Revisao e adaptacao                  !
!      2009 - SHSF - LEITURA DESCLOCADA !. 
!==============================================================================!
! Nota: Aparentemente existem arquivos binarios do grades que possuem ou
!       nao a blocagem do fortran. - Por conveniencia removemos a blocagem.
!       Esta é uma parte que precisa ser melhor vista. 
 subroutine readbin_mgrads(bf,varcode,t,xyz,err,pout)


   !{  vaiaveis da interface 
     type(bindef),intent(inout)         ::bf
     real(kind=4),dimension(:,:,:),intent(inout)::xyz  ! Uma variavel (x,y,z)
     integer,intent(in)                 ::t ! indice relativo ao tempo  t=1,2,.., tfinal
     character(len=*),intent(in)        ::varcode   
     integer,optional,intent(out)       ::err
     logical,optional                   ::pout ! Se present impreme log de saida 
   !}
	   
	 !{- variaveis locais	!
	  integer:: v                    ! Indice da variavel a ser lida (1 a xyz%nvars)
          integer*4::recsize ! Blocagem
	  integer ::x,y,z
	  integer*4 ::irec2
	  integer::nvars,i,i1,i2,is,j1,j2,js,k1,k2
	  real::xyzmax,xyzmin,scalemax,scalemin
	  real*8::jdate !Data e horas correntes no calendario juliano
	  character(len=2)::mm,dd,hh ! Ano, mes,dia e hora
	  character(len=4)::yy ! Ano, mes,dia e hora
	  character(len=1024)::binname
	  
	  !

	  !{ Inciando Variaveis
	if (present(err)) err=0  
	irec2=0;x=0;y=0;z=0;i=0;nvars=0
	scalemax=10.0**(int(log(abs(bf%undef))/log(10.0)))
	scalemin=-scalemax
	xyzmin=scalemax
	xyzmax=scalemin
	xyz(:,:,:)=bf%undef




!{ Verificando existencia da variavel e tambem se instante de tempo e valido

	v=varindex(bf,varcode)

	if (v==0) then 
	    print *,":MGRADS: Error: '",trim(varcode),"' not found in ",trim(bf%ctlname);
	    stop
	end if

	if ((t>bf%tmax).or.(t<1)) then 
	    print *,":MGRADS: Error: Invalid time step"
	    stop
	end if
	!} 
!{ Verificando se e um arquivo TEMPLATE
    if (bf%optemplate) then
	jdate=bf%time_init+bf%time_step*(t-1)
	write(yy,'(i4)')year(jdate)
	write(mm,'(i2.2)')month(jdate)
	write(dd,'(i2.2)')day(jdate)
	write(hh,'(i2.2)')hour(jdate)
	binname=replace(bf%binname,"%y4",yy)
	binname=replace(binname,"%m2",mm)
	binname=replace(binname,"%d2",dd)
	binname=replace(binname,"%h2",hh)
      !print *,":MGRADS:Template t=",t,"var=",trim(varcode)," ",trim(binname)
    else
      binname=bf%binname
    end if
!}

     recsize=(bf%imax*bf%jmax+blk)*record_size_unit
     !{ Lendo arquivo binario
	
	open(8,file=binname,status='unknown',access='DIRECT',recl=recsize)

    !{ Caso o arquivo grads tena sido escrito em ordem reversa em x ou y, inverte a ordem dos indices 
       if (BF%xrev) then 
          i1=bf%imax;i2=1;is=-1
       else

          i1=1;i2=bf%imax;is=1
       end if	
         
       if (BF%yrev) then 
          j1=bf%jmax;j2=1;js=-1
       else
          j1=1;j2=bf%jmax;js=1
       end if	  
      !}     
	  nvars=0 
	  
	  
	!{ Loading  Up Air and surface DATA 
	if (bf%optemplate) then
	  IREC2=VARREG(bf,v,1) ! ser for template o tempo e sempre 1 para o calculo do registro 
	else
	  IREC2=VARREG(bf,V,t)
	end if

	if (bf%varlevs(v)>0) then 
		k1=1
		k2=bf%varlevs(v)
	elseIF (bf%varlevs(v)==0) then
		k1=1
		k2=1
	elseif(present(err)) then
		err=1
		return
	else
	      print *,"MGRADS.READBIN: ERROR: "//TRIM(VARCODE)//" NOT FOUND"
	end if
	
	do z=k1,k2
		irec2=irec2+1
		read(8,rec=irec2)((xyz(x,y,z),x=i1,i2,is),y=j1,j2,js)
	end do
	
	      
	close(8)	
	!{Calculo do valor medio e redefinicao do valor indefinido
	do x=i1,i2,is
		do y=j1,j2,js
			do z=k1,k2
			if ((xyz(x,y,z)<scalemax).and.(xyz(x,y,z)>scalemin))  then 
				if(xyz(x,y,z)>xyzmax) xyzmax=xyz(x,y,z)
				if(xyz(x,y,z)<xyzmin) xyzmin=xyz(x,y,z)
			else   
			      xyz(x,y,z)=bf%undef
			end if
			end do
		end do
	end do

	  !}
	  if  (present(pout)) print *, ":MGRADS:",trim(bf%code(v))," TIME=",t," MIN=",xyzmin," MAX=",xyzmax 		

	end subroutine 

!------------------------------------------------------------------------------
! tdefconvert | converte  formato de tdef                              | SHSF |
!------------------------------------------------------------------------------ 
! Converte data no format do grads para data juliana e obtem demais parametros
!------------------------------------------------------------------------------

 subroutine tdefconvert(tdef,jdate,jstp,nj)
 !{variaveis de inteface
	character(len=*),intent(in)::tdef
	real*8,intent(out)::jdate ! Data inicial no calendario juliano
	real*8,intent(out)::jstp ! passo de tempo em dias e fracoes de dias
	integer,intent(out)::nj   ! numero de passos de tempo

 !}
	character(len=10)::tstp
	character(len=15),dimension(15)::w
	integer::nw,l
	character(len=80)::linha
	character(len=15)::cdate
	jdate=0
	linha=tdef

	call sep_words(linha,w,nw)

	!{ obteecao do numero de passos de tempo
	nj=val(w(2))
	!}

	!{ Obtencao do passo de tempo
	tstp=ucases(trim(w(nw)))
	
	if (index(tstp,"HR")>0) then 
		jstp=val(tstp)/24.0
	else
		jstp=val(tstp)
	end if
	!}
	!{ Obtendo data no calendario juliano
		
	  jdate=fjulian(w(4))
	
	!}  

 end subroutine

!------------------------------------------------------------------------------
! GETLEVELS |                                                         |SHSF   |
!-----------------------------------------------------------------------------
 ! Esta subrotina e chamada exclusivamente por LOADCTL_LINEAR, PARA
 ! fazer a leitura de XDEF, YDEF e ZDEF no caso destas nao serem 
 ! definidas como "LINEAR" e sim como "LEVES"
 !
 !  W e NW corresponde ao resto de linha anterior e poderam conter 
 !  alguns niveis caso NW > 3. Neste caso e feito o resto da leitura
 !  destas variaveis. Caso ainda existam variaveis a serem lidas entao 
 !  continua-se o processo 
 !
 !  
  SUBROUTINE GETLEVELS(W,NW,JMAX,LVAR)
	character(len=*),dimension(:),intent(inout)::W
	integer,intent(inout)::nw
	integer,intent(in)::jmax ! Numero maximo de niveis 
	real,dimension(:),intent(inout)::LVAR

	integer:: jj,II
	CHARACTER(LEN=255)::LINHA

	JJ=0

	
	IF (NW>3) THEN
		DO II=4,NW
			JJ=JJ+1
			LVAR(JJ)=VAL(W(II))
		END DO
	end if

	DO WHILE (JJ<JMAX)
		read(8,'(a)')LINHA
		call sep_words(linha,w,nw)
		do ii=1,nw
			JJ=JJ+1
			LVAR(JJ)=val(w(ii))
		end do
	END DO
		
END SUBROUTINE

!==============================================================================!
! VARREG | REGISTRO INICIAL DOS DADOS DE UMA VARIAVEL                     |SHSF!
!==============================================================================!
!  FUNCAO PRIVADA                                                              !
!                                                                              !
!  Calcula a posicao de uma variavel dentro de um arquivo grads, com base      !
!  nas informacoes lidas por LOADCTL_LINEAR ( ESTRUTURA xyz ) e indice         !
!  que identifica a variavel                                                   !
!                                                                              ! 
!                                                                              !
!==============================================================================! 
!{
 function VARREG(bf,V,t);INTEGER*4::VARREG

 !{ Variaveis de interface
   TYPE(bindef),INTENT(IN)::bf  ! Estrutura de dados lidas por LOADCTL_LINEAR
   INTEGER,INTENT(IN)::V	    ! Indice da variavel 
   integer,intent(in)::t       ! Indice do instante de tempo ( t = 1,2,.., tfinal)
 !}
 !{ Variaveis locais
   INTEGER*4::S
   integer :: II
 !}

   IF ( V>1 )THEN
      S=bf%nregtime*(t-1) 
      DO II=1,V-1
        IF (bf%VARLEVS(II)==0) THEN
          S=S+1
        ELSE
          S=S+bf%VARLEVS(II)
        END IF
      END DO
      VARREG=S
   ELSE
      VARREG=bf%nregtime*(t-1)
   END IF
END FUNCTION
!}
!==============================================================================!
! openw_mgrads2   | Abre arquivo para gravacao no formato do grads        |SHSF!
!==============================================================================!
! Como base nas definicoes de outro arquivo CTL do grads (bi)
!
! Este rotina aproveita todas as propriedades de bi para formar bf,
! com excessao do numero de niveis na coordenada principal 
!-----------------------------------------------------------------------------
 subroutine openw_mgrads2(bf,outfile,bi,nvarmax)
!{ Variaveis da interface
   type(bindef),intent(inout)::bf
   character(len=*),intent(in)::outfile
   type(bindef),intent(in)::bi
   integer,intent(in)::nvarmax ! Numero maximo de variaveis
!} 
   call openw_mgrads(bf,outfile,bi%imax,bi%jmax,bi%kmax,nvarmax)
   bf%lat=bi%lat
   bf%lon=bi%lon
   bf%lev=bi%lev
   bf%dlat=bi%dlat
   bf%dlon=bi%dlon
   bf%tlable=bi%tlable
   bf%undef=undefval
   bf%optemplate=bi%optemplate
   bf%time_init=bi%time_init
   bf%time_step=bi%time_step
!}
end subroutine

!==============================================================================!
! openw_mgrads3   | Abre arquivo para gravacao no formato do grads        |SHSF!
!==============================================================================!
! Una como base as definicoes de outro arquivo CTL do grads (bi)
!
! Este rotina aproveita todas as propriedades de bi para formar bf,
! com excessao do numero de niveis na coordenada principal e do
! numero de variaveis maximas
!-----------------------------------------------------------------------------
!{
 subroutine openw_mgrads3(bf,outfile,bi,nvarmax,levs,nlevmax)

!{ Variaveis de interface
   type(bindef),intent(inout)::bf
   character(len=*),intent(in)::outfile
   type(bindef),intent(in)::bi
   integer,intent(in)::nvarmax ! Numero maximo de variaveis
   real,dimension(:)::levs     !Niveis verticais
   integer,intent(in)::nlevmax ! Numero maximo de niveis verticais
!}
!{ Variaveis locais
   integer::k
!}
   call openw_mgrads(bf,outfile,bi%imax,bi%jmax,nlevmax,nvarmax)
   bf%lat=bi%lat
   bf%lon=bi%lon
   bf%kmax=nlevmax
   do k=1,bf%kmax 
      bf%lev(k)=levs(k)  
   end do
   bf%dlat=bi%dlat
   bf%dlon=bi%dlon
   bf%tlable=bi%tlable
   bf%undef=undefval
   bf%optemplate=bi%optemplate
   bf%time_init=bi%time_init
   bf%time_step=bi%time_step
end subroutine
!}
!==============================================================================!
! openw_mgrads   | Abre arquivo para gravacao no formato do grads         |SHSF!
!==============================================================================!
! Exemplo de uso
 subroutine openw_mgrads1(bf,outfile,imax,jmax,kmax,nvarmax)
 !{ Variaveis da interface
   type(bindef),intent(inout)::bf 
   character(len=*),intent(in)::outfile
   integer,intent(in)::imax,jmax,kmax ! Dimensionamento da matriz em x,y e z
   integer,intent(in)::nvarmax ! Numero maximo de variaveis
!}
!{ Variaveis locais
   logical::exists
   character(len=1024)::binname
!}
   bf%ctlname=trim(outfile)//".ctl"
   bf%binname=trim(outfile)
   bf%nvars=0
   bf%imax=imax
   bf%jmax=jmax
   bf%kmax=kmax
   bf%tmax=0
   bf%undef=undefval
   bf%recsize=bf%imax*bf%jmax
   bf%nvarsmax=nvarmax
   bf%nregtime=0
   bf%nreg=0
   bf%lof=0
   allocate(bf%lon(1:bf%Imax))
   allocate(bf%lat(1:bf%jmax))
   allocate(bf%lev(1:bf%kmax))
   allocate(bf%varlevs(1:nvarmax))
   allocate(bf%code(1:nvarmax))
   allocate(bf%name(1:nvarmax))
   binname=trim(bf%binname)//".bin"
   inquire(FILE = binname, EXIST = exists)
   if (exists) then 
     !print *,""
     !print *,":MGRADS: Error: File already exists."
     !print *,"         Please choose another filename or remove this file"
     !print *,"         ",trim(binname)
     !stop
     call unlink(binname)
   end if
   ! print *,":MGRADS:openw file=",trim(outfile)
  end subroutine
! 
!
!==============================================================================!
! addvar_mgrads   | Adiciona variavel a estrutura de gravacao             |SHSF!
!==============================================================================!
! Exemplo de uso
subroutine addvar_mgrads(bf,varcode,nlev,varname)
  !{ Variaveis da intertface
	type(bindef),intent(inout)::bf ! Definicoes do arquivo binario
	character(len=*),intent(in)::varcode ! Codigo da variavel
	integer,intent(in)::nlev ! Numero de niveis
	character(len=*),intent(in)::varname ! Descricao da Variavel
  !}
        if (nlev>bf%kmax) then 
          print *,":MGRADS:ADDVAR: Error: nlev > kmax: (kmax,varcode,nlev)=",bf%kmax,trim(varcode),nlev
          stop
        end if
	if (bf%nvars<bf%nvarsmax) then 
	bf%nvars=bf%nvars+1
	bf%code(bf%nvars)=varcode
	bf%varlevs(bf%nvars)=nlev
	bf%name(bf%nvars)=varname
	IF (bf%VARLEVS(bf%nvars)==0) THEN
		bf%nregtime=bf%nregtime+1
	ELSE
		bf%nregtime=bf%nregtime+bf%VARLEVS(bf%nvars)
	END IF
	else
	  print *,":MGRADS: Error: Nvarmax excedded (>nvars=)",bf%nvars,bf%nvarsmax
	  stop
	end if
   ! print *,":ADDVARS_MGRADS:",bf%nvars,bf%code(bf%nvars)
	
end subroutine
!==============================================================================!
! writebin_mgrads   | Gravar dados                                       |SHSF!
!==============================================================================!
! Exemplo de uso
subroutine writebin_mgrads(bf,varcode,t,xyz)
  !{ Variaveis da intertface
	type(bindef),intent(inout)::bf ! Definicoes do arquivo binario
	character(len=*),intent(in)::varcode ! Codigo da variavel
	integer,intent(in)::t ! passo de tempo
	real(kind=4),dimension(:,:,:),intent(inout)::xyz
	
  !}
  !{ Variaveis locais
	integer::v
	integer*4::irec2,k1,k2,recsize
	integer::x,y,z
	real::scalemax,scalemin
	character(len=1024)::binname
	character(len=10)::gdate ! Data gregoriana yyyymmddhh
	real*8::jdate
  !}
  !{ Verificar se a variavel existe
	v=varindex(bf,varcode)
	if (v==0) then 
	    print *,":MGRADS: Error: '",trim(varcode),"' not found in ", trim(bf%binname);
	    call close_mgrads(bf)
	    stop
	end if
   !}
   !{ Verifica dimensoes de xyz
     if (size(xyz,1)<bf%imax) then 
		print *,":MGRADS_WRITEBIN: Erro: size(xyz,1)=",size(xyz,1),"< bf%imax=",bf%imax
		call close_mgrads(bf)
		stop
	endif
     if (size(xyz,2)<bf%jmax) then 
		print *,":MGRADS_WRITEBIN: Erro: size(xyz,2)=",size(xyz,2),"< bf%jmax=",bf%jmax
		call close_mgrads(bf)
		stop
	endif
     if (size(xyz,3)<bf%varlevs(v)) then 
	      print *,":MGRADS_WRITEBIN: Erro: size(xyz,3)=",size(xyz,3),"< bf%varlevs=",bf%varlevs(v),"Varcode=",varcode
	      call close_mgrads(bf)
	     stop
	endif
     if (t<1) then 
	      print *,":MGRADS_WRITEBIN: Erro: Time must be positive "
	      call close_mgrads(bf)
	      stop
	endif

    !}
 
	scalemax=10.0**(int(log(abs(bf%undef))/log(10.0))-1)
	scalemin=-scalemax


	
 
	if (bf%tmax<t) bf%tmax=t ! atualiza tempo maximo
	recsize=(bf%imax*bf%jmax)*record_size_unit
	bf%recsize=(bf%imax*bf%jmax)*record_size_unit
        if (recsize<=0) then
           print *,":MGRADS:WRITE_BIN: Error! recsize=",recsize
	   print *,":MGRADS:WRITE_BIN:imax,jmax,record_size_unit=",bf%imax,bf%jmax,record_size_unit
           stop
        end if

        !{ Obtendo nome do arquivo de gravacao
	if (bf%optemplate) then
	      jdate=bf%time_init+bf%time_step*(t-1)
	      write(gdate,'(i4,3i2.2)')year(jdate),month(jdate),day(jdate),hour(jdate)
	    
	    binname=trim(bf%binname)//"_"//gdate//".bin"
	    IREC2=VARREG(bf,V,1)
	else
	    binname=trim(bf%binname)//".bin"
	    IREC2=VARREG(bf,V,t)
	end if
 
  !{ Gravando arquivo binario 
    !print *,":MGRADS:Gravando ",trim(varcode),' in ',trim(binname)
      
    open(8,file=binname,status='unknown',access='DIRECT',recl=recsize)
   
    !print *,":MGRADS: t=",t,"v=",v,"IREC=",IREC2
    if (bf%varlevs(v)>0) then 
      k1=1
      k2=bf%varlevs(v)
    else
      k1=1
      k2=1
    end if

    do z=k1,k2
      do x=1,bf%imax
        do y=1,bf%jmax
          if (xyz(x,y,z)/=bf%undef) then 
            if ((xyz(x,y,z)>scalemax).or.(xyz(x,y,z)<scalemin))  then 
              print *,":MGRADS:Writebin: Overflow Error"
              print *," Var=",trim(varcode),"(",x,y,z,")=",xyz(x,y,z)
              xyz(x,y,z)=bf%undef
              !close(8)
              !stop
            end if	
          end if
        end do
      end do
     
     
      irec2=irec2+1
      if (bf%dlat<0) then 
         write(8,rec=irec2)((xyz(x,y,z),x=1,bf%imax,1),y=bf%jmax,1,-1)
      else 
        write(8,rec=irec2)((xyz(x,y,z),x=1,bf%imax,1),y=1,bf%jmax,1)
      end if
    end do 
    close(8)	
  !}
end subroutine
!==============================================================================!
! writectl_mgrads   | Gravar ctl                                         |SHSF !
!==============================================================================!
! Exemplo de uso

subroutine writectl_mgrads(bf)
!{ Variaveis da interface
	type(bindef),intent(in)::bf
!} 

!{ Variaveis locais
   real::xmin ! Longitude minima
   real::ymin ! 
   real::xmax ! 
   real::ymax !
   real::tmax
   real*8::jdate ! Data em dias e fracoes de dias a partir do ano zero (vide datelib)
   character(len=255),dimension(100)::lines
   character(len=1024)::binname
   integer*2::z,i,v
   real :: stpx
   real :: stpy 
   real :: stpt
   integer ::p,r
   integer::rz
!}
   xmin=bf%lon(1);stpx=bf%dlon
   if (bf%dlat<0) then 
      ymin=bf%lat(1)+bf%dlat*(bf%jmax-1)
      stpy=-bf%dlat
   else 
      ymin=bf%lat(1)
      stpy=bf%dlat
    end if
   tmax=bf%tmax;stpt=bf%time_step*24
   jdate=bf%time_init
   close(8)
   open (8,file=(bf%ctlname),status='replace')
   p=0
  !{ Escrevendo CTL
    !{ nome do arquivo binario 
      if (bf%optemplate) then 
        binname=trim(bf%binname)//"_"//char(37)//"y4"//char(37)//"m2"//char(37)//"d2"//char(37)//"h2.bin"
      else
       binname=trim(bf%binname)//".bin"
      end if
      r=rindex(binname,"/")    
      if (r>0)  binname="^"//binname(r+1:len_trim(binname))
      lines(1)='DSET    '//trim(binname)
    !}
    !{ OPTION TEMPLATE
    if (bf%optemplate) then
      lines(2)='OPTIONS TEMPLATE               '
    else
      lines(2)='                               '
    end if
    !}
    lines(3)='TITLE   Sample Data Set           '
    lines(4)='UNDEF   '//trim(strs(bf%undef))
    !{ XDEF
      lines(5)='XDEF    '//trim(strs(bf%imax))//' LINEAR '//trim(strs(xmin))//' '//trim(strs(stpx))
    !}
    !{ YDEF pode ser LINEAR or LEVEL  caso stpy == 0, isto indica que o YDEF e LEVELS
      if (stpy==0.0) then
        lines(6)='YDEF    '//trim(strs(bf%jmax))//' LEVELS'
        rz=3
        do z=1,bf%jmax
          rz=rz+1
          if (rz>=10) then
            p=p+1
            lines(6+p)=""
            rz=0
          end if
          Lines(6+p)=trim(lines(6+p))//' '//trim(strS(bf%lat(z)))
        end do
      else
        lines(6+p)='YDEF    '//trim(strs(bf%jmax))//' LINEAR '//trim(strs(ymin))//' '//trim(strs(stpy))
      end if
    !}
    !{ ZDEF: SEMPRE LEVES
    lines(7+p)='ZDEF    '//trim(strs(bf%kmax))//' LEVELS'
    do z=1,bf%kmax
      Lines(7+p)=trim(lines(7+p))//' '//trim(strS(bf%lev(z)))
    end do
    !}

	!{ Caso na seja fornecido jdate, entao utiliza o valor em bf%tlable como data inicial
	if (jdate<1950.0) then 
	    lines(8+p)=bf%tlable
	else
	    ! FALTA TESTAR ESTA PARTE 
          !print *,"t1=",bf%tlable
	   ! print *,jdate
	    lines(8+p)='TDEF  '//trim(strs(tmax))//" linear "//grdate(jdate)//" "//trim(strs(stpt))//"hr"
	   ! print *,"t2=",trim(lines(7))
	end if 
	!}

	
        lines(9+p)='VARS   '//trim(strS(bf%nvars))
	do i=1,9+p
	    write(8,'(a)')trim(lines(i))
	end do

	do v=1,bf%nvars
	    write(8,881)bf%code(v),bf%varlevs(v),trim(bf%name(v))
 881    format(a8,i3,' 0 ',a)

	end do
   

        write(8,'(a)')'ENDVARS                            '
 
        close(8)	 


end subroutine
subroutine close_mgrads(bf)
!{ Variaveis da interface
   type(bindef),intent(inout)::bf
!} 
    deallocate(bf%lon)
    deallocate(bf%lat)
    deallocate(bf%lev)
    deallocate(bf%varlevs)
    deallocate(bf%code)
    deallocate(bf%name)

end subroutine


!==============================================================================!
! varindex   | Retorna a posicao de uma variavel na estrutura             |SHSF!
!==============================================================================!

 function varindex (bf,varcode);integer::varindex
!{ Variaveis de entrada
    type(bindef),intent(in)::bf
    character(len=*),intent(in)::varcode
!}
!{ Variaveis locais
      integer::i
	i=0
	varindex=0
	do i=1,bf%nvars
		if (trim(ucases(bf%code(i)))==trim(ucases(varcode))) varindex=i
	end do
end function


!==============================================================================!
! datetime   | Retornar a data relativa a um instante de tempo t         |SHSF!
!==============================================================================!
 function datetime_mgrads(bf,t);real*8::datetime_mgrads
 !{ Cariavel da interface
	  type(bindef),intent(in)::bf ! Definicao do arquivo 
	  integer,intent(in)::t      ! Passo de tempo desejado	  
	  datetime_mgrads=bf%time_init+(bf%time_step*(t-1))
	  
 end function

!==============================================================================!
! timedate   | Retornar o tempo t relativo a uma data                     |SHSF!
!==============================================================================!
 function timedate_mgrads(bf,jdate);integer ::timedate_mgrads
 !{ Cariavel da interface
	  type(bindef),intent(in)::bf ! Definicao do arquivo 
	  real*8,intent(in)::jdate    ! Data no calendario juliano em dias
	  timedate_mgrads=int((jdate-bf%time_init)/bf%time_step)+1
	  
 end function


!==============================================================================!
! overflow_err   | Retornar true ou false para teste de transbordamento     |SHSF!
!==============================================================================!

function overflow_err(v,bl); logical::overflow_err
  !{ Variaveis da interface
  real,dimension(:,:,:),intent(in)::v
  type(bindef),intent(in)::bl
  !}
  !{Variaveis locais
    real::scalemax
    real::scalemin
    integer::i,j,k
  !}
	scalemax=10.0**(int(log(abs(bl%undef))/log(10.0))-1)
	scalemin=-scalemax
	overflow_err=.false.
	do i=1,bl%imax
	do j=1,bl%jmax
	do k=1,bl%kmax
	if (v(i,j,k)/=bl%undef) then
	if ((v(i,j,k)>scalemax).or.(v(i,j,K)<scalemin))  then 
		overflow_err=.true.
		print *,":MGRADS:Overflow error at v(",i,j,k,")=",v(i,j,k)
		print *,"        scalemax,bl%undef=",scalemax,bl%undef
		return
	end if	
	end if
	end do
	end do
	end do

end function

!---------------------------------------------------------------------
! conv_undef | Converte valores indefinidos
!-------------------------------------------------------------------
subroutine conv_undef_mgrads(bi,bf,v)
!{ variaveis de interface
	type(bindef),intent(in)::bi
	type(bindef),intent(in)::bf
	real,dimension(:,:,:),intent(inout)::v
!}
!{ Variaveis locais
	integer::i,j,k
!}
	if (bi%undef/=bf%undef) then 
	do i=1,bi%imax
	do j=1,bi%jmax
	do k=1,bi%kmax

	if (v(i,j,k)==bi%undef) v(i,j,k)=bf%undef
	end do
	end do
	end do
	end if
!}


end subroutine 


!--------------------------------------------------------------
! 
!
!==============================================================================!
! openw_mgrads   | Abre arquivo para gravacao no formato do grads         |SHSF!
!==============================================================================!
! Exemplo de uso
 subroutine newgrid_mgrads(bf,imax,jmax,kmax,nvarmax)
 !{ Variaveis da interface
   type(bindef),intent(inout)::bf 
   integer,intent(in)::imax,jmax,kmax ! Dimensionamento da matriz em x,y e z
   integer,intent(in)::nvarmax ! Numero maximo de variaveis
!}
!{ Variaveis locais
   logical::exists
   character(len=1024)::binname
!}
   bf%nvars=0
   bf%imax=imax
   bf%jmax=jmax
   bf%kmax=kmax
   bf%undef=undefval
   bf%recsize=bf%imax*bf%jmax
   bf%nvarsmax=nvarmax
   bf%nregtime=0
   bf%nreg=0
   bf%lof=0
   allocate(bf%lon(bf%Imax))
   allocate(bf%lat(bf%jmax))
   allocate(bf%lev(bf%kmax))
   allocate(bf%varlevs(nvarmax))
   allocate(bf%code(nvarmax))
   allocate(bf%name(nvarmax))
  
  end subroutine
  
  subroutine init_mgrads(logical_unit_number)
   integer,intent(in)::logical_unit_number
   un=logical_unit_number
   PRINT *,":MGRADS:INIT: Logical Unit Number = ",un
  end subroutine 

END MODULE
