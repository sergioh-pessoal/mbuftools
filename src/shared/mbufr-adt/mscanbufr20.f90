! ############################################################################
!>#                                                                          # 
!!#                               MSCANBUFR20                                #
!!#  Modulo para leitura e conversao de dados em BUFR para uma matriz de     #
!!#  observacoes de dados                                                    #
!!#                    Sergio Henrique Soares Ferreira                       #
!!#                       sergio.ferreira@inpe.br                            #
!!#                                                                          # 
!#############################################################################
!# Revisores:                                                                # 
!# SHSF:  Sergio Henrique Soares Ferreira                                    #
!# ALTF: Ana Lucia Travezani Ferreira                                        #
!#===========================================================================# 
!# 20100521 ALTF,SHSF: Corrigido a inicializacao do contator de numero de dados 
!#                     lidos (nrows) na subrotina load1
MODULE MSCANBUFR20

!{ inclusao dos modulos auxiliares  e dependencias 
 USE stringflib
 USe datelib
 USE MBUFR
 USE MFORMAT20 
 USE METLIB
 USE MSCANBUFR
 USE OBSTHINNER
 USE OBSCODES_BUFR
!}
 
implicit none  ! Todos as variaveis serao declaradas
PUBLIC 
 !> Identification data type 
type idtype
    character(len=12)::kx   !<Data type
    integer*2        ::ks   !<Sonde Index 
    real             ::lat  !<Latitude
    real             ::lon  !<Long
    real             ::lev  !<Vertical Level 
    real             ::tdif !<Time displacement
    integer*2        ::nlev !<Number of vertical levels
    integer          ::alt  !<Altitude
    integer          ::wmo  !<WMO Station Number
    integer          ::year 
    integer          ::month 
    integer          ::day 
    integer          ::hour 
    integer          ::minu 
end type

type obsbtype
    type(idtype),pointer::id (:)    !id(i) i = number of observations
    real,pointer        ::obs(:,:)  !obs(i,j) i = number of observation; j = variable
 end type
!}

!> Interfaces
interface load_mscanbufr20
   module procedure load1
   module procedure load2
end interface 
!}
!{Variaveis globais do modulo 
 integer,parameter      ::bmax=2000000       !Numero maximo de observacoes
 real,allocatable       ::B(:,:)             !Matriz de observacoes BUFR B(bi,bj) 
 integer,dimension(bmax)::ks                 !Vetor de identifica o numero da sondagem
 integer,dimension(bmax)::btype              !Tipo bufr de cada observacao
 integer,dimension(bmax)::bsubtype           !Subtipo BUFR de cada observacao
 integer,dimension(bmax)::center             !Codigo do centro gerador
 type(obsbtype)         ::obsb               ! 
 real*8                 ::jdate00            !DATA RELATIVA AS ZERO UTC" em dias 
 integer                ::ncbt               !
 integer,parameter      ::MObsmax=100000     !Numero maximo de linhas em OBS 
 real                   ::atovs_grsize       !Tamanho da grade para diluicao do atovs (graus)
 real,parameter         ::missing=-340282300 !
 logical                ::load2closed        !
!}
!==============================================================================
CONTAINS



!>INIT_MOBS ! Initialize mscanmbufr20 module
 subroutine init_mscanbufr20(jdate1,mdate,jdate2,qsize,extractor_nml)
   real*8       ,intent(inout)::jdate1       !< Final date
   real*8       ,intent(inout)::jdate2       !< Initial date 
   real*8       ,intent(in)   ::mdate 
   real         ,intent(in)   ::qsize        !<Size of grid box used to thinning ATOVS data (graus)
   real*8                     ::date1,date2
   character(len=*),intent(in)::extractor_nml !< Configuration filename 
!}
!{Variaveis locais
   character(len=1024)        ::nmlfile
   character(len=1024)        ::local_tables !.........Local das tabelas 
   integer::i
!}
   print *,":MBUFR2OBSB:Init"
 !{ Obtem diretorio das tabelas BUFR  
   call getenv("MBUFR_TABLES",local_tables) !.Local das tabelas ODS
   i=len_trim(local_tables)
   if ((local_tables(i:i)/=char(92)).and.(local_tables(i:i)/="/")) then 
      if (index(local_tables,char(92))>0) then 
        local_tables=trim(local_tables)//char(92)
      else
        local_tables=trim(local_tables)//"/"
      end if
    end if
    if (len_trim(extractor_nml)==0) then 
      nmlfile=trim(local_tables)//"extractor.cfg"
    else
      nmlfile=extractor_nml
    end if
!}
!{ Inicializa mscanbufr e obscodes
    date1=jdate1
    date2=jdate2 
    print *,"Namelist=",trim(nmlfile)
    call init_mscanbufr(nmlfile,date1,date2,ncbt)
    call init_obscodes(nmlfile)
    atovs_grsize=qsize
    allocate(B(1:bmax,1:ncbt))
    b(:,:)=missing    !..........................................Zerando matriz de dados BUFR
    load2closed=.false.
    jdate00=int(mdate)
!}
end subroutine
!-------------------------------------------------------------------------------
!close | Finaliza este modulo                                          | SHSF 
!-------------------------------------------------------------------------------
!{
subroutine close_mscanbufr20
   if (load2closed) then 
     deallocate(obsb%id,obsb%obs)
   else
    deallocate(B)
   end if
end subroutine
!}


!#==============================================================================#
!# load     |     SUBROTINA PARA LEITURA E SELECAO DE DADOS BUFR          |SHSF #
!#------------------------------------------------------------------------------#
!# Tipo         : SUBROTINA DE ACESSO PUBLICO               	                #
!#------------------------------------------------------------------------------#
!#  Descricao:                                                                  #
!#  Este subrotina processa a leitura de um ou mais arquivos BUFR, extrai as    #
!#  variaveis de interesse definadas em cbt armazenando-as em  obs(:,:)         #
!#                                                                              #
!#  Para fazer a leitura de todas as mensagens BUFR dentro de um arquivo        #
!#  BUFR e utilizada a sub-rotina READ_MBUFR sucessivamente.                    #
!#  A cada chamada de READ_mbufr, uma nova mensagem e lida e o conteudo completo#
!#  desta mensagem sao retornados em sec1, sec3 e sec4                          #
!#                                                                              #
!#  Como apenas parte das variaveis sao utilizadas, as subrotinas de MFORMAT    #
!#  processam a selecao das variaveis BUFR (definidas em b_desc, armazenando-as #
!#  na matriz de observacoes OBS.                                               #
!#  A cada nova mensagem , os dados sao anexados em OBS e o conteudo de         #
!#  sec3 e sec4 dealocados                                                      #
!#                                                                              #
!#                                                                              #
!#  Caso os dados seja do tipo 3 (ATOVS) e feito um processamento de diluicao   #
!#  Aleatoria, que a cada 4 sondagens, seleciona-se 3 (elimina-se 1 )           #
!#                                                                              #
!#  Ao final do processo OBS contera todas os dados disponiveis e  nrows o      #
!#  numero de linas de dados em OBS                                             #
!#                                                                              #
!# Alem destes valores, sao retornados para cada linha de dados em OBS          #
!#    center(1:nrows)  - Codigo do centro gerador                               #
!#    btype (1:nrows)  - Tipo da observacao BUFR (Conforme tabela BUFR A)       #
!#    bsubtype(1:nrows)- Subtipo da observacao BUFR                             #
!#    ks(1:nrows)      - Numero sequencial que indica uma sondagem ou observacao#
!********************************************************************************
subroutine load1(un,flist,nf,btype,bsubtype,center,obs,ks,nrows)
!{ declaracao de variaveis de interface
   integer,intent(in)                        ::un       ! Unidade de leitura
   character(len=255),dimension(:),intent(in)::flist    ! Nomes dos arquivo BUFR p/ leitura
   integer,intent(in)                        ::nf       ! Numero de arquivos na lista
   integer,dimension(:),intent(out)          ::btype    ! Matriz com os tipo BUFR da observacao 
   integer,dimension(:),intent(out)          ::bsubtype ! Matriz com subtipo BUFR da obsercacao
   integer,dimension(:),intent(out)          ::center   ! Matriz com os codigos dos centros geradores
   real,dimension(:,:),intent(out)           ::obs      ! Matriz de observacoes
   integer,dimension(:),intent(out)          ::ks       ! Vetor que identifica o numero da sondagem
   integer,intent(out)                       ::nrows    ! Numero de observacoes em obs	
!}
!{ declaracao de variaveis locais
   integer :: f,nm,nrows0  !
   integer :: mbtype,mbsubtype,mcenter !..........Tipo, subtipo e centro gerador de uma mensagem 
   integer :: nbufr_obsmax !.....................Numero maximo de observacoes em BUFR
   integer :: qcexc !........................... Numero de obseervacoes excluidas em uma mensagem 
   integer :: qcexc_total !......................Totalizacao das observcoes excluidas 
   real    :: qcexc_media !.........................Media de dados excluidas por observacao 
   integer :: aux_nrows
   integer :: nrows_thinned
   character(len=255)::logfile
   integer :: ncut,nss,nmr
   integer :: iun
   integer :: err
   integer :: nprint  !............................Auxiliar para informacoes impressas na tela  
!}

!{ Inicializacao de variaveis	
   nbufr_obsmax=ubound(ks,1)
   nrows=0
   nrows_thinned=0
   ncut=0
   nprint=0
   iun=un
   qcexc_total=0
   nm=0
   nmr=0
   nss=0
   print *,"MSCANBUFR20:LOAD:1:start"
   call init_obsthinner(mobsmax,ncbt,ATOVS_grsize,map%c_lat,map%c_lon)
!}

! Abaixo sao realizados os seguintes passos:
!
! a)Abre cada um dos "nf" arquivos BUFR fornecidos. 
! b)Dentro de cada arquivo BUFR processa cada uma das mensagens (nm)
! c)Dentro de cada mensagem processa cada um dos subsets de dados
! d)Os subsets sao organizados em linhas de dados (nrows)
! e)Conforme o caso uma observacao pode ter uma ou mais linhas
! f) KS contem um numero sequencial que indentifica a observacao
!{
   do  F = 1, nf !Processa os dados para cada um dos nf arquivos 
      logfile=trim(flist(F))//".log"
      open (15,file=logfile,status="unknown")
      print *,":MSCANBUFR20: Arquivo=",trim(flist(F)) 
      Call OPEN_MBUFR(iun, flist(F))!......Abre arquivo BUFR
10      CONTINUE 
        nm=nm+1
        nrows0=nrows 
15      Call SCAN_MSCANBUFR(iun,mbtype,mbsubtype,mcenter,obs,ks,nrows,qcexc,err)

        !{ Verificacao de erros  
        if (((err>0).or.(nrows==0)).and.(ioerr(iun)==0)) then 
          nmr=nmr+1
          print *,":MSCANBUFR:Error=",err
          goto 15
        end if

        !{Elimina observacoes significativos  
        ! call cut_nonsigobs(obs,ks,nrows0,nrows,nss)
        !}

        if (ioerr(iun)/=0) goto 20 
        if (nrows>=nbufr_obsmax) then
          print *,":MSCANBUFR:Erro! Numero de observacoes em bufr execedeu o limite previsto!"
          nrows=nbufr_obsmax
        end if

        !{ Repete Btype, Bsubtype e Center da mensagem para todas as observacoes
         btype(nrows0+1:nrows)=mbtype
         bsubtype(nrows0+1:nrows)=mbsubtype
         center(nrows0+1:nrows)=mcenter
        !}

        !{ Totaliza o numero de observacoes excluidas por baixa confiabilidade
        qcexc_total=qcexc_total+qcexc
        !}

       !{Se for ATOVS executa a diluicao dos dados da matriz de observacoes 
        if (nrows>0) then
          IF (btype(nrows)==3) THEN
            aux_nrows=nrows
            call line_thinner(obs(:,:),nrows0+1,nrows,1,ncbt,4,ks)
            !call run_obsthinner(obs,(nrows0+1),nrows,ks)
            nrows_thinned=nrows_thinned+aux_nrows-nrows
          end if
        end if
        !}

        !{ Apresenta informacoes na tela enquanto executa
        if (nprint*1000<nrows) then
          nprint=nprint+1
          write(*,'( 1x,":MSCANBUFR20: Messages= ",i7.7," observations =",i7.7," Type=",i3)')nm,nrows,btype(nrows)
        end if
        !}
      if(nrows<nbufr_obsmax) goto 10
20    CONTINUE
      print *,":MSCANBUFR20:Leitura concluida: " ,trim(flist(F))
      call Close_mbufr (iun)
      close(15)
    end do
    qcexc_media=0
    if (nrows>0) qcexc_media=real(qcexc_total)/real(nrows)
    print *,":MSCANBUFR20:Concluido"
    write(*,'( 1x,":MSCANBUFR:Number of read messages = ",i7.7," Number of read subsets  = ",i7.7)')nm,nrows
    if (nrows_thinned>0) then 
    write(*,'( 1x,":MSCANBUFR20:Number of thinned observations  = ",i6.6)')nrows_thinned
    end if
    if (nss>0) then 
    write(*,'( 1x,":MSCANBUFR20:Number of non significatives observations  = ",i7.7)')nss	
    end if
    if (qcexc_media>0) then 
    write(*,'( 1x,":MSCANBUFR20:Mean of excluded data by confidence index  = ",f7.3)')qcexc_media
    end if
    if (ncut>0) then 
    write(*,'( 1x,":MSCANBUFR20: Number of excluded messages by duplicity = ",i7.7)')ncut
    end if
    if (nmr>0) then 
    write(*,'( 1x,":MSCANBUFR20: Number of excluded messages by codification errors  = ",i7.7)')nmr
    end if
  
   call end_obsthinner
   print *,"MSCANBUFR20:LOAD:1:Done"
end subroutine

!#==============================================================================#
!# load     |     SUBROTINA PARA LEITURA E SELECAO DE DADOS BUFR          |SHSF #
!#------------------------------------------------------------------------------#
!# Tipo         : SUBROTINA DE ACESSO PUBLICO               	                #
!#------------------------------------------------------------------------------#
!#  Descricao:                                                                  #
subroutine load2(un_in,flist,nf,obsb,nobsb,idate)
!{ Variaveis da interface 
      integer,intent(in)                           ::un_in !Unidade de leitura
      character(len=255),dimension(:),intent(inout)::flist !Nomes dos arquivo BUFR p/ leitura
      integer,intent(inout)                        ::nf    !Numero de arquivos na lista
      type(obsbtype),intent(out)                   ::obsb  !Observacoes
      integer,intent(out)                          ::nobsb !Numero de observacoes em obsb
      real*8,intent(out)                           ::idate !Menor data do arquivo BUFR lido

!}


!{ Variaveis locais
      real*8::jdate !...........................Data juliana (dias e fracoes de dias ) 
      integer::un,i,auxkt,j,nrows,flag
      integer::nobsv          !.Numero de observacoes validas
      integer::yy,mm,dd,hh,nn !.Data (ano, mes,dia, hora,minuto) 
      integer::flag_wind      !.Bandeira para conversao de valores de vento 
      integer::flag_virt      !.Bandeira para conversao de valores de temp. Virtual 
      integer::flag_rmix      !.Bandeira para conversao de valores de razao de mistura 
      integer::flag_q         !.Bandeira para conversao de valores de Umide especifica 
      integer::flag_pdwpk     !.Bandeira indicativa da disponibilidade de pressa e ponto de orvalho
      real   :: p             !.Pressao atmosferica
      real   :: e             !.Tensao do vapor 
      integer :: err          !Error
!}
!{ Inicializando Variaveis 
    un=un_in
    nobsv=0
    flag_wind=map%c_u * map%c_v * map%c_wdir * map%c_wvel
    flag_pdwpk=map%c_pres*map%c_dwpk
    flag_virt=flag_pdwpk* map%c_virt
    flag_rmix=flag_pdwpk* map%c_mixr
    flag_q=flag_pdwpk* map%c_q
!}
   print *,":MSCANBUFR20:LOAD:2:start"

!{  Utilizando load1 para fazer a leitura dos BUFR
    call load1(un,flist,nf,btype,bsubtype,center,B,ks,nrows)
!}

  print *,":MSCANBUFR20:LOAD:2:Converting data nrows=",nrows," nkt=",nkt
!----------------------------------------------
! Aqui inicia a conversao dos dados de 1 a nrows
!-----------------------------------------------
!{
    allocate(obsb%id(1:nrows),obsb%obs(1:nrows,1:nkt),STAT=ERR)
    if (err>0) then 
      print *,":MSCANBUFR20: Error during the obsb allocation"
      stop
    end if 

 
    do i=1,nrows
      obsb%id(i)%kx=getobstype_obscodes(btype(i),bsubtype(i),B(i,:))   
        !{Obtendo colunas e valores de data e hora 
         yy=b(i,map%c_year)
         mm=b(i,map%c_month)
         dd=b(i,map%c_day)
         hh=b(i,map%c_hour)
         nn=b(i,map%c_min)
         
       !}

      !{ Somente dados validos (com kx) 
      if (len_trim(obsb%id(i)%kx)>0) then 
        obsb%id(i)%ks=ks(i)
        obsb%id(i)%lat=b(i,map%c_lat) 
        obsb%id(i)%lon=b(i,map%c_lon)
        obsb%id(i)%alt=b(i,map%c_alt) 
!      if((map%c_wmo1*map%c_wmo2)>0) then  
        obsb%id(i)%wmo=b(i,map%c_wmo1)*1000+b(i,map%c_wmo2)
!      else 
!        obsb%id(i)%wmo=0
!      end if 

       !{  Atribuindo P=2000 a niveis de superficie
        if (abs(b(i,map%c_lev)/100.0)>2000) then
          obsb%id(i)%lev=2000
        else
          obsb%id(i)%lev=b(i,map%c_lev)/100.0
        end if
       !}
       
       !{ Processando Data e Hora Obtendo tempo diferencial em horas (hora z)
       !{Assumindo hora zero caso nao fornecida
         if (hh<0) hh=0
         if (nn<0) nn=0
        !}
        !{ Obtendo tempo diferencial
          if (jdate00<100)  then  
            idate=fjulian(yy,mm,dd,hh,nn,0,.false.) 
            if (idate>100) then 
              jdate00=fjulian(yy,mm,dd,0,0,0)
              print *,":MSCANBUFR20:LOAD:2:Initial date",iso_8601_Basic(jdate00)
            end if

          end if
          jdate=fjulian(yy,mm,dd,hh,nn,0,.false.)
          if (jdate==0) then 
            print *,"Invalid date in the Observation i (i,lat,lon,kx)",i,obsb%id(i)%lat,obsb%id(i)%lat,obsb%id(i)%kx
            print *,"date=",yy,mm,dd,hh,nn
            obsb%id(i)%kx=""
            jdate=idate
          end if 
          if (idate>jdate) idate=jdate
          obsb%id(i)%tdif=(jdate-jdate00)*24.0
         !}
         !{ Atribuindo data e hora a estrutura de saida
          obsb%id(i)%year=yy
          obsb%id(i)%month=mm
          obsb%id(i)%day=dd
          obsb%id(i)%hour=hh
          obsb%id(i)%minu=nn
          !}
        !}
        !----------------------------------------------------------------
        ! Processamento de variaveis omitidas e/ou derivadas 
        !---------------------------------------------------------------
        !  Caso as seguintes variaveis nao estejam disponiveis, porem 
        !  suas colunas tenham sido identificadas, entao processa o 
        !  calculo destas variaveis 
        !----------------------------------------------------------------	
        !{ Calculo das componentes U e V do vento
        if(flag_wind>0) then
          if (b(i,map%c_u)==missing) then
            if ((b(i,map%c_wdir)/=missing).and.(b(i,map%c_wvel)/=missing)) then 
              call tdvuv(b(i,map%c_wdir),b(i,map%c_wvel),b(i,map%c_u),b(i,map%c_v))
            else
              b(i,map%c_u)=missing;b(i,map%c_v)=missing
            end if
          end if
        end if
        !}
       
       
        !{ calculo de razao de mistura, umidade especifica e temperatura virtual
        if (flag_pdwpk>0)then 
         
          p=missing
          e=missing
          if (b(i,map%c_lev)/=missing) p=b(i,map%c_lev)/100.0
          if (b(i,map%c_pres)/=missing) p=b(i,map%c_pres)/100.0
          if (b(i,map%c_dwpk)/=missing) then 
            e=pvapor(b(i,map%c_dwpk)+273.16)
          end if  
        
          !{ Razao de Mistura
          if (flag_rmix>0) then 
            if ((b(i,map%c_mixr)==missing).and.(p/=missing).and.(e/=missing)) then
              b(i,map%c_mixr)=RMIST(E, p)
            else
              b(i,map%c_mixr)=missing 
            end if
          end if
          !}
       
          !{ Umidade especifica  
          if (flag_q>0) then 
            if ((b(i,map%c_q)==missing).and.(p/=missing).and.(e/=missing)) then
              b(i,map%c_q)=q_humid1(E,p)
            else 
              b(i,map%c_q)=missing
            end if
          end if
         !}
         
         !{ Temperatura Virtual
          if (flag_virt>0) then
           if ((b(i,map%c_virt)==missing).and.(p/=missing).and.(e/=missing).and.(b(i,map%c_tmpk)/=missing)) then
             b(i,map%c_virt)=Tvirtw_Kelvin(b(i,map%c_tmpk), (RMIST(e,p)/1000.0))
           else
             b(i,map%c_virt)=missing
           end if
          end if
          !}
        end if
        !}
       
        !{ Converte NIVEL DE VOO para Nivel isobarico
        if (btype(i)==4) then 
          if((b(i,map%c_lev)==missing)) then 
            b(i,map%c_lev)=pressure_isa(b(i,map%c_geo))
          end if
        end if
        !}
        
        !{ Identificacao do KT e Transposicao de variavies  para obsb
        obsb%obs(i,:)=missing
        flag=0
        do j=1,ncbt
          if (b(i,j)/=missing) then
            auxkt=getktindex(btype(i),j)
            if (auxkt>0) then 
              obsb%obs(i,auxkt)=b(i,j)
              flag=1
            end if
          end if
        end do !j 
        if (flag==1) nobsv=nobsv+1
        !}
      end if !} Dados validos
      end do !i
    !}
!}
!{ Verifica numero de niveis
    obsb%id(1)%nlev=1
    do i=2,nrows
      if (obsb%id(i)%ks==obsb%id(i-1)%ks) then 
        obsb%id(i)%nlev=obsb%id(i-1)%nlev+1
      else
        obsb%id(i)%nlev=1
      end if
    end do
!{ Finalizacao desta rotina
    print *,":MSCANBUFR20:Num_total de obsvacoes=",nrows
    print *,":MSCANBUFR20:Num de observacoes validas=",nobsv
    print *,":MSCANBUFR20:nkt=",nkt
    nobsb=nrows
    deallocate(B)
    load2closed=.true.
    print *,grdate(idate)
    print *,":MSCANBUFR20:LOAD2:Done"
  
!}
end subroutine


!#==============================================================================#
!# CutObsDup                                                              SHSF  #
!#                                                                              #
!#------------------------------------------------------------------------------# 
!# Tipo         : SUBROTINA DE ACESSO PUBLICO               	                #
!# Dependencias :                                                               #
!#------------------------------------------------------------------------------#
!#  Descricao:                                                                  #
!#                                                                              #
!#                                                                              #
!#                                                                              #
!#   Esta subrotina elimina dados duplicados na matriz de observacao            #
!#                                                                              #
!#------------------------------------------------------------------------------#	
subroutine CutObsDup_bufr2obs(M,L1,L2,KS,NCUT)
!{ Variaveis da Interface
   REAL, DIMENSION(:,:),INTENT(INOUT) ::M    ! Matriz de entrada
   INTEGER,INTENT(IN)::L1                    ! Linha inicial da matriz
   INTEGER,INTENT(INOUT)::L2                 ! Linha final da matriz antes e depois da eliminacao  
   integer,DIMENSION(:),intent(inOUT)::kS    !  Coluna de controle
   integer,intent(out)::ncut                 ! Numero de duplicatas eliminadas
 !}
!{ Variaveis LOCAIS
   integer::C1,C2
   integer:: i,j,k
   logical:: nodif
!}
   J=L1
   ncut=0                                    
   C1=1
   C2=UBOUND(M,2)
   PRINT *,"Procurando duplicatas.."

   do I=L1,L2
      IF (KS(J)>0) THEN 
        DO J=I+1,L2
          nodif=.true.
          if (ks(j)>0) then 
            DO K=C1,C2
              IF (M(I,K)/=M(J,K)) THEN 
                NODIF=.false.
                EXIT 
              end if
            end do !K
            !{Caso todas as colunas forem iguais,elimina a linha da matriz
            If (nodif) then 
              KS(J)=0
              M(J,C1:C2)=missing
              ncut=ncut+1
            end if
            !}
          end if
        end do !j
      END IF
    end do !I
END SUBROUTINE 


subroutine cut_nonsigobs(obs,ks,nrows0,nrows,nns)
!------------------------------------------------------------------------------|
!==============================================================================|    
! CUT_NONSIGOBS | Elimina dados nao significativos                       |SHSF |
!------------------------------------------------------------------------------| 
! Esta rotina contabiliza as variaveis C nulas de  cada observacao OBS(L,C)    |
! Caso as variaveis C principais sejam nulas, a observacao e eliminada de obs  |
!------------------------------------------------------------------------------|
!{ Variaveis de interface
   real,dimension(:,:),intent(inout)::obs
   integer,dimension(:),intent(inout)::ks
   integer,intent(in)::nrows0  ! Numero de linhas na passagem anterior de obs
   integer,intent(inout)::nrows ! Numero de linhas de obs
   integer,intent(inout)::nns
!}
!{ Variaveis locais
   integer::i,n,cnn
!}
   n=nrows0
   do i=nrows0+1,nrows
     !{ Verifica se o nivel � valido 
      cnn=0
      !if (obs(i,HGHT_ID)>0) cnn=cnn+1
      if (obs(i,map%c_lev)>0) cnn=cnn+1
      !if (obs(i,TMPK_ID)>0) cnn=cnn+1
      !if (obs(i,DWPK_ID)>0) cnn=cnn+1
      !if (obs(i,TVRK_id)>0) cnn=cnn+1
      !if (obs(I,dvel_id)>0) cnn=cnn+1
      !if (obs(I,vvel_id)>missing) cnn=cnn+1
      !if (obs(I,WWND_ID)>missing) cnn=cnn+1  
      !if (obs(i,VSIG_ID)>missing) cnn=cnn+1
      !if (obs(i,PRES_ID)>missing) cnn=cnn+1
      !IF (obs(i,PMSL_ID)>missing) cnn=cnn+1 
      if (cnn>=1) then 
        n=n+1
        obs(n,:)=obs(i,:)
        ks(n)=ks(i)
      else
        nns=nns+1
      end if
    end do
    nrows=n
 end subroutine

 function jdate00_mscanbufr20()
   real*8::jdate00_mscanbufr20
   jdate00_mscanbufr20=jdate00
 end function 
 end module
