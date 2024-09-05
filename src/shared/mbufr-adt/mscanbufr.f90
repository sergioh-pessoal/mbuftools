module mscanbufr
!------------------------------------------------------------------------------
!                                mscanbufr
!   Modulo para varrer e localizar variaveis dentro de um arquivos BUFR 
!        (Module to scan and to localize variables in BUFR files )
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
! Historico
!  20100519 SHSF introducao da multiplicacao da mascara de bits para conversao
!                de valores em BUFR
!  20110217 SHSF Remocao de selecao do tipo de bufr a ser decodificado (decodifca todos!)

 use mformat20
 use mbufr
 use datelib
 use stringflib
 
 private
    public init_mscanbufr
    public scan_mscanbufr
 
 integer                       :: ncbtmax    !.Numero maximo de conversons na matriz cbt 
 type(corbufrmat),allocatable  ::cbt(:)      ! Vetor de correspondencia entre descritores BUFR e colunas da matriz de dados
 integer,dimension(0:255,0:255)::minqc       !.Valor minimo para controle de qualidade 
 real*8                        ::date1,date2

 interface init_mscanbufr
    module procedure init1
    module procedure init2
    module procedure init3
 end interface 
CONTAINS

 !------------------------------------------------------------------------------
 ! Correspondencia entre os descritores BUFR e as colunas da matraiz de dados
 ! Correspondence btw BUFR descriptions and coluns of data matrix
 !------------------------------------------------------------------------------
 ! Isto e feito por intermedio de uma matriz texto (cbtc) que contem 
 ! os descritores das variaveis que deseja-se extrair do BUFR, assim
 ! como as convercoes de unidades que serao feitas no momento da
 ! leitura. 
 !
 ! A conversao de unidade e estabelecida por um fator multiplicativo (Mult) 
 ! e um Valor de referencia (Ref) informado em cbtc como se seguie
 !
 !  cbtc(i)=DESCRITOR*MULT+REF
 !
 ! Esta parte DESCRITOR, MULR e REF sao separados e colocados em CBT para
 ! que possa ser usado. 
 !
 !------------------------------------------------------------------------------
 subroutine INIT1(cbtc,ncbtc,jdate1,jdate2)

 !{Variaveis de interface
   character(len=*),dimension(:),intent(in)::cbtc
   integer,intent(in)::ncbtc
   real*8,intent(in)::jdate1,jdate2
!}
!{ Variaveis Locais
   character(len=255),dimension(ncbtc*2)::desc,E1
   integer::k,i,NE,J
!}
!{ Iniciando variaveis
    ncbtmax=ncbtc*2 ! Por simplicidade assumimos que o numero de colunas  nunca sera maior
                    ! que o dobro de argumentos passados em CBTC
    allocate(cbt(ncbtmax))
    date1=jdate1
    date2=jdate2
    cbt(:)%mult=1
    cbt(:)%ref=0
    cbt(:)%d=999999 
    cbt(:)%bitm=0
    K=0
    DO I=1,ncbtc
      CALL split(CBTC(I),",",E1,NE)

      DO J=1,NE
        K=K+1
        DESC(K)=E1(J)
	
        cbt(k)%d=VAL(DESC(K))
        cbt(k)%col=I 
      END DO
    END DO
!}
!{ Obtendo o fator multiplicativos MULT
    DO I=1,K
      CALL split(DESC(I),"*",E1,NE)
      IF (NE==2) cbt(I)%mult=VAL(E1(2))
    END DO
    DO I=1,K
      CALL split(DESC(I),"/",E1,NE)
      IF (NE==2) cbt(i)%mult=1/VAL(E1(2))
    END DO
!}
!{Obtendo valores de Referencia 
    DO I=1,K
      CALL split(DESC(I),"+",E1,NE)
      IF (NE==2) cbt(i)%ref=VAL(E1(2))
    END DO
    DO I=1,K
      CALL split(DESC(I),"-",E1,NE)
      IF (NE==2) cbt(i)%ref=-VAL(E1(2))
    END DO
  !{Obtendo mascara de bits
    DO I=1,K
      CALL split(DESC(I),"&",E1,NE)
      IF (NE==2) cbt(i)%bitm=int(VAL(E1(2)))
    END DO
     
    do i=1,k
      write(*,66)i,cbt(i)%d,cbt(i)%col,cbt(i)%MULT,cbt(i)%REF,cbt(i)%bitm
      66 format(":MSCANBUFR:Init:",i3,"> Descriptor=",i6.6," Col=",i3.3,"  Mult=",f10.4,"  Ref=",f10.4," bitm=",i4)
    END DO
   ! stop
!}
   ncbtmax=k
!}
!{ Variaveis globais Valor minimo de corte por centro gerador e por tipo de dado em BUFR
   minqc(:,:)=60  !................. Minimo Valor de confiabilidade (Controle de qualidade )
 !}


 end subroutine

!------------------------------------------------------------------------------
!init2 !
!-----------------------------------------------------------------------------
!  Inicializa o modulo mscambufr para proceder a varredura dos dados BUFR 
!  E colocacao dos mesmos em uma matriz de dados de ncols colunas.
!
!  Para esta rotina é fornecido: 
!  a)  o NAMELIST "bufrvar" que fornece as variaveis BUFR que serao lidas e seus respectivos descritores
!  b) jdate1 e jdade2 ! Datas iniciais e finais dos dados que serao lidos (calendario julino em dias/fracoes de dias)
!
!  Retorna o numero de colunas (ncols) 
!  
!------------------------------------------------------------------------------
! Chamada por : Init_observer
!
!
 subroutine init2(nmlfile,jdate1,jdate2,ncols)
 !{variaveis de interface
   character(len=*),intent(in)::nmlfile
   real*8,intent(inout)::jdate1
   real*8,intent(inout)::jdate2
   integer,intent(out)::ncols ! Numero de colunas da matriz de observacao que serao utilizadas

  !}
  !{ Variaveis locais
   character(len=80),dimension(200)::cbt
   integer::ncbt
   character(len=80),dimension(400)::values
   integer                         ::nelements,i,j
  !}
  !{namelists
   
   call read_config(12,nmlfile,"bufrvar",values,nelements)

   ncbt=0
   j=0
   do i=1,nelements

     if (index(values(i),"@")>0) then 
        
       j=val(values(i))
       cbt(j)=""
       if (ncbt<j) ncbt=j
       
     elseif(len_trim(cbt(ncbt))>0) then
       cbt(j)=trim(cbt(j))//","//trim(values(i))
     else 
      cbt(j)=trim(values(i))
     end if
   end do 
  !}
   
  ! do i=1,ncbt
  !  print *,i,trim(cbt(i))
  ! end do
  !  stop
  call INIT1(cbt,ncbt,jdate1,jdate2)
   ncols=ncbt
 end subroutine


!------------------------------------------------------------------------------
!init3
!------------------------------------------------------------------------------


 subroutine init3(jdate1,jdate2,ncols)
  !{Variaveis da interface
   real*8,intent(inout)::jdate1
   real*8,intent(inout)::jdate2
   integer,intent(inout)::ncols
  !}
  !{variaveis locais
   character(len=1024)::nmlfile
   character(len=1024)::local_tables
   integer::i
  !}
  !{ Obtem diretorio das tabelas BUFR
    call getenv("MBUFR_TABLES",local_tables)
    i=len_trim(local_tables)
    if ((local_tables(i:i)/=char(92)).and.(local_tables(i:i)/="/")) then 
      if (index(local_tables,char(92))>0) then 
        local_tables=trim(local_tables)//char(92)
      else
        local_tables=trim(local_tables)//"/"
      end if
    end if
  !}
  
   nmlfile=trim(local_tables)//"/mscanbufr.cfg"
   call init2(nmlfile,jdate1,jdate2,ncols)

 end subroutine


!#=============================================================================#    
!# SCAN_MSCANBUFR |  SUBROTINA PARA LEITURA E SELECAO DE DADOS BUFR      |SHSF #
!#-----------------------------------------------------------------------------# 
!# Tipo         : SUBROTINA DE ACESSO PUBLICO                                  #
!# Dependencias :                                                              #
!#  a) MBUFR (open_mbufr, read_mbufr)                                          #
!#  b) MFORMAT20 (format_mtabqc, format_tabqc)                                 #
!#  c) line_thinner                                                            #
!#                                                                             #
!#-----------------------------------------------------------------------------#           
!#  Descricao:                                                                 #
!#  Este subrotina processa a leitura de um ou mais arquivos BUFR, extrai as   #
!#  variaveis de interesse definadas em cbt armazenando-as em  obs(:,:)        #
!#                                                                             #
!#  Para fazer a leitura de todas as mensagens BUFR dentro de um arquivo       #
!#  BUFR e utilizada a sub-rotina READ_MBUFR sucessivamente.                   #
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
subroutine scan_mscanbufr(un,btype,bsubtype,center,obs,ks,nrows,nobs_qcexc,err)

!{ declaracao de variaveis de interface

 integer,intent(in)              :: un       !Unidade de leitura
 integer,intent(out)             :: btype    !Matriz com os tipo BUFR da observacao 
 integer,intent(out)             :: bsubtype !Matriz com subtipo BUFR da obsercacao
 integer,intent(out)             :: center   !Matriz com os codigos dos centros geradores
 real,dimension(:,:),intent(out) :: obs      !Matriz de observacoes
 integer,dimension(:),intent(out)::ks        !Vetor que identifica o numero da sondagem
 integer,intent(inout)           :: nrows    !Numero de observacoes em obs	
 integer,intent(out)             ::nobs_qcexc !Numero de observacoes excluidas por baixa confiabilidade
 integer                         :: err 

!}

!{ Declaracao das variaveis utilizadas em read_mbufr 

 type(sec1type) :: sec1
 type(sec3type) :: sec3
 type(sec4type) :: sec4
 integer        :: BUFR_ED         
 integer        :: MBYTES 
 integer        :: nqcexc
 integer        :: iun
 integer        :: i
 type(selecttype),dimension(13)::select

!} 

!{ declaracao de variaveis locais
 integer :: nrows0        !.Linha Inicial dos dados de uma mensagem 
 integer :: nbufr_obsmax  !.Numero maximo de observacoes em BUFR
 integer :: nrows_thinned ! Numero de Linhas diluidas
 real*8  ::sec1jdate      !.Variavel auxliar p/ Data juliana da secao 1 do BUFR
 integer :: minqc2        !.Variavel auxiliar minimos do controle de qualidade 

!}

!{ Inicializacao de variaveis
   nbufr_obsmax=ubound(ks,1)
   nrows_thinned=0
   iun=un
   select(1)%btype=0;select(1)%bsubtype=any
   select(2)%btype=1;select(2)%bsubtype=any
   select(3)%btype=2;select(3)%bsubtype=any
   select(4)%btype=3;select(4)%bsubtype=any
   select(5)%btype=4;select(5)%bsubtype=any
   select(6)%btype=5;select(6)%bsubtype=any
   select(7)%btype=6;select(7)%bsubtype=any
   select(8)%btype=7;select(8)%bsubtype=any
   select(9)%btype=8;select(9)%bsubtype=any
   select(10)%btype=9;select(10)%bsubtype=any
   select(11)%btype=10;select(11)%bsubtype=any
   select(12)%btype=11;select(12)%bsubtype=any
   select(13)%btype=12;select(13)%bsubtype=any
!}



!obs(:,:)=null 
! Abaixo sao realizados os seguintes passoes:
!
! c)Dentro de cada mensagem processa cada um dos subsets de dados
! d)Os subsets sao organizados em linhas de dados (nrows)
! e)Conforme o caso uma observacao pode ter uma ou mais linhas
! f) KS contem um numero sequencial que indentifica a observacao
!{
   Call READ_MBUFR(iun,120000,sec1,sec3,sec4,bUFR_ED,MBYTES,err)
   ! Se nao houver erro de leitura ou se nao tiver chegado ao final do arquivo
   ! processa a separacao das variaveis meteorologicas de interesse, 
   ! guardando-as na matriz de variaveis B
   !{
     !{ Obtem a data juliana da secao 1
     !  caso nao esteja ma faixa, entao ignora os dados


      sec1jdate=fjulian(sec1%year,sec1%month,sec1%day,sec1%hour,0,0)
      if (sec1jdate==0) sec1jdate=date1
      if (ioerr(iun)==0)Then
      If (sec3%ndesc>0)then 
        if (err/=0) then

          write(15,'(" Erro =",i3," in Bufrtype =",i3)')err,sec1%btype

        elseif (((sec1jdate>=date1).and.(sec1jdate<=date2)).or.(date1==date2)) then
          minqc2=minqc(sec1%center,sec1%btype)
          nrows0=nrows
          ! Caso seja dados do tipo multinivel, Organiza os dados com
          ! format_mtabqc. Caso seja de niveis simples, use format_tabqc
          if ((sec1%btype==2).or.(sec1%btype==3)) then
            call format_mtabqc(sec4,sec3%nsubsets,minqc2,cbt,ncbtmax,nrows,obs,ks,nqcexc,err)
          else
            call format_tabqc(sec4,sec3%nsubsets,minqc2,cbt,ncbtmax,nrows,obs,nqcexc,err)
            do i=nrows0,nrows;ks(i+1)=i+1;end do
          end if
        else
          err=2 ! Dados fora da janela de tempo ou dados com erro  
        end if
      else
        err=1 ! erro na secao 3 ou fim de arquivo 
      end if
      end if
      call redefine_subtype(sec3,sec1%btype,sec1%bsubtype)
      btype=sec1%btype
      bsubtype=sec1%bsubtype
      center=sec1%center
      nobs_qcexc=nqcexc
     
      DEallocate(sec4%r,sec4%d,sec4%c,sec3%d)

end subroutine

!------------------------------------------------------------------------------|
!==============================================================================|    
! mbufr_unlink|                                                          |SHSF |
!------------------------------------------------------------------------------| 
subroutine unlink_mscanbufr(un)
	integer,intent(inout)::un
	call close_mbufr(un)
	deallocate(cbt)
end subroutine

end module
