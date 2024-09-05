!******************************************************************************
!*                                  MFORMAT40                                 *
!*                                                                            *
!*         Subroutines to Select and Reorganize Meteorological data           *
!*                     read by MBUFR-ADT module                               *
!*                                                                            *
!*----------------------------------------------------------------------------* 
!* HISTORICO da Versao :                                                                 
!*                 
!*  11/09/2005 - SHSF - Otimizacao da ininicalizacao da matriz obs em 
!*               format_tabqc e format_mtabqc
!*
!*  2007-01-18 - SHSF - Modificacao das dimensoes da matriz de observacoes     
!*               em todas as subrotinas, i.e.OBS(:,:,:) passa a ser OBS(:,:)
!*
!*  2007-02-01 - SHSF - Nova versao com solucao de descritores ambiguos
!*  2007-03-21 - SHSF - Corrigido erro na identificacao do numero de colunas de obs
!*                      em format_mtabqc (substituido ncbtmax por nobscols) 
!*  2008-03-10 - SHSF - Incluido teste em FORMAT_MTABQ para identificar o 
!*                      descritor de coordenada vertical. Somente alguns descritores
!*                      sao aceitos. caso o template tiver um descritor invalido
!*                      este rotina retorna erro
!* 2009-01-18 - SHSF -  Modificado teste de nivel inicial (levini) para ser robusto a niveis 
!*                      intermediarios missing. Incluido teste do descritor 0-08-003
!* 2010-05-19 - SHSF - Incluido multiplicacao por mascara de bits
!* 2010-05-28 - SHSF - Revisao da multiplicacao por mascara de bits
!* 2011-10-07 - SHSF - Revisao de bug na atualizacao de niveis verticais em mtabqc
!* 2013-01-04 - SHSF - Restruturacao do modulo para utilizar uma estrutura de vaiavel que trata melhor
!                      os dados que sao null e que tem valor. Isso deve otimiza o processamento e dar 
!                      uma solucao mais elegante de desenvolvimento, onde utiliza um tipo de dados 
!                      (single data type) que é composto de um valor real(r) e um logico (l) L=.true. quando
!                      r foi definido. 
!* 2016-03-24 - LAA  - Adicionado para radiancia descritor: TOVS/ATOVS/AVHRR INSTRUMENTATION CHANNEL NUMBER (CODE TABLE 2150)
!* 2016-03-30 - LAA  - Mudanças realizadas na subrotina de leitura dos arquivos de niveis (radiancia) na verificacao da 
!                      significancia vertical pq as radiancias do Nicolas nao tem essa opcao no subset dos dados e
!                      foi comentado tambem o elseif da verificacao.
!* 2016-12-02 - LAA  - Adicionado para radiancia IASI descritor: 005042-CHANNEL NUMBER (NUMERIC)
!* 2017-10-17 - SHSF - Modificado nome para mformat40 - Inclusao de rotinas de inicializacao e leitura de namelist
!* 2017-12-13 - SHSF - Introduzido parametro de verbosidade
!* 2017-12-29 - SHSF - Incluindo SID (station Identifier) na interface das rotinas do  mformat40. Também foi incluida
!                       uma subrotina para processar o SID internamente. No caso de dados de superficie ou de nivel simples,
!                       será atribuido um sid para cada subset. Em caso de dados de altitude. O Sid e repetido para cada 
!                       nivel vertical. O Numero de caracteres do SID, depende do como este é declarado no programa principal.
!                       A subrotina Station_IDentifier truncará o SID para caber na variável declarada   


module mformat40
use mbufr
use stringflib , only: val, split,read_config
implicit none 
private
public sec4qctype
public levidtype
public format_qc
public format_tab
public format_tabqc
public format_mtabqc
public corbufrmat
public redefine_subtype
public single
public csingle
public cnum
public init_mformat
!--------------------------------------------------------------------------------------------
! type corbufrtab
! Elementos de correlacao entre dados da secao4 Bufr e as colunas de uma matriz de observacoes
!
! Este tipo de variavel e usado para colocar dados bufr identificado por um descritor 
! da tabela B, organizados em colunas de uma matriz. O Elemento "d" indica o descritor
! bufr, "col" o numero da coluna em que se deseja colocar o respectivo dado meteorologico.
! mult e um fafor multiplicador de scala e ref é um fator de referencia. 
! 
! Mult e Ref sao usados para modificar a unidade original do dados em BUFR segundo a equacao 
!
!  Obs(i,j)=bufr(k,subset)*mult+ref
!
!  Alem desta opeacao, pode-se ter uma mascara de bits (bitm) para modificar o valor bit-a-bit
!  da expressao acima. O resultado e um valor inteiro dado por 
!
!  Obs(i,j)=iand(int(bufr(k,subset)*mult+ref),bitm)
!--------------------------------------------------------------------------------------------
type corbufrmat
   integer::d    !Descritor BUFR (TABELA B)
   integer::col  !Numero de uma coluna da matriz de observacoes
   real   ::mult !Fator de escala entre o dado BUFR e o valor na matriz de observacoes
   real   ::ref  !Fator de referencia entre o dados BUFR e o Valor na matriz de observacao
   integer::bitm !Mascara de bits  
end type 


type sec4qctype
   type(sec4type) ::obs
   integer,pointer::qc(:,:)
   integer,pointer::key(:,:)
   integer        ::nvars
end type

type levidtype
    real:: press
    integer::numlev
end type

!{ Single data type: Possui um valor real(r), e um longico(l). O campo longico l=.true.
!  quando o valor r foi definido  
type single
  real*8         :: r
  logical      :: l
  character*15 :: c
end type

 integer                         ::ncols  ! Numero de colunas da matriz de dados
 type(corbufrmat),allocatable    ::cbt(:) ! Regras de conversao de variaveis do BUFR para colunas da matriz 
 integer                         ::minqc  ! Minimo Valor de confiabilidade (Controle de qualidade )
 integer                         ::ncbtmax ! Numero maximo de regras de atribuicao de colunas da matrax
 
 Real(kind=realk)  :: Null        !valor indefinido 
 Real(kind=realk)  :: Null_mbufr  !Valor  indefinido do mbufr
                                  !Nota: Embora mbufr esteja trabalhando com kind=realk, 
                                  !O valor indefinido ainda esta com kind=4 
 integer            :: verbose    !Verbosidade (0=Minimo,1=baixo,2=medio,3=maximo)
CONTAINS

!------------------------------------------------------------------
! init       |Subrotina de inicializacao
! ----------------------------------------------------------------
!
!-----------------------------------------------------------------
 subroutine init_mformat(nmlfile,minqc_in,verbose_in,ncols_out,missing)
 !{variaveis de interface
   character(len=*),intent(in)::nmlfile          ! Nome do arquivo (namelist)
   integer,         intent(in)::minqc_in         ! Valor minimo para criterio de confiabilidade (%) Defaut=60
   integer,         intent(in)::verbose_in          ! Verbosidade
   integer,        intent(out)::ncols_out        ! Numero de colunas da matriz
   real(kind=realk),optional,intent(in)::missing ! Configura valor indefinido para ser usado na matrix 
                                                 ! Se ausente será usado o valor da funcao undef do mbufr  
 !}
 
 !{ Variaveis locais
   character(len=80),dimension(400)::values
   integer                         ::nelements,i,j
   character(len=80),dimension(400)::cbtc
   integer                         ::ncbt
 !} 
 
 !{ Configura valor indefinido
   null_mbufr=undef()
   if (present(missing)) then
     null=missing
   else 
      null=undef()
   end if
 
 !}
 verbose=verbose_in
 ! Configura controle de qualidade minimo
   minqc=minqc_in
   
 ! ---------------------------------------------------------------------
 ! Ler configuracao do namelist e chama a inicializacao da tabela CBT
 !----------------------------------------------------------------------
 !{ 
   call read_config(12,nmlfile,"bufrvar",values,nelements)
   ncbt=0
   j=0
   do i=1,nelements

     if (index(values(i),"@")>0) then 
        
       j=val(values(i))
       cbtc(j)=""
       if (ncbt<j) ncbt=j
       
     elseif(len_trim(cbtc(ncbt))>0) then
       cbtc(j)=trim(cbtc(j))//","//trim(values(i))
     else 
      cbtc(j)=trim(values(i))
     end if
   end do 
   
   call INIT_cbt(cbtc,ncbt)
   ncols=ncbt
   ncols_out=ncbt
 !}
 end subroutine

!------------------------------------------------------------------------------|
! init_cbt   |Inicializa CBT - Matriz de variaveis e regras de processamento   |
! -----------------------------------------------------------------------------|
!   CBT associa a cada variavel a ser localizada no BUFR um conjunto de        |
!       parametros a serem aplicados durante o processo de leitura             |
!       Estes parametros contem o codigo BUFR (tabela BUFR B) e demais         | 
!      parametros para realizar a conversao de unidades necessaria a patroniza |
!      cao de variaveis em relacao ao padrao interno adotado                   | 
!------------------------------------------------------------------------------|
!--------------------------------
! Processa Variaveis do namelist 
!-------------------------------- 
 subroutine INIT_cbt(cbtc,ncbtc)

 !{Variaveis de interface
   character(len=*),dimension(:),intent(inout)::cbtc
   integer,                      intent(inout)::ncbtc
!}
!{ Variaveis Locais
   character(len=255),dimension(ncbtc*2)::desc,E1
   integer::k,i,NE,J
!}
!{ Iniciando variaveis
    ncbtmax=ncbtc*2 ! Por simplicidade assumimos que o numero de colunas  nunca sera maior
                    ! que o dobro de argumentos passados em CBTC
    allocate(cbt(ncbtmax))
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
    if (verbose>1) then 
    do i=1,k
      write(*,66)i,cbt(i)%d,cbt(i)%col,cbt(i)%MULT,cbt(i)%REF,cbt(i)%bitm
      66 format(":MFORMA40:Init:",i3,"> Descriptor=",i6.6," Col=",i3.3,"  Mult=",f10.4,"  Ref=",f10.4," bitm=",i4)
    END DO
   end if
   ! stop
!}
   ncbtmax=k
!}



 end subroutine
!------------------------------------------------------------------------------|
! close   | Encerra mformat- Libera memoria alocada                            |
! -----------------------------------------------------------------------------|
!------------------------------------------------------------------------------|

 subroutine close_mformat40
   deallocate (cbt)

 end subroutine 
 
!-----------------------------------------------------------------------------|
!FORMAT_TAB | Extrai dados da secao 4 e organiza em colunas (TABELA)    |SHSF |
!-----------------------------------------------------------------------------|
!
!-----------------------------------------------------------------------------
! DEPENDENCIAS
!-----------------------------------------------------------------------------
!HISTORICO

subroutine format_tab(sec4,nsubsets,nrows,obs)
   !{ Interface
	type(sec4type)                              ::sec4     !Secao 4 de uma mensagem BUFR
	integer,                          intent(in)::nsubsets !Numero de subsets da secao 4
	integer,                       intent(inout)::nrows    !Numero de linhas de obs
	real(kind=realk),dimension(:,:), intent(out)::obs      !Tabela de valores.(1:nsubsets,1:ncols)
    !}	
    !{ variaveis locais
	integer :: i,j,bi,bj
	integer:: nobsmax
     !}

	nobsmax=ubound(obs,1)
	bi=0
	! nrows devera conter o numero de linhas lidos anteriormente
	! ou zero, caso seja a primeira vez que foi lido
	! 
	! Por seguranca, nesta parte verifica-se o valore de nrows
	! que for fornecido	e zera a matriz obs apenas para as posicoes
	! necessarias
	!{
	
	if ((nrows<0).or.(nrows>nobsmax)) nrows=0
	bi=nrows	   	
	
	!}
        !-----------------------------------------------------------------------
	! Inicio da organizacao das variaveis 
	!----------------------------------------------------------------------
	! Os valores em sec4r que sao lidos sao feitos ==null para indicar que
	! ja foram lidos anteriormente e que nao serao lidos uma proxima vez e 
	! A menos que haja outros descritores para fazer a leitura
	!{
   do i=1,nsubsets		
      bi=bi+1 
      obs(bi,:)=null
      if (bi<=nobsmax) then 
        do bj=1,ncbtmax
          do j=1,sec4%nvars 
            if ((cbt(bj)%d==sec4%d(j,i)).and.(obs(bi,cbt(bj)%col)/=null)) then          
                obs(bi,cbt(bj)%col)=fcbt(sec4%r(j,i),cbt(bj))
                if (obs(bi,cbt(bj)%col)/=null) then 
                  exit 
                  exit 
                end if
             end if
          end do  ! j
        end do ! bj
      else 
        print *,"Error! Format_tab: Number of observation out of range"
        print *,"Nobsmax=",nobsmax
        exit
      end if
    end do !i 
   
    nrows=bi

end subroutine format_tab





!-----------------------------------------------------------------------------!
!FORMAT_TABQC | Extrai dados da secao 4 e organiza em colunas (TABELA)  |SHSF |
!-----------------------------------------------------------------------------
!!*  Similar ao format_tab, porem, nesta sao excluidos os dados que possuem 
!*  flag de controle de qualidade abaixo do em falor minqc
!*
!*  Os dados que nao possuem o flag sao aceitos  
!-----------------------------------------------------------------------------
! DEPENDENCIAS: format_qc
!-----------------------------------------------------------------------------
!HISTORICO

	
subroutine format_tabqc(sec4,nsubsets,nrows,obs,sid,nqcexc,err)
!{ Variaveis da Interface
	type(sec4type),                 intent(inout)::sec4
	integer,                           intent(in)::nsubsets !Numero de subsets
	integer,                        intent(inout)::nrows    !Numero de linhas de obs
	real(kind=realk),dimension(:,:),intent(inout):: obs     !Tabela de valores (1:nsubsets,1:ndesc)
        character(len=*),dimension(:)  ,intent(inout):: sid     !Identificador da estacao
	integer,                          intent(out)::nqcexc   !Numero de dados excluidos pelo criterio de confiabilidade
	integer,                          intent(out)::err
!{
!{ variaveis locais
	integer            :: i,j,bi,bj,nsub
	integer            :: nobsmax
	type(sec4qctype)   ::sec4qc
	logical,allocatable::atrib(:,:)
!}
	nobsmax=ubound(obs,1)
	bi=nrows
	nqcexc=0	   	
	err=0
        ! Verificar se a matriz obs e suficientemente grande para
	! armazenar os dados. Caso nao seja retorna ao programa principal
	!{
	  
	  if ((nrows+nsubsets+sec4%nvars)>= (nobsmax)) then 
		  print *,"Error! Format_qc: Number of observation out of range"
		  print *,"Nobsmax=",nobsmax
	     err=1
	     return 
	  end if
        !}
	call format_qc(sec4,nsubsets,sec4qc)
	allocate(atrib(1:sec4qc%obs%nvars,1:nsubsets))
	atrib(:,:)=.false.
        nsub=nsubsets
      if (nsub+bi>nobsmax) then
       if (bi<nobsmax) then
         print *,":MFORMAT40: Warning! Number of observations is close to the limit  (Total,limit):",bi,nobsmax 
         nsub=nobsmax-bi
       else
         nsub=0
         print *,":MFORMAT40: Error! Number of observation exceeded the maximum value:",nobsmax 
       end if
      end if

     !-----------------------------------------------------------------------------------
     !Atribuir valor a coluna das matriz somente se passar pelas seguintes condicoes:
     !
     ! 1 - Se o valor BUFR nao foi atribuido anteriormente  ;
     ! 2 - Se o descritor for um dos descritores procurados 
     ! 3 - Se a coluna correspondente ainda nao receber valor (i.e.= null) 
     ! 4- Se o flag de controle de qualidade > minqc ou se nao tiver este flag (<0);
     ! 5 - Se o valor lido nao for "null".
     !------------------------------------------------------------------------------------
     !{
       do i=1,nsub	
        bi=bi+1 
        obs(bi,:)=null
        call Station_Identifier(sec4,i,sid(bi))            
          do bj=1,ncbtmax
            do j=1,sec4qc%obs%nvars
              if (.not. atrib(j,i)) then 
               if (cbt(bj)%d==sec4qc%obs%d(j,i)) then       
                 if (obs(bi,cbt(bj)%col)==null) then                                                         
                   if ((sec4qc%qc(j,i)>minqc).or.(sec4qc%qc(j,i)<0)) then

                     if (sec4qc%obs%r(j,i)/=null_mbufr) then 
                        obs(bi,cbt(bj)%col)=fcbt(sec4qc%obs%r(j,i),cbt(bj))
                        atrib(j,i)=.true.
                        exit 
                        exit
                      end if !5
                   elseif (cbt(bj)%d==sec4qc%obs%d(j,i)) then
                     nqcexc=nqcexc+1   
                   end if !4

                 end if !3
               end if !2
              end if !1
           end do !j
         end do !bj
      end do !i
     !}

 
      nrows=bi
      deallocate(atrib)	
      DEallocate(sec4qc%obs%r,sec4qc%obs%d,sec4qc%qc,Sec4qc%key)
end subroutine format_tabqc

!******************************************************************************
! FORMAT_MTABQC	| Extrai dados da secao 4 e organiza em colunas (TABELA)|SHSF |
!------------------------------------------------------------------------------
! Esta sub-rotina e similar a FORMAT_TABQC, porem, ao contrario da sub-rotina |
! anterior, esta considera que os dados BUFR sao organizados em multiplos     |
! niveis verticais, tal como no caso dos dados de radiossondagens e de ATOVS  |
!                                                                             |
! Entende-se que um novo nivel vertical como um nivel que inicia segundo um   |
! dos seguintes criterios:                                                    |
!   1) A  cada ocorrencia de 0-08-003                                         |
!   2) A  cada ocorrencia de descritor de coordenada vertical                 |
!                                                                             |
! O Descritor de coordenada vertical pode ser o 007004,007006 ou 007007       |
! A rotina seleciona um destes descritores automaticamente, com base no Num.  |
! ocorrencia dos mesmos                                                       |
! As variaveis que so aparecem na superficie, tais como Data, Hora e posicao  |
! geografica, sao repetidas em todas as linhas da matriz                      |
!                                                                             |
!                                                                             |
! IMPORTANTE:                                                                 |
!  Esta sub-rotina nao esta preparada para processar modelos BUFR que         |
!  possuem repeticoes de uma mesma variavel (mesmo derecritor BUFR) em mesmo  |
!  nivel isobarico.                                                           |
!                                                                             |
!  As repeticoes sao aceitas apenas para niveis isobaricos distintos,         |
!  Caso ocorra repeticao no mesmo nivel, as variaveis repetidas serao         |
!  superpostas                                                                |
!                                                                             |
!  Caso o modelo nao use os descritores 007004,007006 ou 007007, entao        |
!  esta rotina nao ira processar a extracao e retornara err = 1 (erro)        | 
!------------------------------------------------------------------------------

subroutine format_mtabqc(sec4,nsubsets,nrows,obs,ks,sid,nqcexc,err)

!{ Variaveis da interface 
 type(sec4type),                 intent(inout) ::sec4     !.Secao 4 do  MBUFR
 integer,                           intent(in) ::nsubsets !.Numero de subsets
 integer,                        intent(inout) ::nrows    !.Numero de linhas de obs
 real(kind=realk),dimension(:,:),intent(inout) ::obs      !.Tabela de valores (1:nsubsets,1:ndesc)
 integer,dimension(:),           intent(inout) ::ks       !.Numero da sondagem
 character(len=*),dimension(:),  intent(inout) ::sid      ! Identificacao da estacao 
 integer,                          intent(out) ::nqcexc   !.Numero de dados excluidos 
 integer,                          intent(out) ::err
 !}

!{ variaveis locais
 integer         :: i,j,bj,ii,l1,l2,l3,l4,l5
 integer         :: bi           ! Numero corrente de observacoes lidas (incluindo niveis verticais)
 integer         :: nobsmax      !.Numero maximo de observacaoes (linhas de obs)
 integer         :: nobscols     !.Numero de variaveis (colunas de obs)
 type(sec4qctype):: sec4qc       !.Secao 4 com controle de qualidade
 integer         :: levini       !.Nivel de superficie da sondagem 
 integer         :: multlev_flag !.Indica nivel de altitude
 integer         :: VSIG         !.Significancia Vertical: Ref. codigo 0-08-003
                                 ! 0-08-003
                                 ! Cod. Fig
                                 !  0    Surface
                                 !  1    Base of Satelite Sound
                                 !  2    Cloud top
                                 !  3    Tropopause
                                 !  4    Precipitable Water
                                 !  5    Sounding Radiances
                                 !  6    Mean Temparature
                                 !  7    Ozone
                                 !  8-62 Reserved
                                 !  63   Missing Value


 integer,dimension(ncbtmax) :: obslev
 integer        ::iks
 integer        ::vcoord         !Descritor que representa a coordenada vertical
 real           ::firstlev       !Valor de pressao do primeiro nivel isobarico 
 character(len=30)::aux_sid


!}
!{ Inicilizacao das variaveis
	firstlev=0
	nobsmax=ubound(obs,1)  
	nobscols=ubound(obs,2) 
	bi=nrows   
               
	nqcexc=0	   	
	obslev(:)=0
	err=0
	
	if (nrows<1) then
	    iks=0
	else 
           ks(bi)=0   
	   iks=ks(nrows)
	end if
 !}

 !{Verificar se a matriz obs eh suficientemente grande para armazenar os dados. Caso nao seja retorna ao programa principal
	  
	  if ((nrows+nsubsets+sec4%nvars)>= nobsmax) then 
		  print *,"Error! Format_mtabqc: Number of observation out of range"
		  print *,"Nobsmax=",nobsmax
	     err=1
	     return 
	  end if
 !}
 !{ Converte sec4 para sec4qc	
	
	call format_qc(sec4,nsubsets,sec4qc)
 !}
!---------------------------------------------------------------------------------------
! Procura pelo descritor que representa a coordenava vertical 
!  Este pode ser o 007004 (coordenada vertical de pressao, mas podera ser 
!  tambem a altitude. 007007 (coordenada vertical de altitude
!--------------------------------------------------------------------------------------
!{ Primeiro coonta o numero de ocorrencias do  descritor 007004	
	l1=0
	do j=1,sec4qc%obs%nvarS
		  	if (sec4qc%obs%d(j,1)==007004) l1=l1+1
	end do
!}
!{ Depois conta o numero de ocorrencias da descritor 007007 
	l2=0
	do j=1,sec4qc%obs%nvarS
		  	if (sec4qc%obs%d(j,1)==007007) l2=l2+1
	end do
!{ Depois conta o numero de ocorrencias da descritor 007006 
	l3=0
	do j=1,sec4qc%obs%nvarS
		  	if (sec4qc%obs%d(j,1)==007006) l3=l3+1
	end do

!adicionado para radiancia descritor: TOVS/ATOVS/AVHRR INSTRUMENTATION CHANNEL NUMBER (CODE TABLE 2150)
!{ Depois conta o numero de ocorrencias da descritor 2150 
	l4=0
	do j=1,sec4qc%obs%nvarS
		  	if (sec4qc%obs%d(j,1)==002150) l4=l4+1
	end do	
	
!adicionado para radiancia IASI descritor: 005042-CHANNEL NUMBER (NUMERIC)
!{ Depois conta o numero de ocorrencias da descritor 005042 
	l5=0
	do j=1,sec4qc%obs%nvarS
		  	if (sec4qc%obs%d(j,1)==005042) l5=l5+1
	end do	




!{ Por fim decide  qual descritor sera usado como referencia para coordenada vertical
 vcoord=0
 if (l1>1)  vcoord=7004
 if (l2>l1) vcoord=7007
 if (l3>l1) vcoord=7006
 if (l4>l1) vcoord=2150 !adicionado radiancia
 if (l5>l1) vcoord=5042 !adicionado radiancia IASI


 if(vcoord==0) then 
	err=1
	goto 300
 end if 

 !}-----------------------------------------------------------------------------------


!{ Processamentos dos subsets

  do i=1,nsubsets
    iks=iks+1
    multlev_flag=0
    bi=bi+1 
    obs(bi,:)=null
    firstlev=0.0
    VSIG=null
    levini=bi
   
    if (bi<=nobsmax) then 

      !** Varredura para encontrar identificacao da estacao ** 
        call Station_Identifier(sec4,i,aux_sid)

      !** Varredura normal **
      do j=1,sec4qc%obs%nvars
       
        !{ Obtem indicativo de significancia vertical (se houver) 
        if ((sec4qc%obs%d(j,i)==8001).or.(sec4qc%obs%d(j,i)==8042)) then
          VSIG=sec4qc%obs%r(j,i)
          !bi=bi+1            ! Proximo nivel
          multlev_flag=1
        end if 
        !}

        ! Verificacao o descritor chave de nivel 
        ! Se houver o indicativo de coordenada
        ! vertical vcoord entao  o numero de linhas aumenta +1
        ! (Processamento de niveis isobaricos em 
        ! um mesmo subset)
        !{
       
       !
       !Mudancas feita nesse if pq a radiancia nao tem significancia vertical e caia no segundo if
       !**foi comentado tambem o elseif dessa verificacao abaixo...
       !
       
       !if ((sec4qc%obs%d(j,i)==vcoord).and.(VSIG>0)) then  !original
       if ((sec4qc%obs%d(j,i)==vcoord)) then    
        multlev_flag=1
        bi=bi+1
        ks(bi)=ks(bi-1)+1
        
        !print*,'MFORMAT 30 - entrou no primeiro IF',sec4qc%obs%d(j,i),vcoord
        !print *,"***************bi=",bi
       
       !elseif (sec4qc%obs%d(j,i)==vcoord) then
       ! VSIG=NULL
        !print*,'MFORMAT 30 - entrou no Segundo IF',sec4qc%obs%d(j,i),vcoord,VSIG
        
       end if
      !}
      !{ Indetificacao do nivel simples vertical 
        if (multlev_flag==0) then 
          ks(bi)=0
        end if   
          
       
       
        do bj=1,ncbtmax 
         !PRINT *,"bj=",bj
          if ((sec4qc%qc(j,i)>minqc).or.(sec4qc%qc(j,i)<0)) then 
            if (cbt(bj)%d==sec4qc%obs%d(j,i)) then    
             !PRINT *,"bj,d,v=",bj,cbt(bj)%d,fcbt(sec4qc%obs%r(j,i),cbt(bj))
	      obs(bi,cbt(bj)%col)=fcbt(sec4qc%obs%r(j,i),cbt(bj))
              obslev(cbt(bj)%col)=multlev_flag       
            end if
          else
            if (cbt(bj)%d==sec4qc%obs%d(j,i)) nqcexc=nqcexc+1   
          end if
        end do  ! bj
      end do    ! j
    else 
      print *,"Error! Format_mtabqc: Number of observation out of range"
      print *,"Nobsmax=",nobsmax
      err=12
      exit
    end if

   !{ Copiando niveis de supeficie para completar matriz para
   !  demais niveis

    if (levini==0) levini=1 ! Para o caso de nenhum nivel ter sido identificado 
      ks(levini)=iks
      sid(levini)=adjustl(aux_sid)
      do ii = levini+1,bi
        do bj=1,ncbtmax
          if (obslev(cbt(bj)%col)==0) then
            obs(ii,cbt(bj)%col)=obs(levini,cbt(bj)%col)
            sid(ii)=adjustl(aux_sid)
            ks(ii)=iks
          end if
        end do
      end do
    end do !i 

    nrows=bi

300 DEallocate(sec4qc%obs%r,sec4qc%obs%d,sec4qc%qc,Sec4qc%key)	
end subroutine format_mtabqc

!******************************************************************************
! station_identifier| Obtem o SID (Station IDentifier)           |SHSF |
!------------------------------------------------------------------------------
!  Artificio de leitura do descritor caracter para os dados  
!  *001011-Ship or mobile land station identifier (CCITT IA5) 
!  *001008-Aircraft registration number or other identification
!
! Utilizar apenas para dados de superficie (tipo 0 ou 1) 
!------------------------------------------------------------------------------
 subroutine Station_Identifier(sec4,lsub,sid)
  !{ Interface
    type(sec4type),intent(inout) ::sec4  !.Secao 4 do  MBUFR
    integer,          intent(in) ::lsub  !.Numero do subset
    character(len=*),intent(out) ::sid   ! Sitation IDentifier ( Identificador da Estacao) 
 !}
 !{Variaveis locais
    integer :: lvar,cchar,nchar
   logical :: attrib   
 !}
                 lvar=1
                 sid=""
                 attrib=.false.
                 do while ((lvar<=sec4%nvars).and.(.not.attrib))                      
                  !                         
	           if(sec4%d(lvar,lsub).eq.001011.or.sec4%d(lvar,lsub).eq.001015.or.sec4%d(lvar,lsub).eq.001008) then

                      nchar=sec4%c(lvar,lsub)                                             
                      if ((nchar>0).and.(nchar<=len(sid))) then 
                          sid=trim(sid)//char(int(sec4%r(lvar,lsub))) 
                      elseif (nchar>len(sid)) then
                        attrib=.true.                                                        
                      end if

                       !print *,'Desc, nc, SID  >>> ',sec4%d(lvar,lsub),nchar,sid
                       !print *,'lvar,lsub,len(sid) >>> ',lvar,lsub,len(sid)

                   end if                     
                   lvar=lvar+1
                end do 
               !print *,">>>> SID =", sid
 end subroutine Station_Identifier                 
!------------------------------------------------------------------------------
!  formatqc      |                                                     | SHSF |  
!-----------------------------------------------------------------------------|
!                                                                             |
! Organiza dados da estrutura da sec4 para sec4qc, atribuindo o flag de       |
! confianca ( controle de qualidade)                                          |
!------------------------------------------------------------------------------	  
! Qc =  inteiro que indicador de controle de qualidade
! key = chave que indica o estado do indicador  qc
!       =-1 Nenhuma chave de estado esta associada
!       = 0 Qc esta no estado aberto para receber novo valor
!       = 1 Qc esta em estado fechado 
!
!-----------------------------------------------------------------------------
subroutine format_qc(sec4,nsubsets,sec4qc)
!{ Declaracao da interface
	type(sec4type)     ::sec4
	integer,intent(in) ::nsubsets                       ! Numero de subsets
	type(sec4qctype)   ::sec4qc
	integer,allocatable::present_val(:)

!}

!{ Declaracao de variaveis locais
	integer ::i,j,err
	integer:: nvars,nvars_qc
	integer,parameter :: fpresent_id = 236000
	integer,parameter :: attribute_id = 222000
	integer,parameter :: present_id = 031031
	integer,parameter :: qc_id=033007
	integer,parameter :: looked=1
	integer,parameter :: unlooked=0
	integer:: attributing
        logical :: is_attribute 
	integer::n,npid,k
!}

!{ Alocando e Zerando estrutura sec4qc
	nvars=sec4%nvars
	allocate(sec4qc%obs%r(nvars,nsubsets),stat=err)
	allocate(sec4qc%obs%d(nvars,nsubsets),stat=err)
	allocate(sec4qc%qc(nvars,nsubsets),stat=err)
	allocate(sec4qc%key(nvars,nsubsets),stat=err)
	allocate(present_val(nvars),stat=err)
	is_attribute=.false.
	nvars_qc=0

	sec4qc%obs%nvars=sec4%nvars
	do j=1,nsubsets
		do i=1,nvars
			sec4qc%obs%r(i,j)=null	                ! Valor real  (Usar este campo quando a variavel for do tipo numerico)
			sec4qc%obs%d(i,j)=null
			sec4qc%qc(i,j)=null
			sec4qc%key(i,j)=-1  
		end do
	end do
!{
	do j=1,nsubsets
		
		n=0
		attributing =0
		npid=0

		do i=1,nvars
			if(sec4%d(i,j)==attribute_id) attributing=1 
			if ((sec4%d(i,j)==fpresent_id).or.(sec4%d(i,j)==present_id)) attributing=2
				if ((sec4%d(i,j)==qc_id).and.(attributing/=3)) then
					do k=1,npid
						sec4qc%key(k,j)=present_val(k)
					end do
			end if

			
			
			if (sec4%d(i,j)==qc_id) attributing=3

			if (attributing==0) then 
			!{ * Copia observacoes para sec4qc
				
				n=n+1
				sec4qc%obs%d(n,j)=sec4%d(i,j)
				sec4qc%obs%r(n,j)=sec4%r(i,j)
			!}
			
			elseif (attributing==1) then 
			!{ * zera variaveis de atribuicao
					npid=0
					attributing=0

			!}
			elseif (attributing==2) then 
			!{ * Obtem os valores de present_id que existem 
				
				if (sec4%d(i,j)==present_id) then 
					npid=npid+1
					present_val(npid)=sec4%r(i,j)
				elseif (npid>0) then 
					do k=1,npid
						sec4qc%key(k,j)=present_val(k)
					end do
					attributing=3
				end if
			 !}							
			elseif (attributing==3) then 
			
			!{ * Atribuindo flag de controle de qualidade
				if(sec4%d(i,j)==qc_id) then 
					do k=1,npid
						if (sec4qc%key(k,j)==unlooked) then 
							sec4qc%qc(k,j)=sec4%r(i,j) 
							sec4qc%key(k,j)=looked
							exit
						end if
					end do
				end if
			!}

			end if

		  end do  ! i

		  !{ Atualizando numero de variaveis de qc
		  if (nvars_qc<n) nvars_qc=n
		end do ! j

		sec4qc%obs%nvars=nvars_qc
		deallocate(present_val)
!}
end subroutine




!-----------------------------------------------------------------------------!
!REDEFINE_SUBTYPE | Obtem o subtipo correto de um BUFR                    |SHSF |
!-----------------------------------------------------------------------------
! comum centros meteorologicos geral dados em BUFR utilizando subtipos 
! definidos localmente. 
!
! Esta subrotina visa redefinir alguns subtipos locais em funcao do tipo de BUFR
! afim de obter um subtipo padronizado
!
! Atualmente esta implementada apenas para dados de estacoes automaticas 
! para o qual o subtipo padrao 7. 
! Esta rotina nao tera efeito para os demais tipos de dados
!-----------------------------------------------------------------------------
! DEPENDENCIAS
!-----------------------------------------------------------------------------
!HISTORICO

subroutine redefine_subtype(sec3,btype,bsubtype)
!{ Variaveis de interface	
	type(sec3type),intent(in)::sec3 !.......................Secao 3 de uma mensagem BUFR
	integer,intent(in)::btype       !.......................Tipo BUFR
	integer,intent(inout)::bsubtype !... Sub-tipo BUFR a ser redefinido caso necessario
!}
!{ Variaveis locais
	integer::i
!}
	
	if (btype==0) then 

		do i=1,sec3%ndesc
		!{ Caso Estacao automatica de superficie 
			if(sec3%d(i)==001102) then 
				bsubtype=7
				exit
			end if
		!}
		end do
	end if 

end subroutine	

!------------------------------------------------------------------------------
!fcbt |Aplica calculo de conversao descrito no vetor cbt                  |SHSF
!------------------------------------------------------------------------------
!Esta funcao realiza as operacoes de conversao de escala e referencia para
!os dados BUFR lidos segundo o vetor cbt
!-------------------------------------------------------------------------------
! 2010-01-28 : SHSF: feito modificacao para permitir precisao no calculo de
!                     multiplicacao e divisao inteiras 
function fcbt(value,cbt);real(kind=realk)::fcbt
!{ variaveis de interface
   real(kind=realk),intent(in)::value ! Valor bufr a ser convertido
   type(corbufrmat),intent(in)::cbt
!}
!{Variaveis locais
    integer::i
    integer::mult  ! Mutiplicando;'Multiplicand' 
    integer::div   ! Divisor;'Divisor' 
    logical::atrib
!}
   
   fcbt=null
   if (value/=null_mbufr) then 
     if (cbt%bitm==0) then 
       fcbt=value*cbt%mult+cbt%ref
     else 
       mult=int(cbt%mult)
       div=int(1/cbt%mult)
       if (abs(mult)>=1) then 
         i=int(value*cbt%mult+cbt%ref)
       else 
         i=int(value/div+cbt%ref)
       end if
       fcbt=iand(i,cbt%bitm)
     end if

  end if
end function

!-------------------------------------------------------- 
! Converte valor  tipo "real"  para valor tipo "single"
!------------------------------------------------------
!{
function csingle(value)
  type(single)::csingle
  real, intent(in)::value 
  csingle%r=value
  csingle%l=.true.
end function
!}

!--------------------------------------------------
! Converte valor tipo "single" para valor tipo real
!--------------------------------------------------
!{
function cnum(value,missing); real::cnum
   type(single),    intent(in)::value
   real,            intent(in)::missing
   if (value%l) then 
     cnum=value%r
   else 
     cnum=missing
   end if

end function
!} 

!
! Concatena caracteres para formar um texto (inicio de uma funcao para concatenar)
!
function ctext(sec4,i,subset); character(len=60)::ctext
 type(sec4type) ::sec4  
 integer,intent(in):: i      ! Variavel inicial
 integer,intent(in):: subset ! Subset corrente
  
  ctext=""
!  if (sec4%c(i,j)<=0) return
    
  ctext="Ok"
  
end function

end module

