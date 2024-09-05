!******************************************************************************
!*                                  MFORMAT20                                 *
!*                                                                            *
!*         Subroutines to Select and Reorganize Meteorological data           *
!*                     read by MBUFR-ADT module                               *
!*                                                                            *
!*             Copyright (C) 2007 Sergio Henrique S. Ferreira  (SHSF)         *
!*                                                                            *  
!*       This library is free software; you can redistribute it and/or        *
!*       modify it under the terms of the GNU Lesser General Public           *
!*       License as published by the Free Software Foundation; either         *
!*       version 2.1 of the License, or (at your option) any later version.   *
!*                                                                            *
!*       This library is distributed in the hope that it will be useful,      *
!*       but WITHOUT ANY WARRANTY; without even the implied warranty of       *
!*       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU    *
!*       Lesser General Public License for more details.                      *
!******************************************************************************
!*                                  MFORMAT20                                 *
!*                                                                            *
!*      Sub-rotinas para Selecionar e Re-organizar dados Meteorolgicos        *
!*                      Lidos pelo mdulo MBUFR-ADT                            * 
!*                                                                            *
!*             Copyright (C) 2007 Sergio Henrique S. Ferreira  (SHSF)         *
!*                                                                            *
!*     Esta biblioteca e um software livre, que pode ser redistribudo e/ou    *
!*     modificado sob os termos da Licenca Publica Geral Reduzida GNU,        *
!*     conforme publicada pela Free Software Foundation, versao 2.1 da licenca*
!*     ou  (a criterio do autor) qualquer vers?o posterior.                   *
!*                                                                            *
!*     Esta biblioteca e distribuda na esperanca de ser util, porem NAO TEM   *
!*     NENHUMA GARANTIA EXPLICITA OU IMPLICITA, COMERCIAL OU DE ATENDIMENTO   *
!*     A UMA DETERMINADA FINALIDADE. Veja a Licenca Publica Geral Reduzida    * 
!*     GNU para maiores detalhes.                                             * 
!*                                                                            *
!*----------------------------------------------------------------------------* 
!* HISTORICO:                                                                 
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

module mformat20
use mbufr
implicit none 
private
public sec4qctype
public levidtype
public format_qc
public format_tab
public format_tabqc
public format_mtabqc
public line_thinner
public corbufrmat
public redefine_subtype
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
   integer::d !...................................Descritor BUFR (TABELA B)
   integer::col !.............Numero de uma coluna da matriz de observacoes
   real::mult ! Fator de escala entre o dado BUFR e o valor na matriz de observacoes
   real::ref   ! Fator de referencia entre o dados BUFR e o Valor na matriz de observacao
   integer::bitm ! Mascara de bits  
end type 


type sec4qctype
   type(sec4type)::obs
   integer,pointer::qc(:,:)
   integer,pointer::key(:,:)
   integer::nvars
end type

type levidtype
    real:: press
    integer::numlev
end type


Real,parameter	:: Null=-340282300      !valor nulo

CONTAINS



!-----------------------------------------------------------------------------!
!FORMAT_TAB | Extrai dados da secao 4 e organiza em colunas (TABELA)    |SHSF |
!-----------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------
! DEPENDENCIAS
!-----------------------------------------------------------------------------
!HISTORICO

subroutine format_tab(sec4,nsubsets,cbt,ncbtmax,nrows,obs)
	type(sec4type)::sec4 !.............................................Secao 4 de uma mensagem BUFR
	integer,intent(in)::nsubsets !.....................................Numero de subsets da secao 4
	type(corbufrmat),dimension(:),intent(in)::cbt !..Matriz que correlaciona a secao 4 com a tabela 
	integer,intent(in)::ncbtmax !.......................Numero maximo de elementos em cbt desc_cols
	integer,intent(inout)::nrows!.............  ............................Numero de linhas de obs
	real,dimension(:,:),intent(out):: obs ! Tabela de valores.................. (1:nsubsets,1:ncols)
	
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
	obs(bi+1:nobsmax,:)=null
	
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
      if (bi<=nobsmax) then 
        do bj=1,ncbtmax
          do j=1,sec4%nvars 
            if ((cbt(bj)%d==sec4%d(j,i)).and.(obs(bi,cbt(bj)%col)==null)) then
              if (sec4%r(j,i)/=null) then
                obs(bi,cbt(bj)%col)=fcbt(sec4%r(j,i),cbt(bj)) 
                !obs(bi,cbt(bj)%col)=sec4%r(j,i)*cbt(bj)%mult+cbt(bj)%ref
                sec4%r(j,i)=null
              else
                obs(bi,cbt(bj)%col)=null
              end if
              exit 
              exit 
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

	
subroutine format_tabqc(sec4,nsubsets,minqc,cbt,ncbtmax,nrows,obs,nqcexc,err)
!{ Variaveis da Interface
	type(sec4type),intent(inout)::sec4
	integer,intent(in)::nsubsets !..............................................Numero de subsets
	integer,intent(in)::minqc !................................Minimo valor de Confianca aceitavel 
	type(corbufrmat),dimension(:),intent(in)::cbt !.Matriz que correlaciona a secao 4 com a tabela
	integer,intent(in)::ncbtmax !......................Numero maximo de elementos em cbt desc_cols
	integer,intent(inout)::nrows !.........................................Numero de linhas de obs
	real,dimension(:,:),intent(inout):: obs !...............Tabela de valores (1:nsubsets,1:ndesc)
	integer,intent(out)::nqcexc
	integer,intent(out)::err
!{
!{ variaveis locais
	integer :: i,j,bi,bj
	integer:: nobsmax
	type(sec4qctype)::sec4qc
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

      do i=1,nsubsets		
        bi=bi+1 
        obs(bi,:)=null
        if (bi<=nobsmax) then 
          do bj=1,ncbtmax
            do j=1,sec4qc%obs%nvars
              if (.not. atrib(j,i)) then 
                if ((sec4qc%qc(j,i)>minqc).or.(sec4qc%qc(j,i)==null)) then 
                  if ((cbt(bj)%d==sec4qc%obs%d(j,i)).and.(obs(bi,cbt(bj)%col)==null)) then
                    if (sec4qc%obs%r(j,i)/=null) then 
                      obs(bi,cbt(bj)%col)=fcbt(sec4qc%obs%r(j,i),cbt(bj))
                      atrib(j,i)=.true.
                    end if
                    exit 
                    exit
                  end if
                else
                  if (cbt(bj)%d==sec4qc%obs%d(j,i)) nqcexc=nqcexc+1   
                end if
              end if
            end do  ! j
          end do ! bj
        else 
          print *,"Error! Format_tabqc: Number of observation out of range"
          print *,"Nobsmax=",nobsmax
          err=1
          exit
        end if
      end do !i 
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

subroutine format_mtabqc(sec4,nsubsets,minqc,cbt,ncbtmax,nrows,obs,ks,nqcexc,err)

!{ Variaveis da interface 
 type(sec4type),intent(inout)            ::sec4     !.Secao 4 do  MBUFR
 integer,intent(in)                      ::nsubsets !.Numero de subsets
 integer,intent(in)                      ::minqc    !.Confianca Minima aceitavel em % 
 type(corbufrmat),dimension(:),intent(in)::cbt      ! Matriz que correlaciona a secao 4 com a tabela
 integer,intent(in)                      ::ncbtmax  !.Numero maximo de elementos em cbt desc_cols
 integer,intent(inout)                   ::nrows    !.Numero de linhas de obs
 real,dimension(:,:),intent(inout)       ::obs      !.Tabela de valores (1:nsubsets,1:ndesc)
 integer,dimension(:),intent(inout)      ::ks       !.Numero da sondagem
 integer,intent(out)                     ::nqcexc   !.Numero de dados excluidos 
 integer,intent(out)                     ::err
 !}

!{ variaveis locais
 integer         :: i,j,bj,ii,l1,l2,L3
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

!{ Por fim decide  qual descritor sera usado como referencia para coordenada vertical
 vcoord=0
 if (l1>1) vcoord=7004
 if (l2>l1) vcoord=7007
 if (l3>l1) vcoord=7006

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

       if ((sec4qc%obs%d(j,i)==vcoord).and.(VSIG>0)) then  
        multlev_flag=1
        bi=bi+1
        !print *,"***************bi=",bi
       elseif (sec4qc%obs%d(j,i)==vcoord) then
        VSIG=NULL
       end if
      !}

        do bj=1,ncbtmax 
          if ((sec4qc%qc(j,i)>minqc).or.(sec4qc%qc(j,i)<0)) then 
            if (cbt(bj)%d==sec4qc%obs%d(j,i)) then 
              !obs(bi,cbt(bj)%col)=sec4qc%obs%r(j,i)*cbt(bj)%mult+cbt(bj)%ref
 
	      obs(bi,cbt(bj)%col)=fcbt(sec4qc%obs%r(j,i),cbt(bj))
              obslev(cbt(bj)%col)=multlev_flag
           !   print *,cbt(bj)%col,obs(bi,cbt(bj)%col)
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
      do ii = levini+1,bi
        do bj=1,ncbtmax
          if (obslev(cbt(bj)%col)==0) then
            obs(ii,cbt(bj)%col)=obs(levini,cbt(bj)%col)
            ks(ii)=iks
          end if
        end do
      end do
    end do !i 
    nrows=bi

300 DEallocate(sec4qc%obs%r,sec4qc%obs%d,sec4qc%qc,Sec4qc%key)	
end subroutine format_mtabqc


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
	type(sec4type)::sec4
	integer,intent(in)::nsubsets                       ! Numero de subsets
	type(sec4qctype)::sec4qc
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




!------------------------------------------------------------------------------
!  Linear_Thinner |                                                    | SHSF |  
!-----------------------------------------------------------------------------|
!  DILUICAO ALEATORIA DE LINHAS DE UMA MATRIZ DE DADOS BIDIMENSIONAL          |
!                                                                             |
!    A(:,:) MATRIZ DE DADOS                                                   |
!    NL NUMERO TOTAL DE DE LINHAS da MATRIZ                                   |
!-----------------------------------------------------------------------------|
			

subroutine Line_Thinner(M,L1,L2,C1,C2,ISTEP,KS)
	REAL, DIMENSION(:,:),INTENT(INOUT) ::M  !...........................Matriz a ser diluida
	INTEGER,INTENT(IN)::L1	!.........................................Linha inicial da matriz
	INTEGER,INTENT(IN)::C1,C2 !...........................Colunas Iniciais e finais da matriz
	INTEGER,INTENT(INOUT)::L2 !..............Linha final da matriz antes e depois da diluicao  
	INTEGER,INTENT(IN)::ISTEP !...............................Numero de linhas a ser saltadas
	integer,DIMENSION(:),intent(inOUT)::kS  !..............................Coluna de controle
	                                        ! ctr<c1 indica utilizacao de salto simples de linha
                                                ! ctr>=c1 indica que um indice na coluna ctr sera usado 
                                                ! para agrupar linhas que serao ou nao excluirdas
	
	REAl,DIMENSION(L1:L2,C1:C2):: AUX
	INTEGER,DIMENSION(l1:l2)::AUXKS
	integer::Iks  ,I	  ,L, STEP

	STEP=ISTEP
	IF (STEP<2) STEP=2
	auxks(l1:l2)=ks(l1:l2)

	 !Salto de grupos de linhas 
	 ! Marcando no auxKs as linhas que serao utilizadas 
	 !{ 

		IKS=1
		DO i=L1,L2
			IF (I>1) THEN
				IF (KS(I)/=KS(I-1)) IKS=IKS+1
			END IF
			IF (MOD(IKS,STEP)==0) AUXKS(I)=0
			
		END DO

	  !}

	  ! As linhas marcadas com auxkx=0 serao excluidas
	  !{	
		L=L1
		do i=L1,L2
			IF (AUXKS(i)>0) THEN 
				AUX(L,C1:C2)=M(i,c1:C2)
				KS(L)=AUXKS(i)
				L=L+1
			END IF
		END DO
			
		L2=L-1  ! Novo nunero de linhas		
		!}
	
				
END SUBROUTINE LINE_THINNER

!-----------------------------------------------------------------------------!
!REDEFINE_SUBTYPE | Obtem o subtipo correto de um BUFR                    |SHSF |
!-----------------------------------------------------------------------------
! �comum centros meteorologicos geral dados em BUFR utilizando subtipos 
! definidos localmente. 
!
! Esta subrotina visa redefinir alguns subtipos locais em fun�o do tipo de BUFR
! afim de obter um subtipo padronizado
!
! Atualmente esta implementada apenas para dados de estacoes automaticas 
! para o qual o subtipo padrao �7. 
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
function fcbt(value,cbt);real::fcbt
!{ variaveis de interface
   real,intent(in)::value ! Valor bufr a ser convertido
   type(corbufrmat),intent(in)::cbt
!}
!{Variaveis locais
    integer::i
    integer::mult  ! Mutiplicando;'Multiplicand' 
    integer::div   ! Divisor;'Divisor' 
!}
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

end function

end module

