!******************************************************************************
!*                                  MFORMATS                                  *
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
!*       Lesser General Public License for more details.                      *                                                                          * 
!******************************************************************************
!*                                  MFORMATS                                  *
!*                                                                            *
!*      Sub-rotinas para Selecionar e Re-organizar dados Meteorologicos       *
!*                      Lidos pelo modulo MBUFR-ADT                           * 
!*                                                                            *
!*             Copyright (C) 2007 Sergio Henrique S. Ferreira  (SHSF)         *
!*                                                                            *
!*     Esta biblioteca e um software livre, que pode ser redistribuido e/ou   *
!*     modificado sob os termos da Licenca Publica Geral Reduzida GNU,        *
!*     conforme publicada pela Free Software Foundation, versao 2.1 da licenca*
!*     ou  (a criterio do autor) qualquer vers?o posterior.                   *
!*                                                                            *
!*     Esta biblioteca e distribuida na esperanca de ser util, porem NAO TEM  *
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
!*               em todas as sub-rotinas, i.e. OBS(:,:,:) passa a ser OBS(:,:)
!*

module mformats

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



!------------------------------------------------------------------------------
!  format_tab |                                                        | SHSF |  
!-----------------------------------------------------------------------------|
!  Extrai dados especificos da estrutura da sec4 do MBUFR colocando-os em     |
!  uma matriz (tabela) de valores                                             |
!                                                                             |
!-----------------------------------------------------------------------------|
!DEPENDENCIAS: MBUFR-ADT                                                      |
!-----------------------------------------------------------------------------|

subroutine format_tab(sec4,nsubsets,desc_cols,ncols,nrows,obs)
	type(sec4type)::sec4
	integer,intent(in)::nsubsets  !..............................................Numero de subsets
	integer,dimension(:),intent(in)::desc_cols !.Codigos descritores de cada coluna da tabela (obs)
	integer,intent(in)::ncols !...............................Numero de colunas de obs e desc_cols
	integer,intent(inout)::nrows !.........................................Numero de linhas de obs
	real,dimension(:,:),intent(out):: obs !..................Tabela de valores (1:nsubsets,1:ndesc)
	
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
	obs(bi+1:nobsmax,1:ncols)=null
	
	!}

	!{ Inicio da organizacao das variaveis 
	do i=1,nsubsets		
		bi=bi+1 
		if (bi<=nobsmax) then 
			do bj=1,ncols

					do j=1,sec4%nvars       
						if (desc_cols(bj)==sec4%d(j,i)) then
							obs(bi,bj)=sec4%r(j,i)
							exit 
							exit
						end if
					end do  ! j   
				end do ! bj

		else 
		  print *,"Erro ! Arquivo muito grande"
		  exit
		end if
	end do !i 
	
	nrows=bi     	   
	
	
end subroutine format_tab




!------------------------------------------------------------------------------
!  format_tabqc |                                                      | SHSF |  
!-----------------------------------------------------------------------------|
!  Extrai dados especificos da estrutura da sec4 do MBUFR colocando-os em     |
!  uma matriz (tabela) de valores                                             |
!                                                                             |
!   Similar ao format_qc, porem, nesta sao excluidos os dados que possuem     |
!   flag de controle de qualidade abaixo do em fator minqc                    |
!                                                                             |
!  Os dados que nao possuem o flag sao aceitos                                |
!                                                                             |
!-----------------------------------------------------------------------------|
!DEPENDENCIAS: MBUFR-ADT                                                      |
!Dependecias Internas: format_qc                                              |
!-----------------------------------------------------------------------------|

subroutine format_tabqc(sec4,nsubsets,minqc,desc_cols,ncols,nrows,obs,nqcexc,err)
	type(sec4type),intent(inout)::sec4
	integer,intent(in)::nsubsets                   ! Numero de subsets
	integer,intent(in)::minqc                      ! Minimo valor de Confianca aceitavel 
	integer,dimension(:),intent(in)::desc_cols	   ! Codigos descritores de cada coluna da tabela (obs)
	integer,intent(in)::ncols                      ! Numero de colunas de obs e desc_cols
	integer,intent(inout)::nrows                     ! Numero de linhas de obs
	real,dimension(:,:),intent(inout):: obs          ! Tabela de valores (1:nsubsets,1:ndesc)
	integer,intent(out)::nqcexc
	integer,intent(out)::err
	
	!{ variaveis locais
	integer :: i,j,bi,bj
	integer:: nobsmax
	type(sec4qctype)::sec4qc

	!}
	nobsmax=ubound(obs,1)
	bi=nrows
	nqcexc=0	   	
	err=0
    ! Verificar se a matriz obs e suficientemente grande para
	!  armazenar os dados. Caso nao seja retorna ao programa principal
	!{
	  
	  if ((nrows+nsubsets+sec4%nvars)>= (nobsmax)) then 
	     print *,"Dados lidos excederam limite previsto" 
	     print *,"Nobsmax=",nobsmax
	     err=1
	     return 
	  end if
        !}

	call format_qc(sec4,nsubsets,sec4qc)


	do i=1,nsubsets		
		bi=bi+1 
		obs(bi,1:ncols)=null
		if (bi<=nobsmax) then 
			do bj=1,ncols
		
				do j=1,sec4qc%obs%nvars
					
					       
					if ((sec4qc%qc(j,i)>minqc).or.(sec4qc%qc(j,i)==null)) then 
						if (desc_cols(bj)==sec4qc%obs%d(j,i)) then
						
							obs(bi,bj)=sec4qc%obs%r(j,i)
							exit 
							exit
		
						end if
				 	else
						nqcexc=nqcexc+1   
					end if
		
				end do  ! j
			end do ! bj

		else 
		  print *,"Erro ! Arquivo muito grande"
		  exit
		end if
	end do !i 
	
	nrows=bi
	
	
end subroutine format_tabqc



!------------------------------------------------------------------------------
!  format_mtabqc |                                                     | SHSF |  
!-----------------------------------------------------------------------------|
!                                                                             |
!  Extrai dados especificos da estrutura da sec4 do MBUFR                     |
!  colocando-os em uma matriz (tabela) de valores                             | 
!  Similar ao format_tabqc, porem, nesta sao considerados multiplos niveis    |
!  isobaricos em um mesmo subset de informacao, tal como nos dados de         |
!  radiossondagem                                                             |
!                                                                             |
!   Os dados que nao possuem o flag sao aceitos.                              |
!   Indicativo de nivel isobarico Lev= 007004                                 |
!                                                                             |
!-----------------------------------------------------------------------------|
!DEPENDENCIAS: MBUFR-ADT                                                      |
!Dependecias Internas: format_qc                                              |
!-----------------------------------------------------------------------------|


subroutine format_mtabqc(sec4,nsubsets,minqc,desc_cols,ncols,nrows,obs,ks,nqcexc,err)
	type(sec4type),intent(inout)::sec4
	integer,intent(in)::nsubsets                   ! Numero de subsets
	integer,intent(in)::minqc                      ! Minimo valor de Confianca aceitavel 
	integer,dimension(:),intent(in)::desc_cols	   ! Codigos descritores de cada coluna da tabela (obs)
	integer,intent(in)::ncols                      ! Numero de colunas de obs e desc_cols
	integer,intent(inout)::nrows                   ! Numero de linhas de obs
	real,dimension(:,:),intent(inout):: obs      ! Tabela de valores (1:nsubsets,1:ndesc)
	integer,dimension(:),intent(inout)::ks         ! Numero da sondagem
	integer,intent(out)::nqcexc					   ! Numero de dados excluidos 
	integer,intent(out)::err
	
	!{ variaveis locais
	integer :: i,j,bi,bj,ii
	integer:: nobsmax
	type(sec4qctype)::sec4qc
	integer :: levini,multlev_flag
	integer,dimension(ncols) :: obslev
	integer ::iks
	real :: firstlev ! Valor de pressao do primeiro nivel isobarico 
	!}
	
	firstlev=0
	nobsmax=ubound(obs,1)
	bi=nrows
	nqcexc=0	   	
	obslev(:)=0
	err=0
	
	
	if (nrows<1) then
	    iks=0
	else 
	   iks=ks(nrows)
	end if

        ! Verificar se a matriz obs eh suficientemente grande para
	!  armazenar os dados. Caso nao seja retorna ao programa principal
	!{
	  
	  if ((nrows+nsubsets+sec4%nvars)>= nobsmax) then 
	     print *,"Dados lidos excederam limite previsto" 
	     print *,"Nobsmax=",nobsmax
	     err=1
	     return 
	  end if
        !}
	
	
	call format_qc(sec4,nsubsets,sec4qc)


	do i=1,nsubsets
	    iks=iks+1
		multlev_flag=0		
		bi=bi+1 
		obs(bi,1:ncols)=null

		if (bi<=nobsmax) then 

		


			do j=1,sec4qc%obs%nvars
			
			! Caso encontre a variavel ano. e porque esta iniciando
			! outra sondagem 
				if (sec4qc%obs%d(j,i)==004001) then
				   firstlev=0.0
				end if

			! Verificacao de nivel isobarico
			! Se houver o indicativo de coordenada
			! vertical 007004 entao levid recebe o valor 
			! de pressao e o numero de linhas aumenta +1
			! (Processamento de niveis isobaricos em 
			! um mesmo subset)
			!{
				if (sec4qc%obs%d(j,i)==007004) then 
					if (firstlev>0.0) then 
						bi=bi+1
					else 
					    multlev_flag=1
						levini=bi
					end if
					
					firstlev=sec4qc%obs%r(j,i)

				end if
			!}


			
				do bj=1,ncols
		
		      

					if ((sec4qc%qc(j,i)>minqc).or.(sec4qc%qc(j,i)<0)) then 
						
					
						
						
						if (desc_cols(bj)==sec4qc%obs%d(j,i)) then
						
							obs(bi,bj)=sec4qc%obs%r(j,i)
							obslev(bj)=multlev_flag
						!	exit 
							!exit
		
						end if
				 	else
						nqcexc=nqcexc+1   
					end if
		
				end do  ! bj
			end do ! j

		else 
		  print *,"Erro ! Arquivo muito grande"
		  exit
		end if
   !{ Copiando niveis de supeficie para completar matriz para
   !  demais niveis


		ks(levini)=iks
		do ii = levini+1,bi
		 do bj=1,ncols
		  if (obslev(bj)==0) then
		    obs(ii,bj)=obs(levini,bj)
			ks(ii)=iks
		  end if
		 end do
		end do




	
	end do !i 
	
	nrows=bi
	
	
end subroutine format_mtabqc




subroutine format_mtabqc2(sec4,nsubsets,minqc,desc_cols,ncols,nrows,obs,ks,nqcexc,err)
	type(sec4type),intent(inout)::sec4
	integer,intent(in)::nsubsets                   ! Numero de subsets
	integer,intent(in)::minqc                      ! Minimo valor de Confianca aceitavel 
	integer,dimension(:),intent(in)::desc_cols	   ! Codigos descritores de cada coluna da tabela (obs)
	integer,intent(in)::ncols                      ! Numero de colunas de obs e desc_cols
	integer,intent(inout)::nrows                   ! Numero de linhas de obs
	real,dimension(:,:),intent(inout):: obs      ! Tabela de valores (1:nsubsets,1:ndesc)
	integer,dimension(:),intent(inout)::ks         ! Numero da sondagem
	integer,intent(out)::nqcexc					   ! Numero de dados excluidos 
	integer,intent(out)::err
	
	!{ variaveis locais
	integer :: i,j,bi,bj,ii
	integer:: nobsmax
	type(sec4qctype)::sec4qc
	integer :: levini,multlev_flag
	integer,dimension(ncols) :: obslev
	integer ::iks
	real :: firstlev ! Valor de pressao do primeiro nivel isobarico 
	!}
	
	firstlev=0
	nobsmax=ubound(obs,1)
	bi=nrows
	nqcexc=0	   	
	obslev(:)=0
	err=0
	
	
	if (nrows<1) then
	    iks=0
	else 
	   iks=ks(nrows)
	end if

        ! Verificar se a matriz obs eh suficientemente grande para
	!  armazenar os dados. Caso nao seja retorna ao programa principal
	!{
	  
	  if ((nrows+nsubsets+sec4%nvars)>= nobsmax) then 
	     print *,"Dados lidos excederam limite previsto" 
	     print *,"Nobsmax=",nobsmax
	     err=1
	     return 
	  end if
        !}
	
	
	call format_qc(sec4,nsubsets,sec4qc)


	do i=1,nsubsets
	    iks=iks+1
		multlev_flag=0		
		bi=bi+1 
		obs(bi,1:ncols)=null

		if (bi<=nobsmax) then 

		


			do j=1,sec4qc%obs%nvars
			
			! Caso encontre a variavel ano. e porque esta iniciando
			! outra sondagem 
				if (sec4qc%obs%d(j,i)==004001) then
				   firstlev=0.0
				end if

			! Verificacao de nivel isobarico
			! Se houver o indicativo de coordenada
			! vertical 007004 entao levid recebe o valor 
			! de pressao e o numero de linhas aumenta +1
			! (Processamento de niveis isobaricos em 
			! um mesmo subset)
			!{
				if (sec4qc%obs%d(j,i)==007004) then 
					if (firstlev>0.0) then 
						bi=bi+1
					else 
					    multlev_flag=1
						levini=bi
					end if
					
					firstlev=sec4qc%obs%r(j,i)

				end if
			!}


			
				do bj=1,ncols
		
		      

					if ((sec4qc%qc(j,i)>minqc).or.(sec4qc%qc(j,i)<0)) then 
						
					
						
						
						if (desc_cols(bj)==sec4qc%obs%d(j,i)) then
						
							obs(bi,bj)=sec4qc%obs%r(j,i)
							obslev(bj)=multlev_flag
						!	exit 
							!exit
		
						end if
				 	else
						nqcexc=nqcexc+1   
					end if
		
				end do  ! bj
			end do ! j

		else 
		  print *,"Erro ! Arquivo muito grande"
		  exit
		end if
   !{ Copiando niveis de supeficie para completar matriz para
   !  demais niveis


		ks(levini)=iks
		do ii = levini+1,bi
		 do bj=1,ncols
		  if (obslev(bj)==0) then
		    obs(ii,bj)=obs(levini,bj)
			ks(ii)=iks
		  end if
		 end do
		end do




	
	end do !i 
	
	nrows=bi
	
	
end subroutine format_mtabqc2

	  
!------------------------------------------------------------------------------
!  format_qc     |                                                     | SHSF |  
!-----------------------------------------------------------------------------|
!                                                                             |
! Organiza dados da estrutura da sec4 para sec4qc, atribuindo o flag de       |
! confianca ( controle de qualidade)                                          |
!------------------------------------------------------------------------------
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

				

Subroutine Line_Thinner(M,L1,L2,C1,C2,ISTEP,KS)
	REAL, DIMENSION(:,:),INTENT(INOUT) ::M  ! Matriz a ser diluida
	INTEGER,INTENT(IN)::L1					! Linha inicial da matriz
	INTEGER,INTENT(IN)::C1,C2				! Colunas Iniciais e finais da matriz
	INTEGER,INTENT(INOUT)::L2				! Linha final da matriz antes e depois da diluicao  
	INTEGER,INTENT(IN)::ISTEP               ! Numero de linhas a ser saltadas
	integer,DIMENSION(:),intent(inOUT)::kS  ! Coluna de controle
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
			
		L2=L-1  ! Novo numero de linhas		
		!}
	
				
END SUBROUTINE LINE_THINNER

								 


			

end module

