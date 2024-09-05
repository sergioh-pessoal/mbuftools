	module mstatistic
!------------------------------------------------------------------------------
!mdtatistic ! Modulo para calculos estatisticos                !MCT-INPE-CPTEC!
!------------------------------------------------------------------------------
! Modulo de subrotinas para calculos estatisticos:
!   media, desvio padrao, controle de qualidade de dodos
!
!------------------------------------------------------------------------------
! AUTOR: Sergio H. S. Ferreira
	private
	Real,parameter	:: Null=-340282300      !valor nulo 
	
	public mean  ! Funcao     - calculo de media de X
	public meansd ! Subrotina - Calculo de media e desvio padrao de x
	public qc1

	contains









!------------------------------------------------------------------------------
!mean   ! Funcao para calculo de media                          !MCT-INPE-CPTEC!
!------------------------------------------------------------------------------
!   Esta subrotina calcula media e desvio padrao de um vetor X(:) com nelemets
!   Os elevemtos que tiverem valor Null nao sao computados
!
!------------------------------------------------------------------------------
! AUTOR: Sergio H. S. Ferreira
	
function mean(X,nelements,nvalues);real::mean

!{ Variaveis da interface
	real,dimension(:),intent(in)::X ! Vetor com os elementos a serem processados
	integer,intent(in)::nelements    ! Numero de elementos de X	
	integer,optional,intent(out):: nvalues   ! Numero de elementos numeridos de X (exceto Null)  
	
!}
!{	Variaveis locais
	real::soma
	real::VAR    ! Varianca
	integer::i
	integer::ct
!}
!{ Inicializacao de variaveis
	soma=0.0
	i=0
	ct=0
	mean=null
!}
!{ Calculo da Media
	do i=1,nelements
		if(X(i)/=null) then 
			soma=soma+X(i)
			ct=ct+1
		end if
	end do
	if (present(nvalues)) nvalues=ct
	if (ct==0) return 
	mean=soma/real(ct)
!}
end function mean












!------------------------------------------------------------------------------
!meansd   ! subrotina para calculo de media e desvio padrao      !MCT-INPE-CPTEC!
!------------------------------------------------------------------------------
!   Esta subrotina calcula media e desvio padrao de um vetor X(:) com nelemets
!   Os elevemtos que tiverem valor Null nao sao computados
!
!------------------------------------------------------------------------------
! AUTOR: Sergio H. S. Ferreira

subroutine meansd(X,nelements,meanX,SDX,nvalues)

!{ Variaveis da interface
	real,dimension(:),intent(in)::X ! Vetor com os elementos a serem processados
	integer,intent(in)::nelements    ! Numero de elementos de X	
	real,intent(out)::meanX         ! Media de X
	real,intent(out)::SDX           ! Desvio Padrao de X
	integer,optional,intent(out)::nvalues ! Numero de elementos numericos em X
	
!}
!{	Variaveis locais
	real::VAR    ! Varianca
	integer::i
	integer::ct
!}
!{ Inicializacao de variaveis
	i=0
	ct=0
!} 
!{ Calculo da Media
	meanX=mean(X,nelements,ct)
	if (present(nvalues)) nvalues=ct
	if (meanX==Null) return

 !{ Calculo do Desvio Padrao

	do i=1,nelements
		if(X(i)/=null) then
			var=var+(X(i)-meanX)**2
		end if
	end do
	sdX=sqrt(var/real(ct))
 !}
end subroutine	meansd






!------------------------------------------------------------------------------
!QC1     ! Controle de qualidade 1                             !MCT-INPE-CPTEC!
!------------------------------------------------------------------------------
!   Esta subrotina elimina dados suspeitos de um conjunto X, adimitindo que
!   este conjunto tenha distribuicao normal e que elementos com valores superior
!   a mediaX+/-3*desvios Padroes sao eliminados
!
!   Os valores eliminados sao substituidos por valor null
!  Nota: XS=3 deve aceitar 95% da amostragem  
!
!------------------------------------------------------------------------------
! AUTOR: Sergio H. S. Ferreira

subroutine QC1(X,nX,PExc,meanX,SDX)

!{ Variaveis da interface
	real,dimension(:),intent(inout)::X ! Vetor com os elementos a serem processados
	integer,intent(in)::nX             ! Numero de elementos de X	
	real,intent(out)::PExc	           ! Percentagem de dados eliminados
	real,intent(out)::meanX 
	real,intent(out)::SDX
	
!}
!{ Variaveis locais

	integer::nexcl	                  ! Numero de elementos de X_out
	
	real::minl,maxl       ! Limites para aceitacao dos dados (minimo e maximo)
	real,parameter::XS=3.0  !4*(Desvio-padrao)
!}
!{ Iniciando Variaveis
	nexcl=0
!}


	call meansd(X,nx,meanX,SDX)
	
	if (SDX==null) return

	minl=meanX-SDX*XS
	maxl=meanX+SDX*XS

	do i=1,nx
		if ((X(i)<minl).or. (X(i)>maxl)) then
			nexcl=nexcl+1
			X(i)=null
		end if
	end do
	PExc=nexcl/nx*100.0

	end subroutine

 Subroutine sort2(a, NL, NC, CREF, dec)
	real,dimension(:),intent(inout)::a ! a matrix a(nl,nc)
	integer,          intent(in)   :: Nl !Number of line in a
	integer,          intent(in)   :: Nc !Number of column in a
 	integer,          intent(in)   :: CREF ! Reference colunm 
	logical         , intent(in)   :: dec ! If true -Decrescent if false crescent 
    
	integer Nll,r,c,rr
	nll=nl-1
 	real :: xx

	do i=1,nll 
    		r = 0
    		do  L = 1,NLL
  
		If (dec)  Then
    			If (a(L, CREF) < a(L + 1, CREF)) Then
                		do  C = 1, NC
                			xx = a(L, C)
                			a(L, C) = a(L + 1, C)
                			a(L + 1, C) = xx
                			!r = 1
                			!RR = 0
                		end do
            		end if 
    		Else
    			If (a(L, CREF) > a(L + 1, CREF)) Then
                		do C = 1, NC
                			xx = a(L, C)
                			a(L, C) = a(L + 1, C)
                			a(L + 1, C) = xx
                			!r = 1
                			!RR = 0
                		END DO 
    
    			End If
    		End If
    		END DO !l 
!    If (RR == 5) Exit 
!    If (r == 0 ) RR = RR + 1
    END Do 


     End Subroutine
end module
