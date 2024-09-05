
!******************************************************************************
!*                                STRINGFLIB                                  *
!*                                                                            *
!*     Biblioteca de funcoes e rotinas para manipulacao de variaveis-texto    *
!*                                                                            *
!*              Copyright (C) 2005, Sergio Henrique Soares Ferreira           *
!*                                                                            *
!*                   MCT-INPE-CPTEC, Cachoeira Paulista, Brasil               * 
!* SHSF: Sergio Henrique Soares Ferreira                                      *
!* ALTF: Ana Lucia Travezani Ferreira                                         *
!* SMJ : Saulo Magnum de Jesus                                                * 
!******************************************************************************
!*                                                                            *
!*  Esta biblioteca fornece um conjunto de subrotinas e funcoes uteis para    *
!*  manipulacao de variaveis-texto (Character) em FORTRAN                     *
!*                                                                            *
!******************************************************************************
!* DEPENDENCIAS: GetArgs (Sistema Operacional)                                *
!******************************************************************************
! 
! HISTORICO
!
!2005-03-10 - SHSF - Corrigido "bug" na funcao str_real que ocorre quando passado 
!                    o valor 0.0000E+00
!
!2007-01-20 - SHSF - Acrescido Subrotina GETARGS2 
!2007-03-22 - SHSF - Modificacao da rotina VAL- Antiga VAL agora esta como VAL2 
!2009-10-02 - SHSF - Acrescido funcao replace 
!2009-10-24 - SHSF - Corrigindo str_realS : Erro no truncamento de zeros a esquerda 
!                    quanto utiliza notacao cientifica
!2010-05-21 -SHSF/ALTF - Corrigido leitura de parametros em getarg2
!2010-09-07 -SHSF  - Corrigido funcao VAL para aceitar os codigos ascII<32, incluindo o TAB 
!2010-09-26 -SHSF  - Criado funcao isVal para verificar se uma linha tem apenas numeros ou nao
!2010-10-11 -SHSF e SMJ - Corrigido funcao split com relacao a separacao do ultimo caracter
!2014-03-02 -SHSF  - modificado read_config para avisar quando nao encontra um elemento. 
!2018-06-30 -SHSD - Aperfeicoado subrotinas set_natnum e split
!2018-07-06 -SHSF  - IgnoreVoid parameter was included
!2019-10-22 -SHSF  - color_text was included
!2020-07-20-SHSF  - Change the name of function rinst to rindex 
!2022-03-12 SHSF  - the "split_a" subroutine was revised 
 module stringflib

   implicit none
   !USE MSFLIB  ! Para compilacao em Windows ( Microsoft Power Station )



  !-----------------------------------------------------------------------------
  ! Subroutinas deste modulo
  !----------------------------------------------------------------------------
  ! GETARGS2 | Obtem lista de argumentos passados por paramentro
  ! IsVal    |Verifica se um texto contem apenas valores numericos
  ! lcases   |Converte texto para letras menusculas  (lowercase)
  ! split    |Separacao de "Strings" em "Sub-String"  
  ! strs     |Converte uma valiavel numerica em variavel texto (string) 
  ! ucases   |Converte um texto para letras maiuhsculas (upercase)
  ! val      |Converte texto em numeros
  ! rindex   |Find index of last occurrence of a substring in a string

  Real:: Null=-340282300       !valor nulo ou indefinido
 private null
  

!******************************************************************************
!  STRS| Converte uma valiavel numerica em variavel texto (string)     | SHSF |
!******************************************************************************
!                                                                             |
! Esta e uma interface para converter variaveis numericas (INTERGER ou REAL)  |
! em texto                                                                    |
!******************************************************************************
!{
	private str_intS	! Converte INTEGER em CHARACTER
	private str_realS	! Converte REAL em CHARACTER 

	interface STRS
		module procedure str_intS
		module procedure str_realS
		module procedure str_reals2
	end interface
	
!******************************************************************************
!  split| Splits a string into substrings.                             | SHSF |
!******************************************************************************
!                                                                             |
!Split a string into substrings delimited by separators characters.           |
!                                                                             |      
! call split ( string, separetors, [IgnoreVoid], substrings, nelements )      |
!                                                                             |
!      string: is the string to split.                                        |
!  separetors: (separators) is a separator character                          |
!  IgnoreVoid: is a boolean that tells Split() not to return void elements.   |
!              Default: IgnoreVoind=.true.                                    |
!   substring: is the subtring array returned                                 |
!   nelements: is the number of elements into substring array                 | 
!                                                                             |
!******************************************************************************
	interface split 
	         module procedure split_a ! Ignore Void element
		 module procedure split_b ! do not Ignore Void element
	end interface 
	

 !}
 !===========================================================================
	contains
 
 
 !set undefined value
!******************************************************************************
!  Init| initialize undefined value                                    | SHSF |
!******************************************************************************
 
 subroutine init_stringflib(undefval)
   real,intent(in)::undefval
   Null=undefval
 end subroutine 
 
!******************************************************************************
!  Sep_NatNum | separates natural numbers                              | SHSF |
!******************************************************************************
!                                                                             !
!   Esta subrotina separa numeros naturais contidos em um texto uma variavel  !
!                                                                             |
!******************************************************************************

Subroutine Sep_NatNum(string, element, nelements)

!{ Variaveis da Interface
     character(len=*), intent (in)      :: string    !Texto de entrada contendo Letras e Numeros          
     integer, dimension(:), intent (out):: element !Matriz contendo apenas os numeros que foram separados
     integer,               intent (out):: nelements !Numero de elementos em "substrings" 

!}
!{ Variaveis Locais
	 integer :: i,l,maxl,F 
	 character(len=1) ::DS
	 character(len=256) :: SS
!}

	F=0
	SS=""
	
	maxl=size(element,1)	  
	l = Len_trim(string)
	
	if (l==0) goto 100
		do i = 1,l
		
		dS = string(i:i)
		If (index("0123456789", dS)== 0) Then
			If (Len_trim(sS) > 0) Then
				F = F + 1
				element(F) = val(sS)
				sS = ""
			End If
		Else
			sS = trim(sS) // dS
		End If
		if (F==maxl+1) exit 
		end do !i

		If (sS /="") then ;F = F + 1; element(F) = val(sS); sS = "";end if

100	  nelements=F


End Subroutine Sep_NatNum


	
!******************************************************************************
!  Sep_NatNum | Separacao de Numeros Naturais                          | SHSF |
!******************************************************************************
! 																			  !
!   Esta subrotina separa numeros naturais contidos em um texto uma variavel  !
!																			  |
!******************************************************************************

Subroutine Sep_Num(string, substrings, nelements)
    
!{ Variaveis da Interface
     character(len=*), intent (in) :: string !.................. Texto de entrada contendo Letras e Numeros          
	 character(len=*), dimension(:), intent (out)::substrings  !.Matriz contendo apenas os numeros que foram separados
	 integer , intent (out) :: nelements !.....................  Numero de elementos em "substrings" 

!}
!{ Variaveis Locais
	 integer :: i,l,maxl,F 
	 character(len=1) ::DS
	 character(len=256) :: SS
!}

	F=0
	SS=""
	substrings=""
	maxl=size(substrings,1)	  
	l = Len_trim(string)
	
	if (l==0) goto 100
	    
		do i = 1,l
        
		  dS = string(i:i)
		  If (index("-0123456789.+", dS)== 0) Then
          
		     If (Len_trim(sS) > 0) Then
                            
				 F = F + 1
                 substrings(F) = trim(sS)
                 sS = ""

			  End If
           
		   Else
     	    
			  sS = trim(sS) // dS
           
		   End If
       	 
		 if (F==maxl+1) exit 

	   end do !i
      
	  If (sS /="") then ;F = F + 1; substrings(F) = sS; sS = "";end if
100	  nelements=F


End Subroutine Sep_Num

 
!******************************************************************************
!  split_a| Splits a string into substrings.                           | SHSF |
!******************************************************************************
!                                                                             |
!Split a string into substrings delimited by separators characters.           |
! (Void elements are ignoreted)                                               |
!                                                                             |      
! call split ( string, sep, substrings, nelements, [IgnoreVoid])              |
!   string   : is the string to split.                                        |
!   set      : (separators) is a separator character                          |
!   substring: is the substring array                                         |
!   nelements: is the number of elements into substring array                 |
!                                                                             |
!*****************************************************************************|
 subroutine split_a(string,sep,substrings,nelements)
!{ Variaveis da Interface
	character (len=*),              intent(in)::string     !is the string to split.
	character(len=*),               intent(in)::sep        !(separators) is a separator character    
	character (len=*),dimension(:),intent(out)::substrings !is the substring array 
	integer,                       intent(out)::nelements  !is the number of elements into substring array
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F, j
	 character(len=1)                    ::D
	 character(len=256)                  ::S       !Substring, auxiliary variable for 
         character(len=1024)                 ::auxline
	 integer                             ::ns            !Number of strings in S (without spaces at begging)
	 integer                             ::nss           !Number of strings in S (with space at begginig)
	 logical                             ::ivoid
 !}
	
	
	 maxl=size(substrings,1)
	 if (maxl<=2) goto 100
	 l=len_trim(string)
	 if (l==0) goto 100
	 
          
         F=0
	 S=""
	 ns=0
	 nss=0

         i=1
	!PRINT *,trim(string)
	 do while ( i<=l)
	 
	    D = string(i:i)
            j=0
	    
	    !{ Procura por texto entre aspas
            if (D=='"') then 
              auxline=string(i+1:len_trim(string))
	      j=index(auxline,'"')
	      if (j==0) then 
	       
	        print *,trim(color_text(":STRINGFLIB:split: Warning! A very long line has been truncated  ",33,.false.))
		print *,trim(string)
		
	      end if
            end if
            !}
	    
            if (j>0) then
              F=F+1
              auxline=string(i+1:i+j-1)
              substrings(F)=auxline
              i=i+j
            else    
		If (index(sep,D)>0) Then
	!		print *,ns,nss,f,S(1:ns)
			   if (nss>0) then
				F = F + 1 
				substrings(F) = S(1:ns)
				S = ""
				ns=0
				nss=0
			   end if
			   
		 Elseif(ichar(D)>31) then  
			nss=nss+1
			if (ns==0) then
				!Do not use space at begning 
				if (D/=" ") then
					ns=ns+1
					S=D
				end if 
			else 
				S = S(1:ns) // D
				ns=ns+1
			end if
		End If
	end if
		
		if (F==maxl+1) then 
	        print *,"Warning! Error in stringflib:split"
	        exit 
	      end if
              i=i+1
	   end do !i

	  If (S /= "") then 
           F = F + 1
	   ! Remove spaces at end 
           substrings(F) = trim(S(1:ns))
       end if
 
 100 continue 
     nelements=F
       
	
end subroutine
!******************************************************************************
!  split-b| Splits a string into substrings.                           | SHSF |
!******************************************************************************
!                                                                             |
!Split a string into substrings delimited by separators characters.           |
!                                                                             |      
! call split ( string, sep, substrings, nelements, [IgnoreVoid])              |
!   string   : is the string to split.                                        |
!   set      : (separators) is a separator character                          |
!  IgnoreVoid: is a boolean that tells Split() not to return void elements.   |
!   substring: is the substring array                                         |
!   nelements: is the number of elements into substring array                 |
!                                                                             |
!******************************************************************************
 subroutine split_b(string,sep,IgnoreVoid,substrings,nelements)
!{ Variaveis da Interface
	character (len=*),              intent(in)::string     !is the string to split.
	character(len=*),               intent(in)::sep        !(separators) is a separator character
	logical,                        intent(in)::IgnoreVoid !is a boolean that tells Split() not to return void elements.    
	character (len=*),dimension(:),intent(out)::substrings !is the substring array 
	integer,                       intent(out)::nelements  !is the number of elements into substring array
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F, j
	 character(len=1)                    ::D
	 character(len=256)                  ::S       !Substring, auxiliary variable for 
         character(len=1024)                  ::auxline
	 integer                             ::ns            !Number of strings in S
	 logical                             ::ivoid
 !}
	
	
	 maxl=size(substrings,1)
	 if (maxl<=2) goto 100
	 l=len_trim(string)
	 if (l==0) goto 100
	 
          
         F=0
	 S=""
	 ns=0

         i=1
	 do while ( i<=l)
	 
	    D = string(i:i)
            j=0
	    
	    !{ Procura por texto entre aspas
            if (D=='"') then 
              auxline=string(i+1:len_trim(string))
	      j=index(auxline,'"')
            end if
            !}
	    
            if (j>0) then
              F=F+1
              auxline=string(i+1:i+j-1)
              substrings(F)=auxline
              i=i+j
            else    
		If (index(sep,D)>0) Then
		  
				F = F + 1 
				if (ns==0) then 
				  substrings(F)=""
				else 
				  substrings(F) = S(1:ns)
				end if
				
				S = ""
				ns=0
			   
		 Elseif(ichar(D)>31) then  
		    if (ns==0) then
		     
		      !Do not use space at begning  and remove void element
		      if (D/=" ") then
		       ns=ns+1
		       S=D
		     end if 
		     
		    else 
		      S = S(1:ns) // D
		      ns=ns+1
		    end if
        	 End If

              end if
	      if (F==maxl+1) then 
	        print *,"Warning! Error in stringflib:split"
	        exit 
	      end if
              i=i+1
	   end do !i

	  If (S /= "") then 
           F = F + 1
	   ! Remove spaces at end 
           substrings(F) = trim(S(1:ns))
       end if
 
 100 continue 
     nelements=F
       
	
end subroutine
 
!******************************************************************************
!  split| Separacao de "Strings" em "Sub-String"                       | SHSF |
!******************************************************************************
!                                                                             |
!  Decompoe um string em um conjunto e sub-strings segundo um caracter de     |
!  separacao.                                                                 |
!                                                                             |
!******************************************************************************
 subroutine split3(string,sep,substrings,nelements)
!{ Variaveis da Interface
	character (len=*), intent (in) :: string !............exto  de entrada
	character(len=*),intent(in)::sep ! ............Caracteres de seperacao
	character (len=*), dimension(:), intent (out)::substrings !..... Palavras separadas do texto
	integer , intent (out) :: nelements !..........................Numero de palavras
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F, j
	 character(len=1) ::D
	 character(len=256) :: S
         character(len=256) ::auxline
 !}
	 F=0
	 S=""
	 substrings=""
	 maxl=size(substrings,1)
	 if (maxl<=2) goto 100
	 l=len_trim(string)
	 if (l==0) goto 100
          
         !{ Procura por texto entre aspas

         i=1
	 do while ( i<=l)
	 
	    D = string(i:i)
            j=0
            if (D=='"') then 
              auxline=string(i+1:len_trim(string))
	      j=index(auxline,'"')
            end if

            if (j>0) then
              F=F+1
              auxline=string(i+1:i+j-1)
              substrings(F)=auxline
              i=i+j
            else    
		If (index(sep,D)>0) Then
			If (Len(trim(S)) > 0) Then
				F = F + 1 
				substrings(F) = S
				S = ""
			End If
		 Else
		    S = TRIM(S) // D
        	 End If

              end if
		 if (F==maxl+1) exit 
           i=i+1
	  end do !i

	  If (S /= "") then 
           F = F + 1
           substrings(F) = S
       end if
 
 100  nelements=F
end subroutine
!******************************************************************************
!  split| Separacao de "Strings" em "Sub-String"                       | SHSF |
!******************************************************************************
!                                                                             |
!  Decompoe um string em um conjunto e sub-strings segundo um caracter de     |
!  separacao.                                                                 |
!                                                                             |
!******************************************************************************
 subroutine split2(string,sep,substrings,nelements)
!{ Variaveis da Interface
	character (len=*), intent (in) :: string !............exto  de entrada
	character(len=*),intent(in)::sep ! ............Caracteres de seperacao
	character (len=*), dimension(:), intent (out)::substrings !..... Palavras separadas do texto
	integer , intent (out) :: nelements !..........................Numero de palavras
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F, j
	 character(len=1) ::D
	 character(len=256) :: S
         character(len=256) ::auxline
 !}
	 F=0
	 S=""
	 substrings=""
	 maxl=size(substrings,1)
	 if (maxl<=2) goto 100
	 l=len_trim(string)
	 if (l==0) goto 100
          
         !{ Procura por texto entre aspas

         i=1
	 do while ( i<=l)
	 
	    D = string(i:i)
            If (index(sep,D)>0) Then
			If (Len(trim(S)) > 0) Then
				F = F + 1 
				substrings(F) = S
				S = ""
			End If
		 Else
		    S = TRIM(S) // D
        	 End If

            if (F==maxl+1) exit 
           i=i+1
	  end do !i

	  If (S /= "") then 
           F = F + 1
           substrings(F) = S
       end if
 
 100  nelements=F
end subroutine


!******************************************************************************
! sep_words	| Separa as palavras que compoe uma linha de texto     | SHSF |
!******************************************************************************
!                                                                             |
!  Separa as pavras de uma linha de texto em uma matriz de palavras,          |
!                                                                             |
!******************************************************************************

 subroutine sep_words(string,words,nwords)

 !{ Variaveis da Interface
	 character (len=*),               intent (in) :: string !Texto  de entrada
	 character (len=*), dimension(:), intent (out):: words  !Palavras separadas do texto
	 integer ,                        intent (out):: nwords !Numero de palavras
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F 
	 character(len=1) ::D
	 character(len=256) :: S
 !}
	 F=0
	 S=""
	 words=""
	 maxl=size(words,1)
	 if (maxl<=2) goto 100
	 l=len_trim(string)
	 if (l==0) goto 100
	 
	 do i=1,l
	 
	    D = string(i:i)
		If (index(" ,;-",D)>0) Then
			If (Len(trim(S)) > 0) Then
				F = F + 1 
				words(F) = S
				S = ""
			End If
		 Else
		    S = TRIM(S) // D
		
         End If
		 if (F==maxl+1) exit 
      
	  end do !i
      	
	  If (S /= "")  F = F + 1; words(F) = S; S = ""
 
 100  nwords=F
 	 
	End Subroutine
!******************************************************************************
! sep_text	| separates a line of text in columns    | SHSF |
!******************************************************************************
!                                                                             |
! separates a line of text in columns.   
! separates a line of text in columns. The elements of separation cab be:
!  comma, semicolon, quotes and braces                                        |
!                                                                             |
!******************************************************************************

 subroutine sep_text(string,words,nwords)

 !{ Variaveis da Interface
	 character (len=*),               intent (in) :: string !Texto  de entrada
	 character (len=*), dimension(:), intent (out):: words  !Palavras separadas do texto
	 integer ,                        intent (out):: nwords !Numero de palavras
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F 
	 character(len=1) ::D
	 character(len=256) :: S
 !}
	 F=0
	 S=""
	 words=""
	 maxl=size(words,1)
	 if (maxl<=2) goto 100
	 l=len_trim(string)
	 if (l==0) goto 100
	 
	 do i=1,l
	 
	    D = string(i:i)
	    
		If (index(" ,;",D)>0) Then
			If (Len(trim(S)) > 0) Then
				F = F + 1 
				words(F) = S
				S = ""
			End If
		 Else
		    S = TRIM(S) // D
		
         End If
		 if (F==maxl+1) exit 
      
	  end do !i
      	
	  If (S /= "")  F = F + 1; words(F) = S; S = ""
 
 100  nwords=F
 	 
	End Subroutine


!******************************************************************************
! str_ints  | Converte uma variavel inteira em uma variavel-texto      | SHSF |
!******************************************************************************
!  Funcao tipo Character que converte o valor de uma variavel inteira nos     | 
!  caracteres correspondentes                                                 |
!******************************************************************************
function str_intS(a); character(len=256) ::str_intS

!{ Variavel de Interface	
	 integer , intent (in) :: a
!}

!{ Variavel Local

	character(len=256)::b
!}
	 b=""
	 write(b,*)a
	 str_intS=adjustl(b)

	end function


!******************************************************************************
! str_reals	| Converte uma variavel REAL em uma variavel-texto     | SHSF |
!******************************************************************************
!  Funcao tipo Character que converte o valor de uma variavel REAL nos        | 
!  caracteres correspondentes                                                 |
!******************************************************************************

 function str_reals(a);character(len=256)   ::str_reals,b
 
 !{ Variaveis da Interface
  real , intent (in) :: a
  !}

 !{ Variaveis Locais
	 integer ::p,r
 !}

   
	 b=""
	 write(b,*)a
	 b=adjustl(b)

	 p=index(b,".")

!{ Eliminando E+00 e E-00

	r=INDEX(B,"E+00")
	IF (R>0) B=B(1:R-1)

	R=INDEX(B,"E-00")
	IF (R>0) B=B(1:R-1)
!}
  
     
!{Eliminando zeros a esquerda se nao for notacao cientifica 
	if (index(B,"E") ==0) then
	   
11	 r=len_trim(b)

	 if ( (b(r:r)=="0").and.(r>p).and.(p/=0))then 
		 r=r-1
		 b=b(1:r)
		 goto 11
	 end if
	end if 
!} 	 

	 if ((r==p).and.(p>0)) then 
	    r=r-1
		b=b(1:r)
	end if
    
	str_realS=adjustl(b)
	 
	end function
!******************************************************************************
! str_reals	| Converte uma variavel REAL em uma variavel-texto     | SHSF |
!******************************************************************************
!  Funcao tipo Character que converte o valor de uma variavel REAL nos        | 
!  caracteres correspondentes                                                 |
!******************************************************************************

 function str_reals2(a,s);character(len=256)   ::str_reals2,b
 
 !{ Variaveis da Interface
  real , intent (in) :: a
  integer,intent(in)::s
  integer::aux
  !}

 !{ Variaveis Locais
	 integer ::p,r
 !}

   
	 b=""
	 write(b,*)a
	 b=adjustl(b)

	 p=index(b,".")

!{ Eliminando E+00 e E-00

	r=INDEX(B,"E+00")
	IF (R>0) B=B(1:R-1)

	R=INDEX(B,"E-00")
	IF (R>0) B=B(1:R-1)
!}
  
     
!{Eliminando zeros a esquerda se nao for notacao cientifica 
	if (index(B,"E") ==0) then
	   if( s>0) then 
	     aux=int(a*10**(s)+0.5)
         b=""
         write(b,*)aux
         b=adjustl(b)
         r=len_trim(b)
         b=b(1:r-s)//"."//b(r-s+1:r)
       elseif(s==0) then 
         aux=int(a+0.5)
         write(b,*)aux
	   !else
	   
	    ! r=log(a)/log(10.0)
	     !aux=a/10.0**r
	     !write(b,*)aux
	   end if
	endif
	str_realS2=adjustl(b)
	 
	end function
		



!******************************************************************************
! VAL	| Converte CHARACTER em REAL                                   | SHSF |
!******************************************************************************
!  Funcao para converter texto com caracteres numericos em uma variavel       |
!  REAL.                                                                      |
!                                                                             |
!  Caso a texto contiver caracteres invalidos VAL retornara o valor "NULL"    |
!  (Veja declaracao da variavel NULL)                                         |
!                                                                             |
!******************************************************************************
 function VAL2(AS);real VAL2

 !{ Variaveis da interface
	 character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
	 real            ::b
	 character(len=1):: cS
	 character(len=255)::numS,aaS
	 integer i,l,chknum
 !}

	 chknum=1
	 numS=""
	 aaS=adjustl(aS)
	 l=len_trim(aaS)
	 
	 if (l>0) then 
	   
		do i=1,l
	 
			cS=aaS(i:i)
			if ((ichar(CS)<32).OR.(CS=='"')) CS=" "
	 
			if (index(" 0123456789.+-",cS)>0) then
			
				if ((CS=="-").and.(len_trim(numS)>0)) chknum=0
				if ((CS=="+").and.(len_trim(numS)>0)) chknum=0
				if ((CS==".").and.(index(numS,".")>0))chknum=0
				if (chknum==1) numS=trim(numS)//cS
				
				
			else
				chknum=0	 
			end if
		end do

		
		if (chknum==1) then 
			read(numS,*)b
		else 
			b=NULL
		end if

     else 
		b=NULL
	 end if

	 val2=b
	end function	
!******************************************************************************
! IsVAL	| Verify if a text has only numerical characters               | SHSF |
!******************************************************************************
!  Funcao que ferifica se um texto possui apenas caracteres numericos  
!  If .true.  there are only numerical characters 
!                                                                             |
!******************************************************************************
 function IsVAL(AS);logical  IsVAL

 !{ Variaveis da interface
   character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
    character(len=1):: cS
    character(len=255)::numS,aaS
    integer i,l
 !}
  IsVal=.false.
  aaS=adjustl(trim(aS))
  l=len_trim(aaS)
  
  if (l>0) then 
    do i=1,l
      cS=aaS(i:i)
      if (ichar(CS)<32) CS=" "
      if (index("0123456789.+- ",cS)>0) then
        isVal=.true.
      else
        IsVal=.false. 
        exit
      end if
    end do
  end if

end function
  

!******************************************************************************
! VAL   | Converte CHARACTER em REAL                                   | SHSF |
!******************************************************************************
!  Funcao para converter texto com caracteres numericos em uma variavel       |
!  REAL.                                                                      |
!                                                                             |
!  Caso a texto contiver caracteres invalidos VAL retornara o valor "NULL"    |
!  (Veja declaracao da variavel NULL)                                         |
!                                                                             |
!******************************************************************************
!HISTORICO
!  20110904 : Acrescentado eliminacao do caracter @ quando junto ao numero ou
!            de eliminacao de caracteres abaixo de ASC 32
 function VAL(AS);real VAL

 !{ Variaveis da interface
   character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
    real            ::b
    character(len=1):: cS
    character(len=255)::numS,aaS
    integer i,l,chknum
    integer::n1,n2
    integer::IE
    integer::rep 
!}
  chknum=0
  numS=""
  aaS=adjustl(trim(aS))
  l=len_trim(aaS)

  if (l>0) then 
    rep=0
    do i=1,l
      cS=aaS(i:i)
      if (CS==",") cs="."
      if ((ichar(CS)>=32).and.(CS /="@")) then
      if ((CS==".").and.(rep==1)) CS=" "
      if (CS==".") rep=1
      if (index("0123456789",cS)>0) chknum=1
      if (index("0123456789.+-E",cS)>0) then
        numS=trim(numS)//cS
      else
        exit
      end if
      end if

    end do
   IE=index(numS,"E")
   if (index(numS,"-")>(IE+1)) chknum=0
   if (index(numS,"+")>(IE+1)) chknum=0
   if (trim(aas)=="-") chknum=0
    if ((chknum==1).and.(len_trim(numS)>0)) then 
      read(numS,*)b
    else 
      b=NULL
    end if
  else 
    b=NULL
  end if
  val=b
end function
!******************************************************************************
! IVAL	| Converte CHARACTER em Inteiro                                | SHSF |
!******************************************************************************
!  Funcao para converter texto com caracteres numericos em uma variavel       |
!  REAL.                                                                      |
!                                                                             |
!  Caso a texto contiver caracteres invalidos VAL retornara o valor "NULL"    |
!  (Veja declaracao da variavel NULL)                                         |
!                                                                             |
!******************************************************************************
 function IVAL(AS);integer:: IVAL

 !{ Variaveis da interface
	 character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
	 real            ::b
	 character(len=1):: cS
	 character(len=255)::numS,aaS
	 integer i,l,chknum
 !}
	 chknum=0
	 numS=""
	 aaS=adjustl(trim(aS))
	 l=len_trim(aaS)


	 if (l>0) then 
	   
		do i=1,l
	 
			cS=aaS(i:i)
			if (ichar(CS)<32) CS=" "
	 
			if (index("0123456789.+-",cS)>0) then
				chknum=1
				numS=trim(numS)//cS
			else
				exit
			end if
		end do
	
		if ((chknum==1).and.(len_trim(numS)>0)) then 
			read(numS,*)b
		else 
			b=NULL
		end if
			

     else 
		b=NULL
	 end if
	 ival=b
	end function
!******************************************************************************
! UCASES| Todos as letras MAIUSCULAS                                   | SHSF |
!******************************************************************************
!  Funcao que retorna um texto em "CAIXA ALTA" de um texto                    |
!                                                                             |
!  Exemplos:                                                                  |
!                  char(97)="a" --> char(65)="A"                              |
!                  char(122)="z" --> char(90)="Z"                             |
!******************************************************************************     
function UCASES(str);character(len=255)::UCASES

	!{ Variaveis da interface	 
		 character(len=*)::str
	!}
	!{ Variaveis locais
		 character(len=255)::b
		integer :: i,l,a
	!}

	 
	 b=str
	 l=len(trim(str))
	 if (l>0) then 
	 do i=1,l
	 
	     a=iachar(str(i:i))
	     
		 if ((a>=97).and.(a<=122)) then 
		    a=a-32
	   	    b(i:i)=achar(a)
		 end if
	 
	 end do
	 end if
	 ucaseS=b
	 
	 end function   
	 
!******************************************************************************
! LCASE| Todas as letras MENUSCULAS                                    | SHSF |
!******************************************************************************
!  Funcao que retorna um texto em "CAIXA BAIXA" de um texto                   |
!                                                                             |
!  Exemplos:                                                                  |
!                char(65)="A" --> char(97)="a"                                |
!                char(90)="Z" --> char(122)="z"                               |
!*****************************************************************************|
 
function LCASES(str); character(len=255)::LCASES

	 !{ Variaveis de Interface
	 character(len=*)::str
	 !}

	 !{ Variaveis internas
	 character(len=255)::b
	 integer :: i,l,a
	 !}


	 b=str
	 l=len(trim(str))
	 
	 do i=1,l
	 
	   a=iachar(str(i:i))

	   if ((a>=65).and.(a<=90)) then 
	      
		  a=a+32
	   	  b(i:i)=achar(a)
	   
	   end if

	 end do
	 
	 lcaseS=b
	 
	 end function  lcaseS


!******************************************************************************
! RIGHTS| Obtem os caracteres a direita de um texto                   | SHSF |
!******************************************************************************
	function rightS(char,lenth); character  :: rightS

	!{ Variaveis da interface
	 character (len=*),intent (in) :: char !....................Texto original 
         integer,intent (in):: lenth !..........comprimento do texto a ser obtido 
	!}
	
	 !{ Variaveis locais	
	  character (len=len(trim(char))):: a
	  integer 			:: l,i
	 !}

	 a=trim(char)
	 l=len(a)
	 i=l-lenth+1
	 rightS=a(i:l)	
	end function rightS 


!******************************************************************************
! LEFTS| Obtem os caracteres a esquerda de um texto                   | SHSF |
!******************************************************************************

	function leftS(char,lenth);	 character  :: leftS

	!{ Variaveis de interface
	 character (len=*),intent (in) :: char	 !....................Texto original
	 integer, intent (in) :: lenth	!..........comprimento do texto a ser obtido

	!}
		 
	 leftS=char(1:lenth)
	end function leftS	  	  


!******************************************************************************
! CUTSTRING| Corta um texto                                            | SHSF |
!******************************************************************************
!  Corta uma linha de texto a partir da primeira ocorrecia de um caracter
!  especificado                                                               |
!*****************************************************************************|


subroutine CutString(line,char)
	!{ Variaveis de interface
		character(len=255),intent(inout)::line ! Linha de texto 
		character(len=1),intent(in)::char 	   ! Caracter para corte
	!}
	!{Variaveis locais
		integer :: cp
	!}

		cp=index(line,char)-1

		if (cp>0) then 

			line=line(1:cp)

		elseif(cp==0) then 
			line=""
		end if

		
	end subroutine 

!******************************************************************************
!BETWEEN_INVCOMMAS|                                                    | SHSF |
!******************************************************************************
!* Obtem a primeira ocorrencia de um texto delimitado por aspas duplas        *
!*                                                                            *
!******************************************************************************
function  between_invdcommas (line); character(len=255)::between_invdcommas

!{ Variaveis de interface
	character(len=*),intent(in)::line
!}

!{ Variaveis locais	 
	integer::i
	character(len=255)::auxline
 !}
	auxline=line
	between_invdcommas=""

	i=index(auxline,'"')

	if (i==0) return
	 
	auxline=auxline(i+1:len_trim(auxline))
	
	i=index(auxline,'"')
	
	if (i==0) return 
		
	between_invdcommas='"'//auxline(1:i+1)
end function


!------------------------------------------------------------------------------
!GETARGS2 ! OBTEM ARGUMENTOS PASSADOS EM LINHA DE COMANDOS              !SHSF !
! -----------------------------------------------------------------------------
!  ESTA SUBROTINA E BASEADA NO  COMANDO GETARG DO UNIX.                       !
!                                                                             !
!  Ao inves de obter os argumentos tal como sao digitados (getarg),  esta     !
!  sub-rotina interpreta as letras que precedidadas por "-" como indicativo   !
!  do tipo de argumento que esta sendo passado                                !
!                                                                             !
!     ex.:   programa -d 20APR2007                                            !
!                                                                             !
!     Neste exemplo "d" e o nome do argumento  e "20apt2007" eï¿½ o valor do  !
!     argumento                                                               !
!                                                                             !
!   Programa Exemplo                                                          !
!     integer,paramenter::x=<Numero de agumentos esperados>                   !
!     character(len=1),dimension(100)::namearg !.......... Nome dos argumentos!
!     character(len=256),dimension(100)::arg  !.................... argumentos!
!     integer::nargs      !........ numero de argumentos efetivamente passados!
!     integer::i          !................................. Variavel auxiliar!
!                                                                             !
!     call getarg2(namearg,arg,nargs)                                         !
!                                                                             !
!     do i=1, nargs                                                           !
!        print *,namearg(i)," = ",trim(arg(i))                                !
!     end do                                                                  !
!  Nota: a Dimensao de namearg e arg estabelece o numero maximo de argumentos !
!        que podem ser passados. Caso sejam passados mais argumentos do que   !
!        esta dimensao, os ultimos argumentos serao ignorados                 !
!-----------------------------------------------------------------------------!
!  DEPENDEDIAS: getarg (sistema operacional)                                  !
!-----------------------------------------------------------------------------!

  subroutine getarg2(argnames,args,nargs)
!{ Variaveis da interface	
   character(len=*),dimension(:),intent(out)::argnames
   character(len=*),dimension(:),intent(out)::args
   integer,intent(out)::nargs
!}
!{ Variaveis locais
 integer*2::argc
 character(len=1024)::indate
 integer::i,j ,k
 integer iargc !It is  necessary if pfg compiler 
!}

   i=ubound(argnames,1)
   argc =  iargc()
   if (argc>i) argc=i
   argnames(1:argc)=" "
   args(1:argc)="                    "
   i=0
   j=0
   k=0
    DO while (i<=argc)
      j=j+1
      10 i=i+1
      call GetArg(i,indate)
      if (len_trim(indate)>0) then
        if ((index(indate,"-")==1).and.(len_trim(indate)==2)) then 
       
          if(k==1) j=j+1
          argnames(j)=indate(2:2)
          k=1
          goto 10
        else 
          if (argnames(j)=="") argnames(j)="?"
          args(j)=indate
          k=0
        end if
      end if 
    end DO
    nargs=j
end subroutine getarg2



!------------------------------------------------------------------------------
!GETVARS ! OBTEM ATRIBUICAO DE VARIAVEIS                                !SHSF !
!-----------------------------------------------------------------------------!

  subroutine getvars(string, varname,values,nvars,nvarmax)
  !{ Variaveis de interface
	character(len=*),intent(in)::string
	character(len=*),dimension(:),intent(out)::varname
	character(len=*),dimension(:),intent(out)::values
	integer,intent(out)::nvars  
	integer,intent(in)::nvarmax 
   !}
   !{Variaveis locais
     integer::nelements
	 character(len=255),dimension(nvarmax)::substring
	 integer::i
   !}
   
	call sep_words(string,substring,nelements)
	i=0
	nvars=0
	do while (i<nelements)
		i=i+1
		if (trim(substring(i))=="=") then 
			i=i+1
			values(nvars)=substring(i)
		else
			nvars=nvars+1
			varname(nvars)=substring(i)
		end if
	end do

end subroutine	getvars

! |----------------------------------------------------------------------------|
!|getpath | Obtem o caminho de um arquivo                                |SHSF|
!'|-----------------------------------------------------------------------------
!'|
Function getpath(filename);character(len=1024)::getpath
	character(len=*),intent(in)::filename    
	
	character(len=1024)::f
	integer::p,p1,p2

	p1 = rindex(filename, char(92))
	p2 = rindex(filename, "/")
	if (p1>p2) then
		p=p1
	else
		p=p2
	end if


    If (p > 0) Then
        f = filename(1:p)
    Else
        f = ""
    End If
    getpath = f
End Function
 
!------------------------------------------------------------------------------
! rinst or rindex
!----------------------------------------------------------------------------
! Find index of last occurrence of a substring in a string - Stack ...
!---------------------------------------------------------------------------
 Function rindex(A, B);integer::rindex
  !{ Variaveis de interface 
   character(len=*),intent(in)::A !String
   character(len=*),intent(in)::B !Substring
  !}
  !{ Variaveis localis
    integer::l1
    integer::l2 
    integer::k,i 
  !} 
   l1 = Len_trim(A)
   l2 = Len_trim(b)
   k = 0
   do i = 1,l1 - l2   
     If (A(i:i+l2-1) == B) k = i
   end do
  rindex = k
End Function

!---------------------------------------------------------------------------------
! replace 
!---------------------------------------------------------------------------------
! Subrititui os caracteres a por b em uma linha texto

function replace(line,a,b);character(len=1024)::replace
  !Variaveis da interface
	character(len=*),intent(in)::line
	character(len=*),intent(in)::a
	character(len=*),intent(in)::b
  !}
	integer::llmax,almax,l
	character(len=1024)::linha
	character(len=1024)::c
	linha=line
	almax=len_trim(a)-1
	llmax=len_trim(linha)
	l=0
	do while (l<(llmax-almax))
	  l=l+1
	  llmax=len_trim(linha)
	  c=linha(l:l+almax)
	
	  if(trim(c)==trim(a)) then 
	      if (l==1) then 
		linha=trim(b)//linha(l+almax+1:llmax)
	      else
		linha=linha(1:l-1)//trim(b)//linha(l+almax+1:llmax)
	      end if
	  end if
	end do

	replace=linha

end function
!---------------------------------------------------------------------------
!prox  | Verifica se dois numeros reais sao muito proximos | SHSF 
!----------------------------------------------------------------------
!  Se rval1 ~ rval2  prox = .true.
!  se rval1 /= rval2 prox = .false. 

function near(rval1,rval2);logical::near
  real,intent(in)::rval1
  real,intent(in)::rval2
  real::dif
  dif=abs(rval1-rval2)

  if (dif<0.0000001) then
      near=.true.
  else
      near=.false.
  end if
end function


!------------------------------------------------------------------------------
!rrnamelist  | Remove Remarks from namelist vector                      | SHSF 
!------------------------------------------------------------------------------
! Remove comentarios  do vetor de lista de nomes(namelist)
! Nota: Este subrotina foi escrita para resolver problemas de compatibilidade
! com a leitura de namelist que contenham comentarios, ao lado do namelist
! No caso do programa compilado com GFORTRAN, quando a comentarios "!" no lado 
! direito, a leitura de um vetor � interrompida, o que nao ocorre quando o 
! programa e compilado com outros compiladores
! Para lidar com esta incompatibilidade, pode se usar o comentario entre aspas (" ")
! Assim o comentario entra como parte do vetor. 
! Esta rotina elimina os comentarios lidos desta forma verificando se o elemento da
! lista inicial com o sinal "!" indicando que � um comentario.
! ---------------------------------------------------------------------------------
subroutine rrnamelist(nlist,ne)
!{ Interface
   character(len=*),dimension(:),intent(inout)::nlist !namelist elements 
   integer,intent(in)                         ::ne    !Number of elemens in namelist (without remarks
!}

!{ local 
   integer::i,j
   character(len=256),dimension(ne)::auxnlist
!}

  i=0
  j=0
  do while (i<ne)
    j=j+1
    if (index(nlist(j),"!")/=1) then 
      i=i+1
      auxnlist(i)=nlist(j)
    end if
  end do  
    
  do i=1,ne
    nlist(i)=auxnlist(i)
  end do

end subroutine

!------------------------------------------------------------------------------
! read_config | Ler arquivo de listas de parametros de configuracao. | SHSF
!-----------------------------------------------------------------------------
! Esta subrotina trata de uma forma alternativa ao uso dos namelist do fortran
! para passagem de parametros de configuracao do programa.
! Como existe diferentes versos de fortran com estrutura diferentes de namelist
! o uso de namelist tradicional pode gerar problemas de compatibilidade quando
! se migra de uma versao para outra do compilador fortran. Assim uma forma
! independente de se ler parametros de configuracao e implementada nesta rotina
!
! Modelo de arquivo de
! :list_name:
!    valor 1 ! Comentarios
!    valor 2 ! comentarios
!   ...
!   valor n  ! Comentarios
! ::
!-----------------------------------------------------------------------------
! Programa exemplo
!program example
! use stringflib
! character(len=255),dimension(1:100)::values
! integer ::nelements,i
!
!   call read_config(1,"extractor.cfg","outfile_options",values,nelements)
!   do i=1,nelements
!    print *,trim(values(i))
!   end do
!end program
!------------------------------------------------------------------------------
! Historico
  
subroutine read_config(un,filename,list_name,values,nelements)
  !{ Interface
     integer,                      intent(in) ::un         ! Unidade de leitura
     character(len=*),             intent(in) ::filename   !Nome do arquivo de parametros
     character(len=*),             intent(in) ::list_name  !Nome da lista de parametros
     character(len=*),dimension(:),intent(out)::values     !Lista de valores lidos (numericos ou caracteres)
     integer,                      intent(out):: nelements ! Numero de elementos lidos na lista 
  !}
  !{ local
  character(len=255)::linha,listname
   integer::i,j,ncc
   character(len=255),dimension(2000)::cc
   integer::maxe
  !}

    listname=":"//trim(ucases(list_name))//":"
    nelements=0
    maxe=size(values,1)
    open (un,file=filename,status='unknown')
      200 read(un,'(a)',end=999)linha
          call cutstring(linha,"!")
          linha=ucases(linha)
          if (index(trim(linha),trim(listname))==0) goto 200  
          i=0
         do while (i==0)

            read(un,'(a)',end=999) linha
            call cutstring(linha,"!")
            i=index(linha,"::") 
            if (i==0) then
              call split3(trim(linha),',[] ',cc,ncc)
              do j=1,ncc
                nelements=nelements+1
                if (nelements>maxe) then
		  print *,":STRINGFLIB:READ_CONFIG: Filename=",trim(filename) 
                  print *,":STRINGFLIB:READ_CONFIG:Error: Number of readed element exceded the maximum values=",maxe
		  
                  stop
                end if
                values(nelements)=cc(j)
              end do
            else 
            end if
          end do
      999 close(un)
      
      !
      ! This part was removed temporarily while the common table is not completed
      !
        !  if (nelements==0) then 
        !    print *," :STRINGFLIB:READ_CONF: WARNING ",trim(listname)," not found in ",trim(filename)
            
         ! end if 
end subroutine read_config


!------------------------------------------------------------------------------
! color_text |returns a colored text for linux terminal (xwindows)      | SHSF
!-----------------------------------------------------------------------------
! Given a color code and text and if the text is bold or not 
!, it returns a colored text for linux terminal (xwindows)
! Usege: 
!   print *, color_text("My text", color, bold)
! 
!   color: 
!
!   30 -> 39  normal color code  
!   40 -> 47  background color 
!   90 -> 97  light color 
!
!  bold = .true, or .false.
!
!  Some colors
!    30 - Black
!    31 - Red
!    32 - green
!    33 - Brow/Orange
!    34 - Blue
!    35 - Purple
!    35 - Cyna
!    27  -Light gray
!-------------------------------------------------------------------------------
function color_text(text,color,bold);character(len=1024)::color_text
		!Interface
		integer,         intent(in)::color
		character(len=*),intent(in)::text
		logical,         intent(in)::bold
		!-----
		!Local
		character(len=2)::c
		!---
		
		write (c,'(i2)')color
		if (.not.bold) then  
		color_text=achar(27)//'[0;'//c//'m '//trim(text)//achar(27)//'[0m'
		else
		color_text=achar(27)//'[1;'//c//'m '//trim(text)//achar(27)//'[0m'
		end if 
	end function 
	
function color(colorname); integer::color
	character(len=*)::colorname
	color=30
	if(colorname=="red") color=31
	if(colorname=="green") color=32
	if(colorname=="orange") color=33
	if(colorname=="blue") color=34
	if(colorname=="purple") color=35
end function
end module stringflib
