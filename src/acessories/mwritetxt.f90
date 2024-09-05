module mwritetxt
!------------------------------------------------------------------------------!
!                               mwritetxt                                      !
!           Modulo para gravacao de arquivos texto das ferramentas bufr        !
!        	       (Module to write text files  form BUFR TOOLS )          !
!									       !
!                  (c) 2009: Sergio H.S.Ferriera (SHSF) [i]  		       !
!									       !	
!------------------------------------------------------------------------------!
!HISTORICO
!   20090602- SHSF - Versao orginal (extraido do software: bufrdump
!   20170623- SHSF - Comecado subrotina merge_tabd para atualizacao da tabela D. 
!                    (falta comcluir)

  use mbufr
  use stringflib
  implicit none 

  public 
  character(len=50),dimension(0:255)  ::tabA,tabCC1
  character(len=255)                  ::mbufr_tables,mbufr_tableA,mbufr_tableB,mbufr_commonTableC1
  character(len=50)                   ::ncod
  character(len=50),dimension(0:99999)::tabncod ! Tabela B
  real                                ::null
  contains
!------------------------------------------------------------------------------!
! init | Inicializacao (inicialization)                                 | SHSF !
!------------------------------------------------------------------------------!
!  Inicializa tabelas e variaveis deste modulo		                       !
!  (inicialize tables and variables of this module)                            !
!                                                                              !
!  ler as tabelas A, CommonTables, e a B00000000.txt existentes                !
!  A tabela B000000 é guardada em tabncod                                      ! 
!------------------------------------------------------------------------------!
! Exemplo:                                                         !    
!        Call init_mwrite													   !
!------------------------------------------------------------------------------!
  subroutine init_mwritetxt
	
	!{ Variaveis localis (Local Variables )
	integer::i,icod
	character(len=6)::ccod
	!}
	
        print *,":MWRITETXT:Init"
	call getenv('MBUFR_TABLES',mbufr_tables)
        null=undef()
	
	i=len_trim(mbufr_tables)
	if ((mbufr_tables(i:i)/=char(92)).and.(mbufr_tables(i:i)/="/")) then 
		if (index(mbufr_tables,char(92))>0) then 
			mbufr_tables=trim(mbufr_tables)//char(92)
		else
			mbufr_tables=trim(mbufr_tables)//"/"
		end if
	end if

	mbufr_tableA=trim(mbufr_tables)//"BufrTableA.txt"
	
	open(2,file=mbufr_tableA,status="unknown")
	 
551		read(2,'(a3,1x,a50)',end=661)ccod,ncod
                icod=val(ccod)
                if (icod>0) then 
		   tabA(icod)=ncod
		end if
		goto 551	
661		continue
    close(2)
!}

!{ Ler OS nome dos descritores da tabela comum C1

	mbufr_commonTableC1=trim(mbufr_tables)//"CommonTableC1.txt"
	open(2,file=mbufr_commonTableC1,status="unknown")
	tabCC1(:)=""
553		read(2,'(a3,1x,a)',end=663)ccod,ncod
                icod=val(ccod)
                if (icod>0) tabCC1(icod)=ncod
		goto 553
663	 continue
	close(2)
 !}

 !{ Ler os nomes dos descritores da tabela B
    mbufr_tableB=trim(mbufr_tables)//"B0000000000.txt"!trim(mbufr_tableB)
	open(2,file=mbufr_tableB,status="unknown")
		tabncod(:)=""
555		read(2,'(1x,a6,1x,a50)',end=666)ccod,ncod
                icod=val(ccod)
		if(icod>0) tabncod(icod)="" ! ncod Provisorio, Tabela B000000 na e uma boa idea
		goto 555
666	continue
	close(2)
!}



  end subroutine

!------------------------------------------------------------------------------!
! write_sec1txt |  Escreve a secao BUFR 1 em formato texto              | SHSF !
!------------------------------------------------------------------------------!
!	(Write tge BUFR SECTION 1 IN TEXT FORMAT )                                 !
!------------------------------------------------------------------------------!
! Exemple:  																   !
!        open (un,file=filename,status='unknown')                              !    
!        Call write_sec1txt (un,sec1)                                          !
!------------------------------------------------------------------------------!

  subroutine write_sec1txt(un,sec1)

!{ Variaveis da interface (Intaface variables )

	integer,intent(in)::un  !.................Unidade de gravacao  
	type(sec1type),intent(in)::sec1 !.......Dados da secao 1 do BUFR
 
!}

	
		write(un,'(1X,a)')":SEC1:"
		write(un,'(1x,I4," # BUFR MASTER TABLE")')sec1%NumMasterTable
		write(un,'(1X,I4," # ORIGINATING CENTER: ",a50)')sec1%center,tabcc1(sec1%center)
		write(un,'(1X,I4," # ORIGINATING SUBCENTER")')sec1%subcenter
		write(un,'(1X,I4," # UPDATE SEQUENCE NUMBER")')sec1%update
		write(un,'(1X,I4," # DATA CATEGORY: ",a50)')sec1%bType,tabA(sec1%btype)
		write(un,'(1X,I4," # DATA SUBCATEGORY ")')sec1%intbsubtype
		write(un,'(1X,I4," # LOCAL DATA SUBCATEGORY ")')sec1%bsubtype
		write(un,'(1X,I4," # BUFR MASTER TABLE VERSION NUMBER")') sec1%VerMasterTable
		write(un,'(1X,I4," # LOCAL TABLE VERSION NUMBER")') sec1%VerLocalTable
		write(un,'(1X,I4," # YEAR ")')sec1%year		
		write(un,'(1X,I4," # MONTH ")')sec1%month
		write(un,'(1X,I4," # DAY ")')sec1%day
		write(un,'(1X,I4," # HOUR ")')sec1%hour
		write(un,'(1X,I4," # MINUTE ")')sec1%minute


  end subroutine
!------------------------------------------------------------------------------!
! write_sec3txt |  Escreve a secao BUFR 3 em formato texto              | SHSF !
!------------------------------------------------------------------------------!
!	(Write the BUFR SECTION 3 IN TEXT FORMAT )                                 !
!------------------------------------------------------------------------------!
! Exemple:  																   !
!        open (un,file=filename,status='unknown')                              !
!        Call write_sec1txt (un,sec1)                                          !    
!        Call write_sec3txt (un,sec3)                                          !
!------------------------------------------------------------------------------!

  subroutine write_sec3txt(un,sec3)

!{ Variaveis da interface

	integer,intent(in)::un !.............Unidade de gravacao
	type(sec3type),intent(in)::sec3	!....Dados da secao 3

!}
 	integer::i,nsubsets
	write(un,'(1X,a)')":SEC3:"
	
		
	write(3,'(1X,i5," # Num.subsets")')sec3%nsubsets
	write(3,'(1X,i5," # Num.descriptors")')sec3%ndesc
	write(3,'(1x,i5," # Flag for Compressed data (1=compressed 0=uncompressed)")')sec3%is_cpk
	nsubsets=sec3%nsubsets
	
	do i=1,sec3%ndesc
		write(3,'(6x,i6.6)')sec3%d(i)
	end do
 
 end subroutine

!------------------------------------------------------------------------------!
! write_sec4txt |  Escreve a secao BUFR 4 em formato texto              | SHSF !
!------------------------------------------------------------------------------!
!	(Write the BUFR SECTION 4 IN TEXT FORMAT )                             !
!------------------------------------------------------------------------------!
! Exemple:  																   !
!        open (un,file=filename,status='unknown')                              !
!        Call write_sec1txt (un,sec1)                                          !    
!        Call write_sec3txt (un,sec3)                                          !
!        Call write_sec4txt (un,sec4,sec3%subsets)                             !
!------------------------------------------------------------------------------!

 subroutine write_sec4txt(un,sec4,nsubsets)
 !{ Variaveis da interface
   integer,       intent(in)::un	!Unidade de gravacao 
   type(sec4type),intent(in)::sec4	!Dados da secao 4
   integer,       intent(in)::nsubsets  !Numero de subsets na secao 4
 !}
 !{ Variaveis locais
  
    integer                          ::l4,i,j
    character(len=255)               ::txt,txt2
    character(len=1024)              ::AUXTXT,line
    character(len=255),dimension(100)::linel
    character(len=18)                ::UNAME
    character(len=64)                ::VNAME
    integer                          ::nchar 
    integer                          ::iline    ! Identificacao da linha (Descritor) que identifica os elementos da tabela
    integer                          ::ctr,numchar
    integer                          ::f,x,y
    integer                          ::refval,scalef
    integer                          :: scalesgn,refsgn,nbits
    logical                          ::IsTabD
    integer,dimension(1000)          ::descd
    integer                          ::ndesc,id
!}
    print *,":MWRITETXT:write_sec4txt: nsubsets=",nsubsets
    IsTabD=.false.
    write(un,'(1X,":SEC4:")')
    l4=0
    write(3,'(1x,i4," # N. VARIABLES !!!")')sec4%nvars
    line=""
    do j=1,nsubsets
    numchar=0  
    ctr=0
    auxtxt=""

      write(3,'(5x,":SUBSET ",I5.5,":")')j
        
       !--------------------------------------------------------------   
       ! Obtem o texto relativo a um descritor que ja esta na tabela B
       ! E guarda em txt 
       !---------------------------------------------------------------
       do i=1,sec4%nvars
         if (sec4%d(i,j)==null) goto 66
         if ((sec4%d(i,j)<99999).and.(sec4%d(i,j)>0)) then 
            txt=tabncod(sec4%d(i,j))
            txt2=txt
         else
          txt=""
          txt2=""
        end if

      !-----------------------------------------------------------------------------------------
      !Processa as variaveis caraceres. Os descritores estao todos como variaveis caracteres que
      !que precisam ser acumuladas ate obter a palavra completa 
      !-----------------------------------------------------------------------------------------
      !{
 
      if (sec4%C(i,j)>0) numchar=numchar+1
       !{ Se for variavel corrente ou anterior for caracter entao processa essa parte	
      ! print *,i,j,sec4%d(i,j),sec4%c(i,j),numchar
   55  if (numchar>0) then	
				!print *,sec4%d(i,j),sec4%c(i,j),sec4%r(i,j),numchar
	if (sec4%c(i,j)==numchar) then !...(Se Variavel corrente acumulla os caracteres)
				    
              IF (numchar>255) then 
                          print *,"Erro" 
                          stop
                        numchar=255 
                 end if
		 auxtxt(numchar:numchar)=char(int(sec4%r(i,j)))
                                   
                                    
	 else  !......................(Se Variavel  anterior, entao imprime a variavel)
				
         
         if (i==1) then 
             goto 66
         end if
        !--------------------------------
         ! Obtensao dos dados da tabela A 
         !-------------------------------
         ! Descritores de 0-00-001 a 0-00-009 Sao relativos as informacoes 
         ! da tabela A  entao, caso descritor <10 consireasse dados da tabela A                       
	   if (sec4%d(i-1,j)<10) then 

			iline=sec4%d(i-1,j)
			if ((iline==1).and.(len_trim(line)>0)) then    
			                       ! write(*,*)trim(line)
				 		write(30,*)trim(line)
				 		!stop
					 	line=""   
			 end if 

				if (iline==2) then 
					line=trim(line)//" "//trim(auxtxt)
				else
					line=trim(line)//trim(auxtxt)                                                          
				end if
				!}	
	 !-------------------------------
	 ! Obtencao dos dados da tabela B
	 !---------------------------------
	 ! Se nao for dados da tabela A nem D entao e da tabela B
	 ! Se o F dentro da tabela For F= 3 Entao e dado da tabela D
	 ! ctr =0 na primeira vez que passa, para acumular os caracteres. 
	 ! Quando retorna novamente no iline=10, indica que os dados acumulados 
	 ! antes sao gravados 
	 
  	   elseif  (.not. IsTabD) then  
                            
              !{ PROCESSAMENTO DA TABELA B
             
              iline=sec4%d(i-1,j)
              if (iline==10)  then

                                if ((ctr>1).and.(F==0).and.(X/=0)) then 
                         
                                   !write(*,400)F,X,Y,VNAME,UNAME,scalef,refval,nbits
                                   write(40,400)F,X,Y,VNAME,UNAME,scalef,refval,nbits
                                   !stop
	                           ctr=0
                                end if
                                F=val(auxtxt)

	                        IF (F==3) then 
	                           IsTabD=.true.
	                           id=0
                                   ndesc=0
                                end if

              end if
							
             ctr=ctr+1
             if (iline==11) X=val(auxtxt)
             if (iline==12) Y=val(auxtxt)
             if (iline==15) UNAME=auxtxt
             if (iline==13) VNAME=trim(auxtxt)
             if (iline==14) VNAME=trim(VNAME)//trim(auxtxt)
             if (iline==16) then  
                               scalesgn=1
                               if (index(auxtxt,"-")>0) scalesgn=-1
             end if			   		

           if (iline==17) scalef=val(auxtxt)*scalesgn
 	   if (iline==18) then  
                              refsgn=1
                              if (index(auxtxt,"-")>0) refsgn=-1
           end if			   	
           if (iline==19) refval=val(auxtxt)*refsgn
           if (iline==20) NBITS=val(auxtxt)
					
          !}					
         elseIF (IsTAbD) then ! Tabela D


         !{ PROCESSAMENTO DA TABELA D
		 iline=sec4%d(i-1,j)
               !  {Obtem o numero de descritores da sequiencia
   	   	if (iline==31001)  then
       		    ndesc=sec4%r(i-1,j)
			    id=0
		end if
	!}
	!{ Toda sequencia de uma chave foi lida. Imprime esssa sequencia da tabela D
		if ((ndesc==id).and.(ndesc>0)) then 
		          write(50,500)F,X,Y,NDESC,DESCD(1)
					do id=2,ndesc
					  write(50,'(11x,I6.6)')DESCD(ID)
					end do
					ndesc=0
		end if
						
		!{ Obtencao dos descritores chaves da tabela D	
		if (iline==10) F=val(auxtxt)
 		if (iline==11) X=val(auxtxt)
 		if (iline==12) Y=val(auxtxt)
 		!}
		!{ Obtencao da sequencia de descritores relacionados a chave
			if (iline==30) then 
				id=id+1
				descd(id)=val(auxtxt)
			end if 
 			!}
						
         end if
				

       !{ Gravacao normal ARQUIVO X0000000 
        if (LEN_TRIM(AUXTXT)<=40) then
	   l4=l4+1
	   write(un,'(1x,a40,1x,"# ",i4,") ",i6.6,"-"a50)')trim(auxtxt),l4,sec4%d(i-1,j),TRIM(txt2)
        else
	   l4=l4+1
           write(un,*) trim(auxtxt)," ",sec4%d(i-1,j)
        end if
    !}
    numchar=sec4%c(i,j)
    auxtxt=""
    goto 55
				
 end if
					
else
!}
!-----------------------------------------------------------------------------
! Se for variavel do tipo numerica entao processa essa parte 							   
! O  que e esperado e que o programa passe aqui primeiro antes de prcoessar
! as tabelas. Aqui tambem sao gravadas a parte da decodificacao das tabelas, 
! que nao sao compostas por caracteres. Os valores daqui completan os dados 
! do arquivo  X0000000...
!--------------------------------------------------------------------------
      !{Obtem o numero de descritores da sequiencia
      if ((sec4%d(i,j)==205064).and.(sec4%d(i+1,j)==031001))  then
                                            ndesc=sec4%r(i+1,j)
                                            id=0
       end if
       txt=""
      if(sec4%r(i,j)/=null) then 
                           l4=l4+1
                            write(3,'(1x,F22.5," # ",i4,") ",i6.6,"-",a50)')sec4%r(i,j),l4,sec4%d(i,j),txt
      elseif (sec4%d(i,j)<99999) then  
	                    l4=l4+1
                            write(3,'(1x,a22," # ",i4,") ",i6.6,"-",a50,1x)')"Null",l4,sec4%d(i,j),txt
					
      else 
	                    l4=l4+1
                             write(3,'(1x,a22," # ",i4,") ",i6.6)')"Null",l4,sec4%d(i,j)
      end if
					  
				!}
 end if 

66  continue

	   	    
end do !nvars
end do	 ! nsubsets
400 Format (1x,I1,I2.2,I3.3,1x,a64,1x,a18,i10,i13,i4)
500 FORMAT (1x,I1,I2.2,I3.3,1x,I2.2,1X,I6.6)

 end subroutine


!-----------------------------------------------------------------------------
! Junta a tabela BUFR B antiga a tabela BUFR nova atualizando os descritores
!------------------------------------------------------------------------------
 subroutine merge_tabb(tabb1,center,version,local)
 !{ Variaveis da interface
 
	character(len=255),intent(in)::tabb1  ! Novos descritores para tabela B(B0000000...)
	integer,intent(in)           ::center ! Centro gerador da tabela a ser atualizada
	integer,intent(in)           ::version! Versao da tabela a ser atualizada
	integer,intent(in)           ::local  ! Versao local da tabela a ser atualizada
 !}
 !{Variaveis locais
 	character(len=255)                   ::tabb2  !Nome da tabela a ser atualizada
	integer                              ::icod
	character(len=200)                   ::ncod
	character(len=200),dimension(1:99999)::tabncod
 !}
 
	tabncod(:)=""
      
   write(tabb2,'("B000",I3.3,I2.2,I2.2,".txt")')center,version,local 
   tabb2=trim(mbufr_tables)//trim(tabb2)
   print *,"Updating table B = ",trim(tabb2)
!{ Leitura da tabela B a ser atualizada
	open(2,file=tabb2,status="unknown")
555		read(2,'(1x,i6,1x,a200)',end=666)icod,ncod
		tabncod(icod)=ncod
		goto 555
666	continue
	close(2)
!}


!{ Gravacao dos descritores atualizados 

	open(2,file=tabb2,status="unknown")

	do icod=1,99999
		if (len_trim(tabncod(icod))>10) then 
			write(2,'(1x,i6.6,1x,a)')icod,trim(tabncod(icod))

		end if
	end do
	close(2)
!}
 end subroutine
!-----------------------------------------------------------------------------
! Junta a tabela BUFR B antiga a tabela BUFR nova atualizando os descritores
!------------------------------------------------------------------------------
 subroutine merge_tabb2(tabb1,center,version,local)
 !{ Variaveis da interface
 
	character(len=255),intent(in)::tabb1  ! Novos descritores para tabela B(B0000000...)
	integer,intent(in)           ::center ! Centro gerador da tabela a ser atualizada
	integer,intent(in)           ::version! Versao da tabela a ser atualizada
	integer,intent(in)           ::local  ! Versao local da tabela a ser atualizada
 !}
 !{Variaveis locais
 	character(len=255)                   ::tabb2  !Nome da tabela a ser atualizada
	integer                              ::icod
	character(len=200)                   ::ncod
	character(len=200),dimension(1:99999)::tabncod
 !}
 
	tabncod(:)=""
      
   write(tabb2,'("B000",I3.3,I2.2,I2.2,".txt")')center,version,local 
   tabb2=trim(mbufr_tables)//trim(tabb2)
   print *,"Updating table B = ",trim(tabb2)
!{ Leitura da tabela B a ser atualizada
	open(2,file=tabb2,status="unknown")
555		read(2,'(1x,i6,1x,a200)',end=666)icod,ncod
		tabncod(icod)=ncod
		goto 555
666	continue
	close(2)
!}

!{ Leitura dos novos valores e atualizacao dos descritores

	open(2,file=tabb1,status="unknown")
556		read(2,'(1x,i6,1x,a200)',end=667)icod,ncod
		tabncod(icod)=ncod
		goto 556
667	continue
	close(2)
!}

!{ Gravacao dos descritores atualizados 

	open(2,file=tabb2,status="unknown")

	do icod=1,99999
		if (len_trim(tabncod(icod))>10) then 
			write(2,'(1x,i6.6,1x,a)')icod,trim(tabncod(icod))

		end if
	end do
	close(2)
!}
 end subroutine


!-----------------------------------------------------------------------------
! Junta a tabela BUFR D antiga a tabela BUFR nova atualizando os descritores
!------------------------------------------------------------------------------
 subroutine merge_tabd(tabd1,center,version,local)
 !{ Variaveis da interface
 
	character(len=255),intent(in)::tabd1  ! Novos descritores para tabela B(B0000000...)
	integer,intent(in)           ::center ! Centro gerador da tabela a ser atualizada
	integer,intent(in)           ::version! Versao da tabela a ser atualizada
	integer,intent(in)           ::local  ! Versao local da tabela a ser atualizada
 !}
 !{Variaveis locais
 	character(len=255)                  ::tabd2  !Nome da tabela a ser atualizada
	integer, dimension(9999)          ::icod
	integer*2,dimension(9999)           ::ndesc
	character(len=200),dimension(9999,200)::ncod
	integer                              ::i,j,ix
	integer                              ::aux1,aux2
	character(len=200)                   ::aux3
         logical                             ::icod_located	
 !}
 
	tabncod(:)=""
      
!{ Leitura da tabela a ser atualizada 

   write(tabd2,'("D000",I3.3,I2.2,I2.2,".txt")')center,version,local 
   tabd2=trim(mbufr_tables)//trim(tabd2)
    print *,"Updating table D = ",trim(tabd2)
   i=0; j=0
	open(2,file=tabd2,status="unknown")
	
566             read(2,'(1x,i6,1x,i3,a200)',end=677)aux1,aux2,aux3
                !print *,aux1,aux2,trim(aux3)
		if (aux2>0) then
		   i=i+1
		   j=1 
		   icod(i)=aux1
		   ndesc(i)=aux2
		   ncod(i,j)=aux3
		else
		   j=j+1
		   ncod(i,j)=aux3
		end if
		! Impressao para verificacao de leitura
		!if (j>=ndesc(i)) then 
		! print *,icod(i),ndesc(i),trim(ncod(i,1))
		! do j=2,ndesc(i)  
		!    print *,"                   ",trim(ncod(i,j))
		! end do
		!end if
		!}
		
		if (j>=ndesc(i)) j=0
		
		goto 566
677	continue
	close(2)
	
!{ Abrir e atualizar arquivo original
       ix=i
       i=0
       j=0
	open(2,file=tabd1,status="unknown")
	
577            read(2,'(1x,i6,1x,i3,a200)',end=688)aux1,aux2,aux3
                !print *,aux1,aux2,trim(aux3)
		if (aux2>0) then
		   icod_located=.false.
		   do i=1,ix
		      if (aux1==icod(i)) then 
		   	icod_located=.true.
		        exit
		      end if
		   end do
		   
		   if ( .not. icod_located) then
		     ix=ix+1
		     i=ix
		   end if
		   icod(i)=aux1
		   ndesc(i)=aux2     
		   j=1 
		   ncod(i,j)=aux3	
		else
		   j=j+1
		   ncod(i,j)=aux3
		end if
		! Impressao para verificacao de leitura
		!if (j>=ndesc(i)) then 
		! print *,icod(i),ndesc(i),trim(ncod(i,1))
		! do j=2,ndesc(i)  
		!    print *,"                   ",trim(ncod(i,j))
		! end do
		!end if
		!}
		
		if (j>=ndesc(i)) j=0
		
		goto 577
688	continue
	close(2)	
	
!}
        open(2,file=tabd2,status="unknown")
       	do i=1,ix 
	   write(2,'(1x,i6.6,1x,i2.2,1x,a)')icod(i),ndesc(i),trim(ncod(i,1))
	   do j=2,ndesc(i)
	     write(2,'(11x,a)')trim(ncod(i,j))
	   end do
	end do
     close(2)
 end subroutine
end module
