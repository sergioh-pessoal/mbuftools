module mcodesflags
!******************************************************************************
!                                   mcodesflags 
! Module to get the significations associated with codes and flags used in
! the FM94-BUFR format
!                                   
!(Módulo para obter os significados associados a códigos e bandeiras utilizados
! no formato FM94-BUFR)
!
!********************************************************************************
! HISTORICO 201411- SHSF - acrescido valor do campo associado
! 20180523 - SHSF - Acrescentado significado de descritores da tabela C (Apenas 2-07-yyy)
! 20180619 - SHSF - Defesa contra uso de caixa baixa/alta na idenrificacao das codetables
! 20190606 - SHSF - Substitui o uso da tabela comum C1 pela tabela C11- Falta incluir a C12
! 20191113 - SHSF - Corrigido leitura da tabela C-11 para a categoria 0
 use stringflib
 use mbufr, only:get_name_mbufr,realk,undef
 implicit none
 private
 public tabA
 public tabCC1
 public tabCC13
 public init_mcodesflags
 public signification_mcodesflags

 type codelist
  character(len=80)::description
  integer          ::descriptor
  integer          ::code
 end type
 
 type associated_field_table
  integer::Code
  integer,dimension(255)::Sub1_code
  character(len=80),dimension(255)::Sub1_name
 end type

 character(len=255)                 ::mbufr_tables
 character(len=255)                 ::mbufr_tableA
 character(len=255)                 ::mbufr_tableB
 character(len=255)                 ::codeflagtables
 character(len=50),dimension(0:255) ::tabA
 character(len=50),dimension(0:255) ::tabCC1
 character(len=50),dimension(0:255,0:255)::tabCC13
 integer,parameter                  ::nrucmax=2000
 character(len=255)                 ::line
 type (codelist), dimension(nrucmax)::ruc                 !list of recently used codes
 type (associated_field_table),dimension(63)::aftable
 integer                            ::nruc                !Number of elements in the ruc
 integer                            ::ncol
 integer                            ::btype,bsubtype     ! Tipo e Subtipo BUFR 
 character(len=50),dimension(1:50)  ::tab                !Tabela generica 
 integer                            ::Associated_Field_Signficance_code ! If zero No associated field
 
 contains

!--------------------------------------------------------------------------------
! initializes this module and loads the table-A and  the table-C1
!--------------------------------------------------------------------------------
!
!---------------------------------------------------------------------------------
 subroutine init_mcodesflags(path2tables)
 !{ Interface
   character(len=*),intent(in)::path2tables
 !}
 !{ local variables
   character(len=50)          ::ncod
   integer                    ::icod
   integer                    ::i,j
   character(len=255)         ::CTable_name
   character(len=255)         ::AFTable_name
 character(len=200),dimension(1:1000)::values
 integer                             ::nelements
   
 !}
  Associated_Field_Signficance_code=0
 !------------------
 ! loads the TableA
 !------------------
 !{
    tabA(:)=""
    if(len_trim(path2tables)==0) then  
       call getenv('MBUFR_TABLES',mbufr_tables)
    else 
       mbufr_tables=path2tables
    end if
    !{ Acrescenta barra no final do diretorio local_tables, caso seja necessario
    ! Nesse processo veirifica se o diretorio contem barras do windows ou barra do linux 

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
	 
551		read(2,'(i3,1x,a50)',end=661)icod,ncod
		tabA(icod)=ncod
		goto 551	
661		continue
    close(2)
!}

!--------------------
! loads the table-C1
!-------------------
	CTable_name=trim(mbufr_tables)//"Common_C11.csv"
	open(2,file=CTable_name,status="unknown")
	tabCC1(:)=""
		read(2,'(a)',end=663) line
553		read(2,'(a)',end=663) line
		call split(line,',',tab,ncol)
		if (ncol>=5) then 
			icod=val(tab(3))
			if ((icod>=0).and.(icod<256)) then 
				tabCC1(icod)=tab(4)
			end if
		end if 
		!read(2,'(i3,1x,a)',end=663)icod,ncod
		!tabCC1(icod)=ncod
		goto 553
663	 continue
	close(2)
!}

!--------------------
! loads the table-C13
!-------------------
	CTable_name=trim(mbufr_tables)//"Common_C13.csv"
	open(2,file=CTable_name,status="unknown")
	tabCC13(:,:)=""
	        read(2,'(a)',end=712)line
711		read(2,'(a)',end=712)line
               ! print *,trim(line)
		call split(line,',',tab,ncol)
		 if (ncol>5) then
		   btype=val2(tab(2))
		   bsubtype=val2(tab(4)) 
		   tabcc13(btype,bsubtype)=trim(tab(5))
		  !PRINT *,BTYPE,BSUBTYPE,TABCC13(BTYPE,BSUBTYPE)
		 end if 
		
	!	tabCC1(icod)=ncod
		goto 711
712	 continue
	close(2)
!}

!-----------------------------------
! Read  the Associated Field table
!----------------------------------
	AFTable_name=trim(mbufr_tables)//"Associated_Filed_tables.txt"
	aftable(:)%code=0
	call read_config(20,AFTable_name,"5",values,nelements)
	j=0
	
	do i=1,nelements,2
		j=j+1
		aftable(5)%code=5
		aftable(5)%sub1_code(j)=val(values(i))
		aftable(5)%sub1_name(j)=trim(values(i+1))
	end do
!	open (2,file=AFTable_name,status="unknown")
!713	read(2,'(a)',end=714)line
!		print *,">",trim(line)
!		goto 713
!714	continue
!	close(2)
!----------------------------------
!  Reads the code and flag tables
!--------------------------------
 codeflagtables=trim(mbufr_tables)//"Code-Flag_tables.txt"
 nruc=0
 !print *,signification_mcodesflags(001007,1.0)
 
end subroutine


function signification_mcodesflags(descriptor,codefigure,associated_field)
 integer,          intent(in)::descriptor 
 real(kind=realk), intent(in)::codefigure
 integer*2        ,intent(in)::associated_field

 character(len=200)                  ::signification_mcodesflags
 character(len=200),dimension(1:1000)::values
 integer                             ::i,nelements
 logical                             ::found 
 character(len=200)                  ::description
 character(len=6)                    ::cdescriptor
 integer ::aux
 integer::AFSC
 write(cdescriptor,'(I6.6)')descriptor
 
 !if (codefigure<0) goto 3737 <---????  Porque isto estava aqui ?  

!-------------------------------------
!Check if it is a associated field
!----------------------------------
AFSC=Associated_Field_Signficance_code

 if (associated_field>0) then 
   write(signification_mcodesflags,'("Value of the associated field (size =",i3,"bits)")')associated_field
   if (AFSC>0) then 
	if (AFtable(AFSC)%code>0) then 
		do i=1,255
			if (AFtable(AFSC)%sub1_code(i)==codefigure) then 
				write(signification_mcodesflags,'(a," <- Value of the associated field")')trim(AFtable(AFSC)%sub1_name(int(i)) )
				exit
			end if
		end do
	end if
   end if 
   goto 3737
 end if 
 
! XXXX REVISAR ESTA PARTE. ESTA DANDO ERRO 
 if (descriptor==031021) then
    if (codefigure>undef()) then 
        Associated_Field_Signficance_code=codefigure
    else
        Associated_Field_Signficance_code=0
    end if 
 end if 
 
!--------------
! table C
!-------------
  
  if ((descriptor>207000).and.(descriptor<207999)) then 
     write(signification_mcodesflags,'("Increase scale, reference value and data width. Scale=",i3)')descriptor-207000
     return 
  end if
  if (descriptor==207000) then 
     signification_mcodesflags="Increase scale, reference value and data width. Cancel"
     return
  end if
  
  
!-----------------------------------------------------------------
! Check if it is the case of a descriptor associated a CODE TABLES 
!-----------------------------------------------------------------
 description=get_name_mbufr(descriptor)
 signification_mcodesflags=description
 if (index(ucases(description),"CODE")==0) goto 3737

 found=.false.

 

!---------------------------------------
! Check the list of recently used codes
!---------------------------------------
   do i=1,nruc
     if(ruc(i)%descriptor==descriptor) then 
       if (ruc(i)%code==codefigure) then 
         signification_mcodesflags=trim(ruc(i)%description)//" <- "//trim(description)
         found=.true.
         exit
       end if
     end if
   end do


  
!
!
!
  if (.not. found) then 
     call read_config(20,codeflagtables,cdescriptor,values,nelements)
    if(mod(nelements,2)>0) then
      print *,"Warning: Error reading codeflagtables: Descriptor=",trim(cdescriptor)
    end if 
     do i=1,nelements,2
      if (val(values(i))==codefigure) then 
         signification_mcodesflags=trim(values(i+1))//" <- "//trim(description)
         nruc=nruc+1
         if (nruc>nrucmax) nruc=nrucmax
         ruc(nruc)%descriptor=descriptor
         ruc(nruc)%description=values(i+1)
         ruc(nruc)%code=codefigure
        ! write (*,22),ruc(nruc)%descriptor,ruc(nruc)%code,trim(ruc(nruc)%description)
        !22 format(2x,":MCODESFLAGS: Including [descriptor,code,description]=",i6.6,1x,i4,1x,a)
       end if
     end do
     close(20)
   end if
3737 continue

end function
end module
