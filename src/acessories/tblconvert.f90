!******************************************************************************
!*    Program to convert bufrtable in text format from WMO (comma separated)  *
!*                        to text format used in MBUFR                        *
!******************************************************************************
!* CPTEC/INPE
!* Sergio Henique Soares Ferreira (SHSF) CPTEC/INPE
!* --------------------------------------------------------------------------
!* HISTORY
! 20130906- SHSF- Initial version
! 20141126- SHSF- Modificado para gravar a parte do CREX na tabela B e compatibilizar
!                 com formato das tabelas usadas no ECMWF-MARS
! 20151008 SHSF - Corrigindo BUFR na gravacao da ultima sequencia do processamento da tabela D. Contornado problema da leitura das linhas com separacao de virgula
!                 Contudo precisa rever o algoripimo de separacao com virgula quando as virgulas ocorrem entre aspas
! 20210711 SHSF -the input format has been updated to make it compatible with the text format available on WMO GitHub  
! 20220604 SHSF -fix a bug reading WMO table with  13 or 14 collumns
program tblconvert
  use stringflib, only:split,split2,replace,val,isval,getarg2,ucases,rights,color_text
  implicit none
  character (len=1024)             ::infile      !-Input file name
  character (len=1024)             ::outfile     !-Output file name
  character (len=2048)             ::line        !-A text line
  character (len=256),dimension(100)::cols        !-columns
  integer                          ::ncols       !-Number of columns 
  integer                          ::version     !-BUFR MASTER TABLE VERSION NUMBER
  integer                          ::master      !-BUFR MASTER TABLE NUMBER
  integer                          ::center      !-ORIGINATING CENTER
  integer                          ::local       !-LOCAL TABLE VERSION NUMBER
  character(len=255)               ::mbufr_tables!-MBUFR_TABLES directory

  integer                          ::C1,C2       ! Variaveis auxiliares p/ CREX 
  character(len=1)                 ::tbl         !-"B" or "D"   
  character(len=6),dimension(300,2)::tbld        !-A sequence of descriptors associated
                                                 ! with a descriptor father (Table D).Col1=fxy1, col2=fxy2
  integer                          ::ntbld       !-Number of descriptors in tbld.
  character(len=1),dimension(10)   ::argname     !-Argument name.
  character(len=255),dimension(10) ::arg         !-An argument. 
  integer                          ::narg        !-Number of Arguments 

  character(len=3)                 ::KVV
  integer                          ::i,k,l,x1,x2,f
  character(len=255)               ::auxc,title 
  character(len=6)   :: fxy ! Descriptor (fxxyyy)
  character(len=64)  ::desc ! Description 
  character(len=22)  ::uni  ! Unit 
  integer            ::scalef !Scale factor
  integer            ::refval ! Reference Value
  integer            ::nbits  ! size (number of bits)
  character(len=22)  ::crex1 ! UNIT of then variablel in CREX
  integer            ::crex2
  integer            ::crex3
  character(len=10),dimension(4)::ss
  integer                       ::nss

!-----------------  
! **  Welcome **
!-----------------
!{

  x1=0
  x2=0
  f=0
  call getarg2(argname,arg,narg)
   do i=1,narg
      if (argname(i)=="i") then 
          infile=arg(i)
          x1=1
      elseif (argname(i)=="o") then
          KVV=arg(i)
          if (len_trim(arg(i))==3) x2=1
      elseif (argname(i)=="f") then
          f=val(arg(i))
      end if
    end do
    x1=x1*x2
    if (x1==0) then
      print *,"---------------------------------------------------------"
      print *," INPE tblconvert: Converts format of BUFRtables          "
      print *," from WMO's text format or from OPERA csv to the text    "
      print *," format used by MBUFR "
      print *,"---------------------------------------------------------"
      print *,"use:"
      print *," tblconvert -f type -i infile -o KVV"
      print *,""
      print *,"          type   = 0 (WMO table in txt format )  "
      print *,"                 = 1 (OPERA table in cvs format)  "
      print *,"          infile = BUFR table from WMO (text format) "
      print *,"          K      = B or D - (Table B or Table D)         "
      print *,"          VV     = BUFR MASTER TABLE VERSION NUMBER    "
      print *,"--------------------------------------------------------"
      stop
     else
      print *,"---------------------------------------------------------"
      print *," CPTEC/INPE tblconvert: Converts format of BUFRtables    "
      print *," from WMO's text format to the text format used by MBUFR "
      print *,"---------------------------------------------------------"
    endif
!}
 !----------------
 ! initialization
 !----------------
 !{
   if (f==0) then
     master=0
     center=46
     local=0
     version=val(KVV(2:3))
      tbl=KVV(1:1)
   
   elseif (f==1) then   
      ! Tabelas de radar OPERA sao definidas pelo nome do arquivo de entrada
      !  localtabZ_Y_X.csv. Onde : 
      !    Z = B OU D
      !    Y = CENTRO GERADOR
      !    X = VERSAO (VERSAO LOCAL?!)
      !    Nota: A Versao geral e definida como versao 0 
      master=0
      center=247
      local=8
      version=0
      tbl=KVV(1:1)
   
      call split(infile,"_",ss,nss)
      if (nss==3) then 
         master=0
         center=val(ss(2)); if (center>255) center=247
         local=val(ss(3))
         tbl=ucases(rights(ss(1),1))
          
          print *,trim(infile),nss
       end if
    end if
   
   call getenv('MBUFR_TABLES',mbufr_tables)
   
   write(outfile,'(A1,2I3.3,2I2.2,".txt")')tbl,master,center,version,local
 
   !--------------------------------------------------------------------------
   ! Adds slash at the end of the path, if necessary.
   ! In this process, check if the path contains "/" or "\" (Windows or Linux) 
   !--------------------------------------------------------------------------
   !{
    i=len_trim(mbufr_tables)
    if ((mbufr_tables(i:i)/=char(92)).and.(mbufr_tables(i:i)/="/")) then 
        if (index(mbufr_tables,char(92))>0) then 
           mbufr_tables=trim(mbufr_tables)//char(92)
        else
           mbufr_tables=trim(mbufr_tables)//"/"
        end if
    end if
   outfile=trim(mbufr_tables)//trim(outfile)
   !}
  !} 
  !------------------------------
  ! Get confirmation before start
  !------------------------------
  !{ 
   print *,"Input table file  =",trim(infile)
   print *,"Output table file =",trim(outfile)
   print *,"Continue (S/N)?"
   read(*,*) auxc
   IF (ucases(auxc(1:1))/="S") then 
      write(outfile,'(A1,2I3.3,2I2.2,".txt")')tbl,master,center,version,local
      print *,"Output table file =",trim(outfile)
      print *,"Continue (S/N)?"
      read(*,*) auxc
      IF (ucases(auxc(1:1))/="S") stop
   end if
   open (2,file=outfile,status='unknown')
  !}

  !--------------
  ! Case Table B 
  !-------------
  !{  
   
   if (tbl=="B") then 
    
      if (f==0) call fb1
      if (f==1) call fb2
      

    elseif (tbl=="D") then
      if (f==0) call fd1
      if (f==1) call fd2
     end if 
    close(1)
    close(2)
    print *,trim(color_text("Done",32,.false.))

stop
 contains
!------------------------------------------------------------------------------
!fb1 |Converts BUFR TABLE B from WMO text format to MBUFR-ADT text format| SHSF| 
!------------------------------------------------------------------------------
!                                                                             |
!-----------------------------------------------------------------------------|
!01 ClassNo,
!02 ClassName_en,
!03 FXY,
!04 ElementName_en,
!05 Note_en,
!06 BUFR_Unit,
!07 BUFR_Scale,
!08 BUFR_ReferenceValue,
!09 BUFR_DataWidth_Bits,
!10 CREX_Unit,
!11 CREX_Scale,
!12 CREX_DataWidth_Char,
!13 Status

  subroutine fb1
    integer::l
    i=0
    open (1,file=infile,status='old')
      read(1,'(a)',end=99)line
      print *,trim(line)
10    read(1,'(a)',end=99)line
      line=replace(line,",,",", ,")
      line=replace(line,'""'," ")
      call split(line,",",cols,ncols)
      
      if ((ncols/=13).and.(ncols/=14)) then
	print *,"Error reding table B"
	print *,trim(line)
	do l=1,ncols
		print *,l,"[",trim(cols(l)),"]"
	end do
	stop
      end if 
      
	if (ncols==13) then 
	
	        !1=ClassNo
		!2=ClassName_en,
		!3=FXY,
		!4=ElementName_en,
		!5=Note_en,
		!6=BUFR_Unit,
		!7=BUFR_Scale
		!8=BUFR_ReferenceValue,
		!9=BUFR_DataWidth_Bits,
		!10=CREX_Unit,
		!11=CREX_Scale,
		!12=CREX_DataWidth_Char
		!13=Status
		
		if (trim(cols(6))/="x") then
			cols(5)=trim(cols(5))//" "//trim(cols(6))
		end if
		scalef=val(cols(7))
		refval=val(cols(8))
		nbits=val(cols(9))
		do x1=4,7
			cols(x1)=ucases(cols(x1))
		end do
		cols(6)=replace(cols(6),"CCITT IA5","CCITTIA5")
		C1=VAL(COLS(11))
		C2=VAL(COLS(12))
		! call writetblb(fxy,     desc,    uni,scalef,refval,nbits,          crex1,crex2,crex3)
		call writetblb(cols(3),cols(4),cols(6),scalef,refval,nbits,UCASES(cols(10)),C1,C2)
	else
		!1=ClassNo,
		!2=ClassName_en,
		!3=FXY,
		!4=ElementName_en,
		!5=Note_en,
		!6=noteIDs,
		!7=BUFR_Unit,
		!8=BUFR_Scale,
		!9=BUFR_ReferenceValue,
		!10=BUFR_DataWidth_Bits,
		!11=CREX_Unit,
		!12=CREX_Scale,
		!13=CREX_DataWidth_Char,
		!14=Status
		
		
		if (trim(cols(7))/="x") then
			cols(5)=trim(cols(5))//" "//trim(cols(7))
		end if
		scalef=val(cols(8))
		refval=val(cols(9))
		nbits=val(cols(10))
		do x1=4,8
			cols(x1)=ucases(cols(x1))
		end do
		cols(7)=replace(cols(7),"CCITT IA5","CCITTIA5")
		C1=VAL(COLS(12))
		C2=VAL(COLS(13))
		! call writetblb(fxy,     desc,    uni,scalef,refval,nbits,          crex1,crex2,crex3)
		call writetblb(cols(3),cols(4),cols(7),scalef,refval,nbits,UCASES(cols(11)),C1,C2)
		
	end if
      i=i+1
      goto 10
 99 continue
  end subroutine
  
  
!------------------------------------------------------------------------------
!fb2 |Ler a TABELA BUFR B do OPERA e converte para formato do MBUFR-ADT | SHSF| 
!------------------------------------------------------------------------------
! Ler a TABLEA BUFR B utilizado no programas de radar do OPERA para o formato |
! usado no MBUFRTOOLS                                                         |
!-----------------------------------------------------------------------------|
  subroutine fb2
    i=0
    open (1,file=infile,status='old')
   
10    read(1,'(a)',end=99)line
   !   line=replace(line,",,",",x,")
   !   line=replace(line,'""'," ")
      call split2(line,";",cols,ncols)
      print *,trim(line)
      fxy=trim(cols(1))//trim(cols(2))//trim(cols(3))
      desc=trim(cols(4))
      uni=trim(cols(5))
      scalef=val(cols(6))
      refval=val(cols(7))
      crex1=""
      crex2=0
      crex3=0
      call writetblb(fxy,desc,uni,scalef,refval,nbits,crex1,crex2,crex3)
      
      i=i+1
      goto 10
 99 continue
  end subroutine

!------------------------------------------------------------------------------
!fd1 |Ler  a TABELA BUFR D, no formato da WMO e converte para MBUFR-ADT | SHSF| 
!------------------------------------------------------------------------------
!                                                                             |
!-----------------------------------------------------------------------------|
! 01 Category,
! 02 CategoryOfSequences_en,
! 03 FXY1,
! 04 Title_en,
! 05 SubTitle_en,
! 06 FXY2,
! 07 ElementName_en,
! 08 ElementDescription_en,
! 09 Note_en,
! 10 Status
 subroutine fd1
    !{
      integer::err
      logical::totheend
      integer,parameter::I_Category=1
      integer,parameter::I_CatOfSeq=2
      integer,parameter::I_fxy1=3
      integer,parameter::I_fxy2=6
      integer,parameter::I_Title=4
      integer,parameter::I_SubTitle=5
      integer,parameter::I_ElementName=7
      integer,parameter::I_ElementDescription=8
      err=0
      totheend=.false.
      i=0
      open (1,file=infile,status='old')
      
      read(1,'(a)',end=98)line;i=i+1
30    k=0
      auxc=""
40    read(1,'(a)',end=98)line;i=i+1
        
        line=replace(line,",,",",x,")
        line=replace(line,'""'," ")
        call split(line,",",cols,ncols)
        if (ncols>99) then 
          print *,"Warning: Number of columns excedded (",ncols,") at line=",i
          goto 30
        end if
      
 50   continue     
      !-----------------------------------------------------------------
      ! caso nova sequencia, grava a sequencia de descritores antetiores
      !----------------------------------------------------------------
      
        if (trim(auxc)/=trim(cols(I_fxy1))) then 
  
          if (k>0) then
             
             if ((.not.isval(tbld(1,1))).or.(.not.isval(tbld(1,2)))) then
               write(*,'(1x,a6,1x,i3,1x,a6,1x,a)')tbld(1,1),k,tbld(1,2),trim(title)
               print *,"Error 1: Not numeric value in col 1 or 2 at line=",i 
               stop
             end if
 
             ! Grava a raiz da familia de descritores
              call writetbl_d1(tbld(1,1),k,tbld(1,2),trim(title))
             
              do l=2,k
               if (.not.isval(tbld(1,2))) then
                  write(*,'(1x,a6,1x,a3,1x,a6,1x,a)')tbld(l,1),"-->",tbld(l,2),trim(title)
                  print *,"Error 2"
                  stop
               end if
               ! Grava o descritor filho 
               call writetbl_d2(tbld(l,2))
              end do
             k=0
          end if
          auxc=cols(I_Fxy1)
          title=trim(cols(I_title))
        end if 
        
       !---------------------------------------------------
       ! Faz a separacao da sequencia atual (linha a linha)
       !---------------------------------------------------
        if(.not.totheend) then 
        k=k+1
        tbld(k,1)=cols(I_Fxy1)
        tbld(k,2)=cols(I_Fxy2)
        if ((.not.isval(tbld(k,1))).or.(.not.isval(tbld(k,2)))) then
           print *,"Error 3"
           write(*,*)ncols,">",trim(line)
           write(*,*)"FXY         =",trim(cols(I_Fxy1))
           write(*,*)"Title_en    =",trim(cols(I_title))
           write(*,*)"SubTitle_en =",trim(cols(I_SubTitle))
           write(*,*)"FXY2        =",trim(cols(I_Fxy2))
           write(*,*)"ElementName =",trim(cols(I_ElementName))
           stop
         end if
         
        goto 40
       end if
       
98    continue
  
      if (k>0) then
        totheend=.true.
        auxc="End"
        goto 50
      end if
      
      
 end subroutine

!------------------------------------------------------------------------------
!fd2 |Ler a TABELA BUFR D do OPERA e converte para formato do MBUFR-ADT | SHSF| 
!------------------------------------------------------------------------------
! Ler a TABLEA BUFR D utilizado no programas de radar do OPERA para o formato |
! usado no MBUFRTOOLS                                                         |
!-----------------------------------------------------------------------------|
subroutine fd2
    !{
      i=0
      open (1,file=infile,status='old')
      auxc=""
      k=0
      
40    read(1,'(a)',end=98)line
    !======================================================
    !Final ou inicio de uma familia de descritores 
    !Caso haja uma familia anterior, grava essa famila 
    !e prepara para  ler a proxima familia
    !{
      if (index(line,"#")>0) then     
        
          if (k>0) then
             call writetbl_d1(tbld(1,1),k,tbld(1,2),trim(title))   
             do l=2,k
               call writetbl_d2(tbld(1,2))
             end do
             k=0
          end if   
          
         title=trim(line) ! Obtem o titulo da proxima familia
       
       else  
      !------------------------------------------------  
      !Ler novo elemento de uma familia de descritores
      !----------------------------------------------
      !{
         call split2(line,";",cols,ncols)
         k=k+1
         if (ncols==6) then 
          tbld(k,1)=trim(cols(1))//trim(cols(2))//trim(cols(3))
          tbld(k,2)=trim(cols(4))//trim(cols(5))//trim(cols(6))
         else
           tbld(k,2)=trim(cols(1))//trim(cols(2))//trim(cols(3))
         end if
         print *,trim(tbld(k,1)),k,trim(tbld(k,2)),NCOLS
       end if
      goto 40
98    continue
 end subroutine



!------------------------------------------------------------------------------
!writetblb |Grava no formato da table B do MBUFR-ADT                    | SHSF| 
!------------------------------------------------------------------------------
!-----------------------------------------------------------------------------|

  subroutine writetblb(fxy,desc,uni,scalef,refval,nbits,crex1,crex2,crex3)
   character(len=*),intent (in):: fxy ! Descriptor (fxxyyy)
   character(len=*),intent(in)::desc ! Description 
   character(len=*),intent(inout)::uni  ! Unit 
   integer,          intent(in)::scalef !Scale factor
   integer,          intent(in)::refval ! Reference Value
   integer,          intent(in)::nbits  ! size (number of bits)
   character(len=*), intent(in)::crex1 ! UNIT of then variablel in CREX
   integer,          intent(in)::crex2
   integer,          intent(in)::crex3

   integer::err
   err=0
      uni=replace(uni,"CCITT IA5","CCITTIA5")
      
      if ((scalef>99999).or.(scalef<-9999)) then
        print *,"Error 4: scalefactor=",scalef
        write(*,30)fxy,desc,uni,scalef,refval,nbits,crex1,Crex2,Crex3
        stop
      end if
      if ((refval>999999999).or.(refval<-1999999999)) then
        print *,"Error 5: reference valuer=",refval
        write(*,30)fxy,desc,uni,scalef,refval,nbits,crex1,Crex2,Crex3
        stop 
      end if
     
     if ((nbits>999).or.(nbits<0)) then
        print *,"Error 6: size =",nbits
        write(*,30)fxy,desc,uni,scalef,refval,nbits,crex1,Crex2,Crex3
        stop 
      end if
      write(2,30)fxy,desc,uni,scalef,refval,nbits,crex1,Crex2,Crex3
      
       
 30   FORMAT(1X,A6,1X,A64,1X,A22,1X,I5,1X,I12,1X,I3,1x,A22,I5,I10)
  end subroutine 

!------------------------------------------------------------------------------
!writetbl_d1 |Grava no formato da table D do MBUFR-ADT                   | SHSF| 
!------------------------------------------------------------------------------
!-----------------------------------------------------------------------------|

  subroutine writetbl_d1(d1,k,d2,title)
   character(len=*),intent (in):: d1  ! Descriptor pai  (3xxyyy)
   integer,          intent(in):: k   ! Numero de descritores filhos  
   character(len=*), intent(in):: d2  ! Descritor filho  
   character(len=*), intent(in)::title! Titulo do descritor pai 
   write(2,'(1x,a6,1x,i3,1x,a6,1x,a)')d1,k,d2,trim(title)
   
  end subroutine 
!------------------------------------------------------------------------------
!writetbl_d1 |Grava no formato da table D do MBUFR-ADT                   | SHSF| 
!------------------------------------------------------------------------------
!-----------------------------------------------------------------------------|

  subroutine writetbl_d2(d2)
   character(len=*), intent(in):: d2  ! Descritor filho  
    write(2,'(12x,a6)')d2
   
  end subroutine 
 
  
end program
