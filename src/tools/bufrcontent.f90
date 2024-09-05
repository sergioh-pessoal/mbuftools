program bufrcontent

!******************************************************************************
!*                                  BUFRLIST                                  * 
!*                                                                            *
!*   Programa para verificar conteudo do arquivos BUFR                        *.
!*        (Program to verify the contents of BUFR files )                     *
!*                                                                            *
!*                                                                            *
!*             Copyright (C) 2013 Sergio Henrique S. Ferreira                 *
!*                                                                            *
!*               MCT-INPE-CPTEC-Sao Jose dos Campos, Brasil                   *
!*                                                                            *
!*                                                                            *
!*----------------------------------------------------------------------------* 
!* This program scans only BUFR sections 1 and 3 to get informatin such as 
!* data category, number of subset (observarions), etc. 
!* The section 4 is not read to increase reading speed.
!******************************************************************************
! HYSTORY:
!   2013-06-26 - SHSF - INITIAL VERSION BASED ON BUFRLIST (old program)
!   2022-06-23 - SHSF - New option to verify if a specific table D descripor 
!                       is present in the BUFR messages - MBUFR v 4.6.7

USE MBUFR
USE STRINGFLIB
USE mcodesflags, only: tabA,tabCC1,tabCC13, init_mcodesflags  
implicit none

!{ Declaracao das variaveis nas interfaces das subrotinas 
  integer,parameter                   ::nss=10000000      !Numero maximo de mensagens  por arquivo
  type(sec1type),dimension(nss)       ::sec1	          !Secao 1 de cada mensagem (nss) 
  integer*8,dimension(nss)            ::pos               !Posicao de cada mensagem (nss)
  integer,dimension(nss)              ::MBYTES            !Tamanho de cada mensagem (nss) 
  integer,dimension(nss)              ::nsubsets          !Numero de subsets de cada mensagem (nss)
  character(len=40),dimension(nss)    ::header            !Encabecamento de telecomunicacoes da messagen(nss)
!}
!{Declaracao das variaveis para entrada de parametros 
  character(len=256),dimension(300)   ::arg      ! argumentos!
  integer                             ::narg     ! numero de argumentos efetivamente passados!
  character(len=1),dimension(300)     ::namearg  ! Nome dos argumentos!
  integer                             ::iargc    ! 
  integer*2                           ::argc
!} 

!{Declaracao das variaveis do programa principal 
  integer,dimension(0:255,0:255,0:255)::TOTAL_CENTROXTIPO !Totalizados de subsets por centro, tipo e subtipo (i,j,k)
  integer,dimension(0:255,0:255,0:255)::TMSG_CENTROXTIPO  !Totalizados de mensagens por centro, tipo e subtipo (i,j,k) 
  integer,dimension(0:255)            ::total_centro	  !Totalizador de bytes por centro (i)

  character(len=255)                  ::outfile           !Nome do arquivo de saida (opcional)
  character(len=255)                  ::infile            !Variavel auxiliar para arquivos de entrada
  character(len=255),dimension(301)   ::flist             !Lista com nome dos arquivos 
  integer                             ::err	          !Codigo de Erro 
  integer                             ::NBYTESF           ! Totalizacao dos bytes por aquivo 
  integer                             ::NMESSAGE,TOTMESSAGE ! Numero total de mensagens 
  integer                             ::NBYTES            ! Totalizacao de bytes de todas as mensagens 
  integer                             ::i,J,k             ! Indices para: (centro,tipo,subtipo)
  integer                             ::l,f,m             ! indices para: ( ? , arquivo, message)
  integer                             ::totcentros        !?
  real                                ::aux,total         !? 
  character(len=70)                   ::auxc,auxa         !? 
  integer                             ::auxi
  integer                             ::nf                ! Numero de arquivos na lista 
  character(len=255)                  ::table_dir         ! Diretorio das tabelas BUFR
  character(len=255)                  ::text              ! Variaveil auxiliar para texto
  integer                             ::un                ! Unidade I/O
  integer                             ::errors 
  logical                             ::oph               ! the header of  all messages will be preseted 
  integer                             :: descriptor
!}
!**************************
! Takes the argument Entry
!**************************
!{ 
      outfile=""
      nf=0
      nsubsets(:)=0
      oph=.false.
      descriptor=0
      call getarg2(namearg,arg,narg)	  !
	if (Narg>0) then
	    
		do i=1,narg
			if(namearg(i)=="o") then 
				outfile=arg(i)
			elseif(namearg(i)=="d") then 
				descriptor=val(arg(i))	
			elseif(namearg(i)=="?") then 
				nf=nf+1
				if (nf > 300) then 
					print *,"Warning! The maximum number of provide files is 300. Other files will be ignored"
					nf=300
					exit
				else 
					flist(nf)=arg(i)
				end if 
                        elseif(namearg(i)=='h') then
                          oph=.true.
			end if
		  end do
        end if     
        print *,"+--------------------------------------------------------+"
        print *,"| CPTEC/INPE BUFRCONTENT : Content table of BUFR files   |"
        print *,"| Include MBUFR-ADT module ",MBUFR_VERSION,"     |"
        print *,"+--------------------------------------------------------+"
        if (nf>0) then 
          print *,"Reading ",nf," files..."
	else 
     
	  print *,"| USE: bufrcontent {-h}  {-o outfile} input_filelist     |"
	  print *,"|  Other options:                                        |"
	  print *,"|   {-d desc} Verify if the table D descripot is present |" 
	  print *,"+--------------------------------------------------------+"	   
	  print *,""
	  stop   	        
  	endif
!}



!***************************************
!Initialization of variables and tables
!***************************************
!{
	total_centroxtipo(:,:,:)=0
	tmsg_centroxtipo(:,:,:)=0
	total_centro(:)=0
     
	call getenv("MBUFR_TABLES",table_dir)
	if ((table_dir(i:i)/="\").and.(table_dir(i:i)/="/")) then 
		if (index(table_dir,"\")>0) then 
			table_dir=trim(table_dir)//"\"
		else
			table_dir=trim(table_dir)//"/"
		end if
	end if
        
	call init_mcodesflags(table_dir)

 	!do i = 0,255
  
 !		l = 50 - Len_trim(TABA(i))
!		  If (l > 0)  then 
!		    do j=1,l
!			 TABA(i) = trim(TABA(i)) // "."
!			end do
!		  end if
!	end do 


!********************************************************************
! Main 
!********************************************************************
!{
if (len_trim(outfile)>0) then 
 un=2
 open(un,file=outfile,status="unknown")
else
 un=6
end if

 call init_mbufr(0,.false.)
 totmessage=0
  do  F = 1, nf
    NBYTESF = 0
    if (len_trim(flist(f))>0) then 
      Call OPEN_MBUFR(1, flist(F))
      errors=0
      CALL  FIND_MESSAGES_MBUFR(1,descriptor,nmessage,pos,sec1,nsubsets,mbytes,header,errors)
      totmessage=totmessage+nmessage
      IF (OPH) then 
      write(un,*)"+----------------------------------------------------------------------------------------------+"
      write(un,*)"|",trim(flist(F))," nmessage=",nmessage, "  nerrors=",errors   
      write(un,*)"+----------------------------------------------------------------------------------------------+"
      write(un,76)"Men","Position ","Header","Center/Sub.","Cat./Sub.","N.Subsets","N.Bytes"
      write(un,*)"+----------------------------------------------------------------------------------------------+"
      
      endif
      
      do m=1,nmessage
	if (sec1(m)%center>255) sec1(m)%center=255
	if (oph) write(un,77)m,pos(m),header(m),sec1(m)%center,sec1(m)%subcenter,sec1(m)%btype,sec1(m)%bsubtype,nsubsets(m),mbytes(m)
      76 format (1x,"|",a4,  "|"a10,    "|",a40,"|",a7,"|",a9,       "|",a7,"|",a10,"|")
      77 format (1x,"|",i4.4,"|",i10.10,"|",a40,"|",i5,"/",i5,"|",i4,"/",i4,"|",i7,"|",i10,"|")
 
          if ((sec1(m)%btype>255)) then
            print *, "Error: Bufr Category unknown !"
            print *, "Center=",sec1(m)%center
            print *, "Bufr Category=",sec1(m)%btype
          else 
            i=sec1(m)%center
            if (i<0) then 
               print *,i
               stop
            end if
            j=sec1(m)%btype
            k=sec1(m)%bsubtype
            TOTAL_CENTROXTIPO(i,j,k)=TOTAL_CENTROXTIPO(i,j,k)+nsubsets(m)
	    if (nsubsets(m)>0) TMSG_CENTROXTIPO(i,j,k)=TMSG_CENTROXTIPO(i,j,k)+1	
            total_centro(i)=total_centro(i)+nsubsets(m)
            NBYTES = NBYTES + MBYTES(m)
            NBYTESF = NBYTESF + MBYTES(m)
          end if
      end do

      Close (1)
     
    end if

end do 
write(un,*) "+----------------------------------------------------------------------------------------------+"

!} Fim da leitura e contabilidade

write(un,*) "+----------------------------------------------------------------------------------------------+"
   total=0.0
   DO i = 0,255
 
	If (total_centro(i) > 0) Then
		Write (un, 98) i,TABCC1(i)  
		totcentros=totcentros+1
		do  J = 0,255
			do k=0,255
					
				If (TOTAL_CENTROXTIPO(i, J,k) > 0) Then
					!tabcc13(j,k)=""
					if (len_trim(tabcc13(j,k))==0) then
					   auxc=taba(j)
					   !auxc=auxc(1:42)//" [Subcategory:"//trim(strs(k))//"]"
					   write(auxc,'(a47," [Subcategory:",i3,"]")')auxc(1:47),k
					else
					   auxa=taba(j)
					   auxi=index(auxa,"(")
					   if (auxi>10) auxa=auxa(1:auxi-1)
					   auxi=64-len_trim(tabcc13(j,k))
					   
					   if (auxi>10) then 
					        if(auxi>len_trim(auxa)) auxi=len_trim(auxa)
						auxc=auxa(1:auxi)//" ["//trim(tabcc13(j,k))//"]"
					   else
						auxc=trim(tabcc13(j,k))
					   end if   
					end if    
				         aux=float(TOTAL_CENTROXTIPO(i, J,k)) 
					 write(un,100)j,auxc,aux,float(TMSG_CENTROXTIPO(i, J,k))
					total=total+aux
				End If
			
			end do
		end do
			
		aux= float(total_centro(i)) 
		write(un,102)'Total =',aux,' subsets'
 
		

	End If
    end do
    write(un,*) "+----------------------------------------------------------------------------------------------+"
    write(un,102)'Total subsets =',total,' subsets'
    write(un,102)'Total messages =',real(TOTMESSAGE),' messages'
    write(un,102)'Limit =',real(nss),' messages'
    write(un,*) "+----------------------------------------------------------------------------------------------+"
    close(un) 
  98	format(' |',i3," - ",A50)
 100	format(' |',3x,i3,"-",A67,"=",F10.0," subsets ",F10.0," messages")
 102	format(' |',4x,a71             ,f10.0,A9)

End 

