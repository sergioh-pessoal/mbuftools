program plotbufrtype
!*******************************************************************************
!*                                 PROTBUFRTYPE                                *
!*                                                                             *
!* Obtem a posicao geografica de dados meteorologicos em um arquivo BUFR       *
!* e indentifica os dados conforme a tabela BUFR A da OMM                      *
!* Os resultados sao gravados em formato binario do software GRADS             *
!* Utilize o Grads para visualizar os resultados.                              *
!*                                                                             *
!*      Copyright (C) 2005    Sergio Henrique S. Ferreira                      *
!*                                                                             *
!*                                                                             *
!*      MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                              *
!*                                                                             *
!*-----------------------------------------------------------------------------*
!* DEPENDENCIAS: MBUFR-ADT,MGRADS, STRINGFLIB, DATELIB, GetArgs.               *  
!*******************************************************************************
!* HISTORICO
!* 20100618 : Substituicao do modulo MGRADS por MGRADS_OBS


USE MBUFR
USE MGRADS_OBS
USE DATELIB
USE STRINGFLIB
!USE MSFLIB  ! Para compilacao em Windows ( Microsoft Power Station )

implicit none

!{DECLARACAO DE VARIAVEIS

!{ Declaracao das variaveis utilizadas em read_mbufr 
  
    integer :: nss
    type(sec1type)::sec1
    type(sec3type)::sec3                         
    type(sec4type)::sec4
    integer :: NBYTES,BUFR_ED         
    integer :: err
  !Real,parameter	:: Null=-340282300      !valor nulo 
 
!} 

 !{ Declaracao das variaveis utilizadas no mgrads
    real*8                          ::cdate     !Data juliana
    character(len=5),dimension(1)   ::codes     !Codigo das variaveis
    character(len=60),dimension(1)  ::desc      !Descricao das variaveis
    integer                         ::nvarsgrd  !Numero das Variaveis
    type(stidtype),dimension(100000)::STID      !Identificacao da estacao   
    real,dimension(100000,1)        ::obs       !Matriz com as observacoes
    integer                         ::nobs      !Numero de observacoes
  !}


  !{ variaveis auxiliares  do progrma principal 
    integer           ::l,i,f,J
    integer*2         ::argc
    integer           ::iargc
    real              :: aux
    character(len=255)::infile,txtfile,indate,outfile
    real*8 :: date1,date2,dj,mdate             ! Datas inicial, final  (calend. juliano) e janela de tempo (dias)
    integer :: iyear,imonth,iday,ihour         ! Ano,mes,dia (Calend. gregoriano)
    character(len=255),dimension(1000) ::flist ! Lista com nome dos arquivos 
    integer                            ::nf    ! Numero de arquivos na lista 
    integer                            ::nm     ! Numero de mensagens bufr
    integer                            ::icod
    character(len=50)                  ::ncod
    character(len=50),dimension(99999) ::tabncod
    integer                            ::ilat,ilon
    integer,dimension(20000)           ::t
    real,dimension(20000)              ::rlat,rlon 
  !}
!} 

!{ INICIO DO PROGRAMA  
 !{ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida
    argc =  iargc()	
    nf=0
  
    if ((argc>3).and.(argc<1000)) then
   
      print *, "-----------------------------------------------------------------------"
      print *, "|plotbufrtype                                                         |"
      print *, "-----------------------------------------------------------------------"
      !{ Pega data inicial e final na forma yyyymmdd, convertendo-as para
      ! formato do calendario juliano (date1 e date2 respectivamente) 
      i=1;call GetArg(i,indate)
      
      print *,"Windows time=",trim(indate)
      mdate=fjulian(indate) 
      i=2;call GetArg(i,indate)
      dj=val(indate)
      date1=mdate-(dj/24.0)
      date2=mdate+(dj/24.0)
      
      write(*,121)grdate(date1),grdate(date2)
      121 format (1x,'Windows from ',a19,' to ',a19)
      
      i=3;call GetArg(i,outfile)
      print *,"Outfile=",trim(outfile)
      
      do i=4,argc 
        call GetArg(i,infile)  
        nf=nf+1
        flist(nf)=infile
      end do
      print *,"Reading ",nf,"files..."
    else 
      print *, "------------------------------------------------------------------------"
      PRINT *, "| plotbufrtype   mdate hh outfile file(1) file(2) ... file(n)          |"
      print *, "|                                                                      |"
      print *, "|                mdate = Data and Time(UTC)--> [yyyymmddhh]            |"
      print *, "|                hh  =   Windows time(hours)-> [hh]                    |"
      print *, "|                outfile= Outfile name for Grads software              |"
      print *, "|                file(n) = BUFR input files                            |"
      print *, "|                                                                      |"
      print *, "------------------------------------------------------------------------"
      stop  
    endif
  !}



 
! Processa a leitura dos dados para cada um dos nf arquivos fornecidos
! {

nobs=0 
   
    do  F = 1, nf
     !{Abre arquivos BUFR
        Call OPEN_MBUFR(1, flist(F),46,14,0)  
        print *," Arquivo=",trim(flist(F)) 
      !}
      !{ Zera variaveis a cada ciclo
        nm=0
        NBYTES = 0
      !}
      !{ Processa a leitura de cada uma das mensagens do arquivo aberto
      10  CONTINUE
        
        nm=nm+1
        call READ_MBUFR(1, sec1,sec3,sec4, bUFR_ED, NBYTES,err)
        
        If ((NBYTES > 0).and.(IOERR(1)==0)) Then
          if (err/=0) then  
            write(*,*)"------------------------------"
            write(*,*)"Mensagem N.=",nm
            write(*,*)"Centro=" ,sec1%center
            write(*,*)"nsubsets",sec3%NSUBSETS
            write(*,*)"ndesc=",sec4%nvars
            write(*,*)"Tipo=",sec1%bType
            write(*,*)"erro=",err
            write(*,*)"------------------------------"  
          else   
            ilat=0
            ilon=0
            iyear=0
            imonth=0
            iday=0
           
            do i=1,sec3%nsubsets
              do j=1,sec4%nvars

			! Localiza e separa a data conforme os descritores da tabela B:
			! Ano = 004001
			! mes = 004002
	    		! dia = 004003
	    
	  		if ((sec4%d(j,i)==4001)) iyear=sec4%r(j,i)
	  		if ((sec4%d(j,i)==4002)) imonth=sec4%r(j,i)
	  		if ((sec4%d(j,i)==4003)) iday=sec4%r(j,i)	
	  		if ((sec4%d(j,i)==4004)) ihour=sec4%r(j,i)  
	  
	   
	 
	  		!  Processa os dados de lat e lon  somente se estiverem dentro da janela de tempo
	  		!  especificada.
	  		!
	  		!  Para isto, primeiro verifica-se todos os valores de ano,mes e dia
	  		!  foram localizados, depois converte para calendatio juliano (cdate) 
	  		!  e em seguida testa se cdate esta dentro dos  limites da janela,
	  		!  i.e.  (cdate>=date1).and.(cdate<=date2)
	  		!  
	  		!{ 

	  		if ((iyear>0).and.(imonth>0).and.(iday>0)) then  
	     			cdate=fjulian(iyear,imonth,iday,ihour,0,0)
	   
	    			if ((cdate>=date1).and.(cdate<=date2)) then   
	 
     	      				! Localiza e separa os valores de latitude
	      				! (descritores da tabela B 005001 e 005002
	      				!{
	       				if((sec4%d(j,i)==5001).or.(sec4%d(j,i)==5002)) then 
	    
	         				ilat=ilat+1
	         				rlat(ilat)=sec4%r(j,i)
	   
	       				end if
              				!}
           
	      				! Localiza e separa os valores de longitude
	      				! (descritores da tabela B 006001 e 006002
	      				!{
	      				if ((sec4%d(j,i)==6001).or.(sec4%d(j,i)==6002)) then 
	   
	       					ilon=ilon+1
	       					rlon(ilon)=sec4%r(j,i)
	       					t(ilon)=sec1%btype
	   
	      				end if
	     				!}
	   		 	end if
	   
	   		end if
	  		!} 
	   
			end do
		end do
	
	

        	!} Fim da leitura de um arquivo(f) da lista de arquivos flist(nf)
	 
	 	! Para os dados do arquivo (f), verifica se nao houve discrepancia 
	 	! entre o numero de dados de latitudes e longitudes e  prepara-os 
	 	!  para ser utilizado na subrotina de gravacao do formato do grads
	 	!{ 
	 
	  
		if((ilon==ilat).and.(ilon/=0)) then 
	
	  		do i=1,ilon
	   
	   		if (nobs<100000) then 
	    			nobs=nobs+1
	    			obs(nobs,1)=t(i)
	    			write(STID(nobs)%cod,'(a8)')nobs
	    			STID(nobs)%lat=rlat(i)
	    			STID(nobs)%lon=rlon(i)
           		else 
	    
	    			goto 333
	    
	   		end if
	   
	  		end do
	
		!elseif ((ilon==ilat).and.(ilon==0)) then 
		!	print *,"Aviso ! Nao foram  encomtrados dados para a janela de tempo"
		!	print *,"cdate=",cdate
	  	!	print *,"date1=",date1
	  	!	print *,"date2=",date2
		!else  
		!	
	  	!	print *,"Erro nas variaveis latitude e/ou longitude"
	  	!	print *,"Numero de latitudes=",ilat
	  	!	print *,"Numero de longitudes=",ilon		
	
		end if
	
	end if
	 
	if (nobs>nm) then 	 
write(*,'( "Numero de Mensagem lidas = ",i4.4," Numero de observacoes lidas = ",i6.6,"  Tipo = ",i3)')nm,nobs,sec1%btype

	elseif (mod(nobs,100)==0) then
 write(*,'( "Numero de Mensagem lidas = ",i4.4," Numero de observacoes lidas = ",i6.6,"  Tipo = ",i3)')nm,nobs,sec1%btype
	end if
	deallocate(sec3%d,sec4%d,sec4%r,sec4%c)
	GoTo 10
    end if
	
	
    !}

    Close (1)
    print *,trim(flist(f))
   !} Proximo arquivo
    end do
 !} Fim da leitura de todos os arquivos da lista 
 
!{ Salvar dados no formato do grads 

  
 !{ Iniciar variaveis do grads
 
 333 continue 
 

   codes(1)="T"
   desc(1)="Tipo de observacao (TABELA-A)"
   nvarsgrd=1  
 !}
 
 !{ Grava os arquivos de dados (bin) e o descritores (ctl) 
 
    call SAVECTL(1,outfile,cdate,codes,desc,nvarsgrd)
    call SAVEBIN(1,outfile,obs,nobs,STID,nvarsgrd)
    
  !}
  
End 
