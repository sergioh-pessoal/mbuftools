!*******************************************************************************
!*                                 MGRADS_OBS                                  *
!*                                                                             *
!* Modulo de funcoes e subrotinas uteis para gravacao de dados observacionais  *
!*         no formato do grads (ponto de estacao )                             *
!*                                                                             *                                                                             *
!*                                                                             *
!*      Copyright (C) 2005 	Sergio Henrique S. Ferreira                    *
!*                                                                             *
!*      MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                              *
!*                                                                             *
!*-----------------------------------------------------------------------------*
!*                                                                             *
!*   SUB-ROTINAS PUBLICAS                                                      *
!*                                                                             *
!*     SAVEBIN - grava linhas de dados observacionais em formato binario do    * 
!*               grads (arquivo.bin)                                           *
!*     SAVECTL - grava os descritores do "arquivo.bin" produzido por SAVEBIN   *
!*                                                                             *
!*  ESTRUTURA DE DADOS PUBLICAS                                                * 
!*                                                                             *      
!*     stidtype - ESTRUTURA PARA DADOS DE ESTACAO METEOROLOGICA                *
!*                                                                             *
!*                                                                             *   
!*******************************************************************************
!* DEPENDENCIAS                                                                * 
!* MODULOS DATELIB E STRINGFLIB                                                * 
!*******************************************************************************
! HISTORICO 
!  DEZ 2002 -SHSF-   Versao Inicial ( Prototipo )

MODULE MGRADS_OBS


 USE DATELIB
 USE stringflib 
 implicit none

 PRIVATE



type stidtype
   character(len=8):: cod
   real::lat
   real::lon
   integer::nlev
end type  


 PUBLIC stidtype
 PUBLIC SAVECTL_obs
 PUBLIC SAVEBIN_obs
 PUBLIC CLOSEBIN_OBS
 PUBLIC SAVECSV_OBS   !Comma-separated values
 PUBLIC OPENCSV_OBS
 PUBLIC CLOSECSV_OBS
 PUBLIC undefval
 logical               ::xrev ! Indica se o eixo x esta em ordem reversa
 logical               ::yrev ! Indica se o eixo y esta em ordem reversa   
 real,parameter        ::undefval=-8388607e31
 integer,parameter     ::ntable=17
 real,dimension(ntable)::ltable
 real                  ::null=-9e9
 integer               ::irec
 integer,allocatable   ::kvar(:)  ! kvar(1:nvar) Indica tipo de nivel vertical de cada variavel 0-superficie, >0 altitude
 real,allocatable      ::klev(:)  ! klev(1:kmax) Nivel vertical
 integer               ::kmax
 integer               ::nvars  
 CONTAINS
!-----------------------------------------------------------------------------
!init
!-----------------------------------------------------------------------------
 
!==============================================================================!
! SaveCTL |                                                               |SHSF!
!==============================================================================!

 SUBROUTINE SAVECTL_obs(un,filename,cdate,tstep,codes,desc,kvar_in,nvars_in,klev_in,kmax_in,null)
  !{Variaveis de interface
    integer,intent(in)                      ::un
    character(len=*),intent(in)             ::filename
    real*8,intent(in)                       ::cdate
    real,                         intent(in)::tstep    ! Passo de tempo em horas
    character(len=*),dimension(:),intent(in)::codes    ! Codigo das variaveis
    character(len=*),dimension(:),intent(in)::desc     ! Descricao das variaveis
    integer,dimension(:),         intent(in)::kvar_in  ! Indica o tipo de nivel de cada variavel (0 = superficie, >0 Numero de niveis verticais 
    integer,intent(in)                      ::nvars_in ! Numero das Variaveis
    real,dimension(:),            intent(in)::klev_in  ! Niveis verticais
    integer,                      intent(in)::kmax_in  ! Numero máximo de niveis verticais 
    real,intent(in)                         ::null
  !}
 
 !{ Variaveis locais 
    Character(len=255)::ctlname,filename2
    integer ::i,uni
    character(len=255)::gradsdate
    character(len=50),dimension(50)::substring
    integer ::nelements
!}
!{ Inicializando variaveis globais
	allocate(kvar(1:nvars),klev(1:kmax))
	kvar=kvar_in
	klev=klev_in
	kmax=kmax_in
	nvars=nvars_in
	irec=0
	print *,":MGRADS_OBS: klev=", klev
!}
!{ Iniciar variaveis Locais
    uni=un
    i=0
    ctlname=trim(filename)//'.ctl'
!}

!{ Cortar os diretorios do nome do arquivo
    call split(filename,'/',substring,nelements)
    filename2=substring(nelements)
!} 

!{ Gravar arquivo descritor (CTL)
    open(uni,file=ctlname,status='unknown')
       
    write(uni,1001)trim(filename2)    
   1001 format('DSET ^',a,'.bin')
          
    write(uni,1002)    
   1002 format('DTYPE station')
   
        write(uni,1003)trim(filename2)    
   1003 format('STNMAP ^',a,'.map')
    
        write(uni,*) "UNDEF ",null
   
        write(uni,1005)
   1005 format('TITLE OBSERVED DATA')

        gradsdate=grdate(cdate)
	if (tstep>=1.0) then 
		write(uni,1006) trim(gradsdate),int(tstep)
	else
		write(uni,1016) trim(gradsdate),int(tstep*60)
	end if
   1006 format('TDEF 1 linear ',a,' ',i3,'hr')
   1016 format('TDEF 1 linear ',a,' ',i2,'mn')
        write(uni,1007)nvars
   1007 format('VARS ',i3)

   do i=1,nvars
 
     write(uni,'(a4,1x,i3, a4,1x,a30)')codes(i),kvar(i)," 99 ",desc(i)
    end do

    write(uni,'(a)')"ENDVARS"
!}
   close(uni)
   
   
 end subroutine  

!==============================================================================!
! SaveBIN | Grava dados na forma de ponto de estacao                      |SHSF!
!==============================================================================!
 SUBROUTINE SAVEBIN_obs(un,filename,obs,rlat,rlon,id,nvars)
 !{ Variaveis de interface
   integer,            intent(in)::un
   character(len=*) ,  intent(in)::filename
   real,dimension(:,:),intent(in)::obs
   real,               intent(in)::rlat,rlon
   character(len=8),   intent(in)::id      ! Statioin identifier
   integer,            intent(in)::nvars
 !}
   
!{ Variaveis Locais
   Integer:: NFLAG,i,uni,j,ub,NLEV,k
   REAL::TIM
   character(len=255)::outfile
   character(len=8)::C
   character(len=4)::C1,C2
!}


!{ Iniciando Variaveis Locais 
   NFLAG = 1    ! 0 - Indica que Não Ha dados de superficie 1 - Indica que há dados de superficie
   TIM=0.0      ! 0.0 Indica dado centrado no tempo
   uni=un     
   outfile=trim(filename)//".bin"
!}      
  
!{ Grava arquivo Binario   

       if (irec==0) then 
		open(uni,file=outfile,STATUS='unknown',FORM='UNFORMATTED',access='DIRECT',recl=4)
		print *,":MGRADS_OBS: Opennig file for writing data: ",trim(outfile) 
        end if  
        NFLAG = 1    ! 0 - Indica que Não Ha dados de superficie 1 - Indica que há dados de superficie
	TIM=0.0      ! 0.0 -Deslocamento no tempo em relação ao tempo da grade em horas e décimos 
	NLEV=1       ! Numero de grupos de dados apos o header (Se so tiver superficie =1)
	IF (KMAX>1) NLEV=1+KMAX 
          C=id 
          C1=C(1:4)
          C2=C(5:8)
	  
          irec=irec+1;WRITE (uni,rec=irec)C1
          irec=irec+1;WRITE (uni,rec=irec)C2
          irec=irec+1;WRITE (uni,rec=irec)rLat
          irec=irec+1;WRITE (uni,rec=irec)rLON
          irec=irec+1;WRITE (uni,rec=irec)TIM
	  irec=irec+1;WRITE (uni,rec=irec)NLEV
          irec=irec+1;WRITE (uni,rec=irec)NFLAG
	  !{ Grava dados de superficie 
	
	  DO J=1,NVARS
	    if (kvar(j)==0) then 
		irec=irec+1
		WRITE (uni,rec=irec) OBS(J,1)
	    end if
          END DO
	  !}
	  
	  !{ Gravando dados dependentes do nivel
	if (kmax>1) then 
	do k =1,kmax
	        irec=irec+1
		WRITE (uni,rec=irec) klev(k)
		DO J=1,NVARS
			if (kvar(j)>0) then 
				irec=irec+1
				WRITE (uni,rec=irec) OBS(J,k)
			end if
		  END DO
	  end do	
	 end if 
	  !}
	!  print *,"irec=",irec
	!  print *,rlat,rlon,obs(1,1),obs(2,1)

!}	
END SUBROUTINE 

SUBROUTINE OPENCSV_obs(un,filename)
	integer,            intent(in)::un
   	character(len=*) ,  intent(in)::filename
	character(len=1023)::outfile   
	
	outfile=trim(filename)//".grid.csv"
	open(un,file=outfile,STATUS='unknown')
	 
end subroutine

subroutine CLOSECSV_obs(un)
	integer,            intent(in)::un
   		
	close(un)

end subroutine 
!==============================================================================!
! SaveCSV | Grava dados na forma de ponto de estacao                      |SHSF!
!==============================================================================!
 SUBROUTINE SAVECSV_obs(un,filename,obs,rlat,rlon,id,nvars)
 !{ Variaveis de interface
   integer,            intent(in)::un
   character(len=*) ,  intent(in)::filename
   real,dimension(:,:),intent(in)::obs
   real,               intent(in)::rlat,rlon
   character(len=8),   intent(in)::id      ! Statioin identifier
   integer,            intent(in)::nvars
 !}
   
!{ Variaveis Locais
   Integer:: NFLAG,i,uni,j,ub,NLEV,k
   REAL::TIM
   character(len=255)::outfile
   character(len=8)::C
   character(len=1023)::line
!}


!{ Iniciando Variaveis Locais 
   NFLAG = 1    ! 0 - Indica que Não Ha dados de superficie 1 - Indica que há dados de superficie
   TIM=0.0      ! 0.0 Indica dado centrado no tempo
   uni=un     

!}      
  
!{ Grava arquivo    
        NFLAG = 1    ! 0 - Indica que Não Ha dados de superficie 1 - Indica que há dados de superficie
	TIM=0.0      ! 0.0 Indica dado centrado no tempo
	NLEV=1       ! Numero de grupos de dados apos o header (Se so tiver superficie =1)
	IF (KMAX>1) NLEV=1+KMAX 
          C=id 
	  C=""
          WRITE(line,10)id,RLAT,RLON,TIM,NLEV,NFLAG
   10     format(a8,2(',',f10.5),',',f3.1,',',I4,','I2)

	  !{ Grava dados de superficie 
	  DO J=1,NVARS
	    if (kvar(j)==0) then 
		line=trim(line)//','//strs(OBS(j,1))
	    end if
          END DO
           
	  write(uni,'(a)')trim (line)
	  !}
	  !{ Gravando dados dependentes do nivel
	if (kmax>1) then 
	do k =1,kmax
		
		line=char(8)//trim(strs(klev(k)))
		DO J=1,NVARS
			if (kvar(j)>0) then 
				line=trim(line)//','//strs(obs(j,k))
				
			end if
		  END DO
		WRITE (uni,'(a)') trim(line)
	  end do	
	 end if 

	  !}
	!  print *,"irec=",irec
	!  print *,rlat,rlon,obs(1,1),obs(2,1)

!}	
END SUBROUTINE

 SUBROUTINE CLOSEBIN_OBS(uni)
	integer,intent(in)::uni
	!{ Variaveis Locais
	Integer:: NFLAG,NLEV,i,j,ub
	REAL::TIM,rlat,rlon
	character(len=4)::C1,C2
	!}
	rlat=0.0
	rlon=0.0
	TIM=0.0
	NLEV=0
	NFLAG=1
	if (irec>0) then
		print *,":MGRADS_OBS:Closing file: Number of recorded data =",irec
		irec=irec+1;WRITE (uni,rec=irec) C1
		irec=irec+1;WRITE (uni,rec=irec) C2
		irec=irec+1;WRITE (uni,rec=irec) rLat
		irec=irec+1;WRITE (uni,rec=irec) rLon
		irec=irec+1;WRITE (uni,rec=irec) TIM
		irec=irec+1;WRITE (uni,rec=irec) NLEV
		irec=irec+1;WRITE (uni,rec=irec) NFLAG
	end if
	close(uni)
	Print *,":MGRADS_OBS:File closed: size=",irec,"bytes"
        deallocate (klev,kvar)
end subroutine

END MODULE
