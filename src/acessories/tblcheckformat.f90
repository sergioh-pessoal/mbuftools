
program tblcheckformat
!-----------------------------------------------------------------------------
!                             TBLCHECKFORMAT
! Programa para verificar erros de formatacao nas tabelas BUFR 
!------------------------------------------------------------------------------
!Historico
! 19-06-2018 SHSF : Incluido revisao da tabela bufr B

        integer            ::f1,x1,y1,nl1,F2,X2,Y2
	character(len=1)   ::t1
	character(len=1024)::filename,linha
	character(len=64)  ::C4,C5
	integer            ::scale,refv
	
	print *,"Enter Tbl filename "
	read(*,*) filename

	OPEN (1, FILE =filename, ACCESS = 'SEQUENTIAL', STATUS = 'OLD')
  	
       
888   READ(1,'(a)',END=9898)linha
          print *,trim(linha)
	  if (filename(1:1)=="D") then
	    read(linha,'(1x,i1,i2,i3,1x,i3,a1,i1,i2,i3)')f1,x1,y1,nl1,t1,F2,X2,Y2
	    if (t1/="") then
	   	  read(linha,'(1x,i1,i2,i3,1x,i2,a1,i1,i2,i3)')f1,x1,y1,nl1,t1,F2,X2,Y2 
            end if
	  elseif (filename(1:1)=="B") then   
             print *,linha(2:2),"|",linha(3:4),"|",linha(5:7),"|",LINHA(9:72),"|",LINHA(74:97),"|"
	     print *,LINHA(99:101),"|",LINHA(103:114),"|",LINHA(116:118)
	     READ(linha ,100)F1,X1,Y1,C4,C5,SCALE,REFV,NBITS
          else
	   print *,"Error"
	   stop
	  end if   
       goto 888
9898   continue
       close(1)
       print *,"nomaly concluded"
        
100 FORMAT(1X,I1,I2,I3,1X,A64,1X,A24,1X,I3,1X,I12,1X,I3)     
end program