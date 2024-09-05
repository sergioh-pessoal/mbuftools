!----------------------------------------------------------------------
! tblcopy: Copy a BUFR table file (B or D) to another file and fixes formats, if necessary 
!---------------------------------------------------------------------- 

program tblcopy

 use stringflib
 implicit none
 character(len=255)::infile
 character(len=255)::outfile
 character(len=1),dimension(10)   ::argname     !-Argument name.
 character(len=255),dimension(10) ::arg         !-An argument. 
 integer                          ::narg        !-Number of Arguments
 integer                          ::X1,X2,i
 logical                          ::screen
!-----------------  
! **  Welcome **
!-----------------
!{
  x1=0
  x2=0
  screen=.false.
  call getarg2(argname,arg,narg)
   do i=1,narg
      if (argname(i)=="i") then 
          infile=arg(i)
          x1=1
      elseif (argname(i)=="o") then
          outfile=arg(i)
          x2=1
      elseif (argname(i)=="s") then
        screen=.true.
      end if
    end do
    x1=x1*x2
    if (x1==0) then
      print *,"---------------------------------------------------------"
      print *," CPTEC/INPE tblcopy: Copies a BUFR table file (B or D) to" 
      print *," another file and fixes the format, if necessary         "
      print *,"---------------------------------------------------------"
      print *,"use:"
      print *," tblcopy -i infile -o outfile {-s}"
      print *,""
      print *,"   infile = it is a (B/D) table in the ECMWF kind format "
      print *,"   outfile= it is the copied file"
      print *,"   -s     = output on the screen"
      print *,"--------------------------------------------------------"
      stop
     else
      print *,"---------------------------------------------------------"
      print *," CPTEC/INPE tblcopy: Copies a BUFR table file (B or D) to" 
      print *," another file and fixes the format, if necessary         "
      print *,"---------------------------------------------------------"
    endif
!}
 
 if (outfile(1:1)=="D") then
   call copy_tabd(infile,outfile)
 elseif (outfile(1:1)=="B") then 
   call copy_tabb(infile,outfile)
 else
   print *,"ERROR"
   stop
 end if

stop

contains


 SUBROUTINE COPY_TABB(infile,outfile)
 
 !{ Variaveis de interface
        character(len=255)::infile,outfile
 !}

 !{ Variaveis locais
	INTEGER::uni ,i,ii,jj
	INTEGER*4::F,X,Y,SCALE,REFV,NBITS
	CHARACTER(len=255)::C4,C5
	CHARACTER(len=255)::A
	CHARACTER(len=255)::filename
 !}

 
  !{ PRECESSING TABLE B
        print *, "Table B"
	print *,"Infile=",trim(infile)
        print *,"Outfile=",trim(outfile)
	OPEN (1, FILE =infile, ACCESS = 'SEQUENTIAL', STATUS = 'OLD')
        OPEN (2, FILE =outfile, status="unknown")
        i=0
  10  READ(1,"(A)",END=999)A
      i=i+1
      !{ Verifica caracteres incorretos na tabela B
      jj=0
      DO II=1,LEN_TRIM(A(1:118))
        IF (ICHAR(A(II:II))==9) then
          A(II:II)="?"
		jj=1
        END IF
        if (ICHAR(A(II:II))<32) A(II:II)=" "
      END DO
       if (jj>0) then
          print *,"Erro reading BUFR TABLE B"
		print *, "Tabulation code found at line:"
          print *,trim(A)
          stop
        end if
      !} 
       
  IF (len_trim(a)>117) THEN
      if (screen) PRINT *,LEN_TRIM(A),">",TRIM(A)
      READ(A,100)F,X,Y,C4,C5,SCALE,REFV,NBITS
      100 FORMAT(1X,I1,I2.2,I3.3,1X,A64,1X,A24,1X,I3,1X,I12,1X,I3)

  
      IF ((F==0).and.(x<=63).and.(x>=0).and.(y<=256).and.(y>=0)) THEN 
         IF (NBITS>256) THEN
            print *,"*** Erro reading BUFR table ***"
            write(*,'(" Number of bits > 256 in ",i1,i2.2,i3.3,"  at Line=",i5," Nbits=",i5)')f,x,y,i,NBITS
            NBITS=NBITS/8
            if ((NBITS<256).and.(index(C5,"IA5")>0)) then 
              print *, "Convert nbits to number of characters =",nbits
              
            else 
              close(1)
              stop
            end if
         END IF  
       END IF
   elseif(len_trim(a)>0) then 
         print *,"Erro reading BUFR TABLE B near line",i
         write(*,'("[",a117,"]")')A 
         stop
   END IF
       C4=ucases(C4)
       C5=ucases(C5)
       write(2,100) F,X,Y,C4,C5,SCALE,REFV,NBITS
   GOTO 10
999   CLOSE(1)
      close(2)
      
  END SUBROUTINE COPY_TABB  


SUBROUTINE COPY_TABD(infile,outfile)
	  character(len=255)::infile,outfile 
	  
	  !{Declaracao de variaveis auxiliares
	  INTEGER::uni
	  INTEGER ::nl,l,f,x,y,f2,x2,y2,i,f1,x1,y1,nl1
	  CHARACTER(len=255)::linha
          character(len=255)::comment
	  character(len=1)::t1
	  !}
        
        print *,"Infile=",trim(infile)
        print *,"Outfile=",trim(outfile)
	OPEN (1, FILE =infile, ACCESS = 'SEQUENTIAL', STATUS = 'OLD')
  	OPEN (2, FILE =outfile, ACCESS = 'SEQUENTIAL', STATUS = 'UNKNOWN')
  	  

888   READ(1,'(a)',END=9898)linha
	 if (screen) print *,trim(linha) 
	  read(linha,444)f1,x1,y1,nl1,t1,F2,X2,Y2,COMMENT
	  if (t1/="") then
	  	  read(linha,443)f1,x1,y1,nl1,t1,F2,X2,Y2 
	  end if

       if (nl1>0) then 
         if (COMMENT(1:1)/=" ") COMMENT=" "//trim(COMMENT)
         write(2,444)f1,x1,y1,nl1," ",F2,X2,Y2,trim(COMMENT)
       else 
         write(2,445)F2,X2,Y2,trim(COMMENT)
       end if

443    FORMAT(1x,i1,i2,i3,1x,i2,a1,i1,i2,i3)
444    FORMAT(1x,i1,i2.2,i3.3,1x,i3,a1,i1,i2.2,i3.3,A)
445    FORMAT(12x,i1,i2.2,i3.3,A)	 
       goto 888
9898  continue

      close(1)
      close(2)
	  END SUBROUTINE COPY_TABD

END PROGRAM
