program bufrtime
!******************************************************************************
!* BUFRTIME ! VERIFICACAO DE DATA  E HORA EM ARQUIVOS BUFR     !MCT-INPE-CPTE *
!******************************************************************************
!       VERSAO 1.0 	 
!*****************************************************************************  
! 1 - DESCRICAO GERAL 
! 
!   Este programa ler as secoes 1 das mensagens BUFR dentro de um arquivo BUFR
!   e verifica as datas e horas declaradas em cada um. 
!  
!   O resultado sao as datas/horas iniciais e finais de cada mensagem BUFR 
!
!  4 - DEPENDENCIAS EXTERNAS: SISTEMA_OPERACIONAL.getenv e modulo MBUFR.F90
! 
!      Notas: a) Para sistema unix e linux getenv nao precisa ser declarado
!             b) Para sistema windows e necessario incluse "USE MSFLIB"
!-------------------------------------------------------------------------------
!   REVISAO HISTORICA
! 
!ABRIL 2006  - SERGIO H. - Versao original 
!SHSF 2011-01-18 : Modificado para poder verificar presenca de estacoes (ainda nao concluido)           



USE MBUFR
!USE MSFLIB  ! Para compilacao em Windows ( Microsoft Power Station )
USE DATELIB 
USE STRINGFLIB, only:replace
implicit none

!{ Declaracao das variaveis utilizadas em read_mbufr 
  type(sec1type)               ::sec1
  type(sec3type)               ::sec3                         
  type(sec4type)               ::sec4
  integer                      ::MBYTES,BUFR_ED         
  integer                      ::err
  Real,parameter               ::Null=-340282300   !valor nulo 
  type(selecttype),dimension(1)::select     
  integer,dimension(10)        ::col               ! Colunas para impressao
  integer                      ::v,s               !indices para variavel e subsets  
!} 


real*8::date_min,date_max,cdate
character(len=19)::labelmax,labelmin
integer*4 ::argc,i
integer::iargc,nfiles
integer::nm                                   ! Numero de mensagens
character(len=255),dimension(1000)::flist    ! Lista de arquivos de entrada 
character(len=255)::outfile                  ! Nome do arquivo de saida
character(len=255)::infile 

!Dim tlbufr As Double

    
select(1)%btype=0 ! Excluir a leitura de todos os tipos de mesagens bufr
                            ! Somente a secao 1 de cada mensagem sera lida 


   !{ Pega os argumentos de Entrada: Data e Nomes dos arquivos de entrada e saida

 	argc =  iargc()	

	
	if ((argc>=2)) then

		i=1;call GetArg(i,outfile)  
		do i=2,argc
			call Getarg(i,infile)
               infile=replace(infile,"//","/")
			flist(i-1)=infile
			nfiles=i-1
		end do
		

	else
	   print *, "+--------------------------------------------------------------------------+"
	   print *, "| bufrtime - Lista datas iniciais e finais de um conjuto de mensagens BUFR |"
	   PRINT *, "| USE: bufrtime outfile bufrfile-1 bufrfile-2 ... bufrfile-n               |" 
	   print *, "└─--------------------------------------------------------------------------+"
	   print *,""
	   stop
  	endif
  !}

!{ Abre arquivo de 
   open (2,file=outfile,status='unknown',access='append')

!{ Abre o arquivo BUFR

do i=1,nfiles
   date_min=0.0
   date_max=0.0
   nm=0
    
    print *,"Pesquisando "//flist(i)
    !Call OPEN_MBUFR(1,flist(i),255,14,0)
    Call OPEN_MBUFR(1,flist(i))

 !}

 !Nesta parte e feita a leitura das mensagens BUFR  (somente a secao 1) 
 !
 !Se nao houver erro de leitura, converte a data da secao1 (ano,mes,dia,etc..)
 !em data juliana, utilizando a funcao fjulian do modulo datelib
 !
 ! A data juliana e comparada com a data minima e maxima obtida anteriormente
 ! se menor que a data minima atualiza a data minina
 ! se maior que a data maxima atualiza a data maxima
 !{

     
write(2,'("+----------------------------------------->")')
write(2,'("| bufr: ",5x,a)')trim(flist(i))
write(2,'("+-------------------+---------------------+")')
 10 CONTINUE   
  
   Call READ_MBUFR(1,sec1,sec3,sec4, bUFR_ED, MBYTES,err,select)
  
    If ((MBYTES > 0).and.(IOERR(1)==0)) Then
        nm=nm+1
       !{ Obtem a data inicial e final de todos os dados 
        cdate=fjulian(sec1%year,sec1%month,sec1%day,sec1%hour,sec1%minute,0)

          if (nm==1) then
             date_min=cdate
             date_max=cdate
          end if
                
          if ((date_min>cdate)) date_min=cdate
          if ((date_max<cdate)) date_max=cdate
       !}
       !{ 
         do s=1, sec3%nsubsets
          COL(:)=0
          do v=1,sec4%nvars 
            if (sec4%d(v,s)==001102)  COL(1)=sec4%r(v,s)  ! NATIONAL STATION NUMBER (NUMERIC)! print *,sec4%d(1,s)
            if (sec4%d(v,s)==004001)  COL(2)=sec4%r(v,s)  !YEAR
            if (sec4%d(v,s)==004002)  COL(3)=sec4%r(v,s)  !MONTH
            if (sec4%d(v,s)==004003)  COL(4)=sec4%r(v,s)  !DAY
            if (sec4%d(v,s)==004004)  COL(5)=sec4%r(v,s)  !hour
           end do
           write(*,'("STATION=",I10,"|-->  DATE=",I4,3I2.2)')COL(1),COL(2),COL(3),COL(4),COL(5)
           write(2,'("| STATION=",I10,"|-->  DATE=",I4,3I2.2," |")')COL(1),COL(2),COL(3),COL(4),COL(5)
         end do 
          
   !deallocate(sec3%d,sec4%d,sec4%r,sec4%c)
 GoTo 10
 End If

 Close (1)

!{ Imprime resultado

  write(labelmin,'("| ",i4,"-",i2.2,"-",i2.2,2x,i2.2,":00")')year(date_min),month(date_min),day(date_min),hour(date_min)
  write(labelmax,'(1x,i4,"-",i2.2,"-",i2.2,2x,i2.2,":00")')year(date_max),month(date_max),day(date_max),hour(date_max)
  write(2,'("+-------------------+---------------------+")')
  write(2,'(2(a," | "))') labelmin,labelmax
  write(2,'("+-------------------+---------------------+")')
  write(2,'("|")')
!}

end do
!}


close(2)

!}

End 

