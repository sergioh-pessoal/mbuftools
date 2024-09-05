!################################################################################
!#                                                                              #
!#                                 DATELIB                                      # 
!#                                                                              #
!#      MODULO DE FUNCOES PARA PROCESSAR DATA E HORAS EM FORTRAN 90             #
!#                                                                              #
!#      Copyright (C) 2004  Sergio Henrique S. Ferreira                         #
!#                                                                              #
!#      MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                               #
!#                                                                              #    
!#------------------------------------------------------------------------------# 
!# Tipo        : MODULO DE USO GERAL  (FORTRAN 90)                              # 
!# Dependencias: NAO HA                                                         #
!#------------------------------------------------------------------------------#
!#  Descricao:                                                                  #
!#                                                                              #
!#   Este modulo contem subrotinas para trabalhar com data e hora em fortran    #
!#   Neste modulo chama-se data-hora variaveis do tipo REAL*8 que representam   # 
!#   a data  no calendario juliano em dias e fracoes de dias                    # 
!#	 Atraves das funcoes disponiveis neste modulo e posivel converter data  #
!#   do calendario gregoriano para juliano e vice e versa                       #
!#                                                                              #
!#   A conversao para calendario juiano (jdate) permite operar aritimeticamente #
!#   com estes valores,  sem ser necessario a preocupacao com as "viradas"      #
!#   de mes e ano                                                               #
!#------------------------------------------------------------------------------#
!#  FUNCOES PUBLICAS                                                            #  
!#       fjulian : Retorna  real*8 que representa a data do  calendario juliano #
!#                 a partir do dia 1 de janeiro do ano zero em                  #
!#                 dias e fracoes de dias                                       #
!#                                                                              #
!#                                                                              #
!#                                                                              #
!#                                                                              #
!#    	  year(jdate) : Retorna inteiro que representa o ano                    #
!#        month(jdate): Retorna inteiro que representa o mes                    #
!#        day (jdate) : Retorna inteiro que representa o dia                    #
!#        hour (jdate): Retorna inteiro que representa a hora                   #
!#        minute(jdate): Retorna inteiro que representa os minutos              #
!#        grdate(jdate): Retorna caracter *19 que corresponde a data            #
!#                       no calendario gregoriano,em formato utilizado          #
!#                       pelo programa Grads                                    #
!#        iso_8601_Basic: Retorna a data no formato da norma ISO8601            #
!#                                                                              #
!#       Notas:                                                                 #
!#        a) Para converter uma data do calendario gregoriano para juliano pode #
!#           ser utilizado um dos dois modos seguintes:                         #
!#                                                                              #
!#      jdate=fjulian(iyear,imonth, iday,ihour,iminute)                         #
!#                                                                              #
!#                                                                              #
!#              jdate=fjulian("yyyymmddhhnn")                                   #
!#                                                                              #
!#        b) jdate eh  real*8 que representa a data do calendario juliano       #
!#           a partir do ano 0 em dias e fracoes de dias                        #
!#                                                                              # 
!#        c) Sao validas todas as operacoes matematicas sobre jdate             #
!#           Exemplo 1 : Para somar um dia a jdate:         jdate = jdate + 1.0 #
!#           Exemplo 2 : Para subtrair 12 horas de jdate:   jdate = jdate - 0.5 #
!#                                                                              #
!#        d) Para retornar ao calendario gregoriano use as funcoes apropriadas  # 
!#            (year,month,day,hour,minute)                                      #
!#                                                                              #
!#                                                                              #
!#   Conversoes para outras datas                                               #
!#                                                                              #
!#                                                                              #
!#         SOFTWARE            VALOR DE CONVERSAO                               #
!#       ------------------   ------------------                                #
!#       De VB para datelib:         + 693975                                   # 
!################################################################################
!#      REVISAO HISTORICA                                                       
!#                                                                               
!# 2004             V 1.0 - VERSAO ORIGINAL SERGIO HENRIQUE S. FERREIRA                     
!# 20051111 -SHSF-  V 1.1  Reduzido erro de precisao na funcao  fjulian1    
!#                         da subrotina fjulian1 devido a conversao de horas,           
!#                         minutos e segundos em dia. O problema da precisao            
!#                         ainda existe e eh de aproximadamente -1 minuto               
!# 20081205 -SHSF- V 1.2   Corrigido bug na subrotina month (com relacao ao mes 12)
!# 20091002 -SHSF  V 1.3  Acrescentado funcionalidade para converter data no formato
!#                        do grads para data juliana. Nota: A partir desta versao
!#                        passa a se ter a dependencia da stringflib v.2009  
!# 20100405 -SHSF V 1.4  Acrescentado rotina para escrita da data no formao ISO:8601 (iso_8601_Basic)
!# 20100410 -SHSF V 1,4,1 Acrescentado rotina minute 
!# 20131117 -SHSF V 1,4,2 Process both time formats: HHZ and HH:00Z
!# 20160518 -SHSF         Arredondamento dos minutos (funcao minute)
!# 20180907 - SHSF        New function to convert time in seconds to hours, minutes and seconds was added 
!# 20221010 - SHSF        Change the name of fucntion iso_8601_Basic to iso_date
 MODULE DATELIB 
  use stringflib, only: ucases, replace
  private
    public fjulian        !Converte para calendario juliano
    public grdate         !Converte para data no formato do grads
    public year           !Obtem o ano de uma data juliana
    public month          !Obtem o mes de uma data juliana ou de norme do mes
    public day            !obtem o dia
    public hour           !Obtem a hora
    public iso_date       !Converte para data no formato ISO 8601
    public minute         !Obtem os minutos
    public year4          !Converte ano com 2 digitos para ano com 4 digitos
    public ymdh
    public sec2hms
    type tpmonth
    integer::nm
    character(len=3)::name
  end type

  type(tpmonth),DIMENSION(12)::monthy

!#==============================================================================#    
!# FJULIAN                                                                 SHSF #
!#------------------------------------------------------------------------------# 
!# Tipo         : INTERFACE PUBLIC                                              #
!#------------------------------------------------------------------------------#                     
!# Descricao:                                                                   #
!# Esta inteface retorna o dia juliano a partir do ano 0 em dias e fracoes de   #
!# dias.                                                                        #
!#                                                                              #
!#   Formas de entrada:                                                         #
!#     A) Varaiavel caracter "YYYYMMDDHH"                                       #
!#     B) Variaveis inteiras (ano, mes, dia, hora,minutos, segundos)            #
!#                                                                              #
!#------------------------------------------------------------------------------# 


	interface fjulian
	 module procedure fjulian1
	 module procedure fjulian2
	end interface

        interface month
          module procedure month1
          module procedure month2
        end interface

	CONTAINS

function year4 (y2);integer::year4
  integer,intent(in)::y2
  if (y2>99) then
     year4=y2
     return
  end if
 
   if (y2<50) then
    year4=y2+2000
   else
    year4=y2+1900
   end if

end function

!#==============================================================================#    
!# INIT - SUB-ROTINA PARA INICIALIZACAO DO MODULO DATELIB                  SHSF #
!#------------------------------------------------------------------------------# 
!# Tipo         : SUBROTINA DE ACESSO PRIVADO               	                #
!# Dependencias :                                                               #
!#------------------------------------------------------------------------------#           
!#  Descricao:                                                                  #
!#  Esta subrotina inicializa o vetor do dias do mes (monthy) conforme          #
!#  o dia do ano                                                                #
!#                                                                              #
!#------------------------------------------------------------------------------# 
 
 subroutine init(year) 
 
 !{ Variaveis de interface
  
	integer,intent(in)::year  ! Ano 
 !}  
 
 
	 monthy(1)%nm=31;monthy(1)%name= "Jan"
	 monthy(2)%nm=28;monthy(2)%name= "Feb" 
	 monthy(3)%nm=31;monthy(3)%name= "Mar"
	 monthy(4)%nm=30;monthy(4)%name= "Apr" 
	 monthy(5)%nm=31;monthy(5)%name= "May" 
	 monthy(6)%nm=30;monthy(6)%name= "Jun" 
	 monthy(7)%nm=31;monthy(7)%name= "Jul" 
	 monthy(8)%nm=31;monthy(8)%name= "Aug" 
	 monthy(9)%nm=30;monthy(9)%name= "Sep" 
	 monthy(10)%nm=31;monthy(10)%name= "Oct"
	 monthy(11)%nm=30;monthy(11)%name= "Nov"
	 monthy(12)%nm=31;monthy(12)%name= "Dec"
	   
	 if (mod(year,4)==0) monthy(2)%nm=29

	 
 end subroutine
	







!#==============================================================================#    
!# FJULIAN1                                                                SHSF #
!#------------------------------------------------------------------------------#           
!#                                                                              #
!# Esta funcao  retorna o dia juliano a partir do ano 0 em dias e fracoes de    #
!# dias. Entrada sao variaveis interiras que representam ano,mes,dia, hora e    #
!# minutos (calendario gregoriano)                                              #
!#                                                                              #
!#------------------------------------------------------------------------------# 
!# FUNCAO REAL *8 de acesso PRIVATIVO, acessado pela INTERFACE PUBLICA FJULIAN  #
!# DEPENDENCIAS: SUBROTINA INIT                                                 #
!#------------------------------------------------------------------------------#

  function fjulian1(year,month,day,hour,min,sec,printerr); real*8::fjulian1
   !{ Variaveis de interface
     integer,intent(in)::year,month,day,hour,min,sec
     logical,optional,intent(in)::printerr ! If true print error message
    !}
    !{ Variaveis locais 
 
       real*8 d,a_hour,a_day,a_min
       logical perr 
     !}
    if (present (printerr)) then 
          perr=printerr
    else 
      perr=.false.
    end if
    if ((month<1).or.(month>12)) then
      !print *, ":DATELIB: Erro! Month=",month
      fjulian1=0
      return
    end if
    if ((hour>24).or.(hour<0))then 
    if(perr)  print *, ":DATELIB: Error: Hour=",hour
      fjulian1=0
      return
    end if

   d=0;h=0;a_day=0;a_min=0;a_hour=0
     call init(year)
      do i=0,(year-1),4
       d=d+1
      end do
  
       d=d+365*year
	  
	  do i=2,month,1
	  
	   d=d+monthy(i-1)%nm
	   
	  end do   
	  
	  
	  a_min=real(min)+real(sec)/60.0
	  a_hour=real(hour)+a_min/60.0
	  a_day=real(day)+a_hour/24.0  	  
	  d=d+a_day
	   
          if (d>0) then  
	  	fjulian1 =d
	  else
	      if(perr) print *,":DATELIB:Error: Invalid date (year,month,day,hour,min,sec)=",year,month,day,hour,min,sec
	      fjulian1=0
	      return
	  end if
	end function

!#==============================================================================#    
!# FJULIAN2                                                                SHSF #
!#------------------------------------------------------------------------------# 
!# Tipo         : FUNCAO REAL *8 de acesso PRIVATIVO          	                #
!# Acessado por : INTERFACE PUBLICA FJULIAN                                     #
!# Dependencias : SUBROTINA INIT, SUBROTINA FJULIAN1                            #
!#------------------------------------------------------------------------------#                     
!# Descricao:                                                                   #
!# Esta funcao  retorna o dia juliano a partir do ano 0 em dias e fracoes de    #
!# dias. A entrada e uma variavel caracter que podem ser dos seguintes tipos:   #
!#  a)  yyyymmddhhnn                                                            #
!#  b)  yyyymmdd"T"hhnn"Z" ( Norma ISO 8601)                                    #
!#  C)  hhZddMMMyyyy (formato do GRads)                                         #
!#------------------------------------------------------------------------------# 
function fjulian2(SDATES);real*8:: fjulian2
 !{ Variaveis da interface 
  character(len=*),intent(in)::SDATES ! Data no formato YYYYMMDDHHmmss
 !}
 !{Variaveis locais
   real*8:: cdate
   integer :: ANO,MES,DIA,HORA,IMIN,SEG
   integer::l,i
   integer::tformat  ! 0=Numerico, 1 = ISO8601, 2 =GRADS
   character(len=15)::sdates2
   integer,parameter::numeric_format=0
   integer,parameter::iso8601_format=1
   integer,parameter::grads_format=2
   
 !}
   sdates2=adjustl(SDATES)
   l=len_trim(sdates2)
   tformat=numeric_format
   
    if((index(sdates2,"T")==9).and.(index(sdates2,"Z")==l)) then 
      tformat=iso8601_format
    else   
      do i=1,l
        if (index("0123456789",sdates2(i:i))==0) then
          tformat=grads_format 
           exit
        end if
      end do
    end if
 
    if (tformat==grads_format) then 
      call CND(SDATES2)
      tformat=numeric_format
    end if

    if (tformat==numeric_format) then 
      read(SDATES2,'(I4,I2,I2,I2,I2,I2)')ANO,MES,DIA,HORA,IMIN,SEG
    else
      read(SDATES2,'(I4,I2,I2,1x,I2,I2)')ANO,MES,DIA,HORA,IMIN
      SEG=0
    end if
    cdate=fjulian1(ANO,MES,DIA,HORA,IMIN,SEG)
    fjulian2=cdate
 end function fjulian2

!#==============================================================================#    
!# month2                                                                 SHSF #
!#------------------------------------------------------------------------------# 
!# Tipo         : FUNCAO REAL                            #
!#------------------------------------------------------------------------------#                     
!# Descricao:                                                                   #
!# Dado o nome do mes, em ingles, esta funcao retorna o numero do mes  
!#------------------------------------------------------------------------------# 

  function month2(cmonth);integer::month2
!{ Variaveis de interface
    character(len=*),intent(in) ::cmonth
   !}
  !{Variavel interna
    integer::i
    character(len=3)::mmm
  !}
   
    mmm=cmonth(1:3)
    month2=0
    monthy(1)%name= "Jan"
    monthy(2)%name= "Feb" 
    monthy(3)%name= "Mar"
    monthy(4)%name= "Apr" 
    monthy(5)%name= "May" 
    monthy(6)%name= "Jun" 
    monthy(7)%name= "Jul" 
    monthy(8)%name= "Aug" 
    monthy(9)%name= "Sep" 
    monthy(10)%name= "Oct"
    monthy(11)%name= "Nov"
    monthy(12)%name= "Dec"
    do i=1,12
      if (ucases(monthy(i)%name)==ucases(mmm)) then
        month2=i
        exit
      end if
    end do
  end function
!#==============================================================================#    
!# GRDATE                                                                  SHSF #
!#------------------------------------------------------------------------------#
!# Tipo        : FUNCAO CHARACTER(LEN=19)  de acesso PUBLICO 	                #
!# Dependencias: funcoes: Year,month,day,hour                                   #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              # 
!#  Retorna um texto  que representa a data no formato (hh:mnZ ddmmmyyyy)",     #
!#  utilizado nos arquivos descritores do grads                                 #
!#                                                                              #	
!# Sintax:  cdate=grdate(jdate)                                                 #
!#                                                                              #
!#           jdate: Ver definicao no inicio do modulo                           #
!#                                                                              # 
!#                                                                              #
!#------------------------------------------------------------------------------# 

function grdate(jdate);character(len=19)::grdate

  
  real*8,intent(in)::jdate

 ! character(len=3),dimension(12)::nm
  character(len=19)::a
  integer :: i
  
 
  i=month(jdate)
  write(a,10)hour(jdate),minute(jdate),day(jdate),monthy(i)%name,year(jdate)
10 format(i2.2,":",I2.2,"Z",i2.2,a3,i4)
  grdate=a
 end function
 
 

!#==============================================================================#    
!# year(jdate)                                                             SHSF #
!#------------------------------------------------------------------------------#
!# Tipo        : FUNCAO	INTEGER  de acesso PUBLICO                              #
!# Dependencias: funcoes: fjulian                                               #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #    
!#  Funcao que retorna o valor do ano (inteiro) de uma data juliana (real*8)    #
!#                                                                              #
!#  Sintax:  iyear=year(jdate)                                                  #
!#                                                                              #
!#------------------------------------------------------------------------------# 
 
 

	function year(d)
	 integer::year
	 real*8,intent(in)::d
	 integer::y
	 real*8::d2
	 
	  y=-1

 10   y=y+1
	   d2=fjulian(y,1,1,0,0,0)
	 if  (d2<=d) goto 10 

	  year=y-1
	end function







!#==============================================================================#    
!# month(jdate)                                                            SHSF #
!#------------------------------------------------------------------------------#
!# Tipo        : FUNCAO	INTEGER  de acesso PUBLICO                              #
!# Dependencias: funcoes: fjulian, year                                         #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  Funcao que retorna o valor do mes (inteiro de 1 a 12) de uma data juliana   #
!#                                                                              #
!#  Sintax:  imonth=month(jdate)                                                #
!#                                                                              #
!#------------------------------------------------------------------------------# 


function month1(d)
	integer :: month1
	real*8,intent(in)::d
	real*8::d2
	integer :: y,m
	 y=year(d)
	 m=0
  10 m=m+1
	 if (m<13)  then
		d2=fjulian(y,m,1,0,0,0)
		if (d2<=d) goto 10 
	 end if
	 month1=m-1

end function


!#==============================================================================#    
!# day(jdate)                                                             SHSF  #
!#------------------------------------------------------------------------------#
!# Tipo        : FUNCAO	INTEGER  de acesso PUBLICO                              #
!# Dependencias: funcoes: fjulian, year,month                                   #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  Funcao que retorna o valor do dia (inteiro de 1 a 31) de uma data juliana   #
!#                                                                              #
!#  Sintax:  iday=day(jdate)                                                    #
!#                                                                              #
!#------------------------------------------------------------------------------# 

 function day(d)
	 integer :: day
	 real*8,intent(in)::d
	 real*8::d2
	 integer :: y,m,a
	 
	     y=year(d)
	     m=month(d)
     	 a=0
  10     a=a+1
	 d2=fjulian(y,m,a,0,0,0)
	 if  (d2<=d) goto 10 
	 day=a-1

	end function



!#==============================================================================#    
!# hour(jdate)                                                            SHSF  #
!#------------------------------------------------------------------------------#
!# Tipo        : FUNCAO	INTEGER  de acesso PUBLICO                              #
!# Dependencias:                                                                #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  Funcao que retorna a hora  (inteiro de 0 a 23) de uma data juliana          #
!#                                                                              #
!#  Sintax:  ihour=hour(jdate)                                                  #
!#                                                                              #
!#------------------------------------------------------------------------------# 

  function hour(d)
	 integer :: hour
	 real*8,intent(in)::d
	 hour=int((d -int(d ))*24+0.01)
	
   end function
!#==============================================================================#    
!# minute(jdate)                                                          SHSF  #
!#------------------------------------------------------------------------------#
!# Tipo        : FUNCAO	INTEGER  de acesso PUBLICO                              #
!# Dependencias:                                                                #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  Funcao que retorna a minuto  (inteiro de 0 a 59) de uma data juliana       #
!#                                                                              #
!#  Sintax:  minute=minute(jdate)                                              #
!#                                                                              #
!#------------------------------------------------------------------------------# 



  function minute(d)
    integer::minute
    real*8,intent(in)::d
    minute=int((d-int(d))*24*60+0.01)-hour(d)*60
    
    
  end function
!#==============================================================================#    
!# cnd|                                                                  SHSF  #
!#------------------------------------------------------------------------------#
!# Tipo        : Subroutin  de acesso retrito                                  #
!# Dependencias: stringflib,ucases                                              #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  Converte mes com 3 letras para valor numerico do mes                        #
!#                                                                              #
!#------------------------------------------------------------------------------# 
! Historico
!   20131117 SHSF : TRATA HORA NO FORMATO HH:00Z e HHZ
  subroutine cnd (NND)
  !{ Variaveis de interface
    character(len=*),intent(inout)::NND
  !}
  integer::i,l
  character(len=15)::cdate
  character(len=3)::mmm
  character(len=2)::mm
  integer::y,z
  integer::d1,d2,h1,h2,pp
  character(len=2)::dc,hc
  
  
  !{ Obtem a data em ordem reversa (YYYYMMMDDHH)
    cdate=NND
    l=len_trim(cdate)
    z=index(ucases(cdate),"Z")
    if (z==0) then 
      print *,"DATELIB:CND:ERROR 1"
      print *,"Erro in Date format =",trim(cdate)
      stop
    end if
    PP=index(cdate,":")
    IF (PP==0) PP=Z
    d1=(z+1)
    d2=(l-7)
    h1=PP-2
    h2=PP-1
    if (h1<1) h1=1
    if (h1==h2) then
      hc="0"//cdate(h1:h2)
    else
      hc=cdate(h1:h2)
    end if
    if (d1==d2) then
      Dc="0"//cdate(D1:D2)
    else
      Dc=cdate(d1:D2)
    end if
    cdate=cdate((l-3):l)//cdate((l-6):(l-4))//dc//hc
    mmm=cdate(5:7)
    mm=""
   !
   !{ Converte MMM em MM
    read(cdate,'(i4)')y
    call init(y)
    do i=1,12
      if (ucases(monthy(i)%name)==ucases(mmm)) then
        write(mm,'(i2.2)')i
        exit
      end if
    end do
    if (len_trim(mm)==0) then
      print *,":DATELIB: ERROR! Incorrected data format ="//trim(NND)
      nnd="0001"
    else
      cdate=replace(cdate,mmm,mm)
      !PRINT *,"CDATE DEP=",TRIM(CDATE)
      nnd=trim(cdate)//"0000"
    end if
 end subroutine
!-------------------------------------------------------------------------------
!#==============================================================================#    
!# ISO_DATE                                                               SHSF  #
!#------------------------------------------------------------------------------#
!# Tipo        : Caracter                                                       #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  Gera a data e hora UTC no formato basico da norma ISO 8601                  #
!#  Isto e [YYYY][MM][DD]T[hh][mm]Z                                             #
!#  Sintax:  ISO8501(JDATE)                                                     #
!#                                                                              #
!#------------------------------------------------------------------------------# 

 function ISO_date(jdate);character(len=18)::ISO_date
   !{ Interface Variables
    real*8,intent(in)::jdate ! Julian date ( in the form used by datelib)
   !}
    write(ISO_date,10)year(jdate),month(jdate),day(jdate),hour(jdate),minute(jdate)
    10 format (i4,2i2.2,"T",2i2.2,"Z")
 end function

!#==============================================================================#    
!# ymdh                                                              SHSF  #
!#------------------------------------------------------------------------------#
!# Tipo        : Caracter                                                       #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  Gera a data e hora UTC no formato basico da norma YYYYMMDD                  #
!#                                                                              #
!#------------------------------------------------------------------------------# 

 function ymdh(jdate);character(len=10)::ymdh
   !{ Interface Variables
    real*8,intent(in)::jdate ! Julian date ( in the form used by datelib)
   !}
    write(ymdh,10)year(jdate),month(jdate),day(jdate),hour(jdate)
    10 format (i4,3i2.2)
 end function
!#==============================================================================#    
!# sec2hms                                                                SHSF  #
!#------------------------------------------------------------------------------#
!# Tipo        : Caracter                                                       #
!#------------------------------------------------------------------------------#           
!# Descricao:                                                                   #
!#                                                                              #                      
!#  From the time in seconds, this function returns the time in hours,          # 
!#  minutes and seconds                                                         #
!#                                                                              #
!#------------------------------------------------------------------------------# 
 
 function sec2hms(sec); character(len=16)::sec2hms
  integer,intent(in)::sec
  integer::mm,hh,ss
  character(len=16)hms
  mm=int(sec/60)
  ss=sec-mm*60
  hh=int(mm/60)
  mm=mm-hh*60
  
  write(hms,'(i10,2(":",i2.2))') hh,mm,ss
  sec2hms=adjustl(hms)
 end function 
END MODULE


	
