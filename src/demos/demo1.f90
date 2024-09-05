
Program demo1
!------------------------------------------------------------------------------
! Demostrative program 1 - Using MBUFR to encode BUFR data 
Use MBUFR


type(sec1type)::sec1 !BUFR section 1 data
type(sec3type)::sec3 !BUFR section 2 data
type(sec4type)::sec4 !BUFR section 3 data
integer       ::n    ! Number of subsets
integer       ::err  

!---------------------------------------------------
!Alocacao de espa�o necess�rio nas estruturas sec3 e sec4
!(ate 10 subsets) 
!----------------------------------------------------

allocate(sec3%d(1:33), STAT = ERR)
allocate(sec4%r(1:2000, 1:10), stat = err)

!---------------------------------------------------------
! Atribuicao dos valores da secao 3  Neste utilizamos
! apenas descritores da tabela BUFR B. 
!----------------------------------------------------------
   sec3%d(1)   = 004001  !year
   sec3%d(2)   = 004002  !month
   sec3%d(3)   = 004003  !day
   sec3%d(4)   = 004004  !hour
   sec3%d(5)   = 004005  !Minute
   sec3%d(6)   = 001007  !SATELLITE IDENTIFIER
   sec3%d(6)   = 002152  !SATELLITE INSTRUMENT USED IN DATA
   sec3%d(7)   = 005001  !Latitude
   sec3%d(8)   = 006001  !Longitude
   sec3%d(9)   = 007025  !SOLAR ZENITH ANGLE
   sec3%d(10)  = 027080  !VIEWING AZIMUTH ANGLE
   sec3%d(11)  = 007024  !SATELLITE ZENITH ANGLE
   sec3%d(12)  = 013202  !TYPE OF SURFACE
   sec3%d(13)  = 015001  !TOTAL OZONE
   sec3%d(14)  = 022043  !SEA/WATER TEMPERATURE
   sec3%d(15)  = 012061  !SKIN TEMPERATURE
   sec3%d(16)  = 007001  !HEIGHT OF STATION (SEE NOTE 1)
   sec3%d(17)  = 020016  !PRESSURE AT TOP OF CLOUD PA  (scalar)
   sec3%d(18)  = 020010  !CLOUD COVER (TOTAL) % (scalar)
   sec3%d(19)  = 013011  !TOTAL PRECIPITATION/TOTAL WATER EQUIVALENT KG/M**2
   sec3%d(20)  = 020249  !SEA-ICE FRACTION (scalar)
   sec3%d(21)  = 011011  !WIND DIRECTION AT 10 M
   sec3%d(22)  = 011012  !WIND SPEED AT 10 M
   sec3%d(23)  = 013206  !TOTAL PRECIPITATION ICE CONTENT
   sec3%d(24)  = 107100  ! Repricatior for 100 times
   sec3%d(25)  = 005042  !CHANNEL NUMBER
   sec3%d(26)  = 012063  !BRIGHTNESS TEMPERATURE
   sec3%d(27)  = 014050  !EMISSIVITY % ( = Emissivity of surface)
   sec3%d(28)  = 206011  !
   sec3%d(29)  = 033214  !1D VAR ERROR(S)
   sec3%d(30)  = 206028  ! 
   sec3%d(31)  = 002193  !1D VAR SATELLITE CHANNEL(S) USED
   sec3%d(32)  = 103042  ! Repricatior for 42 times
   sec3%d(33)  = 010004  !PRESSURE
   sec3%d(34)  = 012001  !TEMPERATURE/DRY-BULB TEMPERATURE K  (profile)
   sec3%d(35)  = 013201  !CLOUD LIQUID WATER KG/KG (profile) 
   
   sec3%ndesc  =     35   ! Number of descriptors in section 3 
   sec3%is_cpk =      0   ! 0=Uncompressed , 1= Compressed 


   sec4%nvars=849
   sec3%nsubsets=10 ! 10 subsets
! Atribuicao dos valores na secao 4 (10 subsets) 
  do n=1,10 
   sec4%r(1,n)=2003     !004001 - Year
   sec4%r(2,n)=10       !004002 - month
   sec4%r(3,n)=01       !004003 - day
   sec4%r(4,n)=01       !004004 -hour 
   sec4%r(5,n)=0        !004005 - minute
   end do 
   

    !-----------------------------------
    ! Atribuicao dos valores da secao 1
    !----------------------------------- 
     sec1%btype          = 0      ! type of data  = Surface data 
	sec1%bsubTYPE       = 1      ! Local data subtype
	sec1%intbsubtype    = 0    ! International data subtype
	sec1%center         = 46  ! Center  (46 = INPE) 
	sec1%subcenter      = 0 
	sec1%update         = 0
	sec1%year           = 2003   ! Ano da data sin�tica 
        sec1%month          = 10     ! mes da data sin�tica
     sec1%day            = 01     ! Dia da data sin�tica 
     sec1%hour           = 0      ! Horario sinoptico 
     sec1%minute         = 0      ! Minutos do hor�rio sin�tico  
     sec1%second         = 0      ! Minutos do hor�rio sin�tico  	
	sec1%NumMasterTable = 0 
	sec1%VerMasterTable = 14
	sec1%VerLocalTable  = 0  
     sec1%sec2present    =.false. ! Nao gravar a secao 2 
   
!------------------------------------------------------------
! Uma vez preenchida todas a estrutura de dados utilizamos a
! as subrotinas open_mbufr, para abrir o arquivo, 
! write_mbufr, para gravar os dados e close_mbufr para 
! fachar o aquivo 
!------------------------------------------------------------

call OPEN_MBUFR(1, "example1.bufr")
Call write_mbufr(1,sec1,sec3,sec4)
Call CLOSE_MBUFR(1)

end
