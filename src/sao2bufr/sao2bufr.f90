program sao2bufr

 use msao
 use mbufr
 use stringflib

 character(len=1024):: sao_filename
 character(len=1024):: bufr_filename
 type(sec1type)::sec1
 type(sec3type)::sec3
 type(sec4type)::sec4
 integer       ::err
 integer :: d_year,d_month,d_day,d_hour,d_minute,d_second
 integer :: i
 integer :: s ! Number of the subset
 integer :: verbose


 sao_filename="CGK21_2024095100000.SAO"
 bufr_filename="CGK21_2024095100000.BUFR"


 !
 ! READ SAO FILE
 !
    call read_sao(sao_filename)

    d_year =val(time_stamp_sounder_settings(3:6))
    !d_jday=val(time_stamp_sounder_settings(7:9))
    d_mouth=val(time_stamp_sounder_settings(10:11))
    d_day=val(time_stamp_sounder_settings(12:13))
    d_hour=val(time_stamp_sounder_settings(14:15))
    d_minute=val(time_stamp_sounder_settings(16:17))
    d_second=val(time_stamp_sounder_settings(18:19))


!
!  WRITE BUFR FILE
!
 verbose=2
 call INIT_MBUFR(verbose,.true.)
 call open_mbufr(2,bufr_filename)

 !------------------
 ! BUFR SECTION 1
 !----------------
   sec1%NumMasterTable=0
   sec1%center        =46   ! INPE- Brazilian space agence
   sec1%subcenter     =0    !
   sec1%update        =0    !
   sec1%btype         =41   ! bUFR TYPE (BUFR CATEGORY )Conforme OSCARa category need definition from WMO (common code table C13 (SUBCATEGORY))
   sec1%Intbsubtype   =0    ! need definition from WMO
   sec1%bsubtype      =0    ! need definition from WMO
   sec1%VerMasterTable=41   ! need definition from WMO (A NOSSA CRITERIO) FUTURAMENTE O NUMERO CORRENTE DA VERSAO
   sec1%VerLocalTable =43   ! definition at INPE (A NOSSA CRITERIO) FUTURAMENTE  = 0
   sec1%year          =     d_year
   sec1%month         =     d_month
   sec1%day           =     d_day
   sec1%hour          =     d_hour
   sec1%minute        =     d_minute

!
! BUFR SECTION 3 - DECRIPTION OF THE DATA SECTION
!
   sec3%nsubsets = 1 ! Number of subsets
   sec3%is_cpk=0     ! if 0 = No compressed file
   sec3%ndesc=8      ! ***** IMPORTANTE ***** Number of descriptors to be  included in section 3 (se acrescentar descritores precisa atualizar o numero  aqui)
   allocate(sec3%d(sec3%ndesc),STAT=ERR)
   i=0
   i=i+1; sec3%d(i)= 301150 !WIGOS IDENTIFIER
   i=i+1; sec3%d(i)= 004001 !YEAR
   i=i+1; sec3%d(i)= 004002 !MONTH
   i=i+1; sec3%d(i)= 004003 !DAY
   i=i+1; sec3%d(i)= 004004 !HOUR
   i=i+1; sec3%d(i)= 004005 !MINUTE
   i=i+1; sec3%d(i)= 004006 !SECOND
   i=i+1; sec3%d(i)= 043001 !ID DA VARIAVEL  "043581 F0ES" (43581) Mudardo para 43001
   sec3%ndesc=i
!
! BUFR SECTION 4 - DATA SECTION
!
  i=0
  s=1
  sec4%nvars=sec3%ndesc+18  !**** IMPORTANTE *****  Se acrescentar variaveism precisa atualizar o numero  aqui!
  allocate(sec4%r(sec4%nvars,sec3%nsubsets),STAT=ERR)
  i=i+1;sec4%r(i,s)=0         ! 0-01-125-WIGOS IDENTIFIER SERIES (NUMERIC)
  i=i+1;sec4%r(i,s)=0         ! 0-01-126-WIGOS ISSUER OF IDENTIFIER (NUMERIC)
  i=i+1;sec4%r(i,s)=0         ! 0-01-127-WIGOS ISSUE NUMBER (NUMERIC)
  do k=1,16                   ! 0-01-128-WIGOS LOCAL IDENTIFIER (CHARACTER) (CCITTIA5)
    i=i+1;sec4%r(i,s)=ichar(" ")
  end do
  i=i+1;sec4%r(i,s)=d_year
  i=i+1;sec4%r(i,s)=d_month
  i=i+1;sec4%r(i,s)=d_day
  i=i+1;sec4%r(i,s)=d_hour
  i=i+1;sec4%r(i,s)=d_minute
  i=i+1;sec4%r(i,s)=d_second
  i=i+1;sec4%r(i,s)=d_F0ES*10**6    !<=== Mudar de MHZ para HZ
  call write_mbufr(2,sec1,sec3,sec4)
  call close_mbufr(2)

end program
