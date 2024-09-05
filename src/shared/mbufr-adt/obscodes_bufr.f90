 module OBSCODES_BUFR
!-------------------------------------------------------------------------------
!                                  OBSCODES
!
! Modulo de convencoes e codigos relacionados aos  dados observacionais
!( Module of conventions and codes related of observed data 
! ----------------------------------------------------------------------------
!  Este modulo relaiza a leitura de um conjunto de "namelists" onde os codigos
!  estao e convencoes estao estabelecidos. Tais codigos ficam disponiveis 
!  para uso publico em outros modulos de programacao 
!
!   KX : Identifica o tipo de dado (synop, temp, ATOVS NOAA15, NOAA16, etc)
!   Kt : Identicica a variavel meteorologica (Pressao, temperatura,..) 
!
! 
!-------------------------------------------------------------------------------
! AUTORES
!    SHSF: Sergio Henrique Soares Ferreira
!-------------------------------------------------------------------------------
!HISTORICO
!  20091127: SHSF : Prototipo da primeira versao

use stringflib, only: val, near,ival,null
implicit none

private
   public init_obscodes         !Inicializa este modulo
   public close_obscodes        !Fecha este  modulo
   public obsid                 
   public map                  ! Mapa de vairaveis e tipos 
   public nkt
   public getktindex 
   public getobstype_obscodes
   public getvarcode_obscodes
   public fformat,p_format
 
type obsid                      
   integer::c_year !Ano
   integer::c_month !mes
   integer::c_day   !Dia
   integer::c_hour !Hora
   integer::c_min ! Minuto
   integer::c_lat !Latitude 
   integer::c_lon !Longitude
   integer::c_wmo1 ! Bloco da Estacao 
   integer::c_wmo2 !Numero da estacao 
   integer::c_lev  ! Nivel isobarico 
   integer::c_wdir ! Coluna da direcao do vento
   integer::c_wvel ! Coluna da Velocidade do vento
   integer::c_u    ! Componente zonal
   integer::c_v    ! Componente meridional
   integer::c_tmpk ! Colunas da Temperatura 
   integer::c_dwpk ! Colunas de ponto de orvalho
   integer::c_mixr ! Colunas de Razao de Mistura
   integer::c_q    ! Coluna de umidade especifica
   integer::c_pres ! Coluna de pressao atmosferica (hPa)
   integer::c_virt ! Temperatura Virtual 
   integer::c_alt  ! Altitude da Estacao  
   integer::c_geo  ! Altura geopotencial ou nivel de voo 
end type

type p_format
  character(len=255)::header
  character(len=10)::year
  character(len=10)::month
  character(len=10)::day 
  character(len=10)::hour
  character(len=10)::minu
  character(len=10)::tdif 
  character(len=10)::lat
  character(len=10)::lon
  character(len=10)::lev
  character(len=10)::wmo
  character(len=10)::ks
  character(len=10)::alt
  logical::print_missing  ! Se verdadeiro entao imprime valor missing 
end type


integer::mapkx_nrows
integer::kxbufr_ncols   !Numero de colunas de tab_obstype 
integer::nkt            !Numero de linhas em KT
integer::nkx            !Numero de linhas em kx
type(obsid)::map        !Mapeamendo das variaveis de identificacao da estacao/data e hora da observacao 
type(p_format)::fformat                           !Formatos do arquivos de saida 
integer,allocatable::tab_bufrvar(:,:)             !Variaveis da secao 4 utilizadas para conversao kx
real,allocatable::tab_mapkx(:,:)                  !Variaveis da secao 4 utilizadas para conversao kx
character(len=12),allocatable::tab_obstype(:,:)   !Tabela de conversao kx (tab-kxbufr)
character(len=30),allocatable::tab_variables(:,:) !Variaveis da secao 4 utilizadas para conversao kx (tab-mapkt)
character(len=50),allocatable::tab_ktnames(:,:)

contains

!----------------------------------------------------------------------------
!close ! Desaloca as variaveis deste modulo                           | SHSF
!----------------------------------------------------------------------------
subroutine close_obscodes
     print *,":OBSCODES: Close"
     deallocate(tab_obstype,tab_mapkx)
end subroutine


!------------------------------------------------------------------------------
!init ! inicializa e aloca as variaveis deste modulo                    | SHSF
!------------------------------------------------------------------------------

!---------------------------------------------------------------------
  subroutine init_obscodes(nmlfile)
!{ Variaveis de interface
   character(len=*),intent(in)::nmlfile ! Arquivo de configuracao (namelist)
!}
!{ Variaveis locais
   character(len=80),dimension(1000)::tab ! tabela de conversao para codigo Kt 
   character(len=80),dimension(1000)::sec4var
   integer                          ::ncols,nrows ! Numero de linhas e colunas das tabelas
   integer                          ::i,j
   integer                          ::c_wmo1,c_wmo2           ! Station ID
   integer                          ::c_year,c_month,c_day    ! Date
   integer                          ::c_hour,c_min            ! time 
   integer                          ::c_lat,c_lon,c_lev,c_alt,c_geo ! Horizontal/vertical  coord.
   integer                          ::c_wdir,c_wvel,c_u,c_v   ! Wind comp.
   integer                          ::c_tmpk,c_virt,c_pres   !
   integer                          ::c_dwpk,c_mixr,c_q  
   character(len=155)               ::aux
   character(len=256)               ::header
   character(len=10)                ::year,month,day,hour,minu,tdif,lat,lon,lev,wmo,ks,alt
   logical                          ::print_missing

!}
!{ namelist
   NAMELIST /map_coords/ c_wmo1,c_wmo2, c_year, c_month, c_day, c_hour, c_min, c_lat, c_lon, c_lev, c_alt,c_geo
   NAMELIST /map_variables/ nrows,ncols,tab
   NAMELIST /map_obsid/ nrows,ncols,tab
   NAMELIST /map_obstype/ nrows,ncols,tab
   NAMELIST /map_wind/ c_wdir,c_wvel,c_u,c_v
   NAMELIST /map_humidity/ c_tmpk,c_dwpk,c_mixr,c_q,c_virt, c_pres 
   NAMELIST /outfile_format/ header,year,month,day,hour,minu,tdif,lat,lon,lev,wmo,ks,print_missing,alt
!}
!{ Processa a leitura do namelist  e modificar configuracoes padroes
    if(len_trim(nmlfile)==0) then
      print *,":OBSCODES:Error reading namelist"
      stop
    end if 
    
    !{ Lendo e processando map_obstype
    OPEN(20,file=nmlfile,status='old')
      READ  (20, map_obstype)
      allocate(tab_obstype(1:nrows,1:ncols))
      call vec2mat(tab,nrows,ncols,tab_obstype)   
      print *,"obstype="
      do i=1,nrows
        aux=""
        do j=1,ncols
          aux=trim(aux)//trim(tab_obstype(i,j))//","
        end do
        !print *,trim(aux)
      end do
      kxbufr_ncols=ncols
      nkx=nrows
    close(20)  
    !}
    !{ Lendo e processando map_obsid
    OPEN(20,file=nmlfile,status='old')
      READ  (20, map_obsid)
      allocate(tab_mapkx(1:nrows,1:ncols))
      call vec2mat_i(tab,nrows,ncols,tab_mapkx)   
      !print *,"mapkx="
      !do i=1,nrows
      !  print *,tab_mapkx(i,1:ncols)
      !end do
      mapkx_nrows=nrows
    close(20)
    !}

    !{ Lendo e processando map_variables
    OPEN(20,file=nmlfile,status='old')
      READ  (20, map_variables)
      allocate(tab_variables(1:nrows,1:ncols))
      call vec2mat(tab,nrows,ncols,tab_variables)   
      !print *,"map_variables="
      do i=1,nrows
       aux=""
        do j=1,ncols 
          aux=trim(aux)//trim(tab_variables(i,j))//","
        end do
        print *,trim(aux)
      end do
      nkt=nrows
    close(20)
    !}

    !{ Lendo e processado mapcoords
    OPEN(20,file=nmlfile,status='old')
      READ  (20, map_coords)
      map%c_year= c_year
      map%c_month=c_month
      map%c_day=  c_day
      map%c_hour= c_hour
      map%c_min=  c_min
      map%c_wmo1= c_wmo1
      map%c_wmo2= c_wmo2
      map%c_lat=  c_lat
      map%c_lon=  c_lon
      map%c_lev=  c_lev
      map%c_alt=  c_alt
      map%c_geo=  c_geo
    close(20)
    !}
    !{ Lendo e processado mapwind
    OPEN(20,file=nmlfile,status='old')
      READ  (20, map_wind)
      map%c_wdir= c_wdir
      map%c_wvel=c_wvel
      map%c_u=  c_u
      map%c_v= c_V
    close(20)
    !}
    !{ Lendo e processado maphumidity
    OPEN(20,file=nmlfile,status='old')
      READ  (20, map_humidity)
      map%c_tmpk= c_tmpk
      map%c_dwpk=c_dwpk
      map%c_mixr= c_mixr
      map%c_q= c_q
      map%c_pres=c_pres
      map%c_virt=C_virt
    close(20)
!}
    !{estabelece format de saida dos dados
    OPEN(20,file=nmlfile,status='old')
     READ  (20, outfile_format)
     fformat%header=header
     fformat%year=year 
     fformat%month=month
     fformat%day=day
     fformat%hour=hour
     fformat%minu=minu
     fformat%tdif=tdif
     fformat%lat=lat
     fformat%lon=lon
     fformat%lev=lev
     fformat%wmo=wmo
     fformat%ks=ks
     fformat%print_missing=print_missing
     fformat%alt=alt
     close(20)
!}
 END SUBROUTINE 
!------------------------------------------------------------------------------
!getktindex |
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
 function getktindex(btype,cbt);integer::getktindex
 !{ Variaveis de interface
    integer,intent(in)::cbt
    integer,intent(in)::btype
 !}
 !{ Variaveis locais
    integer::i
    getktindex=0
!}
    do i=1,nkt
 
      if (cbt==val(tab_variables(i,3))) then 
        if ((btype>=ival(tab_variables(i,4))).and.(btype<=ival(tab_variables(i,5)))) then 
          getktindex=i
          exit
        end if
      end if
    end do 
  end function
!------------------------------------------------------------------------------
!getobstype | Retorna o codigo do tipo de observacao conforme obstype     | SHSF
!------------------------------------------------------------------------------
! Retorna o codigo do tipo de observacao conforme regra estabelecida
! em obstype
! Caso a observacao na satisfazer os limite estabelecidos em obstype,
! o valor "" sera retornado.
!------------------------------------------------------------------------------

function getobstype_obscodes(btyp,bsubtyp,sec4var); character(len=12) :: getobstype_obscodes
!{ variaveis de interface
   integer,intent(in)::btyp !Tipo BUFr
   integer,intent(in)::bsubtyp ! Subtipo BUfr
   real,dimension(:)::sec4var 
!}	
!{ Variaveis locais
   integer::i    !Indice do tipo de observacao (kx)
   integer::k    !Indice da variavel da secao 4 do BUFR
   integer::jn   !Numero da coluna para teste de limites (jn=1 min, jn=2 max)
   integer::j    !Numero da variavel para teste
   integer::v    !Numero de condicoes verdadeiras
!}
  getobstype_obscodes=""
  do i=1,nkx
   
    if ((btyp>=ival(tab_obstype(i,2))).and.(btyp<=ival(tab_obstype(i,3)))) then
      if ((bsubtyp>=ival(tab_obstype(i,4))).and.(bsubtyp<=ival(tab_obstype(i,5)))) then
        jn=4
        v=0
        do j=1,mapkx_nrows
          k=tab_mapkx(j,1)
          jn=jn+2
          
          if ((len_trim(tab_obstype(i,jn))*len_trim(tab_obstype(i,Jn+1)))==0) then
           !{Qualquer valor e aceito 
            v=v+1
           !}
          else 
            !{ Somente valores dentro da faixa sao aceitos
            if (near(sec4var(k),val(tab_obstype(i,Jn)))) sec4var(k)=val(tab_obstype(i,Jn))
            if (near(sec4var(k),val(tab_obstype(i,Jn+1)))) sec4var(k)=val(tab_obstype(i,Jn+1))
            if ((sec4var(k)>=val(tab_obstype(i,Jn))).and.(sec4var(k)<=val(tab_obstype(i,jn+1)))) then 
              v=v+1 
               
            end if
            !}
          end if
        end do   
    
        !{ Se todas as condicoes (mapkx_nrows) forem satisfeitas  entao aceita o dado 
        if (v==mapkx_nrows) then 
          getobstype_obscodes=tab_obstype(i,1)
          exit
        end if
        !}
      end if
    end if
  end do

end function
!------------------------------------------------------------------------------
!getvarcode | obtem o codigo e nome das variaveis                      |SHSF
!------------------------------------------------------------------------------
subroutine getvarcode_obscodes(kt,code,klev,lin,name)
  integer,                  intent(in)  ::kt   ! Indice do variavel 
  character(len=*),         intent(out) ::code ! Codigo da variavel 
  integer,                  intent(out) ::klev ! Tipo de nivel 0= superficie 1 = altitude
  character(len=*),optional,intent(out) ::name
  integer,intent(out)                   ::lin  ! Max number of kt
    lin=nkt
    if (kt>nkt) then 
      print *, ":OBSCODES: Error: Invalid KT number = ",kt
      stop
    end if
    code=tab_variables(kt,1)
    klev=val(tab_variables(kt,2))
    if (present(name)) then
      name=tab_variables(kt,6)
    end if

end subroutine
!------------------------------------------------------------------------------
!vec2mat | From  Vetor To  Matrix                                         |SHSF
!------------------------------------------------------------------------------
! Converte um vetor de scrings em uma matriz inteira com nl linhas e nc colunas
! Valores brancos sao ubstituidos por missing
!-----------------------------------------------------------------------------
subroutine vec2mat_i(vec,nl,nc,mat)
!{ variaveis de interface
	character(len=*),dimension(:),intent(in)::vec !Vetor de entrada
	integer,intent(in)::nl ! Numero de linhas
	integer,intent(in)::nc ! Numero de colunas
	real,dimension(:,:),intent(out)::mat ! Matriz de saida
!}
!{ Variaveis locais
	integer::i,j,k
!}
     k=0
	   do i=1,nl
		do j=1,nc 
		  k=k+1
		  if (len_trim(vec(k))==0)  then 
		    mat(i,j)=null
		  else 
		    mat(i,j)=val(vec(k))
		  end if 
		end do
	   end do


end subroutine



!------------------------------------------------------------------------------
!vec2mat | From  Vetor To  Matrix                                         |SHSF
!------------------------------------------------------------------------------
! Converte um vetor de scrings em uma matriz com nl linhas e nc colunas
!-----------------------------------------------------------------------------
subroutine vec2mat(vec,nl,nc,mat)
!{ variaveis de interface
	character(len=*),dimension(:),intent(in)::vec !Vetor de entrada
	integer,intent(in)::nl ! Numero de linhas
	integer,intent(in)::nc ! Numero de colunas
	character(len=*),dimension(:,:),intent(out)::mat ! Matriz de saida
!}
!{ Variaveis locais
	integer::i,j,k
!}
     k=0
	   do i=1,nl
		do j=1,nc 
		  k=k+1
		  mat(i,j)=vec(k)
		end do
	   end do


end subroutine


  



end module