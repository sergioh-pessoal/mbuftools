module mcoords
!*******************************************************************************
!*                                 MCOORDS                                     *
!*      Tratamento da sistema de coordenada vertical em arquivos do grads      *
!*                                                                             *
!*                      Sergio Henrique S. Ferreira                            *
!*                                                                             *
!*               MCT-INPE-CPTEC-Cachoeira Paulista, Brasil                     *
!*                                                                             *
!*                                  2009                                       *
!*-----------------------------------------------------------------------------*
!*Autores:                                                                     *
!*   SHSF : Sergio Henrique S. Ferreira <sergio.ferreira@cptec.inpe.br>        * 
!*                                                                             *
!*******************************************************************************
! HISTORY
! 2009   - SHSF: Versao original
! 201001 - SHSF: Atualizacao read_mcoords: A variavel cf passa a ser opcional. 
!                Atualizacao cconvert_z_mcoords
! 20101120-SHSF: Colocado defezas contra dados errados em convert_z
use mgrads
use interplib

implicit none
private
    public coords
    public read_mcoords
    public mdxdy_mcoords
    public convert_z_mcoords
    public xlgrid_mcoords
    public interpgrid_mcoords
    public vertical_border2center_mcoords

!{Sistema de coordenadas
  type coords
    integer::horizontal       ! Identificacao da tipo de coordenada  horizontal
                              ! 1 = Lat e lon em graus 2 = Distancia em metros
    integer::vertical         ! Identificacao do tipo de coordenada  vertical
    logical::vcoords_from_ctl ! Se .true. = Coordenadas verticais provenientes do ctl 
    integer::mtype            ! Tipo de modelo 
  end type
  
  interface convert_z_mcoords
     module procedure  convert1_z
     module procedure  convert2_z
   end interface

real,parameter:: Pi=3.141592653597932384626433832795028841971693993751065123785647
real,parameter:: rad=0.01745329252
real,parameter:: a=6.37e6 ! Raio da Terra
real,parameter:: g=9.80665 !Gravidade normal 
real,parameter:: deegree=1
       
!}

contains


!-----------------------------------------------------------------------------!
!| read_coords| Ler dados do formato binario do grads                  | SHSF ! 
!------------------------------------------------------------------------------
! Esta rotina uma variavel específica  tridimensional dat(:,:,:) 
! de um arquivo no formato binario do grads. 
!
! Caso esta variavel solicitada seja a propria coordenada vertical,
! cada um dos niveis k de dat(:,:,k)  sarão preenchidos com calores constantes
! dos respectivos niveis "k" 
!------------------------------------------------------------------------------
subroutine read_mcoords(bf,cf,code,v,scalef,ReferenceV,t,dat)

!{ VARIAVEIS DA INTERFACE
      type(bindef),intent(inout)::bf             !Definicoes do formato do grads
      type(coords),intent(in),optional::cf       !Indices que identificam os tipos de coordenas horizontais e vertical
      character(len=*),intent(in)::code          !Codigo que identifica a variavel ex.: press = pressao 
      real,dimension(:,:,:),intent(out)::dat     !Matriz de dados com dimensoes compativeis as informadas em bf
      integer,intent(in)::v                      !indice que identifica a variavel a ser lida 
      real,intent(in)::scalef                    !Fator de Escala 
      real,intent(in)::ReferenceV                !Valor de referencia
      integer,intent(in)::t                      !Indice que identifica a coordenada de tempo a ser lida
    
      integer::k,i,j,ns
      real ::s
 
      if (present(cf)) then
	!{ Repassando propriedades omitidas no ctl do grads
		bf%horizontal=cf%horizontal
		bf%vertical=cf%vertical
		bf%vcoords_from_ctl=cf%vcoords_from_ctl
		bf%mtype=cf%mtype
        !}
	end if 
	!{Carregando variaveis 
	if ((bf%vcoords_from_ctl).and.(v==bf%vertical)) then
		do k=1,bf%kmax
			dat(:,:,k)=bf%lev(k)
		end do
	else
	      call readbin_mgrads(bf,code,t,dat(:,:,:))
	end if
	!}

	!---------------------------------------------------------------------
	! No caso da variavel ser a coordenada vertical, e no caso 
	! de haver pontos indefinidos, completa o campo com o valor medio 
	! da coordenada vertical 
	!---------------------------------------------------------------------
	!{
	if((v==bf%vertical)) then 
	do k=1,bf%kmax
	   s=0
	   ns=0
	  
	    do i=1,bf%imax;do j=1,bf%jmax
	    if (dat(i,j,k)/=bf%undef) then 
	    s=s+dat(i,j,k)
	    ns=ns+1
	    end if
	   end do;end do

	  if (ns>0) then 
	    s=s/ns
	  else
	    print *,"MCOORDS: Error on vertical coordenates"
	    stop
	  end if
	  
	   do i=1,bf%imax;do j=1,bf%jmax
	    if (dat(i,j,k)==bf%undef) dat(i,j,k)=s
	   end do;end do
	end do
	end if
       !}
       
       !{ Aplica fator de escala e valor de referencia
      if ((scalef/=0.0).or.(Referencev/=0.0)) then 
         do i=1,bf%imax;do j=1,bf%jmax;do k=1,bf%kmax
	    if (dat(i,j,k)/=bf%undef) dat(i,j,k)=dat(i,j,K)*10**scalef+Referencev
	  end do;end do;end do
      end if
       
end subroutine
			
!} 
  



!-----------------------------------------------------------------------------!
!| convert1_z| converte coordenadas verticais                          | SHSF ! 
!------------------------------------------------------------------------------
!                                                                             !
!  Esta rotina converte as coordenadas verticais de campos em datain          !
!  gerando campos na nova coordenada "dataout"                                !
!                                                                             !
!  Para processar a conversao e necessario que se fornece as definicoes       !
!  do campo de entrada (bi) e os de saida (bo e co) onde :                    !
!                                                                             !
!  bo%lev(:) Devera conter os valore dos niveis verticais para interpolacao   !
!  bo%kmax o numero de niveis verticais                                       !
!  bo%vertical: o indice que indica a variavel da coordenada vertical         !
!-----------------------------------------------------------------------------!

subroutine convert1_z(bi,datain,bo,logz,baseonly,fx,dataout)
!{ Variaveis da interface 
    type(bindef),intent(in)            ::bi       ! Definicoes dos dados em ponto de grade 
    real,dimension(:,:,:,:),intent(in) ::datain   ! Dados em ponto de grade para interpolação(x,y,z,var)
    type(bindef),intent(in)            ::bo       ! Definicao dos dados de saida em ponto de grade 
    logical,intent(in)                 ::baseonly ! Se .true. Considera apenas a base da camada homogenia para interpolacao  
    real,dimension(:,:,:,:),intent(out)::dataout  ! Matriz com os dados de saida propriamente ditos
    logical,intent(in)                 ::logz     ! Se .true. Aplica interpolacao logaritimica em relacao a coordenada vertical 
    real,intent(in)::fx                           ! Fator de extrapolacao: se = 0.0 nao permite a extrapolacao de pontos  
                                                  ! se > 0.0 perinte a extrapolacao até o limite de {fx}*{espessura da camada} 
    
!}

!{ Variaveis locais
    integer::i,j,v
    integer:: k        ! Indice para coordenada vertical desejada (superficie k )
    real,dimension(bo%imax,bo%jmax,bo%kmax)::PX
    real,dimension(bo%kmax)::KX
!}
!{      
    if ((bo%vertical<1).or.(bo%vertical>bo%kmax)) then 
     print *, ":MCOORDS: Error in the vertical coordinate especification: bo%vertical=",bo%vertical 
     stop
    end if
   if ((bo%nvars<1).or.(bo%nvars>size(datain,4))) then 
     print *, ":MCOORDS: Error in the number of variables to be interpolated  bo%nvars =",bo%nvars
     stop
    end if
!}	
    
   if (logz) then 
    PX(:,:,:)=log(datain(:,:,:,bo%vertical))
    KX(:)=LOG(bo%lev(:))
   else 
    PX(:,:,:)=datain(:,:,:,bo%vertical)
    KX(:)=bo%lev(:)
   end if

    do v=1,bo%nvars
      if ((v/=bo%vertical).and.(bo%varlevs(v)==bo%kmax)) then 
        do i=1,bo%imax;do j=1, bo%jmax
          call MULTITPL( PX(i,j,:),datain(i,j,:,v),bo%varlevs(v),Kx(:),dataout(i,j,:,v),bo%kmax,fx,bi%undef)
        end do;end do
      end if
    end do

    !{ A variavel da coordenada vertical tera valor constate da respectiva coordenada
      dataout(:,:,k,bo%vertical)=bo%lev(k)
    !} 

    !{ Mudando valor undef caso necessario
      if (bi%undef/=bo%undef) then 
       do v=1,bo%nvars;do i=1,bo%imax;do j=1,bo%jmax; do k=1,bo%kmax
          if (dataout(i,j,j,v)==bi%undef) dataout(i,j,k,v)=bo%undef
       end do;end do;end do;end do
      end if

    !{ Copiando variaveis de superficie
      do v=1,bo%nvars
        if (bo%varlevs(v)<bo%kmax) then        
          dataout(:,:,:,v)=datain(:,:,:,v)
        end if
      end do
    !} 


end subroutine
!-----------------------------------------------------------------------------!
!| convert2_z| converte coordenadas verticais                          | SHSF ! 
!------------------------------------------------------------------------------
!                                                                             !
!  Esta rotina converte as coordenadas verticais de campos em datain          !
!  gerando campos na nova coordenada "dataout"                                !
!                                                                             !
!  Para processar a conversao e necessario que se fornece as definicoes       !
!  do campo de entrada (bi) e os de saida (bo e co) onde :                    !
!                                                                             !
!  bo%lev(:) Devera conter os valore dos niveis verticais para interpolacao   !
!  bo%kmax o numero de niveis verticais                                       !
!  vertvar: o indice que indica a variavel da coordenada vertical             !
!           (na borda ou centro da camada)                                    !
!-----------------------------------------------------------------------------!

subroutine convert2_z(bi,datain,bo,logz,baseonly,fx,dataout,vertvar)
!{ Variaveis da interface 
    type(bindef),intent(in)            ::bi       ! Definicoes dos dados em ponto de grade 
    real,dimension(:,:,:,:),intent(in) ::datain   ! Dados em ponto de grade para interpolação(x,y,z,var)
    type(bindef),intent(in)            ::bo       ! Definicao dos dados de saida em ponto de grade 
    logical,intent(in)                 ::baseonly ! Se .true. Considera apenas a base da camada homogenia para interpolacao  
    real,dimension(:,:,:,:),intent(out)::dataout  ! Matriz com os dados de saida propriamente ditos
    logical,intent(in)                 ::logz     ! Se .true. Aplica interpolacao logaritimica em relacao a coordenada vertical 
    real,intent(in)::fx                           ! Fator de extrapolacao: se = 0.0 nao permite a extrapolacao de pontos  
                                                  ! se > 0.0 perinte a extrapolacao até o limite de {fx}*{espessura da camada} 
    integer,dimension(:),intent(in)    ::vertvar  ! Utilizado para indicar niveis verticais no centro da camada ou na borda da camada
    
!}

!{ Variaveis locais
    integer::i,j,v
    integer:: k        ! Indice para coordenada vertical desejada (superficie k )
    real,dimension(bo%imax,bo%jmax,bo%kmax)::PX
    real,dimension(bo%kmax)::KX
!}
!{      
    if ((bo%vertical<1).or.(bo%vertical>bo%kmax)) then 
     print *, ":MCOORDS: Error in the vertical coordinate especification: bo%vertical=",bo%vertical 
     stop
    end if
   if ((bo%nvars<1).or.(bo%nvars>size(datain,4))) then 
     print *, ":MCOORDS: Error in the number of variables to be interpolated  bo%nvars =",bo%nvars
     stop
    end if
!}	
   if (logz) then 
    KX(:)=LOG(bo%lev(:))
   else 
    KX(:)=bo%lev(:)
   end if
    
   do v=1,bo%nvars
      if (logz) then      
        PX(:,:,:)=log(datain(:,:,:,vertvar(v)))
      else 
        PX(:,:,:)=datain(:,:,:,vertvar(v))
      end if
      if ((v/=vertvar(v)).and.(bo%varlevs(v)==bo%kmax)) then 
       do i=1,bo%imax;do j=1, bo%jmax
          call MULTITPL( PX(i,j,:),datain(i,j,:,v),bo%varlevs(v),Kx(:),dataout(i,j,:,v),bo%kmax,fx,bi%undef)
        end do;end do
      end if
    end do

    !{ A variavel da coordenada vertical tera valor constate da respectiva coordenada
      dataout(:,:,k,vertvar(v))=bo%lev(k)
    !} 

    !{ Mudando valor undef caso necessario
      if (bi%undef/=bo%undef) then 
       do v=1,bo%nvars;do i=1,bo%imax;do j=1,bo%jmax; do k=1,bo%kmax
          if (dataout(i,j,j,v)==bi%undef) dataout(i,j,k,v)=bo%undef
       end do;end do;end do;end do
      end if

    !{ Copiando variaveis de superficie
      do v=1,bo%nvars
        if (bo%varlevs(v)<bo%kmax) then        
          dataout(:,:,:,v)=datain(:,:,:,v)
        end if
      end do
    !} 


end subroutine
!-----------------------------------------------------------------------------!
!| vertical_Border2Center| Interpolacao vertical das bordas para centro| SHSF ! 
!------------------------------------------------------------------------------
!                                                                             !
!  Esta faz a interpolacao dos dados entre as bordas inferiores e superiordes
! de uma camada vertical para o centro da camada                              !
!                                                                             !
!  Esta interpolacao e utilizada para converter as variaveis da grade C       !
!  que estao nas bordas inferiores e superiores para o centro da grade 
!-----------------------------------------------------------------------------!
subroutine Vertical_Border2center_mcoords(bi,datain,V_border,p_border,P_center)
  !{ Variaveis da Interface
    type(bindef),intent(in)               ::bi         ! Definicoes dos dados em ponto de grade 
    real,dimension(:,:,:,:),intent(inout) ::datain     ! Dados em ponto de grade para interpolação(x,y,z,var)
    integer,intent(in)                    ::V_border   !
    integer,intent(in)                    ::P_border   !
    integer,intent(in)                    ::P_center   !
  !}
  !{ Variaveis Locais
    integer::i,j,k                     
    real,dimension(1:bi%kmax+1)::DATAOUT
    real::Z1,Z2,Z
  !}
 
    do i=1,bi%imax;do j=1, bi%jmax
      do k=1,bi%kmax-1
        z1=log(datain(i,j,k,P_border))
        z2=log(datain(i,j,k+1,P_border))
        z =log(datain(i,j,K,P_center)) 
        !z1=(datain(i,j,k,P_border))
        !z2=(datain(i,j,k+1,P_border))
        !z =(datain(i,j,K,P_center)) 

        dataout(k)= ITPL(z1,datain(i,j,k,v_border),z2,datain(i,j,k+1,v_border),z)
      end do 
      datain(i,j,:,v_border)=dataout(:)
    end do;end do
 


end subroutine

!-----------------------------------------------------------------------------!
!| log_gridx| obtem logaritimo de uma variavel em todos os pontos de grade   | SHSF ! 
!------------------------------------------------------------------------------
subroutine log_gridx(bf,dat,ivar)
    type(bindef),intent(inout)::bf ! Definicoes dos dados em ponto de grade 
    real,dimension(:,:,:,:),intent(inout)::dat ! Dados em ponto de grade para interpolação(x,y,z,var)
    integer,intent(in)::ivar ! indica qual a variavel de pressao e que a mesma sera convertida para log(p)logaritimicamente 
  
    integer::i,j,k
    do i=1,bf%imax;do j=1,bf%jmax;do k=1,bf%kmax
	  if(dat(i,j,k,ivar)/=bf%undef) dat(i,j,k,ivar)=log(dat(i,j,k,ivar))
    end do;end do; end do
end subroutine

!------------------------------------------------------------------------------
!-----------------------------------------------------------------------------!
!| exp_gridx| obtem exp de uma variavel em todos os pontos de grade   | SHSF ! 
!------------------------------------------------------------------------------

subroutine exp_gridx(bf,dat,ivar)
    type(bindef),intent(inout)::bf ! Definicoes dos dados em ponto de grade 
    real,dimension(:,:,:,:),intent(inout)::dat ! Dados em ponto de grade para interpolação(x,y,z,var)
    integer,intent(in)::ivar ! indica qual a variavel de pressao e que a mesma sera convertida para log(p)logaritimicamente 
    
    integer::i,j,k
    do i=1,bf%imax; do j=1,bf%jmax; do k=1,bf%kmax
	  if (dat(i,j,k,ivar)/=bf%undef) dat(i,j,k,ivar)=exp(dat(i,j,k,ivar))
    end do;end do; end do

end subroutine     
!------------------------------------------------------------------------------
!mdxdy | Obtem a variacao das coordenadas dx e dy                     | SHSF  |
!-------------------------------------------------------------------------------   
! Obtendo correcoes para dx e dy em fucao das coordenadas horizontais 
! 
!  dy = Espacamento da grade na direcao das latitudes ( e um valor fixo ) 
!  dx = Espacamento da grande na direcao das longitude 
!       Este e um valor que varia com a latitude. Quanto mais proximo dos polos
!       menor e o valor de dx 
!
!       Como nos polos o valor de dx=0 pode causar problemas de calculo
!       adotou-se como valor minio de dx (dxmin = dy/100.0), i.e.  O menor
!       valor de dx é 1/10 de dy
!
!       Esta nao eh uma boa solucao, mas e a unica que encotrei no momento. 
!       Calculos proximo aos polos poderao ter valores distorcidos. 
!------------------------------------------------------------------------------
subroutine mdxdy_mcoords(bi,dx,dy)
    !{ Variaveis da interface 
      type(bindef),intent(inout)::bi
      real,dimension(:),intent(out)::dx
      real,intent(out)::dy
     !}
     integer::s,j
     real::dxmin ! Limite minimo para dx 

		s=size(dx,1)
		if(s<bi%jmax) then
			print *,":MCOORDS:MDXDY: Size DX(j)=",s,"<",bi%jmax
			stop
		end if
	bi%dlat=abs(bi%lat(2)-bi%lat(1))
	bi%dlon=abs(bi%lon(2)-bi%lon(1))


    if (bi%horizontal==2) then 
      dx(:)=bi%dlat
      dy=bi%dlon
 
   else
      dy=a*bi%dlon*rad
      dxmin=dy/100.0
      do j=1,bi%jmax
	dx(j)=a*cos(bi%lat(j)*rad)*bi%dlon*rad
	if (dx(j)<dxmin) dx(j)=dxmin  !<= No momento esta eh a melhor solucao que encontrei para pontos proximos aos polos
      end do

    

    end if

end subroutine
!------------------------------------------------------------------------------
!xlgrid  | Obtem a menor grade regular que contem a grade fornecida | SHSF  |
!-------------------------------------------------------------------------------   
! Esta rotina obtem a menor grade regular que contem todos os pontos de 
! uma grade irregular qualquer, de forma que a resolucao da grade regular 
! obtida seja igual a resolucao mais fina da grade irregular
! 
! Sao fornecidos como dados de entrada as defincoes da grade irregular bi, 
! as coordenadas lat e lon de todos os pontos da grade 
!
! O resultado e a definicao de uma nova grade resultade (bf) que é igualmente
! espaçada em latituide e longitude e cujo o espaçamento tenha resulução igual 
! a melhor resolucao da grade  irregular
!
! IMPORTATE. 
!  1) Por equanto, esta rotina esta preparada apenas para grade irregular com
!     relacao a longitude
!  2) As coordendenadas de longitude devem crescer de i=1,... imax
!  3) 
!------------------------------------------------------------------------------
    subroutine xlgrid_mcoords (bi,xlat,xlon,br)
    !{ Variaveis de interface
      type(bindef),intent(in)        ::bi    ! Definicao dos campos a serem interpolados
      real,dimension(:,:),intent(in) ::xlat  ! matriz as latitudes xlat(i,j) 
      real,dimension(:,:),intent(in) ::xlon  ! matriz das longitudes xlon(i,j)
      type(bindef),       intent(out)::br    ! Definicao da nova grade de interpolacao

    !}
    !{ Variaveis Locais
      integer::i,j 
      real :: i11 ! Minima Longitude oeste 
      real :: i12 ! Minima Longitude leste 
      real :: i21 ! Maxima longitude oeste
      real :: i22 ! Maxima longitude leste
      real :: rx   ! Resolucao maxima
      real :: fscale ! Fator de escala para arredondamento
      integer ::imax
    !}
     br=bi
     i21=xlon(1,1)
     i22=i21
     do j=1,bi%jmax
        if (xlon(1,j)<i21)       i21=xlon(1,j)
        if (xlon(bi%imax,j)>i22) i22=xlon(bi%imax,j)
      end do
      i11=i21
      i12=i22
     do j=1,bi%jmax
        if (xlon(1,j)>i11)       i11=xlon(1,j)
        if (xlon(bi%imax,j)<i12) i12=xlon(bi%imax,j)
      end do
        rx= (i12-i11)/bi%imax
       !print *,"i11,i12,i21,i22=",i11,i12,i21,i22
       !print *,"bi%imax,rx=",bi%imax,rx
       fscale=(log(rx)/log(10.0))
       if (fscale>=0) then
         fscale=int(fscale)+1
       else 
         fscale=int(fscale)-1
       end if
       !print *,"fscale=",fscale
       rx=round(rx,-fscale)
       i21=round(i21,-fscale)
       i22=round(i22,-fscale)
       imax=(i22-i21)/rx
       
       !{ Criando nova grade   
          call newgrid_mgrads(br,imax,bi%jmax,bi%kmax,bi%nvarsmax)
            br%time_init=bi%time_init
            br%time_step=bi%time_step
       !} 
        
       !{ Fornecendo atributos de longitude 
          br%lon(1)=i21
          br%dlon=rx
          do i=2,imax
            br%lon(i)=br%lon(i-1)+br%dlon
          end do
        !}
        !{ Fornecendo os atributos de latitude (copia dos atribudos de bi)
          br%lat(1)=xlat(1,1)
          br%dlat=xlat(1,2)-xlat(1,1)
          do j=2,bi%jmax
            br%lat(j)=br%lat(j-1)+br%dlat
          end do
        !}

    end subroutine
!------------------------------------------------------------------------------
!interpgrid | Interpola campos de uma grade x,y para coordenadas lon/lat  | SHSF  |
!-------------------------------------------------------------------------------   
! Esta rotina converte campos em coordenadas x,y, quaisquer para campos 
! regularmente espassados em termos de latitude e longiture 
!
! Nota: Considera-se que todas as  variaveis de field2 existam em fields1 na 
!       e na mesma ordem. 
!       Caso field1 tenha mais variáveis do que field2, isto não acarreta erro,
!       desde que, estas estjam no final da lista de variaveis. 
!       Caso contrario, i.e., se fields2 tiver mais variaveis que field1, entao
!       para das variaveis não serao interpoladas
!
!      Fields2 e fields 1 podem ter tamanho em x diferentes, porem em z precisao
!      ser iguais, pois nao se tratam de interpolacao em niveis verticais
!------------------------------------------------------------------------------
    subroutine interpgrid_mcoords (b1,vlat,vlon,field1,b2,field2)
    !{ Variaveis de interface
      type(bindef),           intent(inout) ::b1      ! Definicao dos campos de field1
      integer,                intent(in) ::vlat    ! posicao da variavel de latitude em  field1
      integer,                intent(in) ::vlon    ! posicao da variavel de  longitude do campo field1
      real,dimension(:,:,:,:),intent(in) ::field1  ! Campo antes da interpolacao 
      type(bindef),           intent(in) ::b2        ! Definicao dos campos resultantes (obtidos por xlgrid)
      real,dimension(:,:,:,:),intent(out)::field2  ! campo depois da interpolacao
    !}
    !{ Variaveis locais
      integer::i,j,k,v
      integer::xmax,ymax,kmax,nvars
      real,allocatable::aux(:,:,:,:)
    !}
     !{ Verificando variaveis em field1 e field2
      nvars=size(field2,4)
      xmax =size(field2,1)
      ymax =size(field2,2)
      kmax =size(field2,3)
      if (b1%nvars<nvars) nvars=b1%nvars
      allocate(aux(1:xmax,1:ymax,1:kmax,1:nvars))
     !}
     
      !{ Interpolando as longitudes
      do v=1,nvars
        if (b1%varlevs(v)<1) b1%varlevs(v)=1
        print *,":MCOORDS: Interpolando x ",trim(b1%name(v))," [Levels=",b1%varlevs(v),"]"
        do k=1,b1%varlevs(v) 
          do j=1,b2%jmax
           call MULTITPL(field1(:,j,1,vlon),field1(:,j,k,v),b1%imax,b2%lon(:),aux(:,j,k,v),b2%imax,0.0,undefval)
          end do
       end do
      end do
      !}
      !{ Interpolando as latitudes
      do v=1,nvars
        print *,":MCOORDS: Interpolando y ",trim(b1%name(v))," [Levels=",b1%varlevs(v),"]"
        do k=1,b1%varlevs(v) 
          do i=1,b2%imax
           call MULTITPL(aux(i,:,1,vlat),aux(i,:,k,v),b2%jmax,b2%lat(:),field2(i,:,k,v),b2%jmax,0.0,undefval)
          end do
       end do
      end do
      deallocate (aux)
      !}
    end subroutine
!_______________________________________________________________________________
! round | Arredondamento  (rounding)                                    | SHSF                
!-------------------------------------------------------------------------------
! Obtem o valor aproximado de um numero X com um numero de casas decimais C
!------------------------------------------------------------------------------ 
Function ROUND(x, C);real:: ROUND
   !{Variaveis da Interface
      real,intent(in):: X 
      real,intent(in):: C
   !}
   round = real(Int(x * 10 ** C + sign(0.5,x))) / 10.0 ** C!: !: Def. fun. de arredondamento
End Function
!-------------------------------------------------------------------------------
end module
