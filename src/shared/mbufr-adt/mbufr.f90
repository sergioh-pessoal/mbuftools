!>
!!                               MODULO MBUFR-ADT                             
!!                                                                            
!!      Module to Encode/Decode Meteorological Data in the FM94-BUFR            
!!                       using Abstract Data Type                             
!!                 Copyright (C) 2005 Sergio Henrique S. Ferreira             
!!                                                                            
!!       This library is free software; you can redistribute it and/or        
!!       modify it under the terms of the GNU Lesser General Public           
!!       License as published by the Free Software Foundation; either         
!!       version 2.1 of the License, or (at your option) any later version.   
!!                                                                            
!!       This library is distributed in the hope that it will be useful,      
!!       but WITHOUT ANY WARRANTY; without even the implied warranty of       
!!       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU    
!!       Lesser General Public License for more details.                      
!!                                                                            
!******************************************************************************
!*                             MODULO MBUFR-ADT                               *
!*                                                                            *
!*     MODULO PARA CODIFICAR E DECODIFICAR DADOS METEOROLOGICOS EM FM94-BUFR  *
!*                   UTILIZANDO TIPOS DE DADOS ABSTRATOS                      *
!*                                                                            *
!*             Copyright (C) 2005 Sergio Henrique S. Ferreira  (SHSF)         *
!*                                                                            *
!*                                                                            *
!*     Esta biblioteca e um software livre, que pode ser redistribuido e/ou   *
!*     modificado sob os termos da Licenca Publica Geral Reduzida GNU,        *
!*     conforme publicada pela Free Software Foundation, versao 2.1 da licenca*
!*     ou  (a criterio do autor) qualquer versao posterior.                   *
!*                                                                            *
!*     Esta biblioteca e distribuida na esperanca de ser util,  porem NAO TEM *
!*     NENHUMA GARANTIA EXPLICITA OU IMPLICITA, COMERCIAL OU DE  ATENDIMENTO  *
!*     A UMA DETERMINADA FINALIDADE. Veja a Licenca Publica Geral Reduzida    * 
!*     GNU para maiores detalhes.                                             * 
!*                                                                            *
!******************************************************************************

MODULE MBUFR


!
! 1- SUB-ROTINAS PUBLICAS BASICAS
!   As rotinas publicas fornecidas por este modulo sao:
!
!    a) OPEN_MBUFR 
!    b) WRITE_MBUFR 
!    c) READ_MBUFR 
!    d) CLOSE_MBUFR
!
!    A SUB-ROUTINA OPEN_MBUFR abre um arquivo BUFR de acesso aleatorio, para
!    gravacao dos dados BUFR. Tambem inicia as estruturas de dados internas
!    deste modulo, incluindo a tabela de descritores BUFR.
!
!    A SUB-ROTINA	 Grava uma mensagem BUFR em um arquivo aberto por
!    OPEN_MBUFR.
!
!    A SUB-ROTINA READ_MBUFR Le uma mensagem BUFRS em um arquivo aberto por 
!    OPEN_MBUFR
! 
!    A SUBROUTINA CLOSE_MBUFR(UN) Fecha um arquivo BUFR  aberto por OPEN_MBUFR
!  
!   1.1- SUB-ROTINAS PUBILICAS ESPECIAIS 
!     
!     e) INIT_MBUFR
!     f) GET_NAME_MBUFR
!     g) MESSAGEPOS_MBUFR
!     h) SETPOS_MBUFR
!
!  2- TIPOS DE DADOS PUBLICOS
!     sec1TYPE
!     sec2TYPE
!     sec3TYPE
!     sec4TYPE
!     exlistTYPE
!
!  3 - DEPENDENCIAS EXTERNAS: SISTEMA_OPERACIONAL.getenv
!      Notas: a) Para sistema unix e linux getenv nao precisa ser declarado
!             b) Para sistema windows e necessario incluir "USE MSFLIB"
!
!  4 - INTERNAL SUB-ROUTINES 
!      TABLE C DESCRIPTOR -> 2-04-XXX add_associated_descriptor 
!-------------------------------------------------------------------------------
!  REVISAO HISTORICA 
!  NOV2005  -v0.1- SHSF - Versao original compativel com os as edicoes BUFR Edicao:2 e BUFR Edicao: 3             
!  JULHO2006     - SHSF - Corrigido BUG em Write_MBUFR. Em principio a variavel desc_sec4(:)%i=0
!  20060803  -   - SHSF - Corrigido Bug na leitura dos minutos da secao 1
!  20060804  -   - SHSF - Introduzido subrotina interna SAVESEC4RD - Gravacao BUFR com
!                         replicador atrasado 
!  20070118  -   - SHSF - INIT_TABD: Eliminado a atribuicao de valores zeros em todos 
!                         os termos de TABD(:,:,:,:) para otimizar a inicializacao  
!                         desta tabela
!  20070202 -V1.5- SHSF - tabc_setparm: Modificacao no tratamento do erro 51 (remocao do "stop"
!                         para permitir que o programa prossiga. Obs.: E necessario verificar
!                         o processamento do descritor da tabela C 2-04-yyy, pois, eventualmente
!                         aparecem em mensagens SATEM pre-processadas  
!  20070324 -V1.6- SHSF - Corrigido teste de erro na leitura da secao 3 ( antes saia sem desalocar memoria) 
!  20070416 -V1.7- SHSF - Corrigido funcao bits_tabb para que retorne zero ao receber descritor da tabela C
!  20070603 -V1.8- SHSF - Acrescentado verificacao de "\" ou "/" na variavel de ambiente MBUFR_TABLES; 
!                         acrescido inicializacao de sec4%c(:,:)=0 antes da leitura da secao 4.        
!  20070922 -V2.0 SHSF  - Atualizacao da secao 1 para leitura do BUFR EDICAO 4. Acrescentado teste em READ_MBUFR
!                         Para tratamento  do replicador atrasado com BUFR comprimido. Tambem introduzido remocao
!                         de descritores quando o fator de replicao zero, para o caso da leitura em modo 
!                         comprimido. Falta fazer o mesmo nos outros modos e tambem na gravacao. Feito um 
!                         paleativo para evitar erro de leitura em modo comprimido, quando tem dados do tipo
!                         caracter.                
! 20071019 -V2.1 SHSF     Feito modificacao na subrotinas de reinicializacao da tabelas BUFR e inserido o tratamento 
!                         do fator de replicacao ZERO em readsec4rd - Falta comparar resultados
! 20071113 _V2.2 SHSF   a)  Criado nova subrotina (remove_desc) para remocao de descritores, para ser usada com replicadores zero
!                           Em readsec4cmp e readsec4_rd. 
!                       b)  Readsec4cmp foi modificado para passar a aceitar erro de leitura de -12 bits 
!                           devido a erros na leitura de dados de boias. Necessario verificar se o erro esta nos
!                           arquivos de boias do ARGOS ou se e algum detalhe neste programa. Nota: Este erro ocorre
!                           nos casos de BUFR compactato + replicador atrasado+variaveis catacteres
!                       c)  Modificado subrotina de expansao de subdescritores para melhor adequar ao sistema, principalmente
!                           quando envolve replicadores pos-postos. Quando ocorre replicadores pos-postos, o subotina de expansao
!                           de descritores e rodada varias vezes, incluindo a parte de subdescritores. Contudo a subrotina de 
!                           de subdescritores nao podia ser rodada mais de uma ves, pois isto geraria criacao de subdescritores 
!                           que de fato nao existiam.Com a modificacao implementada, somentes as variaveis caracteres que nao
!                           foram convertidas em subdescritores anteriormente serao convertidas na chamada. 
!                       Nota: Tais modificacoes foram testadas na leitura. Para a gravacao de BUFR a gravacao simultanea de
!                       replicadores pos-postos no modo compactado ficam proibidos    
! 20080104 V2.3 SHSF   Corrigido BUG na rotina readsec4cmp no controle do loop de descritores   
! 20080118 V2.4 SHSF   Modificado leitura da tabela B para detectar eventuais erros de formatacao da mesma.
! 20080122 V2.5 SHSF   Modificado CINT e CVAL para tratar valor null. Modificado READSEC4CMP para  fazer consistencia do valor  minimo
!                      e do contador de bits na leitura compactada
! 20080129 V2.6 SHSF   Corrigido readsec4rd.estava repetindo a leitura do primeiro subset 
! 20080201 V3.0 SHSF   Corrigido problema de imprecisao na conversao de numeros reais para inteiros
!                      Foi modificado a funcao CINT,com a inclusao da aproximacao de 0.5 na conversao de inteiros
! 20080308 V3.1 SHSF   Modificado CINT para tratar valores negativos
! 20080417 V3.2 SHSF   Revisao da gravacao da secao 1 savesec1 e da indicacao da gravacao da secao 2
! 20080619 V3.3 SHSF   Introduzido redutor de numero de descritores maximos (ndescmax) em read_mbufr para o caso 
!                      de arquivos sem descritores replicadores 
! 20080620 V4.b SHSF   Corrigido e atualizado gravacao da secao 1, que agora pode gravar a secao 3 ou 4 
!                      Mudado open_mbufr para considear a edicao 4 como edicao padrao de gravacao
! 20080804 V4.0 SHSF   Modificacao do check_table a reinittable para desvinculacao da versao 14 da tabelas mestres anteriores 
!                      e para considerar tabelas mestres diferentes de zero
! 20081205 V4.0.1 SHSF Introduzido teste na leitura da secao 1 para verificar se a edicao 4 tem 22 ou 24 bytes
! 20081210 V4.0.2 SHSF Introduzido arquivo de configuracoes e mudancao da inteface de OPEN_Mbufr
! 20090311 V4.0.3 SHSF Introducao do modo de autogeracao, para geracao automatica de um arquivo BUFR vazio
! 20090411 V4.0.4 SHSF Introducao das rotinas loadmessage 
! 20090519 V4.0.5 SHSF Corrigido reinicializacao do nome dos arquivos da tabela D, que ainda acessavam os
!                      arquivos antigos com terminacao ".ext" 
! 20090618 V4.0.6 SHSF Tratamendo do descritor 2-05-yyy (inclusao de tabela de codifgos alfanumericos 205YYY
! 20100904 v4.0.8 SHSF Incluido impressao de dados no savesec4rd para mostrar os descritores e variaveis processadas
! 20101006 v4.0.9 SHSF Corrigido valor MISSING em GET_OCTETS: Agora considera MISSING todos os valores "1", mesmo que 
!                      o elemento da secao 4 tenha apenas 1 bit
! 20100115 v4.1.0 SHSF Eliminado chamada de tabc_setparm da funcao CINT. Revisado as rotinas  SAVESEC4RD, SAVESEC4
!                      que chamam a funcao CINT.  Esta modificacao e feita para previnir erros  de codificacao de 
!                      templates que utilizam descritores da tabela C (2-01-YYY e 2-02-yyy)
! 20100115 v4.2.0 SHSF - Preparacao para uso futuro
! 20100124 v4.2.3 SHSF - Processamento experimental do descritor 2-07-yyy e processamento do compressao da variavel caracter(somente leitura)
! 20101113 v4.2.4 SHSF - Inclusao de opcao DEBUG em write_mbufr
! 20110115 v4.2.5 SHSF - Verificacao erro de leitura AWS INMET +replicador com 1 bit = null + Inclusao de rotina de controle de erros 
! 20110123 v4.2.6 SHSF - Introducao de funcioanlidades para descritores  2-35-000, 2-24-255, 2-25-255
! 20110717 v4.2.7 SHSF - Novas opcoes para passagem dos caminhos para as tabelas BUFR
! 20110919 v4.2.8 SHSF - Removido incompatibilidade no flag is_tac em savesec3
! 20111001 v4.3.0 SHSF - Acrescentado flag de parentesco entre descritores (filhos e irmaos) 
! 20130927        SHSF - Na leitura das tabelas B, mudado limite para descritores texto de 256 para 496 caracteres 
! 20131010 v4.3.1 SHSF - Revisado a expansão de descritores, para o caso de uso de descritores D para fazer replicação 
!                        posposta nos próximos descritores D. (Caso dos arquivos PREPBUFR.).
! 20140205 V4.3.2 SHSF - Ensuring that the variable Vmin is zero before processing the compressed character variable
! 20140521 V4.4.1 SHSF - Includes selection by generator center 
! 20140612 V4.4.2 SHSF - Correção de bug na leitura da tabela B - Cerreção emergencial para leitura de dados do PCD do INMET
!                        Acompanha inclusao da tabela 431700 para compatibilizar com versoes anteriores (Linha 1904)
! 20140718 V4.3.4 SHSF - Option to read the telecommunications header
! 20141027 V4.4.3 SHSF - Incluido funcionalidade do descritor 2-04-yyy ( testado somente para a leitura ) 
! 20141213 V4.4.4 SHSF - Revisado funcionalidade do descritor 2-04-yyy para o caso do templete 3-11-010 que contem 
!                        replicadores atrasados. Como a funcao precisa ser chamada recursivamente neste caso, entao
!                        precisou-se ter um controle para que o  2-04-yyy nao fosse aplicado mais de uma vez ou deixasse 
!                        de ser aplicado (perdesse a memoria da aplicacao) a cada recurso da rotina  
! 20150427 V4.4.5 SHSF - Revisto a sequencia de procura de tabelas BUFR. A partir dessa versao é verificado a versao da
!                        tabela de codificacao esta presente no diretorio. So no caso de nao existir a tabela e que se 
!                        procura pela tabela similar ou mais atualizada. 
! 20150921 v4.4.6 SHSF  -Corrigido falha  para encontrar tabela similar. com numero de centros gerados diferentes 
! 20150923 v4.4.7 SHSF  - Incluido header de terlcomunicacoes no arquivo de erro para facilitar identificacao
! 20151002        SHSF  -Implementado defesa contra tabela local =255
! 20151005        SHSF  -Corrigido bug na idernticacao da mensagem quando header de telecomunicocoes na existe. 
!                        Incorpotado idernficacao da secao 0 e heade na subrotina headerid
! 20170525 V4.4.8 SHSF - Inclusao de subrotina de inicializacao e parametro de verbosidade 
!                        Acrescentado defesa em readsec4rd2 para o erro 63 (especialmente para dados do ARINC)
!                        Falta verificar o que ocorre com estes dados. Possivel problema com interacao da funcao de 
!                        adicao de parametro associado com a replicadores pospostos 
! 20170701 V4.5.0 SHSF - Adotado uma solucao para desalocacao de momoeria com o uso de read_mbufr. Nesta se inclui uma 
!                        subrotina (deallocate_mbufr) que desaloca todas a estrutura de sec3 e sec4 para a proxima leitura
! 20170815 V4.5.2 SHSF - Passado leitura binaria com inteirdos de  64 bits. Falta verificar como fica a passagem para 
!                        numeros reais e tambem como fica a codificação em 64 bits (esta continua em 32 bits) xima leitura
! 20170712  V4.5.1b SHSF -Feito modificaçao para permitir a leitura de dados do centro 56 que estao aparentando depender de
!                        completar o ultimo byte de cada subset de informação. Esta questão precisa ser verificada com mais
!                        atencao, pois pode ser um problema de tabela 
! 20171010  V4.5.2b SHSF -Incluido subrotinas de acesso direto as mensagens BUFR: MESSAGEPOS_MBUFR. Localiza a posicao 
!                        de todas as mensagens dentro do BUFR:setpos_mbufr - Posiciona a leitura em uma mensagem especifica
!                        permitindo o acesso direta a esta mensagem  
! 20171027  V4.5.3b SHSF-Sincronizado algumas partes com a versao do trank v4.5.3
! 20171114  V4.5.4  SHSF- Inclusao do uso do kind (realk, intk) e feito teste com kind=8
! 20171206  V4.5.4  SHSF- Criacao da funcao undef - Para fornece o valor indefinido utilizado por este modulo
! 20180116  V4.6.0  SHSF -MOdificado inteface do read_mbufr O paramentro ndescmax nao e mais passado. Tambem 
!                         melhorado a allocacao dinamica do mbufr com comando allocated 
! 20180206  V4.6.1  SHSF- Criado subrotina read_info123: Leitura das secoes 0,1, 2, 3 e header a partir do codigo 
!                         existente em read_mbufr. Com isto esta parte do codigo pode ser compartilhada entre 
!                         read_mbufr e outras subrotinas tais como message_pos  
! 20180315  V4.6.1  SHSF - Separado secao 0 de info123. Mudado para info0.  
!                        - Criado interface para messagepos
!                        - Introducao de defesa contra realocacao de memoria  
! 20180522 V4.6.1  SHSF - Incluido defesa contra realocacao de memoria para desc_sec3
! 20180626         SHSF - Incluido subrotine "write_header" (subroutine to write telecommunication header) 
! 20181102         SHSF - Detectado um bug em read_info123 quando secao 2 BUFR esta presente. O problema foi contornado
!                         reposicionado o inicio o registro de leitura para leitura da secao 3
! 20190201 V4.6.2  SHSF - Alguns headers de comunicacao nao era dectardo corretamente deixando o programa em looping. 
!                         Agora a palavra BUFR enserra a detecçao do header. 
! 20190429 V4.6.3 SHSF -  Aumentando numero de descritores maximos para tipo 11 (atualizacao de tabela BUFR). Nota: os campos de texto definidos por 2-05-yyy sao pulados, porem nao sao lidos
!                         E naturalmente pulado por getoctets que nao permite dados acima do idigits. <-- Necessario rever isto
! 20190429 V4.6.3 SHSF -  Added defense in the replicdesc routine regarding the number of replications that exceed the space allocated in memory 
! 20190411 V4.6.4 SHSF -  The  problem regarding precision of the values in write_mbufr and put_oct subroutine was fixed 
! 20200919 V4.6.5 SHSF -  Revision of MESSAGEPOS1 subroutine; Verification of invalid version of master table - Master table versio =0 or missing
! 20210127 V4.6.6 SHSF -  Revision of function CINT to consider associated field by the descriptor 2-04-yyy
! 20220623 V4.6.7 SHSF - Inclusion of the function FIND_MESSAGES_MBUFR
! 20220930        SHSF - Inclusion of subroutines: WRITE_HEADER2 and WRITE_END_OF_MESSAGE
! 20221211 V4.6.8 SHSF - Consider marker operator 2-24-255 but need review
! 20230602 V4.6.9 SHSF - Consider marker operator 2-24-255 but need review. A warning in case of VerMasterTable=missing was added
! 20230605 V4.7.0 SHSF - The operator 2-03-yyy has been revised
! 20240609 V4.7.1 SHSF - readsec4cmp has been revised
!---------------------------------------------------------------------------------------------------------------- 
   IMPLICIT NONE    ! Todas as variaveis serao declaradas
   
   PRIVATE          ! Todas as Variaveis, estruturas e subrotinas 
                    ! serao, em principio, privadas
 !{ Declara como PUBLICAS as seguintes  subrotinas 
   PUBLIC INIT_MBUFR
   PUBLIC OPEN_MBUFR
   PUBLIC WRITE_MBUFR
   PUBLIC READ_MBUFR
   PUBLIC CLOSE_MBUFR
   PUBLIC READBIN_MBUFR 
   PUBLIC GET_NAME_MBUFR
   PUBLIC deallocate_mbufr
   PUBLIC MESSAGEPOS_MBUFR
   PUBLIC FIND_MESSAGES_MBUFR
   PUBLIC SETPOS_MBUFR
   PUBLIC undef             ! Function: It returns the undef value used by this module
   PUBLIC WRITE_HEADER
   PUBLIC WRITE_END_OF_MESSAGE
!}
!{ Declara como PUBLICAS os seguintes tipos de estruturas
   PUBLIC sec1TYPE
   PUBLIC sec2TYPE
   PUBLIC sec3TYPE 
   PUBLIC sec4TYPE
   PUBLIC octTYPE
   PUBLIC IOERR
   PUBLIC exmsgTYPE
   PUBLIC selectTYPE
   PUBLIC any
   PUBLIC none
   PUBLIC MBUFR_VERSION
   PUBLIC Current_TabB_MBUFR
   PUBLIC INTK,REALK
   PUBLIC currentRGMAX
   
    
 
	 

  
  !-----------------------------------------------------------------------------
  ! PARAMETROS DE PRECISAO 
  !----------------------------------------------------------------------------
  ! realk (real Kind number ) E utilizado na precisao dos valores reais       !
  !       decodificados ou para a codificao                                   !
  ! intk  (integer kind number ) E utilizado nas conversoes  binarias         !
  ! intk2 (= intk*2) E utilizado em vmax_numbits (rotinas que precisam de um  !
  !        numero de bits maior que a precisao dupla - (Precisao quadrupla)      
  ! Nota:O Compilador PGF90 nao aceita precisao quadrupla
  !      Nos casos de compilacao com PFG90 use intk2=8  
  !---------------------------------------------------------------------------!
  
  INTEGER,PARAMETER :: realk = 8 !< Real Kind number:precision of real variables used in the decoding or  encoding process
  INTEGER,PARAMETER :: intk  = 8 !< used in binary conversions   
  INTEGER,PARAMETER :: intk2 = 16
 !INTEGER,PARAMETER :: intk2 = 8  
  integer,parameter :: currentRGMax=2100000000
  INTEGER           :: IDIGITS !< Num. digitos da mantissa das var. kind=intk
  INTEGER           :: RDIGITS !< Num. digitos da mantissa das var. kind=realk
  INTEGER           :: MAXEXP
  
 !>Definicao dos tipos PRIVADOS DE DADOS    
    TYPE exmsgTYPE
      INTEGER::bTYPE
      INTEGER::bsubTYPE
    END TYPE 
 
    TYPE selectTYPE
      INTEGER::bTYPE
      INTEGER::bsubTYPE
      INTEGER::center
    END TYPE


    type TABNAME
      INTEGER::nummastertab       !Numero da tabela mestre
      INTEGER::centre             !Codigo do Centro gerador
      INTEGER::verloctab          !Versao da tabela local 
      INTEGER::vermastertab       !Versao da tabela Mestre  
    end type

!{ Internal representation for a BUFR descriptor (Representacao interna para um descritor BUFR) 
   TYPE descbufr 
     INTEGER    :: F !F*-XX-YYY
     INTEGER    :: X !F-XX*-YYY 
     INTEGER    :: y !F-XX-YYY*
     INTEGER*2  :: i ! = 0 :Numeric value; > 0: Character 
     LOGICAL    :: n ! .true. indicate substitution by 2-25-255
     LOGICAL    :: K ! .true. indicate first son, .false. brother
     INTEGER*2  :: a !  Indicate size of associated field  by 2-04-yyy
     logical    :: af ! .true. There is a field associated       
   END TYPE
!}  
   TYPE tabcparm
      INTEGER :: dbits      ! Incremento do numero de bits valido p/ tabb%u=0
      INTEGER :: dscale     ! Incremento do fator de escala p/ tabb%u=0
      INTEGER :: vref       ! Novo valor de referencia 
      INTEGER :: nlocalbits ! Numero de bits de um descritor local inserido por 2-06-yyy
      INTEGER :: ccitt5     ! Numero de caracteres inseridos (caracters do alfabeto 5) 
      INTEGER :: multref    ! Multiplicador para valor de referencia
      INTEGER :: assocbits  ! Numero de bits do campo associado por 2-04-yyy
      integer :: vrefbits   ! Numero de bits do vref introduzido por 2-03-yyy
   END TYPE
!}
  
 !> Section 4 data type 
 TYPE sec4TYPE
    REAL(kind=realk),pointer::r(:,:) !< Valor REAL  (1:nVars,1:nSubsets) -->(Usar este campo quando a variavel for do tipo numerico)
    INTEGER*2,pointer::c(:,:)        !< Numero do caracter ou subdescritor(1:nvar,1:nsubsets) (usar este campo quando for do tipo ASCII)
    INTEGER,pointer  ::d(:,:)        !< Descriptor (1:nvars,1:nsubsets)
    logical,pointer  ::k(:,:)        !< Kinship =1 (first Son/ primeiro filho); =2 (brother/irmao)
    integer*2,pointer::a(:,:)        !< Associated field
    INTEGER          ::nvars
 END TYPE
 !> Section 3 data type 
    TYPE sec3TYPE
      INTEGER,pointer::d(:)
      INTEGER :: ndesc
      INTEGER :: nsubsets
      INTEGER :: is_cpk
      INTEGER :: is_obs
      INTEGER :: is_tac
    END TYPE
 !> Section 1 Type
    TYPE sec1TYPE
      INTEGER :: bTYPE
      INTEGER :: bsubTYPE
      INTEGER :: intbsubtype
      INTEGER :: center
      INTEGER :: subcenter
      INTEGER :: update
      INTEGER :: year
      INTEGER :: month
      INTEGER :: day
      INTEGER :: hour
      INTEGER :: minute
      INTEGER :: second
      INTEGER :: NumMasterTable
      INTEGER :: VerMasterTable
      INTEGER :: VerLocalTable
      LOGICAL :: sec2present
   END TYPE
    TYPE sec2TYPE
      CHARACTER(len=1),pointer:: oct(:)
      INTEGER::nocts
   END TYPE
    TYPE octTYPE
      CHARACTER(len=1),pointer:: oct(:)
      INTEGER::nocts
    END TYPE  

  !{ Definicao  estrutura PRIVATIVA para armazenar tabelas BUFR 
    TYPE bufrtableTYPE     
      REAL    :: scale ! Fator de Escala 
      REAL    :: refv  !.Valor de Referencia 
      INTEGER :: nbits !.Numero de Bits
      INTEGER*2 :: u   !{Tipo de Unidade do descritor
                       ! u=0 : Unidade Fisica ou Valor Numerico
                       ! u=1 : Caractere
                       ! u=3 : Flagtable
                       ! u=4 : CodeTable
                       !}
     CHARACTER(len=88)::txt ! Descricao e unidades 
   END TYPE
  !}

!{ ALOCACAO DAS VARIAVEIS PUBLICAS
   INTEGER,DIMENSION(1:99)::IOERR  
        ! IOERR(un)=IOSTAT da leitura de um arquivo na unidade (UN)
        ! Se IOERR(un)/=0 houve um erro de leitura (provavel fim de arquivo)
        ! 
        ! Nota: O  Fortran padrao nao possui um comando especifico  para detectar "fins de arquivos"
        !       em arquivos de acesso direto. Para contornar o problema, foi adotado
        !       a estrategia de  verificacao do parametro IOSTAT, do comando READ. 
        !       O IOSTAT retorna 0 quando a leitura de um registro e bem sucedida. Quando
        !       se tenta ascessar um registro superior ao fim do arquivo, IOSTAT retorna um 
        !       codigo de erro, que varia de compilador para compilador. Desta forma, 
        !       a melhor solucao encontrada para detectar o fim do arquivo e verIFicar
        !       IOSTAT/=0, isto e, verIFicar se IOERR(UN)=/0 
 
 
!}

!{ ALOCACAO DAS VARIAVEIS GLOBAIS E PRIVATIVAS DO MODULO
   character(len=256),dimension(1:99)          ::BUFR_filename      !Nome do arquivo BUFR  
   INTEGER                                     ::currentRG          !Posicao Corrente no arquivo BUFR
   INTEGER                                     ::RGINI              !Posicao da secao 0 corrente
   INTEGER                                     ::IDataRef           !Index for Data Reference associated to 2-35-000 descriptor 
   type(tabname)                               ::cur_tab            !Tabela Corrente ...Tabela Corrente
   type(tabname)                               ::Init_tab           !Tabela Inicial ....Tabela Inicial (padrao) 
   type(tabname)                               ::Decl_tab           !Tabela declarada na mensagem
   type(tabname),dimension(100,2)              ::tablink            !Listagem de tabelas especiais
   type(sec1TYPE)                              ::cur_sec1           !Contem as informacoes da secao1 corrente
   character(len=1024)                         ::Current_TabB_mbufr !Nome da Tabela BUFR Corrente 
   CHARACTER(len=5)                            ::STATION_NUMBER 
   INTEGER                                     ::NMSG               !Numero de mensagens processadas
   INTEGER                                     ::NSBR               !Numero de Subsets lidos
   CHARACTER (len=255)                         ::subname            !Guarda nome da subrotina para fins de DEBUG
   REAL(kind=4)                                ::Null               !valor nulo 
   INTEGER,PARAMETER                           ::none = -99 
   INTEGER,PARAMETER                           ::any=-11
   CHARACTER(len=255)                          ::local_tables       !Local das tabelas BUFR definido na variavel de ambiente MBUFR_TABLES
   TYPE(tabcparm)                              ::tabc
   INTEGER*2                                   ::BUFR_Edition 
   CHARACTER(len=80),DIMENSION(99)             ::erromessage
   integer                                     ::is_cpk
   type(descbufr)                              ::desc_associated
   character(len=40)                           ::cur_header
   integer                                     ::verbose
   logical                                     ::any_associated_field
   logical                                     ::subset_byte_completed
   ! wmo 386 ANEXO iii Alphabet N.8 
   character(len=1),parameter                  ::SOH=char(1) ! Start of Header
   character(len=1),parameter                  ::CR=char(13) ! Carriage Return
   character(len=1),parameter                  ::LF=char(10) ! Line Feed
   character(len=1),parameter                  ::ETX=char(3) ! End of Text
   !}


 !}
 !{ Iso_fortran_env
  integer, parameter :: Error_Unit = 0 
  integer, parameter :: Output_Unit = 6 
  ! integer, parameter :: Character_Storage_Size = 8 
  ! integer, parameter :: File_Storage_Size = 8 
  ! integer, parameter :: Input_Unit = 5 
  ! integer, parameter :: IOSTAT_END = -1 
  ! integer, parameter :: IOSTAT_EOR = -2 
  ! integer, parameter :: Numeric_Storage_Size = 32 
  logical::logfile
!}
  
!{ ALOCACAO DA TABELAS BUFR  B e D (Allocation of BUFR tables B and D)
 ! Nota: As tabelas sao dimensionais em funcao do numero de bits
 ! que ocupa os termos f,x,y dos descritores
 ! f tem 2 bits
 ! x tem 6 bits = 111111 = 63
 ! y tem 8 bits = 11111111 = 255 

   TYPE(bufrtableTYPE),DIMENSION(0:0,0:63,0:255)::tabb   ! Tabela B
   TYPE(descbufr),DIMENSION(3:3,0:63,0:255,700) ::tabd   ! Tabela D 
   INTEGER,DIMENSION(3:3,0:63,0:255)            ::ndtabd ! Numero de elementos p/ cada descritor da tabela D
   integer::ntabs
   integer                                       :: nds4_debug   ! Numero de descritores da secao 4 para debug 
!}

  logical                                         ::autogen_mode
  CHARACTER(LEN=25),PARAMETER                     ::MBUFR_VERSION=" 4.7.1 2024-06-09   "
  logical                                         ::sec4_is_allocated
  logical                                         ::sec3_is_allocated
  logical                                         ::dsec4_is_allocated
  logical                                         ::desc_sec3_is_allocated
  integer                                         ::PRE_ERROR_CODE=0
  
  !> OPEN_MBUFR 
  !! Open a BUFR file and initialize all tables
  interface open_mbufr
    module procedure open1
    module procedure open2
  end interface

  interface write_header
    module procedure write_header1
    module procedure write_header2
  end interface
  
  interface MESSAGEPOS_MBUFR
    module procedure MESSAGEPOS1 !(uni,nm ,pos,sec1,nsubsets,nbytes,header)
    module procedure MESSAGEPOS2 !(uni,nm ,pos)
    module procedure MESSAGEPOS3 !(uni,btype,nm ,pos)
  end interface 
CONTAINS
!> INIT_MBUFR 
!!-----------------------------------------------------------------------------
!! Initialize this module configuration 
! ----------------------------------------------------------------------------
! Chamadas Externas: Sistema_Operacional.getenv	                              !
! Chamadas Internas: INIT_TABB,INIT_TABD,INIT_ERROMESSAGE                     !
!-----------------------------------------------------------------------------!
!> @param verbose_in   Verbosity parameter (0=minimun, 3=maximum)   y
!> @param logfile_in  If .true. save log in a file. 
!! If .false. log in deafout output error
  
subroutine INIT_MBUFR(verbose_in,logfile_in,verMasterTab)
 
   integer,intent(in)::verbose_in 
   logical,intent(in)::logfile_in
   integer,optional,intent(out)::verMasterTab
                                  
   verbose=verbose_in
   logfile=logfile_in
   if (present(verMasterTab)) then
       call readconf(1,"")
       verMasterTab=Cur_tab%VerMasterTab
   end if
end subroutine
! ----------------------------------------------------------------------------!
! SUBROUTINE PUBLICA: MBUFR.OPEN_MBUFR                                  | SHSF!
! ----------------------------------------------------------------------------!
!
! ABRI UM ARQUIVO BUFR e INICIALIZA TODAS AS TABELAS 
! E VARIAVEIS UTILIZADAS PELO MODULO 
!
! ----------------------------------------------------------------------------!
! Chamadas Externas: Sistema_Operacional.getenv	                              !
! Chamadas Internas: INIT_TABB,INIT_TABD,INIT_ERROMESSAGE                     !
!-----------------------------------------------------------------------------!

 !> OPEN1 
 !! Open a BUFR file and initialize all tables
SUBROUTINE OPEN1(UN,filename,centre,VMasterTable,VLocalTable,BUFRED)

!{ Variaveis da Interface
   INTEGER,intent(in)                  ::un              !< Input or output logic unit number 
   CHARACTER(len=*),intent(in)         ::filename        !< Filename 

   !{ Variaveis declaradas apenas para manter compatibilidade com a versao anterior
   !  Nao Utilizadas nesta versao!!
     INTEGER,optional,intent(in)::centre 
     INTEGER,optional,intent(in)::VMasterTable
     INTEGER,optional,intent(in)::VLocalTable
     INTEGER,optional,intent(in)::BUFRED 

!}
!{ Variaveis locais
    INTEGER::uni !................................Variaavel auxiliar de UN
    INTEGER::i 
!} 
   uni=un
   call readconf(UNI,"")

    if (present(centre)) Cur_tab%centre=centre !centre_mbufr = centre
    if (present(VMasterTable)) Cur_tab%VerMasterTab=VMasterTable
    if (present(VLocalTable)) Cur_tab%VerLocTab=VLocalTable
    if (present(BUFRED)) BUFR_Edition =BUFRED
    
    
    call open0(un,filename)
    
end subroutine 
! ----------------------------------------------------------------------------!
! SUBROUTINE PUBLICA: MBUFR.OPEN_MBUFR                                  | SHSF!
! ----------------------------------------------------------------------------!
!
! ABRI UM ARQUIVO BUFR e INICIALIZA TODAS AS TABELAS 
! E VARIAVEIS UTILIZADAS PELO MODULO 
!
! ----------------------------------------------------------------------------!
! Chamadas Externas: Sistema_Operacional.getenv	                              !
! Chamadas Internas: INIT_TABB,INIT_TABD,INIT_ERROMESSAGE                     !
!-----------------------------------------------------------------------------!
SUBROUTINE OPEN2(UN,filename,path2tables)

!{ Variaveis da Interface
   INTEGER,intent(in)                  ::un              !.Unidade Logica para gravacao/leitura
   CHARACTER(len=*),intent(in)         ::filename        !.Nome do arquivo
   CHARACTER(len=*),intent(in)::path2tables !Caminho para as tabelas BUFR

!}
!{ Variaveis locais
    INTEGER::uni !................................Variaavel auxiliar de UN
    INTEGER::i 
!} 
   uni=un
  
   call readconf(UNI,path2tables)
   call open0(un,filename)
end subroutine 

! ----------------------------------------------------------------------------!
! SUBROUTINE PUBLICA: MBUFR.OPEN_MBUFR                                  | SHSF!
! ----------------------------------------------------------------------------!
!
! ABRI UM ARQUIVO BUFR e INICIALIZA TODAS AS TABELAS 
! E VARIAVEIS UTILIZADAS PELO MODULO 
!
! ----------------------------------------------------------------------------!
! Chamadas Externas: Sistema_Operacional.getenv	                              !
! Chamadas Internas: INIT_TABB,INIT_TABD,INIT_ERROMESSAGE                     !
!-----------------------------------------------------------------------------!
SUBROUTINE OPEN0(UN,filename)

!{ Variaveis da Interface
   INTEGER,intent(in)                  ::un              !.Unidade Logica para gravacao/leitura
   CHARACTER(len=*),intent(in)         ::filename        !.Nome do arquivo
   
!}
!{ Variaveis locais
    INTEGER::uni !................................Variaavel auxiliar de UN
    INTEGER::i 
    INTEGER(KIND=INTK)::aux1
    REAL(KIND=REALK)::aux2
    null=undef()
!} 
   uni=un
    if ((verbose<0).or.(verbose>3))verbose=3
    currentRG=0
    IDataRef =1
    NMSG     =0
    is_cpk   =0 ! Este e uma bandeira que quando 1 indica que um BUFR compactado
                ! e ao mesmo tempo indica para nao processar subdescritores na 
                !  leitura (isto �provisorio ate ter uma solucao para variaveis
                ! caracteres compactados
   sec4_is_allocated=.false.
   sec3_is_allocated=.false.
   dsec4_is_allocated=.false.	
   desc_sec3_is_allocated=.false.
    !--------------------------------------------------------------------
    ! Inicializa numero de digitos das variaveis kind=intk e kind=realk
    ! para serem utilizadas posteriormeente
    !--------------------------------------------------------------------
    !{
    idigits=digits(aux1)
    rdigits=digits(aux2)
    maxexp=range(aux2)
    !}
    if (verbose>0) then 
    print *,""
    print *,"+-----------+----------------------------------------+"
    print *,"| MBUFR-ADT | Module to encode and decode FM-94 BUFR |" 
    Print *,"|           | SHSF - VERSION",MBUFR_VERSION,"|" 
    print *,"|           | (C) 2005       sergio.ferreira@inpe.br |" 
    print *,"+-----------+----------------------------------------+"
    print *,""
    print *," :MBUFR-ADT: Initial configuration"
    print *,""
    write (*,'(3x,"Generating centre............",i3)')Cur_tab%centre
    write (*,'(3x,"BUFR edition.................",i3)')BUFR_Edition
    write (*,'(3x,"Master Table.................",i3)')Cur_tab%NumMasterTab
    write (*,'(3x,"Version of master table .....",i3)')Cur_tab%VerMasterTab
    write (*,'(3x,"Version of local table.......",i3)')Cur_tab%VerLocTab
    print *,""

    end if
    if (verbose>1) then 
    write (*,'(3x,"Float P.N. - Mantissa(bits)..",i3)')rdigits
    write (*,'(3x,"Float P.N. - Max. Exponet....",i3)')maxexp 
    write (*,'(3x,"Maximum supported size(bits).",i3)')idigits
    print *,""
   
    end if 
    

   !{ Acrescenta barra no final do diretorio local_tables, caso seja necessario
   ! Nesse processo veirifica se o diretorio contem barras do windows ou barra do linux 

    i=len_trim(local_tables)
    if ((local_tables(i:i)/=char(92)).and.(local_tables(i:i)/="/")) then 
      if (index(local_tables,char(92))>0) then 
        local_tables=trim(local_tables)//char(92)
      else
        local_tables=trim(local_tables)//"/"
      end if
    end if
   
    call INIT_TABB(Uni) ! Carrega tabela BUFR  em TABB
    call INIT_TABD(Uni) ! Carrega tabela BUFR  em TABD
    call INIT_ERROMESSAGE
    call INIT_tablink(Uni) !Carrecagar lista de tabelas 
!    write( *,'(2x,":MBUFR-ADT: OPEN ",A, " AS #",I2)')trim(filename), UNI
    open(uni,file=filename,STATUS='unknown',FORM='UNFORMATTED',access='DIRECT',recl=1) 
    BUFR_FILENAME(uni)=filename
 END SUBROUTINE OPEN0




!> WRITE_MBUFR 
!!---------------------------------------------------------------------------
!!  ESTA ROTINA GRAVA UMA MENSAGEM BUFR COMPOSTA POR 6 SECOES (de 0 a 5)       
!!                                                                             
!!  Primeiramente e feito um salto de 8 bytes no arquivo, deixando espaco      
!!  para a secao 0, que sera gravada no final                                  
!!                                                                             
!!  Em seguida sao gravados as secoes de 1 a 5. que possuem os seguintes       
!!  tamanhos                                                                   
!!    secao 1 - 18 Bytes                                                       
!!    secao 2 - Variavel (Optional)                                            
!!    secao 3 - Variavel (DepENDe do Numero de descritores )                   
!!    secao 4 - Variavel (DepENDe das variaveis e do numero de sub-grupos)     
!!    secao 5 - 4 bytes "7777"	                                               
!!                                                                             
!!  Apos a gravacao destas secoes e feito o calculo do tamanho da mensagem     
!!  que ee gravado na secao 0                                                  
!!                                                                             
!!  Alguns testes de tamanho tambem saao REALidados                            
!!                                                                             
!!-----------------------------------------------------------------------------
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:                                                          !
!   savesec1,savesec2,savesec3,savesec4b,savesec4rd,savesec4cmp expanddesc3   !
! ----------------------------------------------------------------------------!
 
SUBROUTINE write_mbufr(un,sec1,sec3,sec4,optsec,debug)
	
 !{ VARIAVEIS DA INTERFACE EXTERNA 
   INTEGER,intent(in)::UN
   TYPE(sec1TYPE)::sec1
   TYPE(sec3TYPE)::sec3
   TYPE(sec4TYPE)::sec4
   TYPE(sec2TYPE),optional:: optsec
   logical,optional::debug
 !}
 
  !{  DEFINICOES DE VARIAVEIS LOCAIS
 
   !Aqui sao declarados os vetores que contem os descritores compactos para a secao3
   !(desc_sec3) e os descritores expandidos para a secao 4(desc_sec4). 
   !
   ! O tamanho de desc_sec3 e igual a ndesc, pois os descritores compacto, sao 
   ! os mesmos que sao fornecidos pelo programa principal
   !
   ! O tamanho de desc_sec4 pode ser  bem maior que ndesc. DepENDe da utilizacao 
   ! de replicadores. Note que o  numero de descritores expandidos tem que 
   ! ser igual ao numero de variaveis fornecidas pelo programa principal, contudo 
   ! por seguranca declaramos a dimensao de desc_sec3 como nvars+100.
   !
   !{
    TYPE(descbufr),pointer,DIMENSION(:)::desc_sec3   ! Descritores da secao3 (ndesc)
    TYPE(descbufr),pointer,DIMENSION(:)::desc_sec4	 ! Descritores expandidos (secao 4)
   ! }

   ! Declaracao das demais variaveis locais 
   !{

   INTEGER          ::Tam_BUFR !-Tamanho total do arquivo BUFR (grav. na secao 1)
   INTEGER          ::Tam_sec1 !-Numero de bytes da secao 1
   INTEGER          ::Tam_sec2 !-Numero de bytes da secao 2
   INTEGER          ::Tam_sec3 !-Numero de bytes da secao 3
   INTEGER          ::Tam_sec4 !-Numero de bytes da secao 4
   INTEGER          :: auxi,auxi2       !  Variavel auxiliar para numeros inteiros
   CHARACTER(len=6) ::auxc !  Variavel auxiliar para caracteres
   INTEGER :: err          !- Codigo de erro 
   INTEGER :: ndesc_sec4   !- Numeto de descritores para secao4
   INTEGER :: IFinal       !- Numero de descritores expandidos ate o primeiro replicador atrasado. 
   INTEGER :: ndxmax       !  Numero maximo de descritores da secao 4
   INTEGER :: UNI          !  Numero da Unidade Loogica de Gravacaao 
   INTEGER :: aerr
   INTEGER :: RG0,RGF,NRG  ! RG de Inicio, Fim e Numero de RG de uma 
                         ! Mensagem BUFR
   INTEGER :: RG2          ! Registro de Inicio da Secao 2
   LOGICAL :: Delayed_rep  ! Se verdadeiro Replicador pos-posto e usado  
   !}
  
  !} ----------------------------------------------------------------------
  
    
  !{  Inicializacao  de variaveis
    UNI=UN
    if (sec4%nvars<=0) sec4%nvars=size(sec4%r,1)
    ndxmax=sec4%nvars*2
    ndesc_sec4=0
    delayed_rep=.false.
 
    ! Verifica se opcao de debug de dados de entrada foi selecionada
    if (present(debug)) then 
      nds4_debug  = size(sec4%d,1)
    else
      nds4_debug=0
    end if
     

  if (sec3%nsubsets==0) then
	 autogen_mode=.true.
	 sec3%nsubsets=1
	 ndxmax=6000
	 sec1%vermastertable=init_tab%vermastertab
	 sec1%center        =init_tab%centre
	 sec1%verlocaltable =init_tab%verloctab
	 sec1%nummastertable  =init_tab%nummastertab
	 
    elseif(nds4_debug>0) then 
        autogen_mode  =.false.
	else
	 autogen_mode=.false.
	  allocate(SEC4%d(1:ndxmax,sec3%nsubsets),STAT=ERR)
	end if
		 

	allocate(desc_sec3(1:sec3%ndesc),STAT=aerr)
		IF(aerr>0) THEN 
			print *,":MBUFR-ADT:Erro in the memory alocation for secao 3"
                        print *,":MBUFR-ADT: sec3%ndesc=",sec3%ndesc
			stop
		END IF
		desc_sec3_is_allocated=.true.
	allocate(desc_sec4(1:ndxmax),STAT=aerr)
		IF(aerr>0)  THEN 
			print *,":MBUFR-ADT:Erro in the memory alocation for secao 3"
                         print *,":MBUFR-ADT: sec4%nvars=",sec4%nvars
			stop
		END IF
  !} 



  !{Expansao dos descritores:
  !----------------------------------------------------------------------------
  !   Copia dos descritores da secao 3 e expansao dos mesmos para os 
  !   os descritores da secao 4 
  !----------------------------------------------------------------------------

   do auxi=1,sec3%ndesc
    write(auxc,'(i6.0)')sec3%d(auxi)
    read(auxc,'(i1,i2,i3)')desc_sec3(auxi)%f,desc_sec3(auxi)%x,desc_sec3(auxi)%y
   END do
  
  ndesc_sec4=0
  IFinal=0
  desc_sec4(:)%i=0  ! Em principio nenhum descritor e do tipo caracter
  desc_sec4(:)%n=.false. 
  desc_sec4(:)%k=.false.
 

  call expanddesc3(desc_sec3,sec3%ndesc,ndxmax,desc_sec4,ndesc_sec4,IFinal,err) 
  !print *,"ndesc_sec4",ndesc_sec4
  !do auxi=1,ndesc_sec4
  !print *,desc_sec4(auxi)%f,desc_sec4(auxi)%X,desc_sec4(auxi)%y,desc_sec4(auxi)%a,sec4%r(auxi,1)
  !end do
  !stop
  !print *,"nds4_debug",nds4_debug  
  if (nds4_debug>0) call chksec4_descriptores (desc_sec4,ndesc_sec4,ifinal,sec4,1,err) 

  IF (err/=0) THEN 
    print *,"Erro 53! Invalid Descriptor "
    stop
  END IF

!Caso este operando em modo autogen gera os dados missing na secao 4
!{   
   if (autogen_mode) call sec4gen(sec4,desc_sec4,ndesc_sec4)
!}
 

!{ Verifica uso de replicador atrasado (deleayed replicator) e consistencia de
!  numero de variaveis e numero de descritores  
   IF (IFinal/=ndesc_sec4) THEN
    delayed_rep=.true.
    if (sec3%is_cpk>0) then 
        sec3%is_cpk=0  ! Modo compactado nao e utilizado com  replicador atrasado
        if (verbose==3) print *," :MBUFR-ADT: WARNING: Compressed mode was disabled due to the delayed replicators"
    end if
   ELSE 
    IF(sec4%nvars/=ndesc_sec4) THEN 
      print *,"Error! The numberes of descriptors and The numbers of variables are incompatible"
      print *,"    Expected variables = ",ndesc_sec4
      print *,"    Provided variables = ",sec4%nvars
      call close_mbufr(un)     
     stop
    END IF
   END IF
 !}

 !{ Se numero de subsets menor que 3 nao usar modo comprimido   
  IF ((sec3%is_cpk==1).and.(sec3%nsubsets<3)) THEN
  	if (verbose==3) print *,"Warning ! Using uncompressed bufr" 
	sec3%is_cpk=0
  END IF
 !} 


 !{ Se IdObs for fornecido entao a secao 2 e utilizada 
   
   IF (present(optsec))  THEN
     sec1%sec2present=.true.

   ELSE 
     sec1%sec2present=.false.
   END IF
 !}

 !{ Salva Secoes de 1 a 5

  RG0=currentRG                !Guarda o Registro da fim da mensagem anterior 
  currentRG=RG0+8              !Salta para fim da secao 0 

   call savesec1(un,sec1,tam_sec1)   
  
  !{ Salva a Secao 2 se savesec2_mbufr=.true.    
   
     IF (sec1%sec2present) THEN
	   RG2=currentRg           ! Guarda a Posicao da secao2  	   
	   call savesec2(un,optsec,tam_sec2)                         	  
	 ELSE
	   tam_sec2=0
	 END IF  
  
  !}
  !{  Salva a secao 3	

    call savesec3(un,desc_sec3,sec3%ndesc,sec3%nsubsets,sec3%is_cpk,sec3%is_obs,Tam_Sec3) 
    SUBNAME="write_mbufr"
	IF (delayed_rep) THEN
		call savesec4rd(UN,desc_sec3,sec3%ndesc,sec4,sec3%nsubsets,sec4%nvars,tam_sec4)
	ELSE
	IF (sec3%is_cpk==0) THEN 
		call savesec4(un,desc_sec4,sec4%nvars,sec4%r,sec3%nsubsets,Tam_Sec4) 
		ELSE
	    call savesec4cmp(un,desc_sec4,sec4%nvars,sec4%r,sec3%nsubsets,Tam_Sec4)
	END IF
	END IF
    call savesec5(un)
  !}

  RGF=CurrentRG ! Guarda a posicao do registro do fim da mensagem

!c){ Verificacao de tamanho
!------------------------------------------------------------------------------
!  Calcula o Tamanho do mensagem e verIFica se o tamanho estaao coerente com
!  a posicao do registro corrente (currentRG)
!------------------------------------------------------------------------------ 
  Tam_bufr=8+tam_sec1+tam_sec2+Tam_sec3+Tam_sec4+4
 
  NRG=RGF-RG0
  IF (Tam_bufr/=NRG) THEN 
    print *, "Erro in the SAVE_MBUFR subroutine"
	print *, ""
	close(un)
	stop
  END IF 
!}
!{ GRAVACAO DA SECAO 0
! ----------------------------------------------------------------------------
!  Grava a secao 0 da mensagem corrente e  reposciona o curentRG para gravacao
!  da proxima mensagem
!-----------------------------------------------------------------------------
 
   currentRG=RG0               !Reposiciona registro do inicio da mensagem
   call savesec0(un,Tam_Bufr)  !Salva a secao 0 

 
   currentRG=RGF               !Reposiciona registro no fim da mensagem 
   NMSG=NMSG+1	               !Incrementa o numero de mensagens gravadas
 
!}
 
 
 
 END SUBROUTINE WRITE_MBUFR



! ----------------------------------------------------------------------------!
! SUBROUTINE PUBLICA: MBUFR.CLOSE_MBUFR                                 | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!	FECHA UM ARQUIVO ABERTO POR OPEM_MBUFR                                !
! ----------------------------------------------------------------------------!	
! Chamdas Externas: Nao Ha	                                              !
! Chamadas Internas:Nao Ha                                                    !
! ----------------------------------------------------------------------------!
! HISTORICO:                                                                  !
!	Versao Original: Sergio H. S. Ferreira                                !
!_____________________________________________________________________________!
 

SUBROUTINE CLOSE_MBUFR(UN)
  INTEGER,intent(in):: UN
       IF (verbose>0) then 
	Print *," :MBUFR-ADT: Number of messages=",NMSG
	print *," :MBUFR-ADT: Size=", currentRG," Bytes"
	print *," :MBUFR-ADT: CLOSE #",UN
       end if
	close (un)

END SUBROUTINE CLOSE_MBUFR

SUBROUTINE deallocate_mbufr(sec3,sec4)
   TYPE(sec3TYPE)::sec3
   TYPE(sec4TYPE)::sec4 
  deallocate (sec4%r,sec4%d,sec4%c,sec4%a,sec4%k)
  deallocate (sec3%d)
  sec4_is_allocated=.false.
  sec3_is_allocated=.false.
end subroutine


 ! -----------------------------------------------------------------------------!
 ! SUBROUTINE PRIVADA: MBUFR.PUT_OCTETS                                  | SHSF !
 ! -----------------------------------------------------------------------------!
 !                   SUBROUTINE PRIVADA PUT_OCTETS                              *
 !	                                                                        *
 ! FUNCAO:                                                                      *
 !  COPIA UM VETOR DE VALORES INTEIROS COM DIFERENTES NUMEROS DE BITS PARA UM   *
 !  VETOR DE ELEMENTOS DE 8 BITS (OCTETO)                                       *
 !                                                                              *
 ! UTILIZACAO:                                                                  *
 !                                                                              *
 !  Dentro da arquitetura dos computadores,a unidade numeerica baasica e o Byte *
 !  de 8 bits (OCTETO), de forma que todas as variaveis armazenadas na memooria *
 !  ou gravados em disco, ocupam muultiplos de bytes (8bits).                   *
 !									        *
 !  Uma das formas de REALizar compactacao da informacao em disco, comsiste no  *
 !  reaproveitamento dos bits de uma variaavel, para guadar outras. EE o caso   *
 !  do formato BUFR, que utiliza esta teecnica largamente                       *
 !                                                                              *
 !  Esta subrotina permite que este reaproveitamento seja feito utilizando a    *
 !  seguinte estrutura                                                          *
 !                                                                              *
 !    A_VAL(NA) --> Vetor Inteiro com os NA dados de entrada	                *
 !    A_BITS(NA)--> Vetor Inteiro com o numero de BITS, que deseja-seutilizar   *
 !                  para gravar cada elemento de A_VAL                          *
 !    NA        --> Nuumeor de elementos de A_VAL A_BITS                        *
 !                                                                              *
 !    OCT(NOCT)  <-- Vetor Caracter (de 1 Byte (OCTETO)                         *
 !    NOCT       <-- Nuumero de Elementos de OCT	                        *
 !	  ERR        <-- Zero se a coopia foi bem sucedida                      *
 !   Utilizando esta estrutura cada elemento de A_VAL ee copiado Bit-a-Bit para * 
 !   OCT, de forma aa sempre completar os oito bits de OCT.Os bits que naao     *
 !   cabem em OCT, sao copiados para o prooximo elemento de OCT, de forma que   *
 !   todos os elementos de OCT teraao seus bits completados, com excessaap do   *
 !   uultimo  OCT. Este poderaa, eventualmente ter seus bits incompletos.       *
 !   Neste caso,  err retornaraa o numero de bits faltantes no  uultimo octeto  *
 !                                                                              *
 !   IMPORTANTE.                                                                *
 !                                                                              *
 !      1- Certifique-se que o Ultimo OCT tenha bits completos, fornecendo      *
 !         sempre um conjunto de A_VAL, cuja soma dos bits de todos os          *
 !         elementos sejam multiplos de 8                                       *
 !                                                                              *
 !      2- Esta rotina esta preparada para copiar elementos de A_VAl que nao    *
 !         exceda o tamanho de 64 bits. No caso deste tamanho ser excedido      *
 !         poderaao ocorrer erro de DIMENSIONamento da matriz local BIT         *
 !                                                                              *
 !      3- Esta rotina, pressupoe que os bits fornecidos em A_BITS(:), sao      *
 !         realmente suficientes para guardar os valores A_VAL(:). Caso seja    *
 !         insuficiente, os bits mais signIFicativos seraao cortados.           *
 !         Esta e uma parte que precisa ser melhorada. Poderia ser colocado     *
 !         algum teste para verificar e enviar uma mensagem de erro!            *
 !                                                                              *
 !                                                                              *
 !   EXEMPLO:                                                                   *
 !             Neste Exemplo sao fornecidos 5 Valores A_VAL, cada um com res-   *
 !             pectivamente 14, 3, 7, 5 e 4 bits.  Neste exemplo esta rotina    *
 !             retorna 5 octetos (OCT), que contem os bits de A_VAL redistri-   *
 !             buidos conforme o esquema abaixo                                 *
 !                                                                              *
 !   A_VAL    11111111111111 000  1010101 00000  1111--+                        *
 !            \---+--/\---+---/\----+---/ \----+---/   |                        *
 !                |       |         |          |       |                        *
 !            +------+ +------+  +------+   +------+   |                        *
 !            |      | |      |  |      |   |      |   |                        *
 !  OCT       11111111 11111100  01010101   00000111   10000000                 *
 !                                                                              *
 !                                                                              *
 !            Neste caso, os 5 Elementos de A_VAL sao copiados em 5 octetos     *
 !	      Contudo no Octeto 5, somente 1 bit e utilizado. Os demais 7bits       *
 !            ficam com zero. Por isto, neste caso e retornado  err=7           *
 !                                                                              *
 !                                                                              *
 !*******************************************************************************
 ! -----------------------------------------------------------------------------!
 ! Chamadas Externas: Nao Ha                                                     !
 ! Chamadas Internas:Nao Ha                                                     !
 ! -----------------------------------------------------------------------------!


 SUBROUTINE PUT_OCTETS(A_VAL,A_BITS,NA,OCT,NOCt,err)

 !{ Variaveis da Interface
 INTEGER(kind=intk),DIMENSION(:),intent(inout)::A_VAL  ! Conjunto de valores de entrada com bits dIFerentes
 INTEGER,DIMENSION(:),              intent(in)::A_BITS ! Vetor com o numero de bits do conjunto  A_VAL
 INTEGER,                           intent(in)::NA     ! Numero de elementos de A_VAL e A_BITS
 CHARACTER(len=1),DIMENSION(:),    intent(out)::OCT    ! Vetor caractere com octetos de saida
 INTEGER,                          INTENT(OUT)::nOCT   ! Numero de elementos de OCT
 INTEGER,                          INTENT(OUT)::err    ! Indica se o ultmo octeto foi completado corretamente.
                                                       ! err =  0     :Todos os bits foram completados
                                                       ! err de 1 a 7 :Numero de bits que faltaram  
 !}
 !{ Variaveis locais
 												  
   INTEGER,DIMENSION(8,NA*10)::BIT  ! Array auxiliar para redistribuicao de bits
   INTEGER:: k,j,i,n
   INTEGER(kind=intk) ::valmax
   INTEGER::V
   CHARACTER(len=1)::aux
!}

!{ redistribuir os bits da variavel A_VAL em octetos (Vetor BIT)
  K=0
  j=1
  BIT(1:8,J)=0               ! Zera os bits do primeiro octeto
  DO N=1,NA
  
    !{VerIFicar se o numero  de bits estao  dentro do esperado
    	
    IF (A_BITS(n)>idigits) THEN 
	  PRINT *,"Error in"//TRIM(SUBNAME)//"_copy2oct !"
	  PRINT *, "Possible error on the specification of variable n  ",N
	  print *, "Number of bits=",A_BITS(n)
	  stop
	END IF
	!}
	!VerIFicar se o valor fornecido ee compatiivel  com
	!o nuumero de  bits fornecidos ou se ee valor missing
	!{
	  
	  valmax=vmax_numbits(A_BITS(n))
	!  print *,A_VAL(n),valmax  
	  IF (A_VAL(n)==null) A_VAL(n)=valmax
	  
	  IF (((A_VAL(n)<0).or.(A_VAL(n)>valmax)).and.valmax>0) THEN
		
		IF ((index(SUBNAME,"SAVESEC4")>0).or.(index(SUBNAME,"SAVESEC1")>0))  THEN 
		  A_VAL(n)=valmax
		ELSE
	  	  PRINT *,"Error in "//TRIM(SUBNAME)//"_put_octets !"
		  write(*,500)N,A_VAL(n),A_BITS(n)
500		  FORMAT("    Variable(",I6.6,") Value =",I11,"  Number of bits = ",I11)		
		  STOP
		ENDIF
	  END IF
	  
    DO I=1,A_BITS(N)		
      K=K+1
    	 IF (K>8) THEN
	     J=J+1;K=1
	     BIT(1:8,J)=0         ! Zera os bits para armazenar o proximo octeto 
	  END IF
	 BIT(9-K,J)=IBITS(A_VAL(N),A_BITS(N)-i,1)
   END DO
 END DO
 !} Fim da redistribuicao
 NOCT=J
!{ Converter os octetos BIT para valores decimal(V) e para variavel string (oct)
if (noct>ubound(oct,1)) then 
	print *,"Error in "//TRIM(SUBNAME)//"_put_octets !"
	stop
end if
do j=1,noct
  v = 0 
  do i = 1,8
    v = BIT(I,J)*2 ** (i-1) + v    !
  END do
  aux=char(v)
  oct(j)=aux
END do
!} Fim da conversao para string
 
 !err=0 Indica que o ultimo octeto foi completado corretamente
 ! caso contrario, err indica o numero de bits que faltarem para
 ! completar o ultimo octeto  
 !{
   err=8-k
  !}
 END SUBROUTINE	PUT_OCTETS






! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC0                                    | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!   Esta Subrotina grava a Secao 0 de uma mensagem BUFR
!
!
!    As Variaveis de entrada sao
!      UN         Unidade Logica de Gravacao
!      Tam_BUFR   Tamanho da Mensagem  
!
!  
!    Nesta secao sao gravados as seguintes informacoes:
!     a) A palabra "BUFR"  nos  4 primeiros Bytes  
!     b) A Variavel Tam_BUFR com 3 Bytes 
!     c) O Numero da edicao do codigo BUFR (1Byte)
!
!    Obsercoes 
! 
!    1 - Note que Tam_BUFR poderia assumir valores de atee 16777213 , que ee o maior
!        inteiro positivo com 24 bits.  Contudo, por convencao, uma mensagem
!        BUFR podera ter no maximo 10000 Bytes. Desta forma, esta sub-rotina 
!        imprime na tela um aviso quando for ultrapassado  o limite de 10000.  
!        No caso de tentativa de gravar uma mensagem BUFR acima de 16777213 uma 
!        mensagem de erro e mostrado e a gravacao e cancelada    
! 
!    2 - Variaveis internas Importantes
!        A()    E um vetor de valores inteiros de 4 bytes
!        B()    E um vetor de numero de bits utilizados para representar cada 
!               valor de A()
!        sec0() E o vetor CHARACTER que armazena os octetos para gravacao da 
!               secao 0. Este vetor ee obtido a partir da copia sequencial 
!               bit-a-bit de cada valor de A(), atraves da sub-rotina copy2oct
!-----------------------------------------------------------------------------! 
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:PUT_OCTETS                                                !
! ----------------------------------------------------------------------------!

 SUBROUTINE SAVESEC0(UN,Tam_BUFR)
   
	INTEGER, intent(in)::UN                ! Unidade loogica
	INTEGER, intent(in)::Tam_BUFR          ! Tamanho da mensagem BUFR
   
 !{Variaveis Internas 
	CHARACTER(len=1),DIMENSION(8)::sec0  ! Vetor de octetos	 (1byte)
	INTEGER :: uni,err,noct,i	     ! Outras Variaveis Auxiliares 
	INTEGER(KIND=INTK),DIMENSION(6)::A   ! Vetor de Valores Inteiros 4 bytes a ser convertidos
	INTEGER,           DIMENSION(6)::B   ! Vetor de Valores Inteiros 4 bytes a ser convertidos
 !}
                                             
   uni=un	
  
   SUBNAME="SAVESEC0"   ! Para fins de controle de erros
   !{ Verifica erro no tamanho da mensagem
	IF (Tam_BUFR > 16777213) THEN 
	  
	  print *, "Erro in MBUFR"//SUBNAME
	  print *, "Menssage too big. Size= ", Tam_BUFR
	  print *, "Numeber of Menssage =",NMSG
	  print *, ""
	  close(uni)
	  stop 
    END IF
	
	IF ((Tam_BUFR > 10000 ).and.(verbose==3)) THEN
	  print *," Warning ! Size of message exceeded 10 KBytes "
	  print *," Menssage =", NMSG
	  print *," Size = ", Tam_BUFR
	  print *,""
	END IF
   !}
   
   !{ Prepara para gravar secao 0
	B(1)= 8;A(1)=ichar("B")
	B(2)= 8;A(2)=ichar("U")
	B(3)= 8;A(3)=ichar("F")
	B(4)= 8;A(4)=ichar("R")
    B(5)=24;A(5)=Tam_BUFR       ! Tamanho total do arquivo
    B(6)= 8;A(6)=BUFR_EDITION  ! BUFR edicao = 3 OU 4
 
 !{Organiza os dados da secao 0 em octetos (sec0)
 ! Neste caso o numero de octeros noct tem que ser sempre 8
   call PUT_OCTETS(A,B,6,sec0,noct,err)  
   IF ((noct/=8).or.(err/=0)) THEN 
     print *,"Error in the section 0 especificatios: numeber of octets =",noct
	 print *,""
	 close(uni)
	 stop
   END IF
 !}  
   
 !{Inverte a ordem dos octetos 5,6 e 7 para 7,6 e 5
 
 
 !}
 !{ Grava a secao 0
   do i=1,8
    currentRG=currentRG+1
    write (uni,rec=currentRG) sec0(i)			 
   END do
  !}
 END SUBROUTINE SAVESEC0



! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC1                                    | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!	DEFINE E GRAVA A SECAO 1                                              !
!                                                                             !
!-----------------------------------------------------------------------------!																  !
! Chamdas Externas: Nao Ha				                      !
! Chamadas Internas:PUT_OCTETS                                                !
! ----------------------------------------------------------------------------!
! HISTORICO:						                      !
!	Versao Original: Sergio H. S. Ferreira                                !
!_____________________________________________________________________________!
   
SUBROUTINE SAVESEC1(UN,sec1in,NUM_OCTETS)
  !{ Variaveis da Interface
  INTEGER,          intent(in)   ::UN
  TYPE(sec1TYPE),   intent(inout)::sec1in
  integer,          intent(inout)::NUM_OCTETS  !Numero de octetos (tamanho) da secao 1
  !}
  !{ Variaveis locais
  
  CHARACTER(len=1),DIMENSION(24)  :: sec1         !Para gravar secao 1 de ate 23 bytes 
  INTEGER                         :: uni,noct,err,I
  INTEGER(kind=intk),DIMENSION(19):: A            !Vetor de Valores e Numero de bits de cada valor
  INTEGER,           DIMENSION(19):: B            !Vetor de Valores e Numero de bits de cada valor
  INTEGER                         :: NUM_ELEMENTS !Numero de elementos em A() E B()
  type(sec1type)                  :: sec1e
  !}	  
  uni=un
  SUBNAME="SAVESEC1"
 !----------------------------------------------------
 ! use default values  in the case of master table =0
 !----------------------------------------------------
 !{ 

  sec1e=sec1in
  if (sec1e%VerMasterTable==0) then 
    sec1e%center=init_tab%centre
    sec1e%VerMasterTable=init_tab%VerMasterTab
    sec1e%NumMasterTable=init_tab%NumMasterTab
    sec1e%VerLocalTable=init_tab%VerLocTab
  end if
 !}

  err=check_vertables(sec1e%center,sec1e%NumMasterTable,sec1e%VerMasterTable,sec1e%VerLocalTable)
  if (err>0) stop


  if (BUFR_EDITION<4) THEN 

	BUFR_EDITION=3
	!------------------------------------------------------------------------------
	!Bits   | Values                |Octet| Definitions
	!-----------------------------------------------------------------------------
	B(1)=24;A(1)=18                    ! 1-3 |Lenght of section, in octets
	B(2)=8;A(2)=Cur_tab%NumMasterTab   !  4  |Bufr Master Table number 
	B(3)=8;A(3)=sec1e%subcenter        !   5 |Originating/generating sub-centre
	B(4)=8;A(4)=sec1e%center           !   6 |Originating/generating centre
	B(5)=8;A(5)=sec1e%update           !   7 |Update sequence number 0 for original BUFR messages
	IF (sec1e%sec2present) THEN        !-----|    	
	    B(6)=1; A(6)=1                 !   8 | bit1 =0 No optional secion =1 Optional section included
	ELSE                               !     |
	    B(6)=1; A(6)=0                 !     |
	END IF                             !     |
	B(7)=7;   A(7)=0                   !     | Bits 2-8  set zero (reservate)
	                                   !-----|
	B(8)=8; A(8)=sec1e%bTYPE           !  9  | Data category TYPE (BUFR Table A)  (Byte 9)
	B(9)=8; A(9)=sec1e%bsubTYPE        ! 10  | Data sub-category (Defined by local ADP centres)
	B(10)=8;A(10)=Cur_tab%VerMasterTab ! 11  | Version Number of master table 
	B(11)=8;A(11)=Cur_tab%VerLocTab    ! 12  | Vertion number of local table 
	B(12)=8                            !-----|
	IF (sec1e%year<=2000) THEN         ! 13  | Year of century
	   A(12)= sec1e%year - 1900        !     |
	ELSE                               !     |
	   A(12)=sec1e%year - 2000         !     | 
	END IF                             !-----|
	B(13)=8; A(13)= sec1e%month        ! 14  | Month 
	B(14)=8; A(14)= sec1e%day          ! 15  | Day 
	B(15)=8; A(15)= sec1e%hour         ! 16  | Hour 
	B(16)=8; A(16)= sec1e%minute       ! 17  | Minute 
	B(17)=8; A(17)= 0                  ! 18  | Reservado for local use by ADP centres
 
 	NUM_ELEMENTS=17
	NUM_OCTETS=18
    A(1)=NUM_OCTETS


 ELSE	
	BUFR_EDITION=4
	!-------------------------------------------------------------------------------
	!Bits  ;  Values                     |Octet| Definitions
	!------------------------------------+-----+-----------------------------------
	B(1)=24;  A(1)=24                    ! 1-3 | Lenght of section, in octets
	B(2)=8;   A(2)=Cur_tab%NumMasterTab  !   4 | Bufr Master Table number 
	B(3)=16;  A(3)=sec1e%center          ! 5-6 | Originating/generating center
	B(4)=16;  A(4)=sec1e%subcenter       ! 7-8 | Originating/generating subcenter
	B(5)=8;   A(5)=sec1e%update          !   9 | Update sequence number 0 for original BUFR messages
	IF (sec1e%sec2present) THEN          !-----|    	
	   A(6)=1;A(6)=1                     !  10 | bit1 =0 No optional secion =1 Optional section included
	ELSE                                 !     |
	   B(6)=1;A(6)=0                     !     |
	END IF                               !     |
	B(7)=7;   A(7)=0                     !     | Bits 2-8  set zero (reservate)
	                                     !-----|
	B(8)=8;  A(8) = sec1e%bTYPE          ! 11  | Data category TYPE (BUFR Table A)  (Byte 9)
	B(9)=8;  A(9) = sec1e%intbsubTYPE    ! 12  | Data sub-category (Defined by local ADP centres)
	B(10)=8; A(10)= sec1e%bsubTYPE       ! 13  | Data sub-category local (defined locally by automatic data processing centres)
	B(11)=8; A(11)= Cur_tab%VerMasterTab ! 14  | Version Number of master table 
	B(12)=8; A(12)= Cur_tab%VerLocTab    ! 15  | Vertion number of local table 
	B(13)=16;A(13)= sec1e%year           !16-17| Year
	B(14)=8; A(14)= sec1e%month          ! 18  | Month 
	B(15)=8; A(15)= sec1e%day            ! 19  | Day 
	B(16)=8; A(16)= sec1e%hour           ! 20  | Hour 
	B(17)=8; A(17)= sec1e%minute         ! 21  | Minute 
	B(18)=8; A(18)= sec1e%second         ! 22  | Second 
	B(19)=16;A(19)= 0                    !23-24| Reservado for local use by ADP centres
  	NUM_ELEMENTS=19
	NUM_OCTETS=24
	A(1)=NUM_OCTETS
	
  END IF
	
 ! Organiza os dados da secao 1 em octetos (sec1)

 !{
   call PUT_OCTETS(A,B,NUM_ELEMENTS,sec1,noct,err)  
   IF ((noct/=NUM_OCTETS).or.(err/=0)) THEN 
     print *,"Error in the secion 1 especification: numeber of octets =",noct
	 stop
   END IF
 !}
!Grava os byte da secao 1  (SHSF)
!{
  do i=1,NUM_OCTETS
   currentRG=currentRG+1
   write (uni,rec=currentRG) sec1(i)			 
  END do

!}
END SUBROUTINE SAVESEC1

! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC2                                    | SHSF!
! ----------------------------------------------------------------------------!
!                       													  !
!	GRAVA A SECAO 2                                                           !
! ----------------------------------------------------------------------------!																  !
! Chamdas Externas: Nao Ha													  !
! Chamadas Internas:PUT_OCTETS                                                !
! ----------------------------------------------------------------------------!
! HISTORICO:																  !
!	Versao Original: Sergio H. S. Ferreira									  !
!_____________________________________________________________________________!
   
SUBROUTINE SAVESEC2(UN,sec2,tam_sec2)

!{ Variaveis da Interface
	INTEGER, intent(in)::UN
	TYPE(sec2TYPE),intent(in)::sec2  ! Dados secao 2
	INTEGER,intent(out)::tam_sec2    ! Tamanho secao 2
!}

!{ Variaveis Locais
	INTEGER(kind=intk),DIMENSION(2)::A      ! Vetor de Valores e Numero de bits de cada valor
	INTEGER,           DIMENSION(2)::B      ! Vetor de Valores e Numero de bits de cada valor
	CHARACTER(LEN=1),DIMENSION(4)::sec2cab  ! Cabecalho secao 2
	INTEGER :: noct, i, uni,err
!}  

	SUBNAME="SAVESEC2"
	uni=un
	tam_sec2=sec2%nocts+4

!{ Gravando cabecalho da secao 2 

	B(1)=24; A(1)=tam_sec2     !-Contem o tamanho da secao 2 (Bytes de 1 a 3) que E sempre 18 ?
	B(2)=8 ; A(2)=0            !-Contem a versao da BUFR master table  (Byte 4)
	call PUT_OCTETS(A,B,2,sec2cab,noct,err) 
	if (err>0) then 
		print *,"Erro writing section 2"
		stop
	end if
	
	do i=1,noct
		currentRG=currentRG+1
		write (uni,rec=currentRG) sec2cab(i)			 
	END do
!}

!{ Gravando o restante da secao 2
	
	do i=noct+1,tam_sec2
		currentRG=currentRG+1
		write (uni,rec=currentRG) sec2%oct(i-noct)			 
	END do
 !}
 !{ A secao tem que ter numero par.
	if (mod(tam_sec2,2)/=0) then 
		currentRG=currentRG+1
		write (uni,rec=currentRG) char(0)
	end if			 
	

END SUBROUTINE SAVESEC2

! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC3                                   | SHSF !
! ----------------------------------------------------------------------------!
!                                                                             !
!	DEFINE E GRAVA A SECAO 3:                                             !
!    Esta secao possui tamanho variado: Os  7 primeiros bytes  sao fixos 
!    e permite identificar as caracteristicas da secao. Os demais armazenam os
!    descritores. Cada descritor  ocupam 2 bytes 
! ----------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha                                                   !
! Chamadas Internas:PUT_OCTETS                                                !
! ----------------------------------------------------------------------------!


SUBROUTINE SAVESEC3(UN,D,Ndesc,NSUBSET,is_cmp,is_obs,tam_sec3) 

!{  Interface  
 INTEGER, intent(in)::UN
 TYPE(descbufr),DIMENSION(:),intent(in)::D 	! Descritores BUFR
 INTEGER,intent(in)::Ndesc        ! Numero de descritores
 INTEGER,intent(in)::Nsubset      ! Numero de subsecoes
 INTEGER,intent(in)::is_cmp       ! Indica se eum bufr compactador 0=nao compactado
 INTEGER,intent(in)::is_obs       ! Indica se e um bufr de observacao 
 INTEGER,intent(inout)::tam_sec3  ! tamanho da secao 3
!}

!{  locaL
	CHARACTER(len=1), DIMENSION (2000):: sec3	
	INTEGER(kind=intk),DIMENSION(8000)::A    ! Vetores de Valores  e Numero de bits 
	INTEGER,           DIMENSION(8000)::B    ! Vetores de Valores  e Numero de bits
	INTEGER :: uni	,ib	,i,err,noct,xx
!}

	uni=un
	SUBNAME="SAVESEC3"

 !-------------------------------------------------------------------
 ! Obs.: AUTO-TESTE DE GRAVACAO DA SECAO 3
 ! 
 ! Esta secao devera ter tamanho (Tam_sec3) igual a: 
 ! 7  Bytes que descrevem a secao  + 
 ! 2 vezes o Numero de descritores das variaveis 
 !
 ! Ao final desta sub-rotina e feito uma verificao deste tamanho. 
 ! Caso seja verificado tamanhos diferentes, uma mensagem de erro 
 ! seraa apresentada na tela e o programa interrompido
 !-------------------------------------------------------------------
 
	Tam_sec3 = 7+2*(Ndesc)
 
	IF (mod(Tam_sec3,2)>0 ) Tam_sec3=Tam_sec3+1
	
	B(1)=24;A(1)=Tam_sec3 ! Tamanho da Secao (bites de 1-3)
	b(2)=8;a(2)=0         ! byte reservado   (bite 4)
	b(3)=16;a(3)=Nsubset  ! Numero de data subsets  (observacoes em cada registro  BUFR) byte
	b(4)=1;a(4)=is_obs    ! se 1 Indica dados observacionais
	b(5)=1;a(5)=is_cmp    ! se 1 Indica dados comprimidos
        !if (is_tac==1) then 
        !  b(6)=1;a(6)=1    ! Se 1 Indica dados codificados
        !else 
         b(6)=1;a(6)=0
        !end if  
        b(7)=5;a(7)=0         ! Demais bits do octeto saao 0  (byte 7)

!A partir do  byte 8, comeca a gravacao dos descritores

    ib=7

	do i=1,Ndesc
		ib=ib+1;b(ib)=2;a(ib)=D(I)%F
		ib=ib+1;b(ib)=6;a(ib)=D(I)%X
		ib=ib+1;b(ib)=8;a(ib)=D(i)%Y
	END do
  
  
 !{ Organiza os dados da secao 3 em octetos (sec3)
 
   call PUT_OCTETS(A,B,ib,sec3,noct,err)  
 
    ! Se o numero de octetos naao for par
    ! acrescentar um octeto com zero
    !{
	IF (mod(noct,2)>0) THEN 
		noct=noct	+1
		sec3(noct)=char(0)
	END IF
    !}

	IF ((err/=0).or.(noct/=Tam_sec3)) THEN 
		print *,"Erro in the section 3: number of octets =",NOCT
		stop
   END IF
 !}
   
	xx=ichar(sec3(7))
 
 !} Fim da preparacao para gravacao da secao 3
  
  
 !{ Gravar 
  do i=1,noct
   currentRG=currentRG+1
   write (uni,rec=currentRG) sec3(i)
  END do
!}
!} Fim da Gravacao da secao 3
END SUBROUTINE SAVESEC3






! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC4                                    | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!   DEFINE E GRAVA A SECAO 4 no caso simples (Sem replicadores pos-postos e   !
!    e sem compactacao)                                                       !
! ----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:PUT_OCTETS, TABC_SETPARM,CINT, BITS_TABB2,                !
! ----------------------------------------------------------------------------!



SUBROUTINE SAVESEC4(UN,D,ndesc,v,nsubset,tam_sec4)
 !{ Interface
 TYPE(descbufr),DIMENSION(:),intent(in)::D        ! Descritores BUFR
 INTEGER,                    intent(in)::UN				   !
 INTEGER,                    intent(in)::ndesc    ! Numero de descritores
 REAL(kind=realk),DIMENSION(:,:)       ::v
 INTEGER,                   intent(out)::tam_sec4
 INTEGER,                    intent(in)::nsubset
 !}
 !{Variaveis locais
 INTEGER(kind=intk),DIMENSION((nsubset*(ndesc+5)*4))::A ! Vetor de Valores e Numero de bits de cada valor 
 INTEGER,           DIMENSION((nsubset*(ndesc+5)*4))::B ! Vetor de Valores e Numero de bits de cada valor 
 CHARACTER(len=1),DIMENSION((nsubset*ndesc+5)*16):: sec4 ! Valor anterior e 28 
 CHARACTER(len=1),DIMENSION(4)::auxsec4          
 INTEGER :: uni	,k,err,noct,noctaux ,j  ,i,dimab,dimoct
 !}
 
 uni=un
 SUBNAME="SAVESEC4"
 dimab=(nsubset*(ndesc+5)*4)	  ! Calculo da dimensao de B() e A()
 dimoct=dimab*8	                  ! Estimativa do nuumero de octetos
 
 b(1)=24;a(1)=0                   ! Tamanho da secao 4 Ainda naao ee conhecido 
 b(2)=8; a(2)=0                   ! byte reservado (= 0)
 k=2
! Prepara para gravar todos os subsets (Valors e Indices de confiabilidade) 
! { 
 do i=1,nsubset
	call tabc_setparm(err=err)
  !Valores
  !{
  do j=1, ndesc
    k=k+1
    b(k)=bits_tabb2(d(j))
    a(k)=CINT(v(j,i),d(J))    
  END do
  !}
END do
 ! Organiza os dados da secao 4 em octetos (sec4)
 ! Neste caso o numero de octeros noct tem que ser sempre 52
 !{
   call PUT_OCTETS(A,B,k,sec4,noct,err)  
 ! Se o nuumero de octetos (Secao3+secao4)naao for par
 ! acrescentar um octeto com zero
 !{
     IF (mod(noct,2)>0) THEN 
       noct=noct +1
       sec4(noct)=char(0)
     END IF
    !}
 !}

  ! Agora coloca o tamanho da secao 4
    Tam_Sec4=noct
    B(1)=24;a(1)=Tam_Sec4
   
    call PUT_OCTETS(A,B,1,auxsec4,noctaux,err)
    IF ((noctaux/=3).or.(err/=0)) THEN 
      print *,"erro in the section 4 "
      stop
    END IF
    sec4(3)=auxsec4(3)
    sec4(2)=auxsec4(2)
    sec4(1)=auxsec4(1)
 !} Fim da preparacao para gravacao da secao 4
 !{ Gravar 
  do i=1,noct
   currentRG=currentRG+1
   write (uni,rec=currentRG) sec4(i)
  END do
!}
!} Fim da Gravacao da secao 4
  
END SUBROUTINE SAVESEC4



! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC4RD                                  | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!   DEFINE E GRAVA A SECAO 4 com o uso do replicador pos-posto (delayed repl) !
!    e sem compactacao)      
!   A diferenca desta gravacao para a gravacao normal requer que
!   as estruturas sec3 e sec4 entrem dentro da subrotina diretamente
!   A expansao dos descritores ocorrem aqui dentro                            !
! ----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:PUT_OCTETS, TABC_SETPARM,CINT, BITS_TABB2,                !
! ----------------------------------------------------------------------------!

SUBROUTINE SAVESEC4RD(UN,desc_sec3,ndesc_sec3,sec4e,nsubset,nvarmax,tam_sec4)

!{ Variaveis da interface 
   INTEGER,        intent(in)::UN
   TYPE(descbufr),pointer,DIMENSION(:)::desc_sec3   ! Descritores da secao3 
   INTEGER,       intent(in)          ::ndesc_sec3  ! Numero de descritores da secao 3
   TYPE(sec4TYPE),intent(in)          ::sec4e       ! Secao 4
   INTEGER,       intent(in)          ::nsubset     ! Numero de subsecoes
   INTEGER,       intent(in)          ::nvarmax     ! Numero maximo de variaveis passadas na secao 3
   INTEGER,       intent(out)         ::tam_sec4    ! Tamanho da secao 4 apos gravacao
 
 !}
 !{ Variaveis locais 
   INTEGER                            ::ndesc_sec4     ! Numero de descritores
   INTEGER                            ::IFinal         ! Variavel auxiliar para expanddesc
   INTEGER                            ::fatorR         ! Fator de Replicacao 
   TYPE(descbufr),pointer,DIMENSION(:)::d
   TYPE(descbufr)                     ::auxdesc        ! Variavel auxiliar para descritores
   INTEGER(kind=intk),DIMENSION((nsubset*(nvarmax*2+5)*4))::A   ! Vetor de Valores e Numero de bits de cada valor 
   INTEGER,           DIMENSION((nsubset*(nvarmax*2+5)*4))::B   ! Vetor de Valores e Numero de bits de cada valor 
   CHARACTER(len=1),DIMENSION((nsubset*nvarmax*2+5)*16):: sec4 
   CHARACTER(len=1),DIMENSION(4)      ::auxsec4          
   INTEGER                            ::uni,k,err,noct,noctaux,j,i
   INTEGER                            ::ii
   INTEGER                            ::nvarlimit      ! Numero variaveis limite durante o processo de codificacao


!}
 
   uni=un
   SUBNAME="SAVESEC4RD"
 
   ! Nota: Dutante a codificacao com replicador pospost pode ser necessario um numero de variaveis
   ! maior do que o fornecido. Isto ocorre quando e utilizado fator de replicacao nulo. 
   ! Para evitar problemas de fata de alocacao de espaco, definiu-se nvarlimit duas vezes maior do 
   ! que nvarmax { 
      nvarlimit=nvarmax*2 
   !} 

    allocate(D(1:nvarlimit),STAT=err)
    IF(err>0)  THEN 
      print *,"Error allocating memory for section 3"
      stop
    END IF


    b(1)=24;a(1)=0   ! Tamanho da secao 4 Ainda naao ee conhecido 
    b(2)=8; a(2)=0   ! byte reservado (= 0)
    k=2

! Prepara para gravar todos os subsets (Valors e Indices de confiabilidade) 
! Como o numero de variaveis pode "variar"  em cada subset de 
! informacao, uma nova expansao de descritores precisa ser feita
! para cada subset
!
!  Note que o numero de descritores da secao 3 nao muda. Ele e fixo
!  o que muda sao os descritores expandidos para a secao 4, que
!  podem ser dIFerentes `a cada subsecao de informacao
!
!  Um outro problema `e o numero maximo de variaveis (nvarmax) 
!  esse nao indica de fato o numero de valores na secao 4 de
!  um subset especIFico e sim do maior subset. O problema em
!  questao `e como deteminiar o fim da expansao dos descritores
!
! { 

   do i=1,nsubset
 
      IFinal=0
 
      20 call expanddesc3(desc_sec3,ndesc_sec3,nvarlimit,D,ndesc_sec4,IFinal,err) 
       
        if (nds4_debug>0)  call chksec4_descriptores (D,ndesc_sec4,ifinal,sec4e,i,err) 
       
       !{ Caso ocorra erro, mostra descritores e variaveis da secao 4

        if(err>0) then   
        print *,"Error in subset =",i
        print *,"Descriptors and values processed in the section 4"
          do ii=1,ifinal 
            write( *,111)II,D(ii)%f,D(II)%x,D(II)%y,D(II)%i,sec4e%r(ii,i)
           111 FORMAT(i3,") [",i1,"-",i2.2,"-",i3.3,"(",i3,")]= "f15.4)
          end do
          return 
        end if

       !}
        IF (IFinal<ndesc_sec4) THEN 
        
          IF ((d(IFinal-1)%f==1).and.(d(IFinal-1)%y==0)) THEN 
            FatorR=sec4e%r(IFinal-1,i) ! Obtem o fator de replicacao 
             d(IFinal-1)%y=FatorR ! Transforma o replicador atrasado em replicador normal
                               ! Inverte a ordem dos descritores para poder processar 
                               !  o replicador normal
           
            auxdesc=d(IFinal-1)
            d(IFinal-1)=d(IFinal)
            d(IFinal)=auxdesc
           
            !{ Tratamento do fator de replicacao nulo
             If (FatorR==0) then
                CALL remove_desc(d,ndesc_sec4, IFINAL,IFINAL+D(IFINAL)%X)
             end if 
            !}
            IFinal=IFinal-1 !Retrocede um descritor
            goto 20  !********* Retorno a replicacao  !!!! Verificar esta saida !
          END IF
         
        ELSEIF (nvarmax<ndesc_sec4) THEN
          print *,"Error! Number of expected variables and provided variable are differents"
          print *,"      Check the delayed replicators and replication factors"
          print *,"      Number of provided variables=",nvarmax
          print *,"      Number of expected variaveis=",ndesc_sec4
          stop
        END IF
       
       call tabc_setparm(err=err)

       !{Valores
        do j=1, ndesc_sec4
            k=k+1
            b(k)=bits_tabb2(d(j))
            a(k)=CINT(sec4e%r(j,i),d(J))
	    !print *,D(J)%F,D(J)%X,D(J)%y,B(K),A(K)
        END do
        !}
    END do
 
 !{ Organiza os dados da secao 4 em octetos (sec4)
  
   call PUT_OCTETS(A,B,k,sec4,noct,err)  

    ! Se o numero de octetos (Secao3+secao4)naao for par
    ! acrescentar um octeto com zero
    !{
     IF (mod(noct,2)>0) THEN 
      noct=noct	+1
      sec4(noct)=char(0)
     END IF
    !}
    !}

    !{Agora coloca o tamanho da secao 4
      Tam_Sec4=noct
      B(1)=24;a(1)=Tam_Sec4
   
      call PUT_OCTETS(A,B,1,auxsec4,noctaux,err)
      IF ((noctaux/=3).or.(err/=0)) THEN 
        print *,"error in setion  4 "
        stop
      END IF
      sec4(3)=auxsec4(3)
      sec4(2)=auxsec4(2)
      sec4(1)=auxsec4(1)
    !}
    !{ Gravar 
      do i=1,noct
        currentRG=currentRG+1
        write (uni,rec=currentRG) sec4(i)
     END do
    !}
 !} Fim da Gravacao da secao 4

tam_sec4=noct
  
END SUBROUTINE SAVESEC4rd





  
! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC4CMP                                 | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!   DEFINE E GRAVA A SECAO 4 no MODO COMPRIMIDO                               !
!                                                                             !
! ----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:PUT_OCTETS, TABC_SETPARM,CINT, BITS_TABB2,vmax_numbits    !
! ----------------------------------------------------------------------------!
! HISTORICO:                                                                  !
!       Versao Original: Sergio H. S. Ferreira                                !
!
! 20140205 SHSF Ensuring that the variable Vmin is zero before processing 
!               the compressed character variable


SUBROUTINE SAVESEC4CMP(UN,D,ndesc,v,nsubset,tam_sec4)
 
!{ Variaveis da Interface
 TYPE(descbufr),DIMENSION(:),intent(in)::D        !Descritores BUFR
 INTEGER,                    intent(in)::UN       !Unidade de gravacao
 INTEGER,                    intent(in)::ndesc    !Numero de descritores
 REAL(kind=realk),DIMENSION(:,:)       ::v        !Matriz de valores v(1:ndesc,1:nsubsets)
 INTEGER,                   intent(out)::tam_sec4 !Tamanho da secao 4
 INTEGER,                    intent(in)::nsubset  !numero de subsets
!}

!{ Variaveis locais
 INTEGER(kind=intk),DIMENSION(ndesc*(nsubset+2)+4)::A
 INTEGER,           DIMENSION(ndesc*(nsubset+2)+4)::B        ! Valores convertidos p/ inteiro e respectivo tamnhos em bits 
 CHARACTER(len=1),DIMENSION((ndesc*(nsubset+2)+4)*4):: sec4 ! array de octetos da secao 4 
 INTEGER :: dimab                                           ! Dimensao dos vetores A e B 
 INTEGER :: dimoct                                          ! Estimativa do numero de octetos em sec4
!}

!{ Variaveis auxiliares
 CHARACTER(len=1),DIMENSION(4)::auxsec4                     
 INTEGER :: uni	,k,err,noct,noctaux ,j  ,i
 INTEGER :: nbits,vint,vmaxbits
 INTEGER(kind=intk2):: vmin,vmax 
!}

 
!{ Inicializacao de variaveis
 uni=un
 SUBNAME="SAVESEC4CMP"
 dimab=(nsubset*(ndesc+5)*4) ! Calculo da dimensao de B() e A()
 dimoct=dimab*8              ! Por seguranca, estimatima-se que serao necessarios 8 veze mais octetos do que o numero de valores a ser guardado
 !}

 
 b(1)=24;a(1)=0	    !Tamanho da secao 4 Ainda naao ee conhecido 
 b(2)=8; a(2)=0    ! byte reservado (= 0)
 k=2

! Prepara para gravar dados comprimidos 
! { 
   call tabc_setparm(err=err)
   do j=1, ndesc
     IF (d(j)%f/=0) THEN 
       call tabc_setparm(d(j),err)
     ELSE

       !{ obtENDo numero de bits (nbits)
       nbits=bits_tabb2(d(j))  ! numero de bits da tabela B 
       vmin=0                  ! Ensuring that the variable Vmin is zero 

        IF (d(j)%i>0) goto  77 ! Nao comprimir variaveis caracter

          vmaxbits=vmax_numbits(nbits)
          vmin=vmaxbits
          vmax=0

          do i=1,nsubset
             IF (v(j,i)/=null) THEN
                vint= CINT(v(j,i),D(J))
                IF ((vint>=0).and.(vint<vmaxbits)) THEN 
                   IF (vint<vmin) vmin=vint
                   IF (vint>vmax) vmax=vint
                END IF
             END IF
         END do

        nbits=numbits_vint(vmax-vmin) ! Numero de bits comprimidos
       !}
 77    continue
       !{ Codificar elemento
       !{Valor minimo 
          k=k+1
          b(k)=bits_tabb2(d(j))  
          a(k)=vmin
        !}
        !{ Numero de bits para gravar as dIFerenca	
          k=k+1
          b(k)=6
          a(k)=nbits
        !}
        !{ Gravar as diferencas (Dados comprimidos
        IF (nbits>0) THEN 
          do i=1,nsubset
  
             k=k+1
             b(k)=nbits
  
             vint=CINT(v(j,i),D(J))
             IF ((vint>=vmin).and.(vint<vmaxbits)) THEN 
                a(k)=vint-vmin
             ELSE 
                a(k)=vmaxbits
             END IF
           END do
       END IF
    END IF    
  END do
  !}
 ! Organiza os dados da secao 4 em octetos (sec4)
 !{
   call PUT_OCTETS(A,B,k,sec4,noct,err)  
 ! Se o nuumero de octetos (Secao3+secao4)naao for par
 ! acrescentar um octeto com zero
 ! {
     IF (mod(noct,2)>0) THEN 
       noct=noct +1
       sec4(noct)=char(0)
     END IF
    !}
 !}
 
  ! Agora coloca o tamanho da secao 4
    Tam_Sec4=noct
    B(1)=24;a(1)=Tam_Sec4
    
    call PUT_OCTETS(A,B,1,auxsec4,noctaux,err)
    IF ((noctaux/=3).or.(err/=0)) THEN 
       print *,"erro in  section 4 "
       stop
    END IF
    sec4(3)=auxsec4(3)
    sec4(2)=auxsec4(2)
    sec4(1)=auxsec4(1)
 !} Fim da preparacao para gravacao da secao 4
 !{ Gravar 
  do i=1,noct
   currentRG=currentRG+1
   write (uni,rec=currentRG) sec4(i)
  END do
!}
!} Fim da Gravacao da secao 4
  
END SUBROUTINE SAVESEC4CMP






! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.SAVESEC5                      | SHSF!
! ----------------------------------------------------------------------------!
!                       		                                      !
!	GRAVA A SECAO 5 (7777)                                                !
!                                                                             !
!-----------------------------------------------------------------------------!																  !
! Chamdas Externas: Nao Ha													  !
! Chamadas Internas:Nao Ha                                                    !
! ----------------------------------------------------------------------------!
! HISTORICO:																  !
!	Versao Original: Sergio H. S. Ferreira									  !
!_____________________________________________________________________________!



 SUBROUTINE SAVESEC5(UN)

 !{ Variaveis de interface
	 INTEGER, intent(in)::UN
 !} 
 !{ Variaveis locais
	INTEGER :: uni
	INTEGER :: i
	CHARACTER(len=1),DIMENSION(4)::sec5
 !}
 !{ Grava "7777"

	sec5(1:4) = '7'
	uni=un
	do i=1,4
		currentRg=currentRG+1
		write (uni,rec=currentRG) sec5(i)
	END do

END SUBROUTINE SAVESEC5


! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.INIT_TABB                                   | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!	INICIALIZA A TABELA BUFR B                                                !
!	LER A TABELA B E CARREGANDO OS VALORES NA MATRIZ GLOBAL TABB              !
! ----------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha	                                                  !
! Chamadas Internas:Nao Ha                                                    !
! ----------------------------------------------------------------------------!


 SUBROUTINE INIT_TABB(Un)
 
 !{ Variaveis de interface

	INTEGER,intent(in)::Un ! Unidade para a leitura da tabela BUFR B
 !}

 !{ Variaveis locais
	INTEGER::uni ,i,ii,jj
	INTEGER::F,X,Y,SCALE,REFV,NBITS
	CHARACTER(len=255)::C4,C5
	CHARACTER(len=255)::A
	CHARACTER(len=255)::filename
 !}

 !{ Inicializando variaveis e nome do arquivo da tabela
	uni=un
	i=0
  	
	TABB(:,:,:)%scale=0
	TABB(:,:,:)%refv=0
	TABB(:,:,:)%nbits=0
	TABB(:,:,:)%u=0

	filename=trim(local_tables)//"B"//basetabname(cur_tab)//".txt"
	current_tabB_mbufr=filename
  !}

  !{ CARREGANDO TABELA BUFR B

	if (verbose>=2) print *," :MBUFR-ADT: Table B -> ",trim(filename)
	OPEN (UNI, FILE =filename, ACCESS = 'SEQUENTIAL', STATUS = 'OLD')
  
  10  READ(UNI,"(A)",END=999)A
      
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
       print *,"Error reading BUFR TABLE B"
            print *, "Tabulation code found at line:"
          print *,trim(A)
          stop
        end if
      !} 
       
  IF (len_trim(a)>117) THEN
      READ(A,100)F,X,Y,C4,C5,SCALE,REFV,NBITS
      100 FORMAT(1X,I1,I2,I3,1X,A64,1X,A24,1X,I3,1X,I12,1X,I3)

      !if ((F==0).and.(x==12).and.(y==249)) then 
      !  print *,trim(A)
      !  print *,F,X,Y,"[",trim(C4),"]","[",trim(C5),"]",scale,refv,nbits
      !  stop
      !end if
      IF ((F==0).and.(x<=63).and.(x>=0).and.(y<=256).and.(y>=0)) THEN 
         
         TABB(F,X,Y)%scale=SCALE
         TABB(F,X,Y)%refv=REFV
         TABB(F,X,Y)%nbits=NBITS
         TABB(F,X,Y)%u=0
         IF (INDEX(C5,"CCIT")>0) TABB(F,X,Y)%u=1
         IF (INDEX(C5,"FLAG")>0) TABB(F,X,Y)%u=2 
         IF (INDEX(C5,"CODE")>0) TABB(F,X,Y)%u=3
         IF ((NBITS>496).and.(TABB(F,X,Y)%u/=1)) THEN
            print *,"Error reading BUFR table "
            print *,"Line=",I,"Nbits=",NBITS
            print *,trim(A)
            close(uni)
            stop
         END IF

         TABB(F,X,Y)%txt=trim(C4)//" ("//trim(C5)//")"
         
       END IF
   elseif(len_trim(a)>0) then 
         print *,"Error reading BUFR TABLE B near line",i
         write(*,'("[",a117,"]")')A 
         stop
   END IF
   i=i+1
   GOTO 10
999   CLOSE(UNI)
      
  END SUBROUTINE INIT_TABB  

! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.INIT_TABD                                   | SHSF!
! ----------------------------------------------------------------------------!
!                                                                             !
!       INICIALIZA DESCRITORES  DA TABELA BUFR D                              !
!       LER A TABELA D  CARREGANDO OS VALORES NA MATRIZ GLOBAL TABD           !
!                                                                             !
!
!*  O arquivo .Txt  contem as seguintes 
!*  informacoes :
!*  
!*  nl l F X Y Fl Xl Yl
!*
!*  onde :
!*   nl = Numero de linhas que contem os dados de 
!*        cada  descritor da tabela D 
!*    l = Numero da linha (1:nl) 
!*
!*   F  X  Y  = Descritores da tabela D
!*   Fl Xl Yl = Conjunto de nl descritores das demais tabelas 
!*             que corespondem aos descritores da tabela D
!*             (Descritores expandidos) 
!*                                                                            !
! ----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:Nao Ha                                                    !
! ----------------------------------------------------------------------------!
! HISTORICO:	                                                              !
!	Versao Original: Sergio H. S. Ferreira             
!                   !
!       2007-01-18 - SHSF: Elimizado o zeramento de TABD. E' suficiente  
!                    zarar o NDTABD, QUE CONTEM O NUMERO de DESCRITORES DE TABD
!                   , PARA QUE A SUBROTINA EXPANDESCD POSSA DISTINGUIR 
!                     UM DESCRITOR INESISTENTE NA TABELA D 
!_____________________________________________________________________________!

SUBROUTINE INIT_TABD(Un)
	  INTEGER,intent(in)::Un   !Unidade de leitura do arquivo
	  
	  !{Declaracao de variaveis auxiliares
	  INTEGER::uni
	  INTEGER ::nl,l,f,x,y,f2,x2,y2,i,f1,x1,y1,nl1
	  CHARACTER(len=255)::filename
	  CHARACTER(len=255)::linha
	  character(len=1)::t1
	  integer :: a,b
	  !}
 
	   uni=un
       i=0
   
          !{Zerando variaveis
	    NDTABD(:,:,:)=0
	  !}
	
	
	filename=trim(local_tables)//"D"//basetabname(cur_tab)//".txt"
	if (verbose>=2) Print *," :MBUFR-ADT: Table D -> ",trim(filename)
	OPEN (UNI, FILE =filename, ACCESS = 'SEQUENTIAL', STATUS = 'OLD')
  	

888   READ(UNI,'(a)',END=9898)linha
	b=0
	do a=1,12
	  if (ichar(linha(a:a))<32) then 
	     linha(a:a)="?"
	     b=1
	  end if
	end do
	if (b==1) then 
	     print *,"Error reading Table D"
	     print *,"Line='",trim(linha),"'"
	     stop
	end if 
	     
	  read(linha,'(1x,i1,i2,i3,1x,i3,a1,i1,i2,i3)')f1,x1,y1,nl1,t1,F2,X2,Y2
	  if (t1/="") then
	  	  read(linha,'(1x,i1,i2,i3,1x,i2,a1,i1,i2,i3)')f1,x1,y1,nl1,t1,F2,X2,Y2 
	  end if
		
		IF (nl1>0) THEN 
			f=f1
			x=x1
			y=y1
			l=0
			nl=nl1
		END IF

		l=l+1
		TABD(F,X,Y,l)%F=F2
		TABD(F,X,Y,l)%X=X2
		TABD(F,X,Y,l)%Y=Y2
		NDTABD(F,X,Y)=nl
		i=i+1
	  	!IF ((x==40).and.(y==28)) then
 		!print *,"3-40-028", l,TABD(F,X,Y,l)		
     		!end if 
	  goto 888
9898  continue
       	 !print*,'UNIDADE mbufr ',UNI
	 close(UNI)
	 
	  END SUBROUTINE INIT_TABD






! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.REINITTABLES                                | SHSF!
! ----------------------------------------------------------------------------!
! Reinicializa  tabelas BUFR (utilizando diferentes versoes)
!
!                                                                             !
! ----------------------------------------------------------------------------!																  !
! Chamdas Externas: Nao Ha													  !
! Chamadas Internas:INIT_TABB,INIT_TABD                                       !
! ----------------------------------------------------------------------------!
! HISTORICO:																  !
!	Versao Original: Sergio H. S. Ferreira									  !
!_____________________________________________________________________________!
! Mudado estrategia de localizacao das tabela. Agora verifica primeiro se
! a tabela existe 

 SUBROUTINE REINITTABLES(tabin,Err)
  
  !{ Variaveis de interface
	type(tabname)::tabin
	INTEGER, intent(out) :: err

  !}

  !{ Variaveis locais
	CHARACTER(len=255):: tabb_filename,tabd_filename
	LOGICAL :: exists
        integer :: i

  !}

       write(tabb_filename,14)tabin%NumMasterTab,tabin%centre,Tabin%VerMasterTab,Tabin%VerLocTab 
       write(tabd_filename,15)tabin%NumMasterTab,tabin%centre,Tabin%VerMasterTab,Tabin%VerLocTab 
       tabb_filename=trim(local_tables)//tabb_filename
       tabd_filename=trim(local_tables)//tabd_filename 
       INQUIRE (FILE = tabb_filename, EXIST = exists)
	  IF (exists) inquire(file=tabd_filename, EXIST=exists)
	  IF (exists) THEN 
		Cur_tab=tabin 
		call INIT_TABB(99)
		call INIT_TABD(99)
		err=0
	        return
          end if

  
  !Verifica se a tabela corresponde a uma das tabelas da lista de links de tabelas
  !Se for, substitui a tabela pelo link  
  !{
    do i=1,ntabs
      if (IsEqual(tabin,tablink(I,1))) then 
       ! print *,"MBUFR-ADT: Link: ",basetabname(tablink(i,1)),"-->",basetabname(tablink(i,2))
        tabin=tablink(i,2)
        exit
      end if
    end do
  !}
  
   !{Verify if it is a local table defined as version 1. If was consider as 0
	If (Tabin%VerLocTab==1) Tabin%verloctab=0
   !}
   
	write(tabb_filename,14)tabin%NumMasterTab,tabin%centre,Tabin%VerMasterTab,Tabin%VerLocTab 
	write(tabd_filename,15)tabin%NumMasterTab,tabin%centre,Tabin%VerMasterTab,Tabin%VerLocTab 
	  tabb_filename=trim(local_tables)//tabb_filename
	  tabd_filename=trim(local_tables)//tabd_filename 
  
  
      INQUIRE (FILE = tabb_filename, EXIST = exists)
	  IF (exists) inquire(file=tabd_filename, EXIST=exists)
	  IF (exists) THEN 
		Cur_tab=tabin 
		call INIT_TABB(99)
		call INIT_TABD(99)
		err=0
	  ELSE 
		err=1
	  END IF
14	   format("B",i3.3,i3.3,2i2.2,".txt")
15	   format("D"I3.3,i3.3,2i2.2,".txt")


END SUBROUTINE REINITTABLES


 !------------------------------------------------------------------------------!
 ! SUBROUTINE PRIVADA: MBUFR.GET_OCTETS                                  | SHSF !
 ! -----------------------------------------------------------------------------!
 !                   SUBROUTINE PRIVADA GET_OCTETS                              *
 !                                                                              *
 ! FUNCAO:                                                                      *
 !  COPIA UM VETOR DE ELEVEMTOS DE 8 BITS (OCTETO) PARA VETOR DE VALORES        *
 !  INTEIROS COM DIFERENTES NUMEROS DE BITS                                     *
 !                                                                              *
 ! NOTA:                                                                        *
 !                                                                              *
 !  Processo inverso da rotina PUT_OCTETS - Para mais detalhes vide  PUT_OCTETS *
 ! -----------------------------------------------------------------------------!  !
 ! Chamadas Externas: Nao Ha                                                    !
 ! Chamadas Internas:Nao Ha                                                     !
 ! -----------------------------------------------------------------------------!
 ! HISTORICO:                                                                   !
 !  Versao Original: Sergio H. S. Ferreira	                                
 !  SHSF 20101006 : Modificado identificacao dos valores MISSING na SECAO4CMP
 !                   Ao inves de considerar pelo menos 2 bits para atribuir MISSING, 
 !                   Agora atribui MISSING com peli menos 1 bit 
 ! SHSF  20170710 : Modificado novalemte identificacao dos valores MISSING 
 !                   Ao inves de considerar pelo menos 1 bits para atribuir MISSING, 
 !                   volta a atribui MISSING com pelo menos 2 bit (isto ocorre devido 
 !                   aos campos associados pelo descritos 2-04-yyy (Falta verificar em put_OCT 


SUBROUTINE  GET_OCTETS(oct, noct, A_BITS, A_VAL, NA,APOS, ERR)
 
 !{ Variaveis da interface    
  CHARACTER(len=1),DIMENSION(:), intent(in):: oct    !Vetor de octetos 
  INTEGER,intent(in)                       :: noct   ! Numero de octetos em oct
  INTEGER,DIMENSION(:),intent(in)          ::A_BITS  !'Vetor de  numeros de bits para cada variavel
                                                     ! Este numero nao pode ser superior a (8 * 8 bytes)
                                                     ! A_BITS =< 64
  INTEGER,intent(in)                        :: NA    ! NA = Numero elementos de A_BITS ou A_VAL
  INTEGER,intent(in)                        :: APOS  ! Se maior que zero inidica um salto para
                                                     ! uma posicao de A_VAL, apos a qual receberar
                                                     ! os valores recortados de OCT
                                                     ! A posicao de OCT tambem e localizada, assim como
                                                     ! o Bit exato onde comeca a conversao de OCT para A_VAl  
  INTEGER(kind=intk),DIMENSION(:),intent(out)::A_VAL ! Vetor com os valores extraidos de oct (INTEIRO DE 64 BITS).
                                                     ! A_BITS(i)= Numero de Bits de A_VAL(i)
  INTEGER,intent(out)                        :: err  !'retorna valor que indica se houve discrepancias
                                                     !' Entre o tamanho de oct e o total de bits solicitados
                                                     ! Err >0  indica que existem  mais bits em OCT do que o solicitado em A_BITS
                                                     ! Err = 0 indica que o numero de bits existentes em oct Ã© exatamente o solicitado em A_BITS
                                                     ! Err < 0 indica que naao existem bits suficientes em OCT para atENDer a solicitacao em A_BITS.
                                                     ! Neste ultimo caso, um erro ee apresentado na tela
  !}
  !{ Variaveis locais e auxiliares
    INTEGER,DIMENSION(8,noct)::  bit(8, noct) ! Array auxiliar para redistribuicao de bits
    INTEGER :: i, J, sbits, k, b
    INTEGER ::BYTEINI,BITINI
    INTEGER ::NOCT2,noct8
    INTEGER ::conta1
    INTEGER ::DBUG_VAL,DBUG_J  !Ponto de incio e extensao dos dados do descritor 2-05-yyy
    INTEGER ::NA2

  !}

  !------------------------------------------------------------------------------
  !Calcula  o numero de octetos  que serao decodificados e determin o err
  !Tambem determina o ponto de inicio e extensao dos dados do descritor 2-05-YYY
  !------------------------------------------------------------------------------
  !{
 
   sbits=bits_totalizer (noct,A_BITS,NA,NA2) 
   ERR = (noct * 8 - sbits)
  !}

   IF (ERR < 0) THEN
    ! Numero de bits que se busca é maior que o numero de octetos passados 
    ! Ou seja vao faltar octetos a serem lidos no final
    if (verbose>1) print *,TRIM(SUBNAME)//"_GET_OCTETS: Error: Unexpected number of bits => Diff=",err
     do k=na2,na
       A_VAL(K)=0
     end do
     NOCT2=NOCT
     
   ELSEIF (ERR>0) THEN
     ! Numero de bits que se busca é menor que o numero de octetos passados
     ! Ou seja, vao sobrar octetos sem ler (parcialmente ou totalmente) 
     ! NOCT2 =< NOCT

      NOCT2=SBITS/8+1
   ELSE
      ! Numero de bits que se busca é exatamente igual ao de octetos passados
      ! (ideal)
      NOCT2=NOCT
   END IF
   if (NOCT2<0) return 
  !}
  !{ OBTEM O PRIMEIRO OCTETO QUE SERA UTILADO, ASSIM COMO O PRIMEIRO bite deste octero que sera lido 

    BYTEINI=1
    BITINI=0
    IF (APOS>0) THEN 
      SBITS=0
      do  k = 1,APOS
        sbits = sbits + A_BITS(k)
      END do
      BYTEINI=sbits/8 +1
      BITINI=sbits-(BYTEINI-1)*8
    END IF
  !}
  !{Faz a transferencia  dos octetos de entrada para matriz boleana auxilar
    do i = BYTEINI, noct2
      do J = 0, 7
        bit(8 - J, i) = ibits(ichar(oct(i)), J, 1)
      END do
    END do
  !'}
   
  !------------------------------------------------------------------- 
  ! FAZER A REGISTRIBUICAO 1- Primeiro para palavras ate (ibits) bits. D
  !------------------------------------------------------------------
    i = BYTEINI  ! Incremento de octeto
    J = BITINI+1 ! Incremento de bit
    do k = (APOS+1), NA2
      A_VAL(k) = 0

      !------------------
      !Ate (idigits) bits 
      !------------------  
      IF ((A_BITS(K)>0).AND.(A_BITS(K)<idigits)) THEN

        conta1=0 
        do b = 1,A_BITS(k) !' Varia do bit1 ao numero de bits de A_VAL(k)

          IF (i > noct2) THEN
	    if (verbose>1)  print *,"Erro  "//TRIM(SUBNAME)//"_GET_OCTETS: Unexpected number of bits"
            err=noct2-i
            return 
          END IF
      
          IF (bit(j,i)==1) conta1=conta1+1 
          A_VAL(k) = A_VAL(k) + bit(J, i) * 2 ** (A_BITS(k) - b)
          J = J + 1
      
          IF (J > 8) THEN
             J = 1
             i = i + 1
          END IF 
         
        END do
     
     ! VERIFICACAO E ATRIBUICAO DE VALORES MISSING 
     !   VALORES MISSING SO SAO ATRIBUIDOS A VARIAVEIS NAS SEGUINTES CONDICOES: 
     !   A) Somente aos elementos da sacao 4 quando todos os bits deste elemento for 1 
     !   B) E necessario que o elemento tenha pelo menos 2 bit  (antes era 1 bits ) 
     !{
       IF ((conta1==A_BITS(K)).AND.(INDEX(SUBNAME,"READSEC4")>0)) THEN 
          IF (A_BITS(K)>2) A_VAL(K)=NULL
       END IF
       IF ((conta1==A_BITS(K)).AND.(INDEX(SUBNAME,"READSEC4CMP")>0)) THEN
          IF (A_BITS(K)>1) A_VAL(K)=NULL
       END IF
      !}

    !-------------------------------------------
    ! MAIOR QUE idigits BITS
    ! Igual ao anteior, execeto pela atribuicao
    ! ------------------------------------------
    ELSEIF (A_BITS(K)>idigits) then
		DBUG_VAL=0
		A_VAL(K)=NULL
		if (verbose>1) then 
                 WRITE(*,'("  :MBUFR-ADT:",A)')TRIM(SUBNAME)//": GET_OCTETS: Warning- Overflow error!"
                 WRITE(*,'("            : Variable,",i6," Nbits=",i4)') k,A_BITS(k)
		end if
		!Apenas corre os numeros de bits. Supostamente corresponde a entra do descritor 2-05-yyy 
                !{
		 do b = 1,A_BITS(k) !' Varia do bit1 ao numero de bits de A_VAL(k)
	  
			IF (i > noct2) THEN
				if (verbose>1) print *,"Erro  "//TRIM(SUBNAME)//"_GET_OCTETS: Unexpected number of bits"
				err=noct2-i
				return 
			END IF
      			J = J + 1
     		        IF (J > 8) THEN
				J = 1
				i = i + 1
			END IF 
    
		 END do
             !}
      ELSEIF(A_BITS(K)==0) then
		A_VAL(K)=0
      ELSE 
               A_VAL(K)=NULL
      END IF 
	 
   END do
   
 END SUBROUTINE GET_OCTETS 
 !}
! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.totalizer_                        | SHSF!
! ----------------------------------------------------------------------------!
!  Calcula  o somatorio de numero de bits/octetos que serao decodificados
!  conforme o vetor A_bits. tambem retorno o index do ultimo octeto 
! ----------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha                                                   !
! Chamadas Internas:                                                          !
!   get_octets                                                                !
! ----------------------------------------------------------------------------!

function bits_totalizer (noct,A_BITS,NA,NA2);integer::bits_totalizer
!{ Interface
 integer,              intent(in)::noct   ! Numero de octetos
 integer,dimension(:), intent(in)::A_BITS ! Vetor de bits para cada descritor
 integer,              intent(in)::na     ! Numero de elementos em A_bits  
 integer,             intent(out)::NA2    ! Ultima posicao de A_BITS utilizada (sera menor que NA) se nao houver octetos suficientes	
 !}
 !{Local
  integer ::noct8,k,sbits
 !}
    SBITS=0
    noct8=noct*8     
    do  k = 1, NA
      if (A_BITS(K)<0) then
        print *,TRIM(SUBNAME)//"_GET_OCTETS: Fatal Error! Invalid number of bits. Sequence number=",K
        stop
      end if
       sbits = sbits + A_BITS(k)
       if (noct8>=sbits)NA2=K
      
    END do
    bits_totalizer=sbits
 !}
 end function

! ----------------------------------------------------------------------------!
! SUBROUTINE PUBLICA: MBUFR.READ_MBUFR                                  | SHSF!
! ----------------------------------------------------------------------------!
!  SUBROTINA PARA LEITURA DE UMA MENSAGEM BUFR                                !
! ----------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha                                                   !
! Chamadas Internas:                                                          !
!   readsec1,readsec2,readsec3,readsec4b,readsec4rd,readsec4cmp ]             !
! ----------------------------------------------------------------------------!
! Example 
!  type(sec1type)::sec1
!  type(sec3type)::sec3
!  type(sec4type)::sec4
!  integer       ::BUFR_ED  ! Bufr Edition 
!  integer       ::NBYTES   ! Number of bytes in the read message
!  integer       ::errsec   ! Error code (0= no errors) 
!  integer       ::s        ! index for subsets 
!  integer       ::v        ! Index for variables
! call open_mufr(1,<filename>)
! 10 call read_mbufr(1,200000,sec1,sec3,sec4,bufr_ed,nbytes,errsec)
!
!  if ((errsec==0).and.(nbytes>0)) then 
!    do s=1,sec3%nsubsets
!       do v=1,sec4%nvars  
!         if (sec4%d(v,s)==004001) then 
!            print *,"year=",sec4%r(v,s)
!         end if
!       end do
!    end do
!   if (nbytes>0) goto 10
!   call close_mbufr(1)



 SUBROUTINE  READ_MBUFR(uni,sec1,sec3,sec4, bUFR_ED, NBYTES,errsec,select,optsec,header)
 
 !{ Declaracao das Variaveis da interface 
   INTEGER, intent(in)       ::uni         ! Unidade de leitura
   TYPE(sec1TYPE),intent(out)::sec1
   TYPE(sec3TYPE),intent(out)::sec3
   TYPE(sec4TYPE),intent(out)::sec4 
   INTEGER,       intent(out)::bufr_ed     ! BUFR edition 
   INTEGER,       intent(out)::nbytes      ! mensage size 
   INTEGER,       intent(out)::errsec      ! Erro de leitura. se >=0 indica o numero da secao onde ocorreu o erro (exceto secao0)
   TYPE(selectTYPE),optional,DIMENSION(:),intent(in)::select
   TYPE(sec2TYPE),optional  ::optsec
   character(len=40),optional,intent(inout)::header !Telecommunications header (40 bytes)
 !}

 !{ Declaracao das variavaveis locais

   TYPE(descbufr),pointer,DIMENSION(:)::desc_sec3  ! Descritores  module swap PrgEnv-pgi/4.0.46  PrgEnv-gnu/4.0.46da secao3 (ndesc)
   TYPE(descbufr),pointer,DIMENSION(:)::desc_sec4  ! Descritores da secao4
   type(descbufr)                     ::auxdesc    ! Variavel auxiliar do tipo descbufr
   INTEGER                            ::ndescmax
   INTEGER                            ::tam_sec4, tam_sec4max
   INTEGER                            ::tam_sec2, tam_sec3
   INTEGER                            ::un
   INTEGER                            ::i,RGSEC4,ERR,errcmp,errrd ,j
   INTEGER                            ::IFinal
   INTEGER                            ::alerr
   INTEGER                            ::nvars
   integer                            ::Fator_replicacao
   logical                            ::Delayed_Replicator
   character(len=80)                  ::ccc
   CHARACTER(len=1)                   ::oct
   integer                            ::aux
 !}
 !{ Este programa ler aquivos de ate 4.2 GBytes

  IF (CurrentRG>currentRGMax) then 
    print *,"Error 01:",ERROMESSAGE(01), CurrentRG,"Bytes"
    errsec=01
    return
  end if
 !}

 !{ Inicializando variaveis

   SUBNAME  ="READ_MBUFR"
   un       =UNI
   errcmp   =0
   errsec   =0
   NVARS    =0
   IOERR(UN)=0
   NSBR     =0 !Numero de subsets lidos (no caso de interrupcao)
   delayed_Replicator=.false.
   subset_byte_completed=.false.

!}

! allocate(sec3%d(1:1))
! allocate(desc_sec3(1:1))
  
 10 continue
  if (sec3_is_allocated) then 
   if (associated(sec3%d)) deallocate(sec3%d)
  end if
  
  !------------------------------------------------------------------
  ! Obtem dados das secoes 0,1,2 e 3, header e verifica integridade
  !-----------------------------------------------------------------
  !{ 
   
   call read_info0(un,cur_header,bufr_ed,nbytes)
    
   if (present(optsec)) then 
     call read_info123(un,nbytes,sec1,sec3,desc_sec3,tam_sec2,tam_sec3,errsec,optsec)
   else
     call read_info123(un,nbytes,sec1,sec3,desc_sec3,tam_sec2,tam_sec3,errsec)
   end if 
    if (errsec==32) goto 99 
 
    if (present(header)) then 
      header=cur_header
    end if 

   !Por algum motivo desc_sec3 da erro de segmientacao quando impresso aqui
   ! print *,"desc_sec3",desc_sec3
   
   ! Verify end of file and error reading sections 1,2 or 3
    IF (IOERR(UN)==0) then 
      if (errsec>0) goto 10
    else 
     goto 99 
   end if
    
!{ Verifica se a mensagem e do tipo selecionado
!--------------------------------------------------------------------
! Se forem selecionados tipos e subtipos de mensagem BUFR
! verIFica se a mensagem corrente pertence a um dos tipos/substipos
! selecionados.
!  Caso afirmativo, continua a leitura da mensagem
!  Caso negativo, retorna sem proceder a leitura 
!------------------------------------------------------------------
    
   IF (present(select))  THEN
 
     IF ( .not. select(1)%bTYPE==none) THEN

      ERRSEC=1
      do i=1,ubound(select,1)
      IF ((select(i)%bTYPE==sec1%bTYPE).or.(select(i)%btype==any)) THEN
        IF((select(i)%bsubTYPE==sec1%bsubTYPE).or.(select(i)%bsubTYPE==any)) THEN
         IF ((select(i)%center==sec1%center).or.(select(i)%center==any)) then 
          ERRSEC=0
         end if
        END IF
       END IF
       END do
      END IF
  
      IF (ERRSEC==1) THEN
        sec3%nsubsets=0
        sec4%nvars=0
        errsec=0
        goto 99
      END IF
   END IF
   !}
   
!***********************************
!* Checks version of the BUFR table
!*********************************** 
 err=check_vertables(sec1%center,sec1%NumMasterTable,sec1%VerMasterTable,sec1%VerLocalTable)
 if ((err==16).and.(sec1%VerMasterTable==255)) then
  print *,":MBUFR_ADT: Warning! Version of Master Table is missing. Using version: ",Cur_tab%VerMasterTab
  sec1%VerMasterTable=Cur_tab%VerMasterTab
  err=0
 end if 
   IF (err/=0) THEN
     errsec=err
     ccc=""
     if (RGINI>40) then
       do currentRG =RGINI-40,RGINI+4
         read (un,rec= currentRG,iostat=IOERR(UN)) oct
         if ((ichar(oct)>32).and.(ichar(oct)<123)) then 
          ccc=trim(ccc)//oct
         else
          ccc=trim(ccc)//"."
        end if
       end do
    end if
    CALL ERROLOG2(ERRSEC,sec1,ERROMESSAGE(err),ccc)
   
    goto 99
   END IF
   
 !----------------------------------------------------------------------
 ! Alocacao dinamica das variaveis (deasloca e realoca) antes da expansao 
 ! dos descritores, apos leitura da secao 3
 !-----------------------------------------------------------------------
   ndescmax=50000
   if (sec1%btype== 2) ndescmax= 500000
   if (sec1%btype== 3) ndescmax=1000000
   if (sec1%btype==11) ndescmax=1000000
   if (sec1%btype==6)  ndescmax=1000000

   if (dsec4_is_allocated) then 
      if (associated(desc_sec4)) then 
        print *,size(desc_sec4,1)
        deallocate(desc_sec4,stat=alerr)
	print *,"alerr=",alerr
	dsec4_is_allocated=.false.
      end if 
   end if 
   allocate(desc_sec4(1:ndescmax),stat=alerr)
     
   IF(alerr/=0) THEN
      print *, "Error in the READ_mbufr (memory allocation)"
      print *, "pointer desc_sec4(ndmax)"
      print *, "ndmax=",ndescmax
      errsec=99 
      return
   END IF
   dsec4_is_allocated=.true.

  ! allocate(sec4%a(1:ndescmax,1:1))
  ! allocate(sec4%r(1:ndescmax,1:1),sec4%d(ndescmax,1:1),sec4%c(ndescmax,1:1),sec4%k(ndescmax,1:1),stat=alerr)

   desc_sec4(1:ndescmax)%i=0
   desc_sec4(1:ndescmax)%n=.false.
   desc_sec4(1:ndescmax)%k=.false.
   desc_sec4(1:ndescmax)%a=0
   sec4%nvars=0
   if (verbose==3) print *," :MBUFR-ADT: Memory allocation: maximum number of expanded descriptor =",ndescmax
   
  !}
!***************************************
!* Processa a expancao dos descritores *
!***************************************
!{
      IFinal=0
      is_cpk=sec3%is_cpk 
       
      call expanddesc3(desc_sec3,sec3%ndesc,ndescmax,desc_sec4,nvars,IFinal,err)

70    IF (err/=0) THEN
          errsec=err+50
          if (verbose>1) call ERRORLOG(un,errsec,desc_sec4,ifinal, "Error in expanddesc3")
          goto 99
      END IF
    
      
!}

 !***********************
 !* Reducao do ndescmax *
 !***********************
 ! Caso nao haja repricador atrasado nao existe a necessidade
 ! de usar um ndescmax previamente estabelecido. Neste caso
 ! e usado o ndescmax e reduzido para um numero de descritores
 ! que realmente existem na mensagem BUFR
 !------------------------------------------------------------	  
 !{ 
	if (IFinal<nvars) Delayed_Replicator=.true.
	if (.not. Delayed_replicator) then
		ndescmax=nvars
	end if    
    
 !----------------------------------------------------------------------
 ! Realocacao dinamica das variaveis para processar leitura da secao4
 !-----------------------------------------------------------------------
    
  
    tam_sec4max=NBYTES-tam_sec3-tam_sec2-30

   ! deallocate(sec4%r,sec4%d,sec4%c,sec4%k,sec4%a)
     if (sec4_is_allocated) then 
      if (associated(sec4%r)) deallocate(sec4%r)
      if (associated(sec4%d)) deallocate(sec4%d)
      if (associated(sec4%c)) deallocate(sec4%c)
      if (associated(sec4%d)) deallocate(sec4%d)
      if (associated(sec4%a)) deallocate(sec4%a)
      if (associated(sec4%k)) deallocate(sec4%k)
     end if
   
     allocate(sec4%a(1:ndescmax,1:sec3%nsubsets))
     allocate(sec4%r(1:ndescmax,1:sec3%nsubsets),sec4%d(1:ndescmax,1:sec3%nsubsets),sec4%c(1:ndescmax,1:sec3%nsubsets),stat=alerr)
    if (alerr==0) allocate( sec4%k(1:ndescmax,1:sec3%nsubsets),stat=alerr)
 
    IF (alerr/=0) THEN
      print *,"Error in the section 4 (memory allocation)"
      print *,"sec4%r(ndescmax,sec3%nsubsets)"
      print *,"sec4%d(ndescmax,sec3%nsubsets)"
      print *,"sec4%c(ndescmax,sec3%nsubsets)"
      print *,"ndescmax=",ndescmax
      print *,"sec3%nsubsets"
      stop
    else
      sec4_is_allocated=.true.
    END IF

   !**********************
   !* Leitura da secao 4 *
   !**********************
   !{
    IF ((sec3%is_cpk==0).and.(IFinal<nvars)) THEN 
     !{ Processa a leitura da secao 4 caso haja descritor atrasado 
        if (verbose==3) print *," :MBUFR-ADT: runing Readsec4Rd"
        call READSEC4rd2(un,desc_sec3,sec3%ndesc,sec3%nsubsets,ndescmax,sec4,tam_sec4,errrd)
	IF (errrd/=0) errsec=errrd 
      !}
    ELSEIF ((sec3%is_cpk==0).and.(IFinal==nvars)) THEN
      !{ Processa a leitura da secao 4 naao compactada sem replicador atrasado 
        if (verbose==3) print *," :MBUFR-ADT: runing Readsec4b"
   
	call READSEC4b(un,desc_sec4,nvars,sec3%nsubsets,sec4,tam_sec4,errsec)
      !}
    ELSEIF ((sec3%is_cpk/=0).and.(IFinal==nvars)) THEN
      !{ Processa a leitura da secao 4 compactada
	if (verbose==3) print *," :MBUFR-ADT: runing Readsec4CMP"
        call READSEC4CMP(un,desc_sec4,ifinal,sec3%nsubsets,sec4,tam_sec4,errcmp)
        sec4%nvars=nvars

        IF (errcmp/=0) errsec=errcmp
    ELSEIF ((sec3%is_cpk/=0).and.(IFinal<nvars)) THEN
     ! Processa a leitura da secao 4 compactada com replicador atrasado   
     !  Normalmente nao deveria haver BUFR compactado com replicador atrasado 
     !  O replicador atrasado e' usado para permitir que os subsets tenham 
     !  diferentes repeticoes de uma mesma variael. No caso do BUFR 
     !  compactado, os subsets sao organizados de forma diferente,
     !  Aos ivés de ter uma sequencia de variáveis dentro dos subsets, 
     !  cada subset contem apenas 1 variavel 
     !
     !  Desta forma nao tem sentido mudar a repeticao das variaveis dentro
     !  do subset. 
     !
     !  No entanto o replicador atrasado funciona no caso especifico em
     !  que os fatores de replicao sao todos iguais, o que pode ser convertido em 
     !  um replicador normal. 
     !
     !  Isto pode ser constatado observando, por exemplo, o BUFR das boias argos
     !  onde e'usado o replicador atrasado compacto. O resultado que
     !  o fafor de replicao sempre fixo. 
     ! 
     !  Assim o que e' feito nesta parte e'descompactar o BUFR ate'encontrar o
     !  fator de replicao, e usar este valor para substituir o replicador atrasado 
     !  pelo replicador normal equivalente. Em seguida reprocessa-se a descompactacao
     !  afim de obter a decompactacao completa do BUFR. 
     !  o replicador normal.
     !{
        RGSEC4=CurrentRG
	if (verbose==3) print *," :MBUFR-ADT: runing Readsec4CMP"
        call READSEC4CMP(un,desc_sec4,ifinal,sec3%nsubsets,sec4,tam_sec4,errcmp)
       
      !}
     ! Como supostamente todos os fatores de replicacao sao iguais, pega-se o
     ! do primeiro subset. O replicador atrasado e'convertido em replicador
     ! normal
      !{
        Fator_Replicacao=sec4%r(ifinal,1) 
        desc_sec4(ifinal-1)%y=Fator_Replicacao
      !}
      ! print *,"Fac=",Fator_Replicacao
      ! Permutando ordem entre fator de replicao e o 
      ! replicador normal, para que este fator nao seje replicado
      !{
        auxdesc=desc_sec4(ifinal-1)
        desc_sec4(ifinal-1)=desc_sec4(ifinal)
        desc_sec4(ifinal)=auxdesc
      !}
      !{Quando o Fator de Replicao e'igual a zero, entao
      ! elimina o replicador e os descritores replicados
      If (Fator_Replicacao==0) then
         CALL remove_desc(DESC_SEC4,NVARS, IFINAL,IFINAL+DESC_SEC4(IFINAL)%X)
      end if
      ifinal=ifinal-1 ! Volta 1 descritor
      !ifinal=0
      SUBNAME="READ_MBUFR"
      call expanddesc3(desc_sec3,nvars,ndescmax,desc_sec4,nvars,ifinal,err) 
      currentRG=RGSEC4 !Repositionando no inicio da secao 4 para refazer a leitura
      !do i=1,ifinal
      ! print *,desc_sec4(i)
      !end do
      !print *,"---",err
      if (err==0) goto 70
      !} 
    END IF 
    !}
     !---------------------------------------------------------------
     !                 Copia dos descritores
     !A copia dos descritores e normalmente feita no final de 
     !cada READSEC4, mas quando ocorre erro de leitura e feita aqui.  
     !---------------------------------------------------------------
     !{
      if (verbose>2) then
          print *," :MBUFR_ADT:Error code=",errsec
      end if 
      IF ((errsec>0).and.(errsec<60)) THEN 
	   do j=1,sec3%nsubsets
		Do i=1,nvars
		 sec4%d(i,j)=desc_sec4(i)%f*100000+desc_sec4(i)%x*1000+ desc_sec4(i)%y
		 sec4%c(i,j)=desc_sec4(i)%i
                 sec4%k(i,j)=desc_sec4(i)%k
                 sec4%a(i,j)=desc_sec4(i)%a
		END do
	   END do
       if (verbose>1) call ERRORLOG(un,errsec,desc_sec4,nvars, erromessage(errsec))
     END IF
   !}
     
    !***************
    !* Finalizacao *
    !***************
 99  continue 
 100 if (dsec4_is_allocated) then 
          if(associated(desc_sec4)) then 
            deallocate(desc_sec4)
	    dsec4_is_allocated=.false.
	  end if 
      end if 
    if (desc_sec3_is_allocated) then 
    if(associated(desc_sec3))then 
        deallocate(desc_sec3)
	desc_sec3_is_allocated=.false.
    endif 
    end if 
    currentRG=RGINI+NBYTES-1         ! POSICIONAMENTO NO FINAL DA MENSAGEM
    NMSG=NMSG+1	
    
END SUBROUTINE read_mbufr
  
!------------------------------------------------------------------------------
!read_info123| Leitura do header, das secoes 0,1,2,3 e 5                          
!------------------------------------------------------------------------------
! Subrotina privativa
!------------------------------------------------------------------------------
! Chamada por: read_mbufr
subroutine read_info123(un,NBYTES,sec1,sec3,desc_sec3,tam_sec2,tam_sec3,errsec,optsec)
  integer,                    intent(inout)::un
  integer,                    intent(inout)::NBYTES
  type(sec1type),             intent(inout)::sec1
  TYPE(sec3TYPE),             intent(inout)::sec3
  TYPE(descbufr),pointer,DIMENSION(:),intent(inout)::desc_sec3  ! Descritores secao3 (ndesc)
  integer,                      intent(out)::tam_sec2    ! Tamanho da secao 2 em bytes
  integer,                      intent(out)::tam_sec3    ! Tamanho da secao 3 em bytes
  INTEGER,                    intent(inout)::errsec      ! Erro de leitura.
  TYPE(sec2TYPE),optional,    intent(inout)::optsec

 !}

 !{ Declaracao das variavaveis locais

   INTEGER                            ::tam_sec3max
   integer                            ::tam_sec1
   CHARACTER(len=1)                   ::oct
   CHARACTER(len=1),DIMENSION(4)      ::sec0 
   INTEGER,DIMENSION(2)               ::b
   INTEGER(kind=intk),DIMENSION(2)    ::a
   CHARACTER(len=4)                   ::sec5id 
   INTEGER                             ::i,RGSEC5,RGSEC2,RGSEC4,ERR
   INTEGER                            ::alerr
   character(len=80)                  ::ccc

  !character(len=4)               ::BUFRW
 


!'}-------------------------------------------------------------
!'{ Processar a leitura da sesaao da mensagem a partir da secao 1

   !{ Leitura da secao 1
   currentRG = RGINI + 7
   Call readsec1(un,sec1,tam_sec1,err)
   cur_sec1=sec1
   !}
  
   !{ Verifica caso de subset de byte completo
     if (sec1%center==56) then 
         subset_byte_completed=.true.
     end if
    !}

   !{ Se houve erro na secao 1, apresenta o erro e emcabeçamento se for possivel
   IF (err/=0) THEN
    errsec=err
    print *," :MBUFR-ADT: Erro. ",errsec,"! ",trim(ERROMESSAGE(err))

    print *,"           Generater center=",sec1%center
    print *,"           BUFR Category=",sec1%btype
    if (RGINI>40) then
     ccc=""
     do currentRG =RGINI-40,RGINI+4
        read (un,rec= currentRG,iostat=IOERR(UN)) oct
        if ((ichar(oct)>32).and.(ichar(oct)<123)) then 
          ccc=trim(ccc)//oct
        else
          ccc=trim(ccc)//"."
        end if
     end do
     print *,"           Byte position=",RGINI
     print *,"           near '",trim(ccc),"'"

    end if
    return
   END IF
   !}


   !{ se houver secao 2, obtem o tamanho da secao 2 e 
   !  reposiciona registro no final da secao2, para permitir a leitura da proxima
   !  secao
   tam_sec2=0


   IF(sec1%sec2present) THEN
         RGSEC5=RGINI+NBYTES-4
	 RGSEC2=currentRG
	 if (present(optsec)) then
		call readsec2(un,tam_sec2,optsec)
	 else
		call readsec2(un,tam_sec2)
	 end if
	 IF (currentRG>RGSEC5) THEN
	   if (verbose>2) print *,":MBUFR-ADT:Error reading setion2. Invalid Size=",tam_sec2," Current RG=",RGSEC2
	   errsec=20
	   currentRG=RGSEC2
	   tam_sec2=0
	   GOTO 60
	 END IF
   END IF

!}
 
 
!**************************
!*   Leitura da secao3    *
!**************************
!{
   sec3%ndesc=0
50   tam_sec3max=NBYTES-tam_sec2-30
      call READSEC3(un,tam_sec3max,desc_sec3,sec3,tam_sec3,err)
        ! Caso na secao 3 seja informado que a secao 4 tenha menos de 1 subset,
        ! entao considera essa uma mensagem vazia, sem dados e passa para a proxima mensagem
        if (err==32) then 
            currentRG=RGINI+ NBYTES - 4
            if (verbose>2)  write(*,'(" :MBUFR-ADT: Error ",i2,": ",a)')err,trim(erromessage(err))
	    errsec=32
	    goto 60
        end if 
	IF (err/=0) THEN

	 ! Em muitos casos, as mensagens BUFR nao vem com a secao
	 ! mas, mesmo assim, na secao 1 ee indicado a presenca da secao2
	 ! isto causa erro na leitura: A secao 2 ee lida no lugar da 3 e
	 ! a 3 no lugar da 4.
	 ! Nesta parte procura-se contornar este problema:
	 ! Quando ocorre erro de leitura na secao 3 e
	 ! sec2_present=true, faz-se uma nova tentativa de
	 ! leitura da secao 3 na posicao da secao2
	 !{

	 IF (sec1%sec2present) THEN
	  currentRG=RGSEC2
	  tam_sec3max=NBYTES-30
	  call READSEC3(un,tam_sec3max,desc_sec3,sec3,tam_sec3,err)

	  ! Se nao ocorreu erro na leitura da secao3 entao a
	  ! secao 2 nao existe e vai para a leitura da 4
	  !{
	  IF (err==0) THEN
	    sec1%sec2present=.false.
		tam_sec2=0
		goto 60
	  END IF
	  !}
	 END IF
	 !}READ_MB

	 ! Caso contrario nao le a secao 4 e vai
	 ! para o fim do subrotina
	 ! {
	 write(*,'(" Error ",i2,": ",a)')err,trim(erromessage(err))
	 errsec=err
	 return
	 !}
	END IF

   !}

60   continue
 
!}

end subroutine read_info123

! ----------------------------------------------------------------------------!
! SUBROUTINE PRIVADA: MBUFR.READSEC1                                    | SHSF!
! ----------------------------------------------------------------------------!
!   LER DADOS DA SECAO 1 E VERIFICA A COMPATIBILIDADE DAS 
!   TABELAS BUFR DA MENSAGEM COM A TABELA CARREGADA  POR ESTE 
!   MOODULO 
!                                                                              !
! -----------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha                                                    !
! Chamadas Internas:GET_OCTETS, check_vertables                                !
! -----------------------------------------------------------------------------!
! HISTORICO:                                                                   !
!   Versao Original: Sergio H. S. Ferreira                                     !
!   03/08/2006 : Corrigido leitura dos minutos (Sergio e Ana L. Travezan)      !
!______________________________________________________________________________!
 

SUBROUTINE READSEC1(un,sec1e,tam_sec1,sec1err)
!{ Declaracao de variaveis da interface
	INTEGER,intent(in)::un
	TYPE(sec1TYPE),intent(out)::sec1e  
	integer,intent(inout)::tam_sec1                           
	INTEGER,intent(inout)::sec1err
!}
!{ Declaracao de variaveis locais
	CHARACTER(len=1):: sec1(30)
	INTEGER,DIMENSION(20) :: b
        INTEGER(kind=intk),DIMENSION(20) :: a
	INTEGER :: i,err
!}
!{ Inicializar variaveis
   A(1:20)=0
   sec1err=0
   SUBNAME="READSEC1"
   sec1e%bTYPE = -99
   sec1e%center = -99
!}
!'{ Ler os 3 primeiros octetos da secao 1
   do  i = 1,3
     currentRG = currentRG + 1
     IF (IOERR(UN)==0) read(un,rec= currentRG) sec1(i)
     IF (IOERR(UN)/=0) RETURN 
   END do
!'}
!'{ Obter valores de cada um dos octetos lidos

   b(1) = 24 !'Tan_sec1 = Tamanho da secao 1
   Call GET_OCTETS(sec1, 3, b, a, 1,0, ERR)
   tam_sec1=a(1)

   if (tam_sec1==0) then 
      sec1err=11
      return
   end if
!}

!{ Ler octetos da secao 1
   do  i = 4,tam_sec1
      currentRG = currentRG + 1
      IF (IOERR(UN)==0) read(un,rec= currentRG) sec1(i)
      IF (IOERR(UN)/=0) RETURN 
   END do
!'}

IF (BUFR_EDITION<4) THEN 
	!'{ Obter valores de cada um dos octetos lidos	SECAO 1 EDICAO 2 E 3
	!------------------------------------------------------------------
	!Num. Bits | Descricao                                   |Octeto
	!-----------------------------------------------------------------
	b(1) = 24 !'Tan_sec1 = Tamanho da secao 1................| 1-3
	b(2) = 8  !'BUFR Master Table Se 0 ee a tabela padrao ...|   4
	b(3) = 8  !'Sub centro gerador...........................|   5
	b(4) = 8  !'Centro gerador...............................|   6
	b(5) = 8  !'Numero da Atualizacao........................|   7
	b(6) = 1  !'secao 2 incluida.............................|   8
	b(7) = 7  !' Tudo Zero...................................|   8
	b(8) = 8  !' Categora dos dados .........................|   9
	b(9) = 8  !' sub-categoria dos dados.....................|  10
	b(10) = 8 !' Versao da tabela mestre usada...............|  11
	b(11) = 8 !' versao da tabela local usada................|  12
	b(12) = 8 !' Ano do seculo...............................|  13
	b(13) = 8 !' MES.........................................|  14
	b(14) = 8 !'DIA..........................................|  15
	b(15) = 8 !'hORA.........................................|  16
	b(16) = 8 !'minuto.......................................|  17
 
	Call GET_OCTETS(sec1, tam_sec1, b, a, 16,0, ERR)
   
	IF (ERR < 0) THEN 
		print*,"Erro in section secao 1"
		sec1err=11
                sec1e%bTYPE = a(8)
                sec1e%center = a(4)
		return
	END IF
	IF (a(1) < 18) THEN 
		sec1err=12
                sec1e%bTYPE = a(8)
                sec1e%center = a(4)
              	return
        elseif ((a(1)>18).and.(verbose==3)) then
              print *," :MBUFR-ADT: Warning! Section 1 with unexpected size."
              print *,"             Section 1 size =",a(1), "Expected size = 18"
              print *,"             Bufr Edition=",Bufr_edition," Center =",a(4)
                
	END IF

	sec1e%subcenter=A(3)
	sec1e%center = a(4)
	sec1e%update=a(5)
	IF (a(6) == 1) then 
		 sec1e%sec2present = .True.
	else
		sec1e%sec2present = .false.
	end if
	sec1e%bTYPE = a(8)
	sec1e%bsubTYPE = a(9)
	sec1e%NumMasterTable=a(2)
	sec1e%VerMasterTable=a(10)
	sec1e%VerLocalTable=a(11)
	sec1e%year = a(12)
	sec1e%month = a(13)
	sec1e%day = a(14)
	sec1e%hour = a(15)
	sec1e%minute = a(16)
		
	sec1e%Intbsubtype=0	
	
	
	IF (sec1e%year<1900) THEN   
		IF (sec1e%year>50) THEN 
			sec1e%year=1900+sec1e%year
		ELSE 
			sec1e%year=2000+sec1e%year
		END IF
	END IF
 
ELSE
	! BUFR EDITION 4
	!------------------------------------------------------------
	!  Bits   |Octet|  Definicoes 
	!------------------------------------------------------------
	b(1) = 24 ! 1-3 |Tan_sec1 = Tamanho da secao 1
	b(2) = 8  !   4 |BUFR Master Table Se 0 ee a tabela padrao  
	b(3) = 16 !5-6  |Identification of originating/generating centre (see Common Code Table C-11)
	b(4) = 16 !7-8  |Identification of originating/generating sub-centre (allocated by originating/generating Centre- see Common Code Table C-12) 
	b(5) = 8  !  9  |Update sequence number (zero for original BUFR messages; incremented for updates)
	b(6) = 1  !10	|Bit 1	 =0	No optional section =1	Optional section follows
	b(7) = 7  !     |Bit 2-8		Set to zero (reserved)
	b(8) = 8  !11   |Data Category (Table A)
	b(9) = 8  !12   |Data sub-category international (see Common Code Table C-13)
	b(10)= 8  !13   |Data sub-category local (defined locally by automatic data processing centres)
	b(11)= 8  !14   |Version number of master table (currently 12 for WMO FM 94 BUFR tables - see Note (2))
	b(12)= 8  !15   |Version number of local tables used to augment master table in use - see Note (2)
	b(13)= 16 !16-17|Year (4 digits)		|
	b(14)= 8  !18   |Month			|
	b(15)= 8  !19   |Day			| Most typical for the BUFR message content
	b(16)= 8  !20   |Hour			|
	b(17)= 8  !21   |Minute			|
	b(18)= 8  !22   |Second			|
	b(19)= 8  !23   |Reserved for local use by ADP centres

	!{ Caso a secao 4 tenha apenas 22 bytes entao omite o ultimo byte
    if (tam_sec1==22) then 		
		
		Call GET_OCTETS(sec1, tam_sec1, b, a, 18,0, ERR)

	elseif ((tam_sec1==24).OR.(tam_sec1==23)) then
		
		Call GET_OCTETS(sec1, tam_sec1, b, a, 19,0, ERR)
	elseif (tam_sec1<22) then  	
                sec1e%bTYPE = a(8)
                sec1e%center = a(3)
		sec1err=12
		return
         elseif ((tam_sec1>22).and.(verbose==3)) then
              print *,"Warning! Section 1 with unexpected size."
              print *,"         Section 1 size =",a(1), "Expected size = 24"
              print *,"         Bufr Edition=",Bufr_edition," Center =",a(3)
    end if
 	!}

	IF (ERR < 0) THEN 
		print*,"Error reading section 1"
		sec1err=11
		return
	END IF

	
	sec1e%center = a(3)
	sec1e%subcenter=A(4)
	sec1e%update=a(5)
	IF (a(6) == 1) then  
		sec1e%sec2present = .True.
	else
		sec1e%sec2present = .false.
	end if
	sec1e%bTYPE = a(8)
	sec1e%IntbsubTYPE = a(9)
	sec1e%bsubtype=a(10)
	sec1e%NumMasterTable=a(2)
	sec1e%VerMasterTable=a(11)
	sec1e%VerLocalTable=a(12)
	sec1e%year = a(13)
	sec1e%month = a(14)
	sec1e%day = a(15)
	sec1e%hour = a(16)
	sec1e%minute = a(17)
	sec1e%second = a(18)
	
	
	IF (sec1e%year<1900) THEN   
		IF (sec1e%year>50) THEN 
			sec1e%year=1900+sec1e%year
		ELSE 
			sec1e%year=2000+sec1e%year
		END IF
	END IF
 	
END IF
 
  If (sec1e%VerLocalTable==255) sec1e%VerLocalTable=0
 
  !sec1err=check_vertables(sec1e%center,sec1e%NumMasterTable,sec1e%VerMasterTable,sec1e%VerLocalTable)
  
    
END SUBROUTINE	readsec1



! -----------------------------------------------------------------------------!
! FUNCAO PRIVADA INTEGER : MBUFR.check_vertables                         | SHSF!
! -----------------------------------------------------------------------------!
! VerIFica se a tabela BUFR em uso no modulo ee compativel com outra 
! tabela BUFR e retorna um dos flag de erro abaixo                                                                ! 
! 
!     0  | Tabela compativel 
!    14  | Tabela Master Desatualizada
!    15  | Necessario outra tabela Local
!    16  | Bad number of Master table version 
! -----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                     !
! Chamadas Internas:Nao Ha                                                     !
! -----------------------------------------------------------------------------!

function check_vertables(center,NMT,VMT,VLT); INTEGER :: check_vertables 
!{
!{ Variaveis da Interface
   INTEGER,intent(in)::center
   INTEGER,intent(in)::NMT ! Number of Master Table (for checking)
   INTEGER,intent(in)::VMT ! Version of Master Table (for checking)
   INTEGER,intent(in)::VLT ! Version of Local Table (For checking)
   INTEGER:: err 
!}
!{ Variaveis locais
   logical::reinittab ! true = Tabela incompativel. Reinicializa tabelas
   integer::i
   type(tabname)::tabin
   type(tabname)::aux_tab
   character(len=10)::auxc
!}
 
 !{ Version of master table  must be great than 0
	if ((VMT==0).or.(VMT==255)) then 
		check_vertables=16
		return 
	end if  
 !}
!{ Inicializando variaveis
 
   tabin%centre=center
   tabin%verloctab=VLT
   tabin%nummastertab=NMT
   tabin%vermastertab=VMT
   Decl_tab=tabin
   reinittab=.false.
   err=0
   check_vertables=0
!}

!{ Se tabbin = tabela corrente, entao Ok! Retorna ! 
   if (isEqual(cur_tab,tabin)) return
!}

!{ Verifica se a tabela nova existe. Se existir, entao tudo bem
 call reinittables(tabin,err) 
 if (err==0) return 
!}

!Verifica se a tabela corresponde a uma das tabelas da lista de links de tabelas
!Se for, substitui a tabela pelo link  
!{
    do i=1,ntabs
      if (IsEqual(tabin,tablink(I,1))) then 
       ! print *,"MBUFR-ADT: Link: ",basetabname(tablink(i,1)),"-->",basetabname(tablink(i,2))
        tabin=tablink(i,2)
        call reinittables(tabin,err) 
        exit
      end if
    end do
!}


!{ Verifica se e uma tabela mestre diferente 
   If ((cur_tab%NumMasterTab/=tabin%NumMasterTab))  then 
      reinittab=.true.
    end if
!}
  

! Verifica se a versao das tabelas correntes e inferior a versao das tabelas
! mestre do arquivo de entrada. Se for, tenta utilizar as tabelas iniciais 
!{
    if  (Cur_tab%VerMasterTab<Tabin%VerMasterTab) THEN
     
      if ((tabin%vermastertab<=Init_tab%vermastertab).and.(tabin%nummastertab==Init_tab%nummastertab)) then 
        tabin=Init_tab
      end if
      reinittab=.true.
    end if
!}


! Verifica se a tabela mestre corrente e do arquivo ten versos  >13
! Se ambas forem maior que 13 verifica se sao iguais ou diferentes. 
! Se forem diferentes entao reinicializa 
! Se a corrente for maior e a do arquivo for menor, tenta reinicializar 
! com a tabela 13 do centro das tabelas inicial , para ver se da certo. 
!{ 
   IF ((tabin%VerMasterTab>13).and.(Cur_tab%VerMastertab>13)) THEN  
     IF((Cur_tab%VerMastertab/=tabin%VerMasterTab)) then 
       tabin%centre=init_tab%centre
       reinittab=.true.
     end if
   elseif ((tabin%VerMasterTab<=13).and.(Cur_tab%VerMastertab>13)) then
      aux_tab=Init_tab
      aux_tab%VerMastertab=13
      call reinittables(aux_tab,err) 
      if (err>0) reinittab=.true.
   end if
!}
!{ Verifica se e uma tabela local
   IF ((tabin%VerLoctab>0).or.(Cur_tab%VerLoctab>0)) THEN  
     IF(Cur_tab%VerLoctab/=tabin%VerLoctab) reinittab=.true.
     IF(tabin%centre/=Cur_tab%centre) reinittab=.true.
     if(Cur_tab%VerMasterTab/=Tabin%VerMasterTab) reinittab=.true. 
   end if 
!}
!{ Reinicia tabela se necessario  
    if (reinittab) then 
      call reinittables(tabin,err) 
      
      !Caso haja erro em uma tabelas de centro diferente do centro inicial , tenta mais uma vez
      ! usando o centro inicial
      IF (err>0) THEN
        if ((tabin%centre/=init_tab%centre).and.(tabin%verloctab==0)) then
          tabin%centre=init_tab%centre
          call reinittables(tabin,err) 
        end if
      end if
      
      if (err>0) then 
        auxc=basetabname(tabin)
        if (verbose>1) then 
        print *,"Erro 14! ",ERROMESSAGE(14)
        write(*,'(" Tables B",a10,".txt or/and D",a10,".txt Not Found")')auxc,auxc
        end if 
        check_vertables=14
      end if
    END IF
!}
!}
END function check_vertables 


!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVADA: MBUFR.READSEC2                                    | SHSF!
! ----------------------------------------------------------------------------!
!   Esta subroutina obtem o tamanho da secao 2
!    e posiciona o currentRG ao final desta secao, afim 
!    de permitir a leitura da proxima secao
!
!    Nota: 1 - No momento, esta subrotina nao processa os dados
!              contidos na secao 2
!         2-  Antes de utilizar esta subrotina, certIFique-se
!              que a secao 2 exista REALmente (flag da secao1) e 
!              que o currentRG esteja apontando para o final 
!              da secao1
!     
!  
!-------------------------------------------------------------------------------
! Chamadas Externas: Nao Ha                                                    !
! Chamadas Internas:GET_OCTETS                                                 !
! -----------------------------------------------------------------------------!
! HISTORICO:                                                                   !
!  Versao Original: Sergio H. S. Ferreira                                      !
!_ SHSF 20101010 Melhorado o tratamento de erro com replicador. Agora retorna com
!                a ultima replicacao bem sucedita 

SUBROUTINE READSEC2(unI,tam_sec2,optsec)
!{
!{ Variaveis da interface
   INTEGER,intent(in)::unI
   INTEGER,intent(out)::tam_sec2
   type(sec2type),optional,intent(out)::optsec
!}
!{ variaveis locais
   INTEGER,DIMENSION(15)   :: b(1)
   INTEGER(kind=intk),DIMENSION(15) :: a(1)
   INTEGER ::i,err
   CHARACTER,DIMENSION(3)::sec2
   INTEGER ::antsec2RG     ! Registro anterior ao  inicio da secao2 
   INTEGER ::un,noct
!}

!{ Inicializar variaveis
 un=unI
 A(1)=0
 SUBNAME="READSEC2"
 antsec2RG=currentRG    !Guarda o registro anterior ao inicio da secao2
!}

!{ Ler os 3 primeiros bytes da da secao 2
  do i=1,3
     currentRG = currentRG + 1
     IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(un)) sec2(I)
     IF (IOERR(UN)/=0) RETURN
   END do
!}
!{ Obter valores de cada um dos octetos lidos
 b(1) = 24 !' Tamanho da secao 2 tem  24 bits
 
 Call GET_OCTETS(sec2, 3, b, a, 1, 0,ERR)
 
 IF (ERR < 0) THEN 
    print*,"Erro leitura secao2"
     err=20
     return
 END IF
 tam_sec2=a(1)	! Repassa o tamanho da secao2
 IF (tam_sec2<4) THEN 
   print *,"Error in section 2"
   stop
 END IF

   if (present(optsec)) then !{ Ler a secao 2
      noct=tam_sec2-4 
      allocate(optsec%oct(1:noct),stat=err)
      If (err>0) then 
        err=20
        return
      end if
      optsec%nocts=noct
      currentRG=currentRG+1 ! Pula o byte reservado
      do i=1,noct
        currentRG = currentRG + 1
        IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(un)) optsec%oct(I)
        IF (IOERR(UN)/=0) RETURN
      END do
    !}
    endif
  !{ Posiciona o registro corrente no final da secao2 
    currentRG=antsec2RG+tam_sec2
  !}
END SUBROUTINE readsec2
!}
!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.READSEC3                                  | SHSF!
! ----------------------------------------------------------------------------!
!
!  Esta sub-rotina obtem os descritores da secao 3, 
!
!  Nota: Antes de utilizar esta rotina, certIFique-se que
!        currenteRG esteja apontando para o final da secao 
!        anterior (secao1 ou 2)
! 
!--------------------------------------------------------------
!Octeto      Descriaao
!1-3         Tamanho da Sessaao 3
!4           Zeros (reservado)
!5-6         Nuumero de subsets
!7           Bit 1     : 1= dados observados, 0= outros dados 
!            Bit 2     : 1=  dados comprimidos, 0= dados naao comprimidos 
!            Bit 3 - 8 : Zeros 
!------------------------------------------------------------------------------
! Chamdas Externas: Nao Ha                                                     !
! Chamadas Internas:GET_OCTETS                                                 !
! -----------------------------------------------------------------------------!
! HISTORICO:                                                                   !
!   Versao Original: Sergio H. S. Ferreira                                     !
!______________________________________________________________________________!

 SUBROUTINE READSEC3(UN,tam_sec3max,d,sec3e,tam_sec3,err_sec3) !{
 !{Declaration of variables of the interface
   INTEGER, intent(in)                ::UN
   INTEGER,intent(in)                 ::tam_sec3max ! Tamanho maximo da secao 3 
                                                    ! Se o tamanho REAL da secao3
                                                    ! for maior que tam_sec3max, 
                                                    ! existe um erro na mensagem 
   TYPE(descbufr),pointer,DIMENSION(:)::D           ! Descritores para uso interno
   INTEGER,              intent(inout)::tam_sec3    ! Tamanho da secao 3
   TYPE(sec3TYPE),         intent(out)::sec3e       ! Descritores no formato de saida 
   INTEGER,                intent(out)::err_sec3    ! Erro
!}

!{ Declaration of local variables
   CHARACTER(len=1),  DIMENSION(7):: sec3 ! A identIFicacao da secao 3 tem 7 bytes
   INTEGER(kind=intk),DIMENSION(8):: A1
   INTEGER,           DIMENSION(8):: B1
   CHARACTER(len=1),   allocatable:: sec3b(:)
   INTEGER(kind=intk), allocatable:: A(:)
   INTEGER,            allocatable:: B(:)
   INTEGER :: uni,i ,xx,ib,err,aerr
   INTEGER :: ndesc 
!}

!{ 
 uni=un
 err=0 
 err_sec3=0
 SUBNAME="READSEC3"
 
!'{ Leitura dos 7 primeiros  octetos da secao 3
 do  i = 1,7
  currentRG = currentRG + 1
  IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) sec3(i)
  IF (IOERR(UN)/=0) RETURN 
 END do
!}
!{ Decoficacao dos 7 primeiros bits
  B1(1)=24 ! Tamanho da Secao 3 					  
  b1(2)=8  ! byte reservado   (bite 4)
  b1(3)=16 ! Numero de data subsets  (observacoes em cada registro  BUFR) byte
  b1(4)=1  ! se 1 Indica dados observacionais
  b1(5)=1  ! se 1 Indica dados comprimidos
  b1(6)=1  ! Indentificacao tac data
  b1(7)=5  ! Demais bits =0 para uso funturo
  Call GET_OCTETS(sec3, 7, b1, a1, 6, 0,ERR)
 
  IF (ERR < 0) THEN 
     !print*,"Erro leitura secao3"
     err_sec3=30
  END IF

	 !{ Calculando e alocando espaco para leitura dos descritores
		tam_sec3= a1(1)
		IF(tam_sec3>=tam_sec3max) THEN
			err_sec3=30
			return
		END IF
		ndesc=(tam_sec3-7)/2
		
		if (sec3_is_allocated) then 
                    
		    IF (ASSOCIATED(SEC3E%D)) then 
		       deallocate(sec3e%d)
		       deallocate(D)
		    end if 
		end if
	   
		allocate(D(1:ndesc),STAT=aerr)
			IF(aerr>0) THEN
			print *,"Error during memory allocation for section 3 (D)"
			stop
		END IF
		allocate(sec3b(1:tam_sec3),stat=aerr)
			IF(aerr>0) THEN
			print *,"Error during memory allocation for section 3 (sec3b)"
			stop
		END IF
		allocate(sec3e%d(1:ndesc),STAT=aerr)
			IF(aerr>0) THEN
			print *,"Error during memory allocation for section 3  (sec3e)"
			stop
		END IF
		allocate(A(1:ndesc*4),B(1:ndesc*4),STAT=aerr)
		IF(aerr>0) THEN 
			print *,"Error during memory allocation for section 3 (A,B)"
			stop
		END IF

	        sec3_is_allocated=.true.
	!}
	sec3e%nsubsets = a1(3)
	sec3e%is_obs  = a1(4)    ! se 1 e dados observacional
	sec3e%Is_cpk  = a1(5)	! Se 1 e dados comprimidos
	sec3e%ndesc=ndesc
        sec3e%is_tac=a1(6)           ! Preparacao para uso futuro -> Proposta EVA
 	
 !} Fim da leitura e decodIFicacao dos 7 primeiros bits
 
 
 !{ Verificacao preliminar de  erros de leitura da  secao 3  
   
   IF (tam_sec3<8) THEN 
        ERR_sec3=31 
	deallocate (a,b,sec3b)
	return
   END IF
 !}
  
 	IF(sec3e%nsubsets<1) THEN 
		ERR_sec3=32
		!deallocate (a,b,sec3b)
		!return
	END IF
	
 !{ Leitura dos descritores 
   

   !{ Preparando vetor com o numero dos bits 
     ib=0
     do i=1,Ndesc
      ib=ib+1;b(ib)=2
      ib=ib+1;b(ib)=6
      ib=ib+1;b(ib)=8
     END do
    !}
   xx=tam_sec3-7 ! Numero de octeros que contem os descritores
   !{ LENDo os octetos que contem os descritores
    do  i = 1,xx
      currentRG = currentRG + 1
      IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) sec3b(i)
	  IF (IOERR(UN)/=0) RETURN
    END do
   !} 
   !{ Separando os descritores 
    Call GET_OCTETS(sec3b, xx, b, a, ib,0, ERR)
	ib=0
	do i=1,ndesc
	 
	 ib=ib+1;D(i)%f=a(ib)
	 ib=ib+1;D(i)%x=a(ib)
	 ib=ib+1;D(i)%y=a(ib)
         D(I)%K=.false.
         D(i)%a=0
         D(I)%af=.false.
	 sec3e%d(i)=d(i)%y+d(i)%x*1000+d(i)%f*100000
	END do
   !}
 !} Fim da Leitura dos descritores   
 deallocate (a,b,sec3b)
!}

END SUBROUTINE readsec3
 
 
 
!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.READSEC4B                                 | SHSF!
! ----------------------------------------------------------------------------!
! Esta subrotina "le" os dados da secao 4 (nao compactada e sem replicadores  ! 
!  atrasados).                                                                !
!                                                                             !
!                                                                             !
!  Nota: Antes de utilizar esta rotina, certIFique-se que:                    !
!                                                                             !
!       a) currenteRG esteja apontando para o final da secao                  !
!        anterior (secao 3)                                                   ! 
!       b) Nao esteja sendo utilizados descritores replicadores atrasados     !
!                                                                             ! 
!-----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha					              !
! Chamadas Internas:GET_OCTETS,tabc_setparm,cval                              !
! ----------------------------------------------------------------------------!
! HISTORICO:							
 
SUBROUTINE READSEC4b(UN,D,ndesc,nsubset,sec4e,tam_sec4,erro4b)

!{ Variaveis da Interface										

    TYPE(descbufr),pointer,DIMENSION(:) ::D       ! Descritores BUFR
    INTEGER,                 intent(in) ::UN	  ! Unidade de leitura 
    INTEGER,                 intent(in) ::ndesc   ! Nuumero de descritores
    INTEGER,                 intent(in) ::nsubset ! Nuumero de "subsets"  
    TYPE(sec4TYPE),          intent(out)::sec4e   ! Dados da secao 4   
    INTEGER,                 intent(out)::tam_sec4! tamanho da secao 4 (bytes)
    INTEGER,                 intent(out)::erro4b  ! Codigo de erro na leitura da secao 4
!}

!{ Variaveis locais
 
 INTEGER(kind=intk),DIMENSION((nsubset*(ndesc+5)*4))::A ! Vetor para receber os dados decodIFicados
 INTEGER,DIMENSION((nsubset*(ndesc+5)*4))::B   ! Nuumero de bits de cada valor de A(:)
 
 CHARACTER(len=1),allocatable :: sec4(:)       ! Vetor para receber os octetos da secao 4
 
 !}
 !{ Variaveis auxiliares 
	CHARACTER(len=1),DIMENSION(4)::auxsec4       
	INTEGER :: uni	,k,err,j  ,i,xx
 !}

 !{ Inicializacao de variaveis
	uni=un
	SUBNAME="READSEC4b"
	erro4b=0
       
        if (associated (sec4e%r)) deallocate(sec4e%r)
        if (associated (sec4e%d)) deallocate(sec4e%d)
        if (associated (sec4e%c)) deallocate(sec4e%c)
        if (associated (sec4e%k)) deallocate(sec4e%k)
        if (associated (sec4e%a)) deallocate(sec4e%a)
	!deallocate(sec4e%r,sec4e%d,sec4e%c,sec4e%k,sec4e%a)

        allocate(sec4e%a(1:ndesc,1:nsubset))
	allocate(sec4e%r(1:ndesc,1:nsubset),sec4e%d(1:ndesc,1:nsubset),sec4e%c(1:ndesc,1:nsubset),stat=err)
        if (err==0)allocate(sec4e%k(1:ndesc,1:nsubset),stat=err)
	IF (err>0) THEN
		print *,"Error during memory allocation for section  4"
		stop
	END IF
 !}

!'{ Leitura dos 4 primeiros  octetos da secao 4
 do  i = 1,4
  currentRG = currentRG + 1
  IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) auxsec4(i)
  IF (IOERR(UN)/=0) RETURN
 END do
!}


!{ Obtem o tamanho da secao 4 
 b(1)=24	  ! Tamanho da secao 4 Ainda naao e conhecido 
 b(2)=8        	  ! byte reservado (= 0)
  Call GET_OCTETS(auxsec4, 4, b, a, 2,0, ERR)
 
  IF (ERR < 0) THEN 
     print*,"Erro leitura secao4"
     Stop
  END IF

  tam_sec4=a(1)
  xx=tam_sec4 ! Numero de octetos que contem os descritores
  allocate(sec4(1:xx),stat=err)
  IF (err>0) THEN
    print *, "Error during memory allocation for section  4 b"
	stop
  END IF
 !}

 
!{ LENDo os octetos que contem os dados da secao 4
    do  i = 1,xx
      currentRG = currentRG + 1
      IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) sec4(i)
	  IF (IOERR(UN)/=0) RETURN
    END do
!} 
 
!{  Extraindo os valores do octetos lidos
	k=0
	do i=1,nsubset
		call tabc_setparm(err=err)
	
		do j=1, ndesc
                                    
			k=k+1
			b(k)=bits_tabb2(d(j))
      
		END do
	END do
	Call GET_OCTETS(sec4, xx, b, a, k,0, ERR)

	IF (ERR<0) THEN 
		erro4b=41
		deallocate(sec4)
		return
	END IF
		
!}   
!{ Decodificando valores 
	 k=0
	 
	 do i=1,nsubset
	   j=0
	do while  (j<ndesc)
	   j=j+1
	   k=k+1
		
	 !  Converte os valores inteiros em BUFR A(K)
	 !   para valores reais usando "CVAL". 
	 !   Caso A(K) seja um valor nulo, entao 
	 !   o valor REAL tambem sera nulo
	 !
	 !   Caso A(k) seja relativo a um descritor da tabela C
	 !    CVAL retorna um valor nulo 
	 !{
	   sec4e%d(j,i)=d(j)%f*100000+d(j)%x*1000+d(j)%y
	   sec4e%c(j,i)=d(j)%i	  
	   sec4e%r(j,i)=CVAL(A(K),d(j))
           sec4e%k(j,i)=d(j)%k
           sec4e%a(j,i)=d(j)%a
	   !}
	   END do
	 END do
 !} 
 ! Fim da leitura da secao 4
   sec4e%nvars=ndesc
   deallocate(sec4)
END SUBROUTINE READSEC4b

!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.READSEC4RD2                               | SHSF!
! ----------------------------------------------------------------------------!
! Esta subrotina "le" os dados da secao 4 (nao compactada e COM replicadores  ! 
!  atrasados).                                                                !
!                                                                             !
!                                                                             !
!  Nota: E necessario que:                                                    !
!                                                                             !
!       a) currenteRG esteja apontando para o final da secao                  !
!        anterior (secao 3)                                                   ! 
!       b) Nao esteja sendo utilizados descritores replicadores atrasados     !
!---------------------------------------------------------------------------- ! 
! Um dos problemas desta rotina e que nao se sabe a priore o tamanho dos
! vetores A e B que serao necessarios, visto que este tamanho depende dos
! replicadores atrasados que serao lidos, isto e, depende do numero 
! de vezes em que cada variavel e lida. 
! Por isto o tamanho do A e B e estimado como sendo 8x o tamanho da secao 4,
! que e o maximo numero de variaveis possiveis. 
!
! O que realmente e dificil saber e o tamanho da estrutura 
! de dados sec4e, pois cada subset  pode ter um tamanho 
! diferente, dependendo  dos fatores de replicacao dentro 
! de cada subset. Neste caso sec4e e alocado com base no 
! numero de descritores maximos fornecidos pelo programa 
! principal
! 
!-----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha						      !							  !
! Chamadas Internas:GET_OCTETS,tabc_setparm,cval                              !
! ----------------------------------------------------------------------------!


SUBROUTINE READSEC4rd2(UN,D,ndesc,nsubsets,ndxmax,sec4e,tam_sec4,errrd)

!{ Declaracao das variaveis de interface
 TYPE(descbufr),pointer,DIMENSION(:)::D       ! Descritores BUFR
 INTEGER,                 intent(in)::UN				   !
 INTEGER,                 intent(in)::ndesc   ! Nuumero de descritores
 TYPE(sec4TYPE),         intent(out)::sec4e   ! Dados da secao 4
 INTEGER,                intent(out)::tam_sec4
 INTEGER,                 intent(in)::nsubsets
 INTEGER,                 intent(in)::ndxmax  ! Nuumero maximo de descritores expandidos 
 INTEGER,                intent(out)::errrd   ! Coodigo de erro

!}

!{ Declaracao das variaveis locais 
 INTEGER(kind=intk),allocatable     ::A(:)
 INTEGER,allocatable                ::B(:) ! Vetor de Valores e Nuumero de bits de cada valor 
 INTEGER(kind=intk),DIMENSION(3)    ::A1 
 INTEGER,DIMENSION(3)               ::B1    ! Vetor de valores e nuumero de bits dos primeiros 3 bytes da secao 
 CHARACTER(len=1),allocatable       ::sec4(:)   ! Octetos da secao 4 (nao confundir com sec4e) 
 CHARACTER(len=1),DIMENSION(4)      ::auxsec4          
 INTEGER                            ::uni
 INTEGER                            ::k,k0,kf   !Octento corrente, Octeto Inicial em cada subset, Octeto Final
 INTEGER                            ::err
 INTEGER                            ::j
 INTEGER                            ::i          ! Numero do subset
 INTEGER                            ::l          ! O mesmo que k, mas e usado para controle de replicador atrasado
 INTEGER                            ::xx         ! number of octets of section 4 ( minus the initials octets )
 INTEGER                            ::nvars,IFinal,ifinal_ant
 TYPE(descbufr)                     ::repdelayed
 TYPE(descbufr),pointer,DIMENSION(:)::dx
 INTEGER                            ::repfactor
 INTEGER                            ::nvars_maxsubset ! Numero de variaaveis do maior subset
 integer                            ::ii            !Auxiliar 
 integer                            ::rbits         !Bits restantes
 integer                            ::rr           
 !}

 uni=un
 errrd=0
 SUBNAME="READSEC4RD2"

 if (associated(sec4e%d)) deallocate(sec4e%d)
 if (associated(sec4e%r)) deallocate(sec4e%r)
 if (associated(sec4e%c)) deallocate(sec4e%c)

 !deallocate(sec4e%d,sec4e%r,sec4e%c)
 allocate(sec4e%d(1:ndxmax,nsubsets),sec4e%r(1:ndxmax,1:nsubsets),sec4e%c(1:ndxmax,1:nsubsets),STAT=err)
  sec4e%r(:,:)=0.0
  sec4e%d(:,:)=0
  sec4e%c(:,:)=0
  sec4e%a(:,:)=0
 

 IF (err>0) THEN 
	print *,TRIM(SUBNAME),": Error during memory allocation"
	print *,"sec4e%d(ndxmax,nsubsets)"
	print *,"sec4e%r(ndxmax,nsubsets)"
	print *,"sec4e%c(ndxmax,nsubsets)"
	print *,"ndxmax=",ndxmax
	print *,"nsubsets=",nsubsets

	stop
 END IF

  !if (associated(dx)) then
  !    deallocate (dx)
  !end if      
  allocate(dx(1:ndxmax),STAT=err)
  
 IF (err>0) THEN 
	print *,TRIM(SUBNAME),":  Error during memory allocation"
	stop
 END IF

! ---------------------------------------------
!Get size of section 4
!--------------------------------------------
!{ reading of the 4 previous octets of section 4

 do  i = 1,4
  currentRG = currentRG + 1
  IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) auxsec4(i)
  IF (IOERR(UN)/=0) goto 800

 END do
 b1(1)=24	  	  ! Tamanho da secao 4 Ainda naao e conhecido 
 b1(2)=8        	  ! byte reservado (= 0)
!}
!{ getting size  
  Call GET_OCTETS(auxsec4, 4, b1, a1, 2,0, ERR)
 
  IF (ERR < 0) THEN 
     print*,":MBUFR:READSEC4RD: Fatal Error!"
     Stop
  END IF

  tam_sec4=a1(1)

 !}
 !}
 !-----------------------------------------------------
 ! Alocando espaco e lendo todos os octetos da secao 4
 !----------------------------------------------------
 !{
  xx=tam_sec4-3 ! Number of octets with data (the number of octets  minus 3)
  !allocate(a(1:xx*8),b(1:xx*8),sec4(tam_sec4),STAT=err)
  allocate(a(1:ndxmax),b(1:ndxmax),sec4(tam_sec4),STAT=err) ! pre-allocation was changed from xx*8 to ndxmax
  IF (err>0) THEN 
   print *,":MBUFR:Fatal error during memory allocation (readsec4rd)"
   print *,"a(xx),b(xx),sec4(tam_sec4)"
   print *,"xx=",xx
   print *,"tan_sec4=",tam_sec4

   stop
 END IF
 
 
    !{ lendo os octetos que contem os dados da secao 4
    do  i = 1,xx
      currentRG = currentRG + 1
      IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) sec4(i)
      IF (IOERR(UN)/=0) goto 800 
	     
    END do

   !} 
!}

!----------------------------------------------
!  Extraindo os valores do octetos lidos
!
! Nota: Sao feitas varias varreduras com intensao
!       de obter todos os fatores de replicacao
!----------------------------------------------

 nvars_maxsubset=0
 k=0
 k0=k
  
 do i=1,nsubsets

    call tabc_setparm(err=err)
    IFinal=0     ! restar in each subset - restart the delayed replication expansion 
    ifinal_ant=0

444  	call expanddesc3(d,ndesc,ndxmax,dx,nvars,IFinal,err)
	if (err>0) then 
           deallocate(a,b,sec4,dx)
	    errrd=err
           return
         end if
         
         l=0
	 if (verbose>=3) then 
	        print *,"   *** The last expanded variables after delayed replitor  ***"
	        do j=nvars-1,nvars
		write(*,'(1x,i10,">",i1,"-",i2.2,"-",i3.3)')j,dx(j)%f,dx(j)%x,dx(j)%y
		end do
	 end if 
         IF (xx*8<IFinal) THEN 
            if (verbose==3) then 
	       print *," :MBUFR_ADT: Warning: The Number of expanded descriptor was higher than expected =",IFinal,">",xx*8
	    end if
         END IF

        !---------------------------------------------------
        ! Preparando vetor de bits para chamada de GET_OCTETS
	!----------------------------------------------------
         do j=1, IFinal
            k=k+1
            l=l+1
            b(k)=bits_tabb2(dx(j)) !<== Numero de bits f(k), Descritor f(j) variaveis/descritore
           ! SHSF - REVISAR  print *,"mmm=",j,">",dx(j)%f,dx(j)%x,dx(j)%y,">>",b(k),tabc%dbits,tabb(dx(j)%f,dx(j)%x,dx(j)%y)%nbits
                    
          END do
	
	!-------------------------------------------------------
	! Chamada de GET_OCTETS para obter os valores (a)
        !--------------------------------------------------------
	
          Call GET_OCTETS(sec4, xx, b, a, k,k0, ERR)
	                  !|    |   |  |  |  |   +--- Err   !Codigo de erro 
	                  !|    |   |  |  |  +--------APOS  !posicao anterior (entreda) 
	                  !|    |   |  |  +-----------NA    !Numero de Descritores e elementos em A_Bits    
	                  !|    |   |  +--------------A_VAL !Valores obtidos (saida)
	                  !|    |   +-----------------A_BITS!Sequencia de bits (entrada)  
	                  !|    +---------------------noct  !Numero de octetos  
	                  !+------------------------- oct   !Octetos da secao 4 
	
	   IF (ERR<0) THEN 
             if (verbose>1) then 
		     print *," :MBUFR-ADT: Error 63! ",ERROMESSAGE(63)             
                     call ERRORLOG(un,63,dx,ifinal, ERROMESSAGE(63),a,K0,i)
             end if
                    !print *,"Ifinal,nvars=",ifinal,nvars
                    nvars=ifinal_ant    
		    IFinal=nvars
                    errrd=63
                    deallocate(a,b,sec4,dx)
                    return
	   END IF
	   
	  ! So para verificacao  
	  ! do ii=k0,k
	  !   print *,"subset,i,dx,bits,value=",i,ii,dx(ii),b(ii),a(ii)
	  ! end do   
	  ! print *,"------------------------",k,k0  
  !----------------------------------------------------------------------------!
  !  Verifica se a expansao foi completa ou se parou em um replicador atrasado.! 
  !  Caso tenha parado em um replicador atrasado (delayed replicator), entao   !
  !  pega o fator de replicao, converte em um replicador normal, e reprocessa  !
  !  a expansao e a leitura (volta a 444)                                       !
  !                                                                            !
  ! Nota:                                                                      !
  !    Caso o fator de replicacao for zero, entao e um replicador nulo e os    !
  !   descritores que seguem devem ser eliminados no subset corrente           !
  !----------------------------------------------------------------------------!
  !{

   IF ((IFinal<nvars).and.(errrd==0)) THEN
    repfactor=a(k)
   !{ No caso replicador atrasado de 1 bit, null = 1 
    if ((repfactor==null).and.(b(k)==1)) then 
       repfactor=1 
       a(k)=1
    end if
    !}
    !{ Caso o replicador seja missing entao assume vmax_numbits
     if (repfactor == null ) then 
       repfactor = 0 !vmax_numbits(b(k))
       a(k)=repfactor
       if (verbose>1) call ERRORLOG(un,64,dx,ifinal, ERROMESSAGE(64),a,k0,i) 
       errrd=64 
       deallocate(a,b,sec4,dx)
       return     
    end if
    !}
    if ((repfactor<0).or.(repfactor>xx*8)) then 
      if (verbose>1) then 
       print *,"Error 65: ",trim(ERROMESSAGE(65))
       call ERRORLOG(un,65,dx,ifinal, ERROMESSAGE(65),a,k0,i)
             print *," REPLICATION FACTOR=",repfactor
       end if    
      errrd=65 
      deallocate (a,b,sec4,dx)
      return
    end if 

    repdelayed%f=dx(l-1)%f
    repdelayed%x=dx(l-1)%x
    repdelayed%y=repfactor
    dx(l-1)=dx(l)
    dx(l)=repdelayed
    k=k0  !<==Volta ao inicio do subset

    !{Quando o Fator de Replicao e igual a zero, entao
    ! elimina o replicador e os descritores replicados
    If (REPFACTOR==0)then
       call remove_desc(dx,nvars,ifinal,ifinal+dx(ifinal)%x)
       ifinal=1 ! Volta ao inicio 
      ! para eliminacao
   end if

   ifinal_ant=ifinal
   goto 444
 END IF
 !} 

 !-----------------------------------------------------------------------------!  
 ! Se chegou aqui e porque todos os replicadores atrasados do "subset" i foram !
 ! encontrados e convertidos em replicadores normais. Portanto, a partir deste !
 ! ponto inicia-se a leitura dos dados                                         !
 !-----------------------------------------------------------------------------!
 !{ 
    if (verbose>=3) print *," :MBUFR: All delayed replicators Ok in subset =",i 
      
    !{Zerando a estrutura de dados da secao 4
	   sec4e%d(:,i)=0 !=Null Zerando a estrutura de dados da secao 4
     !}

     !{ Decodificando valores  
	kf=k
	k=k0
	k0=kf	 
	j=0
	do while  (j<IFinal)
		j=j+1
		k=k+1
	
	   ! Se for um descritor da tabela B	obtem o valor, 
	   ! se nao for, atribui valor “missing” 
	   ! Nota: Somente os descritores da tabela B representam
	   ! valores gravados na secao 4
	   !{ 
	    sec4e%d(j,i)=dx(j)%f*100000+dx(j)%x*1000+dx(j)%y
	    sec4e%c(j,i)=dx(j)%i
	    sec4e%r(j,i)=CVAL(A(K),DX(J))
            sec4e%k(j,i)=dx(j)%k
            sec4e%a(j,i)=dx(j)%a
           !}
	END do

	IF (IFinal>nvars_maxsubset) nvars_maxsubset=IFinal
	sec4e%nvars=nvars_maxsubset
        rbits=bits_totalizer (xx,b,kf,k0)
        rr= int(real(rbits)/8.0)+1
        rbits=8-mod(rbits,8)

        ! ROTINA EM TESTE
        ! COMPLETA NUMERO DE BITS NO FINAL DO SUBSET
        ! 1 - CASO HAJA ALGUM DESCRTITOR 2-04-YYY (ASSOCIATED FIELD)  
        if( subset_byte_completed) then 
        if (rbits>0) then 
            k=k+1
            b(k)=rbits
            a(k)=vmax_numbits(rbits)
            sec4e%d(j,i)=9*100000+88*1000+888
	!    sec4e%c(j,i)=dx(j)%i
	    sec4e%r(j,i)=real(a(k))
         !   sec4e%k(j,i)=dx(j)%k
         !   sec4e%a(j,i)=dx(j)%a
         k0=k
         kf=k
        end if
        end if    
	!So para verificacao 
	!print *,"k0,k,kf=",k0,k,kf
     !} Fim da decodIficaccaao (proximo subset) 

END do

!} Fim dos subsets


 
 ! Fim da leitura da seccaao 4
800 deallocate(a,b,sec4,dx)
SUBNAME=""

END SUBROUTINE READSEC4rd2


!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.READSEC4CMP                               | SHSF!
! ----------------------------------------------------------------------------!
! OBJETIVO: LEITURA DA SECAO 4  COMPACTADA
!-----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:GET_OCTETS,tabc_setparm,cval                              !

SUBROUTINE READSEC4CMP(UN,D,ndesc,nsubset,sec4e,tam_sec4,errsec4)
!{ Variaveis da interface
   INTEGER,                intent(in) ::UN      !
   TYPE(descbufr),pointer,DIMENSION(:)::D       ! Descritores BUFR
   INTEGER,                intent(in) ::ndesc   ! Numero de descritores
   INTEGER,                intent(in) ::nsubset ! Numero de subsets      
   TYPE(sec4TYPE),         intent(out)::sec4e   ! Dados da secao 4
   INTEGER,                intent(out)::tam_sec4 ! Tamanho da secao 4 (section 4 size)
   INTEGER,                intent(out)::errsec4  ! Error code
!}
!{ Variaveis locais
   INTEGER(kind=intk),DIMENSION((nsubset*(ndesc+5)*5))::A 
   INTEGER,DIMENSION((nsubset*(ndesc+5)*5))::B       ! Valores e respectivos numeros de bit 
   CHARACTER(len=1),allocatable            ::sec4(:) ! secao 4 
   CHARACTER(len=1),DIMENSION(4)           ::auxsec4          
   INTEGER(kind=intk)                      ::vmini   ! Auxiliar para valor minimo (inteiro)
   INTEGER                                 ::bbit    ! Auxiliar para quantidade de bits compactos
   integer                                 ::ndesc2  ! O mesmo que ndesc
   REAL                                    ::VMINI2
   integer                                 ::diff
   integer                                 ::nbytes  ! Numero de bytes de uma variavel caracter CITTIA5
   integer                                 ::nbytes2 ! Numero de bytes de uma variavel caracter CITTIA5
   INTEGER                                 ::uni,k,err,j ,i ,l,m,k0
   INTEGER                                 ::xx,kmax,aerr,kmax2  
 !}
 !{Iniciar variaveis e parametros
	uni=un
	SUBNAME="READSEC4CMP"
	errsec4=0
	kmax=ubound(A,1)
	kmax2=0
	call tabc_setparm(err=err) !Inicial parametros da tabela C	

!}
!------------------------------------------------------------------------------
!Obter tamanho da secao 4
!------------------------------------------------------------------------------
! {Leitura dos 4 primeiros  octetos da secao 4
	do  i = 1,4
		currentRG = currentRG + 1
		IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) auxsec4(i)
		IF (IOERR(UN)/=0) RETURN
	END do
!}
!{Obter tamanho da secao 4
 b(1)=24     ! Tamanho da secao 4 Ainda naao e conhecido 
 b(2)=8      ! byte reservado (= 0)
 
 Call GET_OCTETS(auxsec4, 4, b, a, 2,0, ERR)
  IF (ERR < 0) THEN 
     errsec4=42
     print *,"ERROR 42:",trim(ERROMESSAGE(42))
     return 
  END IF
  tam_sec4=a(1)
  xx=tam_sec4-3+1 ! Numero de octetos que contem os dados +1 (tolerancia)  

!------------------------------------------------------------------------------
! LER TODA A SECAO 4
!------------------------------------------------------------------------------
!{Alocando sec4
   allocate(sec4(1:xx),stat=aerr)
   IF (aerr>0) THEN 
      print *,"Error during memory allocation for section  4"
      stop
   END IF
  !}
  !{ LENDo os octetos que contem os dados da secao 4
    do  i = 1,xx
      currentRG = currentRG + 1
      IF (IOERR(UN)==0) read(un,rec= currentRG,IOSTAT=IOERR(UN)) sec4(i)
       IF (IOERR(UN)/=0) RETURN 
    END do
   !} 

! {  Extraindo os valores dos octetos lidos (compactados)
! 
 k=0 
 !{ Nesta fase obtem-se todos os valores minimos
 !  para compactacao e tambem os numeros de  bits
 !  de cada valor arquivado 
 j=0
 ndesc2=ndesc
 do while (j<ndesc2)
    j=j+1
    !---------------------------------------------------------
    !ObtENDo o numero de bits de todos os dados, ou seja, 
    !ObtENDo o numero de bit de cada  descritor j da tabela B
    !---------------------------------------------------------
    !
      IF (d(j)%f==0) THEN

         !{*** Caso variavel numerica
          if (d(j)%i==0) then 
              k=k+1;b(k)=bits_tabb2(d(j))
              Call GET_OCTETS(sec4, xx, b, a, k, K-1,ERR) !**** Leitura do valor minimo (min value)
              IF (ERR < 0) then
                  if (verbose>1) then
                      print *,"Warning! Error reading and decompressing sec4 data "
                      print *,"Descriptor ",j,"codigo=",d(j)%f,d(j)%x,d(j)%y
		          end if
                  ndesc2=j
              end if
              vminI=a(k)   !' Valor minimo
           !}
           !{ Leitura do contador de bits (bit counter)
              k=k+1;  b(k)=6
              Call GET_OCTETS(sec4, xx, b, a, k, K-1,ERR)
              IF (ERR < 0) THEN 
                 errsec4=43
                 if (verbose>1) then
                    print *,"ERROR 43: ",trim(ERROMESSAGE(43))
                    print *,"Descriptor ",j,"codigo=",d(j)%f,d(j)%x,d(j)%y
                 end if
                 ndesc2=j
                 bbit=0
              else
                 kmax2=k
                 bbit=a(k)   !' Numero de bits
              END IF  
             !{ ***Verifica consistencia do valor minimo e do contador de bits
              if (bbit<0) then 
                  bbit=0
                  if (verbose>1) then
                      print *,"Warning! Error reading and decompressing sec4 data "
                      print *,"Bit counter = null "
                      print *,"Descriptor ",j,"codigo=",d(j)%f,d(j)%x,d(j)%y
                  end if
                 ndesc2=j
               END IF
               if ((vmini<0).and.(bbit>0)) then
                  !errsec4=1
                  if (verbose>1) then
                      print *,"Warning! Error reading and decompressing sec4 data "
                      print *,"Minimu Value  = null and bit couter >0)"
                      print *,"Descriptor ",j,"codigo=",d(j)%f,d(j)%x,d(j)%y
                  end if
                  bbit=0
              end if          
             !}


                VMINI2=CVAL(vmini,D(J))
                diff=b(k-1)-bbit+tabc%dbits
                IF (diff<-1000) THEN
                  ! Espera-se que o nmero de bits compactados sejam 
                  ! Menores que o nmero de bits definidos na tabela B
                  ! do Contraario nao haveria sentido em processar o 
                  ! BUFR compactado. Assim sENDo esta informacao e
                  ! suspeita de erro de leitura/codIFicacao BUFR
                  !{
                    print *,":MBUFR-ADT:Warning!  Compressed variable bigger than original variable"
                    print *," (",d(j)%f*100000+d(j)%x*1000+d(j)%y,") Diff=",diff,"j=",j  
                    bbit=0   
                  !}
                end if 

                !{ Replicando o numero  de bits para todos os subsets (elementos do descritor j)
                  do i=1,nsubset
                    k=k+1
                    b(k)=bbit
                  END do
                !}
              !}
             else 

              !Caso variavel caracter: 
              ! <<< Neste caso nao precisamos fazer a leitura 
              !do valor minimo, pois os dados caracteres nao sao 
              !comprimidos.
              !-----
              ! Mudado: Procurar documentacao WMO 
              !   Podemos ter variaveis caracter comprimidas 
              !>>>>>
              !No lugar do numero de bits e informado e o numero 
              !de bytes, mas nao podemos calcular estes valores a partir do 
              !da definicao do numero de bits do descritor 
              !
              ! Como trabalhos com subdescritores para variaveis caracteres
              ! O que fazemos e apenas atribuir os valores 8 a  b(k) diretamente 
              ! a cada subdescritor  
              !{
         
               nbytes=tabb(d(j)%f,d(j)%x,d(j)%y)%nbits/8
                do l=1,nbytes
                 k=k+1;b(k)=8
                end do
                j=j+nbytes-1
                k=k+1;  b(k)=6     
                Call GET_OCTETS(sec4, xx, b, a, k, K-nbytes,ERR)
                IF (ERR < 0) THEN 
                  errsec4=44 
                  print *,"ERROR 44: ",TRIM(ERROMESSAGE(44))
                   deallocate(sec4)
                  return
                END IF 
               
                IF (A(K) < NBYTES) then 
                     nbytes2=A(K)            ! << - Caracter comprimido
                elseif(a(k)==nbytes) then
                     nbytes2=nbytes          !<< - Caracter normal 
                else 
                  errsec4=44 
                  print *,"ERROR 44: ",TRIM(ERROMESSAGE(44))
                  write (*,'(" Descriptor=",i1.1,i2.2,i3.3," Diff=",i4," Position=",i4)')d(j)%f,d(j)%x,d(j)%y,diff,j  
                  Print *,"Normal Number of bytes=",nbytes
                  print *,"Number of bytes after compression=",a(k)
                  bbit=0   
                  deallocate(sec4) 
                  return
                end if
         !      print *,"nbytes2,nbytes,j=",nbytes2,nbytes,j," (",d(j)%f*100000+d(j)%x*1000+d(j)%y,")"
         !      stop
               !{ Replicando o numero  de bits para todos os  sub-conjuntos do descritor j 
                do i=1,nsubset
                  do l=1, nbytes
                     k=k+1
                     if (l<=nbytes2) then 
                      b(k)=8
                     else 
                      b(k)=0
                    end if
                  end do 
                END do
              !}
                 
             end if
          !}
           ELSE
          !No caso de descritor da tabela C, nao processa-se o 
          ! salto para o proximo B(k) e A(k). 
          ! O mesmo procedimento e utilizado na descompactacao mais adiante
          ! afim de compensar o numero menor de A(k) em relacao ao numero 
          ! de descritores
          !
          CALL tabc_setparm(d(j),err) 
          !IF (err/=0) THEN 
          !  print *,"Erro tabela C"
          !END IF
       
        END IF
      END do

  ! FASE 2
  !----------------------------------------------------------------------------!
  ! Neste ponto k representa o numero total de                                 !
  ! elementos em sec4, incluindo valores minimos (vmin)                        !
  ! e numero de bits. O vetor b(k) contem o numero de bits                     !
  ! de todos estes elementos.                                                  !
  !                                                                            !
  ! Esta ultima chamada de get_OCTETS e realizada para                         !
  ! obter todos os dados compactados para em seguida ser feita                 !
  ! a descompactacao                                                           !
  !----------------------------------------------------------------------------!
   
  if (err<0) then
    kmax=kmax2
  else
    kmax=k
  end if
  Call GET_OCTETS(sec4, xx, b, a, kmax,0, ERR)

  if (err<-12) then 
    
    errsec4=42
    print *,"ERROR 42: ",TRIM(ERROMESSAGE(42))
    DEALLOCATE(SEC4)
    return
  end if  

   err=0 
   !{ **** Descompactacaao *****  
   k=0
   j=0
   do while (j< ndesc2)
      j=j+1
      if (d(j)%F==0) then
       !{Se for descritor da tabelA B

        IF (d(j)%i==0) THEN
           
          !{ Processamento de variaveis numericas 
            k=k+1
            vmini=a(k)
            k=k+1     ! < Salta o numero de bits que agora nao e mais necessario
            do i=1,nsubset
              k=k+1
              IF ((vmini.ne.null).and.(a(k).ne.null)) THEN 
                sec4e%r(j,i)=CVAL(a(k)+vmini,d(j))
              ELSE
                sec4e%r(j,i)=null
              END IF
              sec4e%d(j,i)=d(j)%f*100000+d(j)%x*1000+d(j)%y
              sec4e%c(j,i)=d(j)%i
              sec4e%a(j,i)=d(j)%a
            END do
          !}

        else  
          !{ Processamento de variaveis caracteres (CCITT IA5 ) 
            !{ Salta o valor minimo
              K0=K
              k=k+1 
              nbytes=tabb(d(j)%f,d(j)%x,d(j)%y)%nbits/8
              k=k+nbytes
            !}
            !{ pega no numero de bytes
              nbytes2=a(k)  
            !}
            if (nbytes2==0) then
               k=k0    !* Retorna para a posicao do valor minimo.  Vai ser necesario
               !*** Aqui pega o valor minimo da variavel caracter e coloca em todos os subsets
               !{
               do l=j,nbytes+j-1
                 k=k+1
                 !print *,d(l)%f*100000+d(l)%x*1000+d(l)%y,char(a(k))
                 do i=1,nsubset
                     sec4e%d(l,i)=d(l)%f*100000+d(l)%x*1000+d(l)%y
                     sec4e%c(l,i)=d(l)%i
                     sec4e%k(l,i)=d(l)%k
                     sec4e%a(l,i)=d(l)%a
                     sec4e%r(l,i)=a(k)
                 end do

              end do
              k=k+1  ! Salta novamente o numero de bits !?
              !}
              !*** Mas ainda tem a(k) sobrando para cada subset e
              !    seus respectivos subdescritores
              !{
              do i=1,nsubset
                    do l=j,nbytes+j-1
                     k=k+1
                    end do
              end do

              !}
             else

             !{Caso nbytes2> 0 entao carrega nbytes2 em nbytes subdescritores, o resto
             ! preenche-se com espaços
             !if (nbytes2>0) then
                 do i=1,nsubset
                    m=0
                    do l=j,nbytes+j-1
                       m=m+1
                       if (m<=nbytes2) then
                         k=k+1
                         if ((a(k)<32).or.(a(k)>128)) a(k)=32
                         sec4e%r(l,i)=a(k)
                       else
                         k=k+1
                         sec4e%r(l,i)=32
                       end if
                       sec4e%d(l,i)=d(l)%f*100000+d(l)%x*1000+d(l)%y
                       sec4e%c(l,i)=d(l)%i
                       sec4e%k(l,i)=d(l)%k
                       sec4e%a(l,i)=d(l)%a
                    end do
                  end do
              end if
              j=j+nbytes-1   ! Corre os subsdescritore nsubdescritores=nbytes
             !}
            !}

          end if
        ELSE
         ! Se for descritor da  tabela C, coloca o descritor em sec4%d,  
         !processa tabc_setparm, porem nao salta para  o proximo valor de
         ! a(k), b(k), pois estes nao consideraram  a existencia destes descritores 
         !  Note que os descritores que modificam o numero de
         !  bits nao tem mais efeito nesta parte do programa, pois
         !  estes valors ja foram utilizados para leitura dos bits
         !  no arquivo BUFR. 
         !  Os fatores de escala e referencia sao usados apenas em CVAL
         !  para obter os valores corretos
         !{ 
            CALL tabc_setparm(d(j),err)
            do i=1,nsubset
             sec4e%d(j,i)=d(j)%f*100000+d(j)%x*1000+d(j)%y
             sec4e%k(j,i)=d(j)%k
             sec4e%r(j,i)=null
             sec4e%a(j,i)=d(j)%a
            END do
          !}
        END IF
      END do 

      !{ Salva os descritores no caso de ter havido erro de leitura
      if(ndesc2<ndesc) then 
        do j=ndesc2+1,ndesc
          do i=1,nsubset
            sec4e%d(j,i)=d(j)%f*100000+d(j)%x*1000+d(j)%y
	    sec4e%k(j,i)=d(j)%k
             sec4e%r(j,i)=null
             sec4e%a(j,i)=d(j)%a
          end do
        end do
      end if
      !}
deallocate (sec4)
END SUBROUTINE READSEC4CMP


!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.EXPANDDESC3                               | SHSF!
! ----------------------------------------------------------------------------!
! ROTINA GENERICA DE EXPANSAO DE DESCRITORES 
!
!  Esta rotina recebe os descritores que sao lidos ou gravados 
!  na secao 3 (Descritores da Tabela B e D e replicadores) 
!  Os descritores D sao expandidos para os descritores da tabela B
!  e os replicadores sao utilizados para replicar os descritores
!  da tabela B, com execessao dos descritores replicadores atrasados. 
!
!  Ao final do processo de expansao a maior parte dos descritores 
!  estaram convertidos em descritores da tabela B 
!  (Descritores expandidos) 
!
!  Os descritores naao expandidos por esta rotina saao
!  1) Descritores da tabela C (acima de 2-23-yyy) 
!  2) Descritores replicadores atrasados (delayed replicators)
!
!  No caso dos descritores replicadores atrasados, a rotina interrompe
!  a expansao. Neste caso o vetor dx tera parte dos descritores expandidos
!  e parte naao expandidos. A variavel IFinal guardara o
!  indice do descritor de fator de replicaao, que sucede
!  o replicador atrasado 
!  
!  
!  Erros
!     0 = Nao ocorreu erros 
!     1 = Descritor desconhecido (tabela D)  Erro cod 50
!     2 = Descritor desconhecido (tabela B)	 Erro cod 51
!     3 = Descritor replicador com erro 	 Erro cod 52 
!     4 = Expancao incompleta  (Um erro desconhecido resultou em um 
!                                descritor que nao e da tabela B)
!
! Nota: Esta rotina nao replica um descritor replicador (replicacao recursiva)
!       Embora seja logico a  replicacao recursiva, nao existe nos manuais 
!       da WMO nenhum comentario a respeito desta possibilidade. Tambem nao encontrei
!       ate o momento nenhuma mensagem utilizando deste recurso.  
!       Desta forma, por facilidade de programacao, nao implementei 
!       esta rotina com  replicacao recursiva. 
!
!       
!-----------------------------------------------------------------------------!
! Rotinas Chamadas: 
!   MBUFR( expandsubdesc,replicdesc,expandescD )                      
! Chamado por:
!   MBUFR
! ----------------------------------------------------------------------------!
! HISTORICO
SUBROUTINE expanddesc3(di,ndi,ndxmax,dx,nvars,IFinal,err)

!{ Variaveis de Interface

TYPE(descbufr),pointer,DIMENSION(:)::di      ! Descritores compactos  (compacted descriptors )
INTEGER,intent(in)                 ::ndi     ! Numero de descritores em di (number of descriptors)
INTEGER,intent(in)                 ::ndxmax  ! Numero maximo de descritores expandidos ( Max number of expanded descriptor )
TYPE(descbufr),pointer,DIMENSION(:)::dx      ! Descritores expandidos (expanded descriptor)
INTEGER,intent(inout)              ::nvars   ! Numero de variaveis ou descriptors dx (Number of Variables or dx descriptors)
INTEGER,intent(inout)              ::IFinal  ! Indice do ultimo descritor expandido. (Last expanded descritor index)

! Nota: Se todos os descritores forem expandidos, esta subrotina retorna IFinal=nvars
!       Caso contrario, i.e., caso exista replicadores atrasados, entaao IFinal<nvars
! Note: If all descritors were expanded than this subrotine return IFINAL=nvars
!       Else, i.e. There are delayed replicators then IFINAL< nvars 

INTEGER,intent(inout)::err ! Se 0 Indica que nao houve erro na expancao dos descritores

!}

!{ variaveis locais
   INTEGER                         :: i          ! Indice para descritores nao expandido
   INTEGER                         :: k          ! Indice para descritores expandidos
   INTEGER                         :: j
   INTEGER                         :: nd,idelayed
   TYPE(descbufr),DIMENSION(ndxmax):: dc 
   LOGICAL                         :: exist_tabd
   integer                         :: nloops
   integer                         :: err2
   integer                         :: add        ! Add descriptor
   integer                         :: IdataRef2  ! IdataRef aux
   logical                         :: marker_operator
!}

!{ Inicializa variaveis
	i=0;k=0;j=0;err=0

! Caso seja a continuacao de uma expansAo interrompida 
! anteriormente IFinal sera maior que zero
! Neste caso a variavel di nao e copiada para dc e sim 
! a variavel dx


 j=0
 IF (IFinal==0) THEN 
    dc(:)%i=0
    dc(:)%n=.false.
    dc(:)%a=0
    dc(:)%af=.false.
    do i=1,ndi
      dc(i)=di(i)
      dc(i)%i=0
      dc(i)%n=.false.
      dc(i)%a=0
      dc(i)%af=.false.
      dc(i)%k=.false.
    END do
    nd=ndi
  ELSE 
	
    do i=1,nvars
      j=j+1
      dc(j)=dx(i)
    END do
	
    nd=nvars
    j=0
  END IF
!}


    nloops=0    
    22 IFinal=nd ! Esquece a posicao do ultimo replicador atrasado
    i=0

!{ Expande descritores replicadores

  DO  while (i<IFinal) 
   
    i=i+1
    call replicdesc(dc,i,nd,ndxmax,idelayed,err)
    if (err>0) then 
     if (verbose>1) then 
     ! call ERRORLOG(un,err,dc,nd, "Error in expanddesc3/replicdesc") 
      print *,"Error in expanddesc3/replicdesc. Called from  ",trim(SUBNAME)
      print *,"Error = ",err
      end if 
      I=iFINAL
      return
    end if
   
   !Interrompe a replicacao se encontra replicador atrasado
    IF (idelayed>0) THEN 
      IFinal=idelayed  
      i=idelayed
    ELSE 
      IFinal=nd
    END IF
   
  END do 
!}
   

!{Expandir descritores da tabela D
  
    i=0
    idelayed=0
    exist_tabd=.false.

	DO  while (i<IFinal) 
		i=i+1
		if (dc(i)%f==3) then 
			call expanddescD(dc,i,nd,idelayed,err)
			exist_tabd=.true.
		end if

		IF (err/=0) THEN  
		  I=IFinal 	 
		  return
		END IF
                
               !Interrompe a expansao da tabela D se encontra replicador atrasado
               IF (idelayed>0) then 
                !  print *,">",idelayed,ifinal,i
                  IFINAL=idelayed
                  i=idelayed
                !  write(*,'(i1,i2.2,i3.3)')dc(i)%f,dc(i)%x,dc(i)%y
               end if
	END do 
!}
  
  !---------------------------------------------------------------------------
  ! Repete-se o processo de expansao de replicadores e descritores da tabela D
  ! Ate que nao haja mais descritores da tabela D.
  ! 
  ! Nota: Realiza-se esta repeticao para processar os descritores replicadores 
  ! que eventualmente possam aparecer com a expansao dos descritores da tabela D 
  ! e para certificar que nao haja mais descritores da tabela D
  !-----------------------------------------------------------------------------
  !	{
  nloops=nloops+1
  if (exist_tabd) then 
   if (nloops<100) then
	  goto 22
	else
		ERR=5
		print *," Erro 55: ",ERROMESSAGE(55)
		stop
			
	END IF
	END IF
	

  !}
  
 i=0
 k=0
 j=0
 
 !-----------------------------------------------------
 !Copia todos os descritores de dc para dx e aproveita 
 !para verificar se existe algum descritor invalido da 
 !tabela B e tabela D
 !-----------------------------------------------------
 !{
add=0
 
 DO  while (i<nd)
   i=i+1
   !Caso seja um descritor normal da tabela B verIFica se 
   !e um descritor valido.
   !Se for um descritor da tabela C aplica descritor se for o caso 
   !
   ! Nota. E necessario verIFicar se os descritores
   !       que estao sENDo fornecidos nao excedem as dimensoes
   !       de TABB
   IF (dc(I)%f==0) THEN 	  
      IF (TABB(dc(I)%f,dc(I)%x,dc(I)%y)%nbits==0) THEN 
         IF(i<=1) then
           err=2
           print *," Error 52: ",ERROMESSAGE(52)
         ELSEIF ((dc(i-1)%f==2).and.(dc(i-1)%x==6)) THEN
           err=0
         ELSE
           err=2
           print *," Error 52: ",ERROMESSAGE(52)
	   write(*,'("  Descriptor (",i6.6,")= ",i1,1x,i2.2,1x,i3.3)')i-1,dc(i-1)%f,dc(i-1)%x,dc(i-1)%y
           write(*,'("  Descriptor (",i6.6,")= ",i1,1x,i2.2,1x,i3.3)')i,dc(i)%f,dc(i)%x,dc(i)%y
         END IF
      END IF
      Idataref2=idataref
   ELSE IF(dc(i)%f==2) THEN
      !{ Descritores da tabela C 2-35-000
     if (dc(i)%x==35)  then 
        IDataRef=i  ! 2-35-000 Cancel backwrd data reference. Restart in i position
      end if
   END IF

!{ Copiando os descritores
   ! Se for descritor 2-(23:32)-255 entao processa a substituicao de descritores
   ! ao inves da copia simpes  

    k=k+1
    marker_operator=.false.
    if ((dc(i)%f==2).and.(dc(i)%x==24)) marker_operator=.true.
    if ((dc(i)%f==2).and.((dc(i)%x>22).and.(dc(i)%x<33)).and.(dc(i)%y==255).and.(i<ifinal).and.(.not.marker_operator)) then 
       
        111 IdataRef2=IdataRef2+1  
            
            If ((dc(IDataRef2)%f==2)) then
              i=i-1
              add=add+1
              ifinal=ifinal+1
            Elseif(dc(IDataRef2)%f==0) then 
              if ((dc(IDataRef2)%x==01).or.(dc(IdataRef2)%x==31)) goto 111
            end if

            DX(K)=DC(IdataRef2) 
            if (dc(i)%x==25) dx(k)%n=.true. 
    
    elseif(add>0) then 
      add=add-1
      IdataRef2=IdataRef2+1
      dx(k)=dc(Idataref2)
      i=i-1
      ifinal=ifinal+1
    else
      dx(k)=dc(i)
    end if
   
 
!} 
	
	
300 continue
END do
nvars=k

 ! i=ifinal
 ! print *,"Ifinal Antes assoc>>",I,DX(I)%F,DX(I)%X,DX(I)%Y
 ! i=nvars
 ! print *,"ndesc Antes assoc>>",I,DX(I)%F,DX(I)%X,DX(I)%Y

 call add_associated_descriptor (dx,nvars,ifinal,ndxmax,err2)
 err=err+err2
  call expandsubdesc(dx,nvars,ifinal,ndxmax,err2)
 err=err+err2

 

END SUBROUTINE expanddesc3


!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.EXPANDSUBDESC                             | SHSF!
! ----------------------------------------------------------------------------!
!  Esta rotina expande descritores caracter da tabela B em subdescritores 
!
!   Os descritores da tabela B referem-se normalmente a variaveis numerica 
!   e ao numero de bits que sao armazenados por estas variaveis. 
!   Referen-se tambem a variaveis caracter cujo tamanho em bits sao 
!   multiplos de 8, isto e, nestas variaveis sao gravados
!   1 caracter p/ byte 
!
!   As rotinas de codIFicacao/decodIFicacao binaria esta preparada para
!   gravar numeros ate 4 bytes por vez, mas nao esta preparada para
!   gravar variaveis string que pode superar os 4 bytes facilmente.
!
!   A estrategia utilizada aqui e de subdividir as variaveis string
!   de forma que estas sejam gravadas byte a byte
!
!   Para insto os descritor tambem sao subdivididos em sub-descritores
!
!    Um descritor da tabela B e um descritor do tipo 
!     1-xx-yyy  e representa um numero b de byte 
!
!    Sub-descritores de 1-xx-yyy sao descritores do tipo 
!    1-xx-yyy-ii  que representa apenas 1 byte. Assim sao 
!    necessarios b * (1-xx-yyy-ii) para representar 1-xx-yyy
!    
!    O indice ii e um indice que designa a posicao do caractere
!    se ii=0 entao e um descritor normal
!    se ii=1 e um sub-descritor para o primeiro caractere de 1-xx-yyy
!    de ii=2 e um sub-descritor para o segundo caracter de 1-xx-yyy
!    e assim sucessivamente
!
!    Esta subrotina recebe descritores da tabela B, verifica quais
!    os descritores da tabela B sao do tipo texto e expande 
!    em sub-descritores
!   
!-----------------------------------------------------------------------------!
! Dependendias:  Nao Ha
! Chamada por: expanddesc3
! ----------------------------------------------------------------------------!

SUBROUTINE expandsubdesc(dx,ndesc,ifinal,ndxmax,err)
!{Variaveis de interface
	TYPE(descbufr),pointer,DIMENSION(:)::dx     ! Descritores para a secao 4
	INTEGER,              intent(inout)::ndesc  ! Numero de descritores em dx
	integer,              intent(inout)::ifinal ! Posicao do replicador atrasado
	INTEGER,              intent(in)   ::ndxmax ! Numero maximo estimado de descritores expandidos
	INTEGER,              intent(out)  ::err    ! Codigo de err - se zero nao hove erro
!}
!{ Variaveis locais
	INTEGER                            ::i,c,i2  ! Variaveis auxuliares 
	INTEGER                            ::is_char ! bandeira para descritore texto
	INTEGER                            ::nbytes  ! Numero de bytes
	INTEGER                            ::nbits   ! Numero de bits
	TYPE(descbufr),DIMENSION(ndxmax)   ::dc
	integer                            ::ii
        integer                            ::NumChar  ! Numero de caracteres definidos pela tabela C (2-08-y)
!}
!1 - Inicializando variaveis
!{
	is_char=0
	err=0
        numChar=0
 
!}
!1- Verifica se existe algum descritor texto que ainda
!   nao foi subdivido. Caso nao exista, retorna sem modificar dx
!{
	i=0
	do while (I < ifinal)
		i=i+1
		IF (dx(i)%f==0) THEN 
		IF ((tabb(dx(i)%f,dx(i)%x,dx(i)%y)%U==1).and.(dx(i)%i==0)) THEN 
			i=ndesc
			is_char=1
		END IF
		END IF
	END do
	IF (is_char==0) return 
	
 !}
 !2- expande sub-descritores
 !   Modificacoes: Nao expandir em subdescritores os seguintes casos: 
 !   a) Quando os descritores que ja sao subdescritores
 !   b) Os descritores apos um replicador pos-posto (deleyed replicator)
 !{
    C=0
    DO I=1,ifinal

      !{ Verifica auteracao no numero de caracter pelo descritor  2-08-yyy
      if ((dx(i)%f==2).and.(dx(i)%x==8))  then 
        NumChar=dx(i)%y
      end if
      !}

      IF (dx(i)%f==0) THEN 
        !{ Se for descritor da tabela B processa esta parte
           IF ((tabb(DX(I)%f,dx(i)%x,dx(i)%y)%U==1).and.(dx(i)%i==0)) THEN 
            !{ Se alem de ser descritor da tabela B tambem for um descritor caracter sem subdescritores
              
              !{ Obtem Numero de caractes de NumChar ou do descritor da tabela B
                if (NumChar==0) then    
               
                  NBITS=TABB(DX(I)%F,DX(I)%X,DX(I)%Y)%NBITS
                  NBYTES=NBITS/8
                  IF (MOD(NBITS,8)>0) THEN
                    print *,"Error in BUFRTABLE B.   Bits of CHARACTER descriptor = ",nbits 
                    ERR=2
                    RETURN
                  END IF
                else
                 NBYTES=NumChar
                end if
              !}
              !{ Adiciona os subdescritores
                do i2=1,nbytes
                 c=c+1
                 dc(c)=dx(i)
                 dc(c)%i=i2
               END do
              !}
            !}
          ELSE
            !{ Copia os demais descritores  
              c=c+1
              dc(c)=dx(i)
            !}
            END IF
      ELSE
        !{ Copia os demais descritores
         c=c+1
         dc(c)=dx(i)
        !}
      END IF
    END do
    
    

 !{ Caso tenha parado em um replicador atrasado, continua a copia dos descritores 
 !  caso contrario ifinal=ndesc=c 
	if (ifinal<ndesc) then
	    ii=ifinal+1
		ifinal=c
		do i=ii,ndesc
			c=c+1
			dc(c)=dx(i)
		end do
	else
		ifinal=c
	end if
 !}
	dx=dc
	ndesc=c
END SUBROUTINE expandsubdesc

!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.ADD_ASSOCIATED_DESCRIPTOR                 | SHSF!
! ----------------------------------------------------------------------------!
!  Esta rotina aplica a funcao da tabelas C [2-04-yyy] que adiciona campos 
!  associados, (tal como controle de qualidade) a cada elemento subsequente. 
!
!  Para introduzir esses campos, esta subrotina adiona os descritores associados 
!  para que representem esses campos no processo de leitura/gracacao, mudando 
!  assim a sequencia de descritores aplicado na secao 4
! 
!  2 04 YYY Add associated field 
!        Precede each data element with Y bits of information. 
!        This operation associates a data field (e.g. quality control information)
!        of Y bits with each data element.
!-----------------------------------------------------------------------------!
! Dependendias:  Nao Ha
! Chamada por: expanddesc3
! ----------------------------------------------------------------------------!

SUBROUTINE add_associated_descriptor(dx,ndesc,ifinal,ndxmax,err)
!{Variaveis de interface
	TYPE(descbufr),pointer,DIMENSION(:)::dx     ! Descritores para a secao 4
	INTEGER,              intent(inout)::ndesc  ! Numero de descritores em dx
	integer,              intent(inout)::ifinal ! Posicao do replicador atrasado
	INTEGER,              intent(in)   ::ndxmax ! Numero maximo estimado de descritores expandidos
	INTEGER,              intent(out)  ::err    ! Codigo de err - se zero nao hove erro
!}
!{ Variaveis locais
	INTEGER                            ::i,c,i1,i2 ! Variaveis auxiliares 
	INTEGER                            ::is_adesc ! bandeira para descritore 2-04-y
	INTEGER                            ::nbits   ! Numero de bits
	TYPE(descbufr),DIMENSION(ndxmax)   ::dc
	integer                            ::ii
        integer                            ::kk      ! Numero de primeiros filhos 
!}
!1 - Inicializando variaveis
!{
	err=0
        is_adesc=0
 
!}

!-----------------------------------------------
!1- Verifica se existe algum descritor 2-04-xxx
!-----------------------------------------------
!{
    i=1
    do while (i<ifinal) 
      if ((dx(i)%f==2).and.(dx(i)%x==4))  then 
        ii=i
        is_adesc=1
        i=ifinal
      end if 
    i=i+1
    end do

   IF (is_adesc==0) return 
 
   nbits=0
!}
	
 !}
 !-------------------------------------------------------------------
 !2- Aplica o descritor 2-04 
 !   Aplica o valor a todos os descritores da tabela B ate encontrar 
 !   o cancelameto 2-04-000 ou ate encontra o proximo primeiro filho
 !   (Caso uso de subniveis com descritore da tabela D ou replicador)
 !-------------------------------------------------------------------
 !{
    i=1
    c=0
    kk=0
    do while (i<ifinal)
      if ((dx(i)%f==2).and.(dx(i)%x==4))  then
        ! O elemento presente e o 2-04-yyy
        !{ Obtem o numero de bits  
          nbits=dx(i)%y
          if (nbits>0) then
              
              ! Salta para o proximo elemento
              !{
                c=c+1
                dc(c)=dx(i)
                i=i+1
                kk=0
              !}
              ! O elemento presente deve ser o controle de qualidade
              ! Copia esse elemento e avanca para o proximo dx(i)
              !{  
                 c=c+1
                 dc(c)=dx(i)
                 !print *,"(1)i,c,dx,dc",i,c,dx(i)%f,dx(i)%x,dx(i)%y
                 i=i+1
               !}
            else
              ! Caso seja 2-04-000 salta para o priximo elemento. 
              ! a operacao ja esta cancelada com nbits=0 
                c=c+1
                dc(c)=dx(i)
                i=i+1
                
            end if
         end if
  
        !}
       
         ! if (dx(i)%k) kk=kk+1
         ! if (kk>1) nbits=0 
          
          if (nbits>0) then 
           if (.not.dx(i)%af) then  
           if ((dx(i)%f==0).and.(dx(i)%x/=31)) then  
               !Adiciona o campo associado aos elementos subsequentes (descritor 0-xx-yyy).
               !O Elemento presente deve ser o elemento com campo que precisa ser associado
               !Este elemento tem que ser do tipo 0-xx-yyy com xx/= 31 
               !O campo associado e colocado na frente 
               !Nesse atribui o mesmo dx(i) presente,
               !porem com atributo de numero de bits do campo associado
               !{
                  c=c+1
                  !dc(c)=dx(i)
                   dc(c)%f=9
                   dc(c)%x=99
                   dc(c)%y=999
                   dc(c)%a=nbits
                   dc(c)%i=0    !Nao pode ser uma variavel caracter
                   dx(i)%af=.true.
                 ! print *,"(3)i,c,dc",i,c,dc(c)%f,dc(c)%x,dc(c)%y,dc(c)%a
                 ! O Elemento presente continua sendo o elemento ao qual acrescentamos o campo a
                 ! associado. O proximo passo é apenas copiar o elemento 
              !}
            end if
           end if
         end if
          
         !{ Copia e depois e avanca para o proximo elemento 
            c=c+1
            dc(c)=dx(i)
            i=i+1 
          !}
       END do
    i=i-1
    c=c-1
  ! print *,ifinal,c,">>>",dx(c)%f,dx(c)%x,dx(c)%y

!{ Caso tenha parado em um replicador atrasado, continua a copia dos descritores 
 !  caso contrario ifinal=ndesc=c 
 ! print *,"ifinal,ndesc,c,ndxmax=",ifinal,ndesc,c,ndxmax
	if (ifinal<ndesc) then
            !print *,"> ",C,DC(C)%F,DC(C)%X,DC(C)%Y
            ii=ifinal-1
	    ifinal=c+1+1
            
		do i=ii,ndesc
			c=c+1
			dc(c)=dx(i)
		end do
                dx=dc
                ndesc=c
	else    
                dx=dc
           	ifinal=c+1
                ndesc=c+1
	end if
 !}
	
 

END SUBROUTINE add_associated_descriptor     

!-----------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.EXPANDESCD                  | SHSF!
! ----------------------------------------------------------------------------!
!  Esta rotina expande descritores da tabela D 
!
!   A expansao consiste em substituir um descritor da tabela D
!   pelo conjunto de descritores que o representa. Para isto 
!   deve ser fornecido um vetor de descritores (Desc) e o índice (idesc)
!   do descritor que se deseja expandir. 
!   
!   Esta subrotina retornará, a partir da mesma posicao idesc os
!   descritores expandidos e em ndesc o novo número de descritores
!   contidos em Desc 
!  
!  Erros
!       
!-----------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha                                                   !
! Chamadas Internas:Nao Ha                                                    !
! ----------------------------------------------------------------------------!
! HISTORICO:                                                                  !
!  Versao Original: Sergio H. S. Ferreira                                     !
!_____________________________________________________________________________!

SUBROUTINE expanddescD(desc,idesc,ndesc,xdelayed,err )

!{ Variaveis da Interface

TYPE(descbufr),DIMENSION(:),intent(inout)::desc      ! Matriz de Descritores compactos para a secao 3
INTEGER,intent(inout)                    ::idesc     ! Indice do descritor que deve ser expandido 
INTEGER,intent(inout)                    ::ndesc     ! Numero de descritores
INTEGER,intent(out)                      ::xdelayed  ! Se > 0 Indica que existe um replicador atrasado
                                                     ! aplicado a descritores nao expandidos no presente ciclo.
                                                     ! (necesarios aplicar a replicacao antes de proximo ciclo de expansao)
INTEGER,intent(inout)                    ::err       ! Se 0 Indica que nao houve erro na expancao dos descritores

!}

!{ variaveis locais
INTEGER                        ::i             ! Indice para descritores nao expandido
INTEGER                        ::k             ! Indice para descritores expandidos
INTEGER                        ::j,jj
TYPE(descbufr),DIMENSION(ndesc)::dc ! Variavel auxiliar 
!CHARACTER(len=6)::aux
!}


!{ Inicializa variaveis
i=0;k=0;j=0;err=0
xdelayed=0
!}

!-------------------------------------------------------
! So processa a expansao se o descritor for da tabela D
! e se este descritore estiver cadastrado 
!--------------------------------------------------------
   IF((desc(idesc)%f==3)) THEN    

      JJ=NDTABD(desc(idesc)%f,desc(idesc)%x,desc(idesc)%y) 

      IF (JJ>0) THEN !{ Se o Descritor for encontrado  
          
	  !{ 1 - Copia todos os descritores acima do descritor que sera expandido 

          do i=idesc,ndesc
             dc(i)=desc(i)
          END do
          
	  
          !{ 2 - Substitui o descritor da Tabela D (expansao)
          k=idesc
          do j=1,jj 
             desc(k)%f=tabd(3,dc(idesc)%x,dc(idesc)%y,j)%f
             desc(k)%x=tabd(3,dc(idesc)%x,dc(idesc)%y,j)%x
             desc(k)%y=tabd(3,dc(idesc)%x,dc(idesc)%y,j)%y
             desc(k)%i=0
             desc(k)%n=.false.
             desc(k)%a=0
             desc(k)%af=.false.
             if (j==1) then 
                desc(k)%k=.true.
             else 
                desc(k)%k=.false.
             end if
          
             if ((desc(k)%f==1).and.(desc(k)%y==0)) then 
                 xdelayed=k+1+desc(k)%x !Checks for a delayed replicator
             end if
             k=k+1
          END do !j
       !}

       !Caso haja um replicador atrasado, verifica se esse é  aplicado apenas aos descritores da presente expansao 
       !Se sim xdelayed=0, Se xdelayed > 0 indica que replica a posicao do descritore ainda nao expandido 
       !{
         if (xdelayed<k) then   
             xdelayed=0
          ! else                | Apenas para teste 
          !     print *,k,xdelayed     
          !     write( *,'(i1,i2.2,i3.3,"-",i4)')desc(xdelayed-2)%f,desc(xdelayed-2)%x,desc(xdelayed-2)%y,xdelayed-2
          !     write( *,'(i1,i2.2,i3.3,"-",i4)')desc(xdelayed-1)%f,desc(xdelayed-1)%x,desc(xdelayed-1)%y,xdelayed-1   
          !     write( *,'(i1,i2.2,i3.3,"-",i4)')dc(k)%f,dc(k)%x,dc(k)%y,k
         end if
       !}

         !Copia os demais descritores (apos idesc) ao final dos descritor
         !substituidos (expandidos)  
         !{
        do i=idesc+1,ndesc
          desc(k)=dc(i)
          k=k+1
        END do
        !}
        !{ Novo numero de descritores apos expansao
          ndesc=k-1
          idesc=idesc+jj-1
	 !}
	 
      
      ELSE !{ Se o Descritor NAO for encontrado  
      
          Print *,"Error! Descriptor =",desc(idesc),"not found"
          print *,"Table: ",basetabname(Decl_tab),"-->",basetabname(cur_tab)
          err=1 ! Descritor da tabela D desconhecido 
          return
      
     END IF 
     
END IF
	
END SUBROUTINE expanddescD

!------------------------------------------------------------------------------!
! SUB-ROTINA PRIVATIVA: MBUFR.REPLICDESC                                 | SHSF!
! -----------------------------------------------------------------------------!
! Este subrotina localiza descritores replicadores e aplica a 
! operacao de replicacao,aos descritores indicados 
!
! Quando o replicador e do tipo atrasado "delayed" entao nao e possivel de proceder 
! a replicacao, pois o numero de vezes Y que os descritores serao 
! replicados e informado dentro da secao 4.
!
!  Neste caso este subrotina retorna em idelayed o indice do  
!  o utimo descritor vinculado ao replicador em delayed
!
!  Entrada : Desc() vetor com descritores
!            idesc - Indice que indica a posicao do descritor
!                    replicador
!            ndesc - Numero total de descritores
!
!  SAIDA   ! Retorna como saida o Desc() e ndesc atualizados
!          ! e err (Codigo de erro)
!           err =0 Indica que nao houve erro 
!
!          ATENCAO: Esta rotina nao cosidera a existencia de subdescritores
!
!   No caso especial quando o  sistema esta rodando em modo de  auto geracao (autogen_mode)
!   o objetivo passa a ser  gerar um BUFR de exemplo, com falores nulos ,
!   Neste caso, esta rotina, ao incontrar um replicador atrasado, considera o fator
!   de replicacao = 1 e processa a replicacao
!    
!
!-----------------------------------------------------------------------------!
! Dependencias: Nao Há
! Chamada por: expanddesc3 
! ----------------------------------------------------------------------------!

  SUBROUTINE replicdesc(desc,idesc,ndesc,ndescmax,idelayed,err)

!{ Variaveis de interface	
  TYPE(descbufr),DIMENSION(:),intent(inout)::desc ! Matriz de Descritores compactos para a secao 3
  INTEGER,intent(inout)::idesc 	                  ! Indice do descritor que deve ser expandido
  INTEGER,intent(inout)::ndesc
  INTEGER,intent(in) ::ndescmax
  INTEGER,intent(out) :: idelayed	              ! Se > 0 Indica o uultimo descritor vinculado a um replicador delayed
  INTEGER,intent(inout)::err                      ! Se 0 Indica que nao houve erro na expancao dos descritores
!}
!{ variaveis locais
 !INTEGER::ndxmax    ! Numero maximo estimado de descritores expandidos
 INTEGER ::i             ! Indice para descritores nao expandido
 INTEGER ::k             ! Indice para descritores expandidos
 INTEGER ::j
 INTEGER ::ix,iy,jx,jy,ydelayed
 INTEGER ::nrepd        ! Number of descriptors to be replicated
 TYPE(descbufr),DIMENSION(ndescmax)::dc
 type(descbufr):: auxdesc
  
!}
!{ Inicializa variaveis
i=0;k=0;j=0;err=0; idelayed=0
!}

  IF ((desc(idesc)%f==1).and.(desc(idesc)%x/=0)) THEN
      !---------------------------------------------------
      !Aqui sao processados os replicadores normais 
      !---------------------------------------------------
      ! Caso esteja em modo de autogeracao converte
      ! os replicadores atrasados em replicadores normais 
      ! com fator de replicacao = 1 
      !--------------------------------------------------
      !{ 
        if ((AutoGen_mode).and.(desc(idesc)%y==0)) then
          desc(idesc)%y=1
          IF (desc(idesc+1)%x/=31) then
            print *,"Erro "
            stop
          else 
            auxdesc=desc(idesc)
            desc(idesc)=desc(idesc+1)
            desc(idesc+1)=auxdesc
            idesc=idesc+1
          end if 
        end if
      !}
     
      !{ Copia os descritores desc para dc
        do i=idesc,ndesc
          dc(i)=desc(i)
        END do
      !}
     
      i=idesc
      k=idesc
      j=0
      
      IF(dc(i)%y>0) THEN  !{2 
       !{ Processa a replicacao dos descritores vinculados ao replicador.
       !  Neste precesso o descritor replicador e excluido de desc()
       !  O primeiro descritor a ser replicado é chamado descritor primeiro filho e o padramentro k=.true
       !  Os demais sao descritores irmãos (k=.false.)
        jy=dc(i)%y
        jx=dc(i)%x
	nrepd=(ndesc-idesc)
	if (jx>nrepd) then
		print *," :MBUFR-ADT: WARNING! ",ERROMESSAGE(57) ! Error in replication replication processing"
		jx=nrepd
	end if

        do iy=1,jy
          do ix=1,jx !jx descritores em dc serao replicados
            !IF (TABB(dc(I+ix)%f,dc(I+ix)%x,dc(I+ix)%y)%nbits==0) err=2
            !IF (dc(I+ix)%f==1) err=3  ! nao replicar um descritor replicador 
            desc(k)=dc(i+ix)
            if (ix==1) then
              desc(k)%k=.true.
             
            else
              desc(k)%k=.false.
            end if
            k=k+1
	    IF (k>=ndescmax) THEN 
              print *,"Error 54!",ERROMESSAGE(54)
              print *,"NDESCMAX=",k
              err=54 
              return
            END IF
          END do !ix

        END do !iy  
        
       !{ Coopia de volta os descritores nao vinculados ao replicador
         
          do i=idesc+jx+1,ndesc
            desc(k)=dc(i)
            k=k+1
	    IF (k>=ndescmax) THEN 
              print *,"Error 54!",ERROMESSAGE(54)
              print *,"NDESCMAX=",k
              err=54 
              return
            END IF
          END do
          ndesc = k-1
          idesc=(jy*jx)-1
        !}

      ELSE
        !-----------------------------------------------------------------------
        ! AQUI PROCESSA-SE OS REPLICADORES ATRASADOS (delayed)
        !----------------------------------------------------------------------
        ! Neste caso, apenas verifica o código 0-31-y e os demais descritore, 
        ! vinculados a este replicador. A replicacao fica postergada. 
        ! As rotinas de leitura devem obter o fator de replicacao, e converter
        ! o replicador atrasado em um replicador normal para depois resumeter 
        ! para expansao 
        !-----------------------------------------------------------------------
        !{
        IF ((desc(idesc+1)%f/=0).and.(desc(idesc+1)%x/=31)) THEN 
          err=55  ! Erro na utilização de replicador delayed
          print *," :MBUFR-ADT: ERROR 56 !",trim(ERROMESSAGE(56))
          do i=idesc,Idesc+1
           write(*,'(2X,i5," -> ",i1,"-",i2.2,"-",i3.3)')i,desc(i)%f,desc(i)%x,desc(i)%y
          end do
         
        ELSE 
          ydelayed=dc(idesc+1)%y
          idelayed=idesc+1!+dc(idesc)%x 
        END IF
      END IF  !}
  END IF 
  !} Fim da replicacao 

 END SUBROUTINE replicdesc


!------------------------------------------------------------------------------!
! SUB-ROTINA PUBLICA: MBUFR.printchar_mbufr                             | SHSF |
! -----------------------------------------------------------------------------!
! printchar_mbufr - Rotina gravar uma linha texto entre 
! mensagens BUFR  
!   Obs.: Esta rotina eventualmente ser útil, quando se deseja acrescenter 
!   informações adicionais ao arquivo, tais como, cabeçalho de 
!  telecomunicações.    
!-----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:Nao Ha                                                    !
! ----------------------------------------------------------------------------!
   
! SUBROUTINE  printchar_mbufr(un,line)
 
! INTEGER,intent(in)::un
! CHARACTER(len=*),intent(in)::line
! 
! INTEGER ::l
!
 ! l=len_trim(line)
 !
 ! do i=1,l
 !   
 !
 ! END do
 
 ! END SUBROUTINE



!-------------------------------------------------------------------------------!
! SUB-ROTINA PUBLICA: MBUFR.tabc_setparm                                 | SHSF |
! ------------------------------------------------------------------------------!
!  
!    Esta subrotina configura parametros de
!    funcionamento do MBUFR, conforme 
!    descritores  tabela Bufr C de cada mensagem
!
!    As informacoes configuradas aqui são utilizadas pela funcao bits_tabb2 
!    que fornece o numero de bits de cada elemento da tabelas B, modificado
!    pela tabela C
!
!    Entrada: Descritores 
!    Saida : Parametros de configuracao
!
!    Descritores da Tabela C que sao processados
!
!    2-01-Y - "Adiciona Y-128 Bits ao comprimento de 
!              cada elemento da tabela B" (desde que seja numerico) 
!              (Parametro tabc%dbits)
!    2-02-y - "Adiciona Y-128 Bits ao fator de escala
!              de cada elemento da tabela B,exceto os
!              que naao saao codigos ou flag tables
!              (Paraametro tabc%dscale)
!
!    2-03-y   "Descritores de elementos subseqüentes definem novos 
!              valores de referência para entradas correspondentes na Tabela B.
!              Cada novo valor de referência é representado por bits YYY na seção Dados.
!              A definição de novos valores de referência é concluída codificando 
!              este operador com YYY = 255.
!              Valores de referência negativos devem ser representados por um inteiro positivo 
!              com o bit mais à esquerda (bit 1) definido como 1.
!    2-04-Y    Adicionar campo associado - preceder cada elemento de dados 
!              com Y bits de informação. Esta operação associa um campo de
!              dados (por exemplo, a informação de controle de qualidade) 
!              de Y bits com cada elemento de dados.
!    2-05-y  : Caracter significativo
!              "Y caracteres (CCITT Alfabeto internacional N.5) 
!              estao inseridos como campos de dados ocupando Y x 8 bits"
!          
!    2-06-Y   " Os proximos Y bits serao descritos pelo proximo
!             ! descritor, que e um descritor local. 
!
!    2-07-Y   Increase scale,reference value and data width   (Operational)
!             For Table B elements, which are not CCITT IA5 (character data),
!             code tables, or flag tables:
!             1. Add Y to the existing scale factor
!             2. Multiply the existing reference value by 10^Y
!             3. Calculate ((10 x Y) + 2) ÷ 3, disregard any fractional remainder 
!                and add the result to the existing bit width.
!    2-08-Y   Change width of CCITT IA5 field...................................Status: Operational
!             Y characters from CCITT International Alphabet#5
!              (representing Y x 8 bits in length) replace the specified data 
!             width given for each CCITT IA5 element in Table B.
!
!    2-09-Y   IEEE floating point representation ...............................Status: VALIDATION
!             “For elements in Table B other than CCITT IA5, Code Tables, 
!             Flag Tables and delayed descriptor  replication factors, this operator
!             shall indicate that values are represented in YYY bits IEEE  floating point
!             where YYY can be set to 032 (single precision) or 064 (double precision).
!             This operator shall override the scaling, reference value and data width 
!             from Table B.  An operand of YYY=000 shall reinstate the Table B scaling, 
!             reference value and data width. (see note 21)
!-----------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                    !
! Chamadas Internas:Nao Ha                                                    !
! ----------------------------------------------------------------------------!


 
 SUBROUTINE tabc_setparm(desc,err)
    TYPE(descbufr),optional,intent(in) ::desc     ! Descritores
    INTEGER,                intent(out)::err      ! Codigo de erro (se err=0 nenhum erro ocorreu
    INTEGER                            ::flag_err ! Bandeira de erro

    err=0 ! Descritor da tabela C nao processado ou encontrado

!{ Se fornecido o descritor,entao configura parametros, caso contrario

IF (present(desc))  THEN
 
	!{ Caso descritor da tabela C
    IF (desc%f==2) THEN 

		flag_err=1
	
		!{**** 2-01-yyy ***  
		
		IF (desc%x==1) THEN 
			IF (desc%y>0) THEN 
				tabc%dbits=tabc%dbits+desc%y-128
			ELSE
				tabc%dbits=0
			END IF
			flag_err=0
		END IF
		!}

		!{**** 2-02-yyy ***  
		IF (desc%x==2) THEN 
			IF (desc%y>0) THEN 
				tabc%dscale=tabc%dscale+desc%y-128		   
			ELSE
				tabc%dscale=0
			END IF
			flag_err=0
		END IF
		!}

		!{**** 2-03-yyy ***  
		!Subsequent element descriptors define new reference values for corresponding Table B entries.
		!Each new reference value is represented by YYY bits in the Data section.
		!Definition of new reference values is concluded by coding this operator with YYY = 255.
		!Negative reference values shall be represented by a positive integer with the left-most bit (bit 1) set to 1.
		! (Um problema aqui)
		IF (desc%x==3) THEN 
			!tabc%vref=desc%y
			tabc%vrefbits=desc%y
			flag_err=0
		END IF
             
	     !-----------------------------------------------------------  
             !  Descritor 2-04-yyy, que adicionam os campos de um descritor
	     ! sao tratados na subrotina "associated_field" Aqui apenas identifica
             ! que existe algum descritor com campo associado presente 
	     !-----------------------------------------------------------
             !{ 
               if (desc%x==4) then 
               any_associated_field=.true.
               flag_err=0 ! 2-04-yyy
               end if
              !}
                


		!{**** 2-05-yyy ***  !Resolver isto em expandsubdesc
		IF (desc%x==5) THEN 
			tabc%ccitt5=desc%y
			flag_err=0
		END IF

		!{**** 2-06-yyy ***  - 	  		  
		IF (desc%x==6) THEN
			tabc%nlocalbits=desc%y
			flag_err=0
			tabc%dbits=0
		ENDIF
              !}
              !{ *** 2-07-yyy
               if (desc%x==7) then
                tabc%dscale=desc%y      ! 1. Adiciona  Y ao fator de escala existente r
                tabc%multref=10**desc%y ! 2. Multiplica o valor de referencia existente por 10^y
                tabc%dbits=int (((10 * desc%Y) + 2) / 3)
                flag_err=0  
               end if
               !}
	       
              !{ Descritores que modificam tabela C sao tratados em expandsubdesc
               if (desc%x==8) flag_err=0 ! 2-08-yyy
              !}

		!{Ignora descritores acima de 2-10-yyy 
		IF (desc%x>10) flag_err=0
		!}

		!{ Processamento dos erros 	  
		IF(flag_err>0) THEN 
			print *,"Error 51 ",ERROMESSAGE(51)
			print *,"Descriptor:",desc%f,desc%x,desc%y
                        print *,"CENTER=",cur_sec1%center," BUFR TYPE",cur_sec1%BType," BUFR SUBTYPE=",cur_sec1%Bsubtype
			err=51
			return
		else
			err=0
		END IF
		!}
	END IF	   
    !}
	
   !{Caso o descritor nao seja fornecido, Aplica configuracoes iniciais 
   ELSE 
    tabc%dbits=0
    tabc%dscale=0
    !tabc%vref=255
    tabc%vrefbits=255
    tabc%nlocalbits=0
    tabc%assocbits=0
    desc_associated%x=0
    any_associated_field=.false.
    err=0.
    tabc%ccitt5=0
    tabc%multref=1
   
   END IF
 

 END SUBROUTINE tabc_setparm


!------------------------------------------------------------------------------!
! FUNCAO INTEIRA : bits_tabb2                                            | SHSF!
! -----------------------------------------------------------------------------!
!
!  Retorna numero de bits de um descritor da tabela B
!
!  A tabela B contem o numero de bits de cada um dos descritores.
!  Contudo este numero pode ser modIFicado em 2 situacoes
!
!   a) Quando um descritor da tabela C altera descritores da tabela B
!
!   b) Quando utilizado sub-descritore da tabela B
!
!   Nota : Os sub-descritores nao faze parte do BUFR padrao.
!   Este foi um artificio criado devido 
!   imposibilida trabalhar com "variaveis texto" de uma unica vez. 
! 
!   Cada subdescritor identifica um caracter de um texto e tera sempre 8 bits
!
!   Valores superios a 32 tambem nao e permitodo na subrotina get_octets,
!  exceto quando procedente do  descritor 2-05-yyy
!-----------------------------------------------------------------------------!
! Rotinas chamadas: Nao Ha                                                     
! Chamada por:                                                             
! ----------------------------------------------------------------------------!
! HISTORICO:                                                                  
!       Versao Original: Sergio H. S. Ferreira                                
!
! SHSF 20110125 - Acrescentado funcioanlidade para 2-25-255


 function bits_tabb2(d);INTEGER :: bits_tabb2
 
!{ Variaveis de interface	
   TYPE(descbufr),intent(in)::d
!}
   INTEGER::bits
   INTEGER::err
!{  Valor inicial 	
   bits=0
!}
!{ Caso nao seja um descritor da tabela B e nem o descritor
!  2-05-y, retorna valor zero
    IF ((d%f/=0).and.(d%f/=9)) THEN
      call tabc_setparm(d,err)
      bits_tabb2=tabc%ccitt5*8  !< Numero de caracteres inseridos por 2-05-y
      TABC%CCITT5=0    
      return
    END IF 
!}

!{ Se for um campo associado por 2-04-yyy usa o numero de bits associados 
    if (d%a>0) then
      bits_tabb2=d%a
      return
    end if   

!}
! If 2-03-yyy >0 or not missing, i.e. 
! The reading of the new reference value has been ativated,
! use the new defined number of bits
!{

    if ((tabc%vrefbits>0).and.(tabc%vrefbits<255)) then
      bits_tabb2=tabc%vrefbits
      return
    end if   

!}


!{ Obtem numero de bits do descritor da tabela B, levando em 
!  consideracao as alteracoes de numero de bits causados por
!  descritores da tabela C
    IF (d%i==0) THEN  
      bits=tabb(d%f,d%x,d%y)%nbits+tabc%dbits
      IF (bits==0) bits=tabc%nlocalbits
      tabc%nlocalbits=0
      if (d%n) bits=bits+1   ! Se modificado por 2-25-255 faz bits=bits+1
    ELSE
      bits=8
    END IF
 !}

  if ((bits>rdigits) .and.(verbose>1))then 
    write(*,33)d%f,d%x,d%y,bits,rdigits,tabc%dbits
 33 format("  :MBUFR-ADT: Warning! Descriptor ",i1.1,i2.2,i3.3," has ",i5," bits (>",i5," bits) Table C bits=",i5) 
  end if

  if (bits<0) then 
    print *,"tabbits fatal error !",tabc%dbits
    stop
  end if 
   bits_tabb2=bits
 END function
 
 !------------------------





!-------------------------------------------------------------------------------!
! FUNCAO PRIVATIVA REAL : MBUFR.CVAL                                      | SHSF!
! ------------------------------------------------------------------------------!
! CVAL(A,D) : Retorna um valor REAL de um inteiro "A" (secao 4 do BUFR)         !
!             atraves da  aplicacao do  fator de escala e referencia determinado!
!             pelo  descritor "D"                                               !
!                                                                               !
!     Obs.:                                                                     !
!         1-Esta rotina inclui as modificacoes da tabela B previamente          !
!           configuradas por descritores da tabela C	                          !
!                                                                               !
!          2-Funcao inversa a CINT (vide CINT)                                  !
!-------------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha                                                     !
! Chamadas Internas:Nao Ha                                                      !
!-------------------------------------------------------------------------------!
! HISTORICO:                                                                    !
!   Versao Original: Sergio H. S. Ferreira                                      !
!_______________________________________________________________________________!

 
FUNCTION cval(a_in,d);REAL(kind=realk):: cval
   !{ Variaveis de entrada
    INTEGER(kind=intk),intent(in) :: a_in  ! Valor inteiro (BUFR secao 4)
    TYPE(descbufr),    intent(in) ::d      ! Descritor (tabela B) relativo ao valor a
   !}
   !{ Variaveis locais
      INTEGER(kind=intk)  :: scale   ! Fators de escala
      INTEGER(kind=intk)  :: refv   ! Valor de referencia
      INTEGER             :: err    ! Codigo de erro
      integer(kind=intk)  :: a
      integer(kind=intk2) :: iaux
      integer             :: rexp  ! expoente de um num. ponto flutuante
      real(kind=realk),parameter::dez=10.0  
      integer ::i
   !}

     a=a_in
     cval=0.0
    !{ Caso descritor da tabela C
    IF (d%f==2) THEN
      call tabc_setparm(d,err)
      return
    END IF  
    !}
    !{ Caso campo associado (descritor 9-99-999) (Nao conversao de escala  e referencia)
    IF (d%f==9) THEN
      cval=real(a,kind=realk)
      return
    END IF  
  
  !{ If actvated 2-03-yyy than replace the reference value in table B and return

   if ((tabc%vrefbits>0).and.(tabc%vrefbits<255)) then
   
    cval=0.0
    do i = 0, (tabc%vrefbits-2)
     cval=cval+ibits(a,i,1)*2**i
    end do
    cval=-ibits(a,tabc%vrefbits-1,1)*cval
    tabb(d%f,d%x,d%y)%refv=cval
     return 
   end if  
  !}

    if (d%a>0) then 
       scale=0
       refv=0
       goto 767
    end if
    
     !{ Em principio esta linha nao seria necessarias, pois 
    ! o problema do descritor atrasado curto estaria resolvido antes.
    ! E apenas para garantir 
        IF ((D%X==31).and.(d%y==0).and.(a<0)) a=1
    !} 
    if (a<0) then  !  (a==null)  
      cval=null
      return
    end if
    !{ Obtem o fator de escala
       scale=tabb(d%f,d%x,d%y)%scale+tabc%dscale
    !}

    !{ Obtem o valor de referencia

    !IF (tabc%vref==255) THEN 
      refv=tabb(d%f,d%x,d%y)%refv
    !ELSE
    ! refv=tabc%vref
    !ENDIF

    !}
    !{ Multiplica fator de referencia existente por multref
          refv=refv*tabc%multref
         !}
     !{ Corrige valor de referencia indicado por 2-25-255
     if (d%n) refv=refv-2**(bits_tabb2(d)-1)
    !}

   
 767  continue
      !{ Decodifica o valor Inteiro para REAL 
       
       IAUX=(a+refv)
       if ((scale<=maxexp).and.(numbits_vint (iaux)<=rdigits)) then

         CVAL=REAL(IAUX,kind=realk)/real(dez**scale,kind=realk)

       else 
         
          print *,":MBUFR-ADT:Warning!  Error in CVAL during integer-real convertion"
           write (*,'("          Descriptor=",I1,"-",I2.2,"-",I3.3)')d%f,d%x,d%y
             print *,"          Integer value = ", IAUX
             print *,"          Atual Expoent = ",scale
             print *,"          Desc. Expoent = ",tabb(d%f,d%x,d%y)%scale
             CVAL=REAL(IAUX,kind=realk)/real(dez**scale,kind=realk)
             print *,"          Value = ",cval
      end if
      
   !{ CASO SEJA VALOR ASC NAO PERMITE QUE VALORES ASC<32 SEMA UTILIZADOS
     IF ((d%I>0).and.(CVAL<32)) CVAL=32
   !}
   !}

  END function cval



!{



!------------------------------------------------------------------------------!
! FUNCAO PRIVATIVA INTEIRA : MBUFR.CINT                                 | SHSF !
! -----------------------------------------------------------------------------!
! CINT(V,D) : Retorna um valor INTEIRO (secao 4 BUFR) de um REAL  "V"          !
!             atraves da  aplicacao do  fator de escala e referencia determinado!
!             pelo  descritor "D"                                              !
!                                                                              !
!     Obs.:                                                                    !
!         1-Esta rotina inclui as modificacoes da tabela B previamente         !
!           configuradas por descritores da tabela C                           !
!                                                                              !
!           2-Funcao inversa a CVAL (vide CVAL)                                !
!------------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                     !
! Chamadas Internas:Nao Ha                                                     !
!------------------------------------------------------------------------------!
! HISTORICO
! SHSF 20101015 - Eliminado chamada de tabc_setparm quando d%f/=0
!                  Nas rotinas de gravacao que utilizam CINT, geralmente 
!                  tabc_setparm ou bits_tabb2 sao chamados tambem. Com isto, 
!                  tabc_setparm e chamado mais de uma vez, podendo levar a 
!                  erro de codificacao, principalmente quanto o descritor 2-01-yyy
!                  esta sendo usado 
function cint(v,d);INTEGER(kind=intk)::cint  

!{ Variaveis da interface

   REAL(kind=realk),intent(in)::v   ! real value to be converted in BUFR
   TYPE(descbufr),intent(in)::d     ! associated descriptor 
!}

!{ Variaveis locais   
    real(kind=realk)::auxcint
    real(kind=realk)::scale
    real(kind=realk):: refv
    INTEGER :: err

!}

   cint=0
   ! Verify if is the associated field
   IF (d%a>0) then 
     cint=v
     return
   end if
   
   IF (d%f/=0) return 
  
   !Caso o valor passado seja "null", retorna null
	IF (v==null) then
		cint=null
		return
	end if
	
	!{ Obtem o fator de escala
	
 
	  scale=tabb(d%f,d%x,d%y)%scale+tabc%dscale
	
	!}
		
	!{
       !( This part was changed to be compatible with the changes in tablec_setparm)
	
	IF ((tabc%vrefbits==255).or.(tabc%vrefbits==0)) THEN 
		refv=tabb(d%f,d%x,d%y)%refv
	ELSE
	
	 ! If 2-03-yyy is activated than 
	 ! a) change the reference value in tableB 
	 ! b) return the reference value - If negative the bit 1 is set to 1
	 !{
     
      tabb(d%f,d%x,d%y)%refv=v
    
      cint=abs(v)
      if (v<0) then 
       cint=cint+2**(tabc%vrefbits-1)
      end if 
      
      return
    ENDIF
	 !}

	!{ Multiplica fator de referencia existente por multref
          refv=refv*tabc%multref
         !}
	!{ Corrige valor de referencia indicado por 2-25-255
        if (d%n) refv=refv-2**(bits_tabb2(d)-1)
       !}
	
	!{ Calcula o valor inteiro para "BUFRIZACAO"
	
	 auxcint=((v*10.0**scale)-refv)
	 if (auxcint>=0) then 
		cint=int(auxcint+0.5,kind=intk)
	 else
		cint=int(auxcint-0.5,kind=intk)
	 end if

	 !}
	
	!{ caso seja uma variavel ascII nao permite que codigos asc <32 
	  ! seja usados
	   IF ((d%I>0).and.(CINT<32)) CINT=32
     !}
	 	
	END function
	
	
	
	
!------------------------------------------------------------------------------!
! FUNCAO PRIVATIVA INTEIRA : MBUFR.NUMBITS_VINT                         | SHSF !
! -----------------------------------------------------------------------------!
! Esta funcao e utilizada na "bufrizacao" de dados compactados
!
!  Retorna o numero de bits necessarios para representar um valor inteiro positivo
!  Retorna zero se o valor for zero ou negativo !
!
!  Obs.: Inverso a vint_numbits (vide VINT_NUMBITS) 
!------------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                     !
! Chamadas Internas:Nao Ha                                                     !
!------------------------------------------------------------------------------!
! HISTORICO:                                                                   !
!       Versao Original: Sergio H. S. Ferreira                                 !
!______________________________________________________________________________!

function numbits_vint (v);INTEGER:: numbits_vint
	INTEGER(kind=intk2),intent(in)::v ! Um Numero Inteiro
	INTEGER::numbits,v2
	IF (v>0) THEN 
		numbits=1
		v2=v
		do while (v2>1)
			v2=v2/2
			numbits=numbits+1
		END do
	ELSE 
		numbits=0
	END IF
	numbits_vint=numbits
END  function numbits_vint
!} 


!------------------------------------------------------------------------------!
! FUNCAO PRIVATIVA INTEIRA : MBUFR.VINT_NUMBITS                          | SHSF!
! -----------------------------------------------------------------------------!
! Esta funcao e utilizada para determinar valores "missing" em BUFR 
!
!  Retorna o maximo valor inteiro positivo representado por um determinado
!  numero de bits
!  Se o numero de bits for igual ou menor que zero, retorna o valor zero 
!
!  Obs.: Inverso a vint_numbits (vide NUMBITS_VINT) 
!------------------------------------------------------------------------------!
! Chamadas Externas: Nao Ha                                                    !
! Chamadas Internas:Nao Ha                                                     !
!------------------------------------------------------------------------------!
! HISTORICO:                                                                   !
!	Versao Original: Sergio H. S. Ferreira                                 !
!______________________________________________________________________________!


! vint_numbits
!{
 function vmax_numbits(nbits);INTEGER(kind=intk2)::vmax_numbits
	  INTEGER,intent(in)::nbits
	  INTEGER(kind=intk2),parameter::d=2
	   vmax_numbits=(d**nbits)-1
          ! Subtrai um para transformar 1000000 -> 01111111
          ! Nota( nao considera o bit de indicacao de sinal)
  END function vmax_numbits
  
!------------------------------------------------------------------------------!
! vmax_real !Fornece o valor maximo de um real de um determinado tipo (realk)  !    
!------------------------------------------------------------------------------!          
         function vmax_real();real(kind=realk)::vmax_real
            real(kind=realk)::r
            real(kind=realk),parameter::d=10.0
            vmax_real=(1.0*d**(range(r)))
         end function
!------------------------------------------------------------------------------!
! vmax_int !Fornece o valor maximo de um inteiro de um determinado tipo (intk) !    
!------------------------------------------------------------------------------!          
         function vmax_int();integer(kind=intk)::vmax_int
            integer(kind=intk)::r
            integer::i
            i=digits(r)
            vmax_int=vmax_numbits(i)
         end function



!------------------------------------------------------------------------------!
! SUBROTINA PRIVATIVA  : REMOVE_DESC                                    | SHSF !
! -----------------------------------------------------------------------------!
! APAGA, DE DENTRO DO VETOR D, OS DESCRITORES dentro do interfalo p1 a p2
!------------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha                                                     !
! Chamadas Internas:Nao Ha                                                     !
!                                                                              !
!  Obs.: Utilizar apeanas pare remocao de descritores apos replicador nulo     !
!-------------------------------------------------------------------------------
! Dependencias: Nao Ha
! Chamada por: read_mbufr, readsec4rd,savesec4rd
!------------------------------------------------------------------------------
subroutine remove_desc(d,nd,p1,p2)
!{ Variaveis de interface
	type(descbufr),dimension(:),intent(inout)::d ! matriz de descritores
	integer,intent(inout)::nd  ! Numero de descritores em nd
	integer,intent(in)::p1	   ! Posicao inicial para remocao
	integer,intent(in)::p2	   ! Posicao final para remocao
!}
!{Variaveis Locais
	integer ::i,j
	type(descbufr),dimension(nd)::daux
!}
!{ 

!{ Assinala os descritores que serao removido com f=-1
	do i=p1,p2
		d(i)%f=-1
		! Este erro nao deve ocorre
		!if (d(i)%i>0) then 
		!	print *,"Erro in remove_desc",i,d(i)%i
		!end if
	end do
	
!}
!{ Copia os descritores para matriz auxiliar, sem os descritores
!  assinalados
	j=0
	do i=1,nd
		if (d(i)%f>=0) then 
			j=j+1
			daux(j)=d(i)
		end if
	end do
	nd=j

!{ Copia de volta os descritores para a matriz inicial 
	do i=1,nd
		d(i)=daux(i)
	end do
 end subroutine remove_desc



!*******************************************************************************
! sep_char  | Separa as palavras que compoe uma linha de texto          | SHSF |
!*******************************************************************************
!                                                                              |
!  Separa as pavras de uma linha de texto em uma matriz de palavras,           |
!                                                                              |
!*******************************************************************************
    
 subroutine sep_char(string,words,nwords)

 !{ Variaveis da Interface
	 character (len=*), intent (in) :: string !..................Texto  de entrada
	 character (len=*), dimension(:), intent (out)::words !..... Palavras separadas do texto
	 integer , intent (out) :: nwords !..........................Numero de palavras
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F 
	 character(len=1) ::D
	 character(len=256) :: S
 !}
	 F=0
	 S=""
	 words(:)=""
	 maxl=size(words,1)	  
	 if (maxl<=2) goto 100  
	 l=len_trim(string)
	 if (l==0) goto 100
	 

	   
	 do i=1,l
	 
	    D = string(i:i)
            
		If (ichar(D)<33) Then
           
		   If (Len(trim(S)) > 0) Then
            
			  F = F + 1 
		      words(F) = S
		      S = ""
            
			End If
            
		 Else
                
		    S = TRIM(S) // D
		
         End If
	    
		 if (F==maxl+1) exit 
      
	  end do !i
      	
	  If (S /= "")  F = F + 1; words(F) = S; S = ""
 
 100  nwords=F
 	 
	End Subroutine

!******************************************************************************
! readconf  | Ler configuracoes                                        | SHSF |
!******************************************************************************
!                                                                             |
!  Ler arquivo de configuracoes mbufr_conf.txt                                |
!                                                                             |
!******************************************************************************


   subroutine readconf(UN,path2tables)
    !{ interface
	INTEGER,INTENT(IN)::UN
    character(len=*)::path2tables
    !}
	 integer::i,l,UNI
	character(len=255)::linha
	UNI=UN
    if (len_trim(path2tables)==0) then 
	call getenv("MBUFR_TABLES",local_tables)
    else 
      local_tables=path2tables
    end if

	!{ Acrescenta barra no final do diretorio local_tables, caso seja necessario
	! Nesse processo veirifica se o diretorio contem barras do windows ou barra do linux 
	
	i=len_trim(local_tables)
	if ((local_tables(i:i)/=char(92)).and.(local_tables(i:i)/="/")) then 
		if (index(local_tables,char(92))>0) then 
			local_tables=trim(local_tables)//char(92)
		else
			local_tables=trim(local_tables)//"/"
		end if
	end if

	open (UNI,file=trim(local_tables)//"mbufr_initconf.txt",status="unknown")

333	read(UNI,'(a)',end=444)linha
		l=index(linha,"#")
		if (l>1) linha=linha(1:l)
		if (l==1) linha=""
		l=len_trim(linha)
		if (l>0) then 
		read(linha,*)Cur_tab%NumMasterTab,Cur_tab%centre,Cur_tab%VerMasterTab,Cur_tab%VerLocTab,BUFR_Edition
		end if
		goto 333
		
444 continue
        CLOSE(UNI)
	Init_tab=Cur_tab

	end subroutine

!*******************************************************************************
! sec4gen  | Gera uma secao 4 com dados nulos                           | SHSF |
!*******************************************************************************
! Genarate a section 4 with missing data                                       |
!                                                                              |
!                                                                              |
!******************************************************************************	
subroutine sec4gen(sec4,desc_sec4,ndesc_sec4) 
!{Variaveis da interface
   TYPE(sec4TYPE),intent(inout)::sec4
   type(descbufr),pointer,dimension(:)::desc_sec4
   integer,intent(in):: ndesc_sec4
!}
!{variaveis locais
   integer::Err,i
!}
  print *," :MBUFR-ADT: Automatic generation mode"
   sec4%nvars=ndesc_sec4
   allocate(sec4%r(sec4%nvars,1),STAT=ERR)

   do i=1,ndesc_sec4
	if (desc_sec4(i)%f==0) then 
		if (desc_sec4(i)%x==31) then
			sec4%r(i,1)=1
		else
			sec4%r(i,1)=0
		endif

	end if
   end do
   
end subroutine sec4gen


!******************************************************************************
! loadmessage_mbufr | load one bufr message in binary form.            | SHSF |
!******************************************************************************
!   Carrega uma mensagem bufr na forma binaria Decodifica apenas a secao 1     |                                    !                                                                              | 
!******************************************************************************  


	SUBROUTINE  READBIN_MBUFR(uni,bufrmessage, Bufr_ed,sec1,errsec,header)
 	
 !{ Declaracao das Variaveis da entrada 
	INTEGER, intent(in)::uni                            ! Unidade de leitura
	
 !}
 !{ Declaracao das variaveis de saida
    TYPE(sec1TYPE),intent(out)::sec1
    INTEGER,intent(out):: bufr_ed   ! Edicaao do formato BUFR lido
    INTEGER,intent(out):: errsec    ! Erro de leitura se 0 indica que nao houve erro de leitura
                                    ! se >=0 indica o numero da secao onde ocorreu o erro (exceto secao0)
    character(len=40),optional,intent(inout)::header !Telecommunications header (40 bytes) 
    TYPE(octTYPE),intent(out):: bufrmessage
    
 !{ Declaracao das variavaveis locais

	integer                        ::tam_sec1
  	integer                        ::nbytes
	CHARACTER(len=1)               ::oct
	CHARACTER(len=1),DIMENSION(4)  ::sec0 
	INTEGER(kind=intk),DIMENSION(2)::a
        INTEGER,DIMENSION(2)           ::b
	INTEGER                        ::bufrid,un
	CHARACTER(len=4)::BUFRW
	CHARACTER(len=4):: sec5id 
	INTEGER :: i, RGINI,RGSEC5,ERR
	INTEGER :: alerr
	
	
 
 !}
 !{ Este programa ler aquivos de ate 4.2 GBytes
  IF (CurrentRG>currentRGMax) then 
	
	print *,"Error 01:",ERROMESSAGE(01), CurrentRGMax,"Bytes"
	errsec=01
	bufrmessage%nocts=0
	return
  end if
	
 !}
 !}
 !{ Inicializando variaveis

	BUFRW="BUFR" 
	SUBNAME="LOADMESSAGE_MBUFR"
	un=UNI
	errsec=0
	IOERR(UN)=0
	
!}

!10  bufrid = 1
10   call headerid(un,cur_header)
   if (present(header)) then 
     header=cur_header
   end if 
!   if (present(header)) then 
!     call headerid(un,header)
!   end if
 !'{ Procura pelo Inicio da proxima secao 0 (palavra "BUFR")
 !'  Quando BUFRID chegar a 4 entao foram enontradas todas as letras de BUFR
 !' e   NBYTES=0
 !  do While (( bufrid <= 4).AND.(IOERR(UN)==0))

!	 IF (IOERR(UN)==0) THEN
!	   currentRG = currentRG + 1
!       read (un,rec= currentRG,iostat=IOERR(UN)) oct
!	   IF (IOERR(UN)/=0) then
!			RETURN
!	   end if
!	 ELSE
!	   NBYTES=0
!	   RETURN
!	 END IF
!     IF (BUFRW(BUFRID:BUFRID) == oct) THEN
!	     bufrid = bufrid + 1
!	 ELSE
!	    bufrid = 1
!     END IF
!   END do
!'} ----------------------------------------------------------------------------
IF (IOERR(UN)/=0) RETURN
!'{ Ler os demais 3 bytes da secao 0
  RGINI = currentRG - 3      !' Registro do Inicio da Mensagem

 DO i = 1, 4
  currentRG = currentRG + 1
  IF (IOERR(UN)==0) read (un,REC= currentRG) sec0(i)
  IF(IOERR(UN)/=0) RETURN
 END DO
 b(1) = 24 !'Tamanho da mensagem BUFR
 b(2) = 8  !'nuumero da Edicaao BUFR
 Call GET_OCTETS(sec0, 4, b, a, 2,0, ERR)
 NBYTES = a(1)
 bUFR_ED = a(2)
 BUFR_EDITION=BUFR_ED

!'}-----------------------------------------------------------------------------
!'{ VerIFica se a edicao  BUFR ee suportada por esta rotina

  IF ((bUFR_ED < 2).Or.( bUFR_ED > 4)) THEN
   print *, "****  Warning  ****"
   PRINT *,"This program was prepared to read BUFR edition 2, 3 and 4"
   print *,"Other edition cam be read in incorrectly way"
   PRINT *,""

  END IF
!'}-----------------------------------------------------------------------------

!'{ VerIFica O FINAL DA MENSAGEM. Se passar por este teste esta
!'  mesagem possui o tamanho correto.
!   Nota: Pode ocorrer erro caso o tamanho do arquivo for menor que a posicao do
!   7777
   RGSEC5 = RGINI + NBYTES - 4 !'Inicio da secao 5

   sec5id = ""
   DO currentRG = RGSEC5, RGSEC5 + 3
    IF (IOERR(UN)==0) read (un,rec=currentRG) oct
	IF (IOERR(UN)/=0) RETURN
    sec5id = TRIM(sec5id) // oct
   END do

   IF (sec5id/="7777") THEN
     print *, " ** Warning ** Corrupted message at position ",currentRG
	 currentRG=RGINI+4
	 goto 10
   END IF
!'}-------------------------------------------------------------
!'{ Processar a leitura da sesaao da mensagem a partir da secao 1

   !{ Leitura da secao 1
   currentRG = RGINI + 7
   Call readsec1(un,sec1,tam_sec1,err)
	IF (err/=0) THEN
	   errsec=err
	   print *,"Erro ",errsec,"! ",trim(ERROMESSAGE(err))
	END IF

   !}

!{ Processando a leitura do BUFR, desde o inicio ate o final
	allocate(bufrmessage%oct(NBYTES),stat=alerr)
	bufrmessage%nocts=nbytes
	currentRG=RGINI-1
	DO i = 1, nbytes
		currentRG = currentRG + 1
		IF (IOERR(UN)==0) read (un,REC= currentRG) bufrmessage%oct(i)
		IF(IOERR(UN)/=0) RETURN
	END DO

!}
END SUBROUTINE

!-------------------------------------------------------------------------------
!INIT_TABLIST |                                                     | SHSF |
!-------------------------------------------------------------------------------
! Esta subrotina faz a leitura de um catalogo de tabelas, que serao consultadas
! pelo modulo com o objetico de escolher a tabela adequada a decodificacao 
!-------------------------------------------------------------------------------
subroutine init_tablink(un)
!{ variaveis de interface
  integer,intent(in)::un
!}
!{variaveis locais
  character(len=10),dimension(300)::tab
  integer::nrows
  character(len=1024)::nmlfile
  integer::i,j,a,c,v,l,k
!}
    namelist /bufr_tablelinks/ nrows,tab
    nmlfile=trim(local_tables)//"tablelinks.txt"
    if (verbose>=2) print *," :MBUFR-ADT: Open tablelinks.txt"
    open(UN,file=nmlfile,status='old')
    read(UN,bufr_tablelinks)
    close(UN)
    ntabs=nrows
    j=0;k=1
    do i=1,nrows*2
      if(len_trim(tab(i))==10) then 
        read(tab(i),'(2i3,2i2)')a,c,v,l
      else
        print *," :MBUFR-ADT:Error reading tablelinks.txt"
        a=0;c=0;c=0;l=0
      end if
      j=j+1
      tablink(k,j)%nummastertab=a
      tablink(k,j)%centre=c       !......Codigo do Centro que gerou o BUFR
      tablink(k,j)%verloctab=l    !.................Versao da tabela local 
      tablink(k,j)%vermastertab=v  !...............Versao da tabela Mestre  
      if (j==2) then
        k=k+1
        j=0
      end if
     end do
   ! do i=1,ntabs
   !   print *,"MBUFR-ADT: Link: ",basetabname(tablink(i,1)),"-->",basetabname(tablink(i,2))
   ! end do
end subroutine
!-------------------------------------------------------------------------------
! IsEqual | Check if a == b  where a and b are  type(tabname)            | SHSF 
!-------------------------------------------------------------------------------
! Verifica se a == b onde a e b sao  type(tabname) 
!
!-------------------------------------------------------------------------------
function IsEqual(a,b);logical::IsEqual
!{ Variaveis da Interface
    type(tabname),intent(in)::a
    type(tabname),intent(in)::b
!}

    IsEqual=.true.
    if(a%Centre/=b%Centre) IsEqual=.false.
    if(a%NumMasterTab/=b%NumMasterTab) IsEqual=.false.
    if(a%VerMasterTab/=b%VerMasterTab) IsEqual=.false.
    if(a%VerLocTab/=b%VerLocTab) IsEqual=.false.

end function

!-------------------------------------------------------------------------------
! basetabname  | Return the base name of a BUFR table                    | SHSF 
!-------------------------------------------------------------------------------
! Retorna no nome basico de uma tabela BUFR
!  o nome basico ao conjunto de 10 caracteres na forma AAACCCBBLL
!  
!-------------------------------------------------------------------------------
function basetabname(tabin);character(len=10)::basetabname
!{ Variaveis da interface (interface variables)
  type(tabname),intent(in)::tabin    !BUFR Table (elements of the name)
!}
      write(basetabname,14)Tabin%NumMasterTab,tabin%centre,tabin%VerMasterTab,tabin%VerLocTab
14    format(2i3.3,2i2.2)
end function

!-------------------------------------------------------------------------------
! chksec4_descriptores | expanded descriptors and sec4 provided descritors | SHSF 
!-------------------------------------------------------------------------------
! Verifica os descritores expandidos contra os descritores fornecidos para a secao 4
!  Se os descritores estiverem iguais, entao retorna .true.
!  Se os descritores forem diferentes, entao retorna .false. e imprime as discrepacias 
!-------------------------------------------------------------------------------

subroutine chksec4_descriptores (desc_sec4,ndesc,ifinal,sec4,subset,err) 
!{ Variaveis da interface 
   TYPE(descbufr),pointer,DIMENSION(:) :: desc_sec4 ! Descritores expandidos provenientes da rotina expanddesc
   integer        ,intent(in) :: ndesc     ! Numero de descritores expandidos em desc_sec4
   integer        ,intent(in) :: ifinal    ! Numero de descritores expandidos em caso de replicador atrasado
   type(sec4type) ,intent(in) :: sec4      ! Secao 4 fornecida para a codificacao
   integer                    :: subset    ! Numero do subset 
   integer                    :: err
!}
!{ Variaveis locais
  integer::nds4_debug
  integer::i,i2
  integer::f,x,y,aux
  integer::ndesc_in
!}   
   
   nds4_debug=size(sec4%d,1)
   if (ifinal<ndesc) then 
     ndesc_in=ifinal-2
    else 
      ndesc_in=ndesc
    end if 
      
   if (nds4_debug>ndesc_in) nds4_debug=ndesc_in

   if (err==0) then 
   do i =1, nds4_debug
      aux= desc_sec4(i)%f*100000+desc_sec4(i)%x*1000+desc_sec4(i)%y
      if (aux/=sec4%d(i,subset)) then  
        do i2=1,i-1
           aux= desc_sec4(i2)%f*100000+desc_sec4(i2)%x*1000+desc_sec4(i2)%y
           write(*,'(i6,")",i6.6)')i2,aux
        end do
        aux= desc_sec4(i)%f*100000+desc_sec4(i)%x*1000+desc_sec4(i)%y
        write(*,'(i6,")",i6.6," Error! Expected descriptor =  ",i6.6) ')i,aux,sec4%d(i,subset)
        stop
      end if
    end do
   else
 ! print *,"nds4_debug=",nds4_debug,ndesc
  do i=1,nds4_debug
    aux= desc_sec4(i)%f*100000+desc_sec4(i)%x*1000+desc_sec4(i)%y
    write(*,'(i6.6,1x,i6.6)')aux,sec4%d(i,subset)
  end do
  end if
end subroutine


!-------------------------------------------------------------------------------
! Headerid | Obtains the Telecommunication Header                         | SHSF 
!-------------------------------------------------------------------------------
! Obtains the telecommunication header from  a BUFR message 
! Obtém o cabeçalho de telecomunicações de  uma mensagem BUFR
!-------------------------------------------------------------------------------
!HISTORICO
!  20151005  Incorporado identificacao da secao 0 junto com a indentificacao do header
!            para resolver bug que ocorre quando nao há header de telecomunicacoes. o que 
!            o que causava a perda da leitura da mensagem. 
!
!Nota: Falta verificar a possibilidade de subtracao da identificao do BUFR dentro da subrotina principal 

subroutine headerid(un,xheader)
   !{Interface variables
        integer,            intent(in)::un      !Unit 
        character(len=*),  intent(out)::xheader !Telecommunication Header 
   !}
   !{ local variables
       character(len=1)   :: b         ! 1 Byte
       character(len=4)   ::separador1 ! separador do header inicio da mensagem
       character(len=8)   ::separador  ! separador do header final da mensagem
       character(len=4)   ::bufrw      ! Indicandor de inicio da secao 0 (BUFR)
       integer            ::s          ! posicao dentro do sepadador (1, 2 ou 3)  
       logical            ::encontrou  ! 
       integer            ::irg
       integer            ::bufrid
       
       encontrou=.false.
       separador1=SOH//CR//CR//LF 
       separador=CR//CR//LF//"BUFR" 
       bufrw="BUFR"
       irg=CurrentRG
       s=1
       bufrid=1

 10    irg=irg+1
        
         read(un,rec= irg,iostat=IOERR(UN)) b
         if (IOERR(UN)>0) then
	     
	    goto 100  
         end if
         if (.not.encontrou) then 
           !-------------------------------------
           ! Searching the  beginning of a header 
           !-------------------------------------
           !{
           if (b==separador1(s:s)) then   
               s=s+1
            else 
              s=1
            end if
	    if (s==4) then 
              encontrou=.true.
              s=1
              xheader=""
            end if
            !}
         else
            !-------------------- 
            ! Reading the header    
            !--------------------
            !{
           
            
            if (ichar(b)>32) then 
               xheader=trim(xheader)//b
             else
               xheader=trim(xheader)//"."
             end if
             !}
             !----------------- 
             ! If at the end... 
             !---------------
             !{
	     if (b==BUFRW(s:s)) then  
	       s=s+1
              else 
                s=1
              end if
             if (s==5) then 
      
                encontrou=.false.
                currentRg=irg !-4
                goto 100
              end if
              !}
          end if
          
 !'Procura pelo Inicio da proxima secao 0 (palavra "BUFR")
 !'Quando BUFRID chegar a 4 entao foram enontradas todas as letras de BUFR
 !'e NBYTES=0
   ! print *,"bufrid=",bufrid,BUFRW(BUFRID:BUFRID),irg
    if (.not.encontrou) then 
      IF (BUFRW(BUFRID:BUFRID) == b) THEN
     
       bufrid = bufrid + 1
       
      ELSE
        bufrid = 1
      END IF
      
      if (bufrid==5) then 
        xheader=""
        currentRg=irg!-4
        goto 100
     END if
    end if
          
        goto 10

100 continue
    !print *,"currentRg=",currentRG
    end subroutine
!-------------------------------------------------------------------------------
! write_header |                                                           | SHSF 
!-------------------------------------------------------------------------------
! PUBCLIC WRITE_HEADER
!------------------------------------------------------------------------------
! User by bufrsplit, mbufr:write_mbufr
!
subroutine write_header1(uni,rg,hvalues)
   integer,                      intent(in)::uni     !Unit 
   integer,                   intent(inout)::rg      !Register 
   character(len=*),dimension(:),intent(in)::hvalues ! Header values
   integer::i
   character(len=3),parameter::sep=CR//CR//LF
   character(len=33)::wheader
   wheader=SOH//sep//trim(hvalues(1))
   wheader=trim(wheader)//sep
   wheader=trim(wheader)//" "//trim(hvalues(2))
   wheader=trim(wheader)//" "//trim(hvalues(3))
   wheader=trim(wheader)//" "//trim(hvalues(4))
   if (index(hvalues(5),"BUFR")==0) then
       wheader=trim(wheader)//" "//trim(hvalues(5))
   end if
   wheader=trim(wheader)//sep
    do i=1,len_trim(wheader)
       rg=rg+1
       write (uni,rec=rg) wheader(i:i)
    end do 
          
end subroutine

!-------------------------------------------------------------------------------
! write_header |                                                           | SHSF 
!-------------------------------------------------------------------------------
! PUBCLIC WRITE_HEADER
!------------------------------------------------------------------------------
! User by bufrsplit, mbufr:write_mbufr
!
subroutine write_header2(un,nnn,line_header)
   !{
   integer,               intent(in)::un         !Unit
   integer,               intent(in)::nnn         !Sequence nnn (000-999)
   character(len=*)      ,intent(in)::line_header !Header (T1T2A1A1iii_cccc_YYYGGgg(_BBB)
   !}
   integer          ::rg
   integer          ::i
   character(len=40)::wheader
   integer          ::uni
   character(len=3) ::snnn
   uni=un
   wheader=""
   if (nnn>0) then
        write(snnn,'(i3.3)')nnn
        wheader=SOH//CR//CR//LF//snnn
   end if
   wheader=trim(wheader)//CR//CR//LF//trim(line_header)//CR//CR//LF
   
   rg=currentRG
   do i=1,len_trim(wheader)
    rg=rg+1
       write (uni,rec=rg) wheader(i:i)
    end do 
   currentRG=Rg

end subroutine
!-------------------------------------------------------------------------------!
! PUBLIC  :  write_end_of_message                                        | SHSF !
! ------------------------------------------------------------------------------!
! Write CR//CR//LF _Note: Use only at the end of a BUFR message                 ! 
!-------------------------------------------------------------------------------!
subroutine write_end_of_message(uni)
    integer,intent(in)::uni
    integer::rg,i
    character(len=3)::wheader
    wheader=""
    rg=currentRG
    wheader=CR//CR//LF
    do i=1,3
    rg=rg+1
       write (uni,rec=rg) wheader(i:i)
    end do 
    currentRG=Rg
end subroutine 
!-------------------------------------------------------------------------------!
! SUBROTINA PRIVATIVA  : INIT_ERROMESSAGE                                | SHSF !
! ------------------------------------------------------------------------------!
! DEFINE AS MENSAGENS DE ERRO APRESENTADAS PELAS DIVERSAS ROTINAS DESTE MODULO  !
!-------------------------------------------------------------------------------!
! Chamdas Externas: Nao Ha	                                                !
! Chamadas Internas:Nao Ha                                                      !


SUBROUTINE INIT_ERROMESSAGE
    ERROMESSAGE(:)=""
    !
    ERROMESSAGE(01)="Maximum file addressing has been exceeded"
    !{ Codigos de erro relativos a secao 1
    ERROMESSAGE(11)="Error in Section1"
    ERROMESSAGE(12)="Error in Section1"
    ERROMESSAGE(14)="Error reading BUFR Tables"
    ERROMESSAGE(16)="Error in Section1: Invalid master table version"
    !}
    !{ Codigos de erro relativo a secao 3
      ERROMESSAGE(30)="Error in section3" 
      ERROMESSAGE(31)="Error in Section3"
      ERROMESSAGE(32)="Error in Section3: Number of subsets in section 4 is less than 1" 
    !}
    !{ Codigos de erro relativo a secao 4
      ERROMESSAGE(41)="Error in Section 4 "
      ERROMESSAGE(42)="Error in Section 4 with compressed data"
      ERROMESSAGE(43)="Error reading and decompressing section 4 data"
      ERROMESSAGE(44)="Error decompresing CCTTIAI5 data from  4 data"
    !}
    !{ Codigo de erros relativo a expansao dos descritores
      ERROMESSAGE(51)="Invalid Table C Descriptor"
      ERROMESSAGE(52)="Invalid Table B Descriptor"
      ERROMESSAGE(53)="Invalid Descriptor"
      ERROMESSAGE(54)="Expanded descriptors list is too big"
      ERROMESSAGE(55)="Error in the table D " 
      ERROMESSAGE(56)="Error in a delayed replicatior or replicator factor"
      ERROMESSAGE(57)="Error in replication processing"
    !}
    !{ Erros relativos a tabelas BUFR ou a codificacao da secao 4
      ERROMESSAGE(61)="Error in the section 4 or in the BUFR tables"
      ERROMESSAGE(62)="Error in the section 4 or in the BUFR tables"
      ERROMESSAGE(63)="Error in the section 4 or in the BUFR tables"
      ERROMESSAGE(64)="Replication Factor= undefined  value: Error in the section 4 or in the BUFR tables"
      ERROMESSAGE(65)="Replication Factor= invalid value: Error in the section 4 or in the BUFR tables"

     !}
END SUBROUTINE INIT_ERROMESSAGE
!-------------------------------------------------------------------------------!
! SUBROTINA PRIVATIVA  : ERROTLOG                               | SHSF !
! ------------------------------------------------------------------------------!
!-------------------------------------------------------------------------------!

SUBROUTINE ERRORLOG(UN,ERROR_CODE,dx,ndx,edescription,a,k0,nss)
!{
     integer,intent(in)                      ::UN,ERROR_CODE
     TYPE(descbufr),pointer,DIMENSION(:)     ::dx           ! Descritores expandidos (expanded descriptor)
     INTEGER,intent(in)                      ::ndx          ! 
     character(len=*),intent(in)             ::edescription ! Error description
     integer(kind=intk),optional,dimension(:)::a            ! Values
     integer,optional                        ::k0           ! indice inicial de a. a(k0:ndx+k0)
     integer,optional                        ::nss          ! Numero do subset
!}
!{
     character(len=1024)::fname
     integer ::i,k 
     real::aux
     integer::err,baux
     integer::log_unit
     
!}
  if(logfile) then 
     log_unit=100
     fname=trim(BUFR_FILENAME(un))//".error"
     open (100,file=fname,status="unknown",position="append")
  else
     log_unit=ERROR_unit
  end if 
    write (log_unit,'("ERROR CODE=",I2, 1x,a)')ERROR_CODE,ERROMESSAGE(ERROR_CODE)
    write (log_unit,'(a)')edescription
    write (log_unit,'(a)')cur_header
    write (log_unit,'("rg=",i10)')currentRG
    IF (present(nss))  write (log_unit,'("Subset=",i3)')nss
    write (log_unit,'(a)')'Expanded descriptors'
    if (present(a).and.present(k0)) then
      k=k0 
      call tabc_setparm(err=err) 
      do i=1,ndx   
      k=k+1
      baux=bits_tabb2(dx(i))
      aux=CVAL(A(K),DX(i))
      !write(100,*),tabb(dx(i)%f,dx(i)%x,dx(i)%y)%scale,tabc%dscale
      if (dx(i)%i==0) write(log_unit,'(i1,"-",i2.2,"-",i3.3," nbits=",i5," Val=",F20.4)')dx(i)%f,dx(i)%x,dx(i)%y,baux,aux
      if ((dx(i)%i>0).and.(dx(i+1)%i==0)) write(log_unit,'(i1,"-",i2.2,"-",i3.3," nbits=",i5)')dx(i)%f,dx(i)%x,dx(i)%y,dx(i)%i*8
      end do
   else
     do i=1,ndx   
     if (dx(i)%i==0) write(log_unit,'(i1,"-",i2.2,"-",i3.3," nbits=",i5)')dx(i)%f,dx(i)%x,dx(i)%y,bits_tabb2(dx(i))
     if ((dx(i)%i>0).and.(dx(i+1)%i==0)) write(log_unit,'(i1,"-",i2.2,"-",i3.3," nbits=",i5)')dx(i)%f,dx(i)%x,dx(i)%y,dx(i)%i*8
     end do
   end if

   if (logfile) close(100)
  
END SUBROUTINE

SUBROUTINE ERROLOG2(ERROR_CODE,sec1,edescription,header)

!{
     integer,         intent(in) ::ERROR_CODE
     type(sec1type),   intent(in)::sec1
     character(len=*),intent(in) ::edescription ! Error description
     character(len=*),intent(in) ::header
!}
     integer::log_unit=ERROR_unit
     if(PRE_ERROR_CODE/=ERROR_CODE) then
	write(log_unit,'(1x,":MBUFR-ADT: Error:",i3,"! ",a)')ERROR_CODE,trim(edescription)
	write(log_unit,'(13x,"Generater center=",i3)')sec1%center
	write(log_unit,'(13x,"BUFR Category=",i3)')sec1%btype
	write(log_unit,'(13x,"header=",a)')trim(header)
	PRE_ERROR_CODE=ERROR_CODE
     end if 
 
END SUBROUTINE 

!-------------------------------------------------------------------------------!
! PUBLIC FUNCTION GET_NAME_mbufr                                         | SHSF !
! ------------------------------------------------------------------------------!
! Obtem nome de um descritor 
!-------------------------------------------------------------------------------!

function get_name_mbufr(descriptor) ; Character(len=91)::get_name_mbufr
 !{
    integer,intent(in)::descriptor
    character(len=6)  ::auxc
    integer           ::F,X,Y
  !}
  write(auxc,'(i6.0)')descriptor
  read(auxc,'(i1,i2,i3)')F,X,Y

  if (F==0) then 
     get_name_mbufr=tabb(f,x,y)%txt!//trim(auxc)
  ELSEif((F==2).AND.(X==4)) then
   get_name_mbufr="Add associated field"
  ELSE 
    get_name_mbufr=""
  end if

end function 


!******************************************************************************
! MESSAGEPOS1
!******************************************************************************
!  Localiza todas as mensagens BUFR dentro do arquivo                          |
!  Fornece o numero de mensagens e um vetor com as posicoes (Registros) de     |
!  cada mensagens, para posteriormente serem utilizados na subrotune setpos    |
!  para acesso direto as mensagens                                             |
!  Tambem retorna informacoes sobre secao1, numero de subsets, headers e errors| 
!******************************************************************************  

SUBROUTINE  MESSAGEPOS1(uni,nm ,pos,sec1,nsubsets,nbytes,header,errors)
 	
 !{ Declaracao das Variaveis da interface 
   INTEGER,                                intent(in)::uni      ! Unidade de leitura
   INTEGER,                               intent(out)::nm       ! Numero de mensagens BUFR
   INTEGER*8,dimension(:),                intent(out)::pos      ! Position 
   TYPE(sec1type),dimension(:),         intent(inout)::sec1     ! secao1 de cada mensagem (nm)
   integer,dimension(:),                intent(inout)::nsubsets ! numero de subsets de cada mensagens (nm)
   INTEGER, dimension(:)               ,intent(inout)::nbytes   ! Tamanho de cada mensagem (nm) em bytes 
   character(len=*),optional,dimension(:),intent(out)::header   ! Encabecamento de telecomunicacos
   integer,optional                      ,intent(out)::errors   ! Numero de erros  
 !}
    
 !{ Declaracao das variavaveis locais
    TYPE(sec3TYPE)                 ::sec3       ! Secao 3 da mensagem corrente 
    TYPE(descbufr),pointer,DIMENSION(:)::desc_sec3  ! Descritores 
    integer                        ::tam_sec1   ! tamanho da secao 1
    INTEGER                        ::tam_sec2   ! tamanho da secao 2
    INTEGER                        ::tam_sec3   ! tamanho da secao 3
    integer                        ::nbytes_cur ! tamanho da mensagem corrente
    INTEGER                        ::un         ! Unidade de leitura
    integer                        ::bufr_ed    ! Edicao BUFR
    INTEGER                        ::errsec     ! Codigo de erro da secao
    integer                        ::possize    ! Tamanho dos vetores: pos,sec1,nubsets
    integer                        ::nerrors 
 !}
 !{ Inicializando variaveis

  SUBNAME="MESSAGEPOS_MBUFR"
  un=UNI
  errsec=0
  IOERR(UN)=0
  possize=size(pos,1)
  nm=0
  nerrors=0	
  allocate(sec3%d(1:1))
  allocate(desc_sec3(1:1))

 
  !------------------------------------------------------------------
  ! Obtem dados das secoes 0,1,2 e 3, header e verifica integridade
  !-----------------------------------------------------------------
  !{ 
  
  
10   nm=nm+1
	if (nm<possize) then 
		call read_info0(un,cur_header,bufr_ed,nbytes(nm))
		call read_info123(un,nbytes(nm),sec1(nm),sec3,desc_sec3,tam_sec2,tam_sec3,errsec)
		if ((errsec>0).and.(errsec/=20)) then 
			nerrors=nerrors+1
			if (present(errors)) errors=nerrors
		end if
    
		currentRG=RGINI+nbytes(nm)-1   ! POSICIONAMENTO NO FINAL DA MENSAGEM

		if(IOERR(UN)==0) then    
			pos(nm)=RGINI-1
			nsubsets(nm)=sec3%nsubsets
			if (present(header)) header(nm)=trim(cur_header)
		end if

	else
		print *,":MBUFR-ADT:Warning! Number of messagens bigger than expected"
		print *,"          :Maximun number of mensagens =",possize
	end if 


	if((IOERR(UN)==0).and.(nm<possize)) then 
		if (errsec>0) nm=nm-1 
		errsec=0 
		goto 10 
	end if

 99  deallocate (sec3%d,desc_sec3)
     nm=nm-1
!}
END SUBROUTINE
!******************************************************************************
! MESSAGEPOS2
!******************************************************************************
!  Localiza todas as mensagens BUFR dentro do arquivo   
!  Fornece o numero de mensagens e um vetor com as posicoes (Registros) de
!  cada mensagens, para posteriormente serem utilizados na subrotune setpos
!  para acesso direto as mensagens                                            | 
!******************************************************************************  

SUBROUTINE  MESSAGEPOS2(uni,nm,pos )
 	
 !{ Declaracao das Variaveis da interface 
   INTEGER,                                intent(in)::uni      ! Unidade de leitura
   INTEGER,                               intent(out)::nm       ! Numero de mensagens BUFR
   INTEGER*8,dimension(:),                intent(out)::pos      ! Posicao  
 !}
    
 !{ Declaracao das variavaveis locais
    integer                        ::nbytes_cur ! tamanho da mensagem corrente
    INTEGER                        ::un         ! Unidade de leitura
    integer                        ::bufr_ed    ! Edicao BUFR
    INTEGER                        ::errsec     ! Codigo de erro da secao
    integer                        ::possize    ! Tamanho dos vetores: pos,sec1,nubsets
 
 !}
 !}
 !{ Inicializando variaveis

  SUBNAME="MESSAGEPOS_MBUFR"
  un=UNI
  errsec=0
  IOERR(UN)=0
  possize=size(pos,1)
  nm=0
	

 
  !------------------------------------------------------------------
  ! Obtem dados da secao 0
  !-----------------------------------------------------------------
  !{ 
  
 
10   nm=nm+1
   if (nm<possize) then 
     call read_info0(un,cur_header,bufr_ed,nbytes_cur)
     currentRG=RGINI+NBYTES_cur-1         ! POSICIONAMENTO NO FINAL DA MENSAGEM
     
     if(IOERR(UN)==0) then    
       pos(nm)=RGINI-1
     end if

   else 
     print *,"Aviso! Numero de mensagens maior do que esperado"
     print *,"       Parte das mensagens foram descartadas"
     print *,"       Numero maximo de mensagens =",possize
  end if 


     if(IOERR(UN)==0) then 
        if (errsec>0) nm=nm-1  
        goto 10 
     end if

 99  continue
     nm=nm-1
      
!}
END SUBROUTINE
!******************************************************************************
! MESSAGEPOS3
!******************************************************************************
!  O mesmo que MESSAGEPOS2 porem apenas para  tipo/categoria BUFR selecionados | 
!******************************************************************************  

SUBROUTINE  MESSAGEPOS3(uni,btype,nm ,pos)
 	
 !{ Declaracao das Variaveis da interface 
   INTEGER,                                intent(in)::uni      ! Unidade de leitura
   integer,                                intent(in)::btype    ! Indica tipo/categoria BUFR selecionado 
   INTEGER,                               intent(out)::nm       ! Numero de mensagens BUFR
   INTEGER*8,dimension(:),                intent(out)::pos      ! Posicao  
   TYPE(sec1type)  ::sec1     ! secao1 de cada mensagem (nm)
   integer ::nsubsets ! numero de subsets de cada mensagens (nm)
   INTEGER ::nbytes   ! Tamanho de cada mensagem (nm) em bytes 
     
 !}
    
 !{ Declaracao das variavaveis locais
    TYPE(sec3TYPE)                 ::sec3       ! Secao 3 da mensagem corrente 
    TYPE(descbufr),pointer,DIMENSION(:)::desc_sec3  ! Descritores 
    integer                        ::tam_sec1   ! tamanho da secao 1
    INTEGER                        ::tam_sec2   ! tamanho da secao 2
    INTEGER                        ::tam_sec3   ! tamanho da secao 3
    integer                        ::nbytes_cur ! tamanho da mensagem corrente
    INTEGER                        ::un         ! Unidade de leitura
    integer                        ::bufr_ed    ! Edicao BUFR
    INTEGER                        ::errsec     ! Codigo de erro da secao
    integer                        ::possize    ! Tamanho dos vetores: pos,sec1,nubsets
    integer                        ::nerrors 
 !}
 !{ Inicializando variaveis

  SUBNAME="MESSAGEPOS_MBUFR"
  un=UNI
  errsec=0
  IOERR(UN)=0
  possize=size(pos,1)
  nm=0
  nerrors=0	
  allocate(sec3%d(1:1))
  allocate(desc_sec3(1:1))

 
  !------------------------------------------------------------------
  ! Obtem dados das secoes 0,1,2 e 3, header e verifica integridade
  !-----------------------------------------------------------------
  !{ 
  
  
10   nm=nm+1
   if (nm<possize) then 
     call read_info0(un,cur_header,bufr_ed,nbytes)
     call read_info123(un,nbytes,sec1,sec3,desc_sec3,tam_sec2,tam_sec3,errsec)
     if ((errsec>0).and.(errsec/=20)) then 
        nerrors=nerrors+1
     end if
     	
     currentRG=RGINI+nbytes-1   ! POSICIONAMENTO NO FINAL DA MENSAGEM
            

     if(IOERR(UN)==0) then    
       if (sec1%btype==btype) then 
         pos(nm)=RGINI-1
      end if   
     end if

   else
     print *,"Warning! Number of messagens bigger than expected"
     print *,"         Maximun number of mensagens =",possize
  end if 


     if((IOERR(UN)==0).and.(nm<possize)) then 
        if (errsec>0) nm=nm-1  
        goto 10 
     end if

 99  deallocate (sec3%d,desc_sec3)
     nm=nm-1
!}
END SUBROUTINE

!******************************************************************************
! find_messages_mbufr
!******************************************************************************
! find  messages by a table D descriptors                            | 
!******************************************************************************  

SUBROUTINE find_messages_mbufr(uni,descriptor,nm ,pos,sec1,nsubsets,nbytes,header,errors)
 	
 !{ Declaracao das Variaveis da interface 
   INTEGER,                                intent(in)::uni      ! Unidade de leitura
   INTEGER,                                intent(in)::descriptor
   INTEGER,                               intent(out)::nm       ! Numero de mensagens BUFR
   INTEGER*8,dimension(:),                intent(out)::pos      ! Posicao  
   TYPE(sec1type),dimension(:),         intent(inout)::sec1     ! secao1 de cada mensagem (nm)
   integer,dimension(:),                intent(inout)::nsubsets ! numero de subsets de cada mensagens (nm)
   INTEGER, dimension(:)               ,intent(inout)::nbytes   ! Tamanho de cada mensagem (nm) em bytes 
   
   character(len=*),optional,dimension(:),intent(out)::header   ! Encabecamento de telecomunicacos
   integer,optional                      ,intent(out)::errors   ! Numero de erros  
 !}
    
 !{ Declaracao das variavaveis locais
    TYPE(sec3TYPE)                 ::sec3       ! Secao 3 da mensagem corrente 
    TYPE(descbufr),pointer,DIMENSION(:)::desc_sec3  ! Descritores 
    integer                        ::tam_sec1   ! tamanho da secao 1
    INTEGER                        ::tam_sec2   ! tamanho da secao 2
    INTEGER                        ::tam_sec3   ! tamanho da secao 3
    integer                        ::nbytes_cur ! tamanho da mensagem corrente
    INTEGER                        ::un         ! Unidade de leitura
    integer                        ::bufr_ed    ! Edicao BUFR
    INTEGER                        ::errsec     ! Codigo de erro da secao
    integer                        ::possize    ! Tamanho dos vetores: pos,sec1,nubsets
    integer                        ::nerrors 
    integer                        ::i
 !}
 !{ Inicializando variaveis

  SUBNAME="MESSAGEPOS_MBUFR"
  un=UNI
  errsec=0
  IOERR(UN)=0
  possize=size(pos,1)
  nm=0
  nerrors=0	
  allocate(sec3%d(1:1))
  allocate(desc_sec3(1:1))

 
  !------------------------------------------------------------------
  ! Obtem dados das secoes 0,1,2 e 3, header e verifica integridade
  !-----------------------------------------------------------------
  !{ 
  
  
10   nm=nm+1
	if (nm<possize) then 
		call read_info0(un,cur_header,bufr_ed,nbytes(nm))
		call read_info123(un,nbytes(nm),sec1(nm),sec3,desc_sec3,tam_sec2,tam_sec3,errsec)
		if ((errsec>0).and.(errsec/=20)) then 
			nerrors=nerrors+1
			if (present(errors)) errors=nerrors
		end if
		
		currentRG=RGINI+nbytes(nm)-1   ! POSICIONAMENTO NO FINAL DA MENSAGEM

		if(IOERR(UN)==0) then    
			if((descriptor>300000).and.(descriptor<400000)) then 
				do i=1,sec3%nsubsets
					if (sec3%d(i)==descriptor) then
						print *,":MBUFR:Find ",descriptor,"on message n.",nm,trim(cur_header)
					end if
			end do
			end if
			pos(nm)=RGINI-1
			nsubsets(nm)=sec3%nsubsets
			
			if (present(header)) header(nm)=trim(cur_header)
		end if

	else
		print *,"Warning! Number of messagens bigger than expected"
		print *,"         Maximun number of mensagens =",possize
	end if 


	if((IOERR(UN)==0).and.(nm<possize)) then 
		if (errsec>0) nm=nm-1 
		errsec=0 
		goto 10 
	end if

 99  deallocate (sec3%d,desc_sec3)
     nm=nm-1
!}
END SUBROUTINE

!******************************************************************************
! 
!******************************************************************************
!  Reposiciona a leitura da mensagem na posicao definida pelo registro reg    | 
!  Reg é obtido atraves do uso de messagepos_mbufr                            |
!******************************************************************************  
subroutine setpos_mbufr(reg)
  integer*8, intent(in)::reg
  currentRG=reg
end subroutine

!******************************************************************************
! 
!******************************************************************************
!  Retorna o valor utilizado como valor indefinido neste modulo              | 
!  it return the value use as undefined value in this module                 |       
!******************************************************************************  

function undef()
  real(kind=4)::undef
  undef=-vmax_int()
end function 

!******************************************************************************
! 
!******************************************************************************
!******************************************************************************
subroutine read_info0(un,cur_header,bufr_ed,nbytes)
  integer,                    intent(inout)::un
  character(len=*),           intent(inout)::cur_header
  integer,                    intent(inout)::bufr_ed
  integer,                    intent(inout)::nbytes

 !}

 !{ Declaracao das variavaveis locais

   CHARACTER(len=1)                   ::oct
   CHARACTER(len=1),DIMENSION(4)      ::sec0 
   INTEGER,DIMENSION(2)               ::b
   INTEGER(kind=intk),DIMENSION(2)    ::a
   CHARACTER(len=4)                   ::sec5id 
   INTEGER                            ::i,RGSEC5,ERR
   INTEGER                            ::alerr
   character(len=80)                  ::ccc

  !character(len=4)               ::BUFRW
 
10  call headerid(un,cur_header)
   
 IF (IOERR(UN)/=0) then
   !if (verbose>0) print *,":MBUFR-ADT:read_info0:Error reading section 0" 
   RETURN
 end if
 !'{ Ler os demais 3 bytes da secao 0
  RGINI = currentRG - 3      !' Registro do Inicio da Mensagem

 DO i = 1, 4
  currentRG = currentRG + 1
  IF (IOERR(UN)==0) read (un,REC= currentRG) sec0(i)
  IF(IOERR(UN)/=0) RETURN
 END DO

 b(1) = 24 !'Tamanho da mensagem BUFR
 b(2) = 8  !'nuumero da Edicaao BUFR
 Call GET_OCTETS(sec0, 4, b, a, 2,0, ERR)
 NBYTES = a(1)
 bUFR_ED = a(2)
 BUFR_EDITION=BUFR_ED


!'}-------------------------------------------------------------------------
!'{ VerIFica se a edicao  BUFR ee suportada por esta rotina

  IF ((bUFR_ED < 2).Or.( bUFR_ED > 4)) THEN
   print *,"************** Warning *******************"
   print *," Error reading file=",trim(BUFR_FILENAME(un))
   PRINT *," It is not a BUFR file (edition 2, 3 or 4)"
   PRINT *,"******************************************"

  END IF
!'}-------------------------------------------------------------------------

!'{ VerIFica O FINAL DA MENSAGEM. Se passar por este teste esta
!'  mesagem possui o tamanho correto.
!   Nota: Pode ocorrer erro caso o tamanho do arquivo for menor que a pos13icao do
!   7777
   
   RGSEC5 = RGINI + NBYTES - 4 !'Inicio da secao 5
   sec5id = ""
   DO currentRG = RGSEC5, RGSEC5 + 3
   
         IF (IOERR(UN)==0) read (un,rec=currentRG) oct
	 IF (IOERR(UN)/=0) RETURN
         sec5id = TRIM(sec5id) // oct
         
   END do
   
   IF (sec5id/="7777") THEN
     print *, " ** Warning ** Unexpected end of message at position ",currentRG
     currentRG=RGINI+4
     goto 10
   END IF

 end subroutine

END MODULE
