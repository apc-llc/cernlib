*
* $Id: cstran.F,v 1.6 2001/02/07 09:27:01 couet Exp $
*
* $Log: cstran.F,v $
* Revision 1.6  2001/02/07 09:27:01  couet
* - In a DO loop, we expect a type conversion of the limit variables to the
*   index variable. Eg. consider the following program compiled with a
*   GNU-Fortran:
*
*        r=3.5
*        do i = 1, r
*           print *, i
*        enddo
*
*        end
*
*  The output of this is:
*
*  1
*  2
*  3
*
*  But when this is done in a function handled by COMIS there was *much* more
*  than three lines of output.
*
* Revision 1.5  1998/03/05 15:52:26  couet
* - .EQV. and .NEQV. added
*
* Revision 1.4  1997/02/20 12:31:26  couet
* The following routine didn't produced an error message (it should because
* the "THEN" statement is missing):
*
*       subroutine bb(i)
*       if (i.eq.1)
*         print*, 'Hello 1'
*       end if
*       print*, ' After end if'
*       end
*
* Revision 1.3  1996/10/31 20:11:46  couet
* - new version to fix the problem with variable names beginning with  a fortran
*   keyword (OPEN etc ...)
*
* Revision 1.2  1996/03/01 14:26:11  berezhno
*  two calls to csadcl routine removed from cstran routine.
*
* Revision 1.1.1.1  1996/02/26 17:16:20  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          25/09/95  16.02.51  by  Julian Bunn
*-- Author : V.Berezhnoi
         SUBROUTINE CSTRAN(IPCB,IPCE,KPRO,LSTCL)
***--------------------------------------------
*       this is comis translater
*       parameters constants are codes of interpreter
*       #beggraf .... #endgraf contents definitions of syntax.
***--------------------------------------------
       INTEGER CSLTGB,CSLTGP,CSLTLI,CSLTGI,CSLTLL
       INTEGER CSITGB,CSITGP,CSITLI,CSNUMB,CSCXNU
       INTEGER CSKIDN,CSICNS,CSNIFN
       LOGICAL BTEST,EXSTAT
#if defined(CERNLIB_PAW)
       INTEGER CSPAWV,CSKUVI
#include "comis/cshfill.inc"
#endif
        PARAMETER ( KIOR=1,   KROR=2,    KIAND=3,   KRAND=4,
     1  KINOT=5,    KRNOT=6,  KIEQ=7,    KREQ=8,    KCEQ=9,
     2  KINE=10,    KRNE=11,  KCNE=12,   KILT=13,   KRLT=14, KCLT=15,
     3  KILE=16,    KRLE=17,  KCLE=18,   KIGE=19,   KRGE=20, KCGE=21,
     4  KIGT=22,    KRGT=23,  KCGT=24,   KIUM=25,   KRUM=26, KIR=27,
     5  KRI=28,     KIR2=29,  KRI2=30,   KIPLUS=31, KRPLUS=32,
     6  KIMINU=33, KRMINU=34, KIMULT=35, KRMULT=36, KIDIV=37,
     7  KRDIV=38,  KIIPW=39,  KRIPW=40,  KRRPW=41,  KASSN=42,
     8  KCASSN=43, KCONC=44,  KLK=45,    KLAK=46,   KLCK=47,
     9  KLACK=48,  KDCV=49,   KLVL=50,   KLAL=51,   KLVG=52 )
        PARAMETER( KLAG=53,
     1  KLVP=54,   KLAP=55,   KLVF=50, KLAF=51,   KLVKA=56, KLAKA=57,
     1  KLVCL=58,  KLACL=59,  KLVCG=60,  KLACG=61,  KLVCP=62,
     2  KLACP=63,  KIRPW=64,  KIDPW=65,  KRDPW=66,  KIFUN1=67,
     3  KIFUN2=68, KIFUNN=69, KDFA=70,   KDFAS=71,  KDFCA=72,
     4  KDFCAS=73, KLEAL=74,  KLAAL=75,  KLHK=76,   KLAHK=77,
     5  KBBLK=78,  KEBLK=79,  KLECA=80,  KLACA=81,  KOPEN=82,
     6  KSTO=83,   KSTOC=84,  KSETPC=85, KAA=86,
     7  KCALLS=87, KIOEND=88, KACA=89,   KASSGO=90, KGOI=91,
     8  KCALL=92,  KRET=93,   KGO=94,    KGOC=95,   KBZI=96,
     9  KBZR=97,   KENTRY=98, KDOI=99,   KDOR=100)
       PARAMETER(   KODI=101,  KODR=102,  KPAUSE=103,
     1  KQUIT=113, KSTOP=113, KNUM=106,  KINP=107,
     2  KTYP=108,  KCONT=109, KJMPT=110, KTVW=111, KRETM=112,
     3  KEXIT=113, KPUSH=114, KLPB=115)
      PARAMETER( KDEQ=116,   KDNE=117,  KDLT=118,  KDLE=119,
     A KDGE=120, KDGT=121,   KDUM=122,  KITOD=123, KRTOD=124,
     B KDTOI=125,KDTOR=126,  KI2TOD=127,KR2TOD=128,KDPLUS=129,
     C KDMINU=130,  KDMULT=131,  KDDIV=132,  KDDPW=133, KDIPW=134,
     D KDRPW=135,  KDASS=136,    KLDK=137,   KLADK=138, KLVDL=139,
     E KLADL=140,  KLVDG=141,    KLADG=142,  KLVDP=143, KLADP=144,
     F KLVDF=139,  KLADF=140,    KLEDAL=147, KLADAL=148,KWRITE=149,
     G KREAD=150,  KIOV=151,     KIOA=152,   KSVDL=153,
     H KIFAI=154,  KIFAR=155,    KIFAD=156,  KCLOSE=157,KREWIN=158,
     X KBACKS=159, KENDFI=160,   KINQU=161 )
      PARAMETER(   KCXEQ= 163,   KCXNE=164,
     A KCXUM=165,  KITOCX=166,   KRTOCX=167,  KDTOCX=168,
     B KCXTOI=169, KCXTOR=170,   KCXTOD=171,
     Q KI2CX=172,  KR2CX=173,    KD2CX= 174, KCXPLS=175,
     C KCXMNU=176, KCXMLT=177,   KCXDIV=178, KCXPW=179, KCXIPW=180,
     D KCXRPW=181, KCXDPW=182,   KCXASS=183, KLCXK=184, KLACXK=185,
     Q KLVCXL=186,
     E KLACXL=187, KLVCXG=188,   KLACXG=189, KLVCXP=190, KLACXP=191,
     F KLVCXF=186, KLACXF=187,   KECXAL=194, KACXAL=195, KSVCXL=196,
     Q KICXPW=197, KRCXPW=198,   KDCXPW=199, KLEKV =200, KLAKV =201)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/csrec.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csdpvs.inc"
#include "comis/cskeys.inc"
#include "comis/csopen.inc"
#include "comis/csfmt.inc"
#include "comis/cskucs.inc"
#include "comis/csichv.inc"
         COMMON/CSFICD/ICOD,IFCODE(512)
#if defined(CERNLIB_VAX)
         COMMON/CSTBCS/ITBC,ITBS
#endif
CD        COMMON/CSDEBG/ICSTRA,ICSPRT,ICSPRC,ICSPRS
***********
      PARAMETER (  KLSFT=0,   KNDOL=10)
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +            KTCHAR=3,   KTLOG=4, KLLOG=1,   KTDOU=5,
     +            KTHOLL=6,   KTCMLX=7,KLCMLX=2 )
      PARAMETER (KSDIM=3, KUSEB=0)
#include "comis/cstvrs.inc"
      INTEGER IDOL(KNDOL)
      INTEGER IDENPR(8),IDENSV(8)
      CHARACTER*(KLENID) KUVNAME
      INTEGER KT1T2(7,7)
************************************************
*         1    2    3    4   5   6  7  T2
*         I    R    CH   L   D   H  CX
*  1I     1    8    0    9   10  11 14
*  2R    -8    2    0    0   12  13 15
*  3CH    0    0    3    0   0   0  0
*  4L    -9    0    0    4   0   0  0
*  5D    -10 -12    0    0   5   0  16
*  6H    -11 -13    0    0   0   6  0
*  7CX   -14 -15    0    0  -16  0  7
*  T1
***************************************
      INTEGER GSNLAB
      COMMON /CSGSCM/IGSST,JGSST,NGSST,NGSPAR,JGSSB,GSNLAB
****
*     *
*     * COMIS SYNTAX
*     *
*     PR   -> 'FUNCTION'                     DPR2    ^FS
*             ':'                 DFORS      DPR2    ^FS
*             'SUBROUTINE'                   DPR1    ^FS
*             'PROGRAM'           IDENT/E    DPR3    ^MP
*             'BLOCK' 'DATA'      IDENT/T    DPR3    ^MP
*              STYPE                                 ^SFUN
*                                  DPR3              ^MP
*     SFUN -> 'FUNCTION' / ':'     DPR2              ^FS
*                                  DPR3  AVL         ^MP
*     FS   -> IDENT                DFS               ^FSP
*     FSP  -> '(' FSPL  ')'/E                        ^FSDS
*                                                    ^FSDS
*     FSPL -> IDENT                DFSPL             ^FSPLE
*             T
*     FSPLE-> ','     IDENT/E      DFSPL             ^FSPLE
*              T
*     FSDS -> DS1                                    ^FSDS
*             DS2                                    ^FSDS
*                                  DEDS              ^ES
*     MP   -> DS1                                    ^MP
*                                  DEMDS             ^ES
*     ES   -> LABEL/T   EST                          ^ES
*             'END'/'#'            DEND
*     *DECLARATION
*     DS1  -> STYPE                                  ^AVL
*            'COMMON'/'COM'        DELIM     DCB     ^SCOM
*            'GLOBAL'/'GLO'        DELIM     DGB     ^SCOM
*            'DIMENSION'           DELIM     DDIM    ^AVL
*     *       'DIMENSION'/'DIM'     DELIM     DDIM    ^AVL
*            'VECTOR'              DELIM     DVECT   ^AVL
*            'EXTERNAL'/'EXT'      DELIM     DEXT    ^VL
*     *       'PARAMETER'/'PAR'                       ^SPAR
*            'PARAMETER'           DELIM             ^SPAR
*            'EQUIVALENCE'         DELIM   '('       ^SEQU
*            'DATA'                DELIM             ^SDATA
*            'SAVE'                DELIM     DSAV    ^VL
*            'IMPLICIT'            DELIM             ^SIMPL
*            'INTRINSIC'           DINTR
*     STYPE -> DSVJNF
*            'INTEGER'/'INT'      DELIM     DINT
*            'REAL'                DELIM     DREAL
*            'CHARACTER'/'CHAR' DELIM  DCHAR  CSCHAR  DLCHAR
*            'DOUBLE' 'PRECISION'/T DELIM    DDOUBL
*            'LOGICAL'/'LOG'       DELIM     DLOG
*            'COMPLEX'             DELIM     DCOMPL
*     DS2  ->'USE'                 DELIM     DUSE    ^VL
*     CSCHAR->'*'                                     ^SDCHAR
*                                  DCHGLO
*     SDCHAR->INUMB                DCHNUM
*            '(*)'                 DCHFP
*            '(' AE ')'            DCHEXP
*             E
*     VL   ->IDENT                 DVLN              ^VLL
*     VLL  ->','                                     ^VL
*             T
*     SCOM ->'/'                                     ^SCOMN
*             DBLKE                          AVL     ^SCOML
*     SCOMN-> IDENT '/'           DBLKN      AVL     ^SCOML
*             '/'                 DBLKE      AVL     ^SCOML
*     SCOML-> '/'                                    ^SCOMN
*              T
*     SPAR -> '('  SPARL   ')'/E
*     SPARL-> IDENT  PNAME  '='  AE    PVAL          ^SPARE
*             E
*     SPARE-> ','  ^SPARL
*              T
*     SIMPL->  'NONE'              IMPNONE
*               STYPE                                ^SIMP
*     SIMP -> '('   LETIMP   ')'                     ^SIMPE
*              E
*     SIMPE-> ','   STYPE/E                          ^SIMP
*              T
*     * EQUIVALENCE
*     SEQU -> DBEQU   SEQUL   ')'/E                  ^SEQUN
*     SEQUN-> ','  '('/E                             ^SEQU
*              T
*     SEQUL-> SVEA     DEQU                          ^SEQULN
*     SEQULN-> ','                                   ^SEQUL
*               T
*     * DATA
*     SDATA-> DBDAT    SDATAN  '/' SDATAV '/' DEDAT  ^SDATAL
*     SDATAL-> ','     ^SDATA
*               T
*     SDATAN-> SVEA    DDATN                         ^SDATNL
*     SDATNL-> ','     ^SDATAN
*               T
*     SDATAV-> SDVAL    DDATV                        ^SDATVL
*     SDATVL-> ','      ^SDATAV
*               T
*     SDVAL->  '-'         D1REP     PCONST    DUM
*              '+'         D1REP     PCONST    DUP
*              RINUMB                                ^SCONSA
*              IDENT                                 ^SCONSB
*              D1REP       PCONST
*              E
*     SCONSA-> '*'         DNREP                     ^SDVALE
*              D1REP       DACON
*     SCONSB-> '*'         DPREP                     ^SDVALE
*              D1REP       DIDEN      DPARCON
*     SDVALE-> IDENT       DIDEN      DPARCON
*              PCONST
*              E
*     PCONST-> RINUMB      DACON
*              '.TRUE.'    DTCON
*              '.FALSE.'   DFCON
*              ''''        DCCON
*              CXNUMB      DACON
*     SVEA -> IDENT        DTID                      ^SVEAC
*             E
*     SVEAC-> '('                                    ^SVEAI
*             TVAR
*             TARR
*     SVEAI-> TVAR         SINDS
*             TARR         SINDA                     ^SVEACS
*     SVEACS-> '('    SINDS
*              DSSNO
*     SINDA->  AE     DCIND                          ^SINDAN
*              E
*     SINDAN-> ','                                   ^SINDA
*              ')'           DINDE
*               E
*     SINDS-> ':'   DCIS1                            ^SKET
*              AE   DCISA   ':'                      ^SKET
*              E
*     SKET -> ')'   DCISL
*              AE   DCISB   ')'
*              E
*     AVL  -> IDENT                     DSVIDN        ^AV
*     AV   ->'(' DADB SINDEX ')'/E DADE CSCHAR DARRD  ^AVLE
*                                       CSCHAR DVARD  ^AVLE
*     AVLE -> ','                                    ^AVL
*              T
*     SINDEX-> INDEX                                 ^SINDXL
*     SINDXL-> ','                                   ^SINDEX
*               T
*     INDEX -> '*'                DADSTA
*               AE/E              DADL               ^INDEXL
*     INDEXL-> ':'                                   ^INDEXLL
*                                 DADU
*     INDEXLL-> '*'               DADSTAL
*               AE                DADLU
*               E
*     *EXPRESSION
*     EXP  ->LTL                          ^LTLE
*     LTLE ->'.EQV.'  LTL/E   DEQV/E      ^LTLE
*            '.NEQV.' LTL/E   DNEQV/E     ^LTLE
*             T
*     LTL  ->LT                           ^LTE
*     LTE  ->'.OR.' LT/E      DOR         ^LTE
*            T
*     LT   -> LMH                         ^LMHE
*     LMHE ->'.AND.' LMH/E    DAND        ^LMHE
*            T
*     LMH  ->'.NOT.'    CAE/E   DNOT      ^REXP
*            CAE                          ^REXP
*     REXP ->'.EQ.'/'==' CAE/E  DEQ       ^REXP
*           '.NE.'/'/=' CAE/E   DNE       ^REXP
*           '.LE.'/'<=' CAE/E   DLE       ^REXP
*           '.LT.'/'<'  CAE/E   DLT       ^REXP
*           '.GE.'/'>=' CAE/E   DGE       ^REXP
*           '.GT.'/'>'  CAE/E   DGT       ^REXP
*            T
*     CAE  ->AE                           ^CAEE
*     CAEE ->'//' AE/E        DCONC       ^CAEE
*            T
*     AE   ->'+' TM/E         DUP         ^ELT
*            '-' TM/E         DUM         ^ELT
*            TM                           ^ELT
*     ELT  ->'+' TM/E         DPLUS       ^ELT
*            '-' TM/E         DMINUS      ^ELT
*            T
*     TM   ->MH                           ^TML
*     TML  ->'*' MH/E         DMULT       ^TML
*            '//'/'/='        DNODIV
*            '/' MH/E         DDIV        ^TML
*            T
*     MH   ->P                            ^MHE
*     MHE  ->'**' P/E         MHE         DPOW
*            T
*     P    ->IDENT            DIDEN      ^TIDEN
*            PCONST
*            '(' EXP/E ')'/E
*     TIDEN -> DPARCON
*              VAR                        ^SFVAR
*              CVAR                       ^CST
*              ARR                        ^AIND
*              CARR                       ^CAIND
*              EXT                        ^FPAR1
*             '('             DFUN2       ^FPAR
*                             DVARL       ^TIDEN
*     SFVAR -> '('            DFUN1       ^FPAR
*               T
*     AIND  ->'('  DBA  AE/E  DIND        ^AINDL
*              T
*     AINDL ->',' AE/E        DIND        ^AINDL
*             ')'             DEIND
*     FPAR1 ->'('                         ^FPAR
*              T
*     FPAR  -> ')'            DEF0
*               PL   ')'      DEF
*               E
*     CST   ->'('                        ^CST1
*              DSTEN
*     CST1  -> ':'            DST1            ^CST2
*              DSTAB    AE/E    ':'/E    DSTAE   ^CST2
*     CST2  -> ')'               DSTL    DSTE
*              DSTAB    AE/E    ')'/E    DSTAE  DSTE
*     CAIND ->AIND                             ^CST
*     *EXECUTE STATEMENT
*     EST   -> DNSTRF
*             'GO' 'TO'/T DELIM           ^SGO
*             'IF'  DELIM                 ^SIF
*             'DO'  DELIM                 ^SDO
*            'INPUT'/'INP' DELIM   DINP        ^SCALL1
*             'TYPE'/'TYP' DELIM   DTYP        ^SCALL1
*             'RETURN'/'RET' DELIM        DRET
*             'STOP'  DELIM        DSTOP
*             'CALL'  DELIM                    ^SCALL
*             'CONTINUE'/'CON' DELIM      DCONT
*             'PAUSE'  DELIM                   ^SPAU
*             'WRITE'  DELIM '(' DOPENB SLUN SCILIST DWRITE ^SIOL
*             'READ'   DELIM        ^SREAD
*             'PRINT'  DELIM  DOPENB DLUNS  SFRMT  DWRITE   ^SIOLE
*             'FORMAT' DELIM  DSFRMT
*             'QUIT'   DELIM  DQUIT
*             'ASSIGN' DELIM  LABEL1  'TO'/E  IDENT/E   DASSGO
*             'OPEN'   DELIM  '('      DOPENB         ^SOPEN
*             'CLOSE'  DELIM  '(' 'UNIT='/T AE  ')'  DCLOSE
*             'REWIND' DELIM   AE       DREWIND
*             'BACKSPACE' DELIM AE       DBACKSP
*             'ENDFILE' DELIM   AE       DENDFIL
*             'INQUIRE' DELIM '('      DOPENB         ^SINQKW
*             IDENT           DIDEN          ^EST1
*     EST1 -> VAR             DVA            ^SASS
*             CVAR            CST/E   DCVA   ^SCASS
*             ARR             AIND/E  DAA    ^SASS
*             CARR           CAIND/E  DCAA   ^SCASS
*             EXT            DFORS           ^SCALL2
*            '='      DVARLA                 ^EST1
*                     DSUB2  DFORS           ^SCALL2
*     SPAU -> ''''            DPRPAU
*                             DPAUSE
*     * IO STATEMENTS
*     SOPEN -> 'UNIT='    AE     DOPLUN   ^SOPENE
*              'FILE='    CAE    DOPFIL   ^SOPENE
*              'STATUS='  CAE    DOPSTA   ^SOPENE
*              'ACCESS='  CAE    DOPACC   ^SOPENE
*              'FORM='    CAE    DOPFOR   ^SOPENE
*              'RECL='    AE     DOPREC   ^SOPENE
*              'IOSTAT='  AE     DOPIOS   ^SOPENE
*              'ERR='     LABEL1 DOPERR   ^SOPENE
*                         AE     DOPLUN   ^SOPENE
*               E
*     SOPENE -> ','      ^SOPEN
*               ')'      DOPENE
*                E
*     SINQKW ->    'FILE='         CAE  DOPFIL   ^SINQL
*                  'UNIT='         AE   DOPLUN   ^SINQL
*                  'ACCESS='       CAE  DINQACC  ^SINQL
*                  'BLANK='        CAE  DINQBLK  ^SINQL
*                  'DIRECT='       CAE  DINQDIR  ^SINQL
*                  'ERR='      LABEL1   DOPERR   ^SINQL
*                  'EXIST='        AE   DINQEXT  ^SINQL
*                  'FORMATTED='    CAE  DINQFMD  ^SINQL
*                  'FORM='         CAE  DINQFM   ^SINQL
*                  'IOSTAT='       AE   DOPIOS   ^SINQL
*                  'NAMED='        AE   DINQNMD  ^SINQL
*                  'NAME='         CAE  DINQNAM  ^SINQL
*                  'NEXTREC='      AE   DINQNXT  ^SINQL
*                  'NUMBER='       AE   DINQNUM  ^SINQL
*                  'OPENED='       AE   DINQOD   ^SINQL
*                  'RECL='         AE   DINQRCL  ^SINQL
*                  'SEQUENTIAL='   CAE  DINQSEQ  ^SINQL
*                  'UNFORMATTED='  CAE  DINQUNF  ^SINQL
*              E
*     SINQL  ->  ','  ^SINQKW
*                ')'  DINQEND
*                 E
*     SREAD -> '(' DOPENB SLUN            SCILIST  DREAD ^SIOL
*                  DOPENB DLUNS     SFRMT          DREAD ^SIOLE
*     SLUN  -> 'UNIT='/T  ^SLUN1
*     SLUN1 -> '*' DLUNS
*               AE DLUN
*               E
*     SFRMT -> 'FMT='/T  ^SFRMT1
*     SFRMT1-> '*' DFFRMT
*              AE  DFRMT
*              E
*     SCILIST -> ')'
*                ','  ^SCILIST1
*                 E
*     SCILIST1 -> 'FMT=' SFRMT1           ^SCILIST
*                 'ERR=' LABEL1 DOPERR    ^SCILIST
*                 'END=' LABEL1 DIOLEND   ^SCILIST
*                 'IOSTAT=' AE  DOPIOS    ^SCILIST
*                 'REC='    AE  DDAREC    ^SCILIST
*                 SFRMT                   ^SCILIST
*                 E
*     SIOL -> DEIOL   DIOEND
*             ^SIOLIST
*     SIOLE -> ','  ^SIOLIST
*              DIOEND
*     SIOLIST -> '(' DOIO  SIODO  ')'/E     ^SIOLE
*                 AE/E     DIO              ^SIOLE
*     SIODO -> SIOLIST '=' DVARIO AE DOEXP ',' AE DOEXP ^SIODO3
*              E
*     SIODO3 -> ',' AE/E      DOEXP   DOIO3
*               DOIO2
*     * END OF IO STATEMENT
*     SGO  -> LABEL1          DGO
*             IDENT           DGOI
*            '('/E            DGOC           ^SGOC
*     SGOC -> LABEL1          DLAB           ^SGOE
*     SGOE ->',' LABEL1       DLAB           ^SGOE
*            ')' ','/T  EXP/E DGOE
*     SIF  -> EXP                            ^THEN
*     THEN ->'THEN' DELIM         DTHEN      ^BTIF
*              LABEL1  DIFA   DLAB                   ^SIFA
*              DTHEN    EST/E      DFI1
*     SIFA ->  ','  LABEL1  DLAB  ','  LABEL1  DLAB  DFIA
*               E
*     BTIF -> LABEL/T  EST                   ^BTIF
*             'ELSEIF'/'ELSE IF'             ^BELSIF
*            'ELSE'           DELSE          ^BFIF
*            'ENDIF'/'END IF'/'FI'       DFI1
*     BELSIF -> DELSE  EXP/E  'THEN'/T  DELSIF      ^BTIF
*     BFIF -> LABEL/T  EST                   ^BFIF
*            'ENDIF'/'END IF'/'FI'       DFI
*     SDO  -> LABEL1/T                       ^SDO1
*     SDO1 -> 'WHILE'  DOWH  EXP/E   DWHILE  ^SDOB
*             IDENT '='    DIDEN     DOV     ^SDOL
*     SDOL -> EXP  DOEXP ',' EXP     DOEXP   ^SDO3
*     SDO3 ->',' EXP/E   DOEXP    DO3        ^SDOB
*                                 DO2        ^SDOB
*     SDOB -> TDOE                 DOE
*      LABEL/T   EST                         ^SDOB
*           'ENDDO'/'END DO'/'OD'    DOE
*     SASS ->  '='  EXP/E         DASS
*              DFUN1              DFORS      ^SCALL2
*     SCASS-> '='   EXP/E         DCASS
*     SCALL->IDENT/E              DIDEN      ^SCALE
*     SCALE-> EXT                            ^PSUB
*                           DFUN3            ^PSUB
*     SCALL1-> PL                DESUB
*     SCALL2-> PSUB
*              PL                DESUB
*     PSUB  -> DSETC '('  PL  ')'/E       DESUB
*              SENDCL                     DESUB
*     PL    -> ARG                           ^RPL
*               T
*     RPL   -> ','    ARG/E                  ^RPL
*              T
*     ARG   -> ',' DBP    ASKIP  DEP
*              '[' DBP   BBLOCK              ^ABLOCK
*                  DBP   AE/E    DEP
*     ABLOCK-> ']' EBLOCK        DEP
*                  LABEL/T       EST/E       ^ABLOCK
*     SENDCL ->    ';'
*                  DSVJNF
*                  DTESTC
*                 '#'             DRSJNT
*            'OD'/'FI'/'ENDDO'/'ENDIF'/'END' DELIM DRSJNT
****
      INTEGER GSCMST(3046),GSSTRC(135),GSSTRP(255)
      INTEGER GSCMS1(361),GSCMS2(361),GSCMS3(361),GSCMS4(361),
     *GSCMS5(361),GSCMS6(361),GSCMS7(361),GSCMS8(361),GSCMS9(158)
      EQUIVALENCE (GSCMST(1),GSCMS1(1)),(GSCMST(362),GSCMS2(1)),
     *(GSCMST(723),GSCMS3(1)),(GSCMST(1084),GSCMS4(1)),(GSCMST(1445),
     *GSCMS5(1)),(GSCMST(1806),GSCMS6(1)),(GSCMST(2167),GSCMS7(1)),
     *(GSCMST(2528),GSCMS8(1)),(GSCMST(2889),GSCMS9(1))
      DATA GSCMS1/
     *9,4,1,6,3,8,88,0,19,4,3,6,33,6,3,8,88,0,27,4,5,6,2,8,88,0,41,4,7,
     *6,5,5,36,3,1,6,4,8,160,0,56,4,9,4,11,6,5,5,51,1,6,4,8,160,0,62,7,
     *311,8,68,0,0,6,4,8,160,0,80,4,1,5,75,4,3,6,3,8,88,0,0,6,4,7,913,8,
     *160,0,0,6,5,6,6,8,96,0,110,4,13,7,114,4,15,5,107,3,1,8,142,0,0,8,
     *142,0,122,6,5,6,9,8,125,0,0,1,0,139,4,17,6,5,5,134,3,1,6,9,8,125,
     *0,0,1,0,148,7,193,8,142,0,154,7,384,8,142,0,0,6,7,8,172,0,166,7,
     *193,8,160,0,0,6,8,8,172,0,183,6,34,5,178,1,7,1624,8,172,0,0,4,19,
     *5,190,4,21,6,36,0,199,7,311,8,913,0,213,4,23,5,206,4,25,6,176,6,
     *11,8,447,0,227,4,27,5,220,4,29,6,176,6,12,8,447,0,237,4,31,6,176,
     *6,13,8,913,0,247,4,33,6,176,6,210,8,913,0,261,4,35,5,254,4,37,6,
     *176,6,18,8,430,0,269,4,39,6,176,8,492,0,279,4,41,6,176,4,13,8,572,
     *0,287,4,11,6,176,8,618,0,297,4,43,6,176,6,20,8,430,0,305,4,45,6,
     *176,8,531,0,0,4,47,6,21,0,315,6,167,0,327,4,49,5,322,4,51,6,176,6,
     *14,0,335,4,53,6,176,6,15,0,351,4,55,5,342,4,57,6,176,6,16,7,394,6,
     *27,0,364,4,59,4,61,5,359,1,6,176,6/
      DATA GSCMS2/
     *17,0,376,4,63,5,371,4,65,6,176,6,22,0,0,4,67,6,176,6,23,0,0,4,69,
     *6,176,6,19,8,430,0,400,4,71,8,404,0,0,6,24,0,410,6,10,6,25,0,416,
     *4,73,6,26,0,426,4,13,7,1264,4,15,6,29,0,0,3,1,0,0,6,5,6,30,8,438,
     *0,444,4,17,8,430,0,0,1,0,453,4,75,8,461,0,0,6,31,7,913,8,483,0,
     *473,6,5,4,75,6,32,7,913,8,483,0,0,4,75,6,31,7,913,8,483,0,489,4,
     *75,8,461,0,0,1,0,0,4,13,7,504,4,15,5,503,3,1,0,518,6,5,6,63,4,77,
     *7,1264,6,64,8,522,0,0,3,1,0,528,4,17,8,504,0,0,1,0,537,4,79,6,65,
     *0,0,7,311,8,543,0,553,4,13,6,66,4,15,8,557,0,0,3,1,0,569,4,17,7,
     *311,5,566,3,1,8,543,0,0,1,0,0,6,54,7,601,4,15,5,583,3,1,8,586,0,
     *598,4,17,4,13,5,595,3,1,8,572,0,0,1,0,0,7,795,6,55,8,609,0,615,4,
     *17,8,601,0,0,1,0,0,6,56,7,643,4,75,7,660,4,75,6,58,8,634,0,640,4,
     *17,8,618,0,0,1,0,0,7,795,6,57,8,651,0,657,4,17,8,643,0,0,1,0,0,7,
     *677,6,59,8,668,0,674,4,17,8,660,0,0,1,0,687,4,81,6,61,7,765,6,73,
     *0,697,4,83,6,61,7,765,6,72,0,703,6,92,8,719,0,709,6,5,8,733,0,715,
     *6,61,7,765,0,0,3,1,0,727,4,71,6/
      DATA GSCMS3/
     *60,8,749,0,0,6,61,6,90,0,741,4,71,6,62,8,749,0,0,6,61,6,94,6,87,0,
     *757,6,5,6,94,6,87,0,761,7,765,0,0,3,1,0,771,6,92,6,90,0,777,4,85,
     *6,88,0,783,4,87,6,89,0,789,4,89,6,91,0,0,6,93,6,90,0,803,6,5,6,44,
     *8,807,0,0,3,1,0,813,4,13,8,821,0,817,6,45,0,0,6,46,0,827,6,45,7,
     *873,0,0,6,46,7,845,8,835,0,841,4,13,7,873,0,0,6,47,0,853,7,1264,6,
     *52,8,857,0,0,3,1,0,863,4,17,8,845,0,869,4,15,6,53,0,0,3,1,0,881,4,
     *3,6,48,8,895,0,891,7,1264,6,50,4,3,8,895,0,0,3,1,0,901,4,15,6,49,
     *0,909,7,1264,6,51,4,15,0,0,3,1,0,0,6,5,6,28,8,921,0,943,4,13,6,37,
     *7,960,4,15,5,934,3,1,6,38,7,394,6,43,8,951,0,0,7,394,6,42,8,951,0,
     *957,4,17,8,913,0,0,1,0,0,7,975,8,966,0,972,4,17,8,960,0,0,1,0,981,
     *4,71,6,211,0,0,7,1264,5,988,3,1,6,39,8,993,0,999,4,3,8,1003,0,0,6,
     *41,0,1009,4,71,6,212,0,1015,7,1264,6,40,0,0,3,1,0,0,7,1064,8,1025,
     *0,1043,4,91,7,1064,5,1034,3,1,6,70,5,1040,3,1,8,1025,0,1061,4,93,
     *7,1064,5,1052,3,1,6,71,5,1058,3,1,8,1025,0,0,1,0,0,7,1087,8,1070,
     *0,1084,4,95,7,1087,5,1079,3,1,6,67,8,1070,0/
      DATA GSCMS4/
     *0,1,0,0,7,1110,8,1093,0,1107,4,97,7,1110,5,1102,3,1,6,68,8,1093,0,
     *0,1,0,1124,4,99,7,1241,5,1119,3,1,6,69,8,1130,0,0,7,1241,8,1130,0,
     *1148,4,101,5,1137,4,103,7,1241,5,1143,3,1,6,78,8,1130,0,1166,4,
     *105,5,1155,4,107,7,1241,5,1161,3,1,6,79,8,1130,0,1184,4,109,5,
     *1173,4,111,7,1241,5,1179,3,1,6,81,8,1130,0,1202,4,113,5,1191,4,
     *115,7,1241,5,1197,3,1,6,80,8,1130,0,1220,4,117,5,1209,4,119,7,
     *1241,5,1215,3,1,6,82,8,1130,0,1238,4,121,5,1227,4,123,7,1241,5,
     *1233,3,1,6,83,8,1130,0,0,1,0,0,7,1264,8,1247,0,1261,4,125,7,1264,
     *5,1256,3,1,6,84,8,1247,0,0,1,0,1278,4,83,7,1329,5,1273,3,1,6,72,8,
     *1298,0,1292,4,81,7,1329,5,1287,3,1,6,73,8,1298,0,0,7,1329,8,1298,
     *0,1312,4,83,7,1329,5,1307,3,1,6,74,8,1298,0,1326,4,81,7,1329,5,
     *1321,3,1,6,75,8,1298,0,0,1,0,0,7,1376,8,1335,0,1349,4,71,7,1376,5,
     *1344,3,1,6,76,8,1335,0,1359,4,125,5,1356,4,107,6,85,0,1373,4,75,7,
     *1376,5,1368,3,1,6,77,8,1335,0,0,1,0,0,7,1399,8,1382,0,1396,4,127,
     *7,1399,5,1391,3,1,7,1382,6,86,0,0,1,0,1407,6,5,6,94,8,1427,0,1411,
     *7,765,0,0,4,13,7,1019,5,1420,3,1,4,15,5,1426,3,1,0,1431,6,87,0,
     *1437,6,95,8,1475,0,1443,6,97,8,1552,0,1449,6/
      DATA GSCMS5/
     *96,8,1486,0,1455,6,98,8,1618,0,1461,6,118,8,1525,0,1469,4,13,6,
     *130,8,1534,0,0,6,122,8,1427,0,1483,4,13,6,129,8,1534,0,0,1,0,1502,
     *4,13,6,99,7,1264,5,1497,3,1,6,101,8,1505,0,0,1,0,1519,4,17,7,1264,
     *5,1514,3,1,6,101,8,1505,0,0,4,15,6,102,0,1531,4,13,8,1534,0,0,1,0,
     *1540,4,15,6,136,0,1548,7,2926,4,15,6,137,0,0,3,1,0,1558,4,13,8,
     *1562,0,0,6,125,0,1570,4,3,6,123,8,1590,0,0,6,124,7,1264,5,1579,3,
     *1,4,3,5,1585,3,1,6,126,8,1590,0,1598,4,15,6,128,6,127,0,0,6,124,7,
     *1264,5,1607,3,1,4,15,5,1613,3,1,6,126,6,127,0,0,7,1486,8,1552,0,
     *1628,6,177,0,1641,4,129,4,131,5,1636,1,6,176,8,2524,0,1649,4,133,
     *6,176,8,2583,0,1657,4,135,6,176,8,2728,0,1671,4,137,5,1664,4,139,
     *6,176,6,119,8,2888,0,1685,4,141,5,1678,4,143,6,176,6,120,8,2888,0,
     *1697,4,145,5,1692,4,147,6,176,6,163,0,1705,4,149,6,176,6,164,0,
     *1713,4,151,6,176,8,2864,0,1725,4,153,5,1720,4,155,6,176,6,165,0,
     *1733,4,157,6,176,8,1968,0,1751,4,159,6,176,4,13,6,193,7,2312,7,
     *2362,6,180,8,2434,0,1759,4,161,6,176,8,2286,0,1775,4,163,6,176,6,
     *193,6,183,7,2337,6,180,8,2444,0,1783,4,165,6,176,6,182,0,1791,4,
     *167,6,176,6,166,0,1813,4,169,6,176,6,35,4,131,5,1804,3,1,6,5/
      DATA GSCMS6/
     *5,1810,3,1,6,144,0,1825,4,171,6,176,4,13,6,193,8,1978,0,1844,4,
     *173,6,176,4,13,4,175,5,1837,1,7,1264,4,15,6,203,0,1854,4,177,6,
     *176,7,1264,6,204,0,1864,4,179,6,176,7,1264,6,205,0,1874,4,181,6,
     *176,7,1264,6,206,0,1886,4,183,6,176,4,13,6,193,8,2086,0,0,6,5,6,
     *94,8,1894,0,1902,6,95,6,142,8,2832,0,1916,6,97,7,1552,5,1911,3,1,
     *6,138,8,2852,0,1930,6,96,7,1486,5,1925,3,1,6,140,8,2832,0,1944,6,
     *98,7,1618,5,1939,3,1,6,141,8,2852,0,1952,6,118,6,33,8,2894,0,1960,
     *4,77,6,121,8,1894,0,0,6,132,6,33,8,2894,0,1974,4,89,6,171,0,0,6,
     *172,0,1988,4,175,7,1264,6,194,8,2070,0,1998,4,185,7,1241,6,195,8,
     *2070,0,2008,4,187,7,1241,6,196,8,2070,0,2018,4,189,7,1241,6,197,8,
     *2070,0,2028,4,191,7,1241,6,198,8,2070,0,2038,4,193,7,1264,6,199,8,
     *2070,0,2048,4,195,7,1264,6,200,8,2070,0,2058,4,197,6,35,6,201,8,
     *2070,0,2066,7,1264,6,194,8,2070,0,0,3,1,0,2076,4,17,8,1978,0,2082,
     *4,15,6,202,0,0,3,1,0,2096,4,185,7,1241,6,195,8,2270,0,2106,4,175,
     *7,1264,6,194,8,2270,0,2116,4,189,7,1241,6,103,8,2270,0,2126,4,199,
     *7,1241,6,104,8,2270,0,2136,4,201,7,1241,6,105,8,2270,0,2146,4,197,
     *6,35,6,201,8,2270,0,2156,4,203,7,1264,6,106,8,2270,0,2166,4,205,7,
     *1241,6,107,8,2270,0,2176/
      DATA GSCMS7/
     *4,191,7,1241,6,108,8,2270,0,2186,4,195,7,1264,6,200,8,2270,0,2196,
     *4,207,7,1264,6,109,8,2270,0,2206,4,209,7,1241,6,110,8,2270,0,2216,
     *4,211,7,1264,6,111,8,2270,0,2226,4,213,7,1264,6,112,8,2270,0,2236,
     *4,215,7,1264,6,113,8,2270,0,2246,4,193,7,1264,6,114,8,2270,0,2256,
     *4,217,7,1241,6,115,8,2270,0,2266,4,219,7,1241,6,116,8,2270,0,0,3,
     *1,0,2276,4,17,8,2086,0,2282,4,15,6,117,0,0,3,1,0,2300,4,13,6,193,
     *7,2312,7,2362,6,181,8,2434,0,0,6,193,6,183,7,2337,6,181,8,2444,0,
     *0,4,175,5,2318,1,8,2321,0,2327,4,71,6,183,0,2333,7,1264,6,184,0,0,
     *3,1,0,0,4,221,5,2343,1,8,2346,0,2352,4,71,6,185,0,2358,7,1264,6,
     *207,0,0,3,1,0,2366,4,15,0,2372,4,17,8,2376,0,0,3,1,0,2384,4,221,7,
     *2346,8,2362,0,2394,4,197,6,35,6,201,8,2362,0,2404,4,223,6,35,6,
     *208,8,2362,0,2414,4,195,7,1264,6,200,8,2362,0,2424,4,225,7,1264,6,
     *209,8,2362,0,2430,7,2337,8,2362,0,0,3,1,0,2440,6,186,6,187,0,0,8,
     *2454,0,2450,4,17,8,2454,0,0,6,187,0,2470,4,13,6,188,7,2482,4,15,5,
     *2467,3,1,8,2444,0,0,7,1264,5,2477,3,1,6,189,8,2444,0,2502,7,2454,
     *4,77,6,190,7,1264,6,157,4,17,7,1264,6,157,8,2506,0,0,3,1,0,2520,4,
     *17,7,1264,5,2515,3,1,6,157,6,192,0,0,6,191,0,2530,6,35,6/
      DATA GSCMS8/
     *152,0,2536,6,5,6,153,0,0,4,13,5,2543,3,1,6,151,8,2548,0,0,6,35,6,
     *150,8,2556,0,2566,4,17,6,35,6,150,8,2556,0,0,4,15,4,17,5,2574,1,7,
     *1019,5,2580,3,1,6,154,0,0,7,1019,8,2589,0,2599,4,227,6,176,6,145,
     *8,2641,0,2609,6,35,6,178,6,150,8,2621,0,0,6,145,7,1624,5,2618,3,1,
     *6,148,0,2637,4,17,6,35,6,150,4,17,6,35,6,150,6,179,0,0,3,1,0,2652,
     *6,34,5,2647,1,7,1624,8,2641,0,2662,4,229,5,2659,4,231,8,2684,0,
     *2670,4,233,6,147,8,2703,0,0,4,235,5,2681,4,237,5,2681,4,239,6,148,
     *0,0,6,147,7,1019,5,2693,3,1,4,227,5,2698,1,6,146,8,2641,0,2714,6,
     *34,5,2709,1,7,1624,8,2703,0,0,4,235,5,2725,4,237,5,2725,4,239,6,
     *149,0,0,6,35,5,2734,1,8,2737,0,2753,4,241,6,155,7,1019,5,2748,3,1,
     *6,158,8,2801,0,0,6,5,4,77,6,94,6,156,8,2765,0,0,7,1019,6,157,4,17,
     *7,1019,6,157,8,2779,0,2795,4,17,7,1019,5,2788,3,1,6,157,6,159,8,
     *2801,0,0,6,160,8,2801,0,2807,6,161,6,162,0,2818,6,34,5,2813,1,7,
     *1624,8,2801,0,0,4,243,5,2829,4,245,5,2829,4,247,6,162,0,2844,4,77,
     *7,1019,5,2841,3,1,6,143,0,0,6,129,6,33,8,2894,0,0,4,77,7,1019,5,
     *2861,3,1,6,139,0,0,6,5,5,2871,3,1,6,94,8,2876,0,2882,6,118,8,2904,
     *0,0,6,131,8,2904,0,0/
      DATA GSCMS9/
     *7,2926,6,135,0,2898,7,2904,0,0,7,2926,6,135,0,2920,6,168,4,13,7,
     *2926,4,15,5,2917,3,1,6,135,0,0,7,3005,6,135,0,2932,7,2950,8,2935,
     *0,0,1,0,2947,4,17,7,2950,5,2944,3,1,8,2935,0,0,1,0,2960,4,17,6,
     *133,6,175,6,134,0,2970,4,249,6,133,6,173,8,2982,0,0,6,133,7,1264,
     *5,2979,3,1,6,134,0,2990,4,251,6,174,6,134,0,0,6,34,5,2996,1,7,
     *1624,5,3002,3,1,8,2982,0,3009,4,253,0,3013,6,167,0,3017,6,169,0,
     *3023,4,21,6,170,0,0,4,247,5,3042,4,239,5,3042,4,243,5,3042,4,235,
     *5,3042,4,19,6,176,6,170,0/
      DATA GSSTRC/
     *4HFUNC,4HTION,4H:SUB,4HROUT,4HINEP,4HROGR,4HAMBL,4HOCKD,4HATA(,
     *4H),EN,4HD#CO,4HMMON,4HGLOB,4HALDI,4HMENS,4HIONV,4HECTO,4HREXT,
     *4HERNA,4HLPAR,4HAMET,4HEREQ,4HUIVA,4HLENC,4HESAV,4HEIMP,4HLICI,
     *4HTINT,4HRINS,4HICIN,4HTEGE,4HREAL,4HCHAR,4HACTE,4HRDOU,4HBLEP,
     *4HRECI,4HSION,4HLOGI,4HCALC,4HOMPL,4HEXUS,4HE*(*,4H)/=N,4HONE-,
     *4H+.TR,4HUE.F,4HALSE,4H.'.E,4HQV.N,4HEQV.,4HOR.A,4HND.N,4HOT.E,
*                                     // must end at column 72
*23456789012345678901234567890123456789012345678901234567890123456789012
     *4HQ.==,4H.NE.,4HLE.<,4H=.LT,4H.GE.,4H>=.G,                  4HT.//
     *,4H**GO,4HIFIN,
     *4HPUTY,4HPERE,4HTURN,4HSTOP,4HCALL,4HCONT,4HINUE,4HPAUS,4HEWRI,
     *4HTERE,4HADPR,4HINTF,4HORMA,4HTQUI,4HTASS,4HIGNO,4HPENC,4HLOSE,
     *4HUNIT,4H=REW,4HINDB,4HACKS,4HPACE,4HNDFI,4HLEIN,4HQUIR,4HEFIL,
     *4HE=ST,4HATUS,4H=ACC,4HESS=,4HFORM,4H=REC,4HL=IO,4HSTAT,4H=ERR,
     *4H=BLA,4HNK=D,4HIREC,4HT=EX,4HIST=,4HFORM,4HATTE,4HD=NA,4HMED=,
     *4HNAME,4H=NEX,4HTREC,4H=NUM,4HBER=,4HOPEN,4HED=S,4HEQUE,4HNTIA,
     *4HL=UN,4HFORM,4HATTE,4HD=FM,4HT=EN,4HD=TH,4HENEL,4HSEIF,4HELSE,
     *4H IFE,4HNDIF,4HEND ,4HIFWH,4HILEN,4HDDOE,4HND D,4HOD[],4H;   /
      DATA GSSTRP/
     *0,8,8,1,9,10,19,7,26,5,31,4,35,1,36,1,37,1,38,3,41,1,42,6,42,3,48,
     *6,48,3,54,9,63,6,69,8,69,3,77,9,86,11,97,4,101,8,109,9,118,7,109,
     *3,124,4,128,9,128,4,137,6,143,9,152,7,152,3,159,7,166,3,169,1,170,
     *3,173,1,174,1,175,4,179,1,180,1,181,6,186,7,193,1,194,5,198,6,203,
     *4,206,5,210,5,214,4,218,2,220,4,173,2,223,4,227,2,229,4,227,1,232,
     *4,236,2,238,4,236,1,242,2,244,2,246,2,66,2,248,2,137,2,250,5,250,
     *3,254,4,254,3,258,6,258,3,264,4,268,4,272,8,272,3,280,5,285,5,290,
     *4,294,5,299,6,305,4,309,6,315,4,319,5,324,5,329,6,335,9,343,7,350,
     *7,357,5,362,7,369,7,376,5,381,5,386,7,393,4,397,6,403,7,410,6,416,
     *10,426,6,432,5,437,8,445,7,452,7,459,11,470,12,482,4,486,4,441,4,
     *490,4,494,6,500,7,494,4,507,5,512,6,249,2,518,5,522,5,527,6,532,2,
     *534,1,535,1,536,1,-1/
      DATA KT1T2/
     +     1, -8,  0, -9, -10, -11, -14,
     +     8,  2,  0,  0, -12, -13, -15,
     +     0,  0,  3,  0,  0,   0,   0,
     +     9,  0,  0,  4,  0,   0,   0,
     +     10, 12, 0,  0,  5,   0,  -16,
     +     11, 13, 0,  0,  0,   6,   0,
     +     14, 15, 0,  0,  16,  0,   7 /
      DATA IOST/0/
**      DATA NHSSC/4H;   /, NHSEQ/4H=   /, NHSBR/4H)   /
      DATA NMPAWC/4HPAWC/
      CALL CCOPYA(IDEN,IDENPR,NWIDEN)
      NCIDPR=NCIDEN
      NWIDPR=NWIDEN
      IPCB=ITA+1
      IPCE=IPCB+3
      LSFT=0
      NDOL=0
      LOCLAB=-1
      KSTRST=0
      ITS=IBSEM
      IQ(IBIMPL)=0
      NSTRL=0
      IPCBL=0
      MTYPE=0
      IMNONE=0
      EXSTAT=.FALSE.
      LSTCL=0
      LISTCB=0
#if defined(CERNLIB_PAW)
      NCLHFI=0
#endif
      JGSSB=0
      NGSPAR=0
    1 CALL CSGSCL(GSCMST(1),GSSTRC(1),GSSTRP(1),IPGB)
      GO TO (2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     *21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
     *40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,
     *59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,
     *78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,
     *97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,
     *112,113,114,115,116,117,118,119,120,121,122,123,124,125,
     *126,127,128,129,130,131,132,133,134,135,136,137,138,139,
     *140,141,142,143,144,145,146,147,148,149,150,151,152,153,
     *154,155,156,157,158,159,160,161,162,163,164,165,166,167,
     *168,169,170,171,172,173,174,175,176,177,178,179,180,181,
     *182,183,184,185,186,187,188,189,190,191,192,193,194,195,
     *196,197,198,199,200,201,202,203,204,205,206,207,208,209,
     *210,211,212,213),GSNLAB
    2 CONTINUE
CD      IF(ICSPRT.EQ.1)CALL PRTAB
CD      IF(ICSPRC.EQ.1)CALL PRCODE(IPCB,IPCE)
      IF(IGSST.LE.0)THEN
        KPRO=IGSST
        CALL CCOPYA(IDENPR,IDEN,NWIDPR)
        NCIDEN=NCIDPR
        NWIDEN=NWIDPR
      ENDIF
      IQ(IBIMPL)=0
      CALL CSFRCBL(LISTCB)
      RETURN
*   DPR1
    3 CONTINUE
      KPRO=1
      ISTATE=2
      GO TO 1
*   DPR2
    4 CONTINUE
      KPRO=2
      ISTATE=2
      GO TO 1
*   DPR3
    5 CONTINUE
      KPRO=3
      ISTATE=3
      KEYWD=0
      CALL CSBUSE(-1)
      GO TO 1
*   IDENT
    6 CONTINUE
 1000 K=MKBLAN(JGSST,NGSST)
*  TEST NGSST=0
      IF(NGSST.LE.0) THEN
*       PAUSE 'IDENT'
        IF(NGSST.EQ.0)THEN
          CALL CSRD(JGSST,NGSST)
          GO TO 1000
        ELSE
          IGSST=-39
          GO TO 1
        ENDIF
      ENDIF
      J=JGSST
      N=NGSST
      IGSST=MCSIDENT(JGSST,NGSST)
      IF(IGSST.EQ.0 )GO TO 1
      IF(KSTRST.EQ.0)THEN
        IF(IGSST.EQ.3 .AND. MLEQS(J,JFI1,3).EQ.1)GO TO 1001
      ELSEIF(IGSST.EQ.5) THEN
        IF(MLEQS(J,JFI1,5).EQ.1)GO TO 1001
        IF(MLEQS(J,JOD1,5).EQ.1)GO TO 1001
      ELSEIF(IGSST.EQ.2) THEN
        IF(MLEQS(J,JFI2,2).EQ.1)GO TO 1001
        IF(MLEQS(J,JOD2,2).EQ.1)GO TO 1001
      ELSEIF(IGSST.EQ.4)THEN
        IF(MLEQS(J,JTHEN,4).EQ.1)GO TO 1001
        IF(MLEQS(J,JELSE,4).EQ.1)GO TO 1001
      ELSEIF(IGSST.EQ.6)THEN
        IF(MLEQS(J,JELSE,6).EQ.1)GO TO 1001
      ELSEIF(IGSST.EQ.3) THEN
        IF(MLEQS(J,JFI1,3).EQ.1)GO TO 1001
      ENDIF
      GO TO 1
 1001 JGSST=J
      NGSST=N
      IGSST=0
      GO TO 1
*   DFS
    7 CONTINUE
      CALL CCOPYA(IDEN,IDENPR,NWIDEN)
      NCIDPR=NCIDEN
      NWIDPR=NWIDEN
      I=CSLTLI(IPVS)
      IF(KPRO.EQ.2)THEN
        IF(MTYPE.EQ.0)THEN
          ITYPGI=CSKIDN(JID,LENEGI)
        ELSE
          ITYPGI=MTYPE
          LENEGI=MLENEL
          IF(MTYPE.EQ.KTCHAR)GO TO 711
        ENDIF
      ELSE
        ITYPGI=KTHOLL
        LENEGI=KLINT
      ENDIF
      NPARFS=0
      LSFT=KLSFT
      ISHGI=LSFT
      NUMGI=-2
      MODEGI=0
      I=CSITLI(IPVS)
      LSFT=LSFT+KDLEN
      CALL CSBUSE(0)
      GO TO 1
*   DEDS
    8 CONTINUE
 800  I=IPLI
       NDVAR=1
*  WHILE ELEMENT DO
      CALL CSPECB(LISTCB)
      CALL CSDEDS(I)
      CALL CSMARK(400)
      EXSTAT=.TRUE.
      GO TO 1
*   DEMDS
    9 CONTINUE
        GO TO 800
*   DFSPL
   10 CONTINUE
      I=CSLTLI(IPVS)
      IF(I.EQ.0)THEN
        NUMGI=-1
        ITYPGI=CSKIDN(JID,LENEGI)
        NPARFS=NPARFS+1
        ISHGI=NPARFS
        I=CSITLI(IPVS)
      ELSE
        IGSST=-1
      ENDIF
      GO TO 1
*   INUMB
   11 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      IGSST=MLNUMB(JGSST,NGSST,INUM)
      GO TO 1
*   DCB
   12 CONTINUE
*cb      KGLO=0
      KEYWD=1
      GO TO 808
*   DGB
   13 CONTINUE
*cb      KGLO=0
      KEYWD=2
      GO TO 808
*   DDIM
   14 CONTINUE
      KEYWD=3
 808  MTYPE=0
      MLENEL=0
      GO TO 1
*   DVECT
  211 CONTINUE
#if defined(CERNLIB_PAW)
      KEYWD=22
      CALL CCOPYS(MJSCHA('CSPAWC  '),JID,8)
      NCIDEN=6
      NWIDEN=2
      IGB=CSLTGB(IPVSB)
      IF(IGB.EQ.0)IGSST=-1
*cb      KGLO=ICGB
      NUMGB=IGB+KSADGB
      MTYPE=0
      MLELEM=0
      CALL CSMARK(200)
#endif
#if !defined(CERNLIB_PAW)
      IGSST=-1
#endif
      GO TO 1
*   DINT
   15 CONTINUE
      KEYWD=4
      MTYPE=KTINT
      MLENEL=KLINT
      GO TO 1
*   DREAL
   16 CONTINUE
      KEYWD=4
      MTYPE=KTREAL
      MLENEL=KLREAL
      GO TO 1
*   DCHAR
   17 CONTINUE
      KEYWD=5
      MTYPE=KTCHAR
      MLENEL=-1
      GO TO 1
*   DDOUBL
   18 CONTINUE
      KEYWD=4
      MTYPE=KTDOU
      MLENEL=KDLEN
      GO TO 1
*   DEXT
   19 CONTINUE
      KEYWD=9
      GO TO 808
*   DUSE
   20 CONTINUE
      KEYWD=10
      GO TO 808
*   DSAV
   21 CONTINUE
      KEYWD=11
      GO TO 808
*   DINTR
   22 CONTINUE
      KEYWD=12
      IGSST=-3
      GO TO 1
*   DLOG
   23 CONTINUE
      KEYWD=4
      MTYPE=KTLOG
      MLENEL=KLLOG
      GO TO 1
*   DCOMPL
   24 CONTINUE
      KEYWD=4
      MTYPE=KTCMLX
      MLENEL=KLCMLX
      GO TO 1
*   DCHGLO
   25 CONTINUE
      LENEL=MLENEL
      NWIDSV=0
      GO TO 1
*   DCHNUM
   26 CONTINUE
      NWIDSV=0
      LENEL=INUM
      IF(MTYPE.NE.KTCHAR)IGSST=-1
      GO TO 1
*   DCHFP
   27 CONTINUE
      NWIDSV=0
      LENEL=0
      IF(MTYPE.NE.KTCHAR)IGSST=-1
      GO TO 1
*   DLCHAR
   28 CONTINUE
      MLENEL=LENEL
      IF(MLENEL.EQ.-1)MLENEL=1
      GO TO 1
*   DSVIDN
   29 CONTINUE
      NWIDSV=NWIDEN
      NCIDSV=NCIDEN
      CALL CCOPYA(IDEN(1),IDENSV(1),NWIDSV)
      GO TO 1
*   DCHEXP
   30 CONTINUE
      IF(NWIDSV.GT.0)THEN
        CALL CCOPYA(IDENSV,IDEN,NWIDSV)
        NWIDEN=NWIDSV
        NCIDEN=NCIDSV
      ENDIF
      NWIDSV=0
      LENEL=KOD(IPCE-1)
      IPCE=IPCE-2
      ITS=ITS-2
      IF(ISEM(ITS).NE.KTINT .OR. ISEM(ITS+1).NE.2)IGSST=-1
      IF(MTYPE.NE.KTCHAR)IGSST=-1
      GO TO 1
*   DVLN
   31 CONTINUE
      CALL CSDVLN(KEYWD)
      GO TO 1
*   DBLKE
   32 CONTINUE
      IDEN(2)=KBLN
      IF(KEYWD.EQ.1)THEN
        CALL CCOPYS(MJSCHA('$BLANK'),JID,6)
      ELSE
        CALL CCOPYS(MJSCHA('$GLOBL'),JID,6)
      ENDIF
      NCIDEN=6
      NWIDEN=2
      GO TO 809
*   DBLKN
   33 CONTINUE
*            COMMON         GLOBAL
* ICGB =   0    1   2      3    4   5
*         BEG ARTH CHAR   BEG ARTH CHAR
* KGLO     0    1   2      0    1   2
*
 809  IGB=CSLTGB(IPVSB)
      IF(NCIDEN.EQ.4.AND.IDEN(1).EQ.NMPAWC)CALL CSMARK(100)
      IF(IGB.EQ.0)THEN
        IF(KEYWD.EQ.1)THEN
*cb          KGLO=0
#if defined(CERNLIB_VAX)
          ICGB=0
***       SEARCH IN MAP FILE
          IADGB=0
          IF(ITBC.NE.0)IADGB=MLSEAR(ITBC,IDEN)
          IF(IADGB.EQ.0)THEN
            ICGB=3
          ELSE
            IADGB=IQ(IADGB)
          ENDIF
#endif
#if !defined(CERNLIB_VAX)
          IADGB=0
          ICGB=3
#endif
        ELSE
          ICGB=3
*cb          KGLO=0
        ENDIF
        IGB=CSITGB(IPVSB)
*cb      ELSE
*cb        IF(ICGB.GE.3) THEN
*cb          KGLO=ICGB-3
*cb        ELSE
*cb          KGLO=ICGB
*cb        ENDIF
      ENDIF
      NUMGB=IGB+KSADGB
      CALL CSADCB(LISTCB,NUMGB,ISFTG)
      GO TO 1
*   DFORS
   34 CONTINUE
      IF(IFORS.NE.0)IGSST=-1
      GO TO 1
*   LABEL
   35 CONTINUE
 996  K=MKBLAN(JGSST,NGSST)
* TEST NGSST=0
      IF(NGSST.LE.0)THEN
        IF(NGSST.EQ.0)THEN
          CALL CSRD(JGSST,NGSST)
          GO TO 996
        ELSE
          IGSST=-39
          GO TO 1
        ENDIF
      ENDIF
      IF(K.EQ.ICHSSC )THEN
        JGSST=JGSST+1
        NGSST=NGSST-1
        GO TO 996
      ENDIF
      IGSST=MLNUMB(JGSST,NGSST,LAB)
      LABOD=LAB
      IF(IGSST.EQ.0) GO TO 1
      CALL CSSLAB(IPCE,IPCB)
      GO TO 1
*   LABEL1
   36 CONTINUE
 997  K=MKBLAN(JGSST,NGSST)
* TEST NGSST=0
      IF(NGSST.LE.0)THEN
        IF(NGSST.EQ.0)THEN
          CALL CSRD(JGSST,NGSST)
          GO TO 997
        ELSE
          IGSST=-1
          GO TO 1
        ENDIF
      ENDIF
      IGSST=MLNUMB(JGSST,NGSST,LAB)
      GO TO 1
*   DEND
   37 CONTINUE
      IF(IPCE.GE.LAST)GO TO 727
*E                     --------
      IF(NGSST.GT.0)THEN
*Error       something after END ?
        IGSST=-1
        GO TO 1
      ENDIF
*  test undeclared vars
      CALL CSUNDF(IMNONE,IDENPR,IGSST)
**************************************
      CALL CCOPYA(IDENPR,IDEN,NWIDPR)
      NCIDEN=NCIDPR
      NWIDEN=NWIDPR
      I=CSLTLI(IPVS)
      IF(KPRO.EQ.3)THEN
        KOD(IPCE)=KRETM
        IF(KEYWD.EQ.0.AND.IPCE.EQ.IPCB+3)KOD(IPCE)=KEXIT
        ITYPGI=1
        ISHGI=0
      ELSE
        IF(KOD(IPCE-1).EQ.KRET)IPCE=IPCE-1
        KOD(IPCE)=KRET
        ISHGI=NPARFS
      ENDIF
      IPCE=IPCE+1
#if defined(CERNLIB_ALIGN)
      IF(MOD((IPCE-IPCB),2).EQ.1)THEN
        KOD(IPCE)=KRET
        IPCE=IPCE+1
      ENDIF
#endif
      KOD(IPCB)=KENTRY
      KOD(IPCB+1)=IPCE-IPCB
      KOD(IPCB+2)=LSFT
      IF(KSTRST.NE.0) GO TO 728
*                     ----------
*  TEST LABEL
      I=IPLL
 951  IF(I.EQ.0)GO TO 1
      IF(IQ(I+2).EQ.0)THEN
        WRITE(*,952)IQ(I+1)
 952    FORMAT(1X,'Undefined statement label',I5)
        IGSST=-30
      ENDIF
      I=IQ(I)
      GO TO 951
*   DADB
   38 CONTINUE
      CALL CSSEMS(NWIDEN+1)
      ISVSEM=ITS-IBSEM
      ISEM(ITS)=NCIDEN
      CALL CCOPYA(IDEN,ISEM(ITS+1),NWIDEN)
      ITS=ITS+NWIDEN+1
      NDIM=0
      IPCAD=IPCE
      IPCL=IPCE
      KDA=1
      NDVAR=-24
*  NDVAR USED IN AEXPR (#DVARL)
      GO TO 1
*   DADE
   39 CONTINUE
      I=IBSEM+ISVSEM
      NCIDEN=ISEM(I)
      NWIDEN=(NCIDEN+3)/4
      CALL CCOPYA(ISEM(I+1),IDEN,NWIDEN)
      ITS=I
      IF(NDIM.GT.7)GO TO 737
*E: MORE THEN 7 DIMENSIONS--
      DO K=1,NDIM
         INFVEC(K+4)=0
      ENDDO
      IF(IABS(KDA).EQ.1)THEN
        IPCE=IPCAD
        INFVEC(4)=NDIM
        I=IPCAD+1
        ICON=0
        NEL=1
        I1=5
        DO 962 K=1,NDIM
          L=KOD(I)
          M=KOD(I+2)-L+1
          IF(M.LE.0)GO TO 708
*                   --------
          ICON=ICON+NEL*L
          NEL=NEL*M
          INFVEC(I1)=NEL
          I1=I1+1
          I=I+4
 962   CONTINUE
        INFVEC(3)=ICON
      ENDIF
      GO TO 1
*   DADL
   40 CONTINUE
 961  ITS=ITS-2
      IT1=ISEM(ITS)
      IB1=ISEM(ITS+1)
      IF(IT1.NE.KTINT .OR. IB1.GT.3)GO TO 722
*                                     -------
      IF(KDA.EQ.-1)IGSST=-1
      IF(IB1.NE.2)KDA=0
      GO TO 1
*   DADLU
   41 CONTINUE
      IPCL=IPCE
      NDIM=NDIM+1
      GO TO 961
*   DADU
   42 CONTINUE
      L=IPCE-IPCL
      IPCE=IPCE+2
      IF(IPCE.GE.LAST)GO TO 727
*                       -------
      CALL CCOPYA(KOD(IPCL),KOD(IPCL+2),L)
      KOD(IPCL)=KLK
      KOD(IPCL+1)=1
      IPCL=IPCE
      NDIM=NDIM+1
      GO TO 1
*   DADSTA
  212 CONTINUE
*
*- kda=1  array with constant indexes
*- kda=0  array with flexible  indexes
*- kda=-1 array with constant indexes and last dim=*
      IF(KEYWD.EQ.22)THEN
        NDIM=0
      ELSE
        IPCE=IPCE+4
        IF(IPCE.GE.LAST)GO TO 727
*                         -------
        KOD(IPCE-4)=KLK
        KOD(IPCE-3)=1
        KOD(IPCE-2)=KLK
        KOD(IPCE-1)=100000
        NDIM=NDIM+1
        IF(KDA.LT.0)IGSST=-1
        IF(KDA.EQ.1)KDA=-1
        IPCL=IPCE
      ENDIF
      GO TO 1
*   DADSTAL
  213 CONTINUE
      IF(KEYWD.EQ.22)THEN
        NDIM=0
      ELSE
        IPCP=IPCE
        IPCE=IPCE+2
        IF(IPCE.GE.LAST)GO TO 727
*                         -------
        KOD(IPCE-2)=KLK
        KOD(IPCE-1)=100000
        NDIM=NDIM+1
        IPCL=IPCE
        IF(KDA.LT.0)IGSST=-1
        IF(KDA.EQ.1)KDA=-1
        IPCL=IPCE
      ENDIF
      GO TO 1
*   DVARD
   43 CONTINUE
      IF(KEYWD.EQ.3)THEN
         IGSST=-1
         GO TO 1
      ENDIF
      NDIM=0
      GO TO 963
*   DARRD
   44 CONTINUE
 963  ITYPE=MTYPE
      IF(MTYPE.EQ.0)THEN
        ITYPE=CSKIDN(JID,LENEL)
      ENDIF
*---------------------------
*---------------------------
      IG=CSLTGI(IPVSG)
      IF(IG.GT.0)GO TO 701
*                    -----
#if defined(CERNLIB_PAW)
      IF(KEYWD.EQ.22)THEN
        IGSST=CSPAWV(NUMGB,ITYPE,KDA,NDIM,INFVEC,IPCE)
        GO TO 1
      ENDIF
#endif
      IL=CSLTLI(IPVSL)
      IF(IL.NE.0)GO TO 964
* ADD NEW ELEMENT
      ITYPGI=ITYPE
      LENEGI=LENEL
      IF(NDIM.GT.0)THEN
        IF(KDA.LE.0)GO TO 710
*                      ------
        INFVEC(1)=LENEL+KON2
        INFVEC(2)=0
        L=NDIM+4
        MODEGI=MHLOC(L+1)+1
        CALL CCOPYA(INFVEC(1),IQ(MODEGI),L)
      ENDIF
      ISHGI=-1
      NUMGI=0
      NDIM=0
      IL=CSITLI(IPVSL)
      IF(KEYWD.EQ.1 .OR. KEYWD.EQ.2)GO TO 964
      GO TO 1
* CORRECT ELEMENT TLI
 964  CONTINUE
********
*    KEYWD=1 -> COMMON BLOCK
*          2 -> GLOBAL BLOCK
*          3 -> DIMENSION
*          4 -> INTEGER, REAL, DOUBLE, LOGICAL
*          5 -> CHARACTER
*******
      IF(KEYWD.EQ.1 .OR. KEYWD.EQ.2)
     +CALL CSADECB(ISFTG,IL)
      CALL CSCTLI(IPCE,IL,ISFTG,ITYPE,KEYWD)
      GO TO 1
*   DTID
   45 CONTINUE
      IPSEL=CSLTLI(IPVS)
      IF(IPSEL.EQ.0)THEN
        ISHGI=-1
        MODEGI=1
        ITYPGI=IABS(CSKIDN(JID,LENEGI))
        IPSEL=CSITLI(IPVS)
      ENDIF
      IF(NUMGI.LT.0 .OR. MODEGI.LT.0)GO TO 717
* ERR:       NOT VAR OR ARR            -------
      ITYPE=IABS(ITYPGI)
      LENEL=LENEGI
      IF(MODEGI.GT.1)THEN
        N=IQ(MODEGI+KSDIM)
        CALL CCOPYA(IQ(MODEGI),INFVEC(1),N+4)
        IARR=1
        IVAR=0
      ELSE
        IARR=0
        IVAR=1
* VAR USED IN DATA OR EQU
        IF(MODEGI.EQ.0)THEN
          MODEGI=1
          CALL CSRTLI(IPSEL)
        ENDIF
      ENDIF
      GO TO 1
*   TVAR
   46 CONTINUE
      IOFS=0
      IGSST=IVAR
      GO TO 1
*   TARR
   47 CONTINUE
      IOFS=0
      NDIM=0
      IGSST=IARR
      GO TO 1
*   DSSNO
   48 CONTINUE
      ICHA=1
      ICHB=LENEL
      GO TO 1
*   DCIS1
   49 CONTINUE
      ICHA=1
      GO TO 1
*   DCISL
   50 CONTINUE
      ICHB=LENEL
      GO TO 1
*   DCISA
   51 CONTINUE
      ICHA=KOD(IPCE-1)
      GO TO 971
*   DCISB
   52 CONTINUE
      ICHB=KOD(IPCE-1)
      GO TO 971
*   DCIND
   53 CONTINUE
      NDIM=NDIM+1
      INDX(NDIM)=KOD(IPCE-1)
 971  IPCE=IPCE-2
      ITS=ITS-2
      IF(ISEM(ITS+1).NE.2 .OR. ISEM(ITS).NE.KTINT)GOTO 725
*   ERR:  NOT INTEGER CONSTANT EXPR.             --------
      GO TO 1
*   DINDE
   54 CONTINUE
      IF(INFVEC(4).NE.NDIM)GO TO 723
*                           -------
      IOFS=INDX(1)-INFVEC(3)
      K=5
      J=2
      N=NDIM
 972  N=N-1
      IF(N.EQ.0)GO TO 973
      IOFS=IOFS+INDX(J)*INFVEC(K)
      K=K+1
      J=J+1
      GO TO 972
 973  IF(IOFS.LT.0 .OR. IOFS.GE.INFVEC(K))GO TO 731
*  ERR: ARR. SUBSCRIPT OUT OF RANGE       ---------
      IF(LENEL.GT.0)IOFS=IOFS*LENEL
      GO TO 1
*   DBEQU
   55 CONTINUE
      IPEL=0
      IBOFS=0
      GO TO 1
*   DEQU
   56 CONTINUE
      IF(ITYPE.EQ.KTCHAR)THEN
        GO TO 732
*  NO EQU CHAR TO CHAR
      ELSE
        IZOFS=IBOFS-IOFS
      ENDIF
      IF(IQ(IPSEL+KSEQU).EQ.0)THEN
* ADD ELEMENT IN EQU CHAIN
        IF(IPEL.EQ.0)THEN
          IPEL=IPSEL
        ELSE
          IQ(IPSEL+KSEQU)=IQ(IPEL+KSEQU)
        ENDIF
        IQ(IPEL+KSEQU)=IPSEL
        IQ(IPSEL+KSOFFS)=IZOFS
      ELSE
*RECOMPUTE OFFSET IN NEW EQU.CHAIN
        IDIFF=IZOFS-IQ(IPSEL+KSOFFS)
        IBOFS=IBOFS-IDIFF
        IQEL=IPEL
        IF(IPEL.EQ.0)THEN
           IPEL=IPSEL
           GO TO 1
* END OF PROCESSING
        ENDIF
 974    IF(IQEL.EQ.IPSEL)THEN
          IF(IDIFF.NE.0)GO TO 706
* ERR. IN EQU              -------
          GO TO 1
        ENDIF
        IQ(IQEL+KSOFFS)=IQ(IQEL+KSOFFS)-IDIFF
        IQ1EL=IQEL
        IQEL=IQ(IQEL+KSEQU)
        IF(IQEL.NE.IPEL)GO TO 974
        IQ(IQ1EL+KSEQU)=IQ(IPSEL+KSEQU)
        IQ(IPSEL+KSEQU)=IPEL
      ENDIF
      GO TO 1
*   DBDAT
   57 CONTINUE
      IF(LDATA.EQ.0)THEN
        I=MHLOC(2)
        LDATA=I
        IQ(I)=0
        CALL CSMARK(300)
      ELSE
        I=LDATA
 975    IF(IQ(I).NE.0)THEN
          I=IQ(I)
          GO TO 975
        ENDIF
      ENDIF
      LDATT=I
      LDATF=I
      NVDAT=0
      NELT=0
      GO TO 1
*   DDATN
   58 CONTINUE
      NEL=1
      IF(IARR.EQ.1.AND.NDIM.EQ.0)THEN
        N=INFVEC(4)
        NEL=INFVEC(N+4)
      ENDIF
      LENT=NEL*LENEL
      IF(ITYPE.EQ.KTCHAR)THEN
        IF(LENT.EQ.0)LENT=4
        L=LENT
        LENT=(LENT-1)/NBYTPW+1
      ENDIF
      NVDAT=NVDAT+NEL
      I=MHLOC(LENT+4)
      IQ(I+1)=IOFS
      IQ(I+2)=NEL
      IQ(I+3)=IPSEL
      IF(ITYPE.EQ.KTCHAR)CALL CSSETC(MJCHAR(IQ(I+4)),L,ICHBLN)
      IQ(I)=0
      IQ(LDATT)=I
      LDATT=I
      GO TO 1
*   DEDAT
   59 CONTINUE
      IF(NVDAT.EQ.0)GO TO 1
      IF(NVDAT.LT.0)GO TO 734
      GO TO 733
*   DDATV
   60 CONTINUE
      ITS=ITS-2
      IT2=ISEM(ITS)
      IB2=ISEM(ITS+1)
      IF(IB2.NE.2)GO TO 710
*                   -------
      IPV=IPCP+1
      IF(IT2.EQ.6)IPV=IPV+1
 976  IF(NELT.EQ.0)THEN
        LDP=LDATF
        LDATF=IQ(LDATF)
        IF(LDATF.EQ.0)GO TO 734
*                       -------
        I=LDATF
        NELT=IQ(I+2)
        CALL CSLDLI(IQ(I+3))
        IT1=IABS(ITYPGI)
        IT1DAT=IT1
        LENEL=LENEGI
        IF(IT1.NE.KTCHAR)THEN
          IPOS=I+4
        ELSE
          LENEL=KOD(IPV)
          IF(IT2.NE.KTCHAR)GO TO 726
*                          ---------
          IF(LENEGI.EQ.0)THEN
            LENEGI= LENEL
            CALL CSRTLI(IQ(I+3))
            L=LENEGI*NELT
            LENT=(L-1)/NBYTPW+1
            I1=MHLOC(LENT+4)
            CALL CSSETC(MJCHAR(IQ(I1+4)),L,ICHBLN)
            CALL CCOPYA(IQ(I),IQ(I1),4)
            CALL MHFREE(I)
            I=I1
            IQ(LDP)=I
          ENDIF
          LENEL=MIN0(LENEL,LENEGI)
          IPOS=MJCHAR(IQ(I+4))
          JPOS=MJCHAR(KOD(IPV+1))
        ENDIF
      ELSE
        IT1=IT1DAT
      ENDIF
*****      ASSIGN 980 TO LAB1
*****              GO TO 1600
* T1=T2 --> T1=T1
      KEY=KT1T2(IT1,IT2)
      IF(KEY.EQ.0)GO TO 726
*                   -------
      IF(IB2.GT.5)GO TO 726
*                  --------
      CALL CS1600(IPCE)
      IF(IGSST.LT.0)GO TO 1
*****
 980  N=MIN0(NREP,NELT)
      IF(KEY.NE.KTCHAR)THEN
        DO 981 I=1,N
        CALL CCOPYA(KOD(IPV),IQ(IPOS),LENEGI)
 981    IPOS=IPOS+LENEGI
      ELSE
        LENEL=KOD(IPV)
        LENEL=MIN0(LENEL,LENEGI)
        JPOS=MJCHAR(KOD(IPV+1))
        DO 982 I=1,N
        CALL CCOPYS(JPOS,IPOS,LENEL)
 982    IPOS=IPOS+LENEGI
      ENDIF
      NREP=NREP-N
      NELT=NELT-N
      NVDAT=NVDAT-N
      IF(NREP.GT.0)GO TO 976
      IPCE=IPCP
      GO TO 1
*   DNREP
   61 CONTINUE
      NREP=INUM
      GO TO 1
*   D1REP
   62 CONTINUE
      NREP=1
      GO TO 1
*   DPREP
   63 CONTINUE
      IL=CSLTLI(IPVS)
      IF(IL.EQ.0)GO TO 724
*                   ------
      IF(MODEGI.NE.-1)GO TO 735
*                        ------
      IF(ITYPGI.NE.KTINT)GO TO 722
*                         --------
      NREP=IVPAR
      GO TO 1
*   PNAME
   64 CONTINUE
      IL=CSLTLI(IPVS)
      IF(IL.EQ.0)THEN
        ITYPGI=IABS(CSKIDN(JID,LENEGI))
        INDPAR=CSITLI(IPVS)
      ELSE
        IF(MODEGI.NE.0)GO TO 701
        IF(NUMGI.NE.0)GO TO 701
*                      --------
        INDPAR=IL
      ENDIF
      IPCPV=IPCE
      GO TO 1
*   PVAL
   65 CONTINUE
      CALL CSLDLI(INDPAR)
      IT1=IABS(ITYPGI)
      ITS=ITS-2
      IT2=ISEM(ITS)
      IB2=ISEM(ITS+1)
      IF(IB2.NE.2)GO TO 710
*                    ------
****      ASSIGN 1010 TO LAB1
****          GO TO 1600
* T1=T2 --> T1=T1
      KEY=KT1T2(IT1,IT2)
      IF(KEY.EQ.0)GO TO 726
*                   -------
      IF(IB2.GT.5)GO TO 726
*                  --------
      CALL CS1600(IPCE)
      IF(IGSST.LT.0)GO TO 1
****
 1010 I=IPCPV+1
      GO TO (1011,1012,1013,1011,1015,1016,10017),KEY
*             I    R    C   L     D    H
 1011 IVPAR=KOD(I)
      GO TO 1017
 1012 CALL CCOPYA(KOD(I),RVPAR,KLREAL)
      GO TO 1017
 1013 IF(LENEGI.EQ.0)LENEGI=KOD(I)
      LENEL=KOD(I)
      N=MIN0(LENEGI,LENEL)
      L=(LENEGI-1)/NBYTPW+1
      IVPAR=MHLOC(L)
      J=MJCHAR(IQ(IVPAR))
      CALL CSSETC(J,LENEGI,ICHBLN)
      CALL CCOPYS(MJCHAR(KOD(I+1)),J,N)
      GO TO 1017
 1015 CALL CCOPYA(KOD(I),DVPAR,KDLEN)
      GO TO 1017
 1016 IVPAR=KOD(I+1)
      GO TO 1017
10017 CALL CCOPYA(KOD(I),CXVPAR,KLCMLX)
 1017 ISHGI=1
      MODEGI=-1
      ITYPGI=IT1
      CALL CSRTLI(INDPAR)
      IPCE=IPCPV
      GO TO 1
*   IMPNONE
   66 CONTINUE
      IMNONE=1
      GO TO 1
*   LETIMP
   67 CONTINUE
      IGSST=-1
      IF(IMNONE.EQ.1)GO TO 1
      CALL CSIMPL(IPLI,MTYPE,MLENEL)
      GO TO 1
*   DOR
   68 CONTINUE
* T;B
* T -->  INT   REAL   CHAR   LOG   DOUBLE  HOLL COMPLEX
*         1     2       3    4      5       6     7
* B -->  EXP  CONST  VAR  EL. ARR  F(X)  NMA  NMF  BLOCK
*         1     2     3      4       5    6    7     8
*
      KI=KIOR
      GO TO 1400
*   DAND
   69 CONTINUE
      KI=KIAND
      GO TO 1400
*   DNOT
   70 CONTINUE
      IT1=ISEM(ITS-2)
      IB1=ISEM(ITS-1)
      IF(IB1.GT.5)GO TO 726
*             -----
      IF(IT1.EQ.3.OR.IT1.GE.5)GO TO 726
*        ----------
      IB1=1
      KEY=IT1
      IT1=1
      KI=KINOT
      GO TO 1410
*   DEQV
   71 CONTINUE
      KI=KIEQ
      GO TO 1400
*   DNEQV
   72 CONTINUE
      KI=KINE
      GO TO 1400
*   DUP
   73 CONTINUE
      IB1=ISEM(ITS-1)
      IF(ISEM(ITS-2).EQ.3.OR.IB1.GE.6)GO TO 726
*                                    -----------
      IF(IB1.NE.2)ISEM(ITS-1)=1
      GO TO 1
*   DUM
   74 CONTINUE
      IT1=ISEM(ITS-2)
      IB1=ISEM(ITS-1)
      IF(IB1.GT.5)GO TO 726
*        --------
      IF(IT1.EQ.3)GO TO 726
*        -------
      IF(IB1.EQ.2.AND.IT1.NE.6)THEN
        I=IPCP+1
        IF(IT1.EQ.1.OR.IT1.EQ.4)THEN
          KOD(I)=-KOD(I)
        ELSEIF(IT1.EQ.2)THEN
          RA(I)=-RA(I)
        ELSEIF(IT1.EQ.5)THEN
          D=-D
          CALL CCOPYA(D,KOD(I),KDLEN)
        ELSEIF(IT1.EQ.7)THEN
          CX=-CX
          CALL CCOPYA(CX,KOD(I),KLCMLX)
        ENDIF
        GO TO 1
      ENDIF
      IB1=1
      KEY=KT1T2(IT1,IT1)
      KI=KIUM
      KDP=KDUM
      KCXP=KCXUM
      GO TO 1410
*   DPLUS
   75 CONTINUE
      KI=KIPLUS
      KDP=KDPLUS
      KCXP=KCXPLS
      GO TO 1141
*   DMINUS
   76 CONTINUE
      KI=KIMINU
      KDP=KDMINU
      KCXP=KCXMNU
      GO TO 1141
*   DMULT
   77 CONTINUE
      KI=KIMULT
      KDP=KDMULT
      KCXP=KCXMLT
      GO TO 1141
*   DDIV
   78 CONTINUE
* T1 OP T2 --> T OP T
      KI=KIDIV
      KDP=KDDIV
      KCXP=KCXDIV
 1141 CALL CS1200(IPCE,KT1T2)
      IF(IGSST.LT.0)GO TO 1
 1140 IF(KEY.EQ.3)GO TO 726
*                ----------
      IF(IBEXP.EQ.2)THEN
        CALL CSCATH( I )
        IPCE=IPCP
        IPCP=I-1
        GO TO 1
      ENDIF
      IT1=KEY
      IB1=1
      GO TO 1410
*   DEQ
   79 CONTINUE
      KI=KIEQ
      KDP=KDEQ
      KCXP=KCXEQ
      GO TO 1500
*   DNE
   80 CONTINUE
      KI=KINE
      KDP=KDNE
      KCXP=KCXNE
      GO TO 1500
*   DLT
   81 CONTINUE
      KI=KILT
      KDP=KDLT
      KCXP=-1
      GO TO 1500
*   DLE
   82 CONTINUE
      KI=KILE
      KDP=KDLE
      KCXP=-1
      GO TO 1500
*   DGE
   83 CONTINUE
      KI=KIGE
      KDP=KDGE
      KCXP=-1
      GO TO 1500
*   DGT
   84 CONTINUE
      KI=KIGT
      KDP=KDGT
      KCXP=-1
      GO TO 1500
*   DCONC
   85 CONTINUE
* T1 OP T2 --> T OP T
      CALL CS1200(IPCE,KT1T2)
      IF(IGSST.LT.0)GO TO 1
 1150 IF(IB1.GE.5.OR.IB2.GE.5)GO TO 726
*                             ---------
* NO CH--FUN
      IF(KEY.NE.3)GO TO 726
*                 ---------
      KI=KCONC-2
      IB1=1
      GO TO 1410
*   DNODIV
   86 CONTINUE
      JGSST=JGSST-2
      NGSST=NGSST+2
      GO TO 1
*   DPOW
   87 CONTINUE
      CALL CSDPOW(IPCE)
      GO TO 1
* OP: OR  AND
 1400 CALL CS1200(IPCE,KT1T2)
      IF(IGSST.LT.0)GO TO 1
 1401 IF(KEY.EQ.KTCHAR)GO TO 726
      IF(KEY.EQ.KTDOU) GO TO 726
      IF(KEY.EQ.KTCMLX)GO TO 726
*        --------
      IB1=1
      IT1=1
* ADD COMMAND
 1410 IPCE=IPCE+1
      IF(IPCE.GT.LAST)GO TO 727
*           -------
      KODT=KI
      IF(KEY.EQ.2)KODT=KI+1
      IF(KEY.EQ.3)KODT=KI+2
      IF(KEY.EQ.5)KODT=KDP
      IF(KEY.EQ.7)KODT=KCXP
      KOD(IPCE-1)=KODT
* SET T;B
 1420 ISEM(ITS-2)=IT1
      ISEM(ITS-1)=IB1
      GO TO 1
* OP: EQ;NE; .... ;GT
 1500 CALL CS1200(IPCE,KT1T2)
      IF(IGSST.LT.0)GO TO 1
      IF(KEY.EQ.7 .AND. KCXP.EQ.-1)THEN
        KCXP=0
        GO TO 726
      END IF
*****************************************
      IF(IB2.EQ.2)THEN
        IF(KI.EQ.KIEQ)THEN
          IF(KEY.EQ.1)THEN
            IF(KOD(IPCP+1).EQ.0)THEN
*         **  I.EQ.0  **
              KI=KINOT
              IPCE=IPCP
            ENDIF
          ELSEIF(KEY.EQ.2)THEN
            IF(RA(IPCP+1).EQ.0.)THEN
*         **  R.EQ.0.  ** or ** I.EQ.0. **
              KI=KINOT
              IF(IT1.EQ.1)KEY=1
              IPCE=IPCP
            ENDIF
          ENDIF
        ELSEIF(KI.EQ.KINE)THEN
          IF(KEY.EQ.1)THEN
            IF(KOD(IPCP+1).EQ.0)THEN
*         **  I.NE.0  **
              IPCE=IPCP
              ISEM(ITS-2)=1
              ISEM(ITS-1)=1
              GO TO 1
            ENDIF
          ENDIF
        ENDIF
      ENDIF
*****************************************
 1501 IB1=1
      IT1=1
      GO TO 1410
*   DPARCON
   88 CONTINUE
      IGSST=0
      IF(IL.EQ.0)GO TO 1
      IF(MODEGI.NE.-1)GO TO 1
      IGSST=1
      GO TO (1651,1652,1653,1651,1655,726,1657),ITYPGI
 1651 INUM=IVPAR
      KNUMB=KTINT
      GO TO 1701
 1652 CALL CCOPYA(RVPAR,INUM,1)
      KNUMB=KTREAL
      GO TO 1701
 1653 L=(LENEGI-1)/NBYTPW+1
      IH=MHLOC(L)
      CALL CCOPYA(IQ(IVPAR),IQ(IH),L)
      NSST=LENEGI
      GO TO 1702
 1655 D=DVPAR
      KNUMB=KTDOU
      GO TO 1701
 1657 CX=CXVPAR
      KNUMB=KTCMLX
      GO TO 1701
*   DTCON
   89 CONTINUE
      INUM=1
      KNUMB=KTLOG
      GO TO 1701
*   DFCON
   90 CONTINUE
      INUM=0
      KNUMB=KTLOG
      GO TO 1701
*   DACON
   91 CONTINUE
      IF(KNUMB.EQ.1 .AND. NGSST.GT.0)THEN
        J=JTHEN+1
        IF(MLEQS(JGSST,J,1).EQ.1)GO TO 1703
      ENDIF
 1701 CALL CSSEMS(2)
      ISEM(ITS)=KNUMB
      ISEM(ITS+1)=2
      ITS=ITS+2
      IF(IPCE+4.GE.LAST)GO TO 727
*                       ---------
      IPCP=IPCE
      IF(KNUMB.EQ.KTDOU)THEN
        KOD(IPCE)=KLDK
        CALL CCOPYA(D,KOD(IPCE+1),KDLEN)
        IPCE=IPCE+1+KDLEN
      ELSEIF(KNUMB.EQ.KTCMLX)THEN
        KOD(IPCE)=KLCXK
        CALL CCOPYA(CX,KOD(IPCE+1),KLCMLX)
        IPCE=IPCE+1+KLCMLX
      ELSE
        KOD(IPCE)=KLK
        CALL CCOPYA(INUM,KOD(IPCE+1),1)
        IPCE=IPCE+2
      ENDIF
      GO TO 1
 1703 JGSST=JGSST+1
      NGSST=NGSST-1
      IF(INUM.GT.NGSST)GO TO 736
* ERR: TOO LONG HOLL       -----
      I=(INUM-1)/NBYTPW+1
      L=I+2
      IF(IPCE+L.GT.LAST)GO TO 727
*                         -------
      CALL CSSEMS(2)
      ISEM(ITS)=6
      ISEM(ITS+1)=2
      ITS=ITS+2
      IPCP=IPCE
      KOD(IPCE)=KLHK
      KOD(IPCE+1)=I
      J=MJCHAR(KOD(IPCE+2))
      CALL CSSETC(J,I*NBYTPW,ICHBLN)
      CALL CCOPYS(JGSST,J,INUM)
      IPCE=IPCE+L
      JGSST=JGSST+INUM
      NGSST=NGSST-INUM
      GO TO 1
*   DCCON
   92 CONTINUE
      IH=CSICNS(JGSST,NGSST,NSST)
      IF(IH.EQ.0) GO TO 729
*                 ---------
 1702 JSST=MJCHAR(IQ(IH))
      CALL CSSEMS(2)
      ISEM(ITS)=3
      ISEM(ITS+1)=2
      ITS=ITS+2
      I=(NSST-1)/NBYTPW+1
      L=I+2
      IF(IPCE+L.GE.LAST) GO TO 727
*                        ---------
      KOD(IPCE)=KLCK
      KOD(IPCE+1)=NSST
      CALL CCOPYS(JSST,MJCHAR(KOD(IPCE+2)),NSST)
      IPCP=IPCE
      IPCE=IPCE+L
      CALL MHFREE(IH)
      GO TO 1
*   RINUMB
   93 CONTINUE
      K=MKBLAN(JGSST,NGSST)
* TEST NGSST=0
      IF(NGSST.EQ.0)CALL CSRD(JGSST,NGSST)
      KNUMB=CSNUMB(JGSST,NGSST,INUM,RNUM,D)
      IGSST=KNUMB
      IF(KNUMB.EQ.2)CALL CCOPYA(RNUM,INUM,1)
      GO TO 1
*   CXNUMB
   94 CONTINUE
      K=MKBLAN(JGSST,NGSST)
* TEST NGSST=0
      IF(NGSST.EQ.0)CALL CSRD(JGSST,NGSST)
      KNUMB=CSCXNU(JGSST,NGSST,CX)
      IGSST=KNUMB
      GO TO 1
*   DIDEN
   95 CONTINUE
      IDES=0
      IL=CSLTLI(IPVSL)
      IF(IL.GT.0)THEN
        IDES=IL
        MODE=MODEGI
        IF(MODE.EQ.0)THEN
          MODEGI=1
          CALL CSRTLI(IL)
        ENDIF
        IF(EXSTAT .AND. .NOT. BTEST(LXXGLI,KUSEB))THEN
          LXXGLI=IBSET(LXXGLI,KUSEB)
          CALL CSRTLI(IL)
        ENDIF
        ITYPGI=IABS(ITYPGI)
        GO TO 1
      ENDIF
      IG=CSLTGI(IPVSG)
      ITYPGI=IABS(ITYPGI)
      IDES=IG
      GO TO 1
*   VAR
   96 CONTINUE
      IGSST=0
      IF(IDES.EQ.0) GO TO 1
      IF(MODEGI.LT.0.OR.MODEGI.GT.1) GO TO 1
      IF(ITYPGI.EQ.KTCHAR)GO TO 1
      IGSST=1
      CALL CSSEMS(2)
      ISEM(ITS)=IABS(ITYPGI)
      ISEM(ITS+1)=3
      ITS=ITS+2
      IPCP=IPCE
      IF(IPCE+3.GE.LAST)GO TO 727
*                       ---------
      IF(NUMGI)2008,2009,2010
 2008 IF(NUMGI.EQ.-1)THEN
          KOD(IPCE)=KLVP
          IF(ITYPGI.EQ.5)KOD(IPCE)=KLVDP
          IF(ITYPGI.EQ.7)KOD(IPCE)=KLVCXP
      ELSEIF(NUMGI.EQ.-2)THEN
         IGSST=NDVAR
         KOD(IPCE)=KLVF
         IF(ITYPGI.EQ.5)KOD(IPCE)=KLVDF
         IF(ITYPGI.EQ.7)KOD(IPCE)=KLVCXF
      ENDIF
      GO TO 2011
 2009 KOD(IPCE)=KLVL
      IF(ITYPGI.EQ.5)KOD(IPCE)=KLVDL
      IF(ITYPGI.EQ.7)KOD(IPCE)=KLVCXL
      GO TO 2011
 2010 KOD(IPCE)=KLVG
      IF(ITYPGI.EQ.5)KOD(IPCE)=KLVDG
      IF(ITYPGI.EQ.7)KOD(IPCE)=KLVCXG
      IPCE=IPCE+1
      KOD(IPCE)=NUMGI
 2011 KOD(IPCE+1)=ISHGI
      IPCE=IPCE+2
      GO TO 1
*   ARR
   97 CONTINUE
      IGSST=0
      IF(IDES.EQ.0) GO TO 1
      IF(MODEGI.LE.1) GO TO 1
      IF(ITYPGI.EQ.KTCHAR)GO TO 1
      IGSST=1
*+++++++++++++++++++++++++++++++++
#if defined(CERNLIB_PAW)
      IF(NUMGI.GT.0)THEN
        IF(IQ(NUMGI).EQ.JKUVBS)CALL CSMARK(1)
      ENDIF
#endif
*+++++++++++++++++++++++++++++++++
      CALL CSSEMS(4)
      ISEM(ITS)=IDES
      ISEM(ITS+1)=0
      ISEM(ITS+2)=IABS(ITYPGI)
      ISEM(ITS+3)=6
      ITS=ITS+4
      GO TO 1
*   CVAR
   98 CONTINUE
      IGSST=0
      IF(IDES.EQ.0) GO TO 1
      IF(ITYPGI.NE.3.OR.MODEGI.LT.0.OR.MODEGI.GT.1) GO TO 1
      IGSST=1
      CALL CSSEMS(2)
      ISEM(ITS)=3
      ISEM(ITS+1)=3
      ITS=ITS+2
      GO TO 1
*   CARR
   99 CONTINUE
      IGSST=0
      IF(IDES.EQ.0)GO TO 1
      IF(ITYPGI.NE.3) GO TO 1
      IF(MODEGI.LE.1)GO TO 1
      IGSST=1
      CALL CSSEMS(4)
      ISEM(ITS)=IDES
      ISEM(ITS+1)=0
      ISEM(ITS+2)=3
      ISEM(ITS+3)=6
      ITS=ITS+4
      GO TO 1
*   DBA
  100 CONTINUE
      ISEM(ITS-1)=-4
      IF(ISEM(ITS-2).EQ.KTCHAR .OR. NUMGI.EQ.-1)ISEM(ITS-1)=4
      GO TO 1
*   DNP
  101 CONTINUE
      ISEM(ITS-1)=5
      GO TO 1
*   DIND
  102 CONTINUE
      IT1=ISEM(ITS-2)
      IF(IT1.GE.3.OR.ISEM(ITS-1).GT.5)GO TO 722
*                                      --------
      IF(ISEM(ITS-1).NE.2)ISEM(ITS-3)=4
      IF(IT1.EQ.2)THEN
        IF(IPCE+1.GE.LAST)GO TO 727
*                        ----------
        KOD(IPCE)=KRI
        IPCE=IPCE+1
        ISEM(ITS-3)=4
      ENDIF
      ITS=ITS-2
      ISEM(ITS-3)=ISEM(ITS-3)+1
      GO TO 1
*   DEIND
  103 CONTINUE
      IDES=ISEM(ITS-4)
      NDIM=ISEM(ITS-3)
      CALL CSLDLI(IDES)
      ITYPGI=IABS(ITYPGI)
      NDIMGI=IQ(MODEGI+3)
      IPCP=IPCE
      IF(NDIM.NE.NDIMGI)GO TO 723
*                      ----------
      IF(ITYPGI.NE.3)THEN
        IF(IPCE+4.GE.LAST)GO TO 727
*                         ---------
        IF(NUMGI.GT.0)THEN
          IF(IQ(NUMGI).EQ.JKUVBS)THEN
            CALL CSGTIDL(IDES,KUVNAME,NCKUV)
            IGSST=CSKUVI(KUVNAME)
            KOD(IPCE)=KLEKV
            IPCE=IPCE+1
            KOD(IPCE)=IGSST
            ISEM(ITS-1)=4
          ELSE
            KOD(IPCE)=KLEAL
            IF(ITYPGI.EQ.5)KOD(IPCE)=KLEDAL
            IF(ITYPGI.EQ.7)KOD(IPCE)=KECXAL
          ENDIF
          IF(IQ(NUMGI+1).GE.3)ISEM(ITS-1)=4
        ELSE
          KOD(IPCE)=KLEAL
          IF(ITYPGI.EQ.5)KOD(IPCE)=KLEDAL
          IF(ITYPGI.EQ.7)KOD(IPCE)=KECXAL
        ENDIF
        IF(ISEM(ITS-1).EQ.-4)THEN
          IPCP=IPCE-NDIM*2
          IOFFST=KOD(IPCP+1)-IQ(MODEGI+2)
          IF(NDIM.GT.1)THEN
            K=IPCP+3
            J=MODEGI+4
            N=NDIM
 1091       N=N-1
            IF(N.EQ.0)GO TO 1092
            IOFFST=IOFFST+KOD(K)*IQ(J)
            K=K+2
            J=J+1
            GO TO 1091
 1092       CONTINUE
          ENDIF
          KOD(IPCP)=KLVKA
          KOD(IPCP+1)=MODEGI
          KOD(IPCP+2)=IOFFST*LENEGI
          IPCE=IPCP+3
        ELSE
          KOD(IPCE+1)=MODEGI
          KOD(IPCE+2)=NDIM
          IPCE=IPCE+3
        ENDIF
      ENDIF
***     CALL CCOPYA(ISEM(ITS-2),ISEM(ITS-4),2)
      ITS=ITS-2
      ISEM(ITS-2)=ISEM(ITS)
      ISEM(ITS-1)=ISEM(ITS+1)
      IF(ISEM(ITS-1).EQ.-4)THEN
        ISEM(ITS-1)=4
        IKEA=1
      ELSE
        IKEA=0
      ENDIF
      GO TO 1
*   EXT
  119 CONTINUE
      IGSST=0
      IF(IDES.EQ.0)GO TO 1
      IF(MODEGI.EQ.-2)THEN
        IADEXT=ISHGI
        IGSST=1
        IF(NUMGI.EQ.-1)IADEXT=-IADEXT
      ELSE
        GO TO 1
      ENDIF
 2023 CONTINUE
      CALL CSSEMS(4)
      ISEM(ITS)=IADEXT
      ISEM(ITS+1)=0
      ISEM(ITS+2)=IABS(ITYPGI)
      ISEM(ITS+3)=7
      ITS=ITS+4
      GO TO 1
*   DINP
  120 CONTINUE
      CALL CSMARK(600)
      IOST=1
      K=10
      GO TO 2222
*   DTYP
  121 CONTINUE
      CALL CSMARK(500)
      IOST=2
      K=11
 2222 CALL CSSEMS(4)
      ISEM(ITS)=0
      ISEM(ITS+1)=0
      ISEM(ITS+2)=0
      ISEM(ITS+3)=K
      ITS=ITS+4
      GO TO 1
*   DVARLA
  122 CONTINUE
      JGSST=JGSST-1
      NGSST=NGSST+1
      GO TO 2012
*   DVARL
  123 CONTINUE
 2012 IF(IDES.NE.0)GO TO 709
*                 ----------
      IGSST=NDVAR
      ISHGI=LSFT
      NUMGI=0
      ITYPGI=CSKIDN(JID,LENEGI)
      IF(IABS(ITYPGI).EQ.KTCHAR)THEN
        LSFT=LSFT+(LENEGI-1)/NBYTPW+1
#if defined(CERNLIB_ALIGN)
      ELSEIF(IABS(ITYPGI).EQ.KTDOU)THEN
        IF(MOD(LSFT,2).EQ.1)THEN
          LSFT=LSFT+1
          ISHGI=LSFT
        ENDIF
        LSFT=LSFT+LENEGI
#endif
      ELSE
        LSFT=LSFT+LENEGI
      ENDIF
      MODEGI=1
      LXXGLI=IBSET(LXXGLI,KUSEB)
      IL=CSITLI(IPVSL)
      ITYPGI=IABS(ITYPGI)
      IDES=IL
      GO TO 1
*   DST1
  124 CONTINUE
      INUM=1
 2013 CONTINUE
      IF(IPCE+2.GE.LAST)GO TO 727
*                      ----------
      KOD(IPCE)=KLK
      KOD(IPCE+1)=INUM
      IPCE=IPCE+2
      GO TO 1
*   DSTAB
  125 CONTINUE
      CALL CSSEMS(1)
      ISEM(ITS)=IDES
      ITS=ITS+1
      GO TO 1
*   DSTAE
  127 CONTINUE
      ITS=ITS-2
      IT1=ISEM(ITS)
      IB1=ISEM(ITS+1)
      IF(IT1.NE.KTINT.OR.IB1.GE.5)GO TO 722
*                                 ---------
      ITS=ITS-1
      IDES=ISEM(ITS)
      GO TO 1
*   DSTEN
  126 CONTINUE
      CALL CSLDLI(IDES)
      IF(IPCE+4.GE.LAST)GO TO 727
      KOD(IPCE)=KLK
      KOD(IPCE+1)=1
      KOD(IPCE+2)=KLK
      KOD(IPCE+3)=LENEGI
      IPCE=IPCE+4
      IKCHSS=1
      GO TO 2014
*   DSTE
  128 CONTINUE
      IKCHSS=0
      CALL CSLDLI(IDES)
 2014 IF(IPCE+3.GE.LAST)GO TO 727
*                       ---------
      IPCP=IPCE
      IF(MODEGI.LT.2)THEN
        LENCV=LENEGI
        IF(NUMGI)2015,2016,2017
 2015   KOD(IPCE)=KLVCP
        IKCHSS=0
        GO TO 2018
 2016   IF(IKCHSS.EQ.1)THEN
          LENCV=-LENEGI
          IPCE=IPCE-4
          IPCP=IPCE
        ENDIF
        KOD(IPCE)=KLVCL
        GO TO 2018
 2017   IF(IKCHSS.EQ.1)THEN
          LENCV=-LENEGI
          IPCE=IPCE-4
          IPCP=IPCE
        ENDIF
        KOD(IPCE)=KLVCG
        IPCE=IPCE+1
        KOD(IPCE)=NUMGI
 2018   KOD(IPCE+1)=LENCV
        KOD(IPCE+2)=ISHGI
      ELSE
        IKCHSS=0
        KOD(IPCE)=KLECA
        KOD(IPCE+1)=MODEGI
        KOD(IPCE+2)=NDIM
      ENDIF
      IPCE=IPCE+3
      GO TO 1
*   DSTL
  129 CONTINUE
      CALL CSLDLI(IDES)
      INUM=LENEGI
      GO TO 2013
*   DFUN1
  130 CONTINUE
 2037 IF(NUMGI.EQ.-2)THEN
         IGSST=-1
      ELSEIF(NUMGI.EQ.-1)THEN
        IF(MODE.NE.0)IGSST=-1
        MODEGI=-2
        IADEXT=-ISHGI
      ELSEIF(NUMGI.EQ.0)THEN
        IF(MODE.NE.0)IGSST=-1
        IADEXT=CSLTGP(IPVSP)
        IF(IABS(ITYPGI).EQ.KTCHAR)GO TO 711
        IF(IADEXT.EQ.0)THEN
          ITYPGP=ITYPGI
          IADEXT=CSITGP(IPVSP)
        ELSE
          IF(ITYPGP.LT.0.AND.ITYPGI.GT.0)ITYPGP=ITYPGI
          CALL CSRTGP(IADEXT)
        ENDIF
        ISHGI=IADEXT
        MODEGI=-2
      ELSEIF(NUMGI.GT.0)THEN
        IGSST=-1
      ENDIF
      IF(IGSST.GT.0)CALL CSRTLI(IL)
      ITS=ITS-2
      IPCE=IPCP
      IDES=IL
*     go to ext
      GO TO 2023
*   DFUN2
  131 CONTINUE
      IF(IDES.NE.0)THEN
         IGSST=-1
         GO TO 1
      ENDIF
      NIFUN=CSNIFN(JID,NPARIF,ITFUN)
      IF(NIFUN.EQ.0)GO TO 2223
      CALL CSSEMS(4)
      ISEM(ITS)=NIFUN
      ISEM(ITS+1)=0
      ISEM(ITS+2)=ITFUN
      ISEM(ITS+3)=-NPARIF
      ITS=ITS+4
      GO TO 1
*   DFUN3
  132 CONTINUE
      IF(IDES.EQ.0)GO TO 2223
*     it was type declaration for subr_name ??
* dfun1 does its=its-2 and ipce=ipcp, so
      ITS=ITS+2
      IPCP=IPCE
* and go to dfun1
      GO TO 2037
*   DSUB2
  133 CONTINUE
 2223 IF(IDES.NE.0)THEN
        IGSST=-1
        GO TO 1
      ENDIF
      IADEXT=CSLTGP(IPVSP)
      IF(IADEXT.EQ.0)THEN
        ITYPGP=CSKIDN(JID,LENEGI)
        IF(IABS(ITYPGP).EQ.KTCHAR)GO TO 711
        IADEXT=CSITGP(IPVSP)
      ENDIF
      ISHGI=IADEXT
      MODEGI=-2
      ITYPGI=CSKIDN(JID,LENEGI)
      LXXGLI=IBSET(LXXGLI,KUSEB)
      IDES=CSITLI(IPVSL)
      GO TO 2023
*   DBP
  134 CONTINUE
      CALL CSSEMS(3)
      IF(ISEM(ITS-1).EQ.7)ISEM(ITS-1)=5
      CALL CCOPYA(ISEM(ITS-4),ISEM(ITS-1),4)
      ISEM(ITS-4)=IPCE-IPCB
      ISEM(ITS-3)=JGSST
      ISEM(ITS-2)=NSTRG*100+(JGSST-JMC)
      ITS=ITS+3
      GO TO 1
*   DEP
  135 CONTINUE
      IB1=ISEM(ITS-1)
      IT1=ISEM(ITS-2)
      IF(IPCE+7.GE.LAST)GO TO 727
*                       ---------
      CALL CSDEPA(IPCE,IPCB,NPARAM,IOST,IKEA,IKCHSS)
      GO TO 1
*   DESUB
  136 CONTINUE
      CALL CS2036(LSTCL,IPCE,IPCB,NPARAM)
 2035 IT=ISEM(ITS-1)
      IOST=0
      IF(.NOT.(IT.EQ.10 .OR. IT.EQ.11))THEN
*        IF(IMNONE.EQ.1.AND.IDESFS.GT.0)THEN
          CALL CSLDLI(IDESFS)
          IF(ITYPGI.LT.0)THEN
            ITYPGI=KTHOLL
            CALL CSRTLI(IDESFS)
          ENDIF
*        ENDIF
      ENDIF
      IPCE=IPCE-2
      ITS=ITS-2
      GO TO 1
*   DEF0
  137 CONTINUE
      ISEM(ITS-1)=5
      GO TO 2070
*   DEF
  138 CONTINUE
 2070 IB1=ISEM(ITS-1)
      IF(IB1.LT.0)THEN
* INTRINSIC FUNCTION
        NPARIF=-IB1
        ITFUN=ISEM(ITS-2)
        NPAR=ISEM(ITS-3)
        IF(IPCE+4.GT.LAST)GO TO 727
*                          ------
        IF(NPARIF.EQ.1)THEN
          KOD(IPCE)=KIFUN1
          IF(NPAR.NE.1)THEN
            IF(ISEM(ITS-4).NE.67 .OR. NPAR.NE.2)GO TO 738
*  function cmplx(a1,a2)
            KOD(IPCE)=KIFUN2
          ENDIF
        ELSEIF(NPARIF.EQ.2)THEN
          IF(NPAR.NE.2)GO TO 738
          KOD(IPCE)=KIFUN2
        ELSEIF(NPARIF.EQ.3)THEN
          IF(NPAR.LT.2)GO TO 738
          KOD(IPCE)=KIFUNN
          IPCE=IPCE+1
          KOD(IPCE)=NPAR
        ENDIF
        KOD(IPCE+1)=ISEM(ITS-4)
        KOD(IPCE+2)=IT1
        IF(ITFUN.LT.0)THEN
          IF(ISEM(ITS-4).EQ.25 .AND. IT1.EQ.7)THEN
*   abs(complex) is real*4
            ITFUN=2
          ELSE
            ITFUN=IT1
          ENDIF
        ENDIF
        IPCE=IPCE+3
        ITS=ITS-NPAR*3-2
        ISEM(ITS-2)=ITFUN
        ISEM(ITS-1)=5
        GO TO 1
      ENDIF
      CALL CS2036(LSTCL,IPCE,IPCB,NPARAM)
      GO TO 1
*   DCVA
  139 CONTINUE
      I=IPCE-3
      IF(NUMGI.GT.0)I=I-1
      KOD(I)=KOD(I)+1
      GO TO 1
*   DCASS
  140 CONTINUE
      IF(ISEM(ITS-2).NE.3) GO TO 718
*                        -----------
      IF(IPCE+1.GE.LAST)GO TO 727
*                       ----------
      KOD(IPCE)=KCASSN
      IPCE=IPCE+1
      ITS=ITS-4
      GO TO 1
*   DAA
  141 CONTINUE
      IF(ISEM(ITS-1).GT.5) GO TO 720
*                          ---------
*****      KOD(IPCE-3)=KOD(IPCE-3)+1
      KOD(IPCP)=KOD(IPCP)+1
      GO TO 1
*   DCAA
  142 CONTINUE
      IF(ISEM(ITS-2).NE.3) GO TO 718
*                          ---------
      KOD(IPCE-3)=KOD(IPCE-3)+1
      GO TO 1
*   DVA
  143 CONTINUE
      KOD(IPCP)=KOD(IPCP)+1
      GO TO 1
*   DASS
  144 CONTINUE
      IT1=ISEM(ITS-4)
      IB1=ISEM(ITS-3)
      IT2=ISEM(ITS-2)
      IB2=ISEM(ITS-1)
****       ASSIGN 2020 TO LAB1
****           GO TO 1600
* T1=T2 --> T1=T1
      KEY=KT1T2(IT1,IT2)
      IF(KEY.EQ.0)GO TO 726
*                   -------
      IF(IB2.GT.5)GO TO 726
*                  --------
      CALL CS1600(IPCE)
      IF(IGSST.LT.0)GO TO 1
****
 2020 IF(IPCE+1.GE.LAST)GO TO 727
*                       ---------
      KOD(IPCE)=KASSN
      IF(IT1.EQ.5)KOD(IPCE)=KDASS
      IF(IT1.EQ.7)KOD(IPCE)=KCXASS
      IPCE=IPCE+1
      ITS=ITS-4
      GO TO 1
*   DASSGO
  145 CONTINUE
      IL=CSLTLI(IPVSL)
      IF(IL.GT.0)THEN
        IF(NUMGI.NE.0 .OR. IABS(ITYPGI).NE.1)GO TO 726
        IF(MODEGI.LT.0 .OR. MODEGI.GT.1)GO TO 726
*E                                        -------
      ELSE
        ISHGI=LSFT
        LSFT=LSFT+1
        ITYPGI=CSKIDN(JID,LENEGI)
        IF(IABS(ITYPGI).NE.1)GO TO 726
*E                             -------
        NUMGI=0
        MODEGI=1
        IL=CSITLI(IPVSL)
      ENDIF
      IF(IPCE+3.GE.LAST)GO TO 727
*E                         ------
      KOD(IPCE)=KASSGO
      KOD(IPCE+1)=ISHGI
      IPCE=IPCE+2
      CALL CSLAB1(IPCE)
      GO TO 1
*   DTHEN
  146 CONTINUE
      IT1=ISEM(ITS-2)
      IB1=ISEM(ITS-1)
      IF(IT1.EQ.3 .OR. IT1.GE.5 .OR. IB1.GT.5)GO TO 726
*                                             --------
      IF(IPCE+2.GE.LAST)GO TO 727
*                        ---------
      KSTRST=KSTRST+1
      ISEM(ITS-1)=0
 2044 KOD(IPCE)=KBZI
      IF(IT1.EQ.2)KOD(IPCE)=KBZR
      IPCE=IPCE+1
      ISEM(ITS-2)=LOCLAB
      LAB=LOCLAB
      LOCLAB=LOCLAB-1
      CALL CSLAB1(IPCE)
      GO TO 1
*   DELSIF
  147 CONTINUE
      IT1=ISEM(ITS-2)
      IF(IT1.EQ.3 .OR. IT1.GE.5 .OR. ISEM(ITS-1).GT.5)GO TO 726
*                                                     ---------
      IF(IPCE+2.GE.LAST)GO TO 727
*                     -----------
      ITS=ITS-2
      GO TO 2044
*   DELSE
  148 CONTINUE
      IF(IPCE+2.GE.LAST)GO TO 727
*                       ---------
      KOD(IPCE)=KGO
      IPCE=IPCE+1
      LAB=ISEM(ITS-1)
      IF(LAB.EQ.0)THEN
         ISEM(ITS-1)=LOCLAB
          LAB=LOCLAB
         LOCLAB=LOCLAB-1
      ENDIF
      CALL CSLAB1(IPCE)
 2040 LAB=ISEM(ITS-2)
      CALL CSSLAB(IPCE,IPCB)
      GO TO 1
*   DFI1
  149 CONTINUE
      LAB=ISEM(ITS-1)
      IF(LAB.LT.0)THEN
        CALL CSSLAB(IPCE,IPCB)
        IF(IGSST.LT.0) GO TO 1
      ENDIF
 2039 LAB=ISEM(ITS-2)
      ITS=ITS-2
      KSTRST=KSTRST-1
      CALL CSSLAB(IPCE,IPCB)
      GO TO 1
*   DFI
  150 CONTINUE
      LAB=ISEM(ITS-1)
      ITS=ITS-2
      KSTRST=KSTRST-1
      CALL CSSLAB(IPCE,IPCB)
      GO TO 1
*   DLAB
  151 CONTINUE
      CALL CSSEMS(1)
      ISEM(ITS)=LAB
      ITS=ITS+1
      NLAB=NLAB+1
      GO TO 1
*   DGOC
  152 CONTINUE
      NLAB=0
      GO TO 1
*   DGO
  153 CONTINUE
      IF(IPCE+2.GE.LAST)GO TO 727
*                       ---------
      KOD(IPCE)=KGO
      IPCE=IPCE+1
      CALL CSLAB1(IPCE)
      GO TO 1
*   DGOI
  154 CONTINUE
      IL=CSLTLI(IPVSL)
      IF(IL.GT.0)THEN
        IF(NUMGI.NE.0 .OR. IABS(ITYPGI).NE.1)GO TO 726
        IF(MODEGI.LT.0 .OR. MODEGI.GT.1)GO TO 726
*E                                        -------
      ELSE
        ISHGI=LSFT
        LSFT=LSFT+1
        ITYPGI=CSKIDN(JID,LENEGI)
        IF(IABS(ITYPGI).NE.1)GO TO 726
*E                             -------
        NUMGI=0
        MODEGI=1
        IL=CSITLI(IPVSL)
      ENDIF
      IF(IPCE+2.GE.LAST)GO TO 727
*E                         ------
      KOD(IPCE)=KGOI
      KOD(IPCE+1)=ISHGI
      IPCE=IPCE+2
      GO TO 1
*   DGOE
  155 CONTINUE
      IT1=ISEM(ITS-2)
      IF(ISEM(ITS-1).GT.5.OR.IT1.GE.3)GO TO 717
*                                     ---------
      L=NLAB+3
      IF(IPCE+L.GE.LAST)GO TO 727
*                      ----------
      IF(IT1.EQ.2)THEN
        KOD(IPCE)=KRI
        IPCE=IPCE+1
      ENDIF
      KOD(IPCE)=KGOC
      KOD(IPCE+1)=NLAB
      IPCE=IPCE+2
      L=ITS-NLAB-2
      ITS=L
 2041 LAB=ISEM(L)
      CALL CSLAB1(IPCE)
 2042 L=L+1
      NLAB=NLAB-1
      IF(NLAB.GT.0)GO TO 2041
      GO TO 1
*   DOWH
  156 CONTINUE
      CALL CSSEMS(5)
      ISEM(ITS)=0
      ISEM(ITS+3)=IPCE-IPCB
      GO TO 2046
*   DOV
  157 CONTINUE
      IF(IDES.EQ.0)THEN
        ISHGI=LSFT
        LSFT=LSFT+1
        NUMGI=0
        ITYPGI=CSKIDN(JID,LENEGI)
        MODEGI=1
        LXXGLI=IBSET(LXXGLI,KUSEB)
        IDES=CSITLI(IPVSL)
      ELSEIF(ITYPGI.GT.2.OR.MODEGI.GT.1 .OR. MODEGI.LT.0)THEN
        GO TO 714
*       ---------
      ENDIF
      ITDO=IABS(ITYPGI)
      CALL CSSEMS(5)
      ISEM(ITS)=ITDO
      ISEM(ITS+1)=NUMGI
      ISEM(ITS+2)=ISHGI
 2046 ITS=ITS+5
      NDOL=NDOL+1
      IF(NDOL.GT.KNDOL)GO TO 716
*                      ---------
      IF(LAB.NE.0)THEN
         I=CSLTLL(IPVS)
         IF(I.GT.0)GO TO 705
*                  ---------
      ELSE
           LAB=-39999
      ENDIF
      IDOL(NDOL)=LAB
      LAB=LOCLAB
      LOCLAB=LOCLAB-1
      KSTRST=KSTRST+1
      GO TO 1
*   DOEXP
  158 CONTINUE
      IT1=ISEM(ITS-2)
      IF(IT1.GE.3.OR.ISEM(ITS-1).GT.5)GO TO 717
*                                     ---------
      ITS=ITS-2
      IF(IT1.EQ.ITDO)GO TO 1
      IF(IPCE+1.GE.LAST)GO TO 727
*                       ---------
      IF(IT1.EQ.2)THEN
        KOD(IPCE)=KRI
      ELSE
        KOD(IPCE)=KIR
      ENDIF
      IPCE=IPCE+1
      GO TO 1
*   DWHILE
  159 CONTINUE
      ITS=ITS-2
      IT1=ISEM(ITS)
      IB1=ISEM(ITS+1)
      IF(IT1.EQ.3.OR.IT1.GT.4.OR.IB1.GT.5)GO TO 717
      IF(IPCE+3.GE.LAST)GO TO 727
*EE                      --------
      IF(IT1.EQ.2)THEN
        KOD(IPCE)=KRI
        IPCE=IPCE+1
      ENDIF
      GO TO 2047
*   DO3
  160 CONTINUE
 2043 IF(IPCE+8.GE.LAST)GO TO 727
*                       ---------
      KOD(IPCE)=KDOI
      IF(ITDO.EQ.2)KOD(IPCE)=KDOR
      CALL CCOPYA(ISEM(ITS-4),KOD(IPCE+1),2)
      IPCE=IPCE+6
      ISEM(ITS-2)=IPCE - IPCB
 2047 KOD(IPCE)=KBZI
      IPCE=IPCE+1
      ISEM(ITS-1)=LAB
      CALL CSLAB1(IPCE)
      GO TO 1
*   DO2
  161 CONTINUE
      IF(IPCE+2.GE.LAST)GO TO 727
*                      -----------
      KOD(IPCE)=KLK
      IF(ITDO.EQ.1)THEN
        KOD(IPCE+1)=1
      ELSE
        RA(IPCE+1)=1.
      ENDIF
      IPCE=IPCE+2
      GO TO 2043
*   TDOE
  162 CONTINUE
      IGSST=0
      IF(IDOL(NDOL).GT.0.AND.IDOL(NDOL).EQ.LABOD)THEN
         LAB=LABOD
         I=CSLTLL(IPVS)
         IF(LABST.EQ.1)IGSST=1
      ENDIF
      GO TO 1
*   DOE
  163 CONTINUE
      IF(IPCE+3.GE.LAST)GO TO 727
*                       ---------
      KSTRST=KSTRST-1
      ITDO=ISEM(ITS-5)
      IF(ITDO.NE.0)THEN
        KOD(IPCE)=KODI
        IF(ITDO.EQ.2)KOD(IPCE)=KODR
        I=ISEM(ITS-2)
        KOD(IPCE+1)=I-3
        KOD(IPCE+2)=I+2
        IPCE=IPCE+3
      ELSE
        KOD(IPCE)=KGO
        KOD(IPCE+1)=ISEM(ITS-2)
        IPCE=IPCE+2
      ENDIF
      LABC=LAB
      LAB=ISEM(ITS-1)
      CALL CSSLAB(IPCE,IPCB)
 2045  CONTINUE
      LAB=LABC
      NDOL=NDOL-1
      ITS=ITS-5
      GO TO 1
*   DRET
  164 CONTINUE
      K=KRET
      IF(KPRO.EQ.3)K=KEXIT
      GO TO 2050
*   DSTOP
  165 CONTINUE
      K=KSTOP
      GO TO 2050
*   DCONT
  166 CONTINUE
*      K=KCONT
*      GO TO 2050
      GO TO 1
*   DQUIT
  167 CONTINUE
      K=KQUIT
      GO TO 2050
*   DSVJNF
  168 CONTINUE
      IGSST=0
      IF(NGSST.EQ.0)CALL CSRD(JGSST,NGSST)
*TEST ;
      K=MKBLAN(JGSST,NGSST)
      IF(K.EQ.ICHSSC )THEN
        JGSST=JGSST+1
        NGSST=NGSST-1
        IF(NGSST.EQ.0)CALL CSRD(JGSST,NGSST)
      ENDIF
      JSV=JGSST
      NSV=NGSST
      GO TO 1
*   DSETC
  169 CONTINUE
      KEYC=1
      IDESFS=IDES
#if defined(CERNLIB_PAW)
      IF(MODHFI.NE.0)THEN
        CALL CSGTIDL(IDES,KUVNAME,NCKUV)
        DO 4040 I=1,MODHFI
          IF(KUVNAME.EQ.HFINMS(I))THEN
            CALL CSMARK(700)
            NCLHFI=NCLHFI+1
          ENDIF
 4040   CONTINUE
      ENDIF
#endif
      GO TO 1
*   DTESTC
  170 CONTINUE
      IGSST=0
      IF(KEYC.EQ.0)IGSST=1
      GO TO 1
*   DRSJNT
  171 CONTINUE
      JGSST=JSV
      NGSST=NSV
      GO TO 1
*   DPRPAU
  172 CONTINUE
      IH=CSICNS(JGSST,NGSST,NSST)
      JSST=MJCHAR(IQ(IH))
      I=NSST/NBYTPW
      IF(MOD(NSST,NBYTPW).NE.0)I=I+1
      L=I+2
      IF(IPCE+L.GE.LAST)GO TO 727
*                       ---------
      KOD(IPCE)=KPAUSE
      KOD(IPCE+1)=NSST
      CALL CCOPYS(JSST,MJCHAR(KOD(IPCE+2)),NSST)
      IPCE=IPCE+L
       CALL MHFREE(IH)
      GO TO 1
*   DPAUSE
  173 CONTINUE
      IF(IPCE+2.GE.LAST)GO TO 727
*                        --------
      KOD(IPCE)=KPAUSE
      KOD(IPCE+1)=0
      IPCE=IPCE+2
      GO TO 1
*   BBLOCK
  174 CONTINUE
      IF(IPCBL.NE.0)THEN
       CALL CSSOUT('[...[...]..] NOT PERMIT')
       GO TO 730
      ENDIF
       IF(IPCE+6.GT.LAST)GO TO 727
*                        ---------
      KOD(IPCE)=KLPB
      KOD(IPCE+2)=KBBLK
      IPCBL=IPCE+2
      KOD(IPCBL+1)=0
      KOD(IPCBL+2)=0
      KOD(IPCBL+3)=0
      ISEM(ITS-7)=IPCBL-IPCB
      IPCE=IPCE+6
      GO TO 1
*   EBLOCK
  175 CONTINUE
      KOD(IPCE)=KEBLK
      IPCE=IPCE+1
      IF(IPCE.GT.LAST)GO TO 727
*                     ---------
      KOD(IPCBL-1)=IPCE-IPCBL
      IPCBL=0
      CALL CSSEMS(2)
      ISEM(ITS)=1
      ISEM(ITS+1)=8
      ITS=ITS+2
      GO TO 1
*   ASKIP
  176 CONTINUE
       CALL CSSEMS(2)
      ISEM(ITS)=1
      ISEM(ITS+1)=9
      ITS=ITS+2
      JGSST=JGSST-1
      NGSST=NGSST+1
      GO TO 1
*   DELIM
  177 CONTINUE
      IF(NGSST.GT.0)THEN
        K=MKCHAR(JGSST)
        IGSST=MDELIM(K)
        IF(K.EQ.ICHBLN )K=MKBLAN(JGSST,NGSST)
        IF(K.EQ.ICHSEQ)IGSST=0
        IF(IGSST.EQ.0)THEN
          JGSST=JSV
          NGSST=NSV
        ENDIF
      ELSE
        IGSST=1
      ENDIF
      GO TO 1
*   DNSTRF
  178 CONTINUE
      IGSST=0
      IF(NGSST.EQ.0)CALL CSRD(JGSST,NGSST)
      JSV=JGSST
      NSV=NGSST
      IDESFS=0
      IF(NTRACE.EQ.1)GO TO 1
      IF(NSTRL.LT.NSTRG)THEN
        IF(IPCE+2.GT.LAST)GO TO 727
*                           -------
        KOD(IPCE)=KNUM
        KOD(IPCE+1)=NSTRG
        IPCE=IPCE+2
        NSTRL=NSTRG
      ENDIF
      GO TO 1
 2050 IF(IPCE+1.GE.LAST)GO TO 727
*                        --------
      KOD(IPCE)=K
      IPCE=IPCE+1
      GO TO 1
*   DIFA
  179 CONTINUE
      IT1=ISEM(ITS-2)
      IF(ISEM(ITS-1).GT.5 .OR. IT1.EQ.3)GOTO 717
*                                       --------
      ITS=ITS-2
      NLAB=0
      GO TO 1
*   DFIA
  180 CONTINUE
      IF(IPCE+4.GE.LAST)GO TO 727
*                       ---------
      IPCP=IPCE
      IPCE=IPCE+1
      L=ITS-3
      ITS=L
      K=1
 2061     LAB=ISEM(L)
          CALL CSLAB1(IPCE)
 2060     L=L+1
      K=K+1
      IF(K.LE.3)GOTO 2061
      NLAB=0
      IF(IT1.EQ.1 .OR. IT1.EQ.4)THEN
             KOD(IPCP)=KIFAI
      ELSEIF(IT1.EQ.2) THEN
             KOD(IPCP)=KIFAR
      ELSEIF(IT1.EQ.5)THEN
             KOD(IPCP)=KIFAD
      ELSE
        GO TO 720
*E      ---------
      ENDIF
      GO TO 1
*   DWRITE
  181 CONTINUE
* KWRITE ISIF,LFMT,LEND,LERR
      K=KWRITE
      IOWR=0
      IF(INFVEC(6).NE.0)THEN
        IGSST=-1
        GO TO 1
      ENDIF
      GO TO 2101
*   DREAD
  182 CONTINUE
* KREAD ISIF,LFMT,LEND,LERR
*             -1   -1   -1   if missing
      K=KREAD
      IOWR=1
 2101 IF(IPCE+25.GT.LAST)GO TO 727
*                       ---------
      IF(INFVEC(7).EQ.0)THEN
        KOD(IPCE)=KLK
        KOD(IPCE+1)=0
        IPCE=IPCE+2
        KOD(IPCE)=KSTO
        KOD(IPCE+1)=LOCF(JIOSTA)
        IPCE=IPCE+2
      ENDIF
      IF(INFVEC(4).EQ.0)THEN
        KOD(IPCE)=KLK
        KOD(IPCE+1)=-1
        IPCE=IPCE+2
        KOD(IPCE)=KSTO
        KOD(IPCE+1)=LOCF(NDAREC)
        IPCE=IPCE+2
      ENDIF
      KOD(IPCE)=K
      KOD(IPCE+1)=IOLUN
      IPCE=IPCE+2
      IF(INFVEC(5).EQ.0)GO TO 2104
*     FMT='(...)'
      I=IPCE+3
      KOD(IPCE)=I-IPCB
      IPCE=IPCE+1
      NW=(INFVEC(5)-1)/NBYTPW+1
      I1=I+3+NW
      II=I1+ICOD-1
      IF(II.GT.LAST)GO TO 727
*                   ---------
      INFVEC(5)=II
      J=MJCHAR(KOD(I+3))
      JF=MJSCHA(FMT)
      CALL CCOPYS(JF,J,NW*NBYTPW)
      CALL CCOPYA(IFCODE(1),KOD(I1),ICOD-1)
      KOD(I)=KJMPT
      KOD(I+1)=NW+ICOD
      KOD(I+2)=NW
      GO TO 2106
 2104 IF(LABFMT.GE.0)GO TO 2105
      IF(LABFMT.EQ.-2 .AND. IOLUN.EQ.1)IGSST=-1
      KOD(IPCE)=LABFMT
      IPCE=IPCE+1
      GO TO 2106
 2105 LAB=LABFMT
      CALL CSLAB1(IPCE)
 2106 IF(INFVEC(6).NE.0)GO TO 2107
      KOD(IPCE)=-1
      IPCE=IPCE+1
      GO TO 2108
 2107 LAB=LABEND
      CALL CSLAB1(IPCE)
 2108 IF(INFVEC(8).NE.0)GO TO 2109
      KOD(IPCE)=-1
      IPCE=IPCE+1
      GO TO 2110
 2109 LAB=LABOPE
      CALL CSLAB1(IPCE)
 2110 IF(INFVEC(5).NE.0)IPCE=INFVEC(5)
      GO TO 1
*   DSFRMT
  183 CONTINUE
*JMPT NTOT,NWF,(FORMAT),INTERNALCODES
      KOD(IPCE)=KJMPT
      IPCE=IPCE+3
      IF(IPCE.GT.LAST)GO TO 727
      LENCH=LEN(FMT)
      IF(NGSST.LE.0)THEN
        IGSST=-1
        GO TO 1
      ENDIF
      JFRMT=MJSCHA(FMT)
      JF=JFRMT
      FMT=' '
      LFRMT=0
      NLCONT=0
 2102 LASTCH=MKCHAR(JGSST+NGSST-1)
      IF(NLCONT.LT.20 .AND. LASTCH.NE.ICHKET )THEN
        LFRMT=LFRMT+NGSST
        IF(LFRMT.GT.LENCH)GO TO 2103
        CALL CCOPYS(JGSST,JFRMT,NGSST)
        JFRMT=JFRMT+NGSST
        CALL CSRD(JGSST,NGSST)
        IF(NGSST.LT.0)THEN
          IGSST=-1
          GO TO 1
        ENDIF
        IF(IFORS.NE.0 .AND. KEYC.EQ.0)THEN
          IGSST=-1
          GO TO 1
        ENDIF
        NLCONT=NLCONT+1
        GO TO 2102
      ENDIF
      LFRMT=LFRMT+NGSST
      IF(LFRMT.GT.LENCH)GO TO 2103
      CALL CCOPYS(JGSST,JFRMT,NGSST)
      NGSST=0
      NW=(LFRMT-1)/NBYTPW+1
      CALL CSFMTP(FMT(1:LFRMT))
      LCOD=ICOD-1
      IF(LCOD.EQ.0)THEN
        IGSST=-1
        GO TO 1
      ENDIF
      L=NW*NBYTPW
      J=MJCHAR(KOD(IPCE))
      I=IPCE+NW
      KOD(IPCE-1)=NW
      NW=NW+LCOD
      KOD(IPCE-2)=NW+1
      IPCE=IPCE+NW
      IF(IPCE.GT.LAST)GO TO 727
      CALL CCOPYS(JF,J,L)
      CALL CCOPYA(IFCODE(1),KOD(I),LCOD)
      GO TO 1
* 2103 IF(LIOERR.EQ.-1) PRINT *,' Too long format. (>256 chars) '
 2103 PRINT *,' Too long format. (>512 chars) '
      IGSST=-1
      GO TO 1
*   DLUNS
  184 CONTINUE
      KOD(IPCE)=KLAK
      KOD(IPCE+1)=-1
      IPCE=IPCE+2
      IF(IPCE.GT.LAST)GO TO 727
      IOLUN=0
      GO TO 1
*   DLUN
  185 CONTINUE
      ITS=ITS-2
      IT1=ISEM(ITS)
      IB1=ISEM(ITS+1)
      KOD(IPCP)=KOD(IPCP)+1
      IF( IB1.EQ.1 .OR. IB1.GT.4)IGSST=-1
      IF(IT1.EQ.1)THEN
        IOLUN=0
      ELSEIF(IT1.EQ.3)THEN
        IF(IB1.EQ.2)IGSST=-1
        IF(IPCE+2.GT.LAST)GO TO 727
        KOD(IPCE)=KDCV
        KOD(IPCE+1)=LSFT
        LSFT=LSFT+2
        IPCE=IPCE+2
        IOLUN=1
      ELSE
        IGSST=-1
      ENDIF
      GO TO 1
*   DFFRMT
  186 CONTINUE
      IF(LABFMT.NE.-2)IGSST=-1
      LABFMT=-1
      GO TO 1
*   DFRMT
  208 CONTINUE
C     FMT=LAB or FMT='(...)'
      IF(LABFMT.NE.-2)IGSST=-1
      ITS=ITS-2
      IT1=ISEM(ITS)
      IB1=ISEM(ITS+1)
      IF(IB1.NE.2)THEN
        IGSST=-1
        GO TO 1
      ENDIF
      IF(IT1.EQ.1)THEN
        LABFMT=KOD(IPCP+1)
      ELSEIF(IT1.EQ.3)THEN
        J=MJCHAR(KOD(IPCP+2))
        NS=KOD(IPCP+1)
        JF=MJSCHA(FMT)
        FMT=' '
        CALL CCOPYS(J,JF,NS)
        INFVEC(5)=NS
        CALL CSFMTP(FMT(1:NS))
        IF(ICOD.EQ.1)IGSST=-1
      ELSE
        IGSST=-1
      ENDIF
      IPCE=IPCP
      GO TO 1
*   DEIOL
  187 CONTINUE
      IF(NGSST.GT.0)THEN
        IGSST=0
        K=MKBLAN(JGSST,NGSST)
        IF(K.EQ.ICHSSC)THEN
          IGSST=1
          JGSST=JGSST+1
          NGSST=NGSST-1
        ENDIF
      ELSE
        CALL CSRD(JGSST,NGSST)
        IF(NGSST.LT.0)THEN
          IGSST=-1
          GO TO 1
        ENDIF
        IGSST=0
        IF(IFORS.NE.0 .AND. KEYC.EQ.0)IGSST=1
      ENDIF
      GO TO 1
*   DIOEND
  188 CONTINUE
      KOD(IPCE)=KIOEND
      IPCE=IPCE+1
      IF(IPCE.GT.LAST)GO TO 727
      GO TO 1
*   DOIO
  189 CONTINUE
      CALL CSSEMS(1)
      ISEM(ITS)=IPCE
      ITS=ITS+1
      KOD(IPCE)=KGO
      IPCE=IPCE+10
      IF(IPCE.GT.LAST) GO TO 727
      GO TO 1
*   DIO
  190 CONTINUE
      ITS=ITS-2
      IB1=ISEM(ITS+1)
      IT1=ISEM(ITS)
      IF(IPCE+7.GE.LAST)GO TO 727
*                       ---------
      CALL CSDIO(IPCE,IOWR)
      GO TO 1
*   DVARIO
  191 CONTINUE
      ITS=ITS-1
      IPCDO=ISEM(ITS)
      IPCE=IPCP
      KOD(IPCE)=KGO
      IPCL2=IPCE+1
      IPCE=IPCE+2
      KOD(IPCDO+1)=IPCE-IPCB
      IPCDO=IPCDO+2
      ITDO=IT1
      KOD(IPCDO)=KDOI
      IF(ITDO.EQ.2)KOD(IPCDO)=KDOR
      KOD(IPCDO+1)=NUMGI
      KOD(IPCDO+2)=ISHGI
      GO TO 1
*   DOIO2
  192 CONTINUE
      KOD(IPCE)=KLK
      IF(ITDO.EQ.1)THEN
         KOD(IPCE+1)=1
      ELSE
         RA(IPCE+1)=1.
      ENDIF
      IPCE=IPCE+2
      GO TO 2121
*   DOIO3
  193 CONTINUE
 2121 IF(IPCE+5.GT.LAST)GO TO 727
      KOD(IPCE)=KGO
      KOD(IPCE+1)=IPCDO-IPCB
      IPCE=IPCE+2
      KOD(IPCL2)=IPCE-IPCB
      KOD(IPCE)=KODI
      IF(ITDO.EQ.2)KOD(IPCE)=KODR
      IPCDO=IPCDO+6
      KOD(IPCE+1)=IPCDO-IPCB-3
      KOD(IPCE+2)=IPCDO-IPCB+2
      IPCE=IPCE+3
      KOD(IPCDO)=KBZI
      KOD(IPCDO+1)=IPCE-IPCB
      GO TO 1
*   DOPENB
  194 CONTINUE
      DO 2131 I=1,18
 2131 INFVEC(I)=0
      LABFMT=-2
      GO TO 1
*   DOPLUN
  195 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.1)GO TO 722
      IF(INFVEC(1).NE.0)IGSST=-1
      INFVEC(1)=1
      KOD(IPCE)=KSTO
      KOD(IPCE+1)=LOCF(LUNOP)
      IPCE=IPCE+2
      IF(IPCE.GT.LAST)GO TO 727
      GO TO 1
*   DOPFIL
  196 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.3)GO TO 718
      IF(INFVEC(2).NE.0)IGSST=-1
      INFVEC(2)=1
      IF(IPCE+3.GT.LAST)GO TO 727
      KOD(IPCE)=KSTOC
      KOD(IPCE+1)=LEN(FILEOP)
      KOD(IPCE+2)=MJSCHA(FILEOP)
      IPCE=IPCE+3
      GO TO 1
*   DOPSTA
  197 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.3)GO TO 718
      IF(INFVEC(3).NE.0)IGSST=-1
      INFVEC(3)=1
      IF(IPCE+3.GT.LAST)GO TO 727
      KOD(IPCE)=KSTOC
      KOD(IPCE+1)=LEN(STATOP)
      KOD(IPCE+2)=MJSCHA(STATOP)
      IPCE=IPCE+3
      GO TO 1
*   DOPACC
  198 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.3)GO TO 718
      IF(INFVEC(4).NE.0)IGSST=-1
      INFVEC(4)=1
      IF(IPCE+3.GT.LAST)GO TO 727
      KOD(IPCE)=KSTOC
      KOD(IPCE+1)=LEN(ACCEOP)
      KOD(IPCE+2)=MJSCHA(ACCEOP)
      IPCE=IPCE+3
      GO TO 1
*   DOPFOR
  199 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.3)GO TO 718
      IF(INFVEC(5).NE.0)IGSST=-1
      INFVEC(5)=1
      IF(IPCE+3.GT.LAST)GO TO 727
      KOD(IPCE)=KSTOC
      KOD(IPCE+1)=LEN(FORMOP)
      KOD(IPCE+2)=MJSCHA(FORMOP)
      IPCE=IPCE+3
      GO TO 1
*   DOPREC
  200 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.1)GO TO 722
      IF(INFVEC(6).NE.0)IGSST=-1
      INFVEC(6)=1
      KOD(IPCE)=KSTO
      KOD(IPCE+1)=LOCF(LRECOP)
      IPCE=IPCE+2
      IF(IPCE.GT.LAST)GO TO 727
      GO TO 1
*   DOPIOS
  201 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.1)GO TO 722
      IB1=ISEM(ITS+1)
      IF(IB1.LT.3 .OR. IB1.GT.4)IGSST=-1
      IF(INFVEC(7).NE.0)IGSST=-1
      INFVEC(7)=1
      KOD(IPCP)=KOD(IPCP)+1
      KOD(IPCE)=KSTO
      KOD(IPCE+1)=LOCF(JIOSTA)
      IPCE=IPCE+2
      IF(IPCE.GT.LAST)GO TO 727
      GO TO 1
*   DDAREC
  210 CONTINUE
      ITS=ITS-2
      IF(ISEM(ITS).NE.1)GO TO 722
      IB1=ISEM(ITS+1)
      IF(IB1.GT.4)IGSST=-1
      IF(INFVEC(4).NE.0)IGSST=-1
      INFVEC(4)=1
      KOD(IPCE)=KSTO
      KOD(IPCE+1)=LOCF(NDAREC)
      IPCE=IPCE+2
      IF(IPCE.GT.LAST)GO TO 727
      GO TO 1
*   DOPERR
  202 CONTINUE
      LABOPE=LAB
      IF(INFVEC(8).NE.0.OR.LAB.EQ.0)IGSST=-1
      INFVEC(8)=1
      GO TO 1
*   DIOLEND
  209 CONTINUE
      LABEND=LAB
      IF(INFVEC(6).NE.0.OR.LAB.EQ.0)IGSST=-1
      INFVEC(6)=1
      GO TO 1
*   DOPENE
  203 CONTINUE
      IF(INFVEC(1).EQ.0)IGSST=-1
* OPEN IFIL,ISTAT,IACCESS,IFORM,IRECL,IOSTAT,IERR
*       1     2      3      4     5     6      7
      IF(IPCE+8.GT.LAST)GO TO 727
      KOD(IPCE)=KOPEN
      CALL CCOPYA(INFVEC(2),KOD(IPCE+1),7)
      IPCE=IPCE+7
      IF(INFVEC(8).EQ.1)THEN
         LAB=LABOPE
         CALL CSLAB1(IPCE)
      ELSE
        IPCE=IPCE+1
      ENDIF
      GO TO 1
*   DCLOSE
  204 CONTINUE
      KOD(IPCE)=KCLOSE
 2141 IPCE=IPCE+1
      ITS=ITS-2
      IF(ISEM(ITS).NE.1)GO TO 722
      IF(IPCE.GT.LAST)GO TO 727
      GO TO 1
*   DREWIND
  205 CONTINUE
      KOD(IPCE)=KREWIN
      GO TO 2141
*   DBACKSP
  206 CONTINUE
      KOD(IPCE)=KBACKS
      GO TO 2141
*   DENDFIL
  207 CONTINUE
      KOD(IPCE)=KENDFI
      GO TO 2141
*   DINQACC
  104 CONTINUE
      J=LOCF(JADRLN(1,1))
      J1=LOCF(JADRLN(1,2))
      I=3
 2142 ITS=ITS-2
      IF(ISEM(ITS).NE.3)GO TO 718
      IF(INFVEC(I).NE.0)IGSST=-1
      INFVEC(I)=1
      IF(IPCE+4.GT.LAST)GO TO 727
      KOD(IPCP)=KOD(IPCP)+1
      KOD(IPCE)=KSTO
      KOD(IPCE+1)=J
      KOD(IPCE+2)=KSTO
      KOD(IPCE+3)=J1
      IPCE=IPCE+4
      GO TO 1
*   DINQBLK
  105 CONTINUE
      J=LOCF(JADRLN(2,1))
      J1=LOCF(JADRLN(2,2))
      I=4
      GO TO 2142
*   DINQDIR
  106 CONTINUE
      J=LOCF(JADRLN(3,1))
      J1=LOCF(JADRLN(3,2))
      I=5
      GO TO 2142
*   DINQEXT
  107 CONTINUE
      J=LOCF(JADRLN(4,1))
      J1=LOCF(JADRLN(4,2))
      I=6
 2143 ITS=ITS-2
      IF(ISEM(ITS).NE.1 .AND. ISEM(ITS).NE.4)GO TO 722
      IB1=ISEM(ITS+1)
      IF(IB1.LT.3 .OR. IB1.GT.4)IGSST=-1
      IF(INFVEC(I).NE.0)IGSST=-1
      INFVEC(I)=1
      KOD(IPCP)=KOD(IPCP)+1
      KOD(IPCE)=KSTO
      KOD(IPCE+1)=J
      IPCE=IPCE+2
      IF(IPCE.GT.LAST)GO TO 727
      GO TO 1
*   DINQFMD
  108 CONTINUE
      J=LOCF(JADRLN(6,1))
      J1=LOCF(JADRLN(6,2))
      I=9
      GO TO 2142
*   DINQFM
  109 CONTINUE
      J=LOCF(JADRLN(5,1))
      J1=LOCF(JADRLN(5,2))
      I=10
      GO TO 2142
*   DINQNMD
  110 CONTINUE
      J=LOCF(JADRLN(8,1))
      I=11
      GO TO 2143
*   DINQNAM
  111 CONTINUE
      J=LOCF(JADRLN(7,1))
      J1=LOCF(JADRLN(7,2))
      I=12
      GO TO 2142
*   DINQNXT
  112 CONTINUE
      J=LOCF(JADRLN(9,1))
      I=13
      GO TO 2143
*   DINQNUM
  113 CONTINUE
      J=LOCF(JADRLN(10,1))
      I=14
      GO TO 2143
*   DINQOD
  114 CONTINUE
      J=LOCF(JADRLN(11,1))
      I=15
      GO TO 2143
*   DINQRCL
  115 CONTINUE
      J=LOCF(JADRLN(12,1))
      I=16
      GO TO 2143
*   DINQSEQ
  116 CONTINUE
      J=LOCF(JADRLN(13,1))
      J1=LOCF(JADRLN(13,2))
      I=17
      GO TO 2142
*   DINQUNF
  117 CONTINUE
      J=LOCF(JADRLN(14,1))
      J1=LOCF(JADRLN(14,2))
      I=18
      GO TO 2142
*   DINQEND
  118 CONTINUE
      IF(INFVEC(1).EQ.1)THEN
        I=0
      ELSEIF(INFVEC(2).EQ.1)THEN
        I=1
      ELSE
        IGSST=-1
        GO TO 1
      ENDIF
* INQUIRE I,IERR
*       1     2      3      4     5     6      7
      IF(IPCE+3.GT.LAST)GO TO 727
      KOD(IPCE)=KINQU
      KOD(IPCE+1)=I
      IPCE=IPCE+2
      IF(INFVEC(8).EQ.1)THEN
         LAB=LABOPE
         CALL CSLAB1(IPCE)
      ELSE
        KOD(IPCE)=-1
        IPCE=IPCE+1
      ENDIF
      GO TO 1
  701 IGSST=-19
      GO TO 1
  705 IGSST=-5
      GO TO 1
  706 IGSST=-6
      GO TO 1
  708 IGSST=-8
      GO TO 1
  709 IGSST=-9
      GO TO 1
  710 IGSST=-10
      GO TO 1
  711 IGSST=-11
      GO TO 1
  714 IGSST=-14
      GO TO 1
  716 IGSST=-16
      GO TO 1
  717 IGSST=-17
      GO TO 1
  718 IGSST=-18
      GO TO 1
  720 IGSST=-20
      GO TO 1
  722 IGSST=-22
      GO TO 1
  723 IGSST=-23
      GO TO 1
  724 IGSST=-24
      GO TO 1
  725 IGSST=-25
      GO TO 1
  726 IGSST=-26
      GO TO 1
  727 IGSST=-27
      GO TO 1
  728 IGSST=-28
      GO TO 1
  729 IGSST=-29
      GO TO 1
  730 IGSST=-30
      GO TO 1
  731 IGSST=-31
      GO TO 1
  732 IGSST=-32
      GO TO 1
  733 IGSST=-33
      GO TO 1
  734 IGSST=-34
      GO TO 1
  735 IGSST=-35
      GO TO 1
  736 IGSST=-36
      GO TO 1
  737 IGSST=-37
      GO TO 1
  738  IGSST=-38
      GO TO 1
      END
