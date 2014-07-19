      PROGRAM CERNLIB
C***********************************************************************
C*                                                                     *
C*    CERNLIB Command v2.27                                            *
C*                                                                     *
C*    Author: F.Carminati 13-01-1989                                   *
C*                                                                     *
C*    Mods        Date   Comment                                       *
C*    MARQUINA  89/02/16 Add HERWIG,ISAJET,JETSET,COJETS,GEANT,PAWLIB  *
C*    MARQUINA  89/04/06 Change root directories for NAG+GKS           *
C*                       Fix NPACKLIB/OPT                              *
C*    MARQUINA  89/04/10 Partially rewritten                           *
C*    MARQUINA  89/07/04 Add CNL qualif., protect against undef.symbols*
C*    MARQUINA  89/07/17 Add EURODEC                                   *
C*    CARMINATI 89/09/07 Correct bug in LIBSET when no GKS & NAG       *
C*    CARMINATI 90/01/03 Add JETSET72 and search lists for root        *
C*    MARQUINA  90/07/25 Add MPALIB                                    *
C*              90/08/16 Add VMSLIB, change LIBVER to CERN_LEVEL       *
C*              90/10/23 Add PHOTOS                                    *
C*              90/10/31 Add TWISTER, provide special GRAF libs        *
C*                       Process X11,DGKS qualifiers                   *
C*              90/11/13 Change NPACKLIB > PACKLIB, NGRAFLIB > GRAFLIB *
C*                       Provide GRAFxxx.OPT with GRAFLIB/xxx          *
C*              91/03/06 Add FRITIOF                                   *
C*              91/04/09 Add PDFLIB,ARIADNE                            *
C*              91/04/17 Add GEANT313                                  *
C*              91/09/12 Swap JETSET and JETSET72                      *
C*              92/02/15 Enable wild-char matching of libraries        *
C*              92/02/28 Add GKS option + NAGGLIB                      *
C*                       Enable access to GKS-only                     *
C*    JAMIE     92/03/03 Integrate CLD in the image                    *
C*    MARQUINA  92/04/09 Support MC versions for GEANT,HERWIG,JETSET   *
C*    MARQUINA  92/06/28 Add MATHLIB, PYTHIA, use PYTHIA 5.6           *
C*                       Fix bug when lib declared after GRAFLIB/NOSHA *
C*              92/07/28 Add LEPTO, move to HERWIG 5.5                 *
C*                       Add PHTOOLS when requiring GENLIB             *
C*              93/02/08 version ISAJET, move to HERWIG 5.6            *
C*              93/09/14 Add MOTIF option, move to ISAJET 7.2          *
C*              93/09/18 Check if GKS is available to choose default   *
C*              93/12/10 Add GPHIGS,PHIGS,DECGKS options and GPHIGS lib*
C*    Jamie     94/02/21 Permit CERN_LEVEL=yyv, e.g. 94A               *
C*              94/03/08 Change versions of MCLIBS                     *
C*                                                                     *
C***********************************************************************
      IMPLICIT INTEGER(A-Z)
      INCLUDE '($LIBCLIDEF)'
      PARAMETER (MCV=6)
      CHARACTER LIBS*255, LIBRARY*255, INCLUDE*255, OUTLIB*255
      CHARACTER OLDDIR*32, OLDDEV*64, LINE*80
      CHARACTER*20 LBKEY(25),LBLIST(25,2),LBKEYM,MCNAME(MCV)
      CHARACTER*9 MODE(5)
      DIMENSION   MODL(5)
      EXTERNAL CLI$_PRESENT, CLI$_NEGATED, CLI$_COMMA
      EXTERNAL CLI$_LOCPRES, CLI$_LOCNEG
      EXTERNAL SS$_NORMAL, RMS$_NMF, LIB$_STRTRU, LIB$_NOSUCHSYM
      EXTERNAL CRN_NOFILE, CRN_NOLIB, CRN_NOSUCC, CRN_PRODV
      EXTERNAL CRN_NEWLIB
#ifndef CERNLIB_CLD
      EXTERNAL LIB$GET_INPUT,CERNLIBCLD
      INTEGER  CLI$DCL_PARSE,STATUS
#endif
      LOGICAL HBOOK4, GTS2D, GTS3D, KEY, FRSKEY, NEWLIB, DEBUG,GKSEXIST
      LOGICAL KERN,PACK,DONE,GKS,X11,DGKS,MOTIF,LEXIST,ADDGKS,GKSSHR
      LOGICAL PHIGS
      CHARACTER*255 CHLINE
      CHARACTER*10 FORCE
      CHARACTER*3  VER(4), DEF, CUR, DCNL, CCNL, MCVER(MCV),MCVERD
      CHARACTER*25 RTDIR
      CHARACTER*19 CLIB
      CHARACTER*26 ALFBET
      CHARACTER*10 CHNUM
      DATA         ALFBET/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA         CHNUM /'1234567890'/
      DATA CLIB/'SYS$LIBRARY:VAXCRTL'/
      DATA LEND/1/,CONTEXT/0/,LIBS/' '/,LLIB/0/,NEWLIB/.FALSE./
      DATA MODE/'SHAREABLE','OPTION','LIBRARY','INCLUDE',' '/
      DATA MODL/9,6,7,7,1/
      DATA DONE/.FALSE./,KERN/.FALSE./,PACK/.FALSE./,GKS/.FALSE./
      DATA GTS2D/.FALSE./,GTS3D/.FALSE./,ADDGKS/.FALSE./
      DATA VER/'OLD','PRO','NEW','CNL'/,DCNL/' '/,CCNL/' '/
      DATA NKEY/23/
      DATA LBKEY/
     +  'MATHLIB' ,'GRAFLIB' ,'GKS'     ,'PACKLIB' ,'KERNLIB' ,
     +  'NAGLIB'  ,'PAWLIB'  ,'HERWIG*' ,'ISAJET*' ,'JETSET*' ,
     +  'COJETS'  ,'GEANT*'  ,'EURODEC' ,'FRITIOF' ,'PYTHIA*' ,
     +  'VMSLIB'  ,'PHOTOS'  ,'TWISTER' ,'ARIADNE' ,'PDFLIB'  ,
     +  'LEPTO* ' ,'PHTOOLS ','GPHIGS ' ,'        ','      '  /
      DATA LBLIST/
     +  'MATHLIB' ,'OGRAFLIB','SGKS'    ,'OPACKLIB','KERNLIB' ,
     +  'NAGLIB'  ,'PAWLIB'  ,'HERWIG*' ,'ISAJET*' ,'JETSET*' ,
     +  'COJETS'  ,'GEANT*'  ,'EURODEC' ,'FRITIOF' ,'PYTHIA*' ,
     +  'VMSLIB'  ,'PHOTOS'  ,'TWISTER' ,'ARIADNE' ,'PDFLIB'  ,
     +  'LEPTO* ' ,'PHTOOLS ','GPHIGS ' ,'        ','      '  ,
     +  'MATHLIB' ,'GRAFLIB' ,'GKS'     ,'PACKLIB' ,'KERNLIB' ,
     +  'NAGLIB  ','PAWLIB'  ,'HERWIG*' ,'ISAJET*' ,'JETSET*' ,
     +  'COJETS'  ,'GEANT*'  ,'EURODEC' ,'FRITIOF' ,'PYTHIA*' ,
     +  'VMSLIB'  ,'PHOTOS'  ,'TWISTER' ,'ARIADNE' ,'PDFLIB'  ,
     +  'LEPTO* ' ,'PHTOOLS ','GPHIGS ' ,'        ','      '  /
      DATA MCNAME/
     +  'GEANT'   ,'HERWIG'  ,'JETSET'  ,'LEPTO'   ,'PYTHIA'  ,
     +  'ISAJET'  /
      DATA MCVER/
     +  '321'     ,'59'      ,'74'      ,'63'      ,'57'      ,
     +  '722'      /
#ifndef CERNLIB_CLD
C
C     Obtain command line
C
      STATUS=LIB$GET_FOREIGN(CHLINE,,LLINE)
      IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
C     Parse command line
C
      STATUS=CLI$DCL_PARSE('CERNLIB '//CHLINE(1:LLINE),
     +                        CERNLIBCLD,LIB$GET_INPUT)
      IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
#endif
C
      MODE(5)=MODE(3)
      OLDDEV = ' '
      OLDDIR = ' '
      RECODE=LIB$FIND_FILE('*.*',OUTLIB,DUMCON,,)
      IF(RECODE) CALL FTRIM(OLDDEV,OLDDIR,OUTLIB,INIT,IEND)
      FRSKEY = .TRUE.
C**** Get default directory: PROduction, OLD or NEW
      DEF = ' '
      CUR = '   '
      LENCUR=0
      RECODE=LIB$GET_SYMBOL('CERN_LEVEL',CUR,LENCUR,)
      IF(RECODE) THEN
         DO J=1,3
            IF(CUR.EQ.VER(J)) DEF=CUR
         END DO
         IF(DEF.EQ.' '.AND.INDXNC(CUR).EQ.0) THEN
            DEF ='CNL'
            DCNL=CUR
         ENDIF
      END IF
*
*     Permit yyv, e.g. 94A, or --since 98--just the year ( ie. 98 )
*
      IF(INDEX(CHNUM,CUR(1:1)).NE.0.AND.INDEX(CHNUM,CUR(2:2)).NE.0.AND.
     +   ( LENCUR.EQ.2 .OR.
     +    (LENCUR.EQ.3 .AND. INDEX(ALFBET,CUR(3:3)).NE.0) ) ) DEF = CUR
 
      IF(DEF.EQ.' ') DEF='PRO'
      DO J=1,4
        IF(CLI$PRESENT(VER(J)).EQ.%LOC(CLI$_PRESENT)) THEN
          DEF = VER(J)
          IF(J.EQ.4) THEN
            RECODE=CLI$GET_VALUE('CNL',DCNL,)
            IF(.NOT.RECODE) RECODE = LIB$SIGNAL(%VAL(RECODE))
          ENDIF
        ENDIF
      END DO
      CALL LIBSET(DEF//DCNL,'CERN_ROOT')
      CALL LIBSET(DEF//DCNL,'GKS_ROOT')
C     CALL LIBSET(DEF//DCNL,'NAG_ROOT')
 
C**** Find out default MC versions
      DO J=1,MCV
         RECODE=LIB$GET_SYMBOL(MCNAME(J)//'_LEVEL',MCVERD,,)
         IF(RECODE) MCVER(J)=MCVERD
      END DO
C**** Parse command: check parameters
C        LBFLAG =1        libraries supplied
C                2   set  packlib
C                3   set  kernlib
C                4   user libraries supplied
 
                              LBFLAG=2
      IF(CLI$PRESENT('USER')) LBFLAG=4
      IF(CLI$PRESENT('P1'))   LBFLAG=1
C**** User libraries missing
      IF(LBFLAG.EQ.4)         GOTO 990
      IF(LBFLAG.EQ.2)         DONE=.TRUE.
 
C**** Set flags for GLOBAL qualifiers
 
      NEWLIB = DEF.EQ.'NEW'
      HBOOK4 = .NOT.(
     +         CLI$PRESENT('HNEW')  .EQ.%LOC(CLI$_NEGATED).OR.
     +         CLI$PRESENT('HOLD')  .EQ.%LOC(CLI$_PRESENT).OR.
     +         CLI$PRESENT('PLOT10').EQ.%LOC(CLI$_PRESENT))
 
      DGKS   = CLI$PRESENT('DGKS')  .EQ.%LOC(CLI$_PRESENT).OR.
     +         CLI$PRESENT('DECGKS').EQ.%LOC(CLI$_PRESENT)
      X11    = CLI$PRESENT('X11')   .EQ.%LOC(CLI$_PRESENT)
      MOTIF  = CLI$PRESENT('MOTIF') .EQ.%LOC(CLI$_PRESENT)
      PHIGS  = CLI$PRESENT('GPHIGS').EQ.%LOC(CLI$_PRESENT).OR.
     +         CLI$PRESENT('PHIGS') .EQ.%LOC(CLI$_PRESENT)
      IF(.NOT.(DGKS.OR.MOTIF.OR.PHIGS.OR.GKSEXIST(DEF))) X11=.TRUE.
 
      IF(.NOT.(X11.OR.DGKS.OR.MOTIF.OR.PHIGS)) THEN
      GTS3D  = CLI$PRESENT('GTS3D') .EQ.%LOC(CLI$_PRESENT).OR.
     +         CLI$PRESENT('GTS2D') .EQ.%LOC(CLI$_NEGATED)
      GTS2D  = HBOOK4.AND..NOT.GTS3D
      ADDGKS = CLI$PRESENT('GKS')   .EQ.%LOC(CLI$_PRESENT).OR.
     +         CLI$PRESENT('GTS2D') .EQ.%LOC(CLI$_PRESENT).OR.GTS3D
      ENDIF
 
      DEBUG =  CLI$PRESENT('DEBUG') .EQ.%LOC(CLI$_PRESENT)
 
C**** Set proper GTS libraries in LBLIST
      IF(GTS2D)     THEN
C       LBLIST(3,1)='GKS_ROOT:[LIB]GKS'
        LBLIST(3,2)='GKS_LIB'
      ELSEIF(GTS3D) THEN
C       LBLIST(3,1)='GKS_ROOT:[LIB]GKS'
        LBLIST(3,2)='GKS3D_LIB'
      ENDIF
C**** Choose the LBLIST Library set: HOLD=1, HNEW=2
                 ISET=1
      IF(HBOOK4) ISET=2
1     CONTINUE
      CUR =DEF
      CCNL=DCNL
      IF(DEBUG) PRINT *,'DEF=',LBFLAG
      GOTO(10,20,30,200),LBFLAG
10    CONTINUE
C**** "parameter" libraries
      LAST = CLI$GET_VALUE('P1',LIBRARY,LLIB)
      IF(DEBUG) PRINT *,'LST=',LAST,' LIB=',LIBRARY(:LLIB)
      IF(.NOT.LAST) LAST = LIB$SIGNAL(%VAL(LAST))
C**** Set CUR version if locally present
      DO J=1,4
        IF(CLI$PRESENT(VER(J)).EQ.%LOC(CLI$_LOCPRES)) THEN
          CUR=VER(J)
          IF(J.EQ.3) NEWLIB=.TRUE.
          IF(J.EQ.4) THEN
            RECODE=CLI$GET_VALUE('CNL',CCNL,LCNL)
            IF(.NOT.RECODE) RECODE = LIB$SIGNAL(%VAL(RECODE))
          ENDIF
        END IF
      END DO
      IF(LAST.EQ.%LOC(SS$_NORMAL)) DONE=.TRUE.
      GOTO 50
20    CONTINUE
C**** Packlib
      KEY    =.TRUE.
      LIBRARY='CRN'
      IKEY   =4
      GOTO 56
30    CONTINUE
C**** Kernlib
      KEY    =.TRUE.
      LIBRARY='CRN'
      IKEY   =5
      GOTO 56
C40   CONTINUE
50    CONTINUE
      CHLINE=' '
C----
C     Point to new MATHLIB library when using old keywords
C
      IF(INDEX('GENLIB BVSL MPALIB LAPACK',LIBRARY(:LLIB)).NE.0) THEN
         IF(LIBRARY(:6).EQ.'GENLIB') CHLINE='PHTOOLS'
         LIBRARY='MATHLIB'
         LLIB   =7
      ENDIF
      IF(LIBRARY(:5).EQ.'LEPTO') CHLINE='JETSET'
C----
C     PYTHIA is inside JETSET
C
      IF(LIBRARY(1:6).EQ.'PYTHIA') THEN
         LIBRARY='JETSET'
         LLIB   =6
      ENDIF
C
      IF(LIBRARY(:LLIB).EQ.'GKS')
     +   GKSSHR=CLI$PRESENT('SHAREABLE').NE.%LOC(CLI$_LOCNEG)
 
51    CONTINUE
      KEY = .FALSE.
      DO K=1,MCV
         IF(LIBRARY(:LLIB).EQ.MCNAME(K)) THEN
           LIBRARY(LLIB+1:)=MCVER(K)
           LLIB=LENOCC(LIBRARY)
           IF(DEBUG) PRINT *,'MCV=',MCVER(K),LIBRARY(:LLIB)
         ENDIF
      END DO
C
      DO 55 N=1,NKEY
        LBKEYM=LBKEY(N)
        IPSTAR=INDEX(LBKEYM,'*')
        IF(IPSTAR.GT.0) LBKEYM(IPSTAR:)=LIBRARY(IPSTAR:LLIB)
        IF(LIBRARY(:LLIB).NE.LBKEYM) GOTO 55
        KEY    =.TRUE.
        IKEY   =N
        LIBRARY='CRN'
        IF(IPSTAR.GT.0) LBLIST(IKEY,ISET)=LBKEYM
        IF(N.EQ.3) THEN
C***    GKS SECTION
         IF(HBOOK4) THEN
           IF(GKSSHR) THEN
             DO K=1,4
               IF(CLI$PRESENT(VER(K)).EQ.%LOC(CLI$_LOCPRES)) THEN
                 CALL LIBSET(VER(K)//CCNL,'GKS_ROOT')
                 CONTEXT=0
               END IF
             END DO
             IF(GTS2D) LBLIST(N,2)='GKS'
             IF(GTS3D) LBLIST(N,2)='GKS3D'
           ELSE
             FORCE='OPTION'
           END IF
         ENDIF
         LIBRARY='GKS'
        ELSEIF(N.EQ.6) THEN
C****   NAG Section
C        IF(CUR.NE.'PRO')
C    +   RECODE=LIB$SIGNAL(CRN_PRODV,%VAL(2),CUR,'NAGLIB')
         IF(CLI$PRESENT('SHAREABLE').NE.%LOC(CLI$_LOCNEG)) THEN
           DO K=1,4
             IF(CLI$PRESENT(VER(K)).EQ.%LOC(CLI$_LOCPRES)) THEN
C              CALL LIBSET(VER(K)//CCNL,'NAG_ROOT')
               CONTEXT=0
             END IF
           END DO
         ELSE
           FORCE='OPTION'
         END IF
         LIBRARY='NAG'
         IF(ADDGKS) LBLIST(N,2)='NAGGLIB'
        ELSEIF(N.EQ.23) THEN
C****   PHIGS Section
           FORCE='OPTION'
        ENDIF
        GOTO 56
55    CONTINUE
      GOTO 60
56    CONTINUE
C**** CERN Library matched
      LIBRARY=RTDIR(CUR//CCNL,LIBRARY(1:3))
      LLIB   =LENOCC(LIBRARY)
      LIBRARY=LIBRARY(:LLIB)//LBLIST(IKEY,ISET)
      LLIB   =LENOCC(LIBRARY)
      IF(IKEY.EQ.4) PACK=.TRUE.
      IF(IKEY.EQ.5) KERN=.TRUE.
60    CONTINUE
      IF(DEBUG) PRINT *,'LEN=',LLIB,' LIB=',LIBRARY(:LLIB)
C****
      IF(KEY.AND.FRSKEY) THEN
        CONTEXT = 0
        OUTLIB  = ' '
        OLDDEV  = ' '
        OLDDIR  = ' '
      END IF
      FRSKEY=.NOT.KEY
 
C**** Find out Library mode. Default PACKLIB+KERNLIB are LIBs
C     IFLAG =1  SHAreable Library
C            2  OPTion
C            3  LIBrary
C            4  INClude
C            5  LIB=default
 
      IFLAG=5
      IF(LBFLAG.EQ.1) THEN
      DO N=1,4
        L=MODL(N)
        IF(CLI$PRESENT(MODE(N)(:L)).EQ.%LOC(CLI$_LOCPRES).OR.
     +     FORCE.EQ.MODE(N)(:L)) IFLAG=N
      ENDDO
      ENDIF
      IF(DEBUG) PRINT *,'LST=',LAST,' FLG=',IFLAG
 
C**** Do not resolve GKS
      IF(IKEY.EQ.3) THEN
        IF(.NOT.GKS) THEN
          LIBS(LEND:) = ','//LIBRARY(:LLIB)//'/'//MODE(IFLAG)(1:3)
          LEND = LEND +LLIB+5
        ENDIF
        IF(CUR.EQ.DEF) GKS=.TRUE.
        OLDDEV = 'GKS_ROOT:'
        OLDDIR = '[LIB]'
        GOTO 160
      ENDIF
      IF(IFLAG.EQ.1.AND.(IKEY.EQ.4.OR.IKEY.EQ.6)) IFLAG=2
C****   Take default for KERNLIB
        GOTO(110,120,130,140,150),IFLAG
110     CONTINUE
C****   Shareable library
        IF(LBFLAG.GT.1) THEN
          RECODE=LIB$SIGNAL(CRN_NOFILE,%VAL(1),LIBRARY(:LLIB))
          GOTO 160
        ENDIF
        RECODE=LIB$FIND_FILE(LIBRARY,OUTLIB,CONTEXT,'.EXE',OUTLIB)
        IF (RECODE) THEN
          CALL FTRIM(OLDDEV,OLDDIR,OUTLIB,INIT,IEND)
          LIBS(LEND:) = ','//OUTLIB(INIT:IEND)//'/SHA'
          LEND = LEND + IEND - INIT + 1 + 5
          IF(CLI$GET_VALUE('SHAREABLE',LIBRARY,LLIB)) THEN
            LIBS(LEND:) = '='//LIBRARY(:LLIB)
            LEND = LEND + 1 + LLIB
          END IF
          IF(CLI$PRESENT('INCLUDE').EQ.%LOC(CLI$_LOCPRES)) THEN
            INCLUDE = '/INC=('
            END = 7
            RECODE = %LOC(CLI$_COMMA)
            DO WHILE(RECODE.NE.%LOC(SS$_NORMAL))
              RECODE=CLI$GET_VALUE('INCLUDE',LIBRARY,LLIB)
              IF(.NOT.RECODE) THEN
                RECODE = LIB$SIGNAL(%VAL(RECODE))
              ELSE IF(RECODE.EQ.%LOC(SS$_NORMAL)) THEN
                INCLUDE(END:)=LIBRARY(:LLIB)//')'
                END = END+LLIB+1
              ELSE IF(RECODE.NE.%LOC(CLI$_ABSENT)) THEN
                INCLUDE(END:)=LIBRARY(:LLIB)//','
                END = END+LLIB+1
              END IF
            END DO
            END = END - 1
            LIBS(LEND:) = INCLUDE(:END)
            LEND = LEND + END
          END IF
        ELSE
          POINT = INDEX(LIBRARY,'.')
          IF(POINT.EQ.0) THEN
            LIBRARY(LLIB+1:)='.EXE'
            LLIB=LLIB+4
          END IF
          IF(RECODE.EQ.%LOC(RMS$_NMF)) THEN
             RECODE=LIB$SIGNAL(CRN_NOSUCC,%VAL(1),LIBRARY(:LLIB))
          END IF
          RECODE=LIB$SIGNAL(CRN_NOFILE,%VAL(1),LIBRARY(:LLIB))
        END IF
        GOTO 160
120     CONTINUE
C****   Option
        IF(LBFLAG.GT.2) THEN
          RECODE=LIB$SIGNAL(CRN_NOFILE,%VAL(1),LIBRARY(:LLIB))
          GOTO 160
        ENDIF
        RECODE=LIB$FIND_FILE(LIBRARY,OUTLIB,CONTEXT,'.OPT',OUTLIB)
        IF (RECODE) THEN
          CALL FTRIM(OLDDEV,OLDDIR,OUTLIB,INIT,IEND)
          LIBS(LEND:) = ','//OUTLIB(INIT:IEND)//'/OPT'
          LEND = LEND + 5 + IEND - INIT + 1
        ELSE
          POINT = INDEX(LIBRARY,']')
          POINT = INDEX(LIBRARY(POINT+1:),'.')+POINT
          IF(POINT.EQ.0) THEN
            LIBRARY(LLIB+1:)='.OPT'
            LLIB=LLIB+4
          END IF
          IF(RECODE.EQ.%LOC(RMS$_NMF)) THEN
             RECODE=LIB$SIGNAL(CRN_NOSUCC,%VAL(1),LIBRARY(:LLIB))
          END IF
          RECODE=LIB$SIGNAL(CRN_NOFILE,%VAL(1),LIBRARY(:LLIB))
        END IF
        GOTO 160
130     CONTINUE
C****   Library
140     CONTINUE
C****   Include
        RECODE=LIB$FIND_FILE(LIBRARY,OUTLIB,CONTEXT,'.OLB',OUTLIB)
        IF(RECODE) THEN
          CALL FTRIM(OLDDEV,OLDDIR,OUTLIB,INIT,IEND)
          LIBS(LEND:) = ','//OUTLIB(INIT:IEND)
          LEND = LEND + IEND - INIT + 1 + 1
          IF(CLI$PRESENT('LIBRARY').EQ.%LOC(CLI$_LOCPRES)) THEN
            LIBS(LEND:) = '/LIB'
            LEND = LEND + 4
          END IF
C         IF(CLI$PRESENT('INCLUDE').EQ.%LOC(CLI$_LOCPRES).AND.
C    +                                      DEFAULT.LE.1) THEN
          IF(CLI$PRESENT('INCLUDE').EQ.%LOC(CLI$_LOCPRES))THEN
            INCLUDE = '/INC=('
            END = 7
            RECODE = %LOC(CLI$_COMMA)
            DO WHILE(RECODE.NE.%LOC(SS$_NORMAL))
              RECODE=CLI$GET_VALUE('INCLUDE',LIBRARY,LLIB)
              IF(.NOT.RECODE) THEN
                RECODE = LIB$SIGNAL(%VAL(RECODE))
              ELSE IF(RECODE.EQ.%LOC(SS$_NORMAL)) THEN
                INCLUDE(END:)=LIBRARY(:LLIB)//')'
                END = END+LLIB+1
              ELSE IF(RECODE.NE.%LOC(CLI$_ABSENT)) THEN
                INCLUDE(END:)=LIBRARY(:LLIB)//','
                END = END+LLIB+1
              END IF
            END DO
            END = END - 1
            LIBS(LEND:) = INCLUDE(:END)
            LEND = LEND + END
          END IF
        ELSEIF(RECODE.EQ.%LOC(RMS$_NMF)) THEN
          POINT = INDEX(LIBRARY,']')
          POINT = INDEX(LIBRARY(POINT+1:),'.')+POINT
          IF(POINT.EQ.0) THEN
            LIBRARY(LLIB+1:)='.OLB'
            LLIB=LLIB+4
          END IF
          RECODE=LIB$SIGNAL(CRN_NOSUCC,%VAL(1),LIBRARY(:LLIB))
          RECODE=LIB$SIGNAL(CRN_NOFILE,%VAL(1),LIBRARY(:LLIB))
        ELSE
          POINT = INDEX(LIBRARY,']')
          POINT = INDEX(LIBRARY(POINT+1:),'.')+POINT
          IF(POINT.EQ.0) THEN
            LIBRARY(LLIB+1:)='.OLB'
            LLIB=LLIB+4
          END IF
          RECODE=LIB$SIGNAL(CRN_NOFILE,%VAL(1),LIBRARY(:LLIB))
        END IF
        GOTO 160
150     CONTINUE
C****   Other
        RECODE=LIB$FIND_FILE(LIBRARY,OUTLIB,CONTEXT,'.OLB',OUTLIB)
        IF(RECODE) THEN
          CALL FTRIM(OLDDEV,OLDDIR,OUTLIB,INIT,IEND)
          LIBS(LEND:) = ','//OUTLIB(INIT:IEND)
          LEND = LEND + IEND - INIT + 1 + 1
          IF(INDEX(OUTLIB,'.OLB').NE.0) THEN
            LIBS(LEND:) = '/LIB'
            LEND = LEND +4
          ELSE
            LIBS(LEND:) = OUTLIB(IEND+1:IEND+4)
            LEND = LEND +4
          END IF
          IF(DEBUG) PRINT *,'LIB$=',LIBS(:LEND)
        ELSE
          POINT = INDEX(LIBRARY,']')
          POINT = INDEX(LIBRARY(POINT+1:),'.')+POINT
          IF(POINT.EQ.0) THEN
            LIBRARY(LLIB+1:)='.OLB'
            LLIB=LLIB+4
          END IF
          IF(RECODE.EQ.%LOC(RMS$_NMF)) THEN
             RECODE=LIB$SIGNAL(CRN_NOSUCC,%VAL(1),LIBRARY(:LLIB))
          END IF
          RECODE=LIB$SIGNAL(CRN_NOFILE,%VAL(1),LIBRARY(:LLIB))
        END IF
C****   End ------------
160     CONTINUE
        IF(DEBUG) PRINT *,'LIB=',LIBRARY(:LLIB),'/',MODE(IFLAG)
          LLIB    = 0
        IF(IKEY.EQ.2) THEN
          IF    (X11 ) THEN
            LIBS(LEND:)=',GRAFX11/LIB,GRAFX11/OPT'
            LEND=LEND+24
          ELSEIF(MOTIF) THEN
            LIBS(LEND:)=',GRAFX11/LIB,GRAFMOTIF/OPT'
            LEND=LEND+26
          ELSEIF(PHIGS) THEN
            LIBS(LEND:)=',GRAFX11/LIB,GPHIGS/OPT'
            LEND=LEND+23
          ELSEIF(DGKS) THEN
            LIBS(LEND:)=',GRAFDGKS/LIB,GRAFDGKS/OPT'
            LEND=LEND+26
          ELSEIF(GTS3D) THEN
            LIBS(LEND:)=',GRAFGKS3D/LIB'
            LEND=LEND+14
            ADDGKS=.TRUE.
          ELSE
            LIBS(LEND:)=',GRAFGKS/LIB'
            LEND=LEND+12
            ADDGKS=.TRUE.
            GKSSHR=CLI$PRESENT('SHAREABLE').NE.%LOC(CLI$_LOCNEG)
          ENDIF
        END IF
        IF(CHLINE.NE.' ') THEN
            LIBRARY=CHLINE
            LLIB   =LENOCC(CHLINE)
            CHLINE =' '
            GOTO 51
        ENDIF
        IF(DONE) THEN
        IF(ADDGKS) THEN
            IF(IKEY.EQ.6) THEN
              LIBS(LEND:)=',NAGLIB/LIB'
              LEND=LEND+11
            ENDIF
            LIBRARY = 'GKS'
            LLIB    = 3
            ADDGKS  = .FALSE.
            GOTO 51
        ENDIF
                  LBFLAG=2
         IF(PACK) LBFLAG=3
         IF(KERN) LBFLAG=4
C    +      LBFLAG=3
C        IF(KERN.OR.(LIBRARY(:LLIB).EQ.'KERNLIB'.AND.CUR.EQ.DEF))
C    +      LBFLAG=4
         IF(CLI$PRESENT('USER')) LBFLAG=4
        END IF
      GOTO 1
200   CONTINUE
      LEND = LEND-1
      IF(LEND.LE.1) GOTO 990
C**** Set LIB$ symbol
      IF(DEBUG) PRINT *,LIBS(2:LEND)
#if !defined(CERNLIB_QMALPH) & !defined(CERNLIB_QCDEC)
C*    Add the C run-time Library if available on standard VMS
      INQUIRE(FILE=CLIB//'.OLB',EXIST=LEXIST)
      IF(LEXIST) THEN
         LIBS(LEND+1:)=','//CLIB//'/LIB'
         LEND=LEND+24
      ENDIF
#endif
      RECODE=LIB$SET_SYMBOL('LIB$',LIBS(2:LEND),LIB$K_CLI_GLOBAL_SYM)
      IF(.NOT.RECODE) RECODE=LIB$SIGNAL(%VAL(RECODE))
      IF(CLI$PRESENT('LOG')) THEN
        RECODE=LIB$PUT_OUTPUT('LIB$ = '//LIBS(2:LEND))
        IF(.NOT.RECODE) RECODE=LIB$SIGNAL(%VAL(RECODE))
      END IF
      CALL UMLOG('CERNLIB',LIBS(2:LEND))
      GOTO 991
 990  CONTINUE
      RECODE=LIB$SIGNAL(CRN_NOLIB,%VAL(0))
 991  CONTINUE
      IF(NEWLIB) THEN
        OPEN(UNIT=1,FILE='CERN:[NEW.DOC]INSTALL.DOC',FORM='FORMATTED',
     +       SHARED,READONLY,STATUS='OLD',IOSTAT=IOS)
        IF(IOS.NE.0) GO TO 995
 994    READ(1,'(A)',END=995) LINE
        RECODE = LIB$SIGNAL(CRN_NEWLIB,%VAL(1),LINE(1:LENOCC(LINE)))
        GO TO 994
 995    CLOSE(1)
      END IF
      END
      CHARACTER*(*) FUNCTION RTDIR(VER,FLG)
      CHARACTER*(*) VER,FLG
      IF(FLG.EQ.'CRN') RTDIR = 'CERN:['//VER(1:LENOCC(VER))//'.LIB]'
C**** Do not resolve GKS library
C     IF(FLG.EQ.'GKS') RTDIR = 'CERN:[GKS.'//VER(1:LENOCC(VER))//'.LIB]'
      IF(FLG.EQ.'GKS') RTDIR = ' '
      IF(FLG.EQ.'NAG') RTDIR = 'NAG_ROOT:[LIB]'
      END
      SUBROUTINE LIBSET(VER,ROOT)
C
      IMPLICIT INTEGER(A-Z)
      INCLUDE '($LNMDEF)'
      PARAMETER(MAXIDX=20)
      CHARACTER*(*) VER
      CHARACTER*8   ROOT
      CHARACTER*255 DIRNAM(0:MAXIDX-1)
      INTEGER*2    ITEML2(80),JTEML2(20)
      INTEGER      ITEML4(40),JTEML4(10)
      EQUIVALENCE  (ITEML2,ITEML4), (JTEML2,JTEML4)
      EXTERNAL SS$_NOLOGNAM, CRN_TOOMALNM, SS$_SUPERSEDE
C
C     RECODE=LIB$DELETE_LOGICAL(ROOT,'LNM$PROCESS_TABLE')
C     IF(.NOT.RECODE.AND.RECODE.NE.%LOC(SS$_NOLOGNAM))
C    +                             RECODE = LIB$SIGNAL(%VAL(RECODE))
C
      ITEML2(1)=4
      ITEML2(2)=LNM$_MAX_INDEX
      ITEML4(2)=%LOC(MINDEX)
      ITEML4(3)=0
      ITEML4(4)=0
      RECODE=SYS$TRNLNM(,'LNM$FILE_DEV',ROOT,,ITEML4)
      IF(RECODE.EQ.%LOC(SS$_NOLOGNAM)) RETURN
      IF(.NOT.RECODE) RECODE = LIB$SIGNAL(%VAL(RECODE))
      IF(MINDEX.GT.MAXIDX)
     +RECODE = LIB$SIGNAL(CRN_TOOMALNM,%VAL(2),%VAL(MINDEX),ROOT)
C
      IOK = -1
      DO NINDEX = 0, MINDEX
         JTEML2(1)=4
         JTEML2(2)=LNM$_INDEX
         JTEML4(2)=%LOC(NINDEX)
         JTEML4(3)=0
         JTEML2(7)=255
         JTEML2(8)=LNM$_STRING
         JTEML4(5)=%LOC(DIRNAM(NINDEX))
         JTEML4(6)=%LOC(DIRLEN)
         JTEML4(7)=0
         RECODE=SYS$TRNLNM(,'LNM$FILE_DEV',ROOT,,JTEML4)
         IF(RECODE.EQ.%LOC(SS$_NOLOGNAM)) RETURN
         IF(.NOT.RECODE) RECODE = LIB$SIGNAL(%VAL(RECODE))
C
         IVLEN= LENOCC(VER)
         IVER = INDEX(DIRNAM(NINDEX)(1:DIRLEN),VER(1:IVLEN))
         ICUR = INDEX(DIRNAM(NINDEX)(1:DIRLEN),'.PRO.')+
     +          INDEX(DIRNAM(NINDEX)(1:DIRLEN),'.OLD.')+
     +          INDEX(DIRNAM(NINDEX)(1:DIRLEN),'.NEW.')+
     +          INDEX(DIRNAM(NINDEX)(1:DIRLEN),'.CNL')
         IF(IVER.NE.0) IOK = IOK + 1
         DIRNAM(NINDEX)(ICUR+1:ICUR+IVLEN)=VER(1:IVLEN)
         DIRNAM(NINDEX)(ICUR+IVLEN+2:ICUR+IVLEN+2)=']'
C
         IPOINT    = (NINDEX + 1) * 3
         ITEML2(IPOINT*2+1) = DIRLEN+IVLEN-3
         ITEML2(IPOINT*2+2) = LNM$_STRING
         ITEML4(IPOINT  +2) = %LOC(DIRNAM(NINDEX))
         ITEML4(IPOINT  +3) = 0
      END DO
      IF(IOK.LT.MINDEX) THEN
        ITEML4(IPOINT  +4) = 0
C
        ITEML2(1) = 4
        ITEML2(2) = LNM$_ATTRIBUTES
        ITEML4(2) = %LOC(LNM$M_CONCEALED)
        ITEML4(3) = 0
C
        RECODE=LIB$SET_LOGICAL(ROOT,,,,ITEML4)
C       IF(.NOT.RECODE) RECODE=LIB$SIGNAL(%VAL(RECODE))
        IF(.NOT.RECODE.AND.RECODE.NE.%LOC(SS$_SUPERSEDE))
     +                             RECODE = LIB$SIGNAL(%VAL(RECODE))
C
      END IF
      END
      SUBROUTINE FTRIM(OLDDEV,OLDDIR,OUTLIB,INIT,IFILEN)
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*(*) OLDDEV,OLDDIR,OUTLIB
C
C     Find NODE + DEVICE
C
      INODIN = 1
      INODEN = INDEX(OUTLIB,'::') + 1
      IF(INODEN.EQ.1) INODEN = 0
      IDEVIN = INODEN + 1
      IDEVEN = INDEX(OUTLIB(IDEVIN:),':') + IDEVIN - 1
      IDIRIN = IDEVEN + 1
      IDIREN = IDEVEN + LASTPO(']',OUTLIB(IDIRIN:),LEN(OUTLIB)-IDIRIN)
      IFILIN = IDIREN + 1
      IFILEN = INDEX(OUTLIB(IFILIN:),'.')+IFILIN-2
      INIT   = IFILIN
      IF(OLDDEV.NE.OUTLIB(INODIN:IDEVEN)) THEN
        OLDDEV = OUTLIB(INODIN:IDEVEN)
        INIT   = INODIN
        IF(OLDDIR.NE.OUTLIB(IDIRIN:IDIREN)) THEN
          OLDDIR = OUTLIB(IDIRIN:IDIREN)
        END IF
      ELSE
        IF(OLDDIR.NE.OUTLIB(IDIRIN:IDIREN)) THEN
          OLDDIR = OUTLIB(IDIRIN:IDIREN)
          INIT   = IDIRIN
        END IF
      END IF
      END
      FUNCTION LASTPO(CHR,STRING,ISTRT)
      CHARACTER*(*) STRING
      CHARACTER*1   CHR
      N=ISTRT
      DO WHILE(STRING(N:N).NE.CHR.AND.N.GT.0)
        N=N-1
      ENDDO
      LASTPO=N
      RETURN
      END
      LOGICAL FUNCTION GKSEXIST(VER)
C
      IMPLICIT INTEGER(A-Z)
      INCLUDE '($LNMDEF)'
      CHARACTER*255 GKSLIB
      CHARACTER*(*) VER
      INTEGER*2    ITEML2(6)
      INTEGER      ITEML4(3)
      EQUIVALENCE  (ITEML2,ITEML4)
      EXTERNAL SS$_NOLOGNAM
 
      ITEML2(1)=255
      ITEML2(2)=LNM$_STRING
      ITEML4(2)=%LOC(GKSLIB)
      ITEML4(3)=%LOC(GKSLEN)
      RECODE=SYS$TRNLNM(,'LNM$DCL_LOGICAL','CERN',,ITEML4)
      GKSEXIST=.FALSE.
      IF(RECODE.EQ.%LOC(SS$_NOLOGNAM)) RETURN
C     IF(.NOT.RECODE) RECODE = LIB$SIGNAL(%VAL(RECODE))
 
      GKSLIB(GKSLEN:)='GKS.'//VER//'.LIB]GKS.OLB'
      INQUIRE(FILE=GKSLIB,EXIST=GKSEXIST)
C
      END
