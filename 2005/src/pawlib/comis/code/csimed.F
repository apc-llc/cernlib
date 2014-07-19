*
* $Id: csimed.F,v 1.1.1.1 1996/02/26 17:16:19 mclareni Exp $
*
* $Log: csimed.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 17/02/94  16.09.33  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSIMED(J,N)
***--------------------------
*     it is local comis editor
***---------------------------
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
        COMMON/CSCONT/LCONT
*
      PARAMETER (MXSLIN=100)
      CHARACTER *80 SOURCE(MXSLIN)
      CHARACTER *32 FILENM, SOLD*80, SNEW*80
      INTEGER CSTINF,CSTFRF
      INTEGER GSNLAB
      COMMON /CSGSCM/IGSST,JGSST,NGSST,NGSPAR,JGSSB,GSNLAB
****
*     *
*     S     -> DINPS                 ^S1
*     S1    ->  PRET        DT       ^S
*              'EDIT'       DSEDT    ^S
*              'S'      DS  RG  DSS  ^S
*              'I'     RG1  DI       ^S
*              'D'      RG  DD       ^S
*              'Q'      DQ
*              'E'      DE
*              'HELP'/'H'   DH       ^S
*              'T'      RG  DT       ^S
*               RG      PRG DT       ^S
*               E
*     RG     -> '-'     INUMB/T      DRM
*               '+'     INUMB/T      DRP
*               'W'     DW
*               'F'     DF    DR1    ^RG2
*               'L'     DL    DR1    ^RG2
*               INUMB   DR1          ^RG2
*               DCUR
*     RG1    -> INUMB/T       DIN
*     RG2    -> ':'/ '/'             ^RG3
*               DRNUM
*     RG3    -> 'F'     DF    DR2
*               'L'     DL    DR2
*                INUMB/T      DR2
****
      INTEGER GSCMST(210),GSSTRC(4),GSSTRP(33)
      DATA GSCMST/
     *0,6,2,8,7,0,15,6,3,6,13,8,1,0,23,4,1,6,24,8,1,0,35,4,3,6,15,7,103,
     *6,16,8,1,0,45,4,5,7,163,6,17,8,1,0,55,4,7,7,103,6,14,8,1,0,61,4,9,
     *6,12,0,67,4,11,6,18,0,79,4,13,5,74,4,15,6,19,8,1,0,89,4,17,7,103,
     *6,13,8,1,0,99,7,103,6,8,6,13,8,1,0,0,3,1,0,114,4,19,6,11,5,111,1,
     *6,4,0,125,4,21,6,11,5,122,1,6,5,0,131,4,23,6,20,0,141,4,25,6,21,6,
     *6,8,172,0,151,4,27,6,22,6,6,8,172,0,159,6,11,6,6,8,172,0,0,6,7,0,
     *0,6,11,5,169,1,6,23,0,182,4,29,5,179,4,31,8,186,0,0,6,10,0,194,4,
     *25,6,21,6,9,0,202,4,27,6,22,6,9,0,0,6,11,5,208,1,6,9,0/
      DATA GSSTRC/
     *4HEDIT,4HSQHE,4HLP-+,4HWF://
      DATA GSSTRP/
     *0,4,4,1,2,1,1,1,5,1,0,1,6,4,6,1,3,1,10,1,11,1,12,1,13,1,8,1,14,1,
     *15,1,-1/
#if !defined(CERNLIB_IBM)
      DATA FILENM/'COMIS.EDT'/
#endif
#if defined(CERNLIB_IBMMVS)
      DATA FILENM/'COMIS.EDT'/
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      DATA FILENM/'COMIS EDT'/
#endif
      JSOLD=MJSCHA(SOLD)
      JSNEW=MJSCHA(SNEW)
      ASSIGN 99 TO LBEG
      GO TO 100
 99   JGSST=J
      NGSST=N
      IPRG=1
      IPRET=0
      IF(NGSST.EQ.0)IPRET=1
      JGSSB=0
        NGSPAR=0
    1 CALL CSGSCL(GSCMST(1),GSSTRC(1),GSSTRP(1),I)
      GO TO (2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     *21,22,23,24,25),GSNLAB
    2 CONTINUE
      IF(IGSST.LE.0)THEN
        GO TO (71,71,72,73,74,75),1-IGSST
        GO TO 77
 71     CALL CSSOUT('MED: syntax error')
        GO TO 77
 72     CALL CSSOUT('ommited delimiter')
        GO TO 77
 73     CALL CSSOUT('[TOP]')
        GO TO 77
 74     CALL CSSOUT('buffer is empty')
        GO TO 77
 75     CALL CSSOUT('[EOB]')
 77     CALL CSSINP(REC,'MED>',NSR)
        IF (NSR .LT. 0) THEN
            CSIMED = 0
            LCONT=0
            RETURN
        END IF
        N=NSR
        J=JSR
        GO TO 99
      ENDIF
      CSIMED=IGSST
      IF(IGSST.EQ.2)CSIMED=0
      LCONT=0
**      IF(CSIMED.EQ.1)THEN
        CALL CSLFRE(JPMB)
        JPMC=0
        DO 333 I=1,NSTRL
          CALL CSPUTL(JPMC,SOURCE(I),LENOCC(SOURCE(I)),0)
          IF(JPMB.EQ.0)JPMB=JPMC
 333    CONTINUE
**      ENDIF
      RETURN
*   DINPS
    3 CONTINUE
      IPRG=1
      IF(NGSST.LE.0) THEN
        IF(IPRET.EQ.0)CALL CSSINP(REC,'MED>',NGSST)
        IF(NGSST .LT. 0) THEN
          CSIMED=0
          LCONT=0
          RETURN
        END IF
        JGSST=JSR
        IPRET=0
        IF(NGSST.EQ.0)IPRET=1
      ENDIF
      GO TO 1
*   PRET
    4 CONTINUE
      IGSST=IPRET
      IPRET=0
      NLB=LINE+1
      NLE=NLB
      GO TO 1
*   DRP
    6 CONTINUE
      NLB=LINE+IABS(NUM)
      NLE=NLB
      GO TO 1
*   DRM
    5 CONTINUE
      NLB=LINE-IABS(NUM)
      NLE=NLB
      GO TO 1
*   DW
   21 CONTINUE
      NLB=1
      NLE=NSTRL
      GO TO 1
*   DF
   22 CONTINUE
      NUM=1
      GO TO 1
*   DL
   23 CONTINUE
      NUM=NLINES
      GO TO 1
*   DR1
    7 CONTINUE
      NLB=NUM
      GO TO 1
*   DCUR
    8 CONTINUE
      IPRG=0
      NLB=LINE
      NLE=LINE
      GO TO 1
*   PRG
    9 CONTINUE
      IGSST=IPRG
      GO TO 1
*   DR2
   10 CONTINUE
      IF(NUM.NE.-1)THEN
        NLE=NUM
      ELSE
        NLE=LINE
      ENDIF
      GO TO 1
*   DRNUM
   11 CONTINUE
      NLE=NLB
      GO TO 1
*   INUMB
   12 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      IGSST=MLNUMB(JGSST,NGSST,NUM)
      IF(IGSST.EQ.0)NUM=-1
      GO TO 1
*   DT
   14 CONTINUE
      KEY=2
      GO TO 101
*   DD
   15 CONTINUE
      KEY=3
      GO TO 101
*   DS
   16 CONTINUE
      KDIL=MKCHAR(JGSST)
      LOLD=0
      JGSST=JGSST+1
      JOB=JGSST
      NGSST=NGSST-1
      IF(NGSST.LE.0)GO TO 702
*                   --------
 112  IF(MKCHAR (JGSST).EQ.KDIL)GO TO 113
      JGSST=JGSST+1
      NGSST=NGSST-1
      IF(NGSST.LE.0)GO TO 702
*                   ---------
      LOLD=LOLD+1
      GO TO 112
 113  JGSST=JGSST+1
      LNEW=0
      NGSST=NGSST-1
        JNB=JGSST
      IF(NGSST.LE.0)GO TO 702
*                    --------
  114 IF(MKCHAR(JGSST).EQ.KDIL) GO TO 115
      JGSST=JGSST+1
      NGSST=NGSST-1
      IF(NGSST.LE.0) GO TO 702
*                   ----------
      LNEW=LNEW+1
      GO TO 114
  115 JGSST=JGSST+1
      NGSST=NGSST-1
      SOLD=' '
      SNEW=' '
      CALL CCOPYS(JOB,JSOLD,LOLD)
      CALL CCOPYS(JNB,JSNEW,LNEW)
      GO TO 1
*   DSS
   17 CONTINUE
      KEY=1
      NSUBST=0
      GO TO 101
*   DI
   18 CONTINUE
      KEY=4
      GO TO 101
*   DIN
   24 CONTINUE
      IF(NUM.NE.-1)THEN
        NLB=NUM
        NLE=NUM
      ELSE
        NLB=LINE
        NLE=LINE
      ENDIF
      GO TO 1
*   DE
   19 CONTINUE
      IGSST=1
      GO TO 1
*   DQ
   13 CONTINUE
      IGSST=2
      GO TO 1
*   DH
   20 CONTINUE
      CALL CSMHEL
      GO TO 1
  101 IF(NLB.LT.0) GO TO 703
*                  ---------
      IF(NLB.GT.NLINES) GO TO 705
*                       ---------
      NLE=MIN0(NLINES,NLE)
      NLE=MAX0(NLE,1)
      IF(KEY.EQ.4)THEN
         NLE=NLB
      ELSE
         IF(NLINES.LE.0)GOTO 704
*                       --------
         IF(NLB.EQ.0)GOTO 703
*                    ---------
      ENDIF
      IF(NLB.GT.NLE)THEN
         ISTEP=-1
      ELSE
         ISTEP=1
      ENDIF
      GO TO(81,82,83,84),KEY
*            S, T, D, I
*SUBSTITUTION
   81 CONTINUE
      DO 601 LINE=NLB,NLE,ISTEP
        I=INDEX(SOURCE(LINE),SOLD(1:LOLD))
        IF(I.GT.0)THEN
          IF(I.EQ.1)THEN
            REC1=SNEW(1:LNEW) // SOURCE(LINE)(I+LOLD:)
          ELSE
            REC1=SOURCE(LINE)(1:I-1) // SNEW(1:LNEW)
     +         //SOURCE(LINE)(I+LOLD:)
          ENDIF
          SOURCE(LINE)=REC1
          NSUBST=NSUBST+1
        ENDIF
 601  CONTINUE
      GO TO 88
* TYPE
   82 CONTINUE
      DO 602 LINE=NLB,NLE,ISTEP
  201 JS=MJSCHA(SOURCE(LINE))
      NB=MNBLAN(JS,80)
      WRITE(REC1(1:4),'(I3,1X)')LINE
      REC1(5:)=SOURCE(LINE)(1:NB)
      CALL CSSOUT(REC1(1:NB+4))
  602 CONTINUE
      GO TO 88
* KILL
   83 CONTINUE
      IF(ISTEP.GT.0)THEN
        JJ=NLB
        K=NLE+1
        L=NLE-NLB+1
      ELSE
        JJ=NLE
        K=NLB+1
        L=NLB-NLE+1
      ENDIF
      DO 603 I=K,NSTRL
        SOURCE(JJ)=SOURCE(I)
  603   JJ=JJ+1
      NLINES=NLINES-L
      NSTRL=NSTRL-L
      GO TO 88
*   KEY=I
  84  KDIL=MKCHAR(JGSST)
      IF(NGSST.LE.0) GO TO 702
      JGSST=JGSST+1
      NGSST=NGSST-1
      ILINE=0
 1170 JB=JGSST
      NB=0
  117 IF(NGSST.LE.0)GO TO 118
      IF(MKCHAR(JGSST).EQ.KDIL)GO TO 120
      JGSST=JGSST+1
      NGSST=NGSST-1
      NB=NB+1
      GO TO 117
  118 ASSIGN 119 TO LINS
      GO TO 401
  119 CALL CSSINP(REC,'MEDI',NGSST)
      JGSST=JSR
      GO TO 1170
  120 JGSST=JGSST+1
      NGSST=NGSST-1
      ASSIGN 88 TO LINS
*      GO TO 401
* INSERT
  401 ILINE=ILINE+1
      LINE=NLB+ILINE
      REC1=' '
      IF(NB.GT.0)CALL CCOPYS(JB,JSR1,NB)
      IF(NSTRL.GE.MXSLIN)THEN
        PRINT *,' CSIMED: Too long source'
        ILINE=ILINE-1
        GO TO 88
      ENDIF
      DO 604 I=NSTRL,LINE,-1
 604  SOURCE(I+1)=SOURCE(I)
      SOURCE(LINE)=REC1
      NLINES=NLINES+1
      NSTRL=NSTRL+1
      GO TO LINS
 88   LINE=NLE
      IF(KEY.EQ.3)THEN
         IF(NLB.LE.NLE)LINE=NLB
         IF(NLB.GT.NLE)LINE=NLE
      ELSEIF(KEY.EQ.4)THEN
         LINE=NLB+ILINE
      ELSEIF(KEY.EQ.1)THEN
         PRINT *,NSUBST,' substitutions'
**         NN=4
**         JJ=JSR1
**         CALL CBIS(NSUBST,JJ,NN)
**         IF(N.GT.0)CALL CSSETC(JJ,NN,KBLN)
**         REC1(5:)='substitutions'
**         CALL CSSOUT(REC1(1:20))
      ENDIF
      GO TO 1
*   DSEDT
   25 CONTINUE
      I=CSTINF(FILENM)
      IF(I.EQ.0)GO TO 1
      CALL CSSEDT(FILENM)
      IF(FILENM(1:1).EQ.'@')THEN
        FILENM(1:1)='C'
        GO TO 1
      ENDIF
      I=CSTFRF(FILENM)
      ASSIGN 501 TO LBEG
      GO TO 100
 501  CALL CSSINP(REC,'MED>',NSR)
      JGSST=JSR
      NGSST=NSR
      IPRG=1
      IPRET=0
      IF(NGSST.EQ.0)IPRET=1
      GO TO 1
 100  LCONT=1
      I=JPMB
      NSTRL=0
 1001 IF(I.EQ.0)GO TO 102
      NSTRL=NSTRL+1
      IF(NSTRL.GE.MXSLIN)THEN
        PRINT *,' CSIMED: Too long source'
        NSTRL=NSTRL-1
        GO TO 705
      ENDIF
      CALL CSGETL(I,SOURCE(NSTRL),NC,MARK)
      GO TO 1001
 102  NLINES=NSTRL
      LINE=NLINES
      GO TO LBEG
*  701 IGSST=-1
*      GO TO 1
  702 IGSST=-2
      GO TO 1
  703 IGSST=-3
      GO TO 1
  704  IGSST=-4
      GO TO 1
  705  IGSST=-5
      GO TO 1
      END
