*
* $Id: readin.F,v 1.1.1.1 1996/01/11 14:14:41 mclareni Exp $
*
* $Log: readin.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:41  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE READIN(IFL)
C     **********************
C-- ADAPTED FROM ISAJET
C          READ IN USER DATA
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/bopar.inc"
#include "cojets/cutoff.inc"
#include "cojets/data1.inc"
#include "cojets/data2.inc"
#include "cojets/data3.inc"
#include "cojets/davers.inc"
#include "cojets/decpar.inc"
#include "cojets/eicht.inc"
#include "cojets/event.inc"
#include "cojets/evint.inc"
#include "cojets/evtype.inc"
#include "cojets/forcsl.inc"
#include "cojets/forgen.inc"
#include "cojets/fstate.inc"
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/kdump.inc"
#include "cojets/keybre.inc"
#include "cojets/keys.inc"
#include "cojets/kfact.inc"
#include "cojets/khadro.inc"
#include "cojets/mb.inc"
#include "cojets/nflav.inc"
#include "cojets/nleave.inc"
#include "cojets/par.inc"
#include "cojets/stable.inc"
#include "cojets/tleave.inc"
#include "cojets/total.inc"
#include "cojets/weakbo.inc"
#include "cojets/zevel.inc"
      CHARACTER*8 TTL(10),WORD,BLANK,DUMMY
      CHARACTER*5 HFLAV
      CHARACTER*2 HFLAV2
      CHARACTER*3 LEPT
      CHARACTER*2 B1,B2
      CHARACTER*8 STRUC
      LOGICAL NOPI0,NOETA,NOEVOL
      DIMENSION LOC(100),MDEC(6)
      DATA BLANK/'        '/
      DATA LOC/100*0/
C
C          ENTRY
      KFORFL=0
      NTIMES=0
      VERSN=IDVER/100.
      WRITE(ITLIS,10) VERSN,DAVERS
10    FORMAT('1',2(/T2,80('*'))
     1/T2,23('*'),'  COJETS VERSION',F6.2,1X,A9,'  ',23('*')
     2,2(/T2,80('*')))
      WRITE(ITLIS,11)
   11 FORMAT(//' USER COMMANDS'/1X,13('='))
      WRITE(ITLIS,88)
88    FORMAT(' ')
C
C          READ TITLE
      READ(ITCOM,1) TTL
    1 FORMAT(10A8)
      WRITE(ITLIS,2) TTL
    2 FORMAT(' ',10A8)
      WRITE(ITLIS,88)
C
C          READ ENERGY AND NO. OF EVENTS
      READ(ITCOM,*) ECM,NLEAVE,NDUMP,NJUMP
      WRITE(ITLIS,*) ECM,NLEAVE,NDUMP,NJUMP
      WRITE(ITLIS,88)
      NJUMP=MAX(NJUMP,1)
      NDUMP=NDUMP*NJUMP
C
C          READ REACTION AND SET IKEY
C
      READ(ITCOM,3) REAC
    3 FORMAT(A8)
      WRITE(ITLIS,4) REAC
   4  FORMAT(1X,A8)
C
      IF(REAC.EQ.'TWOJET  ') THEN
        IEVTYP=1
        IKEY=1
      ELSEIF(REAC.EQ.'E+E-    ') THEN
        WRITE(ITLIS,41) REAC
41      FORMAT(/' ***',A8,' NOT ACTIVATED -- JOB TERMINATED')
        IKEY=2
        STOP
      ELSEIF(REAC.EQ.'DRELLYAN') THEN
        IKEY=3
      ELSEIF(REAC.EQ.'MINBIAS ') THEN
        IEVTYP=1
        IKEY=4
      ELSEIF(REAC.EQ.'SUPERSYM'.OR.REAC.EQ.'SUSY    ') THEN
        WRITE(ITLIS,41) REAC
        IKEY=5
        STOP
      ELSEIF(REAC.EQ.'WPAIR   ') THEN
        WRITE(ITLIS,41) REAC
        IKEY=6
        STOP
      ELSE
        WRITE(ITLIS,70)
70      FORMAT(//' ***YOU FORGOT TO SELECT A PROCESS FOR GENERATION.'
     *    /' -- AVAILABLE AT PRESENT ARE: ',
     *    /'TWOJET, DRELLYAN, MINBIAS')
        STOP
      ENDIF
C
C
   21 CONTINUE
      NSEL=0
C
C          READ KEYWORD. FOR EACH RECOGNIZED KEYWORD READ CORRESPONDING
C          VARIABLES AND SET LOC FLAG.
C
      NSEL=0
100   CONTINUE
      WRITE(ITLIS,88)
      READ(ITCOM,3) WORD
      WRITE(ITLIS,4) WORD
      NSEL=NSEL+1
C          KEYWORD STOP
      IF(WORD.EQ.'STOP    ') THEN
        GO TO 101
C          KEYWORD
C-- MAX LOC = 64
C-- ============
C
C-- LIMITS
C-- ======
      ELSEIF(WORD.EQ.'PTFFF   ') THEN
        READ(ITCOM,*)  PTMGE,PTMAX
        WRITE(ITLIS,*) PTMGE,PTMAX
        IF(REAC.NE.'TWOJET  ') THEN
          WRITE(ITLIS,51)
51        FORMAT(/' ***SELECTED REACTION IS NOT TWOJET'
     *    ,' -- THIS COMMAND WILL BE IGNORED')
          GO TO 100
        ENDIF
        PTMAXX=ECM*.45
        IF(PTMAX.GT.PTMAXX) WRITE(ITLIS,43) PTMAXX
43      FORMAT(/' ***PTFFFX TOO LARGE FOR ECM -- TRUNCATED AT PTFFFX = '
     *  ,E12.5)
        IF(PTMAX.EQ.0.) PTMAX=PTMAXX
        PTMAX=MIN(PTMAX,REAL(PTMAXX))
        IF(PTMGE.GT.PTMAX) THEN
          WRITE(ITLIS,42)
42        FORMAT(/' ***INVALID RANGE FOR PTFFF'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(1)=NSEL
      ELSEIF(WORD.EQ.'DYGAMASS') THEN
        READ(ITCOM,*)  BOMSMN,BOMSMX
        WRITE(ITLIS,*) BOMSMN,BOMSMX
        IF(REAC.NE.'DRELLYAN') THEN
          WRITE(ITLIS,47)
          GO TO 100
        ENDIF
        BOXX=ECM*.8
        IF(BOMSMX.GT.BOXX) WRITE(ITLIS,45) BOXX
45      FORMAT(/' ***UPPER LIMIT FOR DYMASS TOO LARGE FOR ECM'
     *  ,' -- TRUNCATED AT ',E12.5)
        IF(BOMSMN.GE.BOMSMX) THEN
          WRITE(ITLIS,44)
44        FORMAT(/' ***INVALID RANGE FOR DYMASS'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(ABS(BOMSMN-BOMSMX)/BOMSMN.GT.BRATIO) WRITE(ITLIS,46) BRATIO
46      FORMAT(/' ***WARNING: RELATIVE DYMASS RANGE WIDER THAN ',F6.2)
        LOC(2)=NSEL
C
C-- DRELLYAN
C-- ========
      ELSEIF(WORD.EQ.'WTYPE   ') THEN
        IF(LOC(3).NE.0) THEN
          READ(ITCOM,*)  DUMMY
          WRITE(ITLIS,*) DUMMY
          WRITE(ITLIS,86)
86        FORMAT(/' ***ONLY ONE WTYPE COMMAND IS ALLOWED --'
     *   ,' THIS COMMAND WILL BE IGNORED')
          GO TO 100
        ENDIF
        READ(ITCOM,*)  WTYPE
        WRITE(ITLIS,*) WTYPE
        IF(REAC.NE.'DRELLYAN') THEN
          WRITE(ITLIS,47)
47        FORMAT(/' ***SELECTED REACTION IS NOT DRELLYAN'
     *    ,' -- THIS COMMAND WILL BE IGNORED')
          GO TO 100
        ELSEIF(WTYPE.EQ.'GM') THEN
          IEVTYP=4
        ELSEIF(WTYPE.EQ.'Z ') THEN
          IEVTYP=2
        ELSEIF(WTYPE.EQ.'W ') THEN
          IEVTYP=3
        ELSE
          WRITE(ITLIS,48)
48        FORMAT(/' ***INCORRECT INPUT FOR WTYPE (ALLOWED INPUTS: '
     *    ,13H'GM','Z','W'),' -- DRELLYAN JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(3)=NSEL
      ELSEIF(WORD.EQ.'WDECMODE') THEN
        IF(LOC(4).NE.0) THEN
          READ(ITCOM,*)  LL
          WRITE(ITLIS,*) LL
          WRITE(ITLIS,83)
83        FORMAT(/' ***ONLY ONE WDECMODE COMMAND IS ALLOWED --'
     *    ,' THIS COMMAND WILL BE IGNORED')
          GO TO 100
        ENDIF
        READ(ITCOM,*)  IDECBO
        WRITE(ITLIS,*) IDECBO
        IF(REAC.NE.'DRELLYAN') THEN
          WRITE(ITLIS,47)
          GO TO 100
        ENDIF
        LOC(4)=NSEL
      ELSEIF(WORD.EQ.'WZOPTION') THEN
        READ(ITCOM,*)  KOPTWZ
        WRITE(ITLIS,*) KOPTWZ
        IF(REAC.NE.'DRELLYAN') THEN
          WRITE(ITLIS,47)
          GO TO 100
        ENDIF
        IF(KOPTWZ.NE.0.AND.KOPTWZ.NE.1) THEN
        WRITE(ITLIS,35)
35      FORMAT(/' ***UNACCEPTABLE VALUE'
     *  ,' (0: PRODUCTION WITHOUT BREIT-WIGNER;'
     *  ,' 1: PRODUCTION WITH BREIT-WIGNER'
     *  /' ***JOB WILL BE ABORTED')
        IFL=1
        ENDIF
        LOC(61)=NSEL
      ELSEIF(WORD.EQ.'KYWQED  ') THEN
        READ(ITCOM,*)  LEPRAD
        WRITE(ITLIS,*) LEPRAD
        IF(REAC.NE.'DRELLYAN') THEN
          WRITE(ITLIS,47)
          GO TO 100
        ENDIF
        IF(LEPRAD.NE.0.AND.LEPRAD.NE.1.AND.LEPRAD.NE.2) THEN
        WRITE(ITLIS,49)
49      FORMAT(/' ***UNACCEPTABLE VALUE (0: NO QED RADIATION;'
     *  ,' 1: O(ALFA) CORRECTED LPA; 2: PURE O(ALFA)'
     *  /' ***JOB WILL BE ABORTED')
        IFL=1
        ENDIF
        LOC(5)=NSEL
      ELSEIF(WORD.EQ.'MASSW   ') THEN
        READ(ITCOM,*)  PMAS(3)
        WRITE(ITLIS,*) PMAS(3)
      PMAS(4)=PMAS(3)
        WMASSM=10.
        IF(PMAS(3).LT.WMASSM) THEN
          WRITE(ITLIS,50) WMASSM
50        FORMAT(/' ***MASS OF W+ AND W- SET LOWER THAN ',E12.5
     *    ,' -- TOO LOW, JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(PMAS(3).GT.ECM-10.) THEN
          WRITE(ITLIS,85)
85        FORMAT(/' ***MASS OF W+ AND W- SET TOO HIGH FOR ECM'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(6)=NSEL
      ELSEIF(WORD.EQ.'MASSZ   ') THEN
        READ(ITCOM,*)  PMAS(2)
        WRITE(ITLIS,*) PMAS(2)
        WMASSM=10.
        IF(PMAS(2).LT.WMASSM) THEN
          WRITE(ITLIS,84) WMASSM
84        FORMAT(/' ***MASS OF Z0 SET LOWER THAN ',E12.5
     *    ,' -- TOO LOW, JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(PMAS(2).GT.ECM-10.) THEN
          WRITE(ITLIS,82)
82        FORMAT(/' ***MASS OF Z0 SET TOO HIGH FOR ECM'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(7)=NSEL
      ELSEIF(WORD.EQ.'DELTAR  ') THEN
        READ(ITCOM,*)  DELTAR
        WRITE(ITLIS,*) DELTAR
        IF(DELTAR.GE.1.) THEN
          WRITE(ITLIS,36)
36        FORMAT(/' ***DELTAR SET TOO HIGH, MUST BE DELTAR < 1.'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(62)=NSEL
      ELSEIF(WORD.EQ.'WZMINMAS') THEN
        READ(ITCOM,*)  WZTHRS
        WRITE(ITLIS,*) WZTHRS
        WMASSM=10.
        IF(WZTHRS.LT.WMASSM) THEN
          WRITE(ITLIS,34) WMASSM
34        FORMAT(/' ***WZMINMAS SET LOWER THAN ',E12.5
     *    ,' -- TOO LOW, JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(WZTHRS.GT.ECM-10.) THEN
          WRITE(ITLIS,32)
32        FORMAT(/' ***WZMINMAS SET TOO HIGH FOR ECM'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(63)=NSEL
C
C-- SUBPROCESSES
C-- ============
      ELSEIF(WORD.EQ.'HEAVYQRK') THEN
        READ(ITCOM,*)  HFLAV
        WRITE(ITLIS,*) HFLAV
        IF(REAC.NE.'TWOJET  ') THEN
          WRITE(ITLIS,51)
          GO TO 100
        ENDIF
        IF(LOC(9).GT.0.OR.LOC(10).GT.0.OR.LOC(11).GT.0
     *  .OR.LOC(57).GT.0) THEN
          WRITE(ITLIS,52)
52        FORMAT(/' ***MULTIPLE HEAVYQRK, FUSION, FLAVEX, GSPLIT'
     *    ,' COMMANDS',' -- THIS COMMAND WILL BE IGNORED')
          GO TO 100
        ENDIF
        DO 120 L=4,6
        IF(HFLAV.NE.CHA4(L,1)) GO TO 120
        HFLAV2=HFLAV
        CALL HEAVYQ(HFLAV2)
        KFORFL=L
        GO TO 121
120     CONTINUE
        WRITE(ITLIS,53)
53      FORMAT(/' ***UNACCEPTABLE INPUT (ALLOWED INPUTS ARE '
     *  ,14H'CH','BT','TP',' -- JOB WILL BE ABORTED')
        IFL=1
121     CONTINUE
        LOC(9)=NSEL
      ELSEIF(WORD.EQ.'FUSION  ') THEN
        READ(ITCOM,*)  HFLAV
        WRITE(ITLIS,*) HFLAV
        IF(REAC.NE.'TWOJET  ') THEN
          WRITE(ITLIS,51)
          GO TO 100
        ENDIF
        IF(LOC(9).GT.0.OR.LOC(10).GT.0.OR.LOC(11).GT.0
     *  .OR.LOC(57).GT.0) THEN
          WRITE(ITLIS,52)
          GO TO 100
        ENDIF
        DO 122 L=4,6
        IF(HFLAV.NE.CHA4(L,1)) GO TO 122
        HFLAV2=HFLAV
        CALL FUSION(HFLAV2)
        KFORFL=L
        GO TO 123
122     CONTINUE
        WRITE(ITLIS,53)
        IFL=1
123     CONTINUE
        LOC(10)=NSEL
      ELSEIF(WORD.EQ.'FLAVEX  ') THEN
        READ(ITCOM,*)  HFLAV
        WRITE(ITLIS,*) HFLAV
        IF(REAC.NE.'TWOJET  ') THEN
          WRITE(ITLIS,51)
          GO TO 100
        ENDIF
        IF(LOC(9).GT.0.OR.LOC(10).GT.0.OR.LOC(11).GT.0
     *  .OR.LOC(57).GT.0) THEN
          WRITE(ITLIS,52)
          GO TO 100
        ENDIF
        DO 124 L=4,6
        IF(HFLAV.NE.CHA4(L,1)) GO TO 124
        HFLAV2=HFLAV
        CALL FLAVEX(HFLAV2)
        KFORFL=L
        GO TO 125
124     CONTINUE
        WRITE(ITLIS,53)
        IFL=1
125     CONTINUE
        LOC(11)=NSEL
      ELSEIF(WORD.EQ.'GSPLIT  ') THEN
        READ(ITCOM,*)  HFLAV
        WRITE(ITLIS,*) HFLAV
        IF(REAC.NE.'TWOJET  ') THEN
          WRITE(ITLIS,51)
          GO TO 100
        ENDIF
        IF(LOC(9).GT.0.OR.LOC(10).GT.0.OR.LOC(11).GT.0
     *  .OR.LOC(57).GT.0) THEN
          WRITE(ITLIS,52)
          GO TO 100
        ENDIF
        DO 126 L=4,6
        IF(HFLAV.NE.CHA4(L,1)) GO TO 126
        HFLAV2=HFLAV
        CALL GSPLIT(HFLAV2)
        KFORFL=L
        GO TO 127
126     CONTINUE
        WRITE(ITLIS,53)
        IFL=1
127     CONTINUE
        LOC(57)=NSEL
C
C-- PARTON AND PARTICLE DECAYS
C-- ==========================
      ELSEIF(WORD.EQ.'KYEVOLVE') THEN
        READ(ITCOM,*)  KEYBRE
        WRITE(ITLIS,*) KEYBRE
        IF(KEYBRE.LT.0.OR.KEYBRE.GT.3) THEN
          WRITE(ITLIS,54)
54        FORMAT(/' ***BAD INPUT (ACCEPTED VALUES: 0,1,2,3)'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(12)=NSEL
      ELSEIF(WORD.EQ.'KYTOPQCD') THEN
        READ(ITCOM,*)  KYTQCD
        WRITE(ITLIS,*) KYTQCD
        IF(KYTQCD.NE.0.AND.KYTQCD.NE.1.AND.KYTQCD.NE.3) THEN
          WRITE(ITLIS,154)
154       FORMAT(/' ***BAD INPUT (ACCEPTED VALUES: 0,1,3)'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(53)=NSEL
      ELSEIF(WORD.EQ.'KYTOPQED') THEN
        READ(ITCOM,*)  KYTQED
        WRITE(ITLIS,*) KYTQED
        IF(KYTQED.NE.0.AND.KYTQED.NE.1) THEN
          WRITE(ITLIS,149)
149       FORMAT(/' ***UNACCEPTABLE VALUE (0: NO QED RADIATION;'
     *    ,' 1: MULTIPLE QED RADIATION WITH LLA)'
     *    /' ***JOB WILL BE ABORTED')
        IFL=1
        ENDIF
        LOC(54)=NSEL
      ELSEIF(WORD.EQ.'NOEVOLVE') THEN
        READ(ITCOM,*)  NOEVOL
        WRITE(ITLIS,*) NOEVOL
        IF(NOEVOL) KEYBRE=3
        LOC(13)=NSEL
      ELSEIF(WORD.EQ.'NOFRGMNT') THEN
        READ(ITCOM,*)  NOFRGM
        WRITE(ITLIS,*) NOFRGM
        IF(NOFRGM) KHADRO=0
        LOC(14)=NSEL
      ELSEIF(WORD.EQ.'FRAGTHOM') THEN
        READ(ITCOM,*)  FRGTHO,FTHOMP,ETHOMP
        WRITE(ITLIS,*) FRGTHO,FTHOMP,ETHOMP
        LOC(53)=NSEL
      ELSEIF(WORD.EQ.'STOPDECY') THEN
        READ(ITCOM,*)  MDEC1
        WRITE(ITLIS,*) MDEC1
        INTP=INTID(MDEC1)
        IF(INTP.GT.0) THEN
          RDECAY(INTP)=0.
          NSTOPD=NSTOPD+1
          KSTOPD(NSTOPD)=MDEC1
        ELSE
        WRITE(ITLIS,55)
55      FORMAT(/' ***BAD INPUT -- LOOK AT PARTICLE ID'
     *  ,' IN PARTICLE TABLE -- JOB WILL BE ABORTED')
        IFL=1
        ENDIF
        LOC(15)=NSEL
      ELSEIF(WORD.EQ.'NOPI0   ') THEN
        READ(ITCOM,*)  NOPI0
        WRITE(ITLIS,*) NOPI0
        IF(NOPI0) THEN
        RDECAY(51)=0.
          NSTOPD=NSTOPD+1
          KSTOPD(NSTOPD)=IDENTF(51)
        ENDIF
        LOC(16)=NSEL
      ELSEIF(WORD.EQ.'NOETA   ') THEN
        READ(ITCOM,*)  NOETA
        WRITE(ITLIS,*) NOETA
        IF(NOETA) THEN
          RDECAY(52)=0.
          NSTOPD=NSTOPD+1
          KSTOPD(NSTOPD)=IDENTF(52)
        ENDIF
        LOC(17)=NSEL
      ELSEIF(WORD.EQ.'NODECAY ') THEN
        READ(ITCOM,*)  NODECY
        WRITE(ITLIS,*) NODECY
        IF(NODECY) THEN
        DO 56 IL=5,352
56      RDECAY(IL)=0.
        ENDIF
        LOC(18)=NSEL
      ELSEIF(WORD.EQ.'FORCE   ') THEN
        DO 98 L=1,6
98      MDEC(L)=0
        READ(ITCOM,*)  MDEC
        WRITE(ITLIS,*) MDEC
        IF(LOC(19).EQ.0) NFORCE=0
        IF(NFORCE.LT.100) THEN
          NFORCE=NFORCE+1
          DO 57 L=1,6
57        KFORCE(L,NFORCE)=MDEC(L)
        ELSE
          WRITE(ITLIS,58)
58        FORMAT(/' ***FORCE USED MORE THAN 100 TIMES'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(19)=NSEL
      ELSEIF(WORD.EQ.'FORCESL ') THEN
        IF(LOC(20).GT.0) THEN
          WRITE(ITLIS,59)
59        FORMAT(/' ***FORCESL USED MORE THAN ONCE'
     *    ,' -- JOB WILL BE ABORTED')
          READ(ITCOM,*)  DUMMY,DUMMY,NTIMES
          WRITE(ITLIS,*) DUMMY,' ',DUMMY,NTIMES
          IFL=1
          GO TO 100
        ENDIF
        NTIMES=0
        READ(ITCOM,*)  HFLAV2,LEPT,NTIMES
        WRITE(ITLIS,*) HFLAV2,' ',LEPT,NTIMES
        IF(HFLAV2.EQ.'CH') THEN
          KFORSL=4
        ELSEIF(HFLAV2.EQ.'BT') THEN
          KFORSL=5
        ELSEIF(HFLAV2.EQ.'TP') THEN
          KFORSL=6
        ELSE
          WRITE(ITLIS,92)
92        FORMAT(/' ***BAD 1ST ENTRY FOR FORCESL (ALLOWED INPUTS ARE '
     *    ,14H'CH','BT','TP',' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(IEVTYP.EQ.1.AND.KFORSL.NE.KFORFL) THEN
          WRITE(ITLIS,60)
60        FORMAT(/' ***WITH TWOJET, FORCESL CAN BE USED ONLY WITH HEAVY'
     *    ,' FLAVOR FORCING OF THE SAME TYPE AS IN THE FORCESL CARD'
     *    /' --- JOB WILL BE ABORTED')
          IFL=1
          GO TO 100
        ENDIF
        IF(IEVTYP.EQ.2.AND.KFORSL+10.NE.IDECBO) THEN
          WRITE(ITLIS,91)
91        FORMAT(/' ***WITH Z DRELLYAN, FORCESL CAN BE USED ONLY WITH'
     *    ,' A FLAVOR COMPATIBLE WITH THE Z DECAY CHANNEL FORCED'
     *    /' --- JOB WILL BE ABORTED')
          IFL=1
          GO TO 100
        ENDIF
        IF(IEVTYP.EQ.3.AND.((KFORSL.EQ.4.AND.IDECBO.NE.5).OR.
     *  (KFORSL.GE.5.AND.IDECBO.NE.6))) THEN
          WRITE(ITLIS,93)
93        FORMAT(/' ***WITH W DRELLYAN, FORCESL CAN BE USED ONLY WITH'
     *    ,' A FLAVOR COMPATIBLE WITH THE W DECAY CHANNEL FORCED'
     *    /' --- JOB WILL BE ABORTED')
          IFL=1
          GO TO 100
        ENDIF
        IF(IEVTYP.EQ.4) THEN
          WRITE(ITLIS,94)
94        FORMAT(/' ***FORCESL CANNOT BE USED WITH GM DRELLYAN'
     *    ,' (WHAT FOR ?)'
     *    /' --- JOB WILL BE ABORTED')
          IFL=1
          GO TO 100
        ENDIF
        IF(LEPT.EQ.'E  ') THEN
          LFORSL=IDENTF(8)
        ELSEIF(LEPT.EQ.'MU ') THEN
          LFORSL=IDENTF(12)
        ELSEIF(LEPT.EQ.'TAU') THEN
          LFORSL=IDENTF(16)
        ELSE
          WRITE(ITLIS,61)
61        FORMAT(/' ***BAD 2ND ENTRY FOR FORCESL'
     *    ,' -- JOB WILL BE ABORTED')
        ENDIF
        IF(NTIMES.GT.2) THEN
          WRITE(ITLIS,76)
76        FORMAT(/' ***NO. OF FORCING TIMES .GT.2'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
          GO TO 100
        ENDIF
        IF(IEVTYP.EQ.3.AND.NTIMES.GT.1) THEN
          WRITE(ITLIS,95)
95        FORMAT(/' ***WITH W DRELLYAN, NO. OF FORCING TIMES'
     *    ,' CAN BE ONLY 1 -- JOB WILL BE ABORTED')
          IFL=1
          GO TO 100
        ENDIF
        IF(NTIMES.EQ.0) NTIMES=1
        NFORSL=NTIMES
        LOC(20)=NSEL
C
C-- OTHER
C-- =====
      ELSEIF(WORD.EQ.'BEAMS   ') THEN
        READ(ITCOM,*)  B1,B2
        WRITE(ITLIS,*) B1,B2
        IF((B1.EQ.'AP'.AND.B2.EQ.'P').OR.(B2.EQ.'AP'.AND.B1.EQ.'P'))
     *  THEN
          IPBAR=1
        ELSEIF(B1.EQ.'P'.AND.B2.EQ.'P') THEN
          IPBAR=0
        ELSE
          WRITE(ITLIS,62)
62        FORMAT(/' ***ONLY ',8H'AP','P',' AND ',7H'P','P',' ACCEPTED'
     *    ,' AS BEAM PAIRS -- JOB ABORTED')
          IFL=1
        ENDIF
        LOC(21)=NSEL
      ELSEIF(WORD.EQ.'STRUC   ') THEN
        READ(ITCOM,*)  STRUC
        WRITE(ITLIS,*) STRUC
        IF(STRUC.EQ.'NA3     ') THEN
          IEICHT=0
        ELSEIF(STRUC.EQ.'EICHTEN1') THEN
          IEICHT=1
        ELSEIF(STRUC.EQ.'EICHTEN2') THEN
          IEICHT=2
        ELSE
          WRITE(ITLIS,63)
63        FORMAT(/' ***BAD INPUT (ONLY ACCEPTED: '
     *    ,28H'EICHTEN1','EICHTEN2','NA3')
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(22)=NSEL
      ELSEIF(WORD.EQ.'TLEAVE  ') THEN
        READ(ITCOM,*)  TLEAVE
        WRITE(ITLIS,*) TLEAVE
        LOC(23)=NSEL
      ELSEIF(WORD.EQ.'NSIGMA  ') THEN
        READ(ITCOM,*)  NSIGMA
        WRITE(ITLIS,*) NSIGMA
        LOC(56)=NSEL
      ELSEIF(WORD.EQ.'TOPFORM ') THEN
        READ(ITCOM,*)  KTPFRM
        WRITE(ITLIS,*) KTPFRM
        IF(KTPFRM.NE.0.AND.KTPFRM.NE.1) THEN
        WRITE(ITLIS,129)
129     FORMAT(/' ***UNACCEPTABLE VALUE (0: DECAY PARTON HISTORY;'
     *  ,' 1: DECAY HISTORY AS IN ISAJET (NO DECAY PARTONS))'
     *  /' ***JOB WILL BE ABORTED')
        IFL=1
        ENDIF
        LOC(64)=NSEL
C
C-- CONSTANTS
C-- =========
      ELSEIF(WORD.EQ.'LAMBDA  ') THEN
        READ(ITCOM,*)  ALAMB
        WRITE(ITLIS,*) ALAMB
        LOC(25)=NSEL
      ELSEIF(WORD.EQ.'QZEVOL  ') THEN
        READ(ITCOM,*)  QZEV
        WRITE(ITLIS,*) QZEV
        LOC(58)=NSEL
      ELSEIF(WORD.EQ.'CUTJET  ') THEN
        READ(ITCOM,*)  QZ
        WRITE(ITLIS,*) QZ
        LOC(26)=NSEL
      ELSEIF(WORD.EQ.'SIN2W   ') THEN
        READ(ITCOM,*)  S2THW
        WRITE(ITLIS,*) S2THW
        LOC(27)=NSEL
      ELSEIF(WORD.EQ.'TOPMAS  ') THEN
        READ(ITCOM,*)  QMAS(6)
        WRITE(ITLIS,*) QMAS(6)
        IF(QMAS(6).LT.16.) THEN
          WRITE(ITLIS,155)
155       FORMAT(/' ***BAD INPUT (MINIMUM ACCEPTED'
     *    ,' VALUE FOR TOPMAS IS 16.)'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(28)=NSEL
      ELSEIF(WORD.EQ.'SIGTINEL') THEN
        READ(ITCOM,*)  SIGINL
        WRITE(ITLIS,*) SIGINL
        LOC(29)=NSEL
      ELSEIF(WORD.EQ.'KFACTOR ') THEN
        READ(ITCOM,*)  FACTK
        WRITE(ITLIS,*) FACTK
        LOC(30)=NSEL
      ELSEIF(WORD.EQ.'NFLAVOR ') THEN
        READ(ITCOM,*)  NFLAV
        WRITE(ITLIS,*) NFLAV
        IF(NFLAV.LT.3.OR.NFLAV.GT.6) THEN
          WRITE(ITLIS,64)
64        FORMAT(/' ***BAD INPUT (ACCEPTED RANGE: 3 - 6)'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(31)=NSEL
      ELSEIF(WORD.EQ.'PTCUTOFF') THEN
        READ(ITCOM,*)  PTMIN
        WRITE(ITLIS,*) PTMIN
        LOC(32)=NSEL
      ELSEIF(WORD.EQ.'PTSQINTR') THEN
        READ(ITCOM,*)  PT2INT
        WRITE(ITLIS,*) PT2INT
        LOC(33)=NSEL
C
C-- JET FRAGMENTATION
C-- =================
      ELSEIF(WORD.EQ.'SIGQTQ  ') THEN
        READ(ITCOM,*)  SIGMAQ
        WRITE(ITLIS,*) SIGMAQ
        LOC(34)=NSEL
      ELSEIF(WORD.EQ.'SIGQTG  ') THEN
        READ(ITCOM,*)  SIGMAG
        WRITE(ITLIS,*) SIGMAG
        LOC(59)=NSEL
      ELSEIF(WORD.EQ.'LONGEXPQ') THEN
        READ(ITCOM,*)  CX2Q
        WRITE(ITLIS,*) CX2Q
        LOC(35)=NSEL
      ELSEIF(WORD.EQ.'LONGEXPG') THEN
        READ(ITCOM,*)  CX2G
        WRITE(ITLIS,*) CX2G
        LOC(60)=NSEL
      ELSEIF(WORD.EQ.'PUD     ') THEN
        READ(ITCOM,*)  PUD
        WRITE(ITLIS,*) PUD
        LOC(36)=NSEL
      ELSEIF(WORD.EQ.'PS1-6   ') THEN
        READ(ITCOM,*)  PS1
        WRITE(ITLIS,*) PS1
        IFLPS1=0
        DO 65 L=1,6
        IF(PS1(L).GE.0..AND.PS1(L).LE.1.) GO TO 65
        IFLPS1=1
65      CONTINUE
        IF(IFLPS1.EQ.1) THEN
          WRITE(ITLIS,66)
66        FORMAT(/' ***BAD RANGE (ACCEPTED 0. - 1. FOR EACH ENTRY)'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(37)=NSEL
      ELSEIF(WORD.EQ.'PISOPN   ') THEN
        READ(ITCOM,*)  PISOPN
        WRITE(ITLIS,*) PISOPN
        IF(PISOPN.LT.0..OR.PISOPN.GT.1.) THEN
          WRITE(ITLIS,67)
67        FORMAT(/' ***BAD RANGE (ACCEPTED 0. - 1.)'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        LOC(38)=NSEL
      ELSEIF(WORD.EQ.'PBARYO  ') THEN
        READ(ITCOM,*)  PBARYO
        WRITE(ITLIS,*) PBARYO
        IF(PBARYO.LT.0..OR.PBARYO.GT.1.) THEN
          WRITE(ITLIS,67)
          IFL=1
        ENDIF
        LOC(39)=NSEL
      ELSEIF(WORD.EQ.'EPSPET-3') THEN
        READ(ITCOM,*)  EPSPET
        WRITE(ITLIS,*) EPSPET
        IFLEPS=0
        DO 68 L=1,3
        IF(EPSPET(L).GE.0..AND.EPSPET(L).LE.1.) GO TO 68
        IFLEPS=1
68      CONTINUE
        IF(IFLEPS.EQ.1) THEN
          WRITE(ITLIS,66)
          IFL=1
        ENDIF
        LOC(40)=NSEL
      ELSEIF(WORD.EQ.'CMIX-12 ') THEN
        READ(ITCOM,*)  CMIX
        WRITE(ITLIS,*) CMIX
        IFLCMX=0
        DO 69 L1=1,6
        DO 69 L2=1,6
        IF(CMIX(L1,L2).GE.0..AND.CMIX(L1,L2).LE.1.) GO TO 69
        IFLCMX=1
69      CONTINUE
        IF(IFLCMX.EQ.1) THEN
          WRITE(ITLIS,66)
          IFL=1
        ENDIF
        LOC(41)=NSEL
      ELSEIF(WORD.EQ.'BZDMIX  ') THEN
        READ(ITCOM,*)  BZDMIX
        WRITE(ITLIS,*) BZDMIX
        IF(BZDMIX.LT.0..OR.BZDMIX.GT.1.) THEN
          WRITE(ITLIS,67)
          IFL=1
        ENDIF
        LOC(42)=NSEL
      ELSEIF(WORD.EQ.'BZSMIX  ') THEN
        READ(ITCOM,*)  BZSMIX
        WRITE(ITLIS,*) BZSMIX
        IF(BZSMIX.LT.0..OR.BZSMIX.GT.1.) THEN
          WRITE(ITLIS,67)
          IFL=1
        ENDIF
        LOC(43)=NSEL
      ELSEIF(WORD.EQ.'CND1    ') THEN
        READ(ITCOM,*)  CND1
        WRITE(ITLIS,*) CND1
        LOC(44)=NSEL
      ELSEIF(WORD.EQ.'CND2    ') THEN
        READ(ITCOM,*)  CND2
        WRITE(ITLIS,*) CND2
        LOC(45)=NSEL
C
C-- BEAM JETS
C-- =========
      ELSEIF(WORD.EQ.'CLPRO   ') THEN
        READ(ITCOM,*)  CLPRO
        WRITE(ITLIS,*) CLPRO
        LOC(46)=NSEL
      ELSEIF(WORD.EQ.'PT2PRO  ') THEN
        READ(ITCOM,*)  PT2PRO
        WRITE(ITLIS,*) PT2PRO
        LOC(47)=NSEL
      ELSEIF(WORD.EQ.'FPTMB   ') THEN
        READ(ITCOM,*)  FPTMB
        WRITE(ITLIS,*) FPTMB
        LOC(48)=NSEL
      ELSEIF(WORD.EQ.'FMULMB  ') THEN
        READ(ITCOM,*)  FMULMB
        WRITE(ITLIS,*) FMULMB
        LOC(49)=NSEL
      ELSEIF(WORD.EQ.'RETPIZ  ') THEN
        READ(ITCOM,*)  RETPIZ
        WRITE(ITLIS,*) RETPIZ
        LOC(50)=NSEL
      ELSEIF(WORD.EQ.'RPAIR-9 ') THEN
        READ(ITCOM,*)  RPAIR
        WRITE(ITLIS,*) RPAIR
        LOC(51)=NSEL
C          NONE OF THE ABOVE
      ELSE
        WRITE(ITLIS,71)
71      FORMAT(//' ***NOT A RECOGNIZABLE PARAMETER'
     *  ,' -- JOB WILL BE ABORTED')
        IFL=1
      ENDIF
C
      GO TO 100
C
C-- FINAL CHECKS FOR ERRORS
101   CONTINUE
      IF(LOC(2).GT.0.AND.IEVTYP.NE.4) THEN
        WRITE(ITLIS,72)
72      FORMAT(/' ***SELECTED PROCESS IS NOT QED DRELLYAN'
     *  ,' -- DYGAMASS COMMAND WILL BE IGNORED')
      ENDIF
      IF(LOC(4).NE.0) THEN
        IF(IEVTYP.EQ.2.AND.IDECBO.GT.12) THEN
          WRITE(ITLIS,73)
73        FORMAT(/' ***BAD WDECMODE INPUT -- ACCEPTED RANGE FOR Z'
     *    ,' DRELLYAN PRODUCTION IS 1 - 12'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ELSEIF(IEVTYP.EQ.3.AND.IDECBO.GT.6) THEN
          WRITE(ITLIS,74)
74        FORMAT(/' ***BAD WDECMODE INPUT -- ACCEPTED RANGE FOR W'
     *    ,' DRELLYAN PRODUCTION IS 1 - 6'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ELSEIF(IEVTYP.EQ.4.AND.(IDECBO.LT.1.OR.IDECBO.GT.3)) THEN
          WRITE(ITLIS,75)
75        FORMAT(/' ***BAD WDECMODE INPUT -- ACCEPTED RANGE FOR QED'
     *    ,' DRELLYAN PRODUCTION IS 1 - 3'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
      ENDIF
      IF(LOC(4).NE.0.AND.KOPTWZ.EQ.0) THEN
        IF(IEVTYP.EQ.2.AND.IDECBO.EQ.12) THEN
          DIFF=PMAS(2)-2.*QMAS(6)-2.
          IF(DIFF.LE.0) THEN
            KOPTWZ=1
            WRITE(ITLIS,96)
96          FORMAT(/' ***WZOPTION HAS BEEN CHANGED TO 1 BECAUSE'
     *      ,' MASS(Z) < 2*MASS(TOP) + 2 ')
          ENDIF
        ENDIF
        IF(IEVTYP.EQ.3.AND.IDECBO.EQ.6) THEN
          DIFF=PMAS(3)-QMAS(6)-QMAS(5)-2.
          IF(DIFF.LE.0) THEN
            KOPTWZ=1
            WRITE(ITLIS,97)
97          FORMAT(/' ***WZOPTION HAS BEEN CHANGED TO 1 BECAUSE'
     *      ,' MASS(W) < MASS(TOP) + MASS(BOTTOM) + 2 ')
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
