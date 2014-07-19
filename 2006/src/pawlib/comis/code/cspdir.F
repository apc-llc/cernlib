*
* $Id: cspdir.F,v 1.1.1.1 1996/02/26 17:16:19 mclareni Exp $
*
* $Log: cspdir.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:19  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_OLD)
*CMZ :  1.18/00 24/01/94  18.39.09  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPDIR
***-----------------------
*    parsing of comis directives
***--------------------------
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
#include "comis/cskeys.inc"
         CHARACTER NAME*(KLENID), NMFILE*80
         CHARACTER SHKEYS(4)*8
         INTEGER CSLTGP
      INTEGER GSNLAB
      COMMON /CSGSCM/IGSST,JGSST,NGSST,NGSPAR,JGSSB,GSNLAB
****
*     S   ->'EDIT'             DEDIT
*           'FILE'             DFILE  ^LN
*           'LOGFILE'/'LOG'    DLOG   ^LN
*           'MAPFILE'/'MAP'    DMAP   ^LN
*           'LIBRARY'/'LIB'    DLIB   ^LN
*           'EOF'              DEOF
*           'VAX'/'SHELL'      DVAX
*           'FORTRAN'/'FOR'    DFORT
*           'COMIS'            DCOMIS
*           'SHOW'             DSHOW
*           'NOCHECKB'         DNOCHB
*           'CHECKB'           DCHB
*           'NOTRACE'          DNOTR
*           'TRACE'            DTRACE
*           'NOPARAM'          DNOPAR
*           'PARAM'            DPARAM
*           'HELP'             DHELP
*           'REMOVE'           DREMOV
*           'CLEAR'            DCLEAR
*            E
*     LN  -> DLUN     ','/T     DNAME
****
      INTEGER GSCMST(157),GSSTRC(24),GSSTRP(51)
      DATA GSCMST/
     *7,4,1,6,2,0,15,4,3,6,3,8,147,0,27,4,5,5,22,4,7,6,4,8,147,0,39,4,9,
     *5,34,4,11,6,5,8,147,0,51,4,13,5,46,4,15,6,6,8,147,0,57,4,17,6,9,0,
     *67,4,19,5,64,4,21,6,10,0,77,4,23,5,74,4,25,6,11,0,83,4,27,6,12,0,
     *89,4,29,6,19,0,95,4,31,6,13,0,101,4,33,6,14,0,107,4,35,6,15,0,113,
     *4,37,6,16,0,119,4,39,6,17,0,125,4,41,6,18,0,131,4,43,6,20,0,137,4,
     *45,6,21,0,143,4,47,6,22,0,0,3,1,0,0,6,7,4,49,5,155,1,6,8,0/
      DATA GSSTRC/
     *4HEDIT,4HFILE,4HLOGF,4HILEM,4HAPFI,4HLELI,4HBRAR,4HYEOF,4HVAXS,
     *4HHELL,4HFORT,4HRANC,4HOMIS,4HHOWN,4HOCHE,4HCKBN,4HOTRA,4HCENO,
     *4HPARA,4HMHEL,4HPREM,4HOVEC,4HLEAR,4H,   /
      DATA GSSTRP/
     *0,4,4,4,8,7,8,3,15,7,15,3,22,7,22,3,29,3,32,3,35,5,40,7,40,3,47,5,
     *51,4,55,8,57,6,63,7,65,5,70,7,72,5,77,4,81,6,87,5,92,1,-1/
      DATA SHKEYS/
     +    'MEMORY','ROUTINES','COMMONS','NAMES'  /
      DATA KBLN/4H    /
      JGSSB=0
      NGSPAR=0
    1 CALL CSGSCL(GSCMST(1),GSSTRC(1),GSSTRP(1),I)
      GO TO (2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     *21,22,23),GSNLAB
    2 CONTINUE
      IF(IGSST.LE.0)THEN
         GO TO (71,72,73,74),1-IGSST
  71     CALL CSSOUT('CSDIR: syntax error')
         GO TO 77
  72     CALL CSSOUT('CSDIR: no such directive')
         GO TO 77
  73     CALL CSSOUT('CSDIR: channel is reserved')
         GO TO 77
  74     CALL CSSOUT('CSDIR: missing parameter(s)')
         GO TO 77
  77     CONTINUE
      ENDIF
      RETURN
*   DEDIT
    3 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      N=MIDENT(JGSST,NGSST,JID,KLENID)
      J=MJSCHA(NAME)
      NAME=' '
      IF(N.EQ.0)THEN
        CALL CCOPYS(JSMAIN,J,8)
      ELSE
         CALL CCOPYS(JID,J,NCIDEN)
      ENDIF
      CALL CSEDIT(NAME)
      GO TO 1
*   DFILE
    4 CONTINUE
      KEY=2
      GO TO 1
*   DLOG
    5 CONTINUE
      KEY=3
      GO TO 1
*   DMAP
    6 CONTINUE
      KEY=4
      GO TO 1
*   DLIB
    7 CONTINUE
      KEY=5
      GO TO 1
*   DEOF
   10 CONTINUE
      IF(ISTFIL.NE.0)THEN
         CLOSE(LUNFIL)
         ISTFIL=0
      ENDIF
      GO TO 1
*   DVAX
   11 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      REC1=' '
      IF(NGSST.GT.0)THEN
        CALL CCOPYS(JGSST,JSR1,NGSST)
        CALL CSCLI(REC1)
      ENDIF
      GO TO 1
*   DFORT
   12 CONTINUE
      IFORS=1
      GO TO 1
*   DCOMIS
   13 CONTINUE
      IFORS=0
      GO TO 1
*   DNOCHB
   14 CONTINUE
      NCBARR=1
      GO TO 1
*   DCHB
   15 CONTINUE
      NCBARR=0
      GO TO 1
*   DNOTR
   16 CONTINUE
      NTRACE=1
      GO TO 1
*   DTRACE
   17 CONTINUE
      NTRACE=0
      GO TO 1
*   DNOPAR
   18 CONTINUE
      NPARAM=1
      GO TO 1
*   DPARAM
   19 CONTINUE
      NPARAM=0
      GO TO 1
*   DLUN
    8 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      IF(NGSST.EQ.0)THEN
        IGSST=-3
        NGSST=1
        GO TO 1
      ENDIF
      I=MLNUMB(JGSST,NGSST,LUN)
      IF(I.NE.0)THEN
        DO 66 K=1,4
        IF(LUNARR(K).EQ.LUN)THEN
          IF(K.NE.KEY)THEN
             IGSST=-2
             GO TO 1
          ENDIF
        ENDIF
  66    CONTINUE
        IF(ISTARR(KEY).NE.0)THEN
          CLOSE(LUNARR(KEY))
          ISTARR(KEY)=0
        ENDIF
        LUNARR(KEY)=LUN
      ENDIF
      GO TO 1
*   DNAME
    9 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      IF(NGSST.EQ.0)THEN
        IGSST=-3
        NGSST=1
        GO TO 1
      ENDIF
      J=MJSCHA(NMFILE)
      NMFILE=' '
      CALL CCOPYS(JGSST,J,NGSST)
      CALL CSFLCH(NMFILE)
      GO TO (102,103,104,105),KEY-1
 102       CALL CSFILE(NMFILE)
           GO TO 1
 103   CALL CSLOG(NMFILE)
       GO TO 1
 104       CALL CSMAP(NMFILE)
           GO TO 1
 105   CONTINUE
       CALL CSLIB(NMFILE)
       GO TO 1
*   DSHOW
   20 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      IF(NGSST.EQ.0)THEN
        PRINT *,' !show parameters are:'
        PRINT *,' memory, routines, commons, names common_name.'
        PRINT *,' Ex: !show mem   or   !show names common_name '
        NGSST=1
        GO TO 1
      ENDIF
      J=MJSCHA(NMFILE)
      NMFILE=' '
      CALL CCOPYS(JGSST,J,NGSST)
      DO 41 I=2,NGSST
        IF(NMFILE(I:I).EQ.' ')GO TO 42
 41   CONTINUE
      N=NGSST
      GO TO 43
 42   N=I-1
 43   IF(INDEX(SHKEYS(1),NMFILE(1:N)).EQ.1)THEN
        CALL CSSHMU
      ELSEIF(INDEX(SHKEYS(2),NMFILE(1:N)).EQ.1)THEN
        CALL CSPTFS
      ELSEIF(INDEX(SHKEYS(3),NMFILE(1:N)).EQ.1)THEN
        CALL CSPTCL
      ELSEIF(INDEX(SHKEYS(4),NMFILE(1:N)).EQ.1)THEN
        NN=NGSST
        NGSST=NGSST-N
        IF(NGSST.LE.0)THEN
          IGSST=-3
          NGSST=1
          GO TO 1
        ENDIF
        DO 44 I=N+1,NN
          IF(NMFILE(I:I).NE.' ')GO TO 45
 44     CONTINUE
        IGSST=-3
        GO TO 1
 45     CALL CSPTBL(NMFILE(I:NN))
      ELSE
        IGSST=0
      ENDIF
      GO TO 1
*   DHELP
   21 CONTINUE
      CALL CSDIRH
      GO TO 1
*   DREMOV
   22 CONTINUE
      K=MKBLAN(JGSST,NGSST)
      IF(NGSST.EQ.0)THEN
        IGSST=-3
        NGSST=1
      ELSE
        NCIDEN=MIN(KLENID,NGSST)
        NWIDEN=(NCIDEN+3)/4
        IDEN(NWIDEN)=KBLN
        CALL CCOPYS(JGSST,JID,NCIDEN)
        IP=CSLTGP(IPVS)
        IF(IP.EQ.0)THEN
          PRINT *,' no such cs_routine'
        ELSE
          CALL CSDPRO(IP)
        ENDIF
      ENDIF
      GO TO 1
*   DCLEAR
   23 CONTINUE
      CALL CSDCLR
      GO TO 1
      END
#endif
