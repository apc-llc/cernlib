*
* $Id: cdi_mess.F,v 1.1.1.1 1996/02/28 16:23:41 mclareni Exp $
*
* $Log: cdi_mess.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDI_MESS(MESS,NUMB)
*     ------------------------------
#if defined(CERNLIB_COMMENT)
*********************************************************
*     It is a nice subroutine which takes the           *
*     message text from MESS, finds one or more         *
*     zero ( 0 or 00 or 000000 etc.) and replaces       *
*     this substring with encoded integer NUMB.         *
*     Then CDX_MESS is called: output on LU=6           *
      and on DB protocol file (if requested).           *
*     The same with @s, but then the field will         *
*     be left filled with zeros (not blancs).           *
*     Examples: MESS      NUMB      output              *
*                                                       *
*            'A= 000cm'     12    'A=  12cm'            *
*            'A= @@@cm'     12    'A= 012cm'            *
*            'A= 000cm'   1234    'A= 1234cm'           *
*            'A=0cm'      1234    'A=1234cm'            *
*            'A0=0cm'     1234    'A1234=0cm'           *
*            'A`0=0cm'    1234    'A0=1234cm'           *
*            'N=0000*'      12    'N=  12*'             *
*            'N=000`*'      12    'N=12  *'             *
*                                                       *
*     Attention to double using of "`" (last examples): *
              a) `0 protects "0" from substitution;     *
*             b) 000` , the number will be left shifted *
*********************************************************
#endif
*
*                            Author: Boris Khomenko
*
      IMPLICIT NONE
*
*     -- arguments --
      CHARACTER*(*) MESS
      INTEGER       NUMB
      REAL          RNUM
*
*     -- externals --
      INTEGER  JSEARC , JSEANC
      INTEGER  LNBLNK
*
*     -- local variables --
      INTEGER  NNN
      REAL     RRR
      EQUIVALENCE(NNN,RRR)
*
      INTEGER  IFI , IHX , IL , LS , LM , IP
      CHARACTER MESL*120 , CODE*10 , CH*1
      INTEGER  JJ , LL , J1 , J2 , J , L
      INTEGER  LN , LZ , LR
      CHARACTER SAVL*120
      INTEGER   LSAV
*
      DATA LSAV/0/
*
*     - init integer number attribures
      NNN=NUMB
      IFI=1
*
   10 CONTINUE
*
      IHX=0
      IL=0
*
*     - JJ -copied part of MESS, LL -filled part of MESL
      JJ=0
      LL=0
*
      IF(LSAV.GT.0) THEN
        MESL=SAVL(:LSAV)
        LL=LSAV
        LSAV=0
      ENDIF
*
   11 CONTINUE
*     - search for a "0" or "@"
      J1=JSEARC(MESS(JJ+1:),'0@')+JJ
      IF(J1.GT.JJ) THEN
*       - "0" (or "@") is found, but is it a "`0" ?
        CH=MESS(J1:J1)
        IF(J1.GT.1.AND.MESS(J1-1:J1-1).EQ.'`') THEN
*         - yes, it is a "`0", copy the scanned part of MESS but "`"
          L=J1-2-JJ
          IF(L.GT.0) THEN
            MESL(LL+1:)=MESS(JJ+1:JJ+L)
            JJ=JJ+L
            LL=LL+L
          ENDIF
          JJ=J1
          LL=LL+1
          MESL(LL:LL)=CH
          GOTO 11
        ENDIF
*
*       - copy the previous text if any
        L=J1-1-JJ
        IF(L.GT.0) THEN
          MESL(LL+1:)=MESS(JJ+1:JJ+L)
          JJ=JJ+L
          LL=LL+L
        ENDIF
*
*       - length of the number code (starting at J1)
        LN=JSEANC(MESS(J1:),'0@')-1
        IF(LN.LE.0) LN=LEN(MESS)-J1+1
*
*       - point JJ to "the after number's char"
*         and check on "."(if float), "`" and "~"
        JJ=J1+LN
*
        IF(IFI.EQ.0) THEN
          IF(MESS(JJ:JJ).EQ.'.') THEN
            IP=1
            LN=LN+1
            IF(MESS(JJ:JJ+1).EQ.'..') THEN
              IP=0
              LM=JSEANC(MESS(JJ+1:),'.')-1
              LM=LM+JSEANC(MESS(JJ+1+LM:),'0@')-1
            ELSE
              LM=JSEANC(MESS(JJ+1:),'0@')-1
            ENDIF
            LN=LN+LM
            JJ=J1+LN
            LM=MIN0(LM,9)
          ELSE
            IP=0
            LM=9
          ENDIF
*
          LS=0
          DO J=J1,J1+LN-1
            IF(MESS(J:J).EQ.'@') LS=LS+1
          ENDDO
          IF(LS.EQ.1) LS=4
          IF(LS.LE.0) LS=6
*
        ENDIF
*
        IF(MESS(JJ:JJ).EQ.'`') THEN
          IL=1
          IP=0
          LN=LN+1
          IF(IFI.EQ.0.AND.LM.LT.9) LM=LM+1
        ELSE IF(MESS(JJ:JJ).EQ.'~') THEN
          LN=LN+1
          IHX=1
        ELSE
          JJ=JJ-1
        ENDIF
*       - now JJ is pointed to the last number's char
*
        IF(IHX.NE.0) THEN
          CALL HEXENC(NNN,LN,CODE(:LN),LR)
        ELSE IF(IFI.NE.0) THEN
          LZ=1
          IF(CH.EQ.'@') LZ=LN
          CALL ENCODI(NNN,-LZ,CODE,LR)
        ELSE
          L=LN
          IF(L.EQ.1) L=LEN(CODE)
          IF(IP.NE.0) THEN
            CALL ENCODR(RRR,IP*100+LS*10+LM,CODE(:L),LR)
            LR=L
          ELSE
            CALL ENCODR(RRR,-(LS*10+LM),CODE(:L),LR)
          ENDIF
        ENDIF
        IF(LR.GE.LN) THEN
          MESL(LL+1:)=CODE
          LL=LL+LR
        ELSE IF(IL.GT.0) THEN
          MESL(LL+1:)=CODE
          LL=LL+LN
        ELSE
          MESL(LL+1+LN-LR:)=CODE
          LL=LL+LN
        ENDIF
      ENDIF
      IF(JJ.LT.LEN(MESS).AND.LL.LT.LEN(MESL)) MESL(LL+1:)=MESS(JJ+1:)
*
   66 CONTINUE
      LL=MAX0(LNBLNK(MESL),2)
      IF(MESL(LL-1:LL).EQ.'.>') THEN
        LL=LL-2
        IF(LL.GT.0) THEN
          SAVL=MESL(:LL)
          LSAV=LL
        ENDIF
      ELSE
        CALL CDX_MESS(MESL(:LNBLNK(MESL)))
      ENDIF
      RETURN
*
*
      ENTRY CDR_MESS(MESS,RNUM)
*     -------------------------
*
      IFI=0
      RRR=RNUM
      GOTO 10
*
*
      ENTRY CDN_MESS(MESS)
*     --------------------
*
      IF(LSAV.GT.0) THEN
        MESL=SAVL(:LSAV)
        LL=LSAV
        LSAV=0
      ELSE
        LL=0
      ENDIF
*
      MESL(LL+1:)=MESS
      GOTO 66
*
      END
