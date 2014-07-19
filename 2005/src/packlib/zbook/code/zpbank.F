*
* $Id: zpbank.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zpbank.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZPBANK(IZ,JPBANK,NFROM,NTO)
*
* ROUTINE TO PRINT ONE ZBOOK BANK ACCORDING TO FORMAT
*
* author HG       7/4/84
*
* INPUT
* IZ         STRUCTURE
* JPBANK     BANK POINTER
* NFROM      FIRST WORD IN BANK TO PRINT
* NTO        LAST WORD IN BANK TO PRINT
*
      PARAMETER (MLINK=6,MFBANK=4,MPMOD=10,MINL=3)
      EQUIVALENCE (IOUT,OUT)
      DIMENSION IZ(*)
      CHARACTER SLINE*120,SNAME*4,SBUFF*120
      DIMENSION IW(53)
*
      LOGICAL ZIDOK
*
      IF(ZIDOK(IZ,JPBANK)) GO TO 10
      CALL ZERROR(IZ,300,'ZPBANK',JPBANK)
      RETURN
*
   10 LOUT=IZ(IZ(1)+4)
      LENGTH=IZ(JPBANK)-2
      NUP=MIN(LENGTH,NTO)
      NAME=IZ(JPBANK+LENGTH+1)
      CALL ZNAMSR(IZ,NAME,IST,LAST)
      IF(IST.LE.0)  THEN
*--- NO FORMAT FOR THIS BANK STORED - USE FLOATING
         NW=2
         IW(1)=0
         IW(2)=31000
      ELSE
         CALL ZGTFOR(IZ,IST,NW,IW)
         DO 20 I=1,NW
            IF (IW(I).EQ.0) GOTO 30
   20    CONTINUE
*--- NO INFINITE REPETITION GIVEN - PRINT REST IN FLOATING
         IW(NW+1)=0
         IW(NW+2)=31000
         NW=NW+2
      ENDIF
   30 CONTINUE
      SLINE=' '
      ICPT=0
      ICNT=0
      IPNT=0
      IH=0
      IFNT=0
      JCNT=0
   40 IFNT=IFNT+1
      IF (IFNT.GT.NW) GOTO 110
      IF(IW(IFNT).LT.10000)  THEN
*--- REPETITION COUNT
         IF (IFNT.EQ.NW) GOTO 110
         IREP=IW(IFNT)
         IF (IREP.EQ.0) IREP=10000000
         IL1=IFNT+1
         DO 50 I=IL1,NW
            IF (IW(I).EQ.50000) GOTO 60
   50    CONTINUE
         I=NW+1
   60    CONTINUE
         IL2=I-1
         IFNT=I
      ELSE
         IREP=1
         IL1=IFNT
         IL2=IFNT
      ENDIF
   70 CONTINUE
      DO 100 I=IL1,IL2
         ICODE=IW(I)/10000
         DO 90 J=1,IW(I)-10000*ICODE
            IF (IPNT.GE.NUP) GOTO 110
            IPNT=IPNT+1
            IF (IPNT.LT.NFROM) GOTO 90
            IF(ICPT.EQ.0)  LEFT=IPNT
            IOUT=IZ(JPBANK+IPNT)
            IF (ICODE.EQ.4)  THEN
               CALL UHTOC(IOUT,4,SNAME,4)
               SLINE(ICPT+5:ICPT+8)=SNAME
            ELSEIF (ICODE.EQ.1)  THEN
               WRITE (SLINE(ICPT+1:),'(Z11)') IOUT
            ELSEIF (ICODE.EQ.2)  THEN
               WRITE (SLINE(ICPT+1:),'(I12)') IOUT
            ELSEIF (ICODE.EQ.3)  THEN
               WRITE (SLINE(ICPT+1:),'(G12.4)') OUT
            ENDIF
            ICNT=ICNT+1
            IF (ICNT.EQ.MPMOD)  THEN
               IF(JCNT.EQ.0) THEN
*--- first line
                  LSTART=LEFT
                  SBUFF=SLINE
                  JCNT=1
               ELSEIF(SBUFF.EQ.SLINE) THEN
                  JCNT=JCNT+1
                  LEFTP=LEFT
               ELSE
                  IF(JCNT.GE.MINL) THEN
                     WRITE(LOUT,10000) LSTART
                     WRITE(LOUT,10010) LEFTP,SBUFF
                  ELSE
                     LF=LSTART-MPMOD
                     DO 80 JJ=1,JCNT
                        LF=LF+MPMOD
                        WRITE(LOUT,10010) LF,SBUFF
   80                CONTINUE
                  ENDIF
                  SBUFF=SLINE
                  JCNT=1
                  LSTART=LEFT
               ENDIF
               SLINE=' '
               ICPT=0
               ICNT=0
            ELSE
               ICPT=ICPT+12
            ENDIF
   90    CONTINUE
  100 CONTINUE
      IREP=IREP-1
      IF (IREP.EQ.0) GOTO 40
      GOTO 70
  110 CONTINUE
      IF(JCNT.GE.MINL)  THEN
         WRITE(LOUT,10000) LSTART
         WRITE(LOUT,10010) LEFTP,SBUFF
      ELSEIF(JCNT.GT.0)  THEN
         LF=LSTART-MPMOD
         DO 120 JJ=1,JCNT
            LF=LF+MPMOD
            WRITE(LOUT,10010) LF,SBUFF
  120    CONTINUE
      ENDIF
      IF(ICNT.NE.0)  THEN
         WRITE (LOUT,10010) LEFT,SLINE
      ENDIF
10000 FORMAT(1X,I5,': ',5('===='), ' and following lines identical to:')
 
10010 FORMAT(1X,I5,':',1X,A120)
  999 END
