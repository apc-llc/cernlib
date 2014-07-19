*
* $Id: getbyt.F,v 1.1.1.1 1996/02/15 17:47:50 mclareni Exp $
*
* $Log: getbyt.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:50  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_FORTRAN)||defined(CERNLIB_VAX)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_CRAY)||defined(CERNLIB_PDP)||defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)||defined(CERNLIB_NORD)
      SUBROUTINE GETBYT(CHAR,IBEG,NBITS,INT)
C
C CERN PROGLIB# M439    GETBYT          .VERSION GEN      2.09  831122
C ORIG. MODIFIED UPKCH FROM KERNFOR 4.07
C
      DIMENSION CHAR(9)
      INTEGER   CHAR
C
C
#if defined(CERNLIB_DOUBLE)
      DATA NBITSW / 32 /
#endif
#if defined(CERNLIB_PDP)||defined(CERNLIB_UNIVAC)
      DATA NBITSW / 36 /
#endif
#if defined(CERNLIB_CDC)
      DATA NBITSW / 60 /
#endif
#if (defined(CERNLIB_SINGLE))&&(!defined(CERNLIB_CDC))
      DATA NBITSW / 64 /
#endif
C
C
      IF (NBITS.LE.0) GO TO 58
      IGNOR = IBEG - 1
      NZONE= IGNOR + NBITS
      JCH = 1
      IF (NZONE.NE.NBITSW)  GO TO 41
C
C----              ZONE = WORD
C
C*UL   21 JGO  = NBITSW+1 - IGNOR
      JGO  = NBITSW+1 - IGNOR
      NBEND= NBITS + 1
      NBEND= MAX (NBEND,JGO)
C
C*UL   22 JBT = JGO
      JBT = JGO
C
C*UL   24 JBT = JBT - NBITS
      JBT = JBT - NBITS
      INT= JBYT (CHAR(JCH),JBT,NBITS)
      GO TO 58
C
C----              ZONE NOT EQUALS A WORD
C
   41 JBT   = NBITSW - IGNOR
      NBEND = NZONE+1 - NBITS
      NBEND = MIN (NBEND,IGNOR+NBITS)
C
C*UL   42 NBDONE= IGNOR
      NBDONE= IGNOR
   43 IF (JBT.GE.0)  GO TO 44
      JBT= JBT + NBITSW
      JCH= JCH + 1
      GO TO 43
C
   44 JBT = JBT - NBITS
      IF (JBT.LT.0)  GO TO 51
      INT= JBYT (CHAR(JCH),JBT+1,NBITS)
      GO TO 58
C
C--                CHARACTER ACROSS WORD BOUNDARY
   51 NLEFT= NBITS + JBT
      NRIGH= -JBT
      JBT  = JBT + NBITSW
      INT= JBYT (CHAR(JCH+1),JBT+1,NRIGH)
      IF (NLEFT.EQ.0)  GO TO 54
      CALL CBYT (CHAR(JCH),1,INT,NRIGH+1,NLEFT)
   54 JCH= JCH + 1
C
   58 RETURN
      END
#endif
