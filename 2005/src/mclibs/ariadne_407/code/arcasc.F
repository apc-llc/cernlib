*
* $Id: arcasc.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arcasc.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arcasc.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARCASC
 
C...ARiadne subroutine perform dipole CASCade
 
C...Performs a colour dipole cascade on string put in the ariadne
C...event record.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARLIST/ B1SAVE(2),B3SAVE(2),IPTOT(MAXPAR),NPTOT,
     $     IPSTQ(MAXPAR),NPSTQ,IPREM(MAXPAR),NPREM,IRDIR(2),
     $     YIQQ(2),PT2IQQ(2),PT2SAV(2),IRASAV(2),A1SAVE(2),A3SAVE(2)
 
      SAVE /ARLIST/
 
 
C...Calculate total momentum of strings for debugging
      IF (MSTA(9).GT.0) CALL ARCHEM(1)
 
C...Reset counter
      IO=0
      NPTOT=0
 
C...Perform the evolution
      CALL AREVOL(SQRT(PT2LST),0.0)
 
C...Check momentum and dump to /LUJETS/
      IF (.NOT.QDUMP) CALL ARDUMP
      IF (MSTA(9).GT.0) CALL ARCHEM(0)
 
      RETURN
 
C**** END OF ARCASC ****************************************************
      END
