*
* $Id: sigtt2.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: sigtt2.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  12.14.45  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGTT2(INDEX,LENGT,N,DIM)
C
C    ....................................................
C         PURPOSE
C            ALLOCATE TEMPORARY STORAGE AREA AND ASSIGN
C            A TEMPORARY $-NAME FOR A TEMP RESULT (ARRAY)
C
C         USAGE
C            CALL SIGTT2(INDEX,LENGT,N,DIM)
C         DESCRIPTION OF PARAMETERS
C            LENGT .. LENGTH OF REQUIRED TEMP.
C                     (TOTAL LENGTH OF REQUIR. AREA)
C            N ...... NUMBER OF DIMENSIONS OF ARRAY
C            DIM .... DIMENSION VECTOR (ONLY FIRST N LOOKED AT)
C
C         COMM. BLOCKS USED
C            COM1
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SITRAC
C            SILSTK
C            SIMSTK
C            SIUPNA
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
C
      CHARACTER*1  TYPE
C
      DIMENSION DIM(10),LL(6)
      INTEGER IDIM(10)
      CHARACTER CL*8
      CALL SITRAC(' SIGTT2   ')
      TYPE='R'
      CALL SIUPNA
      ITYPE=2
      IF(MODE.EQ.2)TYPE='C'
      IDIM(1)=DIM(1)
      IDIM(2)=1
      IDIM(3)=1
      IF(N.GE.2)IDIM(2)=DIM(2)
      IF(N.EQ.3)IDIM(3)=DIM(3)
*
      LETO=IDIM(1)*IDIM(2)*IDIM(3)
      IF(LENGT.LT.LETO) GOTO 10
      CALL KUVCRE(CNAME(1:8),TYPE,IDIM,LLOW,LHIGH)
      IF(LLOW.EQ.0)GOTO 10
******************************************
      INDEX=LLOW
      CL=CNAME
      CALL SILSTK(LL,CL)
      CALL SIMSTK(INDEX,N)
      GO TO 999
   10 CALL SINERR(19)
C
  999 END
