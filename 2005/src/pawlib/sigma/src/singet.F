*
* $Id: singet.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: singet.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/00 26/07/93  16.22.13  by  Carlo E. Vandoni
*-- Author :
         SUBROUTINE SINGET(ISI,N,DIM)
#include "sigma/sicsig.inc"
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
C         PURPOSE
C            TO RETURN INFORMATION ABOUT THE ITEM IN STACK POSITION N
C             SET COMM. VARIABLES
C
C         USAGE
C            IT=NGET(N,DIM)
C            IT=NGET2(N,DIM)
C            ENTRY NGET2 USED FOR ITEMS WITH NO OF ARGUMENTS GE 2
C
C         DESCRIPTION OF PARAMETERS
C            N - POSITION IN STACK(USUAL CONVENTIONS)
C            DIM - THE DIMENSION VECTOR.
C
C         COMM. BLOCKS USED
C            COM1
C
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            SILSKK
C            SISTAK
C            SISTRI
C
C
C         AUTHOR.   C.E.VANDONI          DATE 04/01/74
C    ..................................................
C
      DIMENSION DIM(10)
      CALL SITRAC(' SINGET')
18    CALL SILSKK(N,INAME,CNAME)
      ITYPE=0
      ISI=0
      ISTRI=0
*      IF(DIM(1).EQ.0.0) GOTO 100
      DO 20 I=1,10
      DIM(I)=1.
20    CONTINUE
100   CONTINUE
*      IF(INAME(5).EQ.1) CALL SISTRI
      CALL SIGECD
      IF(SITRAK(40).EQ.1)PRINT 991,ITYPE
991   FORMAT(' ITYPE IN SINGET AFTER SIGECD ',I4)
C   GET SETS ALL COMM. BLOCK PARAMETERS
      IF(KIT.GT.0) PRINT *,CNAME,' IS UNDEFINED'
      IF(KIT.GT.0) CALL SINERR(9)
       IF(KIT.GT.0) RETURN
       IADDR=-(KIT)+4
       NDIM=IQ(IADDR-1)
*       IF(DIM(1).EQ.0.0) GOTO 300
       DIM(1)=IQ(IADDR-4)
       DIM(2)=IQ(IADDR-3)
       DIM(3)=IQ(IADDR-2)
       IF(NDIM.EQ.3) GOTO 300
       DIM(3)=1.0
       IF(NDIM.EQ.1)DIM(2)=1.0
300    CONTINUE
       LENGTH=IQ(IADDR-5)
      ITYPE=2
      IF(KIT.LT.0)  ISI=2
      IF(ISI.EQ.0) CALL SINERR(9)
 999  END
