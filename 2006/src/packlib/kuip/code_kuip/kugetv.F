*
* $Id: kugetv.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kugetv.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.64/07 20/09/90  11.37.39  by
*-- Author :
      SUBROUTINE KUGETV(VNAME,LLOW,LHIGH)
*
********************************************************************************
*
* Get existing vector from decoded command line
* If the vector is not existing then LLOW=LHIGH=0
*
* The vector VNAME can be accessed by Q(LLOW:LHIGH) if ITYPE=1,
* or IQ(LLOW:LHIGH) if ITYPE=2
*
* It returns also in IQUEST :
*
* IQUEST(10) : NCHNAM (number of chars of VNAME)
* IQUEST(11) : LENTOT (total number of elements of vector)
* IQUEST(12) : ILOW   (low index)
* IQUEST(13) : IHIGH  (high index)
* IQUEST(14) : ITYPE  (type: 1=real, 2=integer, 3=complex)
* IQUEST(15) : IVEC   (vector index, 1<=IVEC<=NUMVEC)
*
* IQUEST(20) : ICOPY  (if <> 0 a copy on temporary vectory was done,
*                      with LENFR and LENTO addresses defined as follow)
* IQUEST(21) : LENFR(1)
* IQUEST(22) : LENFR(2)
* IQUEST(23) : LENFR(3)
* IQUEST(31) : LENTO(1)
* IQUEST(32) : LENTO(2)
* IQUEST(33) : LENTO(3)
*
*     Output :
*
      CHARACTER*(*) VNAME
      INTEGER LLOW
      INTEGER LHIGH
*
********************************************************************************
*
      CALL KUGETC(VNAME,NCHNAM)
      CALL KUVECT(VNAME,LLOW,LHIGH)
      IF (LLOW.EQ.0) THEN
        CALL KUALFA
        PRINT *,'*** Unknown vector ',VNAME
      ENDIF
      END



