*
* $Id: czdgta.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czdgta.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDGTA(CHMAIL,ISTAT)
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
*
      CHARACTER*(*) CHMAIL
      CHARACTER*80  CHTEMP
*
      INTEGER NCH, ISTAT
*
      NCH = LEN(CHMAIL)
      IF(NCH.GT.80) NCH = 80
      CHTEMP = ' '
      READ(UNIT=LUNDEC,FMT='(A80)',END=99,ERR=98) CHTEMP
      CHMAIL(1:NCH) = CHTEMP(1:NCH)
      ISTAT = 0
      IF(IDEBXZ.GE.3) PRINT *,'CZGPTA. read ',
     +   CHMAIL(1:NCH)
      RETURN
98    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZDGTA read error on unit: ',LUNDEC
      ISTAT = 1
      RETURN
99    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** CZDGTA EOF on unit: ', LUNDEC
      ISTAT = 1
      END
