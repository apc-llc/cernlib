*
* $Id: czdpta.F,v 1.1.1.1 1996/03/08 15:44:24 mclareni Exp $
*
* $Log: czdpta.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:24  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE CZDPTA( CHMAIL, ISTAT )
************************************************************************
*.
*... CZDPTA   to send the string CHMAIL to a remote host
*.
*.
*. Author  : B.Holl
*. Version : 1.00
*. Created : 05-Apr-1990
*. Last Mod:
*.
*. Modification Log.
*.
*.**********************************************************************
*
#include "cspack/czdecnet.inc"
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
*
      CHARACTER*(*) CHMAIL
      CHARACTER*80  CHTEMP
*
      INTEGER NCH, ISTAT
*
*.......................................................................
*
      NCH  = LEN(CHMAIL)
      IF(NCH.GT.80) NCH = 80
      IF(IDEBXZ.GE.3) PRINT *,'CZDPTA. enter for ',
     +   CHMAIL(1:NCH)
      CHTEMP = ' '
      CHTEMP = CHMAIL(1:NCH)
      WRITE(UNIT=LUNDEC,FMT='(A80)',ERR=99) CHTEMP
      ISTAT = 0
      RETURN
99    CONTINUE
      WRITE(LUNCZ,'(1X,A,I4)') '**** Error in CZDPTA for unit: ',LUNDEC
      ISTAT = 1
      END
 
