*
* $Id: epdefu.F,v 1.1.1.1 1996/03/08 15:21:43 mclareni Exp $
*
* $Log: epdefu.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:43  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPDEFU(LUNIT,NAME,IERR)
C.
C. ************* VAX AND APOLLO *******************
C.
C. ALLOWS TO SPECIFY A FILE NAME OTHER THAN THE DEFAULT
C. NAME FOR0$$.DAT  ($$ FOR UNIT NUMBER)
C.
C. INPUT
C. LUNIT      LOGICAL UNIT NUMBER
C. NAME       FILE NAME, TYPE CHARACTER
C.
C. OUTPUT
C. IERR     ERROR FLAG. SEE SEPARATE LIST.
C.
C. THE NAME IS STORED IN EPVXUN, COMMON BLOCK EPVAXN, FOR THE VAX
C.
C. THE NAME IS STORED IN EPVXUN, COMMON BLOCK EPAPON, FOR THE APOLLO
C.
#include "epio/epiocom.inc"
#if defined(CERNLIB_VAX)||defined(CERNLIB_STF77VX)
#include "epio/epvaxcom.inc"
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_STF77VX))
#include "epio/epapocom.inc"
#endif
      CHARACTER NAME*(*)
      CALL EPUNIT(LUNIT,IERR)
      IF (IERR.EQ.0)  EPVXUN(LREF)=NAME
      END
