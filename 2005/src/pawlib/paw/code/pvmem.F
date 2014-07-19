*
* $Id: pvmem.F,v 1.1.1.1 1996/03/01 11:38:47 mclareni Exp $
*
* $Log: pvmem.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/14 12/07/94  17.50.53  by  Fons Rademakers
*-- Author :    Fons Rademakers   17/04/93
      SUBROUTINE PVMEM
************************************************************************
*                                                                      *
* Changes or shows the upper limit of the dynamic memory buffer        *
* facility.                                                            *
*                                                                      *
************************************************************************
*
#include "hbook/hcpiaf.inc"
*
      CHARACTER*2 CH
*
      CALL KUGETI(MXSIZE)
*
      IF (MXSIZE .EQ. -1) THEN
         CALL HITOC(MYSID, CH, L, IER)
         IF (MASTPF) PRINT *, ' *** Piaf Master Statistics ***'
         IF (SLAVPF) PRINT '(A)',
     +           ' *** Piaf Slave '//CH(1:L)//' Statistics ***'
         CALL HBPRNT(1)
#if defined(CERNLIB_CZ)
         IF (CONNPF .AND. .NOT.SERVPF) THEN
            WRITE(CHSMPF,'(A)') 'nt/vmem -1'
            CALL PFKUIP(CHSMPF, ISTAT)
            IF (ISTAT .NE. 0) THEN
               CALL HBUG('Problem executing VMEM on Piaf server',
     +                   'PVMEM',0)
            ENDIF
         ENDIF
#endif
      ELSEIF (MXSIZE .EQ. -2) THEN
         CALL HITOC(MYSID, CH, L, IER)
         IF (MASTPF) PRINT *, ' *** Piaf Master Statistics ***'
         IF (SLAVPF) PRINT '(A)',
     +           ' *** Piaf Slave '//CH(1:L)//' Statistics ***'
         CALL HBPRNT(2)
#if defined(CERNLIB_CZ)
         IF (CONNPF .AND. .NOT.SERVPF) THEN
            WRITE(CHSMPF,'(A)') 'nt/vmem -2'
            CALL PFKUIP(CHSMPF, ISTAT)
            IF (ISTAT .NE. 0) THEN
               CALL HBUG('Problem executing VMEM on Piaf server',
     +                   'PVMEM',0)
            ENDIF
         ENDIF
#endif
      ELSE
         CALL HBSMAX(MXSIZE)
      ENDIF
*
       END
