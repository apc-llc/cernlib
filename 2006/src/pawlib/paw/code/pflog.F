*
* $Id: pflog.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pflog.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 18/08/93  14.38.47  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   02/05/93
      SUBROUTINE PFLOG(LEV,NAME,CHMAIL)
      CHARACTER*(*) NAME,CHMAIL
*
* print incoming messages from Piaf server
*
#include "hbook/hcpiaf.inc"
*
      IF (LOGLPF .LT. LEV) RETURN
      L = LENOCC(CHMAIL)
      IF (L .EQ. 0) L=1
      IF (SERVPF) THEN
         PRINT '(2A,I2,2A)',NAME,'(',MYSID,'): ',CHMAIL(:L)
      ELSE
         PRINT '(3A)',NAME,': ',CHMAIL(:L)
      ENDIF
      END
