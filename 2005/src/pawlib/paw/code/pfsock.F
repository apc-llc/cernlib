*
* $Id: pfsock.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfsock.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  2.03/07 18/08/93  18.23.04  by  Fons Rademakers
*-- Author :    Fons Rademakers   18/08/93
      SUBROUTINE PFSOCK(ISLAV)
************************************************************************
*                                                                      *
* Set the sockets to communicate with slave ISLAV. When ISLAV=0        *
* communicate with client.                                             *
*                                                                      *
************************************************************************
*
#include "hbook/czsock.inc"
#include "hbook/hcpiaf.inc"
*
      IF (ISLAV .EQ. 0) THEN
         ISKIN  = CLINPF
         ISKOUT = CLUTPF
      ELSE
         ISKIN  = SLINPF(ISLAV)
         ISKOUT = SLUTPF(ISLAV)
      ENDIF
*
      END
#endif
