*
* $Id: pfinit.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfinit.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/12 01/02/94  12.58.56  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   07/04/93
      SUBROUTINE PFINIT
*
* initialize /HCPIAF/ for client side
*

#include "hbook/hcpiaf.inc"

      NTPIAF = .FALSE.
      CONNPF = .FALSE.
      SERVPF = .FALSE.
      MASTPF = .FALSE.
      SLAVPF = .FALSE.
      LOGLPF = 0
      NSLAVE = 0
      NSLVPF = 0
      NSLBAD = 0
      NGSIZE = 0
      MYSID  = 0
      MASPID = 0

      END
