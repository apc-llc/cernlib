*
* $Id: kilun.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: kilun.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.00/00 01/12/92  18.48.32  by  Alfred Nathaniel
*-- Author :    Alfred Nathaniel   01/12/92
      SUBROUTINE KILUN(LUN)
#include "kuip/kcmcmd.inc"
#include "kuip/kcgen1.inc"
#include "kuip/kcunit.inc"
      LUIBAS=LUN
      IF (LUIBAS.LE.0) LUIBAS=11

      LUIFIL=LUIBAS
      LUIVR =LUIBAS+1
      LUIVW =LUIBAS+2
      LUIHIS=LUIBAS+3
      LUIAPP=LUIBAS+4
      LUIHIW=LUIAPP
      LUIMAN=LUIAPP

      LUNFIL=LUIFIL
      LUNVR =LUIVR
      LUNVW =LUIVW
      LUNHIS=LUIHIS
      LUNAPP=LUIAPP
      LUNHIW=LUIHIW
      LUNMAN=LUIMAN

CCCM
      LUNML=LUIBAS+5

      END
