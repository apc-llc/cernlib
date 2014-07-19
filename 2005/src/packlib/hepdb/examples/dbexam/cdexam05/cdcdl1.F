*
* $Id: cdcdl1.F,v 1.1.1.1 1996/02/28 16:23:47 mclareni Exp $
*
* $Log: cdcdl1.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:47  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDCDL1
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDCDL1                                                  *
*                                                                      *
*   Creates the directory structure of example Level 1 energy trigger  *
*   Data base                                                          *
*                                                                      *
*   Called by CDL1ET                                                   *
*                                                                      *
************************************************************************
*
      PARAMETER       (NKTPS=3, NKBRA=4, NKMAT=10, NKMOD=7, NKCAB=8)
      CHARACTER       CFTPS*3, CFBRA*4, CFMAT*10, CFMOD*7, CFCAB*8
      CHARACTER*8     CTTPS(3), CTBRA(4), CTMAT(10), CTMOD(7), CTCAB(8)
      DATA            CFTPS /'IHA'/, CFBRA /'IIII'/, CFMOD /'HAHIIII'/
      DATA            CFMAT /'IIIIIIIIII'/, CFCAB /'IIIIIIIH'/
      DATA            CTTPS /'TOPOLOGY', 'SERIAL_1', 'SERIAL_2'/
      DATA            CTBRA /'BRANCH  ', 'CAMAC   ', 'RACK    ',
     +                       'CRATE   '/
      DATA            CTMAT /'TOPOL.NU', 'TYPE    ', 'BRANCH  ',
     +                       'CAMAC   ', 'STATION ', 'TIMING  ',
     +                       'SWITCHES', 'IN/OUT_C', 'RES.OUT1',
     +                       'RES.OUT2'/
      DATA            CTMOD /'SERIAL_1', 'SERIAL_2', 'STATUS  ',
     +                       'MODU_TYP', 'NOBITS  ', 'RES.OUT1',
     +                       'RES.OUT2'/
      DATA            CTCAB /'#OUT_MOD', 'OUT_1ST ', 'OUT_LAST',
     +                       '#INP_MOD', 'INP_1ST ', 'INP_LAST',
     +                       'NICKNAME', 'POLARITY'/
*
*     ------------------------------------------------------------------
*
      IPREC  = 0
      DELTA  = 0.0
      CALL CDMDIR ('//DBL3/TRIG/TOPSER',          NKTPS, CFTPS, CTTPS,
     +             0, IPREC, DELTA, ' ', IRC)
      CALL CDMDIR ('//DBL3/TRIG/HARDCONF/BRACAM', NKBRA, CFBRA, CTBRA,
     +             0, IPREC, DELTA, ' ', IRC)
      CALL CDMDIR ('//DBL3/TRIG/HARDCONF/MODATT', NKMAT, CFMAT, CTMAT,
     +             0, IPREC, DELTA, ' ', IRC)
      CALL CDMDIR ('//DBL3/TRIG/MODPRP',          NKMOD, CFMOD, CTMOD,
     +             0, IPREC, DELTA, ' ', IRC)
      CALL CDMDIR ('//DBL3/TRIG/HARDCONF/CABCON', NKCAB, CFCAB, CTCAB,
     +             0, IPREC, DELTA, ' ', IRC)
*                                                             END CDCDL1
      END
