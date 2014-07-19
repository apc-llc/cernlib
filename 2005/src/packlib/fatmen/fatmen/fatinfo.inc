*
* $Id: fatinfo.inc,v 1.1.1.1 1996/03/07 15:17:55 mclareni Exp $
*
* $Log: fatinfo.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:55  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATINFO_INC
#define CERNLIB_FATMEN_FATINFO_INC
*
*
* fatinfo.inc
*
*
*     Common to store FATMEN information per LUN
*     Access method (disk, stage, setup) and filemode (CMS only)
*     Virtual address for tape drive
*     This information is set by FMOPEN/FIND/MAKE and subsequently
*     used by FMCLOS.
*
*     IFMODE: 0: FZin, 1: FZout, 2: RZ
*     JFMODE: ICFOP from FMOPEN (Fortran, C, package I/O)
*     LFMODE: 1=disk, 2=stage, 3=tape
*
      COMMON/FATINF/IVADDR(99),IDEV(99),  IFMODE(99),
     +              JFMODE(99),LFMODE(99),IFPNTR(99),CHMODE(99)
      CHARACTER*1   CHMODE

#endif
