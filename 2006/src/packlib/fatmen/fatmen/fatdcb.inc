*
* $Id: fatdcb.inc,v 1.1.1.1 1996/03/07 15:17:54 mclareni Exp $
*
* $Log: fatdcb.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:54  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATDCB_INC
#define CERNLIB_FATMEN_FATDCB_INC
*
*
* fatdcb.inc
*
      COMMON/FATDCB/TYPE,HDSN,DISP,VOL,LABEL,IDCB,
     +              UNIT,ISPACE,IUSED
      INTEGER       TYPE,HDSN(14),DISP(3),VOL(2),LABEL(3),IDCB(4),
     +              UNIT(2),ISPACE(4),IUSED(5)
      COMMON/FATSPA/CHSPAC
      CHARACTER*4   CHSPAC

#endif
