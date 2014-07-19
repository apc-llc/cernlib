*
* $Id: fattyp.inc,v 1.1.1.1 1996/03/07 15:17:57 mclareni Exp $
*
* $Log: fattyp.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:57  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATTYP_INC
#define CERNLIB_FATMEN_FATTYP_INC
*
* Media type definitions
*
* fattyp.inc
*
      PARAMETER    (NMTYP=100)
      COMMON/FATTYP/NFMTYP,CHMTYP,CHMGEN,CHMSIZ,
     +              CHMDEN,CHMMNT,CHMLAB,MEDSIZ,
     +              MEDDEN,MEDHWM,MEDMFL,MEDMFS
      INTEGER       NFMTYP
      INTEGER       MEDSIZ(NMTYP),MEDDEN(NMTYP),
     +              MEDHWM(NMTYP),MEDMFL(NMTYP),
     +              MEDMFS(NMTYP)
      CHARACTER*8   CHMTYP(NMTYP),CHMGEN(NMTYP),
     +              CHMSIZ(NMTYP),CHMDEN(NMTYP),
     +              CHMMNT(NMTYP),CHMLAB(NMTYP)
 

#endif
