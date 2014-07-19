*
* $Id: dzdocobk.inc,v 1.1.1.1 1996/03/04 16:12:59 mclareni Exp $
*
* $Log: dzdocobk.inc,v $
* Revision 1.1.1.1  1996/03/04 16:12:59  mclareni
* Dzdoc/Zebpack
*
*
#ifndef CERNLIB_DZDOC_DZDOCOBK_INC
#define CERNLIB_DZDOC_DZDOCOBK_INC
*
* Parameter values for the link description bank
*
* dzdocobk.inc
*
C--     Offset for NL, NS and ND for bank being described
      PARAMETER (MONLQ = 1 , MONSQ = MONLQ+1 , MONDQ = MONSQ+1)
C--     Offset for Hollerith identifier of bank
      PARAMETER (MOIDHQ = 7)
C--     Offset of ORigin, UP and NeXt links
      PARAMETER (MOORQ = 8 , MOUPQ = MOORQ+1 , MONXQ = MOUPQ+1)
C--     Offset for 0th address of links of bank
      PARAMETER (MOLK0Q = MONXQ)
C--     Offset in IQUEST of return address for link description bank
      PARAMETER (MOQUEQ = 11)

#endif
