*
* $Id: celoss.inc,v 1.1.1.1 1995/10/24 10:22:20 cernlib Exp $
*
* $Log: celoss.inc,v $
* Revision 1.1.1.1  1995/10/24 10:22:20  cernlib
* Geant
*
*
*
* celoss.inc
*
      PARAMETER (NBIN= 50)
      COMMON/CELOSS/ ELEAK(3),
     +               SEL1(NBIN), SEL1C(NBIN), SER1(NBIN), SER1C(NBIN),
     +               SEL2(NBIN), SEL2C(NBIN), SER2(NBIN), SER2C(NBIN),
     +             SNPAT1(NBIN,4),SNPAT2(NBIN,4),
     +               DEDL(NBIN),DEDR(NBIN),FNPAT(NBIN,4)
