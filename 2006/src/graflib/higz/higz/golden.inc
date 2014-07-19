*
* $Id: golden.inc,v 1.1.1.1 1996/02/14 13:10:18 mclareni Exp $
*
* $Log: golden.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:18  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_GOLDEN_INC
#define CERNLIB_HIGZ_GOLDEN_INC
*
*
* golden.inc
*
#if defined(CERNLIB_MSDOS)
C--
C--     PARAMETERS FOR SURFER META-FILE
C
        INTEGER    PA,MA,TR,PS,SC,SS,SP
        PARAMETER (NJOB = 7)
        PARAMETER (PA=1,MA=2,TR=3,PS=4,SC=5,SS=6,SP=7)

        PARAMETER (LBUF=256)
        COMMON /GOLDEN/ LUNG(9), NREC, M_OPEN, BIMODE, BUFFER(LBUF/4)
     *,                        IPOINT, SYMFIL,COOR(4), LOCK, FRADEL
        LOGICAL BIMODE, M_OPEN, LOCK, FRADEL
        CHARACTER*80 SYMFIL
        CHARACTER*512 CHMBUF
        EQUIVALENCE (BUFFER,CHMBUF)
#endif

#endif
