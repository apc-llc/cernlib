*
* $Id: csdpvs.inc,v 1.1.1.1 1996/02/26 17:16:35 mclareni Exp $
*
* $Log: csdpvs.inc,v $
* Revision 1.1.1.1  1996/02/26 17:16:35  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_CSDPVS_INC
#define CERNLIB_COMIS_CSDPVS_INC
*
*
* csdpvs.inc
*
      DOUBLE PRECISION D,  D1,  D2
      COMPLEX          CX, CX1, CX2
      COMMON/CSDPVS/D,D1,D2
      EQUIVALENCE (D,CX), (D1,CX1), (D2,CX2)
      PARAMETER (KDLEN=2, KDLEN2=KDLEN*2)

#endif
