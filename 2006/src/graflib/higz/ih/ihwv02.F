*
* $Id: ihwv02.F,v 1.1.1.1 1996/02/14 13:10:54 mclareni Exp $
*
* $Log: ihwv02.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:54  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 14/01/92  14.01.24  by  O.Couet
*-- Author :
      SUBROUTINE IHWV02(S,C,COSPHI,SINPHI,COSTHE,SINTHE,COSPSI,SINPSI,
     &                  TNORM,TBACK)
************************************************************************
*                                                                      *
*     IHWV02                                     Date:    13.08.90     *
*     Author: E. Chernyaev (IHEP/Protvino)       Revised: 07.01.92     *
*                                                                      *
*     Function: Define view direction (in spherical coordinates)       *
*               Compute transformation matrix from world coordinates   *
*               to normalised coordinates (-1 to +1)                   *
*                                                                      *
*     Input: S(3)    - scale factors                                   *
*            C(3)    - centre of scope                                 *
*            COSPHI  - longitude COS                                   *
*            SINPHI  - longitude SIN                                   *
*            COSTHE  - latitude COS (angle between +Z and view direc.) *
*            SINTHE  - latitude SIN                                    *
*            COSPSI  - screen plane rotation angle COS                 *
*            SINPSI  - screen plane rotation angle SIN                 *
*                                                                      *
************************************************************************
      REAL              S(3),C(3),TNORM(4,3),TBACK(4,3)
      DOUBLE PRECISION  SCALEX,SCALEY,SCALEZ,ROTA(4,4),TRAN(4,4)
      DOUBLE PRECISION  COSPHI,SINPHI,COSTHE,SINTHE,COSPSI,SINPSI
      DOUBLE PRECISION  C1,S1,C2,S2,C3,S3
*-
      SCALEX = S(1)
      SCALEY = S(2)
      SCALEZ = S(3)
*
**          S E T   T R A N S L A T I O N   M A T R I X
*
      TRAN(1,1) = 1./SCALEX
      TRAN(2,1) = 0.
      TRAN(3,1) = 0.
      TRAN(4,1) =-C(1)/SCALEX
*
      TRAN(1,2) = 0.
      TRAN(2,2) = 1./SCALEY
      TRAN(3,2) = 0.
      TRAN(4,2) =-C(2)/SCALEY
*
      TRAN(1,3) = 0.
      TRAN(2,3) = 0.
      TRAN(3,3) = 1./SCALEZ
      TRAN(4,3) =-C(3)/SCALEZ
*
      TRAN(1,4) = 0.
      TRAN(2,4) = 0.
      TRAN(3,4) = 0.
      TRAN(4,4) = 1.
*
**          S E T    R O T A T I O N   M A T R I X
*
* ( C(PSI) S(PSI) 0)   (1      0          0 )   ( C(90+PHI) S(90+PHI) 0)
* (-S(PSI) C(PSI) 0) * (0  C(THETA) S(THETA)) * (-S(90+PHI) C(90+PHI) 0)
* (   0      0    1)   (0 -S(THETA) C(THETA))   (     0           0   1)
*
      C1     = COSPSI
      S1     = SINPSI
      C2     = COSTHE
      S2     = SINTHE
      C3     =-SINPHI
      S3     = COSPHI
*
      ROTA(1,1) = C1*C3 - S1*C2*S3
      ROTA(2,1) = C1*S3 + S1*C2*C3
      ROTA(3,1) = S1*S2
      ROTA(4,1) = 0.
*
      ROTA(1,2) =-S1*C3 - C1*C2*S3
      ROTA(2,2) =-S1*S3 + C1*C2*C3
      ROTA(3,2) = C1*S2
      ROTA(4,2) = 0.
*
      ROTA(1,3) = S2*S3
      ROTA(2,3) =-S2*C3
      ROTA(3,3) = C2
      ROTA(4,3) = 0.
*
      ROTA(1,4) = 0.
      ROTA(2,4) = 0.
      ROTA(3,4) = 0.
      ROTA(4,4) = 1.
*
**          F I N D   T R A N S F O R M A T I O N   M A T R I X
*
      DO 220 I=1,3
        DO 210 K=1,4
          TNORM(K,I) = ROTA(1,I)*TRAN(K,1) +
     +                 ROTA(2,I)*TRAN(K,2) +
     +                 ROTA(3,I)*TRAN(K,3) +
     +                 ROTA(4,I)*TRAN(K,4)
  210     CONTINUE
  220   CONTINUE
*
**          S E T   B A C K   T R A N S L A T I O N   M A T R I X
*
      TRAN(1,1) = SCALEX
      TRAN(4,1) = C(1)
*
      TRAN(2,2) = SCALEY
      TRAN(4,2) = C(2)
*
      TRAN(3,3) = SCALEZ
      TRAN(4,3) = C(3)
*
**          F I N D   B A C K   T R A N S F O R M A T I O N
*
      DO 320 I=1,3
        DO 310 K=1,4
          TBACK(K,I) = TRAN(1,I)*ROTA(1,K) +
     +                 TRAN(2,I)*ROTA(2,K) +
     +                 TRAN(3,I)*ROTA(3,K) +
     +                 TRAN(4,I)*ROTA(4,K)
  310     CONTINUE
  320   CONTINUE
      RETURN
      END
