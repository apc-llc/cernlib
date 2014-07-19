*
* $Id:
*
* $Log:
*
*
#include "paw/pilot.h"
      SUBROUTINE PADRISO(NX,NY,NZ,X,Y,Z,VALUE,S)
*
***************************************************************************
*-- Entry point for the plot of the Gouraud shading 
*     (option 'G' in Nt/plot)
*   Does some init (lights, color) and calls IHISO
*   Called by "qp_exec_end" in file "qp_execute.c"
*
*     INTEGER           NX,NY,NZ              Nb of bins in each dimensions
*     REAL              X(NX),Y(NY),Z(NZ)     Coordinates
*     REAL              S(3)                  Isosurface level
*     REAL              VALUE(NX,NY,NZ)       Histogram values
*
***************************************************************************
*
#include "paw/pcolv.inc"

      INTEGER           NX,NY,NZ
      REAL              S(3),X(NX),Y(NY),Z(NZ)
      REAL              VALUE(NX,NY,NZ)

      PARAMETER    (YDIFF=1.,YLIGH1=10.)
      PARAMETER    (QA=0.15,QD=0.15,QS=0.8,NQS=1)
      EXTERNAL PADRFA
*
*     
*--   Define 3 light palettes
*     
      NCOLOR = 20
      RH1    = 20.
      RH2    = 140.
      RH3    = 200.
      RS     = 1.
      DD     = 1./(2*NCOLOR)
      IC1    = 8
      IC2    = IC1+NCOLOR
      IC3    = IC2+NCOLOR
      DO 340 I=1,NCOLOR
         CALL IGHTOR(RH1,.4+I*DD,RS,R,G,B)
         CALL ISCR(1,I+7,R,G,B)
         CALL IGHTOR(RH2,.4+I*DD,RS,R,G,B)
         CALL ISCR(1,I+7+NCOLOR,R,G,B)
         CALL IGHTOR(RH3,.4+I*DD,RS,R,G,B)
         CALL ISCR(1,I+7+2*NCOLOR,R,G,B)
 340  CONTINUE
*     
* Set light sources
*     
      CALL IHLIGH(0,YDIFF, 0.,0.,0.,IREP)
      CALL IHLIGH(1,YLIGH1,1.,1.,1.,IREP)
      CALL IHPROP(QA,QD,QS,NQS,IREP)
      FMIN = YDIFF*QA
      FMAX = YDIFF*QA + (YLIGH1+0.1)*(QD+QS)
      CALL IHISO(3,S,NX,NY,NZ,
     +     X,Y,Z,VALUE,PADRFA,'BF')
*     
      END
