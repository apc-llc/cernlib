*
* $Id: labsys.F,v 1.1.1.1 1996/03/22 16:42:47 mclareni Exp $
*
* $Log: labsys.F,v $
* Revision 1.1.1.1  1996/03/22 16:42:47  mclareni
* Phtools
*
*
      SUBROUTINE LABSYS
C--  CALLED FROM -  USER  (OPTIONAL)
C--  PURPOSE -  TRANSFORMS PCM FROM CMS TO LAB USING ETA AND GAM
      COMMON/UTIL/PLACE(150),WEIGHT(150),NP,TECM,AMASS(18),PCM(5,18),WT
     +/BEAM  / PRM       ,TARGM    ,PBM      ,ETA      ,GAM      ,DPBM
      DO  100  I= 1, NP
      EI = PCM(4,I)
      PX = PCM(1,I)
      PCM(1,I) = GAM*PX + ETA*EI
      PCM(4,I) = ETA*PX + GAM*EI
  100 PCM(5,I) = SQRT (PCM(1,I)**2 + PCM(2,I)**2 + PCM(3,I)**2)
      RETURN
      END
