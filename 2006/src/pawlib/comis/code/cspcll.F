*
* $Id: cspcll.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: cspcll.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.17/07 07/01/94  12.46.42  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSPCLL(JS,MD,M)
***------------------------------
      INTEGER CSLTGP,CSITGP,CSINT1
         PARAMETER (KLK=45, KCALLS=87, KEXIT=113)
         INTEGER M(2),MD(11)
         CHARACTER *8 EXTR(10)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
*unused+SEQ,CSPNTS.
#include "comis/cssysd.inc"
         DATA EXTR/'*EXT1','*EXT2','*EXT3','*EXT4','*EXT5',
     +    '*EXT6','*EXT7','*EXT8','*EXT9','*EXT10'/
       NP=MD(1)
       I=MHLOC(5*NP+4)
       K=I+2*NP
       IQ(K)=KCALLS
       IQ(K+1)=JS
       IQ(K+2)=NP
       I3=K+3
       I2=I
       ID=I+3*NP+4
       DO 1 K=1,NP
         JJ=MD(K+1)
         IQ(I3)=-JJ
         IF(JJ.EQ.-7)THEN
           CALL CSCHID(EXTR(K))
           IP=CSLTGP(IPVS)
           IADGP=M(K)
           ITYPGP=1
           IFCS=-1
           IF(IP.EQ.0)THEN
             IP=CSITGP(IPVS)
           ELSE
             CALL CSRTGP(IP)
           ENDIF
           M(K)=IP
           IQ(I3)=701
         ENDIF
         IF(JJ.GT.0)THEN
           IQ(I3)=303
#if !defined(CERNLIB_VAX)
           IQ(ID)=JJ+KON1
         IQ(ID+1)=M(K)*NBYTPW
           M(K)=LOCF(IQ(ID))
           ID=ID+2
#endif
         ENDIF
         IQ(I2)=KLK
         IQ(I2+1)=M(K)
         I2=I2+2
   1  I3=I3+1
      IQ(I3)=KEXIT
      INT=CSINT1(I)
      CALL MHFREE(I)
      END
