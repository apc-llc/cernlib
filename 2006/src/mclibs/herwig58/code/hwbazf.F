*
* $Id: hwbazf.F,v 1.1.1.1 1996/03/08 17:02:10 mclareni Exp $
*
* $Log: hwbazf.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:10  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.45  by  Unknown
*-- Author :
CDECK  ID>, HWBAZF.
*CMZ :-        -26/04/91  11.11.54  by  Bryan Webber
*-- Author :    Ian Knowles
C------------------------------------------------------------------------
      SUBROUTINE HWBAZF(IPAR,JPAR,VEC1,VEC2,VEC3,VEC)
C     Azimuthal correlation functions for Collins' algorithm,
C     see I.G.Knowles, Comp. Phys. Comm. 58 (90) 271 for notation.
C------------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      LOGICAL GLUI,GLUJ
      INTEGER IPAR,JPAR
      DOUBLE PRECISION Z1,Z2,DOT12,DOT23,DOT31,TR,
     & FN(7),VEC1(2),VEC2(2),VEC3(2),VEC(2)
      IF (.NOT.AZSPIN) RETURN
      Z1=PPAR(4,JPAR)/PPAR(4,IPAR)
      Z2=1.-Z1
      GLUI=IDPAR(IPAR).EQ.13
      GLUJ=IDPAR(JPAR).EQ.13
      IF (GLUI) THEN
         IF (GLUJ) THEN
C           Branching: g--->gg
            FN(2)=Z2/Z1
            FN(3)=1./FN(2)
            FN(4)=Z1*Z2
            FN(1)=FN(2)+FN(3)+FN(4)
            FN(5)=FN(2)+2.*Z1
            FN(6)=FN(3)+2.*Z2
            FN(7)=FN(4)-2.
         ELSE
C           Branching: g--->qqbar
            FN(1)=(Z1*Z1+Z2*Z2)/2.
            FN(2)=0.
            FN(3)=0.
            FN(4)=-Z1*Z2
            FN(5)=-(2.*Z1-1.)/2.
            FN(6)=-FN(5)
            FN(7)=FN(1)
         ENDIF
      ELSE
         IF (GLUJ) THEN
C           Branching: q--->gq
            FN(1)=(1.+Z2*Z2)/(2.*Z1)
            FN(2)=Z2/Z1
            FN(3)=0.
            FN(4)=0.
            FN(5)=FN(1)
            FN(6)=(1.+Z2)/2.
            FN(7)=-FN(6)
         ELSE
C           Branching: q--->qg
            FN(1)=(1.+Z1*Z1)/(2.*Z2)
            FN(2)=0.
            FN(3)=Z1/Z2
            FN(4)=0.
            FN(5)=(1.+Z1)/2.
            FN(6)=FN(1)
            FN(7)=-FN(5)
         ENDIF
      ENDIF
      DOT12=VEC1(1)*VEC2(1)+VEC1(2)*VEC2(2)
      DOT23=VEC2(1)*VEC3(1)+VEC2(2)*VEC3(2)
      DOT31=VEC3(1)*VEC1(1)+VEC3(2)*VEC1(2)
      TR=1./(FN(1)+FN(2)*DOT23+FN(3)*DOT31+FN(4)*DOT12)
      VEC(1)=((FN(2)+FN(5)*DOT23)*VEC1(1)
     &       +(FN(3)+FN(6)*DOT31)*VEC2(1)
     &       +(FN(4)+FN(7)*DOT12)*VEC3(1))*TR
      VEC(2)=((FN(2)+FN(5)*DOT23)*VEC1(2)
     &       +(FN(3)+FN(6)*DOT31)*VEC2(2)
     &       +(FN(4)+FN(7)*DOT12)*VEC3(2))*TR
      END
