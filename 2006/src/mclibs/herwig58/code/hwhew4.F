*
* $Id: hwhew4.F,v 1.1.1.1 1996/03/08 17:02:14 mclareni Exp $
*
* $Log: hwhew4.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:14  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHEW4.
*CMZ :-        -26/04/91  10.18.57  by  Bryan Webber
*-- Author :    Zoltan Kunszt, modified by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWHEW4(N1,N2,N3,N4,N5,N6)
      IMPLICIT NONE
      INTEGER N1,N2,N3,N4,N5,N6
      COMPLEX ZH,ZCH,ZD,HWHEW4
      COMMON/HWHEWQ/ZH(7,7),ZCH(7,7),ZD(7,7)
      HWHEW4=4.D0*ZH(N1,N3)*ZCH(N2,N6)*(ZH(N1,N5)*ZCH(N1,N4)
     X                                 +ZH(N3,N5)*ZCH(N3,N4))
      RETURN
      END
