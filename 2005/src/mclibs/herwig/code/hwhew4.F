CDECK  ID>, HWHEW4.
*CMZ :-        -26/04/91  10.18.57  by  Bryan Webber
*-- Author :    Zoltan Kunszt, modified by Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWHEW4(N1,N2,N3,N4,N5,N6)
C-----------------------------------------------------------------------
      IMPLICIT NONE
      COMPLEX HWHEW4,ZH,ZCH,ZD
      INTEGER N1,N2,N3,N4,N5,N6
      COMMON/HWHEWQ/ZH(7,7),ZCH(7,7),ZD(7,7)
      HWHEW4=CMPLX(4.0)*ZH(N1,N3)*ZCH(N2,N6)*(ZH(N1,N5)*ZCH(N1,N4)
     X                                       +ZH(N3,N5)*ZCH(N3,N4))
      RETURN
      END
