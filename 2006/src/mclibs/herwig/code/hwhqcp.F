CDECK  ID>, HWHQCP.
*CMZ :-        -26/04/91  10.18.57  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWHQCP(ID3,ID4,IPERM,IHPR,*)
C-----------------------------------------------------------------------
C     IDENTIFIES HARD SUBPROCESS
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      INTEGER HWRINT,ID3,ID4,IPERM,IHPR,ND3
      EXTERNAL HWRINT
      IHPRO=IHPR
      IF (ID3.GT.0) THEN
        IDN(3)=ID3
        IDN(4)=ID4
      ELSE
        ND3=-ID3
        IF (ID3.GT.-7) THEN
    1     IDN(3)=HWRINT(1,MAXFL)
          IF (IDN(3).EQ.ND3) GOTO 1
          IDN(4)=IDN(3)+6
        ELSE
    2     IDN(3)=HWRINT(1,MAXFL)+6
          IF (IDN(3).EQ.ND3) GOTO 2
          IDN(4)=IDN(3)-6
        ENDIF
      ENDIF
      ICO(1)=IPERM/1000
      ICO(2)=IPERM/100-10*ICO(1)
      ICO(3)=IPERM/10 -10*(IPERM/100)
      ICO(4)=IPERM    -10*(IPERM/10)
      RETURN 1
      END
