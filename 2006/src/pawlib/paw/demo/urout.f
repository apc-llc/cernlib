*CMZ :  1.00/02 03/05/93  16.10.32  by  Unknown
*-- Author :
      SUBROUTINE UROUT(NEV)
      DO 10 I=1,NEV
          X=HRNDM1(100)
          CALL HFILL(110,X,0.,1.)
   10 CONTINUE
      END

