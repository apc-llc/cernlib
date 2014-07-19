*CMZ :  1.09/00 04/05/90  15.50.53  by  Rene Brun
*-- Author :
      REAL FUNCTION MANDEL(XP)
      DIMENSION XP(2)
      DATA NMAX/30/
      X=XP(1)
      Y=XP(2)
      XX=0.
      YY=0.
            DO 30 N=1,NMAX
               TT=XX*XX-YY*YY+X
               YY=2.*XX*YY+Y
               XX=TT
               IF (4..LT.XX*XX+YY*YY) GO TO 1
   30       CONTINUE
    1       MANDEL=FLOAT(N)/FLOAT(NMAX)
      END

