CDECK  ID>, HWSSUD.
*CMZ :-        -26/04/91  11.11.56  by  Bryan Webber
*-- Author :    Bryan Webber
C-----------------------------------------------------------------------
      FUNCTION HWSSUD(I)
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWSSUD,HWSGQQ,DMIN,QSCA,XLAST,DIST(13)
      INTEGER I,N0,IS,ID
      EXTERNAL HWSGQQ
      COMMON/HWTABC/XLAST,N0,IS,ID
      DATA DMIN/1.D-15/
      QSCA=QEV(N0+I,IS)
      CALL HWSFUN(XLAST,QSCA,IDHW(INHAD),NSTRU,DIST,JNHAD)
      IF (ID.EQ.13) DIST(ID)=DIST(ID)*HWSGQQ(QSCA)
      IF (DIST(ID).LT.DMIN) DIST(ID)=DMIN
      HWSSUD=SUD(N0+I,IS)/DIST(ID)
      END
