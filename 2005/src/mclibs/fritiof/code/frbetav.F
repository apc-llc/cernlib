*
* $Id: frbetav.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frbetav.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRGAUSS ************************
 
C********************************* FRBETAV ****************************
 
      SUBROUTINE FRBETAV(ID,DBETA,DP)
 
C...FOR GIVEN PAIR OF MEMENTA DP(2,4), THIS IS TO FILL THE ARRAY
C...DBETA(3) WHICH ARE THE BETA FACTORS FOR THE CMS FRAME.
C.. ID = 0 FOR NORMAL 4-VECTORS
C.. ID = 1 IF THE VECTORS ARE LIGHT-CONE FORM: PX,PY,P-,P+
 
      IMPLICIT DOUBLE PRECISION (D)
      DIMENSION DBETA(3),DP(2,5),DR(2,4)
 
      DO 10 I = 1, 2
      DR(I,1) = DP(I,1)
      DR(I,2) = DP(I,2)
      IF(ID.EQ.0) THEN
      DR(I,3) = DP(I,3)
      DR(I,4) = DP(I,4)
      ELSE
      DR(I,3) = (DP(I,4)-DP(I,3))/2.D0
      DR(I,4) = (DP(I,4)+DP(I,3))/2.D0
      ENDIF
10    CONTINUE
 
C..... BETA_X, BETA_Y, BETA_Z
      DESUM = (DR(1,4)+DR(2,4))
      IF(DESUM.LE.0.) STOP 'FRBETA: E 0'
      DO 15 I = 1, 3
15    DBETA(I) = (DR(1,I)+DR(2,I))/DESUM
 
      RETURN
      END
