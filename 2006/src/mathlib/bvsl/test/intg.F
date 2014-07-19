*
* $Id: intg.F,v 1.1.1.1 1996/03/21 17:19:54 mclareni Exp $
*
* $Log: intg.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:54  mclareni
* Bvsl
*
*
*CMZ :          06/11/90  10.43.59  by  Michel Roethlisberger/IBM
*-- Author :
*********************************************************************
*CMZ :          04/05/90  16.17.27  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE INTG(NW,AIN,UVEC,ALVEC,IV,IFOUND)
      DIMENSION AIN(*), UVEC(*),ALVEC(*),IV(*)
      IFOUND=0
      DO I=1,NW
        IF(AIN(I).LT.UVEC(I).AND.AIN(I).GT.ALVEC(I))THEN
          IFOUND=IFOUND+1
          IV(IFOUND)=I
        ENDIF
      ENDDO
      RETURN
      END
