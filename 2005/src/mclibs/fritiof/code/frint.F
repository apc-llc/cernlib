*
* $Id: frint.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frint.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C************************ END FUNCTION FRVOV ********************************
 
C*************************************************************************
 
      FUNCTION FRINT(FF,FX,NN,X)
 
CC........this function deals with the linear interpolation
 
      DIMENSION FF(NN),FX(NN)
 
      IF (X.LT.FX(1).OR.X.GT.FX(NN)) THEN
         CALL FRMGOUT(0,0,
     >   'X out of range! Be advised to set KFR(6)=0.',
     >    X,FX(1),FX(2),0.,0.)
      ENDIF
        DO 100 I=1,NN-1
      IF (X.GT.FX(I).AND.X.LE.FX(I+1)) THEN
      FRINT=FF(I)+(FF(I+1)-FF(I))*(X-FX(I))/(FX(I+1)-FX(I))
      goto 200
      ENDIF
100     CONTINUE
 
200   RETURN
      END
