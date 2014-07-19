*
* $Id: frorder.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frorder.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRFILHW **************************
 
C********************************* FRORDER ******************************
 
      SUBROUTINE FRORDER(L,NS,NE)
 
C......TO ORDER PARTICLES (gluons) ACCORDING TO
C........FOR KFR12=1, ASCENDING RAPIDITY FOR PROJECTILE
C........              DESCENDING RAPIDITY FOR TARGET
C........FOR KFR12>,=2, ASCENDING PT
 
      PARAMETER (KSZ2=300,KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRATLE1/NA1,KA1(KSZ2,5),PA1(KSZ2,5)
      COMMON/FRDUMYOR/KO(KSZ2,5),PO(KSZ2,5),Y(KSZ2),II(KSZ2)
      SAVE /FRPARA1/,/FRATLE1/,/FRDUMYOR/
 
      KFR12 = 2
      IF(NE.LE.NS) RETURN
      IF(NE-NS+1.GT.KSZ2) CALL FRMGOUT(0,1,'FRDUMYOR array size
     > insufficient', FLOAT(NS),FLOAT(NE),0.,0.,0.)
 
      SML = 1.E-20
      if(L.EQ.1.OR.KFR12.ge.2) THEN
C             !ascending
      IQ = -1
      ELSE
C              !descending
      IQ = 1
      ENDIF
 
      IOR=0
      DO 31 IO = NS, NE
      IR = IO - NS + 1
      DO 33 J=1, 5
      KO(IR,J) = KA1(IO,J)
33    PO(IR,J) = PA1(IO,J)
      PPLS = PA1(IO,4) + PA1(IO,3)
      PMIS = PA1(IO,4) - PA1(IO,3)
      IF(KFR12.EQ.1) THEN
      Y(IR) = .5* LOG( MAX(PPLS,SML)/MAX(PMIS,SML))
      ELSEIF(KFR12.GE.2) THEN
      Y(IR) = PA1(IO,1)**2+PA1(IO,2)**2
      ENDIF
      IF(IOR.EQ.0.AND.IR.GE.2) THEN
       IF(IQ.EQ.1.AND.Y(IR).GT.YLIM) IOR=1
       IF(IQ.EQ.-1.AND.Y(IR).LT.YLIM) IOR=1
      ENDIF
      IF(IR.EQ.1) THEN
      YLIM = Y(1)
      ELSEIF(IR.GT.1.AND.IQ.EQ.1) THEN
      YLIM = MIN(YLIM,Y(IR))
      ELSEIF(IQ.EQ.-1) THEN
      YLIM = MAX(YLIM,Y(IR))
      ENDIF
 
31    CONTINUE
 
      IF(IOR.EQ.0) RETURN
 
      CALL FRORD01(Y, II, NE-NS+1, IQ)
 
      DO 35 IO = NS,NE
      IR = IO - NS + 1
       DO 35 J = 1, 5
      KA1(IO,J) = KO(II(IR),J)
35    PA1(IO,J) = PO(II(IR),J)
 
      RETURN
      END
