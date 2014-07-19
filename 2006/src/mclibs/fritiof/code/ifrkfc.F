*
* $Id: ifrkfc.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: ifrkfc.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C********************************* END FRBELEO **************************
 
C********************************* IFRKFC ******************************
 
C... THE KF CODE FOR A 2- OR 3-QUARK SYSTEM OF SPIN S COMPOSED BY
C... FLAVOR IA, IB, IC: (THE SYSTEM MUST BE qq or qqq, not qqbar, etc).
C... IT CORRESPONDS TO A DIQUARK SYSTEM IF IC=0.........................
 
      INTEGER FUNCTION IFRKFC(IA,IB,IC,S)
 
      IA0 = MAX( IABS(IA), MAX(IABS(IB),IABS(IC)))
      IC0 = MIN( IABS(IA), MIN(IABS(IB),IABS(IC)))
      IB0 = IABS(IA+IB+IC)-IA0-IC0
      IFRKFC = 1000*IA0 + 100*IB0 + 10*IC0 + INT(2.*(S+0.2))+ 1
      IF(IA.NE.IABS(IA).OR.IB.NE.IABS(IB)) IFRKFC = -IFRKFC
      RETURN
      END
