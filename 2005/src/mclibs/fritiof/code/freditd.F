*
* $Id: freditd.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: freditd.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C*************************************************************************
 
 
C*************************************************************************
C*                                                                       *
C*   FRITIOF 7.02 subroutine packages                                     *
C*                                                                       *
C------------------------------------------------------------------------*
C*************************************************************************
 
C**************************** FREDITD ***********************************
 
      SUBROUTINE FREDITD()
 
C...This is a dummy subroutine in connection to option KFR(13)>=4.
C...User may elect to write his own special purpose codes here that
C...edits and compresses the event record LUJETS.  There may also be
C...times the user wish to keep a trace on certain decay products by
C...assigning them a special codes here.
 
      PARAMETER (KSZJ=4000,KSZ1=20)
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      SAVE /FRPARA1/,/LUJETS/
 
C.....DUMMY
      RETURN
      END
