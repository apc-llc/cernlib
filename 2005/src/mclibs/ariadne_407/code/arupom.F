*
* $Id: arupom.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: arupom.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arupom.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARUPOM(KFT,KFSTR,X,XQ2,XPOM,TPOM,KFTF,KFPR,XFP,XFPOM)
 
C...ARiadne dummy routine User POMeron structure function
 
C...Produce an error message if user defined function for pomeron
C...structure function has not been linked
 
C...Return some information about possible pomeron interaction given
C...a target KFT with a struck parton KFSTR at X and XQ2. XFPOM is x
C...times the part of the KFT structure function for KFSTR which is due
C...to a pomeron. It must be smaller or equal to the total
C...given by XFP. XPOM and TPOM are the momentum fraction and
C...virtuality of the responsible pomeron. KFPR is the flavour of the
C...pomeron remnant and KFTF is the KF code of the outgoing target.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
 
      CALL ARERRM('ARUPOM',24,0)
 
      RETURN
 
C**** END OF ARUPOM ****************************************************
      END
