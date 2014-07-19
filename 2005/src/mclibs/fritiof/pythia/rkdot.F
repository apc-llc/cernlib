*
* $Id: rkdot.F,v 1.1.1.1 1996/01/11 14:05:30 mclareni Exp $
*
* $Log: rkdot.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:30  mclareni
* Fritiof
*
*
*==================================================================
 
      FUNCTION RKDOT(P,Q)
      DOUBLE PRECISION P(0:4),Q(0:4),RKDOT
      RKDOT=P(0)*Q(0)-P(1)*Q(1)-P(2)*Q(2)-P(3)*Q(3)
      END
