#include <math.h>
#include <string.h>

void fcn(int npar, double grad[3], double * fcnval,
           double xval[2],int iflag, void (*Dummy)())
{
      double Xc=1.11,Yc=3.14;
/* 
 *  demo FCN 
 *  Gunter Folger, May 95
 */

	switch(iflag) {
	  case 1:
/*
 *      Initialaise.
 */
		printf(" fcn_c called to initialise\n");
		break;
	  case 2:		
/*
 *        derivatives...
 */
		break;

	  default:
		*fcnval = pow(xval[0]-Xc,2.) + pow((xval[1]-Yc),3.)*xval[1];
		break;
	}      
}
