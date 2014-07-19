#include <math.h>
#include <string.h>
#include <cfortran.h>

/* Authors: William Hanlon <whanlon@cosmic.utah.edu>,
 *          Gunter Folger <Gunter.Folger@cern.ch>
 */

/* prototype for benefit of the wrapper */
void fcn(int npar, double grad[3], double * fcnval,
         double xval[2],int iflag, void (*Dummy)());

/* this macro creates a wrapper function called fcn_, which in turn calls the
 * function fcn defined below. The wrapper properly passes ints by value to
 * fcn, while it receives ints by reference from the fortran calling routine.
 */
FCALLSCSUB6(fcn,FCN,fcn,INT,DOUBLEV,PDOUBLE,DOUBLEV,INT,ROUTINE)


void fcn(int npar, double grad[3], double * fcnval,
         double xval[2],int iflag, void (*Dummy)())
{
   double Xc=1.11,Yc=3.14;

   switch(iflag) {
      case 1:
/*
 *      Initialise.
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
