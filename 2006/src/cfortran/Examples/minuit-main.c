/*
 * This example demonstrate how to call MINUIT from C.
 *
 * For question/problems contact: Heplib.Support@cern.ch
 *
 *                                                                               
 * Authors: William Hanlon <whanlon@cosmic.utah.edu>,
 *          Gunter Folger <Gunter.Folger@cern.ch>
 */

#include "cfortran.h"
#include "minuit.h"
#define Ncont 20
int main()
{
   int error_flag=0;
   PROTOCCALLSFSUB0(FCN,fcn)
   struct {
      double x[Ncont];
      double y[Ncont];
      int n;
   } pts;
   double f_null=0.;

   MNINIT(5,6,7);   /*  initialise  */
   MNSETI(" Minuit Example ");   /* set title */
   MNPARM(1,"X",0.,.1,f_null,f_null,error_flag);
   MNPARM(2,"-Y-",0.,.01,f_null,f_null,error_flag);
   MNEXCM(C_FUNCTION(FCN,fcn),"MIGRAD",0,0,error_flag,0);
   MNEXCM(C_FUNCTION(FCN,fcn),"MINOS",0,0,error_flag,0);
   MNCONT(C_FUNCTION(FCN,fcn),1,2,Ncont,pts.x[0],pts.y[0],pts.n, 0);
}
