/*
 * This example demonstrate how to call HBOOK from C (histogram part).
 *
 * For question/problems contact: Heplib.Support@cern.ch
 *
 */

#include <stdlib.h>
#include <cfortran.h>
#include <hbook.h>

#if defined(__hpux) || defined(_IBMR2)
# define extname
#endif

#define PAWC_SIZE 50000

typedef struct { float PAW[PAWC_SIZE]; } PAWC_DEF;
#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);

#if defined(__vms) || defined(vms)
/*        On VMS rand has a range of 2**31 */
# define RAND_RANGE 2146483648.
#else
# define RAND_RANGE 32768.
#endif

main()
{
   int hid=1;
   int i,j;
   float r;
/*
 * Some C++ compiler generates a memset error when a character string
 * is passed directly as argument to a F77 function. Using an intermediate 
 * variable make it work.
 */

   HLIMIT(PAWC_SIZE);
   HBOOK1(hid,"Some random distribution",20000,-4.,4.,0.);

   for (i=0;i<100000;i++){
      for (j=0,r=0.;j<10;r += rand()/RAND_RANGE -0.5 ,j++);
         HFILL(hid,r,0.,1.);
   }
   HPRINT(hid);
}
