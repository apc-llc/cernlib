#include <stdlib.h>
#include <cfortran.h>
#include <packlib.h>

#define Memory_Size 50000
#define Number_of_Variables 27
#define Number_of_Events 10

typedef struct { float PAW[Memory_Size]; } PAWC_DEF;
#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(PAWC_DEF,PAWC);

main()
{
   int istat  = 0;
   int icycle = 0;
   int i,j,Ierr;
   int IDN = 0;
   int record_size = 1024;

   float Event[Number_of_Variables];
   char Chtags[Number_of_Variables][3] = 
                  {"A1","A2","A3","A4","A5","A6","A7","A8","A9",
                   "B1","B2","B3","B4","B5","B6","B7","B8","B9",
                   "C1","C2","C3","C4","C5","C6","C7","C8","C9"};

   HLIMIT(Memory_Size);

   HROPEN(1,"EXAMPLE","example.hbook","N",record_size,istat);
   if (istat != 0) {
      printf("Error in opening file ...\n");
      exit;
   }

   HBOOKN(10,"Ntuple example",Number_of_Variables,"//EXAMPLE",9000,Chtags);

   for (i=1; i<=Number_of_Events; i++) {
      for (j=0; j<Number_of_Variables; j++) {
         Event[j] = (float)i*(float)(j+1);
      }
      HFN(10,Event);
   }

   HROUT(0,icycle," ");
   HREND("EXAMPLE");
   KUCLOS(1," ",1);
/*
 *       Read back the ntuple 10
 */
   HROPEN(1,"EXAMPLE","example.hbook","U",record_size,istat);
   HRIN(10,9999,0);

   for (i=1; i<=10; i++) {
      HGN(10,IDN,i,Event,Ierr);
      printf("%f %f %f \n", Event[0],Event[1],Event[2]);
   }

   HREND("EXAMPLE");
   KUCLOS(1," ",1);
}
