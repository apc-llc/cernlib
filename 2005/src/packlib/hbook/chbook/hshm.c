/*
 * $Id: hshm.c,v 1.2 1996/03/13 10:13:08 couet Exp $
 *
 * $Log: hshm.c,v $
 * Revision 1.2  1996/03/13 10:13:08  couet
 * Mods for ALPHA_OSF: William Badgett, Univ of Wisconsin <BADGETT@vxdesy.desy.de>
 *
 * Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
 * First import
 *
 */
#include "hbook/pilot.h"
#if defined(CERNLIB_SYS5)
/*CMZ :  4.20/10 11/10/93  15.44.27  by  Rene Brun*/
/*-- Author :    Fons Rademakers   20/03/91*/
/*-- Modified:   Wojtek Burkot     02/03/92*/
 
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
 
#if defined(CERNLIB_QX_SC)
#define  hcreatei  hcreatei_
#define  hmapi     hmapi_
#define  hfreem    hfreem_
#endif
#if defined(CERNLIB_LINUX)
#define  SHM_R      0400
#define  SHM_W      0200
#endif
 
static int    shm_pawc;
static void  *paddr;
static long   len;
 
 
/***********************************************************************
 *                                                                     *
 *   Create a shared memory segment.                                   *
 *   mkey          identifier for the shared segment                   *
 *   size          length of common in 32 bit words                    *
 *   comaddr       required starting address of mapping on input       *
 *                 starting address of the mapping on output           *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_ALPHA_OSF)
int hcreatei(key_t *mkey, int *size, void *comaddr)
#endif
#if !defined(CERNLIB_ALPHA_OSF)
int hcreatei(key_t *mkey, int *size, long *comaddr)
#endif
{
   int             istat;
   int             flag = IPC_CREAT | 0666;
   unsigned long   inter;
   void           *req_addr;
 
   len = *size * 4;
 
   /* create shared memory segment */
   if ((shm_pawc = shmget(*mkey, len, flag)) < 0) {
      perror("shmget");
      istat = -errno;
      return(istat);
   }
 
   /*
    * attach shared memory segment
    * starting at req_addr
    */
#if !defined(CERNLIB_ALPHA_OSF)
    req_addr = (void*)(*comaddr);
#endif
#if defined(CERNLIB_ALPHA_OSF)
    req_addr = comaddr;
#endif
    if ((paddr = shmat(shm_pawc, req_addr, SHM_RND)) == (void *)-1) {
      perror("shmat");
      istat = -errno;
   } else {
      istat    = 0;
      inter    = (unsigned long) paddr;
#if !defined(CERNLIB_ALPHA_OSF)
      *comaddr = (long) (inter >> 2);
#endif
#if defined(CERNLIB_ALPHA_OSF)
      if ( paddr != comaddr )
      {
        printf ( "shmat: error attaching map to requested memory address\n" ) ;
        istat = -1;
      }
#endif
   }
   return(istat);
}
 
/***********************************************************************
 *                                                                     *
 *   Attach to existing shared memory segment.                         *
 *   mkey          identifier for the shared segment                   *
 *   comaddr       required starting address of mapping on input       *
 *                 starting address of the mapping on output           *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_ALPHA_OSF)
int hmapi(key_t *mkey, void *comaddr)
#endif
#if !defined(CERNLIB_ALPHA_OSF)
int hmapi(key_t *mkey, long *comaddr)
#endif
{
   int              istat;
   unsigned long    inter;
   void            *req_addr;
   struct shmid_ds  shm_stat;
 
   /* get id of existing shared memory segment */
   if ((shm_pawc = shmget(*mkey, 0, SHM_R | SHM_W)) < 0) {
      perror("shmget");
      istat = -errno;
      return(istat);
   }
 
   /* get size of shared memory segment */
   if (shmctl(shm_pawc, IPC_STAT, &shm_stat) == -1) {
      perror("shmctl");
      istat = -errno;
      return(istat);
   }
   len = shm_stat.shm_segsz;
 
  /*
   * set required mapping address - actual mapping addres depends on
   * setting of system variables. This requires modification in
   * the interface fortran routine HMAPM
   */
#if !defined(CERNLIB_ALPHA_OSF)
   req_addr=(void*)(*comaddr);
#endif
#if defined(CERNLIB_ALPHA_OSF)
   req_addr=comaddr;
#endif
   /* attach shared memory segment */
   if ((paddr = shmat(shm_pawc, req_addr, SHM_RND)) == (void *)-1) {
      perror("shmat");
      istat = -errno;
   } else {
      istat    = 0;
      inter    = (unsigned long) paddr;
#if !defined(CERNLIB_ALPHA_OSF)
      *comaddr = (long) (inter >> 2);
#endif
#if defined(CERNLIB_ALPHA_OSF)
      if ( paddr != comaddr )
      {
        printf ( "shmat: error attaching map to requested memory address\n" ) ;
        istat = -1;
      }
#endif
   }
   return(istat);
}
 
/***********************************************************************
 *                                                                     *
 *   Free a shared memory segment.                                     *
 *   comaddr      address of common that should be unmapped (not used) *
 *                                                                     *
 ***********************************************************************/
#if defined(CERNLIB_ALPHA_OSF)
int hfreem(void *comaddr)
#endif
#if !defined(CERNLIB_ALPHA_OSF)
int hfreem(long *comaddr)
#endif
{
   int istat;
 
   /* unmaps segment from address space */
   if ((istat = shmdt(paddr)) == -1) {
      perror("shmdt");
      istat = -errno;
      return(istat);
   }
 
   /* delete shared segment */
/*****
   if ((istat = shmctl(shm_pawc, IPC_RMID, (struct shmid_ds *)0)) == -1) {
      perror("shmctl");
      istat = -errno;
   }
*****/
   return(istat);
}
#endif
