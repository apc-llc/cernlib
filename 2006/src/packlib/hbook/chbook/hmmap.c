/*
 * $Id: hmmap.c,v 1.3 1997/09/02 13:08:59 mclareni Exp $
 *
 * $Log: hmmap.c,v $
 * Revision 1.3  1997/09/02 13:08:59  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/03/14 17:03:19  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:27:55  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
 * First import
 *
 */
#include "hbook/pilot.h"
#if defined(CERNLIB_BSD)||defined(CERNLIB_WINNT)
/*CMZ :  4.20/04 05/08/93  15.06.17  by  Rene Brun*/
/*-- Author :    Fons Rademakers   20/03/91*/
 
#ifdef WIN32
/*         One needs to set the following options to compile this module:
 *   $(cc) -DALPHA=1 -DWIN32 -DNT -DWIN -DNOT_IMPLEMENTED
 -D_PORTABLE_32BIT_CONTEXT
 */
  #include <windows.h>
  #include <io.h>
#endif
 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#ifndef WIN32
  #include <sys/mman.h>
  #include <sys/file.h>
  #include <sys/stat.h>
#endif
 
#if defined(CERNLIB_QXCAPT)
#  define  hcreatei  type_of_call HCREATEI 
#  define  hmapi     type_of_call HMAPI 
#  define  hfreem    type_of_call HFREEM 
#else
#  if defined(CERNLIB_QX_SC)
#    define  hcreatei  hcreatei_
#    define  hmapi     hmapi_
#    define  hfreem    hfreem_
#endif
#endif
 
static int  fd_pawc;
static long len;
 
 
/***********************************************************************
 *                                                                     *
 *   Create a memory mapped file and map common on it.                 *
 *   mfile         the filename                                        *
 *   mflen         length of filename                                  *
 *   base_common   common that should be mapped to file                *
 *   size          length of common in 32 bit words                    *
 *   comaddr       starting address of the mapping on output           *
 *                                                                     *
 ***********************************************************************/
#ifndef CERNLIB_MSSTDCALL)
int hcreatei(char *mfile, int *mflen, long *base_common, int *size,
             long *comaddr)
#else
int hcreatei(char *mfile, int len_mfile, int *mflen, long *base_common, int *size,
             long *comaddr)
#endif
{
#ifdef WIN32
   HANDLE hMap, paddr=NULL;
#else
   caddr_t         paddr;
   char           *file, *buf;
#endif
   int             istat;
   unsigned long   inter;
 
   len = *size * 4;
#ifndef WIN32
   file = calloc(*mflen+6, 1);
   strcpy(file, "/tmp/");
   strncat(file, mfile, *mflen);
 
   buf = calloc(len, 1);
 
   fd_pawc = open(file,O_RDWR|O_CREAT,0666);
   free(file);
   if (fd_pawc < 0) {
      perror("open");
      istat = -errno;
      free(buf);
   } else
#endif
     {
      /* reserve space in file */
#if defined(CERNLIB_WINNT)
      char *mapname;
      int i;
      istat = -1;
      mapname = malloc(*mflen+1);
      memcpy(mapname,mfile,*mflen);mapname[*mflen] = 0;
      hMap = CreateFileMapping((HANDLE)0xffffffff, NULL, PAGE_READWRITE,
                                  0, len, (LPSTR) mapname);
      free(mapname);
      paddr = NULL;
      if (hMap != NULL)
          paddr = MapViewOfFile(hMap, FILE_MAP_READ | FILE_MAP_WRITE, 0 , 0, 0);
#else
      istat = write(fd_pawc, buf, len);
      free(buf);
      if (istat != len) {
        istat = -1;
        close(fd_pawc);
        return(istat);
      }
#endif
#if defined(CERNLIB_APOLLO)
      paddr = mmap((caddr_t) base_common, &len,
                   PROT_READ|PROT_WRITE, MAP_FILE|MAP_SHARED, fd_pawc, 0);
#endif
#if defined(CERNLIB_SUN)
      paddr = mmap((caddr_t) base_common, len,
                   PROT_READ|PROT_WRITE, MAP_SHARED, fd_pawc, 0);
#endif
      if (paddr == NULL) {
         perror("mmap");
#ifdef WIN32
         istat = GetLastError();
         istat = -istat;
#else
         istat = -errno;
         close(fd_pawc);
#endif
      } else {
         istat    = 0;
         inter    = (unsigned long) paddr;
         *comaddr = (long) (inter >> 2);
      }
    }
  return(istat);
}
 
/***********************************************************************
 *                                                                     *
 *   Map common to file.                                               *
 *   mfile         the filename                                        *
 *   mflen         length of filename                                  *
 *   base_common   common that should be mapped to file                *
 *   size          length of common in 32 bit words                    *
 *   comaddr       starting address of the mapping on output           *
 *                                                                     *
 ***********************************************************************/
#ifndef CERNLIB_MSSTDCALL 
  int hmapi(char *mfile, int *mflen, long *base_common, long *comaddr)
#else
  int hmapi(char *mfile, int len_mfile, int *mflen, long *base_common, long *comaddr)
#endif
{
#ifdef WIN32
   HANDLE hMap, paddr;
#else
   caddr_t         paddr;
#endif
   unsigned long   inter;
   int             istat;
#if defined(CERNLIB_WINNT)
   int             i;
   char *mapname;
 
    mapname = malloc(*mflen+1);
    memcpy(mapname,mfile,*mflen);mapname[*mflen] = 0;
    hMap = OpenFileMapping(FILE_MAP_READ | FILE_MAP_WRITE, FALSE,
                          (LPTSTR)mapname);
    free(mapname);
 
    if (hMap == NULL) {
      perror("open");
      istat = GetLastError();
      istat = -istat;
    } else {
      paddr= MapViewOfFile(hMap, FILE_MAP_READ | FILE_MAP_WRITE,0,0,0);
#else
   char           *file;
   struct stat     buf;
 
   file = calloc(*mflen+6, 1);
   strcpy(file, "/tmp/");
   strncat(file, mfile, *mflen);
 
   fd_pawc = open(file,O_RDWR);
   free(file);
   if (fd_pawc < 0 ) {
      perror("open");
      istat = -errno;
   } else {
      stat(file,&buf);
      len = buf.st_size;
#endif
#if defined(CERNLIB_APOLLO)
      paddr = mmap((caddr_t) base_common, &len,
                   PROT_READ|PROT_WRITE, MAP_FILE|MAP_SHARED, fd_pawc, 0);
#endif
#if defined(CERNLIB_SUN)
      paddr = mmap((caddr_t) base_common, len,
                   PROT_READ|PROT_WRITE, MAP_SHARED, fd_pawc, 0);
#endif
      if (paddr == NULL) {
         perror("mmap");
#ifdef WIN32
         istat = GetLastError();
         istat = -istat;
#else
         istat = -errno;
         close(fd_pawc);
#endif
      } else {
         istat    = 0;
         inter    = (unsigned long) paddr;
         *comaddr = (long) (inter >> 2);
      }
   }
   return(istat);
}
 
/***********************************************************************
 *                                                                     *
 *   Unmap common and close file.                                      *
 *   comaddr       address of common that should be unmapped           *
 *                                                                     *
 ***********************************************************************/
int hfreem(long *comaddr)
{
   int istat;
 
#ifdef WIN32
  istat = 0;
  if (!UnmapViewOfFile((LPVOID)(*comaddr << 2))) istat = -1;
#else
   istat = munmap((caddr_t) *comaddr, len);
#endif
   if (istat == -1) {
      perror("munmap");
#ifdef WIN32
      istat = GetLastError();
      istat = -istat;
#else
      istat = -errno;
#endif
   }
#ifndef WIN32
   close(fd_pawc);
#endif
   return(istat);
}
#endif
