/*
 * $Id: cs_hlshl.c,v 1.5 1998/01/12 09:56:07 couet Exp $
 *
 * $Log: cs_hlshl.c,v $
 * Revision 1.5  1998/01/12 09:56:07  couet
 * - Mods for LINUX (shared libraries)
 *
 * Revision 1.4  1996/12/05 09:59:05  berejnoi
 * correct definition of RTLD_NOW_CONST
 *
 * Revision 1.3  1996/12/03 16:38:15  berejnoi
 * New mode added: RTLD_GLOBAL
 * Mods in csnumb:more then 30 digits in a number
 * Mods in cscrexec: for AIX -berok added to solve problem
 * with incremental dyn. loading
 *
 * Revision 1.2  1996/05/13 08:06:03  berejnoi
 * Mods for SHL version on IBMRT
 *
 * Revision 1.1.1.1  1996/02/26 17:16:56  mclareni
 * Comis
 *
 */
#include "comis/pilot.h"
#if (defined(CERNLIB_SUN)||defined(CERNLIB_SGI)||defined(CERNLIB_IBMRT)||defined(CERNLIB_ALPHA_OSF)||defined(CERNLIB_LINUX))&&(defined(CERNLIB_SHL))
/*CMZ :          22/05/95  18.40.45  by  Julian Bunn*/
/*-- Author :*/
#if defined(CERNLIB_SUN)||defined(CERNLIB_LINUX)
#define SUN
#endif
#if defined(CERNLIB_SGI)
#define SGI
#endif
#if defined(CERNLIB_IBMRT)
#define AIX
#endif
#if defined(CERNLIB_ALPHA_OSF)
#define ALPHA_OSF
#endif

#if defined(SUN) || defined(SGI) || defined(ALPHA_OSF)
#include <dlfcn.h>
#endif
#ifdef AIX
#include "comis/dlfcn.inc"
#endif

#include <sys/time.h>

#include <signal.h>
#include <string.h>

#define MAXLENFL 60

struct procedures {
   char procname[32];
   int  (*funcptr)();
   struct procedures *next;
};

 struct files {
   char filename[MAXLENFL];
#if defined(SUN) || defined(SGI)
   int *file_handle;
#endif
#ifdef ALPHA_OSF
   void *file_handle;
#endif
#ifdef AIX
   void *file_handle;
#endif
   struct procedures *first_proc;
   struct files      *next;
};

static struct files *first_file = NULL;

static int debug_level = 0;

struct files *searchfile(filename, f)
  char *filename;
  struct files *f;
{
 while (f != NULL) {
    if (strcmp(filename,f->filename) == 0)
        return(f);
    else
        f = f->next;
 }

 return(f);
}

struct procedures *searchproc(procname, p)
  char *procname;
  struct procedures *p;
{
 while (p != NULL) {
    if (strcmp(procname,p->procname) == 0)
        return(p);
     else
        p = p->next;
 }
 return(p);

}

void Delete_all(f)
struct files *f;
{
 struct procedures *p;

 while (f->first_proc != NULL) {
    p = f->first_proc;
    f->first_proc = p->next;
    free(p);
 }

}

int  cs_shl_load_(path, n)
   char *path;
   int n;
{
   struct files *f;

#if defined(SUN) || defined(SGI)
   int    *file_handle;
#endif
#ifdef ALPHA_OSF
   void   *file_handle;
#endif
#ifdef AIX
   void   *file_handle;
#endif
   char   lib_name[MAXLENFL];

    strncpy(lib_name, path, n);
    lib_name[n] = '\0';

#if defined(SGI) || defined(ALPHA_OSF)

#define RTLD_NOW_CONST	RTLD_NOW

#elif defined(AIX) || defined(SUN)

#if defined(RTLD_GLOBAL)
#define RTLD_NOW_CONST  RTLD_NOW | RTLD_GLOBAL
#else
#define RTLD_NOW_CONST	RTLD_NOW
#endif

#endif
#if defined(SUN) || defined(SGI) || defined(ALPHA_OSF)
    if (strcmp(lib_name, "./0.sl") == 0) {
      file_handle = dlopen(NULL, RTLD_NOW_CONST);
    } else {
      file_handle = dlopen(lib_name, RTLD_NOW_CONST);
    }
#endif
#ifdef AIX
    file_handle = dlopen(lib_name, RTLD_NOW_CONST);
#endif

    if (file_handle ==  NULL) {
       printf("  %s \n",dlerror() );
       printf(" CS: Could not load shared library: %s\n",lib_name);
       return 1;
     }

/*   Add new file to the files list
*/
   f = (struct files *) malloc(sizeof(struct files));
   strcpy(f->filename,lib_name);
   f->file_handle = file_handle;
   f->next = first_file;
   f->first_proc = NULL;
   first_file = f;
   return 0;
}

void cs_shl_unload_(path, n)
   char *path;
   int n;
{
   struct files *f,
          *before;
   char   lib_name[MAXLENFL];

    strncpy(lib_name, path, n);
    lib_name[n] = '\0';

   /*
    *   find file
    */

   f = searchfile(lib_name,first_file);
   if (f == NULL) {
      if (debug_level > 0) printf("File not found.\n");
      return;
   }
   if (f != first_file) {
      before = first_file;
      while (before->next != f) before = before->next;
      before->next = f->next;
   }
   else {
      first_file = f->next;
   }
   Delete_all(f);

#if defined(SUN) || defined(SGI) || defined(ALPHA_OSF)
   if (dlclose(f->file_handle) != 0) {
#endif
#ifdef AIX
   if (dlclose(f->file_handle) != 0) {
#endif
      if (debug_level > 0) printf("Error in dlclose()...\n");
         return;
   }
   free(f);
   if (debug_level > 0) printf("Unlink %s file.\n",lib_name);
   return;
}

#if defined(SUN) || defined(SGI) || defined(ALPHA_OSF)
void *
#endif
#ifdef AIX
void *
#endif
cs_get_func_(sym,n)
   char *sym;
   int n;
{
   struct procedures *p;
   struct files *f;

#ifdef SUN
   int *fill_procaddr;
#endif
#ifdef AIX
   void *fill_procaddr;
#endif
#ifdef SGI
   void *fill_procaddr;
#endif
#ifdef ALPHA_OSF
   void   *fill_procaddr;
   int jumpad_();
   unsigned long ptr = (unsigned long)jumpad_;
#endif
   char  procname[80];

   strncpy(procname, sym, n);
   procname[n] = '\0';

/*
*--   Search for all files
*/
   f = first_file;
   while (f != NULL) {
     p = searchproc(procname, f->first_proc);
     if (p != NULL) return (void *)(p->funcptr);
     fill_procaddr =  dlsym(f->file_handle, procname);
     if (fill_procaddr != (void *) NULL) {
         p = (struct procedures *) malloc(sizeof(struct procedures));
         strcpy(p->procname, procname);
#ifdef ALPHA_OSF
         ptr = (unsigned long) fill_procaddr - ptr;
         p->funcptr = (int (*) ()) ptr;
#else
         p->funcptr = (int (*) ()) fill_procaddr;
#endif
         p->next = f->first_proc;
         f->first_proc = p;
         return (void *)(p->funcptr);
      }
     f = f->next;
   } /* end while */
  return 0;
}


void cs_shl_get_(ns, libname, n)
   char *libname;
   int  *ns;
   int   n;
{
   /* find name of ns [ns=0 for first] shared library  */
  struct files *f;

  int i=0;
  f=first_file;
  while (f != NULL && i < *ns) {
        f = f->next;
        i++;
  }

   memset(libname, ' ', n);
   if (f != NULL)
      strncpy(libname,f->filename , strlen(f->filename));
}

void cs_shl_symbols_(path, ns, symbol, n, nsy)
   char *path, *symbol;
   int  *ns;
   int   n, nsy;
{

   char   lib_name[MAXLENFL];
   struct files *f;
   static struct procedures *p;

   if (*ns == -1) {
      strncpy(lib_name, path, n);
      lib_name[n] = '\0';

      /* find shared library using its name */
      f = first_file;
      while (f != NULL) {
         if (!strcmp(lib_name, f->filename)) {
            p = f->first_proc;
            break;
         } else f = f->next;
      }
      if (f == NULL) {
         *ns = -2;
         return;
      }

      if (p == NULL) {
/**
         printf(" CS: Could not get symbols from shared library: %s\n",
                lib_name);
**/
         *ns = -2;
         return;
      }

      *ns = 0;
   } else {
      if (p == NULL) {
         *ns = -2;
         return;
      }
   }

   memset(symbol, ' ', 32);
   strncpy(symbol, p->procname, strlen(p->procname));
   p = p->next;
   (*ns)++;

}

#endif
