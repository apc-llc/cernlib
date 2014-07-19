/*
* $Id: dlfcn.inc,v 1.2 1996/12/03 16:36:55 berejnoi Exp $
*
* $Log: dlfcn.inc,v $
* Revision 1.2  1996/12/03 16:36:55  berejnoi
* Mods in csnumb:more then 30 digits in a number
* Mods in cscrexec: for AIX -berok added to solve problem
* with incremental dyn. loading
*
* Revision 1.1  1996/05/13 08:05:25  berejnoi
* Mods for SHL version on IBMRT
*
* Revision 1.1.1.1  1996/02/26 17:16:46  mclareni
* Comis
*
*
*
* dlfcn.inc
*/
/*CMZ :  1.18/02 13/04/94  17.40.02  by  Fons Rademakers*/
/*-- Author :    Vladimir Berezhnoi   01/04/94*/
/*
 * @(#)dlfcn.h	1.4 revision of 95/04/25  09:36:52
 * This is an unpublished work copyright (c) 1992 HELIOS Software GmbH
 * 30159 Hannover, Germany
 */

#ifndef __dlfcn_h__
#define __dlfcn_h__

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Mode flags for the dlopen routine.
 */
#define RTLD_LAZY	1	/* lazy function call binding */
#define RTLD_NOW	2	/* immediate function call binding */
#define RTLD_GLOBAL	0x100	/* allow symbols to be global */

/*
 * To be able to intialize, a library may provide a dl_info structure
 * that contains functions to be called to initialize and terminate.
 */
struct dl_info {
	void (*init)(void);
	void (*fini)(void);
};

#if __STDC__ || defined(_IBMR2)
void *dlopen(const char *path, int mode);
void *dlsym(void *handle, const char *symbol);
char *dlerror(void);
int dlclose(void *handle);
#else
void *dlopen();
void *dlsym();
char *dlerror();
int dlclose();
#endif

#ifdef __cplusplus
}
#endif

#endif /* __dlfcn_h__ */
